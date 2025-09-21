# server_session_management.R
# Server logik for session management inklusive auto-gendannelse og manuel gem/ryd

# Dependencies ----------------------------------------------------------------

# SESSION MANAGEMENT SETUP ====================================================

## Hovedfunktion for session management
# Opsætter al server logik relateret til session håndtering
setup_session_management <- function(input, output, session, app_state, emit, ui_service = NULL) {
  log_debug_block("SESSION_MGMT", "Initializing session management observers")

  # Check if centralized state is available
  use_centralized_state <- !is.null(app_state)
  # Auto-gendan session data når tilgængelig (hvis aktiveret)
  shiny::observeEvent(input$auto_restore_data,
    {
      shiny::req(input$auto_restore_data)

      # Tjek om auto-gendannelse er aktiveret
      if (!AUTO_RESTORE_ENABLED) {
        return()
      }

      safe_operation(
        "Auto restore session data",
        code = {
          saved_state <- input$auto_restore_data

          if (!is.null(saved_state$data)) {
            # Sæt gendannelses guards for at forhindre interferens
            # Unified state assignment only
            app_state$session$restoring_session <- TRUE
            # Unified state assignment only
            app_state$data$updating_table <- TRUE
            # Unified state assignment only
            app_state$data$table_operation_in_progress <- TRUE
            # Unified state assignment only
            app_state$session$auto_save_enabled <- FALSE

            # Oprydningsfunktion til at nulstille guards
            on.exit(
              {
                # Unified state assignment only
                app_state$data$updating_table <- FALSE
                # Unified state assignment only
                app_state$session$restoring_session <- FALSE
                # Unified state assignment only
                app_state$session$auto_save_enabled <- TRUE
                # Set flag for delayed cleanup - handled by separate observer
                # Unified state assignment only
                app_state$data$table_operation_cleanup_needed <- TRUE
              },
              add = TRUE
            )

            # Rekonstruer data.frame fra gemt struktur
            saved_data <- saved_state$data

            if (!is.null(saved_data$values) && !is.null(saved_data$nrows) && !is.null(saved_data$ncols)) {
              # Reconstruct data.frame manually
              reconstructed_data <- data.frame(
                matrix(nrow = saved_data$nrows, ncol = saved_data$ncols),
                stringsAsFactors = FALSE
              )

              # Set column names first if available
              if (!is.null(saved_data$col_names)) {
                names(reconstructed_data) <- saved_data$col_names
              }

              # Populate columns one by one
              for (i in seq_along(saved_data$values)) {
                reconstructed_data[[i]] <- saved_data$values[[i]]
              }

              # Restore column classes if available
              if (!is.null(saved_data$class_info)) {
                for (col_name in names(saved_data$class_info)) {
                  if (col_name %in% names(reconstructed_data)) {
                    target_class <- saved_data$class_info[[col_name]]
                    if (target_class == "numeric") {
                      reconstructed_data[[col_name]] <- as.numeric(reconstructed_data[[col_name]])
                    } else if (target_class == "character") {
                      reconstructed_data[[col_name]] <- as.character(reconstructed_data[[col_name]])
                    } else if (target_class == "logical") {
                      reconstructed_data[[col_name]] <- as.logical(reconstructed_data[[col_name]])
                    }
                  }
                }
              }

              # Dual-state sync during migration
              set_current_data(app_state, reconstructed_data)
              app_state$data$original_data <- reconstructed_data

              # Emit event to trigger downstream effects
              emit$data_loaded()
            } else {
              # Fallback for older save format
              # Dual-state sync during migration
              fallback_data <- as.data.frame(saved_state$data)
              set_current_data(app_state, fallback_data)
              app_state$data$original_data <- fallback_data

              # Emit event to trigger downstream effects
              emit$data_loaded()
            }

            # Unified state assignment only
            app_state$session$file_uploaded <- TRUE
            # Unified state assignment only
            app_state$columns$auto_detect$completed <- TRUE

            # Restore metadata if available
            if (!is.null(saved_state$metadata)) {
              restore_metadata(session, saved_state$metadata)
            }

            # Show notification about auto restore
            data_rows <- if (!is.null(saved_state$data$nrows)) {
              saved_state$data$nrows
            } else {
              nrow(saved_state$data)
            }

            shiny::showNotification(
              paste(
                "Tidligere session automatisk genindlæst:", data_rows, "datapunkter fra",
                format(as.POSIXct(saved_state$timestamp), "%d-%m-%Y %H:%M")
              ),
              type = "message",
              duration = 5
            )
          }
        },
        fallback = {
          # Reset guards even on error
          # Unified state assignment only
          app_state$data$updating_table <- FALSE
          # Unified state assignment only
          app_state$session$restoring_session <- FALSE
          # Unified state assignment only
          app_state$session$auto_save_enabled <- TRUE
          # Unified state assignment only
          app_state$data$table_operation_in_progress <- FALSE
        },
        session = session,
        error_type = "processing",
        show_user = TRUE,
        emit = emit,
        app_state = app_state
      )
    },
    once = TRUE
  )

  # Manual save handler
  shiny::observeEvent(input$manual_save, {
    # Unified state: Use centralized state for current data
    current_data_check <- app_state$data$current_data
    shiny::req(current_data_check)

    metadata <- collect_metadata(input)

    saveDataLocally(session, current_data_check, metadata)
    # Unified state assignment only
    app_state$session$last_save_time <- Sys.time()
    shiny::showNotification("Session gemt lokalt!", type = "message", duration = 2)
  })

  # Clear saved handler
  shiny::observeEvent(input$clear_saved, {
    handle_clear_saved_request(input, session, app_state, emit)
  })

  # Upload modal handler
  shiny::observeEvent(input$show_upload_modal, {
    show_upload_modal()
  })

  # Confirm clear saved handler
  shiny::observeEvent(input$confirm_clear_saved, {
    handle_confirm_clear_saved(session, app_state, emit, ui_service)
  })

  # Track file selection for modal
  output$fileSelected <- shiny::reactive({
    !is.null(input$data_file) && !is.null(input$data_file$datapath)
  })
  outputOptions(output, "fileSelected", suspendWhenHidden = FALSE)

  # Confirm upload handler
  shiny::observeEvent(input$confirm_upload, {
    # Set navigation flags for file upload flow
    # Use unified state management
    app_state$session$user_started_session <- TRUE
    app_state$session$file_uploaded <- FALSE  # Will be set to TRUE by actual file upload handler

    shiny::removeModal()
  })

  # Save status display
  output$save_status_display <- shiny::renderUI({
    # Unified state: Use centralized state for last save time
    last_save_time_check <- app_state$session$last_save_time

    if (!is.null(last_save_time_check)) {
      time_diff <- as.numeric(difftime(Sys.time(), last_save_time_check, units = "mins"))
      if (time_diff < 1) {
        shiny::span(shiny::icon("check"), " Gemt lige nu", style = "color: green;")
      } else if (time_diff < 60) {
        shiny::span(shiny::icon("clock"), paste(" Gemt for", round(time_diff), "min siden"))
      } else {
        shiny::span(shiny::icon("clock"), " Gemt for mere end 1 time siden")
      }
    }
  })

  # NOTE: output$dataLoaded is now handled in server_helpers.R with smart logic
}

# Helper functions for session management
restore_metadata <- function(session, metadata, ui_service = NULL) {

  shiny::isolate({
    if (!is.null(ui_service)) {
      # Use centralized UI service for metadata restoration
      ui_service$update_form_fields(metadata)
    } else {
      # Fallback to direct updates
      if (!is.null(metadata$title)) {
        shiny::updateTextInput(session, "indicator_title", value = metadata$title)
      }
      if (!is.null(metadata$unit_type)) {
        shiny::updateRadioButtons(session, "unit_type", selected = metadata$unit_type)
      }
      if (!is.null(metadata$unit_select)) {
        shiny::updateSelectizeInput(session, "unit_select", selected = metadata$unit_select)
      }
      if (!is.null(metadata$unit_custom)) {
        shiny::updateTextInput(session, "unit_custom", value = metadata$unit_custom)
      }
      if (!is.null(metadata$description)) {
        updateTextAreaInput(session, "indicator_description", value = metadata$description)
      }
      if (!is.null(metadata$chart_type)) {
        shiny::updateSelectizeInput(session, "chart_type", selected = metadata$chart_type)
      }
      if (!is.null(metadata$x_column)) {
        shiny::updateSelectizeInput(session, "x_column", selected = metadata$x_column)
      }
      if (!is.null(metadata$y_column)) {
        shiny::updateSelectizeInput(session, "y_column", selected = metadata$y_column)
      }
      if (!is.null(metadata$n_column)) {
        shiny::updateSelectizeInput(session, "n_column", selected = metadata$n_column)
      }
      if (!is.null(metadata$target_value)) {
        shiny::updateTextInput(session, "target_value", value = metadata$target_value)
      }
      if (!is.null(metadata$centerline_value)) {
        shiny::updateTextInput(session, "centerline_value", value = metadata$centerline_value)
      }
      if (!is.null(metadata$y_axis_unit)) {
        shiny::updateSelectizeInput(session, "y_axis_unit", selected = metadata$y_axis_unit)
      }
    }
  })
}

collect_metadata <- function(input) {
  shiny::isolate({
    list(
      title = input$indicator_title,
      unit_type = input$unit_type,
      unit_select = input$unit_select,
      unit_custom = input$unit_custom,
      description = input$indicator_description,
      x_column = if (is.null(input$x_column) || input$x_column == "") "" else input$x_column,
      y_column = if (is.null(input$y_column) || input$y_column == "") "" else input$y_column,
      n_column = if (is.null(input$n_column) || input$n_column == "") "" else input$n_column,
      skift_column = if (is.null(input$skift_column) || input$skift_column == "") "" else input$skift_column,
      kommentar_column = if (is.null(input$kommentar_column) || input$kommentar_column == "") "" else input$kommentar_column,
      chart_type = input$chart_type,
      target_value = input$target_value,
      centerline_value = input$centerline_value,
      y_axis_unit = if (is.null(input$y_axis_unit) || input$y_axis_unit == "") "count" else input$y_axis_unit
    )
  })
}

handle_clear_saved_request <- function(input, session, app_state, emit) {
  # Check if there's data or settings to lose - Use unified state
  current_data_check <- app_state$data$current_data
  has_data <- !is.null(current_data_check) &&
    any(!is.na(current_data_check), na.rm = TRUE) &&
    nrow(current_data_check) > 0

  has_settings <- (!is.null(input$indicator_title) && input$indicator_title != "") ||
    (!is.null(input$indicator_description) && input$indicator_description != "") ||
    (!is.null(input$unit_select) && input$unit_select != "") ||
    (!is.null(input$unit_custom) && input$unit_custom != "") ||
    # Unified state: Check centralized state for last save time
    (!is.null(app_state$session$last_save_time))

  # If no data or settings, start new session directly
  if (!has_data && !has_settings) {
    reset_to_empty_session(session, app_state, emit, ui_service)
    shiny::showNotification("Ny session startet", type = "message", duration = 2)
    return()
  }

  # If there IS data or settings, show confirmation dialog
  show_clear_confirmation_modal(has_data, has_settings)
}

handle_confirm_clear_saved <- function(session, app_state, emit, ui_service = NULL) {
  reset_to_empty_session(session, app_state, emit, ui_service)
  shiny::removeModal()
  shiny::showNotification("Ny session startet - alt data og indstillinger nulstillet", type = "message", duration = 4)
}

reset_to_empty_session <- function(session, app_state, emit, ui_service = NULL) {
  # Unified state: App state is always available
  use_centralized_state <- !is.null(app_state)
  log_debug_kv(
    session_reset_started = TRUE,
    centralized_state_available = use_centralized_state,
    app_state_hash_before = if(!is.null(app_state)) digest::digest(app_state$data$current_data) else "NULL",
    .context = "SESSION_RESET"
  )
  clearDataLocally(session)
  # Unified state assignment only
  app_state$session$last_save_time <- NULL

  # Unified state only
  app_state$data$updating_table <- TRUE

  # Force hide Anhøj rules until real data is loaded
  # Unified state assignment only
  app_state$ui$hide_anhoej_rules <- TRUE

  # Reset to standard column order using helper function
  # Sync current_data to both old and new state management
  # Brug synlige standarddata (så tabel er synlig) men force name-only detection
  standard_data <- create_empty_session_data()

  # Unified state assignment only
  app_state$data$current_data <- standard_data
  # Emit event to trigger downstream effects
  emit$data_loaded()

  # UNIFIED EVENTS: Trigger navigation change through event system
  emit$navigation_changed()

  # Unified state assignment only
  app_state$session$file_uploaded <- FALSE
  # Unified state assignment only
  app_state$session$user_started_session <- TRUE # NEW: Set flag that user has started
  # Unified state assignment only
  app_state$data$original_data <- NULL
  # Unified state assignment only
  app_state$columns$auto_detect$completed <- FALSE

  # Unified state: Get new standard session data
  new_data <- app_state$data$current_data

  # Reset UI inputs using centralized service
  shiny::isolate({
    if (!is.null(ui_service)) {
      # Use centralized UI service for all form resets
      ui_service$reset_form_fields()
    } else {
      # Fallback to direct updates
      shiny::updateTextInput(session, "indicator_title", value = "")
      shiny::updateRadioButtons(session, "unit_type", selected = "select")
      shiny::updateSelectizeInput(session, "unit_select", selected = "")
      shiny::updateTextInput(session, "unit_custom", value = "")
      updateTextAreaInput(session, "indicator_description", value = "")
      shiny::updateSelectizeInput(session, "chart_type", selected = "run")
      shiny::updateSelectizeInput(session, "y_axis_unit", selected = "count")

      # Opdater kolonnevalg med nye standardkolonner fra empty session data
      if (!is.null(new_data) && ncol(new_data) > 0) {
        new_col_names <- names(new_data)
        col_choices <- setNames(new_col_names, new_col_names)
        col_choices <- c("Vælg kolonne" = "", col_choices)


        shiny::updateSelectizeInput(session, "x_column", choices = col_choices, selected = "")
        shiny::updateSelectizeInput(session, "y_column", choices = col_choices, selected = "")
        shiny::updateSelectizeInput(session, "n_column", choices = col_choices, selected = "")
        shiny::updateSelectizeInput(session, "skift_column", choices = col_choices, selected = "")
        shiny::updateSelectizeInput(session, "frys_column", choices = col_choices, selected = "")
        shiny::updateSelectizeInput(session, "kommentar_column", choices = col_choices, selected = "")
      } else {
        # Fallback til tomme choices
        shiny::updateSelectizeInput(session, "x_column", choices = c("Vælg kolonne" = ""), selected = "")
        shiny::updateSelectizeInput(session, "y_column", choices = c("Vælg kolonne" = ""), selected = "")
        shiny::updateSelectizeInput(session, "n_column", choices = c("Vælg kolonne" = ""), selected = "")
        shiny::updateSelectizeInput(session, "skift_column", choices = c("Vælg kolonne" = ""), selected = "")
        shiny::updateSelectizeInput(session, "frys_column", choices = c("Vælg kolonne" = ""), selected = "")
        shiny::updateSelectizeInput(session, "kommentar_column", choices = c("Vælg kolonne" = ""), selected = "")
      }

      shiny::updateTextInput(session, "target_value", value = "")
      shiny::updateTextInput(session, "centerline_value", value = "")
    }

    shinyjs::reset("data_file")
  })

  # Force name-only detection på de nye standardkolonner efter UI opdatering
  if (!is.null(new_data) && ncol(new_data) > 0) {
    log_debug_kv(
      new_data_dimensions = paste(dim(new_data), collapse = "x"),
      new_data_columns = paste(names(new_data), collapse = ", "),
      .context = "SESSION_RESET"
    )

    # Kald den unified autodetect_engine for session reset
    # Dette sikrer konsistent event-logik og unified state management
    autodetect_result <- autodetect_engine(
      data = new_data,
      trigger_type = "session_start",  # Session reset behandles som ny session start
      app_state = app_state,
      emit = emit
    )
  }

  # Unified state only
  app_state$data$updating_table <- FALSE
}

show_upload_modal <- function() {
  shiny::showModal(shiny::modalDialog(
    title = shiny::div(
      shiny::icon("upload"),
      " Upload datafil",
      style = paste("color:", HOSPITAL_COLORS$primary)
    ),
    size = "m",
    shiny::div(
      style = "margin: 20px 0;",

      # File input i modal
      shiny::fileInput(
        "data_file",
        "Vælg datafil:",
        accept = c(".xlsx", ".xls", ".csv", ".CSV"),
        placeholder = "Ingen fil valgt...",
        width = "100%"
      ),
      shiny::hr(),
      shiny::div(
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; font-size: 0.9rem;",
        shiny::h6("Understøttede filformater:", style = "font-weight: 500; margin-bottom: 10px;"),
        shiny::tags$ul(
          style = "margin-bottom: 0;",
          shiny::tags$li(shiny::strong("Excel filer:"), " .xlsx, .xls - med automatisk kolonnedetekttion"),
          shiny::tags$li(shiny::strong("CSV filer:"), " .csv - danske indstillinger (semikolon, komma som decimal)")
        ),
        shiny::br(),
        shiny::div(
          style = "font-size: 0.8rem; color: #666;",
          shiny::icon("info-circle"),
          " Filer med 'Data' og 'Metadata' sheets genindlæser komplette sessioner automatisk."
        )
      )
    ),
    footer = shiny::tagList(
      shiny::modalButton("Annuller"),
      shiny::conditionalPanel(
        condition = "output.fileSelected == true",
        shiny::actionButton("confirm_upload", "Upload fil", class = "btn-primary", icon = shiny::icon("check"))
      )
    ),
    easyClose = TRUE
  ))
}

show_clear_confirmation_modal <- function(has_data, has_settings) {
  shiny::showModal(shiny::modalDialog(
    title = "Start ny session?",
    size = "m",
    shiny::div(
      shiny::icon("refresh"),
      " Er du sikker på at du vil starte en helt ny session?",
      shiny::br(), shiny::br(),
      shiny::p("Dette vil:"),
      shiny::tags$ul(
        if (has_data) shiny::tags$li("Slette eksisterende data i tabellen"),
        if (has_settings) shiny::tags$li("Nulstille titel, beskrivelse og andre indstillinger"),
        # Unified state: Check centralized state for last save time
        if (!is.null(app_state$session$last_save_time)) shiny::tags$li("Fjerne gemt session fra lokal storage"),
        shiny::tags$li("Oprette en tom standardtabel")
      ),
      shiny::br(),
      shiny::p("Denne handling kan ikke fortrydes.")
    ),
    footer = shiny::tagList(
      shiny::modalButton("Annuller"),
      shiny::actionButton("confirm_clear_saved", "Ja, start ny session", class = "btn-warning")
    ),
    easyClose = FALSE
  ))
}
# server_welcome_page.R
# Server logik for velkomstside interaktioner

# Dependencies ----------------------------------------------------------------

# VELKOMSTSIDE SETUP ==========================================================

## Hovedfunktion for velkomstside
# Opsætter alle handlers for velkomstside interaktioner
setup_welcome_page_handlers <- function(input, output, session, app_state, emit, ui_service = NULL) {

  # Håndtér "Start ny analyse" knap fra velkomstsiden
  shiny::observeEvent(input$start_new_session, {

    if (is.null(app_state)) {
      log_error("app_state is NULL - navigation will not work properly", "WELCOME_PAGE")
      return()
    }

    # Samme logik som eksisterende start_new_session
    # Unified state assignment only
    empty_session_data <- create_empty_session_data()
    app_state$data$current_data <- empty_session_data
    app_state$data$original_data <- empty_session_data

    # Emit event to trigger downstream effects
    emit$data_loaded()

    # REACTIVE WRAPPER FIX: Increment version to trigger reactive navigation
    old_version <- app_state$data$table_version
    app_state$data$table_version <- app_state$data$table_version + 1
    new_version <- app_state$data$table_version

    # Unified state assignment only - FALSE for manual session
    app_state$session$file_uploaded <- FALSE
    # Set user_started_session to TRUE for proper navigation
    app_state$session$user_started_session <- TRUE
    # Unified state assignment only
    app_state$ui$hide_anhoej_rules <- TRUE
    # Unified state: Clear session file name
    # Legacy session_file_name assignment removed - not used elsewhere
    app_state$session$file_name <- NULL

    # Nulstil konfigurationer
    # Unified state: Reset auto detect for welcome page
    # Using unified state management for auto-detect status
    app_state$columns$auto_detect$completed <- FALSE

    # Use centralized UI service for column resets
    if (!is.null(ui_service)) {
      ui_service$update_column_choices(clear_selections = TRUE)
    } else {
      # Fallback to direct updates - use updateSelectizeInput for consistency
      shiny::updateSelectizeInput(session, "x_column", selected = "")
      shiny::updateSelectizeInput(session, "y_column", selected = "")
      shiny::updateSelectizeInput(session, "n_column", selected = "")
      shiny::updateSelectizeInput(session, "skift_column", selected = "")
      shiny::updateSelectizeInput(session, "frys_column", selected = "")
      shiny::updateSelectizeInput(session, "kommentar_column", selected = "")
    }

    log_debug_kv(
      current_data_rows = nrow(empty_session_data),
      user_started_session = TRUE,
      .context = "WELCOME_PAGE"
    )
  })

  # Håndtér "Upload data" knap fra velkomstsiden
  shiny::observeEvent(input$upload_data_welcome, {
    # Fokusér på fil input eller åbn fil dialog
    shinyjs::click("file_upload")
  })

  # Håndtér "Quick start demo" knap
  shiny::observeEvent(input$quick_start_demo, {
    # Indlæs eksempel data
    test_file_path <- "R/data/spc_exampledata.csv"

    if (file.exists(test_file_path)) {
      safe_operation(
        "Load demo data for quick start",
        code = {
          demo_data <- readr::read_csv2(
            test_file_path,
            locale = readr::locale(
              decimal_mark = ",",
              grouping_mark = ".",
              encoding = "ISO-8859-1"
            ),
            show_col_types = FALSE
          )

          if (is.null(demo_data) || nrow(demo_data) == 0) {
            stop("No data loaded from file")
          }

          # Sikr at standard kolonner er til stede
          demo_data <- ensure_standard_columns(demo_data)

          # Sæt reaktive værdier
          # Unified state assignment only
          app_state$data$current_data <- demo_data
          app_state$data$original_data <- demo_data

          # Emit event to trigger downstream effects
          emit$data_loaded()
          # Unified state assignment only
          app_state$session$file_uploaded <- TRUE
          # Unified state: Set user started session for demo navigation
          app_state$session$user_started_session <- TRUE
          # Unified state: Reset auto detect for demo
          # Using unified state management - will trigger auto-detect
          app_state$columns$auto_detect$completed <- FALSE
          # Using unified state management - reset for new data
          # Unified state assignment only
          app_state$ui$hide_anhoej_rules <- FALSE # Vis Anhøj regler for rigtige data
          # Unified state: Set demo file name
          # Legacy session_file_name assignment removed - not used elsewhere
          app_state$session$file_name <- "Eksempel data (SPC demo)"

          # Vis succes besked
          shiny::showNotification(
            "Eksempel data indlæst! Du kan nu se SPC analysen.",
            type = "message",
            duration = 3
          )
        },
        fallback = {},
        session = session,
        error_type = "processing",
        show_user = TRUE,
        emit = emit,
        app_state = app_state
      )
    } else {
      log_warn("Demo data file not found at:", test_file_path, "DEMO_DATA")
      shiny::showNotification(
        "Eksempel data ikke tilgængelig. Prøv at uploade dine egne data.",
        type = "warning",
        duration = 5
      )
    }
  })
}
