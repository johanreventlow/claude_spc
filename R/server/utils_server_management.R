# server_session_management.R
# Server logik for session management inklusive auto-gendannelse og manuel gem/ryd

# Dependencies ----------------------------------------------------------------

# SESSION MANAGEMENT SETUP ====================================================

## Hovedfunktion for session management
# Opsætter al server logik relateret til session håndtering
setup_session_management <- function(input, output, session, app_state, emit, ui_service = NULL) {
  log_debug_block("SESSION_MGMT", "Initializing session management observers")
  # log_debug("Received app_state environment address:", capture.output(print(app_state)), .context = "SESSION_MGMT")

  # Check if centralized state is available
  use_centralized_state <- !is.null(app_state)
  # Auto-gendan session data når tilgængelig (hvis aktiveret)
  observeEvent(input$auto_restore_data,
    {
      req(input$auto_restore_data)

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

            showNotification(
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
  observeEvent(input$manual_save, {
    # Unified state: Use centralized state for current data
    current_data_check <- app_state$data$current_data
    req(current_data_check)

    metadata <- collect_metadata(input)

    saveDataLocally(session, current_data_check, metadata)
    # Unified state assignment only
    app_state$session$last_save_time <- Sys.time()
    showNotification("Session gemt lokalt!", type = "message", duration = 2)
  })

  # Clear saved handler
  observeEvent(input$clear_saved, {
    handle_clear_saved_request(input, session, app_state, emit)
  })

  # Upload modal handler
  observeEvent(input$show_upload_modal, {
    show_upload_modal()
  })

  # Confirm clear saved handler
  observeEvent(input$confirm_clear_saved, {
    handle_confirm_clear_saved(session, app_state, emit, ui_service)
  })

  # Track file selection for modal
  output$fileSelected <- reactive({
    !is.null(input$data_file) && !is.null(input$data_file$datapath)
  })
  outputOptions(output, "fileSelected", suspendWhenHidden = FALSE)

  # Confirm upload handler
  observeEvent(input$confirm_upload, {
  # log_debug("Confirm upload clicked", .context = "UPLOAD_MODAL")

    # Set navigation flags for file upload flow
    # Use unified state management
    app_state$session$user_started_session <- TRUE
    app_state$session$file_uploaded <- FALSE  # Will be set to TRUE by actual file upload handler

  # log_debug("Navigation flags set, closing modal", .context = "UPLOAD_MODAL")
    removeModal()
  })

  # Save status display
  output$save_status_display <- renderUI({
    # Unified state: Use centralized state for last save time
    last_save_time_check <- app_state$session$last_save_time

    if (!is.null(last_save_time_check)) {
      time_diff <- as.numeric(difftime(Sys.time(), last_save_time_check, units = "mins"))
      if (time_diff < 1) {
        span(icon("check"), " Gemt lige nu", style = "color: green;")
      } else if (time_diff < 60) {
        span(icon("clock"), paste(" Gemt for", round(time_diff), "min siden"))
      } else {
        span(icon("clock"), " Gemt for mere end 1 time siden")
      }
    }
  })

  # NOTE: output$dataLoaded is now handled in server_helpers.R with smart logic
}

# Helper functions for session management
restore_metadata <- function(session, metadata, ui_service = NULL) {
  # log_debug("===================================", "METADATA_RESTORE")
  # log_debug("Starting metadata restoration", "METADATA_RESTORE")
  # log_debug(paste("Metadata keys:", paste(names(metadata), collapse = ", ")), "METADATA_RESTORE")

  isolate({
    if (!is.null(ui_service)) {
      # Use centralized UI service for metadata restoration
      ui_service$update_form_fields(metadata)
  # log_debug("✅ Used centralized ui_service for metadata restore", "METADATA_RESTORE")
    } else {
      # Fallback to direct updates
      if (!is.null(metadata$title)) {
        updateTextInput(session, "indicator_title", value = metadata$title)
      }
      if (!is.null(metadata$unit_type)) {
        updateRadioButtons(session, "unit_type", selected = metadata$unit_type)
      }
      if (!is.null(metadata$unit_select)) {
  # log_debug(paste("Updating unit_select to:", metadata$unit_select), "METADATA_RESTORE")
        updateSelectizeInput(session, "unit_select", selected = metadata$unit_select)
  # log_debug("✅ unit_select updated", "METADATA_RESTORE")
      }
      if (!is.null(metadata$unit_custom)) {
        updateTextInput(session, "unit_custom", value = metadata$unit_custom)
      }
      if (!is.null(metadata$description)) {
        updateTextAreaInput(session, "indicator_description", value = metadata$description)
      }
      if (!is.null(metadata$chart_type)) {
  # log_debug(paste("Updating chart_type to:", metadata$chart_type), "METADATA_RESTORE")
        updateSelectizeInput(session, "chart_type", selected = metadata$chart_type)
  # log_debug("✅ chart_type updated", "METADATA_RESTORE")
      }
      if (!is.null(metadata$x_column)) {
  # log_debug(paste("Updating x_column to:", metadata$x_column), "METADATA_RESTORE")
        updateSelectizeInput(session, "x_column", selected = metadata$x_column)
  # log_debug("✅ x_column updated", "METADATA_RESTORE")
      }
      if (!is.null(metadata$y_column)) {
  # log_debug(paste("Updating y_column to:", metadata$y_column), "METADATA_RESTORE")
        updateSelectizeInput(session, "y_column", selected = metadata$y_column)
  # log_debug("✅ y_column updated", "METADATA_RESTORE")
      }
      if (!is.null(metadata$n_column)) {
  # log_debug(paste("Updating n_column to:", metadata$n_column), "METADATA_RESTORE")
        updateSelectizeInput(session, "n_column", selected = metadata$n_column)
  # log_debug("✅ n_column updated", "METADATA_RESTORE")
      }
      if (!is.null(metadata$target_value)) {
        updateTextInput(session, "target_value", value = metadata$target_value)
      }
      if (!is.null(metadata$centerline_value)) {
        updateTextInput(session, "centerline_value", value = metadata$centerline_value)
      }
      if (!is.null(metadata$y_axis_unit)) {
        updateSelectizeInput(session, "y_axis_unit", selected = metadata$y_axis_unit)
      }
    }
  })
}

collect_metadata <- function(input) {
  isolate({
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
    showNotification("Ny session startet", type = "message", duration = 2)
    return()
  }

  # If there IS data or settings, show confirmation dialog
  show_clear_confirmation_modal(has_data, has_settings)
}

handle_confirm_clear_saved <- function(session, app_state, emit, ui_service = NULL) {
  reset_to_empty_session(session, app_state, emit, ui_service)
  removeModal()
  showNotification("Ny session startet - alt data og indstillinger nulstillet", type = "message", duration = 4)
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
  # log_debug("Session reset: synced standard_data to app_state, dims:", paste(dim(standard_data), collapse="x"), .context = "SESSION_RESET")

  # Emit event to trigger downstream effects
  emit$data_loaded()
  # log_debug("app_state hash after:", digest::digest(app_state$data$current_data), .context = "SESSION_RESET")

  # UNIFIED EVENTS: Trigger navigation change through event system
  # log_debug("Emitting navigation_changed event", .context = "SESSION_RESET")
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
  isolate({
    if (!is.null(ui_service)) {
      # Use centralized UI service for all form resets
      ui_service$reset_form_fields()
  # log_debug("✅ Used centralized ui_service for form reset", .context = "SESSION_RESET")
    } else {
      # Fallback to direct updates
      updateTextInput(session, "indicator_title", value = "")
      updateRadioButtons(session, "unit_type", selected = "select")
      updateSelectizeInput(session, "unit_select", selected = "")
      updateTextInput(session, "unit_custom", value = "")
      updateTextAreaInput(session, "indicator_description", value = "")
      updateSelectizeInput(session, "chart_type", selected = "run")
      updateSelectizeInput(session, "y_axis_unit", selected = "count")

      # Opdater kolonnevalg med nye standardkolonner fra empty session data
      if (!is.null(new_data) && ncol(new_data) > 0) {
        new_col_names <- names(new_data)
        col_choices <- setNames(new_col_names, new_col_names)
        col_choices <- c("Vælg kolonne" = "", col_choices)

  # log_debug("Opdaterer selectizeInput med nye kolonner:", paste(new_col_names, collapse = ", "), .context = "SESSION_RESET")

        updateSelectizeInput(session, "x_column", choices = col_choices, selected = "")
        updateSelectizeInput(session, "y_column", choices = col_choices, selected = "")
        updateSelectizeInput(session, "n_column", choices = col_choices, selected = "")
        updateSelectizeInput(session, "skift_column", choices = col_choices, selected = "")
        updateSelectizeInput(session, "frys_column", choices = col_choices, selected = "")
        updateSelectizeInput(session, "kommentar_column", choices = col_choices, selected = "")
      } else {
        # Fallback til tomme choices
        updateSelectizeInput(session, "x_column", choices = c("Vælg kolonne" = ""), selected = "")
        updateSelectizeInput(session, "y_column", choices = c("Vælg kolonne" = ""), selected = "")
        updateSelectizeInput(session, "n_column", choices = c("Vælg kolonne" = ""), selected = "")
        updateSelectizeInput(session, "skift_column", choices = c("Vælg kolonne" = ""), selected = "")
        updateSelectizeInput(session, "frys_column", choices = c("Vælg kolonne" = ""), selected = "")
        updateSelectizeInput(session, "kommentar_column", choices = c("Vælg kolonne" = ""), selected = "")
      }

      updateTextInput(session, "target_value", value = "")
      updateTextInput(session, "centerline_value", value = "")
    }

    shinyjs::reset("data_file")
  })

  # Force name-only detection på de nye standardkolonner efter UI opdatering
  if (!is.null(new_data) && ncol(new_data) > 0) {
  # log_debug("Force name-only detection:", .context = "SESSION_RESET")
    log_debug_kv(
      new_data_dimensions = paste(dim(new_data), collapse = "x"),
      new_data_columns = paste(names(new_data), collapse = ", "),
      .context = "SESSION_RESET"
    )

    # Kør autodetect_engine med name-only strategy for session reset
  # log_debug("Running autodetect_engine with name-only strategy...", .context = "SESSION_RESET")

    # Kald den unified autodetect_engine for session reset
    # Dette sikrer konsistent event-logik og unified state management
    autodetect_result <- autodetect_engine(
      data = new_data,
      trigger_type = "session_start",  # Session reset behandles som ny session start
      app_state = app_state,
      emit = emit
    )

  # log_debug("✅ Autodetect_engine completed for session reset", .context = "SESSION_RESET")
  }

  # Unified state only
  app_state$data$updating_table <- FALSE
}

show_upload_modal <- function() {
  showModal(modalDialog(
    title = div(
      icon("upload"),
      " Upload datafil",
      style = paste("color:", HOSPITAL_COLORS$primary)
    ),
    size = "m",
    div(
      style = "margin: 20px 0;",

      # File input i modal
      fileInput(
        "data_file",
        "Vælg datafil:",
        accept = c(".xlsx", ".xls", ".csv", ".CSV"),
        placeholder = "Ingen fil valgt...",
        width = "100%"
      ),
      hr(),
      div(
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; font-size: 0.9rem;",
        h6("Understøttede filformater:", style = "font-weight: 500; margin-bottom: 10px;"),
        tags$ul(
          style = "margin-bottom: 0;",
          tags$li(strong("Excel filer:"), " .xlsx, .xls - med automatisk kolonnedetekttion"),
          tags$li(strong("CSV filer:"), " .csv - danske indstillinger (semikolon, komma som decimal)")
        ),
        br(),
        div(
          style = "font-size: 0.8rem; color: #666;",
          icon("info-circle"),
          " Filer med 'Data' og 'Metadata' sheets genindlæser komplette sessioner automatisk."
        )
      )
    ),
    footer = tagList(
      modalButton("Annuller"),
      conditionalPanel(
        condition = "output.fileSelected == true",
        actionButton("confirm_upload", "Upload fil", class = "btn-primary", icon = icon("check"))
      )
    ),
    easyClose = TRUE
  ))
}

show_clear_confirmation_modal <- function(has_data, has_settings) {
  showModal(modalDialog(
    title = "Start ny session?",
    size = "m",
    div(
      icon("refresh"),
      " Er du sikker på at du vil starte en helt ny session?",
      br(), br(),
      p("Dette vil:"),
      tags$ul(
        if (has_data) tags$li("Slette eksisterende data i tabellen"),
        if (has_settings) tags$li("Nulstille titel, beskrivelse og andre indstillinger"),
        # Unified state: Check centralized state for last save time
        if (!is.null(app_state$session$last_save_time)) tags$li("Fjerne gemt session fra lokal storage"),
        tags$li("Oprette en tom standardtabel")
      ),
      br(),
      p("Denne handling kan ikke fortrydes.")
    ),
    footer = tagList(
      modalButton("Annuller"),
      actionButton("confirm_clear_saved", "Ja, start ny session", class = "btn-warning")
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
  # log_debug("Setting up welcome page handlers", .context = "WELCOME_PAGE_SETUP")
  # log_debug("app_state provided:", !is.null(app_state), .context = "WELCOME_PAGE_SETUP")

  # Håndtér "Start ny analyse" knap fra velkomstsiden
  observeEvent(input$start_new_session, {
  # log_debug("Start new session clicked", .context = "WELCOME_PAGE")
  # log_debug("app_state available:", !is.null(app_state), .context = "WELCOME_PAGE")

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
  # log_debug("table_version incremented from", old_version, "to", new_version, .context = "WELCOME_PAGE")

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
      updateSelectizeInput(session, "x_column", selected = "")
      updateSelectizeInput(session, "y_column", selected = "")
      updateSelectizeInput(session, "n_column", selected = "")
      updateSelectizeInput(session, "skift_column", selected = "")
      updateSelectizeInput(session, "frys_column", selected = "")
      updateSelectizeInput(session, "kommentar_column", selected = "")
    }

  # log_debug("New empty session created", .context = "WELCOME_PAGE")
    log_debug_kv(
      current_data_rows = nrow(empty_session_data),
      user_started_session = TRUE,
      .context = "WELCOME_PAGE"
    )
  })

  # Håndtér "Upload data" knap fra velkomstsiden
  observeEvent(input$upload_data_welcome, {
  # log_debug("Welcome page: Upload data clicked", "SERVER_MGMT")
    # Fokusér på fil input eller åbn fil dialog
    shinyjs::click("file_upload")
  })

  # Håndtér "Quick start demo" knap
  observeEvent(input$quick_start_demo, {
  # log_debug("Welcome page: Quick start demo clicked", "SERVER_MGMT")

    # Indlæs eksempel data
    test_file_path <- "R/data/spc_exampledata.csv"

    if (file.exists(test_file_path)) {
      safe_operation(
        "Load demo data for quick start",
        code = {
  # log_debug("Welcome page: Starting demo data load...", "SERVER_MGMT")

          # Loading demo data
  # log_debug("Loading demo data...", "SERVER_MGMT")

          # Indlæs demo data med readr::read_csv2 (samme som fungerende fil upload)
  # log_debug("Loading demo data with readr::read_csv2...", "SERVER_MGMT")
          demo_data <- readr::read_csv2(
            test_file_path,
            locale = readr::locale(
              decimal_mark = ",",
              grouping_mark = ".",
              encoding = "ISO-8859-1"
            ),
            show_col_types = FALSE
          )

  # log_debug("Successfully loaded with read_csv2", "SERVER_MGMT")
  # log_debug(paste("Column names:", paste(names(demo_data), collapse = ", ")), "SERVER_MGMT")
  # log_debug(paste("Rows loaded:", nrow(demo_data)), "SERVER_MGMT")

          if (is.null(demo_data) || nrow(demo_data) == 0) {
            stop("No data loaded from file")
          }

  # log_debug(paste("Demo data column names before ensure_standard_columns:", paste(names(demo_data), collapse = ", ")), "SERVER_MGMT")

          # Sikr at standard kolonner er til stede
          demo_data <- ensure_standard_columns(demo_data)

  # log_debug(paste("Demo data column names after ensure_standard_columns:", paste(names(demo_data), collapse = ", ")), "SERVER_MGMT")
  # log_debug(paste("Final data dimensions:", paste(dim(demo_data), collapse = "x")), "SERVER_MGMT")

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

          # Demo data processing complete
  # log_debug("Demo data processing complete", "SERVER_MGMT")

  # log_debug("Welcome page: Demo data loaded successfully", "SERVER_MGMT")

          # Vis succes besked
          showNotification(
            "Eksempel data indlæst! Du kan nu se SPC analysen.",
            type = "message",
            duration = 3
          )
        },
        fallback = {
  # log_debug("Demo data loading failed, fallback executed", "SERVER_MGMT")
        },
        session = session,
        error_type = "processing",
        show_user = TRUE,
        emit = emit,
        app_state = app_state
      )
    } else {
      log_warn("Demo data file not found at:", test_file_path, "DEMO_DATA")
      showNotification(
        "Eksempel data ikke tilgængelig. Prøv at uploade dine egne data.",
        type = "warning",
        duration = 5
      )
    }
  })
}
