# server_session_management.R
# Server logik for session management inklusive auto-gendannelse og manuel gem/ryd

# Dependencies ----------------------------------------------------------------

# SESSION MANAGEMENT SETUP ====================================================

## Hovedfunktion for session management
# Opsætter al server logik relateret til session håndtering
setup_session_management <- function(input, output, session, values, waiter_file, app_state = NULL) {
  cat("DEBUG: [SESSION_MGMT] ===========================================\n")
  cat("DEBUG: [SESSION_MGMT] Initializing session management observers\n")
  cat("DEBUG: [SESSION_MGMT] Received app_state environment address:", capture.output(print(app_state)), "\n")

  # PHASE 4: Check if centralized state is available
  use_centralized_state <- !is.null(app_state)
  # Auto-gendan session data når tilgængelig (hvis aktiveret)
  observeEvent(input$auto_restore_data,
    {
      req(input$auto_restore_data)

      # Tjek om auto-gendannelse er aktiveret
      if (!AUTO_RESTORE_ENABLED) {
        return()
      }

      tryCatch(
        {
          saved_state <- input$auto_restore_data

          if (!is.null(saved_state$data)) {
            # Sæt gendannelses guards for at forhindre interferens
            # Unified state: Set restoring session flag
            values$restoring_session <- TRUE
            app_state$session$restoring_session <- TRUE
            # Unified state: Set table updating flag
            values$updating_table <- TRUE
            app_state$data$updating_table <- TRUE
            # Unified state: Set table operation in progress flag
            values$table_operation_in_progress <- TRUE
            app_state$data$table_operation_in_progress <- TRUE
            # Unified state: Disable auto save during restore
            values$auto_save_enabled <- FALSE
            app_state$session$auto_save_enabled <- FALSE

            # Oprydningsfunktion til at nulstille guards
            on.exit(
              {
                # Unified state: Clear table updating flag
                values$updating_table <- FALSE
                app_state$data$updating_table <- FALSE
                # Unified state: Clear restoring session flag
                values$restoring_session <- FALSE
                app_state$session$restoring_session <- FALSE
                # Unified state: Re-enable auto save after restore
                values$auto_save_enabled <- TRUE
                app_state$session$auto_save_enabled <- TRUE
                # Set flag for delayed cleanup - handled by separate observer
                # Unified state: Set cleanup needed flag
                values$table_operation_cleanup_needed <- TRUE
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

              # Unified state: Set reconstructed current data
              values$current_data <- reconstructed_data
              app_state$data$current_data <- reconstructed_data
              # Unified state: Set reconstructed original data
              values$original_data <- reconstructed_data
              app_state$data$original_data <- reconstructed_data
            } else {
              # Fallback for older save format
              # Unified state: Set fallback current data
              values$current_data <- as.data.frame(saved_state$data)
              app_state$data$current_data <- as.data.frame(saved_state$data)
              # Unified state: Set fallback original data
              values$original_data <- as.data.frame(saved_state$data)
              app_state$data$original_data <- as.data.frame(saved_state$data)
            }

            # Unified state: Set file uploaded flag
            values$file_uploaded <- TRUE
            app_state$session$file_uploaded <- TRUE
            # Unified state: Set auto detect completed flag
            values$auto_detect_done <- TRUE
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
        error = function(e) {
          showNotification(paste("Fejl ved automatisk genindlæsning:", e$message), type = "error")

          # Reset guards even on error
          # Unified state: Clear table updating flag on error
          values$updating_table <- FALSE
          app_state$data$updating_table <- FALSE
          # Unified state: Clear restoring session flag on error
          values$restoring_session <- FALSE
          app_state$session$restoring_session <- FALSE
          # Unified state: Re-enable auto save on error
          values$auto_save_enabled <- TRUE
          app_state$session$auto_save_enabled <- TRUE
          # Unified state: Clear table operation flag on error
          values$table_operation_in_progress <- FALSE
          app_state$data$table_operation_in_progress <- FALSE
        }
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
    # Unified state: Set last save time
    values$last_save_time <- Sys.time()
    app_state$session$last_save_time <- Sys.time()
    showNotification("Session gemt lokalt!", type = "message", duration = 2)
  })

  # Clear saved handler
  observeEvent(input$clear_saved, {
    handle_clear_saved_request(input, session, values, app_state)
  })

  # Upload modal handler
  observeEvent(input$show_upload_modal, {
    show_upload_modal()
  })

  # Confirm clear saved handler
  observeEvent(input$confirm_clear_saved, {
    handle_confirm_clear_saved(session, values, app_state)
  })

  # Track file selection for modal
  output$fileSelected <- reactive({
    !is.null(input$data_file) && !is.null(input$data_file$datapath)
  })
  outputOptions(output, "fileSelected", suspendWhenHidden = FALSE)

  # Confirm upload handler
  observeEvent(input$confirm_upload, {
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
restore_metadata <- function(session, metadata) {
  log_debug("===================================", "METADATA_RESTORE")
  log_debug("Starting metadata restoration", "METADATA_RESTORE")
  log_debug(paste("Metadata keys:", paste(names(metadata), collapse = ", ")), "METADATA_RESTORE")

  isolate({
    if (!is.null(metadata$title)) {
      updateTextInput(session, "indicator_title", value = metadata$title)
    }
    if (!is.null(metadata$unit_type)) {
      updateRadioButtons(session, "unit_type", selected = metadata$unit_type)
    }
    if (!is.null(metadata$unit_select)) {
      log_debug(paste("Updating unit_select to:", metadata$unit_select), "METADATA_RESTORE")
      updateSelectizeInput(session, "unit_select", selected = metadata$unit_select)
      log_debug("✅ unit_select updated", "METADATA_RESTORE")
    }
    if (!is.null(metadata$unit_custom)) {
      updateTextInput(session, "unit_custom", value = metadata$unit_custom)
    }
    if (!is.null(metadata$description)) {
      updateTextAreaInput(session, "indicator_description", value = metadata$description)
    }
    if (!is.null(metadata$chart_type)) {
      log_debug(paste("Updating chart_type to:", metadata$chart_type), "METADATA_RESTORE")
      updateSelectizeInput(session, "chart_type", selected = metadata$chart_type)
      log_debug("✅ chart_type updated", "METADATA_RESTORE")
    }
    if (!is.null(metadata$x_column)) {
      log_debug(paste("Updating x_column to:", metadata$x_column), "METADATA_RESTORE")
      updateSelectizeInput(session, "x_column", selected = metadata$x_column)
      log_debug("✅ x_column updated", "METADATA_RESTORE")
    }
    if (!is.null(metadata$y_column)) {
      log_debug(paste("Updating y_column to:", metadata$y_column), "METADATA_RESTORE")
      updateSelectizeInput(session, "y_column", selected = metadata$y_column)
      log_debug("✅ y_column updated", "METADATA_RESTORE")
    }
    if (!is.null(metadata$n_column)) {
      log_debug(paste("Updating n_column to:", metadata$n_column), "METADATA_RESTORE")
      updateSelectizeInput(session, "n_column", selected = metadata$n_column)
      log_debug("✅ n_column updated", "METADATA_RESTORE")
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

handle_clear_saved_request <- function(input, session, values, app_state = NULL) {
  # Check if there's data or settings to lose
  has_data <- !is.null(values$current_data) &&
    any(!is.na(values$current_data), na.rm = TRUE) &&
    nrow(values$current_data) > 0

  has_settings <- (!is.null(input$indicator_title) && input$indicator_title != "") ||
    (!is.null(input$indicator_description) && input$indicator_description != "") ||
    (!is.null(input$unit_select) && input$unit_select != "") ||
    (!is.null(input$unit_custom) && input$unit_custom != "") ||
    # Unified state: Check centralized state for last save time
    (!is.null(app_state$session$last_save_time))

  # If no data or settings, start new session directly
  if (!has_data && !has_settings) {
    reset_to_empty_session(session, values, app_state)
    showNotification("Ny session startet", type = "message", duration = 2)
    return()
  }

  # If there IS data or settings, show confirmation dialog
  show_clear_confirmation_modal(has_data, has_settings, values)
}

handle_confirm_clear_saved <- function(session, values, app_state = NULL) {
  reset_to_empty_session(session, values, app_state)
  removeModal()
  showNotification("Ny session startet - alt data og indstillinger nulstillet", type = "message", duration = 4)
}

reset_to_empty_session <- function(session, values, app_state = NULL) {
  # Unified state: App state is always available
  cat("DEBUG: [SESSION_RESET] Session reset started, centralized state available:", use_centralized_state, "\n")
  cat("DEBUG: [SESSION_RESET] app_state hash before:", if(!is.null(app_state)) digest::digest(app_state$data$current_data) else "NULL", "\n")
  clearDataLocally(session)
  # Unified state: Clear last save time
  values$last_save_time <- NULL
  app_state$session$last_save_time <- NULL

  # Unified state: Set table updating flag
  values$updating_table <- TRUE
  app_state$data$updating_table <- TRUE

  # Force hide Anhøj rules until real data is loaded
  # Unified state: Hide Anhøj rules
  values$hide_anhoej_rules <- TRUE
  app_state$ui$hide_anhoej_rules <- TRUE

  # Reset to standard column order using helper function
  # PHASE 4: Sync current_data to both old and new state management
  # Brug synlige standarddata (så tabel er synlig) men force name-only detection
  standard_data <- create_empty_session_data()

  # Unified state: Set standard data
  values$current_data <- standard_data
  app_state$data$current_data <- standard_data
  cat("DEBUG: [SESSION_RESET] Session reset: synced standard_data to app_state, dims:", paste(dim(standard_data), collapse="x"), "\n")
  cat("DEBUG: [SESSION_RESET] app_state hash after:", digest::digest(app_state$data$current_data), "\n")

  # Unified state: Clear file uploaded flag
  values$file_uploaded <- FALSE
  app_state$session$file_uploaded <- FALSE
  # Unified state: Set user started session flag
  values$user_started_session <- TRUE # NEW: Set flag that user has started
  app_state$session$user_started_session <- TRUE
  # Unified state: Clear original data
  values$original_data <- NULL
  app_state$data$original_data <- NULL
  # Unified state: Reset auto detect flag
  values$auto_detect_done <- FALSE
  app_state$columns$auto_detect$completed <- FALSE
  values$initial_auto_detect_completed <- FALSE # Reset for new session

  # Unified state: Get new standard session data
  new_data <- app_state$data$current_data

  # Reset UI inputs
  isolate({
    updateTextInput(session, "indicator_title", value = "")
    updateRadioButtons(session, "unit_type", selected = "select")
    updateSelectizeInput(session, "unit_select", selected = "")
    updateTextInput(session, "unit_custom", value = "")
    updateTextAreaInput(session, "indicator_description", value = "")
    updateSelectizeInput(session, "chart_type", selected = "run")

    # Opdater kolonnevalg med nye standardkolonner fra empty session data

    if (!is.null(new_data) && ncol(new_data) > 0) {
      new_col_names <- names(new_data)
      col_choices <- setNames(new_col_names, new_col_names)
      col_choices <- c("Vælg kolonne" = "", col_choices)

      cat("DEBUG: [SESSION_RESET] Opdaterer selectizeInput med nye kolonner:", paste(new_col_names, collapse = ", "), "\n")

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
    updateSelectizeInput(session, "y_axis_unit", selected = "count")
    shinyjs::reset("data_file")
  })

  # Force name-only detection på de nye standardkolonner efter UI opdatering
  if (!is.null(new_data) && ncol(new_data) > 0) {
    cat("DEBUG: [SESSION_RESET] Force name-only detection:\n")
    cat("DEBUG: [SESSION_RESET] - New data dimensions:", dim(new_data), "\n")
    cat("DEBUG: [SESSION_RESET] - New data columns:", paste(names(new_data), collapse = ", "), "\n")

    # Kør name-only detection direkte i stedet for normal auto-detection
    cat("DEBUG: [SESSION_RESET] Running name-only detection directly...\n")

    # Kald name-only detection direkte med de nye kolonnenavne
    name_only_result <- detect_columns_name_only(names(new_data), NULL, session, values, app_state)

    cat("DEBUG: [SESSION_RESET] ✅ Name-only detection completed for session reset\n")
  }

  # Unified state: Clear table updating flag
  values$updating_table <- FALSE
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

show_clear_confirmation_modal <- function(has_data, has_settings, values) {
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
setup_welcome_page_handlers <- function(input, output, session, values, waiter_file) {
  # Håndtér "Start ny analyse" knap fra velkomstsiden
  observeEvent(input$start_new_session, {
    cat("Welcome page: Start new session clicked\n")

    # Samme logik som eksisterende start_new_session
    # Unified state: Set empty session data
    values$current_data <- create_empty_session_data()
    app_state$data$current_data <- create_empty_session_data()
    # Unified state: Set original data from current data
    values$original_data <- values$current_data
    app_state$data$original_data <- values$current_data
    # Unified state: Set file uploaded flag
    values$file_uploaded <- TRUE
    app_state$session$file_uploaded <- TRUE
    # Unified state: Hide Anhøj rules for welcome page
    values$hide_anhoej_rules <- TRUE
    app_state$ui$hide_anhoej_rules <- TRUE
    # Unified state: Clear session file name
    values$session_file_name <- NULL
    app_state$session$file_name <- NULL

    # Nulstil konfigurationer
    # Unified state: Reset auto detect for welcome page
    values$auto_detect_done <- FALSE
    app_state$columns$auto_detect$completed <- FALSE
    updateSelectInput(session, "x_column", selected = "")
    updateSelectInput(session, "y_column", selected = "")
    updateSelectInput(session, "n_column", selected = "")

    cat("Welcome page: New empty session created\n")
  })

  # Håndtér "Upload data" knap fra velkomstsiden
  observeEvent(input$upload_data_welcome, {
    cat("Welcome page: Upload data clicked\n")
    # Fokusér på fil input eller åbn fil dialog
    shinyjs::click("file_upload")
  })

  # Håndtér "Quick start demo" knap
  observeEvent(input$quick_start_demo, {
    cat("Welcome page: Quick start demo clicked\n")

    # Indlæs eksempel data
    test_file_path <- "R/data/spc_exampledata.csv"

    if (file.exists(test_file_path)) {
      tryCatch(
        {
          cat("Welcome page: Starting demo data load...\n")

          # Vis indlæsnings waiter
          waiter_file$show()

          # Indlæs demo data med readr::read_csv2 (samme som fungerende fil upload)
          cat("Loading demo data with readr::read_csv2...\n")
          demo_data <- readr::read_csv2(
            test_file_path,
            locale = readr::locale(
              decimal_mark = ",",
              grouping_mark = ".",
              encoding = "ISO-8859-1"
            ),
            show_col_types = FALSE
          )

          cat("Successfully loaded with read_csv2\n")
          cat("Column names:", paste(names(demo_data), collapse = ", "), "\n")
          cat("Rows loaded:", nrow(demo_data), "\n")

          if (is.null(demo_data) || nrow(demo_data) == 0) {
            stop("No data loaded from file")
          }

          cat("Demo data column names before ensure_standard_columns:", paste(names(demo_data), collapse = ", "), "\n")

          # Sikr at standard kolonner er til stede
          demo_data <- ensure_standard_columns(demo_data)

          cat("Demo data column names after ensure_standard_columns:", paste(names(demo_data), collapse = ", "), "\n")
          cat("Final data dimensions:", paste(dim(demo_data), collapse = "x"), "\n")

          # Sæt reaktive værdier
          # Unified state: Set demo current data
          values$current_data <- demo_data
          app_state$data$current_data <- demo_data
          # Unified state: Set demo original data
          values$original_data <- demo_data
          app_state$data$original_data <- demo_data
          # Unified state: Set file uploaded for demo
          values$file_uploaded <- TRUE
          app_state$session$file_uploaded <- TRUE
          # Unified state: Reset auto detect for demo
          values$auto_detect_done <- FALSE # Vil udløse auto-detekt
          app_state$columns$auto_detect$completed <- FALSE
          values$initial_auto_detect_completed <- FALSE # Reset for new data
          # Unified state: Show Anhøj rules for demo data
          values$hide_anhoej_rules <- FALSE # Vis Anhøj regler for rigtige data
          app_state$ui$hide_anhoej_rules <- FALSE
          # Unified state: Set demo file name
          values$session_file_name <- "Eksempel data (SPC demo)"
          app_state$session$file_name <- "Eksempel data (SPC demo)"

          # Skjul waiter
          later::later(function() {
            waiter_file$hide()
          }, 0.5)

          cat("Welcome page: Demo data loaded successfully\n")

          # Vis succes besked
          showNotification(
            "Eksempel data indlæst! Du kan nu se SPC analysen.",
            type = "message",
            duration = 3
          )
        },
        error = function(e) {
          cat("ERROR: Failed to load demo data:", e$message, "\n")
          cat("ERROR: Stacktrace:", "\n")
          traceback()

          waiter_file$hide()

          # Vis fejlbesked til bruger
          showNotification(
            paste("Kunne ikke indlæse eksempel data:", e$message),
            type = "error",
            duration = 8
          )
        }
      )
    } else {
      cat("WARNING: Demo data file not found at:", test_file_path, "\n")
      showNotification(
        "Eksempel data ikke tilgængelig. Prøv at uploade dine egne data.",
        type = "warning",
        duration = 5
      )
    }
  })
}
