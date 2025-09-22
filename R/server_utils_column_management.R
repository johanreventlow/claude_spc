# utils_column_management.R
# Server utilities for column management including auto-detection and validation
# Migrated from fct_data_processing.R - contains only active functions

# Dependencies ----------------------------------------------------------------

# KOLONNEHÅNDTERING SETUP ====================================================

#' Opsæt kolonnehåndtering for SPC app
#'
#' Hovedfunktion der opsætter al server-side logik relateret til kolonne-management,
#' inklusive auto-detektion, validering og reactive observers for kolonnevalg.
#' Understøtter både legacy values-baseret og ny centraliseret state management.
#'
#' @param input Shiny input object med brugerinteraktioner
#' @param output Shiny output object for rendering
#' @param session Shiny session object for server kommunikation
#' @param values Reactive values list med app state (legacy system)
#' @param app_state List med centraliseret app state (Phase 4 system), optional
#'
#' @details
#' Funktionen opsætter følgende observers:
#' \itemize{
#'   \item Kolonneopdatering ved data ændringer
#'   \item Auto-detektion trigger ved file upload
#'   \item UI synkronisering efter auto-detektion
#'   \item Fejlhåndtering og user feedback
#' }
#'
#' Compatibility: Funktionen detekterer automatisk om centraliseret
#' state management er tilgængeligt og tilpasser sig entsprechend.
#'
#' @return NULL (side effects via observers)
#'
#' @examples
#' \dontrun{
#' # I Shiny server function:
#' setup_column_management(input, output, session, values, app_state)
#' }
#'
#' @seealso \code{\link{autodetect_engine}}, \code{\link{ensure_standard_columns}}
setup_column_management <- function(input, output, session, app_state, emit) {
  log_debug_block("COLUMN_MGMT", "Setting up column management")
  # log_debug("Received app_state environment address:", capture.output(print(app_state)), .context = "COLUMN_MGMT")

  # log_debug("Centralized state available for column management", .context = "COLUMN_MGMT")

  # LEGACY: Column choices update observer moved to unified event system
  # Now handled by emit$data_changed() -> update_column_choices_unified()
  # CONVERTED: Direct reactive observation removed in favor of event-driven pattern

  # LEGACY: Auto-detection trigger observer removed - now handled by unified event system
  # Manual auto-detection is triggered via emit$auto_detection_started() in the event system
  # The shiny::observeEvent(app_state$events$data_loaded) -> emit$auto_detection_started() chain
  # handles all auto-detection triggering automatically through the unified event architecture

  # TEST MODE: Now handled by unified event system in utils_event_system.R
  # Legacy test mode observer removed - replaced by emit$test_mode_ready() pattern

  # UNIFIED EVENT SYSTEM: File upload auto-detection triggers are now handled by data_loaded events
  # The emit$data_loaded() -> shiny::observeEvent(app_state$events$data_loaded) -> emit$auto_detection_started()
  # chain handles all auto-detection triggering automatically

  # UNIFIED EVENT SYSTEM: File upload triggers are now handled by data_loaded events
  # The event system automatically handles auto-detection when data is loaded

  # UNIFIED EVENT SYSTEM: Auto-detection is now handled by event listeners in utils_event_system.R
  # The shiny::observeEvent(app_state$events$auto_detection_started) handles all auto-detection logic

  # UNIFIED EVENT SYSTEM: Auto-detection and UI sync are now handled through events
  # emit$auto_detection_started() triggers auto-detection -> emit$ui_sync_needed() -> sync_ui_with_columns_unified()
  # The complete UI sync logic is implemented in utils_event_system.R
  # log_debug("Auto-detection and UI sync handled by unified event system", .context = "AUTODETECT_SETUP")

  # Auto-detekterings knap handler - kører altid når bruger trykker
  shiny::observeEvent(input$auto_detect_columns, {
    # FASE 3: Use event-driven manual trigger for consistency
    safe_operation(
      "Manual auto-detection trigger",
      code = {
        emit$manual_autodetect_button()  # This triggers the event listener with frozen state bypass
      },
      fallback = NULL,
      session = session,
      show_user = TRUE,
      error_type = "processing",
      emit = emit,
      app_state = app_state
    )
  })

  # Kolonnevaliderings output
  output$column_validation_messages <- shiny::renderUI({
    # Use unified state management
    current_data_check <- app_state$data$current_data
    shiny::req(current_data_check)

    if ((is.null(input$x_column) || input$x_column == "") ||
      (is.null(input$y_column) || input$y_column == "")) {
      return(NULL)
    }

    chart_type <- get_qic_chart_type(if (is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type)
    warnings <- character(0)

    # Tjek om Y-kolonne er numerisk
    if (!is.null(input$y_column) && input$y_column != "" && input$y_column %in% names(current_data_check)) {
      y_data <- current_data_check[[input$y_column]]
      if (!is.numeric(y_data)) {
        numeric_test <- parse_danish_number(y_data)
        if (sum(!is.na(numeric_test)) < length(y_data) * 0.8) {
          warnings <- c(warnings, paste("Y-kolonne '", input$y_column, "' er ikke numerisk"))
        }
      }
    }

    # Tjek P/U chart krav
    if (chart_type %in% c("p", "pp", "u", "up")) {
      if (is.null(input$n_column) || input$n_column == "") {
        warnings <- c(warnings, paste("Chart type", chart_type, "kræver en nævner-kolonne (N)"))
      } else if (input$n_column %in% names(current_data_check)) {
        n_data <- current_data_check[[input$n_column]]
        if (!is.numeric(n_data)) {
          numeric_test <- parse_danish_number(n_data)
          if (sum(!is.na(numeric_test)) < length(n_data) * 0.8) {
            warnings <- c(warnings, paste("Nævner-kolonne '", input$n_column, "' er ikke numerisk"))
          }
        }
      }
    }

    # Tjek om samme kolonne er valgt flere gange
    selected_cols <- c(input$x_column, input$y_column, input$n_column)
    selected_cols <- selected_cols[!is.null(selected_cols) & selected_cols != ""]

    if (length(selected_cols) != length(unique(selected_cols))) {
      warnings <- c(warnings, "Samme kolonne kan ikke bruges til flere formål")
    }

    # Vis resultater
    if (length(warnings) > 0) {
      shiny::div(
        class = "alert alert-warning",
        style = "font-size: 0.85rem; padding: 8px; margin: 5px 0;",
        shiny::icon("exclamation-triangle"),
        shiny::strong(" Kolonne advarsler:"),
        shiny::tags$ul(
          style = "margin: 5px 0; padding-left: 20px;",
          lapply(warnings, function(warn) shiny::tags$li(warn))
        )
      )
    } else if (length(selected_cols) >= 2) {
      shiny::div(
        class = "alert alert-success",
        style = "font-size: 0.85rem; padding: 8px; margin: 5px 0;",
        shiny::icon("check-circle"),
        shiny::strong(" Kolonner valideret! "),
        sprintf("Klar til %s chart", chart_type)
      )
    }
  })

  # Redigér kolonnenavne modal
  shiny::observeEvent(input$edit_column_names, {
    show_column_edit_modal(session, app_state)
  })

  # Bekræft kolonnenavn ændringer
  shiny::observeEvent(input$confirm_column_names, {
    handle_column_name_changes(input, session, app_state, emit)
  })

  # Tilføj kolonne
  shiny::observeEvent(input$add_column, {
    show_add_column_modal()
  })

  shiny::observeEvent(input$confirm_add_col, {
    handle_add_column(input, session, app_state, emit)
  })

  # UNIFIED EVENT SYSTEM: No longer returning autodetect_trigger
  # Auto-detection is handled through emit$auto_detection_started() events
  # log_debug("Auto-detection now handled by unified event system", .context = "COLUMN_MGMT")
}

# MODAL FUNKTIONER ============================================================

## Vis kolonne-redigeré modal
# Viser modal dialog for redigering af kolonnenavne
show_column_edit_modal <- function(session, app_state = NULL) {
  # Use unified state management
  current_data_check <- app_state$data$current_data
  shiny::req(current_data_check)

  current_names <- names(current_data_check)

  name_inputs <- lapply(1:length(current_names), function(i) {
    shiny::textInput(
      paste0("col_name_", i),
      paste("Kolonne", i, ":"),
      value = current_names[i],
      placeholder = paste("Navn for kolonne", i)
    )
  })

  shiny::showModal(shiny::modalDialog(
    title = "Redigér kolonnenavne",
    size = "m",
    shiny::div(
      style = "margin-bottom: 15px;",
      shiny::h6("Nuværende kolonnenavne:", style = "font-weight: 500;"),
      shiny::p(paste(current_names, collapse = ", "), style = "color: #666; font-style: italic;")
    ),
    shiny::div(
      style = "max-height: 300px; overflow-y: auto;",
      name_inputs
    ),
    footer = shiny::tagList(
      shiny::modalButton("Annuller"),
      shiny::actionButton("confirm_column_names", "Gem ændringer", class = "btn-primary")
    )
  ))
}

## Håndtér kolonnenavn ændringer
# Behandler ændringer af kolonnenavne fra modal dialog
handle_column_name_changes <- function(input, session, app_state = NULL, emit = NULL) {
  # Use unified state management
  current_data_check <- app_state$data$current_data
  shiny::req(current_data_check)

  current_names <- names(current_data_check)
  new_names <- character(length(current_names))

  for (i in 1:length(current_names)) {
    input_value <- input[[paste0("col_name_", i)]]
    if (!is.null(input_value) && input_value != "") {
      new_names[i] <- trimws(input_value)
    } else {
      new_names[i] <- current_names[i]
    }
  }

  if (any(duplicated(new_names))) {
    shiny::showNotification(
      "Kolonnenavne skal være unikke. Ret duplikater og prøv igen.",
      type = "error",
      duration = 5
    )
    return()
  }

  # Unified state assignment only
  names(app_state$data$current_data) <- new_names

  # Emit event to trigger downstream effects
  if (!is.null(emit)) {
    emit$data_changed()
  }

  shiny::removeModal()

  if (!identical(current_names, new_names)) {
    changed_cols <- which(current_names != new_names)
    change_summary <- paste(
      paste0("'", current_names[changed_cols], "' -> '", new_names[changed_cols], "'"),
      collapse = ", "
    )

    shiny::showNotification(
      paste("Kolonnenavne opdateret:", change_summary),
      type = "message",
      duration = 4
    )
  } else {
    shiny::showNotification("Ingen ændringer i kolonnenavne", type = "message", duration = 2)
  }
}

## Vis tilføj kolonne modal
# Viser modal dialog for tilføjelse af nye kolonner
show_add_column_modal <- function() {
  shiny::showModal(shiny::modalDialog(
    title = "Tilføj ny kolonne",
    shiny::textInput("new_col_name", "Kolonnenavn:", value = "Ny_kolonne"),
    shiny::selectInput("new_col_type", "Type:",
      choices = list("Numerisk" = "numeric", "Tekst" = "text", "Dato" = "date")
    ),
    footer = shiny::tagList(
      shiny::modalButton("Annuller"),
      shiny::actionButton("confirm_add_col", "Tilføj", class = "btn-primary")
    )
  ))
}

## Håndtér tilføjelse af kolonne
# Behandler tilføjelse af nye kolonner til data
handle_add_column <- function(input, session, app_state = NULL, emit = NULL) {
  # Use unified state management
  current_data_check <- app_state$data$current_data
  shiny::req(input$new_col_name, current_data_check)

  new_col_name <- input$new_col_name
  new_col_type <- input$new_col_type

  if (new_col_type == "numeric") {
    # Unified state assignment only
    app_state$data$current_data[[new_col_name]] <- rep(NA_real_, nrow(current_data_check))
  } else if (new_col_type == "date") {
    # Unified state assignment only
    app_state$data$current_data[[new_col_name]] <- rep(NA_character_, nrow(current_data_check))
  } else {
    # Unified state assignment only
    app_state$data$current_data[[new_col_name]] <- rep(NA_character_, nrow(current_data_check))
  }

  # Emit event to trigger downstream effects
  if (!is.null(emit)) {
    emit$data_changed()
  }

  shiny::removeModal()
  shiny::showNotification(paste("Kolonne", new_col_name, "tilføjet"), type = "message")
}

# DATA TABLE FUNKTIONER ======================================================

## Hovedfunktion for datatabel
# Opsætter al server logik relateret til data-tabel håndtering
setup_data_table <- function(input, output, session, app_state, emit) {
  log_debug_block("DATA_TABLE", "Setting up data table with unified state")

  # UNIFIED EVENT SYSTEM: Direct access to app_state data instead of reactive pattern
  # log_debug("Using unified event system for data table setup", .context = "DATA_TABLE")

  # Hovedtabel rendering med excelR
  output$main_data_table <- excelR::renderExcel({
    # UNIFIED EVENT SYSTEM: Direct access to current data
    current_data_check <- app_state$data$current_data
    shiny::req(current_data_check)

  # log_debug("Rendering table with data dimensions:", paste(dim(current_data_check), collapse = "x"), .context = "DATA_TABLE")

    # Inkluder table_version for at tvinge re-render efter gendannelse
    # Use unified state management
    version_trigger <- app_state$session$table_version

    data <- current_data_check

    # Behold logiske kolonner som logiske for excelR checkbox
    # excelR håndterer logiske værdier direkte for checkbox type

    excelR::excelTable(
      data = data,
      columns = data.frame(
        title = names(data),
        type = dplyr::case_when(
          names(data) == "Skift" ~ "checkbox",
          names(data) == "Frys" ~ "radio",
          TRUE ~ "text"
        ),
        width = dplyr::case_when(
          names(data) == "Skift" ~ 60,
          names(data) == "Frys" ~ 60,
          names(data) == "Dato" ~ 100,
          names(data) %in% c("Tæller", "Nævner") ~ 80,
          names(data) == "Kommentar" ~ 300,
          TRUE ~ 120
        ),
        stringsAsFactors = FALSE
      ),
      allowInsertRow = FALSE,
      allowInsertColumn = FALSE,
      allowDeleteRow = FALSE,
      allowDeleteColumn = FALSE,
      allowRenameColumn = FALSE,
      columnSorting = FALSE,
      rowDrag = FALSE,
      columnDrag = FALSE,
      autoFill = TRUE
    )
  })

  # Håndtér excelR tabel ændringer
  shiny::observeEvent(input$main_data_table,
    {
      # Use unified state management
      updating_table_check <- app_state$data$updating_table

      # Use unified state management
      restoring_session_check <- app_state$session$restoring_session

      if (updating_table_check || restoring_session_check) {
        return()
      }

      # Use unified state management
      app_state$data$updating_table <- TRUE
      # Use unified state management
      app_state$data$table_operation_in_progress <- TRUE

      on.exit(
        {
          # Use unified state management
          app_state$data$updating_table <- FALSE
        },
        add = TRUE
      )

      # Trigger event-driven cleanup instead of timing-based
      # Use unified state management
      app_state$data$table_operation_cleanup_needed <- TRUE

      safe_operation(
        operation_name = "ExcelR tabel data opdatering",
        code = {
          new_data <- input$main_data_table

          if (is.null(new_data) || length(new_data) == 0) {
            return()
          }


          # excelR sender data i new_data$data som liste af rækker
          if (!is.null(new_data$data) && length(new_data$data) > 0) {
            # Hent kolonnenavne fra colHeaders
            col_names <- unlist(new_data$colHeaders)

            # Konvertér liste af rækker til data frame
            row_list <- new_data$data

            # Opret tom data frame med korrekt struktur
            new_df <- data.frame(matrix(NA, nrow = length(row_list), ncol = length(col_names)))
            names(new_df) <- col_names

            # Fyld data frame række for række
            for (i in seq_along(row_list)) {
              row_data <- row_list[[i]]
              for (j in seq_along(row_data)) {
                if (j <= length(col_names)) {
                  new_df[i, j] <- row_data[[j]]
                }
              }
            }

            # Konvertér datatyper korrekt
            # Skift kolonne (logisk) - excelR sender checkbox som logisk allerede
            if ("Skift" %in% names(new_df)) {
              # Håndtér både logiske og streng repræsentationer
              skift_values <- new_df$Skift
              if (is.character(skift_values)) {
                new_df$Skift <- skift_values == "TRUE" | skift_values == "true" | skift_values == TRUE
              } else {
                new_df$Skift <- as.logical(skift_values)
              }
            }

            # Frys kolonne (logisk) - excelR sender radio som logisk
            if ("Frys" %in% names(new_df)) {
              # Håndtér både logiske og streng repræsentationer
              frys_values <- new_df$Frys
              if (is.character(frys_values)) {
                new_df$Frys <- frys_values == "TRUE" | frys_values == "true" | frys_values == TRUE
              } else {
                new_df$Frys <- as.logical(frys_values)
              }
            }

            # Numeriske kolonner
            numeric_cols <- c("Tæller", "Nævner")
            for (col in numeric_cols) {
              if (col %in% names(new_df)) {
                new_df[[col]] <- as.numeric(new_df[[col]])
              }
            }

            # Dato kolonne
            if ("Dato" %in% names(new_df)) {
              new_df$Dato <- as.character(new_df$Dato)
            }

            # Karakter kolonner
            if ("Kommentar" %in% names(new_df)) {
              new_df$Kommentar <- as.character(new_df$Kommentar)
            }
          } else {
            return()
          }

          # Dual-state sync for compatibility during migration
          set_current_data(app_state, new_df)

          # Emit event to trigger downstream effects
          emit$data_changed()

          shiny::showNotification("Tabel opdateret", type = "message", duration = 2)
        },
        error_type = "processing",
        emit = emit,
        app_state = app_state,
        show_user = TRUE,
        session = session
      )
    },
    ignoreInit = TRUE
  )

  # Tilføj række
  shiny::observeEvent(input$add_row, {
    # UNIFIED EVENT SYSTEM: Direct access to current data
    current_data_check <- app_state$data$current_data
    shiny::req(current_data_check)

    # Sæt vedvarende flag for at forhindre auto-save interferens
    # Use unified state management
    app_state$data$table_operation_in_progress <- TRUE

    new_row <- current_data_check[1, ]
    new_row[1, ] <- NA

    # Dual-state sync for compatibility during migration
    current_data <- get_current_data(app_state)
    set_current_data(app_state, rbind(current_data, new_row))

    # Emit event to trigger downstream effects
    emit$data_changed()

    shiny::showNotification("Ny række tilføjet", type = "message")

    # Trigger event-driven cleanup instead of timing-based
    # Use unified state management
    app_state$data$table_operation_cleanup_needed <- TRUE
  })

  # UNIFIED STATE: Table reset functionality moved to utils_server_management.R
  # Uses emit$session_reset() events and unified app_state management
}
