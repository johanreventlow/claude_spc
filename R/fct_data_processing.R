# server_column_management.R
# Server logik for kolonnehåndtering inklusive auto-detektion og validering

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
#' PHASE 4 compatibility: Funktionen detekterer automatisk om centraliseret
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
#' @seealso \code{\link{detect_columns_name_only}}, \code{\link{ensure_standard_columns}}
setup_column_management <- function(input, output, session, values, app_state = NULL) {
  cat("DEBUG: [COLUMN_MGMT] ===========================================\n")
  cat("DEBUG: [COLUMN_MGMT] Setting up column management\n")
  cat("DEBUG: [COLUMN_MGMT] Received app_state environment address:", capture.output(print(app_state)), "\n")

  # PHASE 4: Centralized state is now always available
  cat("DEBUG: [PHASE4] Centralized state available for column management\n")

  # Opdater kolonnevalg når data ændres
  observe({
    log_debug("Column update observer triggered", "COLUMN_MGMT")

    # PHASE 4: Use unified state management
    updating_table_check <- app_state$data$updating_table

    if (updating_table_check) {
      log_debug("Skipping - table update in progress", "COLUMN_MGMT")
      return()
    }

    # Skip hvis auto-detect er i gang for at undgå at overskrive auto-detect resultater
    # PHASE 4: Use unified state management
    # PHASE 4: Use unified state management for auto-detect status check
    auto_detect_active <- if (!is.null(app_state)) app_state$columns$auto_detect$in_progress else FALSE

    if (auto_detect_active) {
      log_debug("Skipping - auto-detect in progress", "COLUMN_MGMT")
      return()
    }

    # Skip hvis UI sync er pending for at undgå race condition
    if (!is.null(app_state$columns$ui_sync$needed)) {
      log_debug("Skipping - UI sync pending, would override auto-detect results", "COLUMN_MGMT")
      return()
    }

    # Skip hvis UI sync netop er udført (3 sekunder cooling-off periode)
    if (!is.null(app_state$columns$ui_sync$last_sync_time)) {
      time_since_sync <- as.numeric(difftime(Sys.time(), app_state$columns$ui_sync$last_sync_time, units = "secs"))
      if (time_since_sync < 3.0) {
        cat("DEBUG: [COLUMN_MGMT] Skipping - UI sync completed", round(time_since_sync, 2), "seconds ago, cooling off\n")
        return()
      }
    }

    # PHASE 4: Use unified state management
    current_data_check <- app_state$data$current_data
    req(current_data_check)
    cat("DEBUG: [COLUMN_MGMT] Data available - processing column choices\n")

    data <- current_data_check
    all_cols <- names(data)
    cat("DEBUG: [COLUMN_MGMT] Available columns:", paste(all_cols, collapse = ", "), "\n")

    if (length(all_cols) > 0) {
      cat("DEBUG: [COLUMN_MGMT] Creating column choices for", length(all_cols), "columns\n")

      # Kun "Vælg kolonne..." som første option - selectizeInput kan rydde selv
      col_choices <- setNames(
        c("", all_cols),
        c("Vælg kolonne...", all_cols)
      )

      isolate({
        # Bevar nuværende valgte værdier når choices opdateres
        current_x <- input$x_column
        current_y <- input$y_column
        current_n <- input$n_column
        current_skift <- input$skift_column
        current_frys <- input$frys_column
        current_kommentar <- input$kommentar_column

        cat("DEBUG: [COLUMN_MGMT] Current selections before update:\n")
        cat("DEBUG: [COLUMN_MGMT] - X column:", if(is.null(current_x)) "NULL" else current_x, "\n")
        cat("DEBUG: [COLUMN_MGMT] - Y column:", if(is.null(current_y)) "NULL" else current_y, "\n")
        cat("DEBUG: [COLUMN_MGMT] - N column:", if(is.null(current_n)) "NULL" else current_n, "\n")

        # Isolate UI updates to prevent reactive loops during choices update
        updateSelectizeInput(session, "x_column", choices = col_choices, selected = current_x)
        updateSelectizeInput(session, "y_column", choices = col_choices, selected = current_y)
        updateSelectizeInput(session, "n_column", choices = col_choices, selected = current_n)
        updateSelectizeInput(session, "skift_column", choices = col_choices, selected = current_skift)
        updateSelectizeInput(session, "frys_column", choices = col_choices, selected = current_frys)
        updateSelectizeInput(session, "kommentar_column", choices = col_choices, selected = current_kommentar)

        cat("DEBUG: [COLUMN_MGMT] ✅ All selectize inputs updated with current selections\n")
      })
    } else {
      cat("DEBUG: [COLUMN_MGMT] ⚠️ No columns available\n")
    }
  }, priority = OBSERVER_PRIORITIES$DATA_PROCESSING)

  # Auto-detekterings trigger flag - bruges kun til manuel triggering (ikke test mode)
  observeEvent({
    # PHASE 4: Use unified state management
    app_state$data$current_data
  },
    {
      cat("DEBUG: [AUTO_DETECT] Manual auto-detect observer triggered\n")

      # Skip automatisk auto-detect hvis vi allerede har været igennem det i test mode
      # PHASE 4B: Use unified state management only
      auto_detect_completed <- app_state$columns$auto_detect$completed %||% FALSE

      if (auto_detect_completed) {
        cat("DEBUG: [AUTO_DETECT] Skipping - initial auto-detect already completed\n")
        return()
      }

      # PHASE 4: Use unified state management
      current_data_check <- app_state$data$current_data
      data_available <- !is.null(current_data_check)
      x_empty <- is.null(input$x_column) || input$x_column == ""
      y_empty <- is.null(input$y_column) || input$y_column == ""

      cat("DEBUG: [AUTO_DETECT] Conditions check:\n")
      cat("DEBUG: [AUTO_DETECT] - Data available:", data_available, "\n")
      cat("DEBUG: [AUTO_DETECT] - X column empty:", x_empty, "\n")
      cat("DEBUG: [AUTO_DETECT] - Y column empty:", y_empty, "\n")

      if (data_available && x_empty && y_empty) {
        cat("DEBUG: [AUTO_DETECT] ✅ Triggering manual auto-detect\n")
        timestamp <- Sys.time()

        # PHASE 4: Use unified state management
        app_state$columns$auto_detect$trigger <- timestamp
        cat("DEBUG: [PHASE4] Synced auto_detect_trigger to centralized state\n")
      } else {
        cat("DEBUG: [AUTO_DETECT] ❌ Conditions not met for auto-detect\n")
      }
    },
    ignoreInit = TRUE
  )

  # Test mode auto-detect trigger (event-driven instead of later::later)
  # PHASE 4: Unified state observer for test mode auto-detect trigger
  if (!is.null(app_state)) {
    cat("DEBUG: [TEST_MODE] Creating test mode auto-detect observer\n")

    # Check if flag is already set (race condition fix)
    if (!is.null(app_state$test_mode$auto_detect_ready) && app_state$test_mode$auto_detect_ready != FALSE) {
      cat("DEBUG: [TEST_MODE] Flag already set, triggering autodetection immediately\n")
      timestamp <- Sys.time()
      app_state$columns$auto_detect$trigger <- timestamp
      cat("DEBUG: [PHASE4] Immediate auto_detect_trigger set in centralized state\n")
    }

    observeEvent(app_state$test_mode$auto_detect_ready,
      {
        cat("DEBUG: [TEST_MODE] Test mode auto-detect observer triggered\n")
        cat("DEBUG: [TEST_MODE] auto_detect_ready value:", if(is.null(app_state$test_mode$auto_detect_ready)) "NULL" else as.character(app_state$test_mode$auto_detect_ready), "\n")

        auto_detect_ready_value <- app_state$test_mode$auto_detect_ready
        if (is.null(auto_detect_ready_value)) {
          cat("DEBUG: [TEST_MODE] ❌ auto_detect_ready is NULL, skipping trigger\n")
          return()
        }

        cat("DEBUG: [TEST_MODE] ✅ Event-driven auto-detect trigger fired!\n")
        timestamp <- Sys.time()

        # PHASE 4: Use unified state management
        app_state$columns$auto_detect$trigger <- timestamp
        cat("DEBUG: [PHASE4] Synced test mode auto_detect_trigger to centralized state:", as.character(timestamp), "\n")
      },
      ignoreInit = FALSE, ignoreNULL = TRUE,
      priority = OBSERVER_PRIORITIES$STATE_MANAGEMENT
    )
  }

  # REACTIVE FIX: Watch for trigger_needed and fire direct reactiveVal
  # Use a helper reactiveVal to bridge the nested state to direct trigger
  trigger_needed_watcher <- reactiveVal(FALSE)

  # Update the watcher when data changes
  observe({
    needed <- if (!is.null(app_state$columns$auto_detect$trigger_needed)) {
      app_state$columns$auto_detect$trigger_needed
    } else {
      FALSE
    }
    if (needed != trigger_needed_watcher()) {
      cat("DEBUG: [TRIGGER_WATCHER] trigger_needed changed to:", needed, "\n")
      trigger_needed_watcher(needed)
    }
  })

  # File upload auto-detect trigger conversion via watcher
  observeEvent(trigger_needed_watcher(),
    {
      cat("DEBUG: [FILE_UPLOAD_TRIGGER] File upload auto-detect observer triggered\n")
      req(trigger_needed_watcher() == TRUE)
      cat("DEBUG: [FILE_UPLOAD_TRIGGER] ✅ Converting trigger_needed to direct autodetect_trigger!\n")
      timestamp <- Sys.time()

      # Fire the direct reactiveVal trigger
      autodetect_trigger(timestamp)
      cat("DEBUG: [FILE_UPLOAD_TRIGGER] ✅ Direct autodetect_trigger fired!\n")

      # Reset the trigger_needed flag
      app_state$columns$auto_detect$trigger_needed <- FALSE
      trigger_needed_watcher(FALSE)
      cat("DEBUG: [FILE_UPLOAD_TRIGGER] Reset trigger_needed flag\n")
    },
    ignoreInit = TRUE,
    priority = OBSERVER_PRIORITIES$STATE_MANAGEMENT
  )

  # REACTIVE FIX: Direct autodetection trigger execution
  observeEvent(autodetect_trigger(),
    {
      cat("DEBUG: [AUTO_DETECT_EXEC] Centralized auto-detect execution started\n")
      cat("DEBUG: [AUTO_DETECT_EXEC] Setting auto_detect_in_progress = TRUE\n")

      # PHASE 4: Use unified state management
      app_state$columns$auto_detect$in_progress <- TRUE
      cat("DEBUG: [PHASE4] Synced auto_detect_in_progress TRUE to centralized state\n")

      cat("DEBUG: [AUTO_DETECT_EXEC] Calling auto_detect_and_update_columns...\n")
      # PHASE 4: Pass centralized state to auto-detect function with UI sync trigger
      auto_detect_and_update_columns(input, session, values, app_state, ui_sync_trigger)

      cat("DEBUG: [AUTO_DETECT_EXEC] Setting initial_auto_detect_completed = TRUE\n")
      # PHASE 4: Use unified state management
      app_state$columns$auto_detect$completed <- TRUE
      cat("DEBUG: [PHASE4] Synced initial_auto_detect_completed to centralized state\n")

      cat("DEBUG: [AUTO_DETECT_EXEC] ✅ Centralized auto-detect execution completed\n")
    },
    ignoreInit = TRUE,
    priority = OBSERVER_PRIORITIES$AUTO_DETECT
  )

  # PHASE 4: Legacy auto-detect observer removed - now using direct reactiveVal triggers
  # See autodetect_trigger implementation for new direct callback pattern

  # REACTIVE FIX: Create dedicated reactiveVal for UI sync trigger
  cat("DEBUG: [UI_SYNC_SETUP] Creating dedicated UI sync trigger reactiveVal\n")
  ui_sync_trigger <- reactiveVal(NULL)

  # REACTIVE FIX: Create dedicated reactiveVal for autodetection trigger
  cat("DEBUG: [AUTODETECT_SETUP] Creating dedicated autodetection trigger reactiveVal\n")
  autodetect_trigger <- reactiveVal(NULL)

  # CRITICAL FIX: Bridge observer between app_state trigger and reactiveVal
  # This was the missing link that prevented auto-detection at app startup
  cat("DEBUG: [BRIDGE_SETUP] Creating bridge observer for app_state auto_detect trigger\n")
  observe({
    cat("DEBUG: [BRIDGE] Bridge observer triggered\n")
    if (!is.null(app_state)) {
      trigger_value <- app_state$columns$auto_detect$trigger
      cat("DEBUG: [BRIDGE] Checking trigger_value:", if(is.null(trigger_value)) "NULL" else as.character(trigger_value), "\n")
      if (!is.null(trigger_value)) {
        cat("DEBUG: [BRIDGE] app_state trigger detected:", as.character(trigger_value), "\n")
        cat("DEBUG: [BRIDGE] Firing autodetect_trigger reactiveVal\n")
        autodetect_trigger(trigger_value)
        cat("DEBUG: [BRIDGE] ✅ reactiveVal triggered successfully\n")
      } else {
        cat("DEBUG: [BRIDGE] No trigger value available yet\n")
      }
    } else {
      cat("DEBUG: [BRIDGE] app_state is NULL\n")
    }
  })

  # Reaktiv UI sync observer med direct reactiveVal trigger
  cat("DEBUG: [UI_SYNC_SETUP] Setting up UI sync observer for ui_sync_trigger\n")
  observeEvent(ui_sync_trigger(),
    {
      cat("DEBUG: [UI_SYNC] =============================================\n")
      cat("DEBUG: [UI_SYNC] UI sync observer triggered - CRITICAL for input field updates\n")
      cat("DEBUG: [UI_SYNC] 🔄 REACTIVE FIX: Direct trigger fired successfully!\n")

      sync_data <- ui_sync_trigger()
      req(sync_data)

      cat("DEBUG: [UI_SYNC] Direct sync data received from reactiveVal trigger\n")
      cat("DEBUG: [UI_SYNC] Sync data received:\n")
      cat("DEBUG: [UI_SYNC] - X column suggestion:", if(is.null(sync_data$x_col)) "NULL" else sync_data$x_col, "\n")
      cat("DEBUG: [UI_SYNC] - Y column suggestion:", if(is.null(sync_data$taeller_col)) "NULL" else sync_data$taeller_col, "\n")
      cat("DEBUG: [UI_SYNC] - N column suggestion:", if(is.null(sync_data$naevner_col)) "NULL" else sync_data$naevner_col, "\n")

      cat("DEBUG: [UI_SYNC] 🔄 Auto-detect triggered selectize opdateringer\n")

      # Opdater alle selectize inputs med de detekterede kolonner
      if (!is.null(sync_data$col_choices)) {
        cat("DEBUG: [UI_SYNC] Column choices available - updating selectize inputs\n")
        cat("DEBUG: [UI_SYNC] Col choices length:", length(sync_data$col_choices), "\n")

        # Isolate all UI updates to prevent reactive loops
        isolate({
          # X-kolonne (dato/tid)
          x_selection <- sync_data$x_col %||% ""
          cat("DEBUG: [UI_SYNC] Updating X column to:", x_selection, "\n")
          updateSelectizeInput(session, "x_column",
                             choices = sync_data$col_choices,
                             selected = x_selection)

          # Y-kolonne (tæller/værdi)
          y_selection <- sync_data$taeller_col %||% ""
          cat("DEBUG: [UI_SYNC] Updating Y column to:", y_selection, "\n")
          updateSelectizeInput(session, "y_column",
                             choices = sync_data$col_choices,
                             selected = y_selection)

          # N-kolonne (nævner)
          n_selection <- sync_data$naevner_col %||% ""
          cat("DEBUG: [UI_SYNC] Updating N column to:", n_selection, "\n")
          updateSelectizeInput(session, "n_column",
                             choices = sync_data$col_choices,
                             selected = n_selection)

          # Skift kolonne
          updateSelectizeInput(session, "skift_column",
                             choices = sync_data$col_choices,
                             selected = sync_data$skift_col %||% "")

          # Frys kolonne
          updateSelectizeInput(session, "frys_column",
                             choices = sync_data$col_choices,
                             selected = sync_data$frys_col %||% "")

          # Kommentar kolonne
          updateSelectizeInput(session, "kommentar_column",
                             choices = sync_data$col_choices,
                             selected = sync_data$kommentar_col %||% "")
        })
      }

      # Ryd sync request og sæt timestamp for at forhindre immediate column mgmt override
      # PHASE 4: Use unified state management
      app_state$columns$ui_sync$needed <- NULL
      # NOTE: Timestamp allerede sat i auto-detect for at forhindre race condition
      cat("DEBUG: [UI_SYNC] ✅ UI sync completed, timestamp was set earlier to prevent race condition\n")
    },
    ignoreInit = TRUE, ignoreNULL = TRUE,
    priority = OBSERVER_PRIORITIES$UI_SYNC
  )

  # Auto-detekterings knap handler - kører altid når bruger trykker
  observeEvent(input$auto_detect_columns, {
    # PHASE 4: Use unified state management
    app_state$columns$auto_detect$in_progress <- TRUE

    # PHASE 4: Pass centralized state to auto-detect function
    auto_detect_and_update_columns(input, session, values, app_state, ui_sync_trigger)

    # PHASE 4: Use unified state management
    app_state$columns$auto_detect$completed <- TRUE

    # PHASE 4: Use unified state management
    app_state$columns$auto_detect$in_progress <- FALSE
  })

  # Kolonnevaliderings output
  output$column_validation_messages <- renderUI({
    # PHASE 4: Use unified state management
    current_data_check <- app_state$data$current_data
    req(current_data_check)

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
      div(
        class = "alert alert-warning",
        style = "font-size: 0.85rem; padding: 8px; margin: 5px 0;",
        icon("exclamation-triangle"),
        strong(" Kolonne advarsler:"),
        tags$ul(
          style = "margin: 5px 0; padding-left: 20px;",
          lapply(warnings, function(warn) tags$li(warn))
        )
      )
    } else if (length(selected_cols) >= 2) {
      div(
        class = "alert alert-success",
        style = "font-size: 0.85rem; padding: 8px; margin: 5px 0;",
        icon("check-circle"),
        strong(" Kolonner valideret! "),
        sprintf("Klar til %s chart", chart_type)
      )
    }
  })

  # Redigér kolonnenavne modal
  observeEvent(input$edit_column_names, {
    show_column_edit_modal(session, values, app_state)
  })

  # Bekræft kolonnenavn ændringer
  observeEvent(input$confirm_column_names, {
    handle_column_name_changes(input, session, values, app_state)
  })

  # Tilføj kolonne
  observeEvent(input$add_column, {
    show_add_column_modal()
  })

  observeEvent(input$confirm_add_col, {
    handle_add_column(input, session, values, app_state)
  })

  # Return autodetect_trigger so it can be used by file upload
  cat("DEBUG: [COLUMN_MGMT] Returning autodetect_trigger for external use\n")
  return(autodetect_trigger)
}

# AUTO-DETEKTION FUNKTIONER ==================================================

#' Auto-detekter SPC kolonner baseret på kolonnenavne
#'
#' Intelligent matching af kolonnenavne til standard SPC kolonner, designet
#' til tomme datasæt hvor data-baseret auto-detektion ikke er mulig.
#' Bruger case-insensitive pattern matching og fallback strategier.
#'
#' @param col_names Character vector med tilgængelige kolonnenavne
#' @param input Shiny input object (kan være NULL for pure name matching)
#' @param session Shiny session object for UI opdateringer
#' @param values Reactive values list med app state (legacy system)
#' @param app_state List med centraliseret app state (Phase 4 system), optional
#'
#' @details
#' Matching prioriteter:
#' \enumerate{
#'   \item Eksakte matches (case-insensitive)
#'   \item Substring matches med høj specificitet
#'   \item Fuzzy matching for almindelige navne
#'   \item Fallback til første tilgængelige kolonne
#' }
#'
#' Standard SPC kolonner der matches:
#' \itemize{
#'   \item X-akse: "Dato", "Date", "Time", "Periode"
#'   \item Tæller: "Tæller", "Count", "Events", "Numerator"
#'   \item Nævner: "Nævner", "Total", "Denominator", "N"
#'   \item Skift: "Skift", "Shift", "Phase"
#'   \item Frys: "Frys", "Freeze", "Frost"
#'   \item Kommentar: "Kommentar", "Comment", "Note"
#' }
#'
#' @return List med detekterede kolonner:
#' \describe{
#'   \item{x_col}{X-akse kolonne navn}
#'   \item{taeller_col}{Tæller kolonne navn}
#'   \item{naevner_col}{Nævner kolonne navn}
#'   \item{skift_col}{Skift kolonne navn}
#'   \item{frys_col}{Frys kolonne navn}
#'   \item{kommentar_col}{Kommentar kolonne navn}
#'   \item{ui_sync_data}{Data til UI synkronisering}
#' }
#'
#' @examples
#' \dontrun{
#' # Standard SPC kolonner
#' cols <- c("Dato", "Tæller", "Nævner", "Skift", "Frys", "Kommentar")
#' result <- detect_columns_name_only(cols, NULL, session, values)
#'
#' # Engelske kolonner
#' cols_en <- c("Date", "Count", "Total", "Phase", "Comment")
#' result <- detect_columns_name_only(cols_en, NULL, session, values)
#' }
#'
#' @seealso \code{\link{setup_column_management}}, \code{\link{ensure_standard_columns}}
detect_columns_name_only <- function(col_names, input, session, values, app_state = NULL, ui_sync_trigger = NULL) {
  cat("DEBUG: [AUTO_DETECT_FUNC] ========================================\n")
  cat("DEBUG: [AUTO_DETECT_FUNC] Starting name-only detection\n")

  # Initialiser resultater
  x_col <- NULL
  taeller_col <- NULL
  naevner_col <- NULL
  skift_col <- NULL
  frys_col <- NULL
  kommentar_col <- NULL

  col_names_lower <- tolower(col_names)

  # X-kolonne (dato/tid detection)
  dato_idx <- which(grepl("dato|date|tid|time|år|year|måned|month|uge|week|dag|day", col_names_lower, ignore.case = TRUE))
  if (length(dato_idx) > 0) {
    x_col <- col_names[dato_idx[1]]
    cat("DEBUG: [AUTO_DETECT_FUNC] Name-only X column:", x_col, "\n")
  } else if (length(col_names) > 0) {
    x_col <- col_names[1]
    cat("DEBUG: [AUTO_DETECT_FUNC] Name-only X column (fallback):", x_col, "\n")
  }

  # Y-kolonne (taeller detection)
  taeller_idx <- which(grepl("t.ller|tael|num|count", col_names_lower, ignore.case = TRUE))
  if (length(taeller_idx) > 0) {
    taeller_col <- col_names[taeller_idx[1]]
    cat("DEBUG: [AUTO_DETECT_FUNC] Name-only Y column:", taeller_col, "\n")
  }

  # N-kolonne (naevner detection)
  naevner_idx <- which(grepl("n.vner|naev|denom|total", col_names_lower, ignore.case = TRUE))
  if (length(naevner_idx) > 0) {
    naevner_col <- col_names[naevner_idx[1]]
    cat("DEBUG: [AUTO_DETECT_FUNC] Name-only N column:", naevner_col, "\n")
  }

  # Skift-kolonne (eksakt match)
  skift_idx <- which(grepl("^skift$", col_names_lower, ignore.case = TRUE))
  if (length(skift_idx) > 0) {
    skift_col <- col_names[skift_idx[1]]
    cat("DEBUG: [AUTO_DETECT_FUNC] Name-only Skift column:", skift_col, "\n")
  }

  # Frys-kolonne (eksakt match)
  frys_idx <- which(grepl("^frys$", col_names_lower, ignore.case = TRUE))
  if (length(frys_idx) > 0) {
    frys_col <- col_names[frys_idx[1]]
    cat("DEBUG: [AUTO_DETECT_FUNC] Name-only Frys column:", frys_col, "\n")
  }

  # Kommentar-kolonne
  kommentar_idx <- which(grepl("kommentar|comment|note|bemærk", col_names_lower, ignore.case = TRUE))
  if (length(kommentar_idx) > 0) {
    kommentar_col <- col_names[kommentar_idx[1]]
    cat("DEBUG: [AUTO_DETECT_FUNC] Name-only Comment column:", kommentar_col, "\n")
  }

  cat("DEBUG: [AUTO_DETECT_FUNC] Name-only mode results:\n")
  cat("DEBUG: [AUTO_DETECT_FUNC] - X=", ifelse(is.null(x_col), "NULL", x_col), "\n")
  cat("DEBUG: [AUTO_DETECT_FUNC] - Y=", ifelse(is.null(taeller_col), "NULL", taeller_col), "\n")
  cat("DEBUG: [AUTO_DETECT_FUNC] - N=", ifelse(is.null(naevner_col), "NULL", naevner_col), "\n")
  cat("DEBUG: [AUTO_DETECT_FUNC] - Skift=", ifelse(is.null(skift_col), "NULL", skift_col), "\n")
  cat("DEBUG: [AUTO_DETECT_FUNC] - Frys=", ifelse(is.null(frys_col), "NULL", frys_col), "\n")

  # Opdater UI ligesom normal auto-detection
  use_centralized_state <- !is.null(app_state)

  # Gem resultater
  auto_detected_columns <- list(
    x_col = x_col,
    taeller_col = taeller_col,
    naevner_col = naevner_col,
    skift_col = skift_col,
    frys_col = frys_col,
    kommentar_col = kommentar_col
  )

  if (use_centralized_state && !is.null(app_state)) {
    # FIX: Use correct state path that visualization expects
    app_state$columns$auto_detected_columns <- auto_detected_columns
    cat("DEBUG: [PHASE4] Synced name-only auto_detected_columns to centralized state (FIXED PATH)\n")
  } else {
    # PHASE 4: Use unified state management - this assignment is now handled in centralized state
  }

  # Trigger UI sync
  col_choices <- setNames(col_names, col_names)
  col_choices <- c("Vælg kolonne" = "", col_choices)

  ui_sync_data <- list(
    x_col = x_col,
    taeller_col = taeller_col,
    naevner_col = naevner_col,
    skift_col = skift_col,
    frys_col = frys_col,
    kommentar_col = kommentar_col,
    col_choices = col_choices,
    timestamp = Sys.time()
  )

  if (use_centralized_state && !is.null(app_state)) {
    cat("DEBUG: [UI_SYNC_TRIGGER] Setting app_state$columns$ui_sync$needed\n")
    cat("DEBUG: [UI_SYNC_TRIGGER] Before: app_state$columns$ui_sync$needed is", if(is.null(app_state$columns$ui_sync$needed)) "NULL" else "SET", "\n")
    app_state$columns$ui_sync$needed <- ui_sync_data
    cat("DEBUG: [UI_SYNC_TRIGGER] After: app_state$columns$ui_sync$needed is", if(is.null(app_state$columns$ui_sync$needed)) "NULL" else "SET", "\n")
    cat("DEBUG: [UI_SYNC_TRIGGER] UI sync data keys:", paste(names(ui_sync_data), collapse = ", "), "\n")
    cat("DEBUG: [PHASE4] Synced name-only UI sync data to centralized state\n")

    # CRITICAL: Set debounce timestamp BEFORE triggering UI sync to prevent race condition
    app_state$columns$ui_sync$last_sync_time <- Sys.time()
    cat("DEBUG: [UI_SYNC_TRIGGER] ⏰ Debounce timestamp set BEFORE UI sync (name-only mode) to prevent column mgmt override\n")

    # REACTIVE FIX: Trigger direct reactiveVal if available
    if (!is.null(ui_sync_trigger)) {
      cat("DEBUG: [UI_SYNC_TRIGGER] 🔄 Triggering direct reactiveVal for UI sync\n")
      ui_sync_trigger(ui_sync_data)
      cat("DEBUG: [UI_SYNC_TRIGGER] ✅ Direct trigger fired successfully\n")
    } else {
      cat("DEBUG: [UI_SYNC_TRIGGER] ⚠️ No ui_sync_trigger available - relying on nested observeEvent\n")
    }
  } else {
    # PHASE 4: Legacy path removed - now using unified state management only
    cat("DEBUG: [UI_SYNC_TRIGGER] Legacy values$ui_sync_needed assignment skipped (unified state only)\n")
  }

  cat("DEBUG: [AUTO_DETECT_FUNC] ✅ Name-only detection completed\n")
  cat("DEBUG: [AUTO_DETECT_FUNC] ========================================\n")

  return(auto_detected_columns)
}

## Auto-detekter og opdater kolonner
# Automatisk detektion af kolonnetyper baseret på data indhold
auto_detect_and_update_columns <- function(input, session, values, app_state = NULL, ui_sync_trigger = NULL) {
  # Get session ID for debugging
  session_id <- if (!is.null(session)) session$token else NULL

  # Start auto-detect workflow tracing
  autodetect_tracer <- debug_workflow_tracer("auto_detect_columns", app_state, session_id)
  autodetect_tracer$step("auto_detect_initiated")

  cat("DEBUG: [AUTO_DETECT_FUNC] ========================================\n")
  cat("DEBUG: [AUTO_DETECT_FUNC] Starting auto_detect_and_update_columns\n")
  cat("DEBUG: [AUTO_DETECT_FUNC] app_state hash:", if(!is.null(app_state)) digest::digest(app_state$data$current_data) else "NULL", "\n")

  debug_log("Auto-detect columns process started", "AUTO_DETECT_FLOW", level = "INFO",
            context = list(
              use_centralized_state = !is.null(app_state)
            ),
            session_id = session_id)

  # PHASE 4: Centralized state availability check
  use_centralized_state <- !is.null(app_state)
  if (use_centralized_state) {
    cat("DEBUG: [PHASE4] Centralized state available for auto-detect function\n")
  }

  autodetect_tracer$step("data_source_validation")

  # PHASE 4: Use unified state management for current_data access
  current_data_check <- if (!is.null(app_state)) {
    app_state$data$current_data
  } else {
    NULL
  }
  req(current_data_check)

  data <- current_data_check

  debug_log("Data source validated for auto-detect", "AUTO_DETECT_FLOW", level = "TRACE",
            context = list(
              data_source = if (!is.null(app_state)) "app_state" else "values",
              data_available = !is.null(current_data_check)
            ),
            session_id = session_id)

  # DEBUGGING: Verificer hvilke data auto-detection faktisk læser
  cat("DEBUG: [AUTO_DETECT_FUNC] DATA SOURCE VERIFICATION:\n")
  cat("DEBUG: [AUTO_DETECT_FUNC] - use_centralized_state:", use_centralized_state, "\n")
  if (!is.null(app_state)) {
    cat("DEBUG: [AUTO_DETECT_FUNC] - Reading from app_state$data$current_data\n")
    if (!is.null(app_state$data$current_data)) {
      cat("DEBUG: [AUTO_DETECT_FUNC] - app_state data dims:", dim(app_state$data$current_data), "\n")
      cat("DEBUG: [AUTO_DETECT_FUNC] - app_state data cols:", paste(names(app_state$data$current_data), collapse = ", "), "\n")
    }
  } else {
    # PHASE 4: Legacy values$ access removed - now using unified state only
    cat("DEBUG: [AUTO_DETECT_FUNC] - Legacy values$current_data access skipped (unified state only)\n")
  }

  autodetect_tracer$step("data_inspection_started")

  # Tjek for tomme datasæt - skift til name-only mode
  if (is.null(data) || ncol(data) == 0) {
    cat("DEBUG: [AUTO_DETECT_FUNC] ⚠️ Tomt datasæt (NULL eller ingen kolonner) - afbryder auto-detection\n")
    debug_log("Auto-detect aborted - empty dataset", "AUTO_DETECT_FLOW", level = "WARN",
              context = list(reason = "null_or_no_columns"),
              session_id = session_id)
    autodetect_tracer$complete("auto_detect_aborted_empty_dataset")
    return(NULL)
  }

  # Name-only mode for datasæt med 0 rækker men med kolonneoverskrifter
  name_only_mode <- nrow(data) == 0
  if (name_only_mode) {
    cat("DEBUG: [AUTO_DETECT_FUNC] Tomt datasæt (0 rækker) - skifter til name-only detection\n")
    debug_log("Switching to name-only detection mode", "AUTO_DETECT_FLOW", level = "INFO",
              context = list(rows = 0, columns = ncol(data)),
              session_id = session_id)
    autodetect_tracer$step("name_only_mode_activated")
  }

  col_names <- names(data)

  cat("DEBUG: [AUTO_DETECT_FUNC] Data dimensions:", dim(data), "\n")
  cat("DEBUG: [AUTO_DETECT_FUNC] Column names:", paste(col_names, collapse = ", "), "\n")

  debug_log("Data analysis for auto-detect", "AUTO_DETECT_FLOW", level = "TRACE",
            context = list(
              rows = nrow(data),
              columns = ncol(data),
              column_names = col_names,
              name_only_mode = name_only_mode
            ),
            session_id = session_id)

  # NAME-ONLY DETECTION for tomme datasaet
  if (name_only_mode) {
    autodetect_tracer$step("executing_name_only_detection")
    result <- detect_columns_name_only(col_names, input, session, values, app_state, ui_sync_trigger)
    autodetect_tracer$complete("auto_detect_name_only_complete")
    return(result)
  }

  autodetect_tracer$step("full_content_analysis_started")

  # Forbedret detektering af potentielle dato-kolonner
  # Evaluer ALLE kolonner og find den bedste dato-kandidat

  cat("DEBUG: [AUTO_DETECT_FUNC] Starting date candidate analysis...\n")
  date_candidates <- list()

  for (col_name in col_names) {
    cat("DEBUG: [AUTO_DETECT_FUNC] Analyzing column:", col_name, "\n")
    col_data <- data[[col_name]]

    # Skip ikke-dato kolonner baseret på navn
    if (grepl("^(nr|id|count|antal|total|sum)$", col_name, ignore.case = TRUE)) {
      cat("DEBUG: [AUTO_DETECT_FUNC] Skipping", col_name, "- excluded by name pattern\n")
      next
    }

    # Skip kolonner der kun indeholder NA værdier
    if (all(is.na(col_data)) || length(col_data) == 0) {
      cat("DEBUG: [AUTO_DETECT_FUNC] Skipping", col_name, "- all NA or empty\n")
      next
    }

    candidate <- list(
      name = col_name,
      score = 0,
      type = "unknown",
      success_rate = 0,
      reason = ""
    )

    # HØJESTE PRIORITET: Allerede parsede dato-objekter
    if (inherits(col_data, c("Date", "POSIXct", "POSIXt"))) {
      cat("DEBUG: [AUTO_DETECT_FUNC] ⭐", col_name, "- Already parsed date object (score: 100)\n")
      candidate$score <- 100
      candidate$type <- "parsed_date"
      candidate$success_rate <- 1.0
      candidate$reason <- paste("Allerede", class(col_data)[1], "format")
      date_candidates[[col_name]] <- candidate
      next
    }

    # MELLEMPRIORITY: Navn-baseret detektion
    if (grepl("dato|date|tid|time|år|year|måned|month|uge|week|dag|day", col_name, ignore.case = TRUE)) {
      candidate$score <- candidate$score + 50
      candidate$reason <- paste(candidate$reason, "Navn-match")
    }

    # TEST: Parsing kvalitet for character/factor data
    if (is.character(col_data) || is.factor(col_data)) {
      char_data <- as.character(col_data)[!is.na(col_data)]

      if (length(char_data) > 0) {
        test_sample <- char_data[1:min(5, length(char_data))]

        # Test danske formater først med ny safe parsing
        danish_result <- safe_date_parse(test_sample,
          locale = "da_DK.UTF-8",
          operation_name = paste("dansk parsing for", col_name)
        )

        if (danish_result$success) {
          candidate$score <- candidate$score + (danish_result$success_rate * 40)
          candidate$success_rate <- danish_result$success_rate
          candidate$type <- "danish_date"
          candidate$reason <- paste(candidate$reason, "dansk dmy() format")
          date_candidates[[col_name]] <- candidate
          next
        }

        # Fallback til guess_formats med standardiseret fejlhåndtering
        guess_result <- safe_operation(
          operation_name = paste("guess_formats for", col_name),
          code = {
            guessed_formats <- suppressWarnings(
              lubridate::guess_formats(test_sample, c("ymd", "dmy", "mdy", "dby", "dmY", "Ymd", "mdY"))
            )

            if (!is.null(guessed_formats) && length(guessed_formats) > 0) {
              # Filtrer ugyldige formater
              valid_formats <- guessed_formats[!grepl("^n$|Unknown", guessed_formats)]

              if (length(valid_formats) > 0) {
                date_test <- suppressWarnings(
                  lubridate::parse_date_time(test_sample, orders = valid_formats, quiet = TRUE)
                )

                if (!is.null(date_test)) {
                  success_rate <- sum(!is.na(date_test)) / length(date_test)
                  if (success_rate >= 0.5) {
                    list(success_rate = success_rate, success = TRUE)
                  } else {
                    list(success_rate = success_rate, success = FALSE)
                  }
                } else {
                  list(success_rate = 0, success = FALSE)
                }
              } else {
                list(success_rate = 0, success = FALSE)
              }
            } else {
              list(success_rate = 0, success = FALSE)
            }
          },
          fallback = list(success_rate = 0, success = FALSE)
        )

        if (!is.null(guess_result) && guess_result$success) {
          candidate$score <- candidate$score + (guess_result$success_rate * 30)
          candidate$success_rate <- guess_result$success_rate
          candidate$type <- "guessed_date"
          candidate$reason <- paste(candidate$reason, "lubridate guess")
          date_candidates[[col_name]] <- candidate
        }
      }
    }

    # Gem kandidat hvis den har nogen score
    if (candidate$score > 0) {
      date_candidates[[col_name]] <- candidate
    }
  }

  # Vælg bedste kandidat baseret på score
  cat("DEBUG: [AUTO_DETECT_FUNC] 📊 Date candidate selection results:\n")
  x_col <- NULL
  best_score <- 0

  if (length(date_candidates) > 0) {
    cat("DEBUG: [AUTO_DETECT_FUNC] Found", length(date_candidates), "date candidates:\n")
    # Log kandidater for debugging
    for (name in names(date_candidates)) {
      cand <- date_candidates[[name]]
      cat("DEBUG: [AUTO_DETECT_FUNC] ", sprintf(
        "- %s: score=%.1f (%s, success=%.2f) - %s\n",
        name, cand$score, cand$type, cand$success_rate, cand$reason
      ))
    }
    # Find bedste kandidat
    for (name in names(date_candidates)) {
      cand <- date_candidates[[name]]
      if (cand$score > best_score) {
        best_score <- cand$score
        x_col <- name
      }
    }
    cat("DEBUG: [AUTO_DETECT_FUNC] 🎯 Best date candidate:", x_col, "(score:", best_score, ")\n")
  } else {
    cat("DEBUG: [AUTO_DETECT_FUNC] ⚠️ No date candidates found\n")
  }

  # Fallback til første kolonne hvis ingen dato-kandidater
  if (is.null(x_col) && length(col_names) > 0) {
    x_col <- col_names[1]
    cat("DEBUG: [AUTO_DETECT_FUNC] 📝 Fallback X column:", x_col, "\n")
  }

  # Tjek om vi har en gyldig X kolonne
  if (is.null(x_col)) {
    cat("DEBUG: [AUTO_DETECT_FUNC] ⚠️ Ingen gyldig X kolonne fundet - afbryder auto-detection\n")
    return(NULL)
  }

  # POST-PROCESSING: Konverter detekterede datokolonner til Date objekter (med reaktiv loop beskyttelse)
  # NOTE: values$original_data bevares uændret, kun values$current_data modificeres

  # Sikker dato-konvertering uden at trigger reaktive loops
  tryCatch({
    # Temporarily disable reactive observers during data modification
    isolate({
      for (candidate_name in names(date_candidates)) {
        candidate <- date_candidates[[candidate_name]]

        # Konverter character kolonner der blev detekteret som datoer
        if (candidate$type %in% c("danish_date", "guessed_date") && candidate$success_rate >= 0.7) {
          col_data <- current_data_check[[candidate_name]]

          if (is.character(col_data) || is.factor(col_data)) {

            # Perform safe date conversion
            converted_dates <- safe_operation(
              operation_name = paste("konvertering af", candidate_name, "til", candidate$type),
              code = {
                if (candidate$type == "danish_date") {
                  # Use safe_date_parse for Danish formats
                  result <- safe_date_parse(col_data,
                    locale = "da_DK.UTF-8",
                    operation_name = paste("dansk konvertering for", candidate_name)
                  )
                  as.POSIXct(result$data)
                } else if (candidate$type == "guessed_date") {
                  # Use parse_date_time for other formats
                  char_data <- as.character(col_data)
                  test_sample <- char_data[!is.na(char_data)][1:min(3, length(char_data[!is.na(char_data)]))]

                  guessed_formats <- suppressWarnings(
                    lubridate::guess_formats(test_sample, c("ymd", "dmy", "mdy", "dby", "dmY", "Ymd", "mdY"))
                  )

                  if (!is.null(guessed_formats) && length(guessed_formats) > 0) {
                    valid_formats <- guessed_formats[!grepl("^n$|Unknown", guessed_formats)]
                    if (length(valid_formats) > 0) {
                      result <- suppressWarnings(
                        lubridate::parse_date_time(col_data, orders = valid_formats, quiet = TRUE)
                      )
                      result
                    } else {
                      NULL
                    }
                  } else {
                    NULL
                  }
                } else {
                  NULL
                }
              },
              fallback = NULL
            )

            # Apply conversion if successful (inside isolate to prevent reactive triggers)
            if (!is.null(converted_dates) && sum(!is.na(converted_dates)) / length(converted_dates) >= 0.7) {
              # PHASE 4: Dimension safety check before assignment - unified state only
              current_rows <- nrow(app_state$data$current_data)
              converted_rows <- length(converted_dates)

              if (current_rows == converted_rows) {
                # Safe to assign - dimensions match - PHASE 4: Unified state only
                # Legacy values$current_data assignment removed
                app_state_rows <- nrow(app_state$data$current_data)
                if (app_state_rows == converted_rows) {
                  app_state$data$current_data[[candidate_name]] <- converted_dates
                } else {
                  log_warn(paste("Skipping app_state assignment - dimension mismatch:", app_state_rows, "vs", converted_rows), "POST_PROCESSING")
                }
              } else {
                log_warn(paste("Skipping date conversion assignment - dimension mismatch:", current_rows, "vs", converted_rows), "POST_PROCESSING")
              }
            }
          }
        }
      }
    })
  }, error = function(e) {
    log_error(paste("POST-PROCESSING fejlede:", e$message))
  })

  # Detekter numeriske kolonner
  cat("DEBUG: [AUTO_DETECT_FUNC] 🔢 Detecting numeric columns...\n")
  numeric_cols <- character(0)
  for (col_name in col_names) {
    if (col_name != x_col) {
      col_data <- data[[col_name]]
      is_numeric <- is.numeric(col_data)
      danish_parse_success <- sum(!is.na(parse_danish_number(col_data))) > length(col_data) * 0.8

      if (is_numeric || danish_parse_success) {
        numeric_cols <- c(numeric_cols, col_name)
        cat("DEBUG: [AUTO_DETECT_FUNC]   ✅", col_name, "- numeric (native:", is_numeric, ", danish:", danish_parse_success, ")\n")
      } else {
        cat("DEBUG: [AUTO_DETECT_FUNC]   ❌", col_name, "- not numeric\n")
      }
    } else {
      cat("DEBUG: [AUTO_DETECT_FUNC]   ⏭️", col_name, "- skipped (X column)\n")
    }
  }
  cat("DEBUG: [AUTO_DETECT_FUNC] Found", length(numeric_cols), "numeric columns:", paste(numeric_cols, collapse = ", "), "\n")

  # Smart detektion af tæller/nævner
  cat("DEBUG: [AUTO_DETECT_FUNC] 🎯 Smart detection for Y/N columns...\n")
  col_names_lower <- tolower(col_names)
  taeller_col <- NULL
  naevner_col <- NULL

  taeller_idx <- which(grepl("t.ller|tael|num|count", col_names_lower, ignore.case = TRUE))
  naevner_idx <- which(grepl("n.vner|naev|denom|total", col_names_lower, ignore.case = TRUE))

  cat("DEBUG: [AUTO_DETECT_FUNC] Pattern matches - tæller:", length(taeller_idx), ", nævner:", length(naevner_idx), "\n")

  if (length(taeller_idx) > 0 && length(naevner_idx) > 0) {
    taeller_col <- col_names[taeller_idx[1]]
    naevner_col <- col_names[naevner_idx[1]]
    cat("DEBUG: [AUTO_DETECT_FUNC] 📝 Name-based assignment - Y:", taeller_col, ", N:", naevner_col, "\n")
  } else if (length(numeric_cols) >= 2) {
    taeller_col <- numeric_cols[1]
    naevner_col <- numeric_cols[2]
    cat("DEBUG: [AUTO_DETECT_FUNC] 📊 Fallback assignment (first two numerics) - Y:", taeller_col, ", N:", naevner_col, "\n")
  } else if (length(numeric_cols) >= 1) {
    taeller_col <- numeric_cols[1]
    cat("DEBUG: [AUTO_DETECT_FUNC] 📊 Single numeric assignment - Y:", taeller_col, ", N: NULL\n")
  } else {
    cat("DEBUG: [AUTO_DETECT_FUNC] ⚠️ No numeric columns found for Y/N assignment\n")
  }

  # Detekter skift/fase kolonne (boolean eller tekst med skift-relaterede termer)
  skift_col <- NULL
  skift_idx <- which(grepl("skift|shift|fase|phase|change|periode", col_names_lower, ignore.case = TRUE))

  if (length(skift_idx) > 0) {
    skift_col <- col_names[skift_idx[1]]
  } else {
    # Søg efter boolean kolonner som kan repræsentere skift
    for (col_name in col_names) {
      col_data <- data[[col_name]]
      if (is.logical(col_data)) {
        skift_col <- col_name
        break
      }
    }
  }

  # Detekter frys/kontrol-frysning kolonne (boolean eller tekst med frys-relaterede termer)
  frys_col <- NULL
  frys_idx <- which(grepl("frys|freeze|kontrol|control|stop|pause", col_names_lower, ignore.case = TRUE))

  if (length(frys_idx) > 0) {
    frys_col <- col_names[frys_idx[1]]
  } else {
    # Søg efter boolean kolonner som kan repræsentere frysning (efter skift kolonne)
    for (col_name in col_names) {
      if (col_name != skift_col) { # Skip skift kolonne
        col_data <- data[[col_name]]
        if (is.logical(col_data)) {
          frys_col <- col_name
          break
        }
      }
    }
  }

  # Detekter kommentar kolonne (tekst-baserede kolonner)
  kommentar_col <- NULL
  kommentar_idx <- which(grepl("kommentar|comment|note|noter|bemærk|remark", col_names_lower, ignore.case = TRUE))

  if (length(kommentar_idx) > 0) {
    kommentar_col <- col_names[kommentar_idx[1]]
  } else {
    # Søg efter karakter kolonner som ikke allerede er tildelt og indeholder tekst
    for (col_name in col_names) {
      if (col_name != x_col && col_name != taeller_col && col_name != naevner_col && col_name != skift_col && col_name != frys_col) {
        col_data <- data[[col_name]]
        if (is.character(col_data) && any(nzchar(col_data, keepNA = FALSE), na.rm = TRUE)) {
          kommentar_col <- col_name
          break
        }
      }
    }
  }

  # Opdater dropdowns til at VISE de detekterede værdier
  cat("DEBUG: [AUTO_DETECT_FUNC] 📋 Final column assignments:\n")
  cat("DEBUG: [AUTO_DETECT_FUNC]   X (date/time):", x_col %||% "NULL", "\n")
  cat("DEBUG: [AUTO_DETECT_FUNC]   Y (tæller):", taeller_col %||% "NULL", "\n")
  cat("DEBUG: [AUTO_DETECT_FUNC]   N (nævner):", naevner_col %||% "NULL", "\n")
  cat("DEBUG: [AUTO_DETECT_FUNC]   Skift:", skift_col %||% "NULL", "\n")
  cat("DEBUG: [AUTO_DETECT_FUNC]   Frys:", frys_col %||% "NULL", "\n")
  cat("DEBUG: [AUTO_DETECT_FUNC]   Kommentar:", kommentar_col %||% "NULL", "\n")

  # Først sikr at choices er opdateret med nuværende kolonnenavne
  all_cols <- col_names
  col_choices <- setNames(
    c("", all_cols),
    c("Vælg kolonne...", all_cols)
  )

  # UI opdateringer håndteres nu af reaktiv observer - fjern duplikeret logik
  isolate({
    detected_msg <- paste0(
      "Auto-detekteret og opdateret dropdowns: ",
      "X=", if (is.null(x_col)) "ingen" else x_col, ", ",
      "Y=", if (is.null(taeller_col)) "ingen" else taeller_col,
      if (!is.null(naevner_col)) paste0(", N=", naevner_col) else ", N=ingen",
      if (!is.null(skift_col)) paste0(", Skift=", skift_col) else ", Skift=ingen",
      if (!is.null(frys_col)) paste0(", Frys=", frys_col) else ", Frys=ingen",
      if (!is.null(kommentar_col)) paste0(", Kommentar=", kommentar_col) else ", Kommentar=ingen"
    )

    showNotification(
      detected_msg,
      type = "message",
      duration = 4
    )

    # PHASE 4: Legacy values$auto_detected_columns assignment removed - now using unified state only
    cat("DEBUG: [PHASE4] Skipping legacy values$auto_detected_columns assignment (unified state handles this)\n")
    # PHASE 4: Sync auto_detected_columns to centralized state - FIX: Use correct path that visualization expects
    app_state$columns$auto_detected_columns <- list(
      x_col = x_col,
      y_col = taeller_col,
      n_col = naevner_col,
      skift_col = skift_col,
      frys_col = frys_col,
      kommentar_col = kommentar_col,
      timestamp = Sys.time()
    )
    cat("DEBUG: [PHASE4] Synced auto_detected_columns to centralized state (FIXED PATH)\n")

    # Trigger UI sync for visual feedback on Kolonnematch tab using reactive trigger
    cat("DEBUG: [AUTO_DETECT_FUNC] 🎯 CRITICAL: Setting ui_sync_needed with:\n")
    cat("DEBUG: [AUTO_DETECT_FUNC]   x_col:", x_col %||% "NULL", "\n")
    cat("DEBUG: [AUTO_DETECT_FUNC]   taeller_col:", taeller_col %||% "NULL", "\n")
    cat("DEBUG: [AUTO_DETECT_FUNC]   naevner_col:", naevner_col %||% "NULL", "\n")
    cat("DEBUG: [AUTO_DETECT_FUNC]   col_choices length:", length(col_choices), "\n")

    # Use reactive trigger with timestamp to force reactivity
    cat("DEBUG: [AUTO_DETECT_FUNC] 🔄 Triggering UI sync - should update input fields!\n")
    sync_data <- list(
      x_col = x_col,
      taeller_col = taeller_col,
      naevner_col = naevner_col,
      skift_col = skift_col,
      frys_col = frys_col,
      kommentar_col = kommentar_col,
      col_choices = col_choices,
      timestamp = as.numeric(Sys.time())  # Force reactivity trigger
    )

    # PHASE 4: Use unified state management
    app_state$columns$ui_sync$needed <- sync_data

    # CRITICAL: Set debounce timestamp BEFORE triggering UI sync to prevent race condition
    app_state$columns$ui_sync$last_sync_time <- Sys.time()
    cat("DEBUG: [UI_SYNC_TRIGGER] ⏰ Debounce timestamp set BEFORE UI sync to prevent column mgmt override\n")

    # REACTIVE FIX: Trigger direct reactiveVal if available (CRITICAL for UI sync)
    if (!is.null(ui_sync_trigger)) {
      cat("DEBUG: [UI_SYNC_TRIGGER] 🔄 FULL MODE: Triggering direct reactiveVal for UI sync\n")
      ui_sync_trigger(sync_data)
      cat("DEBUG: [UI_SYNC_TRIGGER] ✅ FULL MODE: Direct trigger fired successfully\n")
    } else {
      cat("DEBUG: [UI_SYNC_TRIGGER] ⚠️ FULL MODE: No ui_sync_trigger available - relying on nested observeEvent\n")
    }

    cat("DEBUG: [AUTO_DETECT_FUNC] ✅ Auto-detect completed successfully\n")
    cat("DEBUG: [AUTO_DETECT_FUNC] ========================================\n")

    # Complete auto-detect workflow tracing
    autodetect_tracer$step("ui_sync_data_created")
    autodetect_tracer$complete("auto_detect_full_workflow_complete")

    # CRITICAL: Trigger navigation update efter autodetect completion
    # Dette sikrer at alle eventReactive komponenter (inklusive generateSPCPlot) re-evalueres
    if (!is.null(app_state$navigation_trigger)) {
      old_trigger <- app_state$navigation_trigger()
      app_state$navigation_trigger(old_trigger + 1)
      new_trigger <- app_state$navigation_trigger()
      cat("DEBUG: [AUTO_DETECT] navigation_trigger incremented from", old_trigger, "to", new_trigger, "\n")
      log_debug(paste("AUTO_DETECT: navigation_trigger incremented from", old_trigger, "to", new_trigger), "AUTO_DETECT")
    } else {
      cat("DEBUG: [AUTO_DETECT] ⚠️ navigation_trigger not available - plots may not update\n")
      log_debug("AUTO_DETECT: navigation_trigger not available - plots may not update", "AUTO_DETECT")
    }

    debug_log("Auto-detect process completed successfully", "AUTO_DETECT_FLOW", level = "INFO",
              context = list(
                x_col = if(exists("x_col")) x_col else "unknown",
                y_col = if(exists("y_col")) y_col else "unknown",
                n_col = if(exists("n_col")) n_col else "unknown",
                ui_sync_triggered = TRUE,
                navigation_trigger_incremented = !is.null(app_state$navigation_trigger)
              ),
              session_id = session_id)

    # Take final state snapshot
    if (!is.null(app_state)) {
      debug_state_snapshot("after_auto_detect_complete", app_state, session_id = session_id)
    }
  })

}

# MODAL FUNKTIONER ============================================================

## Vis kolonne-redigeré modal
# Viser modal dialog for redigering af kolonnenavne
show_column_edit_modal <- function(session, values, app_state = NULL) {
  # PHASE 4: Use unified state management
  current_data_check <- app_state$data$current_data
  req(current_data_check)

  current_names <- names(current_data_check)

  name_inputs <- lapply(1:length(current_names), function(i) {
    textInput(
      paste0("col_name_", i),
      paste("Kolonne", i, ":"),
      value = current_names[i],
      placeholder = paste("Navn for kolonne", i)
    )
  })

  showModal(modalDialog(
    title = "Redigér kolonnenavne",
    size = "m",
    div(
      style = "margin-bottom: 15px;",
      h6("Nuværende kolonnenavne:", style = "font-weight: 500;"),
      p(paste(current_names, collapse = ", "), style = "color: #666; font-style: italic;")
    ),
    div(
      style = "max-height: 300px; overflow-y: auto;",
      name_inputs
    ),
    footer = tagList(
      modalButton("Annuller"),
      actionButton("confirm_column_names", "Gem ændringer", class = "btn-primary")
    )
  ))
}

## Håndtér kolonnenavn ændringer
# Behandler ændringer af kolonnenavne fra modal dialog
handle_column_name_changes <- function(input, session, values, app_state = NULL) {
  # PHASE 4: Use unified state management
  current_data_check <- app_state$data$current_data
  req(current_data_check)

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
    showNotification(
      "Kolonnenavne skal være unikke. Ret duplikater og prøv igen.",
      type = "error",
      duration = 5
    )
    return()
  }

  # PHASE 4: Unified state assignment only
  names(app_state$data$current_data) <- new_names

  removeModal()

  if (!identical(current_names, new_names)) {
    changed_cols <- which(current_names != new_names)
    change_summary <- paste(
      paste0("'", current_names[changed_cols], "' -> '", new_names[changed_cols], "'"),
      collapse = ", "
    )

    showNotification(
      paste("Kolonnenavne opdateret:", change_summary),
      type = "message",
      duration = 4
    )
  } else {
    showNotification("Ingen ændringer i kolonnenavne", type = "message", duration = 2)
  }
}

## Vis tilføj kolonne modal
# Viser modal dialog for tilføjelse af nye kolonner
show_add_column_modal <- function() {
  showModal(modalDialog(
    title = "Tilføj ny kolonne",
    textInput("new_col_name", "Kolonnenavn:", value = "Ny_kolonne"),
    selectInput("new_col_type", "Type:",
      choices = list("Numerisk" = "numeric", "Tekst" = "text", "Dato" = "date")
    ),
    footer = tagList(
      modalButton("Annuller"),
      actionButton("confirm_add_col", "Tilføj", class = "btn-primary")
    )
  ))
}

## Håndtér tilføjelse af kolonne
# Behandler tilføjelse af nye kolonner til data
handle_add_column <- function(input, session, values, app_state = NULL) {
  # PHASE 4: Use unified state management
  current_data_check <- app_state$data$current_data
  req(input$new_col_name, current_data_check)

  new_col_name <- input$new_col_name
  new_col_type <- input$new_col_type

  if (new_col_type == "numeric") {
    # PHASE 4: Unified state assignment only
    app_state$data$current_data[[new_col_name]] <- rep(NA_real_, nrow(current_data_check))
  } else if (new_col_type == "date") {
    # PHASE 4: Unified state assignment only
    app_state$data$current_data[[new_col_name]] <- rep(NA_character_, nrow(current_data_check))
  } else {
    # PHASE 4: Unified state assignment only
    app_state$data$current_data[[new_col_name]] <- rep(NA_character_, nrow(current_data_check))
  }

  removeModal()
  showNotification(paste("Kolonne", new_col_name, "tilføjet"), type = "message")
}
# server_data_table.R
# Server logik for data tabel rendering og interaktion

# Dependencies ----------------------------------------------------------------

# DATATABEL SETUP =============================================================

## Hovedfunktion for datatabel
# Opsætter al server logik relateret til data-tabel håndtering
setup_data_table <- function(input, output, session, values, app_state = NULL) {
  cat("DEBUG: [DATA_TABLE] ===========================================\n")
  cat("DEBUG: [DATA_TABLE] Setting up data table with unified state\n")

  # NAVIGATION TRIGGER: Create reactive that uses the navigation trigger
  app_data_reactive <- eventReactive(app_state$navigation_trigger(), {
    # PHASE 8: Enhanced reactive execution tracking
    debug_reactive_execution("app_data_reactive", "trigger_fired",
                            list(trigger_value = app_state$navigation_trigger()),
                            session_id = session$token)

    current_data_value <- app_state$data$current_data

    cat("DEBUG: [DATA_TABLE] Table reactive triggered\n")
    cat("DEBUG: [DATA_TABLE] trigger_value:", app_state$navigation_trigger(), "\n")
    cat("DEBUG: [DATA_TABLE] current_data is null:", is.null(current_data_value), "\n")

    # PHASE 8: Enhanced reactive execution tracking
    debug_reactive_execution("app_data_reactive", "data_retrieved",
                            output_value = current_data_value,
                            session_id = session$token)

    # Return the actual data from unified state
    return(current_data_value)
  }, ignoreNULL = FALSE)

  # Hovedtabel rendering med excelR
  output$main_data_table <- excelR::renderExcel({
    # NAVIGATION TRIGGER: Use eventReactive pattern for consistent behavior
    current_data_check <- app_data_reactive()
    req(current_data_check)

    cat("DEBUG: [DATA_TABLE] Rendering table with data dimensions:", dim(current_data_check), "\n")

    # Inkluder table_version for at tvinge re-render efter gendannelse
    # PHASE 4: Use unified state management
    version_trigger <- app_state$session$table_version

    data <- current_data_check

    # Behold logiske kolonner som logiske for excelR checkbox
    # excelR håndterer logiske værdier direkte for checkbox type

    excelR::excelTable(
      data = data,
      columns = data.frame(
        title = names(data),
        type = case_when(
          names(data) == "Skift" ~ "checkbox",
          names(data) == "Frys" ~ "radio",
          TRUE ~ "text"
        ),
        width = case_when(
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
  observeEvent(input$main_data_table,
    {
      # PHASE 4: Use unified state management
      updating_table_check <- app_state$data$updating_table

      # PHASE 4: Use unified state management
      restoring_session_check <- app_state$session$restoring_session

      if (updating_table_check || restoring_session_check) {
        return()
      }

      # PHASE 4: Use unified state management
      app_state$data$updating_table <- TRUE
      # PHASE 4: Use unified state management
      app_state$data$table_operation_in_progress <- TRUE

      on.exit(
        {
          # PHASE 4: Use unified state management
          app_state$data$updating_table <- FALSE
        },
        add = TRUE
      )

      # Trigger event-driven cleanup instead of timing-based
      # PHASE 4: Use unified state management
      app_state$data$table_operation_cleanup_needed <- TRUE

      tryCatch(
        {
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

          # PHASE 4: Unified state assignment only
          app_state$data$current_data <- new_df

          showNotification("Tabel opdateret", type = "message", duration = 2)
        },
        error = function(e) {
          log_error(paste("ERROR in excelR table change:", e$message), "DATA_TABLE")
          showNotification(
            paste("Fejl ved tabel-opdatering:", e$message),
            type = "error",
            duration = 3
          )
        }
      )
    },
    ignoreInit = TRUE
  )

  # Tilføj række
  observeEvent(input$add_row, {
    # NAVIGATION TRIGGER: Use eventReactive pattern for consistent behavior
    current_data_check <- app_data_reactive()
    req(current_data_check)

    # Sæt vedvarende flag for at forhindre auto-save interferens
    # PHASE 4: Use unified state management
    app_state$data$table_operation_in_progress <- TRUE

    new_row <- current_data_check[1, ]
    new_row[1, ] <- NA

    # PHASE 4: Unified state assignment only
    app_state$data$current_data <- rbind(app_state$data$current_data, new_row)

    showNotification("Ny række tilføjet", type = "message")

    # Trigger event-driven cleanup instead of timing-based
    # PHASE 4: Use unified state management
    app_state$data$table_operation_cleanup_needed <- TRUE
  })

  # Nulstil tabel
  # observeEvent(input$reset_table, {
  #   values$updating_table <- TRUE
  #   values$table_operation_in_progress <- TRUE
  #
  #   values$current_data <- data.frame(
  #     Dato = rep(NA_character_, 5),
  #     Taeller = rep(NA_real_, 5),
  #     Naevner = rep(NA_real_, 5),
  #     stringsAsFactors = FALSE
  #   )
  #
  #   values$file_uploaded <- FALSE
  #   values$original_data <- NULL
  #   values$auto_detect_done <- FALSE
  #
  #   isolate({
  #     shinyjs::reset("data_file")
  #   })
  #
  #   values$updating_table <- FALSE
  #
  #   # Ryd vedvarende flag efter forsinkelse
  #   later::later(function() {
  #     values$table_operation_in_progress <- FALSE
  #   }, delay = 1)
  #
  #   showNotification(
  #     "Tabel og fil-upload tømt - indtast nye data eller upload ny fil. Titel og beskrivelse bevaret.",
  #     type = "message",
  #     duration = 4
  #   )
  # })
}
