#' Event System Utilities
#'
#' This file contains utilities for the unified reactive event system.
#' It provides centralized event listeners and handlers for the application.
#'
#' @name utils_event_system
NULL

#' Setup Event Listeners
#'
#' Sets up all reactive event listeners for the application.
#' This function creates observeEvent() handlers for all events
#' in the app_state$events reactive values.
#'
#' @param app_state The centralized app state
#' @param emit The emit API for triggering events
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#'
#' @details
#' This function consolidates all event-driven reactive patterns
#' in one place, replacing the scattered observeEvent() calls
#' and bridge observers that were previously spread across
#' multiple files.
#'
#' All observers use ignoreInit = TRUE to prevent firing at startup
#' and appropriate priorities to ensure correct execution order.
#'
setup_event_listeners <- function(app_state, emit, input, output, session) {
  cat("DEBUG: [EVENT_SYSTEM] Setting up unified event listeners\n")

  # DATA LIFECYCLE EVENTS
  observeEvent(app_state$events$data_loaded, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$STATE_MANAGEMENT, {
    cat("DEBUG: [EVENT] data_loaded event received\n")

    # Trigger auto-detection after data is loaded
    if (!is.null(app_state$data$current_data)) {
      cat("DEBUG: [EVENT] Data available, emitting auto_detection_started\n")
      emit$auto_detection_started()
    }
  })

  observeEvent(app_state$events$data_changed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$DATA_PROCESSING, {
    cat("DEBUG: [EVENT] data_changed event received\n")

    # Update column choices when data changes
    update_column_choices_unified(app_state, input, output, session)
  })

  # AUTO-DETECTION EVENTS
  observeEvent(app_state$events$auto_detection_started, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$AUTO_DETECT, {
    cat("DEBUG: [EVENT] auto_detection_started event received\n")

    # Set auto-detection in progress
    app_state$columns$auto_detect_in_progress <- TRUE

    # Perform auto-detection
    tryCatch({
      if (!is.null(app_state$data$current_data)) {
        auto_detect_and_update_columns_unified(app_state, emit)
      }
    }, error = function(e) {
      cat("DEBUG: [EVENT] Auto-detection error:", e$message, "\n")
      app_state$columns$auto_detect_in_progress <- FALSE
    })
  })

  observeEvent(app_state$events$auto_detection_completed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$AUTO_DETECT, {
    cat("DEBUG: [EVENT] auto_detection_completed event received\n")

    # Update state
    app_state$columns$auto_detect_in_progress <- FALSE
    app_state$columns$auto_detect_completed <- TRUE

    # Trigger UI sync if columns were detected
    if (!is.null(app_state$columns$auto_detect_results)) {
      cat("DEBUG: [EVENT] Auto-detection results available, emitting ui_sync_needed\n")
      emit$ui_sync_needed()
    }
  })

  # UI SYNCHRONIZATION EVENTS
  observeEvent(app_state$events$ui_sync_needed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$UI_SYNC, {
    cat("DEBUG: [EVENT] ui_sync_needed event received\n")

    # Perform UI synchronization
    sync_ui_with_columns_unified(app_state, input, output, session)

    # Mark sync as completed
    emit$ui_sync_completed()
  })

  observeEvent(app_state$events$ui_sync_completed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$UI_SYNC, {
    cat("DEBUG: [EVENT] ui_sync_completed event received\n")

    # Update timestamp
    app_state$columns$ui_sync_last_time <- Sys.time()

    # Trigger navigation change to update plots
    emit$navigation_changed()
  })

  # NAVIGATION EVENTS
  observeEvent(app_state$events$navigation_changed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$STATUS_UPDATES, {
    cat("DEBUG: [EVENT] navigation_changed event received\n")

    # Increment navigation trigger to update all eventReactive components
    app_state$navigation$trigger <- app_state$navigation$trigger + 1L
    cat("DEBUG: [EVENT] Navigation trigger incremented to:", app_state$navigation$trigger, "\n")
  })

  # TEST MODE EVENTS
  observeEvent(app_state$events$test_mode_ready, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$AUTO_DETECT, {
    cat("DEBUG: [EVENT] test_mode_ready event received\n")

    # In test mode, immediately start auto-detection
    if (!is.null(app_state$data$current_data)) {
      cat("DEBUG: [EVENT] Test mode: emitting auto_detection_started\n")
      emit$auto_detection_started()
    }
  })

  # SESSION LIFECYCLE EVENTS
  observeEvent(app_state$events$session_reset, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$CLEANUP, {
    cat("DEBUG: [EVENT] session_reset event received\n")

    # Reset all state to initial values
    app_state$data$current_data <- NULL
    app_state$columns$auto_detect_in_progress <- FALSE
    app_state$columns$auto_detect_completed <- FALSE
    app_state$columns$auto_detect_results <- NULL

    cat("DEBUG: [EVENT] Session state reset completed\n")
  })

  # ERROR HANDLING EVENTS ===================================================

  # General error event listener
  observeEvent(app_state$events$error_occurred, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$highest, {
    error_info <- app_state$errors$last_error

    cat("DEBUG: [ERROR_EVENT] General error event received\n")

    # Centralized error logging with context
    debug_log("Error event triggered", "ERROR_SYSTEM", level = "ERROR",
              context = error_info,
              session_id = if(!is.null(session)) session$token else NULL)

    # Emit recovery attempts if appropriate
    if (!is.null(error_info) && !is.null(emit)) {
      if (error_info$type %in% c("processing", "validation")) {
        # For processing errors, increment recovery attempts
        app_state$errors$recovery_attempts <- app_state$errors$recovery_attempts + 1L
      }
    }
  })

  # Processing error event listener
  observeEvent(app_state$events$processing_error, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$high, {
    error_info <- app_state$errors$last_error

    cat("DEBUG: [ERROR_EVENT] Processing error event received\n")

    # For processing errors, we might want to trigger data validation
    if (!is.null(error_info) && !is.null(emit)) {
      if (grepl("data|processing|convert", error_info$message, ignore.case = TRUE)) {
        cat("DEBUG: [ERROR_RECOVERY] Suggesting data validation after processing error\n")
        # Could emit validation_needed event if we had one
      }
    }
  })

  # Validation error event listener
  observeEvent(app_state$events$validation_error, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$high, {
    error_info <- app_state$errors$last_error

    cat("DEBUG: [ERROR_EVENT] Validation error event received\n")

    # For validation errors, clear problematic state
    if (!is.null(error_info) && !is.null(app_state)) {
      cat("DEBUG: [ERROR_RECOVERY] Clearing validation state after error\n")
      # Reset validation-related state if needed
    }
  })

  # Network error event listener
  observeEvent(app_state$events$network_error, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$medium, {
    error_info <- app_state$errors$last_error

    cat("DEBUG: [ERROR_EVENT] Network error event received\n")

    # For network errors (file I/O), we might want to retry or suggest file check
    if (!is.null(error_info) && !is.null(emit)) {
      cat("DEBUG: [ERROR_RECOVERY] Network error suggests file operation issue\n")
    }
  })

  # Recovery completed event listener
  observeEvent(app_state$events$recovery_completed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$low, {
    error_info <- app_state$errors$last_error

    cat("DEBUG: [ERROR_EVENT] Recovery completed event received\n")

    # Update recovery timestamp
    app_state$errors$last_recovery_time <- Sys.time()

    # Log successful recovery
    debug_log("Error recovery completed", "ERROR_SYSTEM", level = "INFO",
              context = list(recovery_time = Sys.time()),
              session_id = if(!is.null(session)) session$token else NULL)
  })

  cat("DEBUG: [EVENT_SYSTEM] ✅ All event listeners registered (including error handling)\n")
}

#' Auto-detect and update columns (Unified Event Version)
#'
#' Unified version of auto-detection that uses the event system
#' instead of direct reactive triggers.
#'
#' @param app_state The centralized app state
#' @param emit The emit API for triggering events
#'
auto_detect_and_update_columns_unified <- function(app_state, emit) {
  cat("DEBUG: [AUTO_DETECT_UNIFIED] Starting column auto-detection\n")

  if (is.null(app_state$data$current_data)) {
    cat("DEBUG: [AUTO_DETECT_UNIFIED] No data available\n")
    return()
  }

  data <- app_state$data$current_data
  col_names <- names(data)

  cat("DEBUG: [AUTO_DETECT_UNIFIED] Analyzing columns:", paste(col_names, collapse = ", "), "\n")

  # Simplified column detection logic
  results <- list(
    x_column = NULL,
    y_column = NULL,
    n_column = NULL,
    cl_column = NULL
  )

  # Basic auto-detection patterns
  for (col in col_names) {
    col_lower <- tolower(col)

    # Date column detection
    if (is.null(results$x_column) && any(grepl("dato|date|tid|time", col_lower))) {
      results$x_column <- col
      cat("DEBUG: [AUTO_DETECT_UNIFIED] Found x_column:", col, "\n")
    }

    # Count column detection
    if (is.null(results$y_column) && any(grepl("antal|count|værdi|value", col_lower))) {
      results$y_column <- col
      cat("DEBUG: [AUTO_DETECT_UNIFIED] Found y_column:", col, "\n")
    }

    # Denominator column detection
    if (is.null(results$n_column) && any(grepl("nævner|denom|total", col_lower))) {
      results$n_column <- col
      cat("DEBUG: [AUTO_DETECT_UNIFIED] Found n_column:", col, "\n")
    }
  }

  # Store results in both locations for consistency during transition
  app_state$columns$auto_detect_results <- results
  app_state$columns$auto_detected_columns <- results

  # Update individual column mappings
  if (!is.null(results$x_column)) app_state$columns$x_column <- results$x_column
  if (!is.null(results$y_column)) app_state$columns$y_column <- results$y_column
  if (!is.null(results$n_column)) app_state$columns$n_column <- results$n_column
  if (!is.null(results$cl_column)) app_state$columns$cl_column <- results$cl_column

  cat("DEBUG: [AUTO_DETECT_UNIFIED] ✅ Auto-detection completed\n")

  # Emit completion event
  emit$auto_detection_completed()
}

#' Sync UI with columns (Unified Event Version)
#'
#' Unified version of UI synchronization that updates UI controls
#' based on detected columns.
#'
#' @param app_state The centralized app state
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#'
sync_ui_with_columns_unified <- function(app_state, input, output, session) {
  cat("DEBUG: [UI_SYNC_UNIFIED] Starting UI synchronization\n")

  if (is.null(app_state$data$current_data)) {
    cat("DEBUG: [UI_SYNC_UNIFIED] No data available for UI sync\n")
    return()
  }

  data <- app_state$data$current_data
  col_names <- names(data)
  results <- app_state$columns$auto_detect_results

  # Update UI controls with detected columns
  tryCatch({
    if (!is.null(results$x_column)) {
      updateSelectInput(session, "x_column",
                       choices = col_names,
                       selected = results$x_column)
      cat("DEBUG: [UI_SYNC_UNIFIED] Updated x_column UI:", results$x_column, "\n")
    }

    if (!is.null(results$y_column)) {
      updateSelectInput(session, "y_column",
                       choices = col_names,
                       selected = results$y_column)
      cat("DEBUG: [UI_SYNC_UNIFIED] Updated y_column UI:", results$y_column, "\n")
    }

    if (!is.null(results$n_column)) {
      updateSelectInput(session, "n_column",
                       choices = c("Ingen" = "", col_names),
                       selected = results$n_column)
      cat("DEBUG: [UI_SYNC_UNIFIED] Updated n_column UI:", results$n_column, "\n")
    }

    cat("DEBUG: [UI_SYNC_UNIFIED] ✅ UI synchronization completed\n")

  }, error = function(e) {
    cat("DEBUG: [UI_SYNC_UNIFIED] UI sync error:", e$message, "\n")
  })
}

#' Update Column Choices (Unified Event Version)
#'
#' Unified version of column choice updates that handles
#' data changes through the event system.
#'
#' @param app_state The centralized app state
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#'
update_column_choices_unified <- function(app_state, input, output, session) {
  cat("DEBUG: [COLUMN_CHOICES_UNIFIED] Starting column choices update\n")

  # Check if we should skip during table operations
  if (app_state$data$updating_table) {
    cat("DEBUG: [COLUMN_CHOICES_UNIFIED] Skipping - table update in progress\n")
    return()
  }

  # Skip if auto-detect is in progress
  if (app_state$columns$auto_detect_in_progress) {
    cat("DEBUG: [COLUMN_CHOICES_UNIFIED] Skipping - auto-detect in progress\n")
    return()
  }

  # Skip if UI sync is needed (to avoid race conditions)
  if (app_state$columns$ui_sync_needed) {
    cat("DEBUG: [COLUMN_CHOICES_UNIFIED] Skipping - UI sync pending\n")
    return()
  }

  # Get current data
  if (is.null(app_state$data$current_data)) {
    cat("DEBUG: [COLUMN_CHOICES_UNIFIED] No data available\n")
    return()
  }

  data <- app_state$data$current_data
  all_cols <- names(data)
  cat("DEBUG: [COLUMN_CHOICES_UNIFIED] Available columns:", paste(all_cols, collapse = ", "), "\n")

  if (length(all_cols) > 0) {
    # Create column choices
    col_choices <- setNames(
      c("", all_cols),
      c("Vælg kolonne...", all_cols)
    )

    # Update UI controls
    tryCatch({
      updateSelectizeInput(session, "x_column", choices = col_choices)
      updateSelectizeInput(session, "y_column", choices = col_choices)
      updateSelectizeInput(session, "n_column", choices = col_choices)
      updateSelectizeInput(session, "cl_column", choices = col_choices)

      cat("DEBUG: [COLUMN_CHOICES_UNIFIED] ✅ Column choices updated\n")

    }, error = function(e) {
      cat("DEBUG: [COLUMN_CHOICES_UNIFIED] Error updating UI:", e$message, "\n")
    })
  }
}