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
setup_event_listeners <- function(app_state, emit, input, output, session, ui_service = NULL) {
  log_debug("Setting up unified event listeners", .context = "EVENT_SYSTEM")

  # DATA LIFECYCLE EVENTS
  observeEvent(app_state$events$data_loaded, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$STATE_MANAGEMENT, {
    log_debug("data_loaded event received", .context = "EVENT")

    # Trigger auto-detection after data is loaded
    if (!is.null(app_state$data$current_data)) {
      log_debug("Data available, emitting auto_detection_started", .context = "EVENT")
      emit$auto_detection_started()
    }
  })

  observeEvent(app_state$events$data_changed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$DATA_PROCESSING, {
    log_debug("data_changed event received", .context = "EVENT")

    # Update column choices when data changes
    update_column_choices_unified(app_state, input, output, session, ui_service)
  })

  # AUTO-DETECTION EVENTS
  observeEvent(app_state$events$auto_detection_started, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$AUTO_DETECT, {
    log_debug("auto_detection_started event received", .context = "EVENT")

    # Set auto-detection in progress
    app_state$columns$auto_detect_in_progress <- TRUE

    # Perform auto-detection using unified engine
    tryCatch({
      if (!is.null(app_state$data$current_data)) {
        # Use unified autodetect engine - data available, so full analysis
        autodetect_engine(
          data = app_state$data$current_data,
          trigger_type = "file_upload",  # This event is triggered by data uploads
          app_state = app_state,
          emit = emit
        )
      } else {
        # No data available - session start scenario (name-only)
        autodetect_engine(
          data = NULL,
          trigger_type = "session_start",
          app_state = app_state,
          emit = emit
        )
      }
    }, error = function(e) {
      log_debug("Auto-detection error:", e$message, .context = "EVENT")
      app_state$columns$auto_detect_in_progress <- FALSE
    })
  })

  observeEvent(app_state$events$auto_detection_completed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$AUTO_DETECT, {
    log_debug("auto_detection_completed event received", .context = "EVENT")

    # Update state
    app_state$columns$auto_detect_in_progress <- FALSE
    app_state$columns$auto_detect_completed <- TRUE

    # Trigger UI sync if columns were detected
    if (!is.null(app_state$columns$auto_detect_results)) {
      log_debug("Auto-detection results available, emitting ui_sync_needed", .context = "EVENT")
      emit$ui_sync_needed()
    }
  })

  # UI SYNCHRONIZATION EVENTS
  observeEvent(app_state$events$ui_sync_needed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$UI_SYNC, {
    log_debug("ui_sync_needed event received", .context = "EVENT")

    # Perform UI synchronization
    sync_ui_with_columns_unified(app_state, input, output, session, ui_service)

    # Mark sync as completed
    emit$ui_sync_completed()
  })

  observeEvent(app_state$events$ui_sync_completed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$UI_SYNC, {
    log_debug("ui_sync_completed event received", .context = "EVENT")

    # Update timestamp
    app_state$columns$ui_sync_last_time <- Sys.time()

    # Trigger navigation change to update plots
    emit$navigation_changed()
  })

  # NAVIGATION EVENTS
  observeEvent(app_state$events$navigation_changed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$STATUS_UPDATES, {
    log_debug("navigation_changed event received", .context = "EVENT")

    # Increment navigation trigger to update all eventReactive components
    app_state$navigation$trigger <- app_state$navigation$trigger + 1L
    log_debug("Navigation trigger incremented to:", app_state$navigation$trigger, .context = "EVENT")
  })

  # TEST MODE EVENTS
  observeEvent(app_state$events$test_mode_ready, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$AUTO_DETECT, {
    log_debug("test_mode_ready event received", .context = "EVENT")

    # In test mode, immediately start auto-detection
    if (!is.null(app_state$data$current_data)) {
      log_debug("Test mode: emitting auto_detection_started", .context = "EVENT")
      emit$auto_detection_started()
    }
  })

  # SESSION LIFECYCLE EVENTS
  observeEvent(app_state$events$session_reset, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$CLEANUP, {
    log_debug("session_reset event received", .context = "EVENT")

    # Reset all state to initial values
    app_state$data$current_data <- NULL
    app_state$columns$auto_detect_in_progress <- FALSE
    app_state$columns$auto_detect_completed <- FALSE
    app_state$columns$auto_detect_results <- NULL

    log_debug("Session state reset completed", .context = "EVENT")
  })

  # ERROR HANDLING EVENTS ===================================================

  # General error event listener
  observeEvent(app_state$events$error_occurred, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$highest, {
    error_info <- app_state$errors$last_error

    log_debug("General error event received", .context = "ERROR_EVENT")

    # Centralized error logging with context
    log_error("Error event triggered", .context = "ERROR_SYSTEM")
    if (!is.null(error_info)) {
      log_debug_kv(
        error_type = error_info$type %||% "unknown",
        error_message = error_info$message %||% "no message",
        session_id = if(!is.null(session)) session$token else "no session",
        .context = "ERROR_SYSTEM"
      )
    }

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

    log_debug("Processing error event received", .context = "ERROR_EVENT")

    # For processing errors, we might want to trigger data validation
    if (!is.null(error_info) && !is.null(emit)) {
      if (grepl("data|processing|convert", error_info$message, ignore.case = TRUE)) {
        log_debug("Suggesting data validation after processing error", .context = "ERROR_RECOVERY")
        # Could emit validation_needed event if we had one
      }
    }
  })

  # Validation error event listener
  observeEvent(app_state$events$validation_error, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$high, {
    error_info <- app_state$errors$last_error

    log_debug("Validation error event received", .context = "ERROR_EVENT")

    # For validation errors, clear problematic state
    if (!is.null(error_info) && !is.null(app_state)) {
      log_debug("Clearing validation state after error", .context = "ERROR_RECOVERY")
      # Reset validation-related state if needed
    }
  })

  # Network error event listener
  observeEvent(app_state$events$network_error, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$medium, {
    error_info <- app_state$errors$last_error

    log_debug("Network error event received", .context = "ERROR_EVENT")

    # For network errors (file I/O), we might want to retry or suggest file check
    if (!is.null(error_info) && !is.null(emit)) {
      log_debug("Network error suggests file operation issue", .context = "ERROR_RECOVERY")
    }
  })

  # Recovery completed event listener
  observeEvent(app_state$events$recovery_completed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$low, {
    error_info <- app_state$errors$last_error

    log_debug("Recovery completed event received", .context = "ERROR_EVENT")

    # Update recovery timestamp
    app_state$errors$last_recovery_time <- Sys.time()

    # Log successful recovery
    log_info("Error recovery completed", .context = "ERROR_SYSTEM")
    log_debug_kv(
      recovery_time = as.character(Sys.time()),
      session_id = if(!is.null(session)) session$token else "no session",
      .context = "ERROR_SYSTEM"
    )
  })

  # UI UPDATE EVENTS ========================================================

  # Column choices changed event listener
  observeEvent(app_state$events$column_choices_changed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$low, {
    log_debug("Column choices changed event received", .context = "UI_EVENT")

    if (!is.null(ui_service)) {
      ui_service$update_column_choices()
    } else {
      log_debug("No UI service available for column choices update", .context = "UI_EVENT")
    }
  })

  # Form reset needed event listener
  observeEvent(app_state$events$form_reset_needed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$low, {
    log_debug("Form reset needed event received", .context = "UI_EVENT")

    if (!is.null(ui_service)) {
      ui_service$reset_form_fields()
    } else {
      log_debug("No UI service available for form reset", .context = "UI_EVENT")
    }
  })

  # Form restore needed event listener
  observeEvent(app_state$events$form_restore_needed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$low, {
    log_debug("Form restore needed event received", .context = "UI_EVENT")

    # For form restore, we need metadata from app_state
    # This could be triggered by session restore events
    if (!is.null(ui_service) && !is.null(app_state$session$restore_metadata)) {
      ui_service$update_form_fields(app_state$session$restore_metadata)
    } else {
      log_debug("No UI service or metadata available for form restore", .context = "UI_EVENT")
    }
  })

  # General UI update needed event listener
  observeEvent(app_state$events$ui_update_needed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$low, {
    log_debug("General UI update needed event received", .context = "UI_EVENT")

    # This could trigger multiple UI updates
    if (!is.null(ui_service)) {
      # Update column choices if data is available
      if (!is.null(app_state$data$current_data)) {
        ui_service$update_column_choices()
      }
    }
  })

  log_debug("✅ All event listeners registered (including error handling and UI updates)", .context = "EVENT_SYSTEM")
}

# NOTE: auto_detect_and_update_columns_unified REMOVED
# Replaced with unified autodetect_engine() for consistency and better functionality

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
sync_ui_with_columns_unified <- function(app_state, input, output, session, ui_service = NULL) {
  log_debug_block("UI_SYNC_UNIFIED", "Starting UI synchronization")

  if (is.null(app_state$data$current_data)) {
    log_debug("No data available for UI sync", .context = "UI_SYNC_UNIFIED")
    return()
  }

  data <- app_state$data$current_data
  col_names <- names(data)
  results <- app_state$columns$auto_detect_results

  # Update UI controls with detected columns using centralized service
  tryCatch({
    if (!is.null(ui_service)) {
      # Use centralized UI service with detected selections
      col_choices <- setNames(c("", col_names), c("Vælg kolonne...", col_names))
      selected_columns <- list(
        x_column = results$x_column %||% "",
        y_column = results$y_column %||% "",
        n_column = results$n_column %||% "",
        cl_column = results$cl_column %||% ""
      )

      ui_service$update_column_choices(choices = col_choices, selected = selected_columns)
      log_debug("✅ Used centralized ui_service for column sync", .context = "UI_SYNC_UNIFIED")
    } else {
      # Fallback to direct updates
      if (!is.null(results$x_column)) {
        updateSelectInput(session, "x_column",
                         choices = col_names,
                         selected = results$x_column)
        log_debug_kv(updated_x_column_ui = results$x_column, .context = "UI_SYNC_UNIFIED")
      }

      if (!is.null(results$y_column)) {
        updateSelectInput(session, "y_column",
                         choices = col_names,
                         selected = results$y_column)
        log_debug_kv(updated_y_column_ui = results$y_column, .context = "UI_SYNC_UNIFIED")
      }

      if (!is.null(results$n_column)) {
        updateSelectInput(session, "n_column",
                         choices = c("Ingen" = "", col_names),
                         selected = results$n_column)
        log_debug_kv(updated_n_column_ui = results$n_column, .context = "UI_SYNC_UNIFIED")
      }

      log_debug("✅ UI synchronization completed", .context = "UI_SYNC_UNIFIED")
    }

  }, error = function(e) {
    log_error("UI sync error", .context = "UI_SYNC_UNIFIED")
    log_debug_kv(error_message = e$message, .context = "UI_SYNC_UNIFIED")
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
update_column_choices_unified <- function(app_state, input, output, session, ui_service = NULL) {
  log_debug_block("COLUMN_CHOICES_UNIFIED", "Starting column choices update")

  # Check if we should skip during table operations
  if (app_state$data$updating_table) {
    log_debug("Skipping - table update in progress", .context = "COLUMN_CHOICES_UNIFIED")
    return()
  }

  # Skip if auto-detect is in progress
  if (app_state$columns$auto_detect_in_progress) {
    log_debug("Skipping - auto-detect in progress", .context = "COLUMN_CHOICES_UNIFIED")
    return()
  }

  # Skip if UI sync is needed (to avoid race conditions)
  if (app_state$columns$ui_sync_needed) {
    log_debug("Skipping - UI sync pending", .context = "COLUMN_CHOICES_UNIFIED")
    return()
  }

  # Get current data
  if (is.null(app_state$data$current_data)) {
    log_debug("No data available", .context = "COLUMN_CHOICES_UNIFIED")
    return()
  }

  data <- app_state$data$current_data
  all_cols <- names(data)
  log_debug_kv(
    available_columns = paste(all_cols, collapse = ", "),
    .context = "COLUMN_CHOICES_UNIFIED"
  )

  if (length(all_cols) > 0) {
    # Create column choices
    col_choices <- setNames(
      c("", all_cols),
      c("Vælg kolonne...", all_cols)
    )

    # Update UI controls using centralized service
    tryCatch({
      if (!is.null(ui_service)) {
        ui_service$update_column_choices(choices = col_choices)
        log_debug("✅ Used centralized ui_service for column choices", .context = "COLUMN_CHOICES_UNIFIED")
      } else {
        # Fallback to direct updates
        updateSelectizeInput(session, "x_column", choices = col_choices)
        updateSelectizeInput(session, "y_column", choices = col_choices)
        updateSelectizeInput(session, "n_column", choices = col_choices)
        updateSelectizeInput(session, "cl_column", choices = col_choices)

        log_debug("✅ Column choices updated", .context = "COLUMN_CHOICES_UNIFIED")
      }

    }, error = function(e) {
      log_error("Error updating UI", .context = "COLUMN_CHOICES_UNIFIED")
      log_debug_kv(error_message = e$message, .context = "COLUMN_CHOICES_UNIFIED")
    })
  }
}