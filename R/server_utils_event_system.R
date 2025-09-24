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
#' This function creates shiny::observeEvent() handlers for all events
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
#' in one place, replacing the scattered shiny::observeEvent() calls
#' and bridge observers that were previously spread across
#' multiple files.
#'
#' All observers use ignoreInit = TRUE to prevent firing at startup
#' and appropriate priorities to ensure correct execution order.
#'
setup_event_listeners <- function(app_state, emit, input, output, session, ui_service = NULL) {
  # Setting up unified event listeners

  # DATA LIFECYCLE EVENTS
  shiny::observeEvent(app_state$events$data_loaded, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$STATE_MANAGEMENT, {
    # Data loaded event handler

    # FASE 3: Unfreeze autodetect system when new data is loaded
    app_state$columns$auto_detect$frozen_until_next_trigger <- FALSE
    # Autodetect system unfrozen

    # Trigger auto-detection after data is loaded
    if (!is.null(app_state$data$current_data)) {
      # Triggering auto-detection
      emit$auto_detection_started()
    }
  })

  shiny::observeEvent(app_state$events$data_changed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$DATA_PROCESSING, {
    # Data changed event handler

    # Update column choices when data changes
    update_column_choices_unified(app_state, input, output, session, ui_service)
  })

  # AUTO-DETECTION EVENTS
  shiny::observeEvent(app_state$events$auto_detection_started, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$AUTO_DETECT, {
    # Auto-detection started event handler

    # Set auto-detection in progress
    app_state$columns$auto_detect$in_progress <- TRUE

    # Perform auto-detection using unified engine
    safe_operation(
      "Auto-detection processing",
      code = {
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
      },
      fallback = {
        app_state$columns$auto_detect$in_progress <- FALSE
      },
      session = NULL,
      error_type = "processing",
      emit = emit,
      app_state = app_state
    )
  })

  shiny::observeEvent(app_state$events$auto_detection_completed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$AUTO_DETECT, {

    # Update state
    app_state$columns$auto_detect$in_progress <- FALSE
    app_state$columns$auto_detect$completed <- TRUE

    # Trigger UI sync if columns were detected
    auto_detect_results <- shiny::isolate(app_state$columns$auto_detect$results)

    if (!is.null(auto_detect_results)) {
      emit$ui_sync_needed()
    } else {
    }
  })

  # UI SYNCHRONIZATION EVENTS
  shiny::observeEvent(app_state$events$ui_sync_needed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$UI_SYNC, {

    # Add extra debugging

    safe_operation(
      "UI synchronization",
      code = {
        # Perform UI synchronization
        sync_ui_with_columns_unified(app_state, input, output, session, ui_service)
      },
      fallback = NULL,
      session = session,
      error_type = "processing",
      emit = emit,
      app_state = app_state
    )

    # Mark sync as completed
    emit$ui_sync_completed()
  })

  shiny::observeEvent(app_state$events$ui_sync_completed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$UI_SYNC, {

    # Update timestamp
    app_state$columns$ui_sync$last_sync_time <- Sys.time()

    # Trigger navigation change to update plots
    emit$navigation_changed()
  })

  # NAVIGATION EVENTS
  shiny::observeEvent(app_state$events$navigation_changed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$STATUS_UPDATES, {

    # Increment navigation trigger to update all eventReactive components
    app_state$navigation$trigger <- app_state$navigation$trigger + 1L
  })

  # TEST MODE EVENTS
  shiny::observeEvent(app_state$events$test_mode_ready, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$AUTO_DETECT, {

    # In test mode, immediately start auto-detection
    if (!is.null(app_state$data$current_data)) {
      emit$auto_detection_started()
    }
  })

  # SESSION LIFECYCLE EVENTS
  shiny::observeEvent(app_state$events$session_started, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$AUTO_DETECT, {

    # FASE 3: Session start trigger for name-only detection
    autodetect_engine(
      data = NULL,  # No data available at session start
      trigger_type = "session_start",
      app_state = app_state,
      emit = emit
    )
  })

  shiny::observeEvent(app_state$events$manual_autodetect_button, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$AUTO_DETECT, {

    # FASE 3: Manual trigger always runs, bypassing frozen state
    autodetect_engine(
      data = app_state$data$current_data,
      trigger_type = "manual",  # This bypasses frozen state check
      app_state = app_state,
      emit = emit
    )
  })

  shiny::observeEvent(app_state$events$session_reset, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$CLEANUP, {

    # Reset all state to initial values
    app_state$data$current_data <- NULL
    app_state$columns$auto_detect$in_progress <- FALSE
    app_state$columns$auto_detect$completed <- FALSE
    app_state$columns$auto_detect$results <- NULL

    # FASE 3: Reset frozen state
    app_state$columns$auto_detect$frozen_until_next_trigger <- FALSE
    app_state$columns$auto_detect$last_run <- NULL

  })

  # ERROR HANDLING EVENTS ===================================================

  # General error event listener
  shiny::observeEvent(app_state$events$error_occurred, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$highest, {
    error_info <- app_state$errors$last_error


    # Centralized error logging with context
    log_error("Error event triggered", "ERROR_SYSTEM")
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
  shiny::observeEvent(app_state$events$processing_error, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$high, {
    error_info <- app_state$errors$last_error


    # For processing errors, we might want to trigger data validation
    if (!is.null(error_info) && !is.null(emit)) {
      if (grepl("data|processing|convert", error_info$message, ignore.case = TRUE)) {
        # Could emit validation_needed event if we had one
      }
    }
  })

  # Validation error event listener
  shiny::observeEvent(app_state$events$validation_error, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$high, {
    error_info <- app_state$errors$last_error


    # For validation errors, clear problematic state
    if (!is.null(error_info) && !is.null(app_state)) {
      # Reset validation-related state if needed
    }
  })

  # Network error event listener
  shiny::observeEvent(app_state$events$network_error, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$medium, {
    error_info <- app_state$errors$last_error


    # For network errors (file I/O), we might want to retry or suggest file check
    if (!is.null(error_info) && !is.null(emit)) {
    }
  })

  # Recovery completed event listener
  shiny::observeEvent(app_state$events$recovery_completed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$low, {
    error_info <- app_state$errors$last_error


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

  # NOTE: column_choices_changed observer disabled due to UI clearing issue
  # Event still emitted for tracking, but UI sync handled via ui_sync_needed

  # Form reset needed event listener
  shiny::observeEvent(app_state$events$form_reset_needed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$low, {

    if (!is.null(ui_service)) {
      ui_service$reset_form_fields()
    } else {
    }
  })

  # Form restore needed event listener
  shiny::observeEvent(app_state$events$form_restore_needed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$low, {

    # For form restore, we need metadata from app_state
    # This could be triggered by session restore events
    if (!is.null(ui_service) && !is.null(app_state$session$restore_metadata)) {
      ui_service$update_form_fields(app_state$session$restore_metadata)
    } else {
    }
  })

  # General UI update needed event listener
  shiny::observeEvent(app_state$events$ui_update_needed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$low, {

    # This could trigger multiple UI updates
    if (!is.null(ui_service)) {
      # Update column choices if data is available
      if (!is.null(app_state$data$current_data)) {
        ui_service$update_column_choices()
      }
    }
  })

  # INPUT CHANGE OBSERVERS ===================================================
  # Keep app_state$columns aligned with UI selections when user manually changes dropdowns

  columns_to_observe <- c("x_column", "y_column", "n_column", "skift_column", "frys_column", "kommentar_column")

  for (col in columns_to_observe) {
    shiny::observeEvent(input[[col]], {
      input_received_time <- Sys.time()
      new_value <- input[[col]]

      # DROPDOWN DEBUGGING: Log input change details
      old_value <- shiny::isolate(app_state$columns[[col]]) %||% ""
      #               "to", paste0("'", new_value, "'")), "DROPDOWN_DEBUG")

      # TIMING LOGGING: Calculate time since last programmatic update
      last_update_time <- shiny::isolate(app_state$ui$last_programmatic_update)
      time_since_update <- if (!is.null(last_update_time)) {
        as.numeric(difftime(input_received_time, last_update_time, units = "secs")) * 1000
      } else { NA }

      #               if (!is.na(time_since_update)) paste("(", round(time_since_update, 2), "ms after last update)") else ""),
      #         .context = "LOOP_PROTECTION")

      # FREEZE-AWARE LOGGING: Observe freeze state without modification
      freeze_state <- shiny::isolate(app_state$columns$auto_detect$frozen_until_next_trigger) %||% FALSE

      #               ", autodetect frozen =", freeze_state), "DROPDOWN_DEBUG")

      # TOKEN CONSUMPTION: Primary and only loop protection mechanism
      # Check for pending programmatic input tokens
      pending_token <- app_state$ui$pending_programmatic_inputs[[col]]

      if (!is.null(pending_token) && pending_token$value == new_value) {
        # CONSUME TOKEN: This is a programmatic input, don't emit event
        app_state$ui$pending_programmatic_inputs[[col]] <- NULL
        app_state$columns[[col]] <- new_value

        # PERFORMANCE METRICS: Track token consumption for monitoring
        shiny::isolate({
          app_state$ui$performance_metrics$tokens_consumed <- app_state$ui$performance_metrics$tokens_consumed + 1L
        })

      #               "- no event emitted"), "TOKEN_DEBUG")
        return()
      }

      # Update app_state to keep it synchronized with UI
      app_state$columns[[col]] <- new_value

      # Only emit events for user-driven changes (not programmatic updates)
      if (exists("column_choices_changed", envir = as.environment(emit))) {
        emit$column_choices_changed()
      }

    }, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$MEDIUM)
  }

  # PASSIVE TIMING OBSERVER: Monitor system performance without interfering
  # This observer tracks timing metrics for optimization without emitting events
  if (!is.null(app_state$ui)) {
    shiny::observeEvent(app_state$ui$last_programmatic_update, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$lowest, {
      current_time <- Sys.time()
      last_update <- shiny::isolate(app_state$ui$last_programmatic_update)

      if (!is.null(last_update)) {
        # FREEZE-AWARE TIMING: Track performance metrics with context
        freeze_state <- shiny::isolate(app_state$columns$auto_detect$frozen_until_next_trigger) %||% FALSE

        autodetect_in_progress <- if (!is.null(app_state$columns)) {
          shiny::isolate(app_state$columns$auto_detect$in_progress) %||% FALSE
        } else { FALSE }

      #               ", autodetect active:", autodetect_in_progress), .context = "TIMING_MONITOR")
      }
    })
  }

}

# NOTE: Duplikeret sync_ui_with_columns_unified funktion fjernet
# Den korrekte funktion findes længere nede i filen

# NOTE: auto_detect_and_update_columns_unified deprecated
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
  safe_operation(
    "UI sync debug block",
    code = {
      log_debug_block("UI_SYNC_UNIFIED", "Starting UI synchronization")
    },
    fallback = NULL,
    session = session,
    error_type = "general"
  )

  # DROPDOWN DEBUGGING: Log autodetect results that will be used
  auto_detect_results <- shiny::isolate(app_state$columns$auto_detect$results)
  if (!is.null(auto_detect_results)) {
    for (col_name in names(auto_detect_results)) {
    }
  } else {
  }

  # Use shiny::isolate() to access reactive values safely
  current_data <- shiny::isolate(app_state$data$current_data)
  if (is.null(current_data)) {
    return()
  }

  data <- current_data
  col_names <- names(data)
  columns_state <- shiny::isolate(app_state$columns)

  # Update UI controls with detected columns using centralized service
  safe_operation(
    "UI controls update with detected columns",
    code = {
      if (!is.null(ui_service)) {
        # Use centralized UI service with detected selections for all 6 columns
        col_choices <- setNames(c("", col_names), c("Vælg kolonne...", col_names))
        selected_columns <- list(
          x_column = shiny::isolate(columns_state$x_column) %||% "",
          y_column = shiny::isolate(columns_state$y_column) %||% "",
          n_column = shiny::isolate(columns_state$n_column) %||% "",
          skift_column = shiny::isolate(columns_state$skift_column) %||% "",
          frys_column = shiny::isolate(columns_state$frys_column) %||% "",
          kommentar_column = shiny::isolate(columns_state$kommentar_column) %||% ""
        )

        # DROPDOWN DEBUGGING: Log alle 6 kolonner eksplicit
        for (col_name in names(selected_columns)) {
        }

        ui_service$update_column_choices(
          choices = col_choices,
          selected = selected_columns,
          columns = c("x_column", "y_column", "n_column", "skift_column", "frys_column", "kommentar_column")
        )
      } else {
        # Fallback to direct updates using safe wrapper to prevent loops
        standard_choices <- setNames(c("", col_names), c("Vælg kolonne...", col_names))

        safe_programmatic_ui_update(session, app_state, function() {
          # Primary columns (required)
          x_col_val <- shiny::isolate(columns_state$x_column)
          if (!is.null(x_col_val)) {
            shiny::updateSelectizeInput(session, "x_column",
                               choices = standard_choices,
                               selected = x_col_val)
            log_debug_kv(updated_x_column_ui = x_col_val, .context = "UI_SYNC_UNIFIED")
          }

          y_col_val <- shiny::isolate(columns_state$y_column)
          if (!is.null(y_col_val)) {
            shiny::updateSelectizeInput(session, "y_column",
                               choices = standard_choices,
                               selected = y_col_val)
            log_debug_kv(updated_y_column_ui = y_col_val, .context = "UI_SYNC_UNIFIED")
          }

          n_col_val <- shiny::isolate(columns_state$n_column)
          if (!is.null(n_col_val)) {
            shiny::updateSelectizeInput(session, "n_column",
                               choices = standard_choices,
                               selected = n_col_val)
            log_debug_kv(updated_n_column_ui = n_col_val, .context = "UI_SYNC_UNIFIED")
          }

          # Control columns (optional)
          skift_col_val <- shiny::isolate(columns_state$skift_column)
          if (!is.null(skift_col_val)) {
            shiny::updateSelectizeInput(session, "skift_column",
                               choices = standard_choices,
                               selected = skift_col_val)
            log_debug_kv(updated_skift_column_ui = skift_col_val, .context = "UI_SYNC_UNIFIED")
          }

          frys_col_val <- shiny::isolate(columns_state$frys_column)
          if (!is.null(frys_col_val)) {
            shiny::updateSelectizeInput(session, "frys_column",
                               choices = standard_choices,
                               selected = frys_col_val)
            log_debug_kv(updated_frys_column_ui = frys_col_val, .context = "UI_SYNC_UNIFIED")
          }

          kommentar_col_val <- shiny::isolate(columns_state$kommentar_column)
          if (!is.null(kommentar_col_val)) {
            shiny::updateSelectizeInput(session, "kommentar_column",
                               choices = standard_choices,
                               selected = kommentar_col_val)
            log_debug_kv(updated_kommentar_column_ui = kommentar_col_val, .context = "UI_SYNC_UNIFIED")
          }

        })
      }
    },
    fallback = NULL,
    session = session,
    error_type = "processing",
    emit = emit,
    app_state = app_state
  )
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
    return()
  }

  # Skip if auto-detect is in progress
  if (app_state$columns$auto_detect$in_progress) {
    return()
  }

  # Skip if UI sync is needed (to avoid race conditions)
  if (app_state$columns$ui_sync$needed) {
    return()
  }

  # Get current data
  if (is.null(app_state$data$current_data)) {
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

    # Retain existing selections from both input and app_state
    columns_to_update <- c("x_column", "y_column", "n_column", "skift_column", "frys_column", "kommentar_column")
    current_selections <- list()

    for (col in columns_to_update) {
      # Priority: input[[col]] > app_state$columns[[col]] > ""
      current_val <- if (!is.null(input[[col]]) && input[[col]] != "") {
        input[[col]]
      } else if (!is.null(shiny::isolate(app_state$columns[[col]]))) {
        shiny::isolate(app_state$columns[[col]])
      } else {
        ""
      }
      current_selections[[col]] <- current_val
    }

    # Update UI controls using centralized service
    safe_operation(
      "Column choices UI update",
      code = {
        if (!is.null(ui_service)) {
          ui_service$update_column_choices(choices = col_choices, selected = current_selections)
        } else {
          # Fallback to direct updates with retained selections
          for (col in columns_to_update) {
            shiny::updateSelectizeInput(session, col, choices = col_choices, selected = current_selections[[col]])
          }
        }
      },
      fallback = NULL,
      session = session,
      error_type = "processing"
    )
  }
}
