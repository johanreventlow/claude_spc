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
  log_debug("setup_event_listeners function started", "EVENT_SYSTEM")
  log_debug("Setting up unified event listeners", .context = "EVENT_SYSTEM")
  log_debug("EVENT_SETUP: Starting data lifecycle events", .context = "EVENT_SYSTEM")

  # DATA LIFECYCLE EVENTS
  observeEvent(app_state$events$data_loaded, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$STATE_MANAGEMENT, {
    log_debug("data_loaded event received", .context = "EVENT")

    # FASE 3: Unfreeze autodetect system when new data is loaded
    if (!is.null(app_state$autodetect)) {
      app_state$autodetect$frozen_until_next_trigger <- FALSE
      log_debug("Autodetect system unfrozen due to new data", .context = "EVENT")
    }

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
    app_state$columns$auto_detect$in_progress <- TRUE

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
      app_state$columns$auto_detect$in_progress <- FALSE
    })
  })

  observeEvent(app_state$events$auto_detection_completed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$AUTO_DETECT, {
    log_debug("auto_detection_completed event received", .context = "EVENT")

    # Update state
    app_state$columns$auto_detect$in_progress <- FALSE
    app_state$columns$auto_detect$completed <- TRUE

    # Trigger UI sync if columns were detected
    auto_detect_results <- isolate(app_state$columns$auto_detect$results)
    log_debug(paste("DIAGNOSTIC: auto_detect_results =", capture.output(str(auto_detect_results))), .context = "EVENT")

    if (!is.null(auto_detect_results)) {
      log_debug("Auto-detection results available, emitting ui_sync_needed", .context = "EVENT")
      log_debug(paste("Results contain:", paste(names(auto_detect_results), collapse = ", ")), .context = "EVENT")
      emit$ui_sync_needed()
    } else {
      log_debug("❌ No auto-detection results found - UI sync skipped", .context = "EVENT")
      log_debug("DIAGNOSTIC: Checking if autodetect engine was called...", .context = "EVENT")
    }
  })

  # UI SYNCHRONIZATION EVENTS
  log_debug("About to register ui_sync_needed observer", "DEBUG")
  log_debug("Registering ui_sync_needed observer", .context = "EVENT_SETUP")
  observeEvent(app_state$events$ui_sync_needed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$UI_SYNC, {
    log_debug("ui_sync_needed observer triggered!", "DEBUG")
    log_debug("ui_sync_needed event received", .context = "EVENT")

    # Add extra debugging
    log_debug("About to call sync_ui_with_columns_unified", "DEBUG")
    log_debug("About to call sync_ui_with_columns_unified", .context = "EVENT")

    tryCatch({
      # Perform UI synchronization
      sync_ui_with_columns_unified(app_state, input, output, session, ui_service)
      log_debug("sync_ui_with_columns_unified completed successfully", .context = "EVENT")
    }, error = function(e) {
      log_debug(paste("sync_ui_with_columns_unified ERROR:", e$message), .context = "EVENT")
    })

    # Mark sync as completed
    emit$ui_sync_completed()
  })

  observeEvent(app_state$events$ui_sync_completed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$UI_SYNC, {
    log_debug("ui_sync_completed event received", .context = "EVENT")

    # Update timestamp
    app_state$columns$ui_sync$last_sync_time <- Sys.time()

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
  observeEvent(app_state$events$session_started, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$AUTO_DETECT, {
    log_debug("session_started event received", .context = "EVENT")

    # FASE 3: Session start trigger for name-only detection
    autodetect_engine(
      data = NULL,  # No data available at session start
      trigger_type = "session_start",
      app_state = app_state,
      emit = emit
    )
  })

  observeEvent(app_state$events$manual_autodetect_button, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$AUTO_DETECT, {
    log_debug("manual_autodetect_button event received", .context = "EVENT")

    # FASE 3: Manual trigger always runs, bypassing frozen state
    autodetect_engine(
      data = app_state$data$current_data,
      trigger_type = "manual",  # This bypasses frozen state check
      app_state = app_state,
      emit = emit
    )
  })

  observeEvent(app_state$events$session_reset, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$CLEANUP, {
    log_debug("session_reset event received", .context = "EVENT")

    # Reset all state to initial values
    app_state$data$current_data <- NULL
    app_state$columns$auto_detect$in_progress <- FALSE
    app_state$columns$auto_detect$completed <- FALSE
    app_state$columns$auto_detect$results <- NULL

    # FASE 3: Reset frozen state
    app_state$autodetect$frozen_until_next_trigger <- FALSE
    app_state$autodetect$last_run <- NULL

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

  # DISABLED: Column choices changed event listener - problematisk da den clearer dropdowns
  # Dette event system kaldte update_column_choices() uden parametre, hvilket resulterede i
  # choices=NULL og selected=NULL, og dermed clearede alle dropdown værdier efter autodetect.
  # UI sync håndteres allerede korrekt via ui_sync_needed event systemet.

  # observeEvent(app_state$events$column_choices_changed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$low, {
  #   log_debug("Column choices changed event received", .context = "UI_EVENT")
  #
  #   if (!is.null(ui_service)) {
  #     ui_service$update_column_choices()  # <-- PROBLEM: Ingen parametre resulterer i NULL choices/selected
  #   } else {
  #     log_debug("No UI service available for column choices update", .context = "UI_EVENT")
  #   }
  # })

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

  # INPUT CHANGE OBSERVERS ===================================================
  # Keep app_state$columns aligned with UI selections when user manually changes dropdowns

  columns_to_observe <- c("x_column", "y_column", "n_column", "skift_column", "frys_column", "kommentar_column")

  for (col in columns_to_observe) {
    observeEvent(input[[col]], {
      input_received_time <- Sys.time()
      new_value <- input[[col]]

      # DROPDOWN DEBUGGING: Log input change details
      old_value <- isolate(app_state$columns[[col]]) %||% ""
      log_debug(paste("Input", col, "changed from", paste0("'", old_value, "'"),
                     "to", paste0("'", new_value, "'")), "DROPDOWN_DEBUG")

      # TIMING LOGGING: Calculate time since last programmatic update
      last_update_time <- isolate(app_state$ui$last_programmatic_update)
      time_since_update <- if (!is.null(last_update_time)) {
        as.numeric(difftime(input_received_time, last_update_time, units = "secs")) * 1000
      } else { NA }

      log_debug(paste("LOOP_PROTECTION:", col, "input received",
                     if (!is.na(time_since_update)) paste("(", round(time_since_update, 2), "ms after last update)") else ""),
               .context = "LOOP_PROTECTION")

      protection_active <- isolate(app_state$ui$updating_programmatically)

      # FREEZE-AWARE LOGGING: Observe freeze state without modification
      freeze_state <- if (!is.null(app_state$autodetect)) {
        isolate(app_state$autodetect$frozen_until_next_trigger) %||% FALSE
      } else { FALSE }

      log_debug(paste("LOOP_PROTECTION check for", col, ": updating_programmatically =", protection_active,
                     ", autodetect frozen =", freeze_state), "DROPDOWN_DEBUG")

      # TOKEN CONSUMPTION: Check for pending programmatic input tokens
      pending_token <- app_state$ui$pending_programmatic_inputs[[col]]

      if (!is.null(pending_token) && pending_token$value == new_value) {
        # CONSUME TOKEN: This is a programmatic input, don't emit event
        app_state$ui$pending_programmatic_inputs[[col]] <- NULL
        app_state$columns[[col]] <- new_value

        # FASE 3: PERFORMANCE METRICS - Track token consumption
        isolate({
          app_state$ui$performance_metrics$tokens_consumed <- app_state$ui$performance_metrics$tokens_consumed + 1L
        })

        log_debug(paste("Programmatic input consumed for"), "TOKEN_DEBUG")
        return()
      }

      # LEGACY LOOP PROTECTION: Skip event emission if we're in the middle of programmatic updates
      if (protection_active) {
        log_debug(paste("LOOP_PROTECTION ACTIVE - Skipping event emission for"), "DROPDOWN_DEBUG")
        log_debug(paste("Programmatic update detected - value", paste0("'", new_value, "'"), "accepted but no events emitted for", col), "DROPDOWN_DEBUG")
        # Still update app_state to keep it synchronized, but don't emit events
        app_state$columns[[col]] <- new_value
        return()
      }

      # Update app_state to keep it synchronized with UI
      app_state$columns[[col]] <- new_value

      # Only emit events for user-driven changes (not programmatic updates)
      if (exists("column_choices_changed", envir = as.environment(emit))) {
        log_debug(paste("USER-DRIVEN change detected for"), "DROPDOWN_DEBUG")
        log_debug(paste("User changed", col, "to", paste0("'", new_value, "'"), "- triggering column_choices_changed event"), "DROPDOWN_DEBUG")
        emit$column_choices_changed()
      }

      log_debug(paste("✅ app_state$columns$", col, "synchronized with input"), .context = "INPUT_OBSERVER")
    }, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$MEDIUM)
  }

  # PASSIVE TIMING OBSERVER: Monitor system performance without interfering
  # This observer tracks timing metrics for optimization without emitting events
  if (!is.null(app_state$ui)) {
    observeEvent(app_state$ui$last_programmatic_update, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$lowest, {
      current_time <- Sys.time()
      last_update <- isolate(app_state$ui$last_programmatic_update)

      if (!is.null(last_update)) {
        # FREEZE-AWARE TIMING: Track performance metrics with context
        freeze_state <- if (!is.null(app_state$autodetect)) {
          isolate(app_state$autodetect$frozen_until_next_trigger) %||% FALSE
        } else { FALSE }

        autodetect_in_progress <- if (!is.null(app_state$columns)) {
          isolate(app_state$columns$auto_detect$in_progress) %||% FALSE
        } else { FALSE }

        log_debug(paste("TIMING_MONITOR: System state at update - frozen:", freeze_state,
                       ", autodetect active:", autodetect_in_progress), .context = "TIMING_MONITOR")
      }
    })
  }

  log_debug("✅ All event listeners registered (including input observers and timing monitor)", .context = "EVENT_SYSTEM")
}

# OVERFLØDIGT: Fjernet duplikeret sync_ui_with_columns_unified funktion
# Den korrekte funktion findes længere nede i filen (linje 427)

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
  log_debug("sync_ui_with_columns_unified function STARTED", "DEBUG")
  tryCatch({
    log_debug_block("UI_SYNC_UNIFIED", "Starting UI synchronization")
    log_debug("log_debug_block completed", "DEBUG")
  }, error = function(e) {
    log_debug(paste("log_debug_block ERROR:", e$message), "DEBUG")
  })

  # DROPDOWN DEBUGGING: Log autodetect results that will be used
  log_debug("About to get auto_detect_results", "DEBUG")
  auto_detect_results <- isolate(app_state$columns$auto_detect$results)
  log_debug("Got auto_detect_results, about to log_debug", "DEBUG")
  log_debug("sync_ui_with_columns_unified called", "DROPDOWN_DEBUG")
  log_debug("log_debug completed, continuing", "DEBUG")
  if (!is.null(auto_detect_results)) {
    log_debug("Using autodetect results:", "DROPDOWN_DEBUG")
    for (col_name in names(auto_detect_results)) {
      log_debug(paste(col_name, "=", auto_detect_results[[col_name]]), "DROPDOWN_DEBUG")
    }
  } else {
    log_debug("No autodetect results available", "DROPDOWN_DEBUG")
  }

  # Use isolate() to access reactive values safely
  current_data <- isolate(app_state$data$current_data)
  if (is.null(current_data)) {
    log_debug("No data available for UI sync", .context = "UI_SYNC_UNIFIED")
    return()
  }

  data <- current_data
  col_names <- names(data)
  columns_state <- isolate(app_state$columns)

  # Update UI controls with detected columns using centralized service
  log_debug("About to check ui_service", "DEBUG")
  log_debug(paste("ui_service is null:", is.null(ui_service)), "DEBUG")
  tryCatch({
    if (!is.null(ui_service)) {
      log_debug("ui_service is available, proceeding with UI updates", "DEBUG")
      # Use centralized UI service with detected selections for all 6 columns
      col_choices <- setNames(c("", col_names), c("Vælg kolonne...", col_names))
      selected_columns <- list(
        x_column = isolate(columns_state$x_column) %||% "",
        y_column = isolate(columns_state$y_column) %||% "",
        n_column = isolate(columns_state$n_column) %||% "",
        skift_column = isolate(columns_state$skift_column) %||% "",
        frys_column = isolate(columns_state$frys_column) %||% "",
        kommentar_column = isolate(columns_state$kommentar_column) %||% ""
      )

      # DROPDOWN DEBUGGING: Log alle 6 kolonner eksplicit
      log_debug("All 6 column values being sent to UI:", "DROPDOWN_DEBUG")
      for (col_name in names(selected_columns)) {
        log_debug(paste(col_name, "=", paste0("'", selected_columns[[col_name]], "'")), "DROPDOWN_DEBUG")
      }

      log_debug("About to call ui_service$update_column_choices", "DEBUG")
      ui_service$update_column_choices(
        choices = col_choices,
        selected = selected_columns,
        columns = c("x_column", "y_column", "n_column", "skift_column", "frys_column", "kommentar_column")
      )
      log_debug("✅ Used centralized ui_service for all 6 column sync", .context = "UI_SYNC_UNIFIED")
    } else {
      # Fallback to direct updates using safe wrapper to prevent loops
      standard_choices <- setNames(c("", col_names), c("Vælg kolonne...", col_names))

      safe_programmatic_ui_update(session, app_state, function() {
        # Primary columns (required)
        x_col_val <- isolate(columns_state$x_column)
        if (!is.null(x_col_val)) {
          updateSelectizeInput(session, "x_column",
                             choices = standard_choices,
                             selected = x_col_val)
          log_debug_kv(updated_x_column_ui = x_col_val, .context = "UI_SYNC_UNIFIED")
        }

        y_col_val <- isolate(columns_state$y_column)
        if (!is.null(y_col_val)) {
          updateSelectizeInput(session, "y_column",
                             choices = standard_choices,
                             selected = y_col_val)
          log_debug_kv(updated_y_column_ui = y_col_val, .context = "UI_SYNC_UNIFIED")
        }

        n_col_val <- isolate(columns_state$n_column)
        if (!is.null(n_col_val)) {
          updateSelectizeInput(session, "n_column",
                             choices = standard_choices,
                             selected = n_col_val)
          log_debug_kv(updated_n_column_ui = n_col_val, .context = "UI_SYNC_UNIFIED")
        }

        # Control columns (optional)
        skift_col_val <- isolate(columns_state$skift_column)
        if (!is.null(skift_col_val)) {
          updateSelectizeInput(session, "skift_column",
                             choices = standard_choices,
                             selected = skift_col_val)
          log_debug_kv(updated_skift_column_ui = skift_col_val, .context = "UI_SYNC_UNIFIED")
        }

        frys_col_val <- isolate(columns_state$frys_column)
        if (!is.null(frys_col_val)) {
          updateSelectizeInput(session, "frys_column",
                             choices = standard_choices,
                             selected = frys_col_val)
          log_debug_kv(updated_frys_column_ui = frys_col_val, .context = "UI_SYNC_UNIFIED")
        }

        kommentar_col_val <- isolate(columns_state$kommentar_column)
        if (!is.null(kommentar_col_val)) {
          updateSelectizeInput(session, "kommentar_column",
                             choices = standard_choices,
                             selected = kommentar_col_val)
          log_debug_kv(updated_kommentar_column_ui = kommentar_col_val, .context = "UI_SYNC_UNIFIED")
        }

        log_debug("✅ All 6 columns UI synchronization completed", .context = "UI_SYNC_UNIFIED")
      })
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
  if (app_state$columns$auto_detect$in_progress) {
    log_debug("Skipping - auto-detect in progress", .context = "COLUMN_CHOICES_UNIFIED")
    return()
  }

  # Skip if UI sync is needed (to avoid race conditions)
  if (app_state$columns$ui_sync$needed) {
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

    # Retain existing selections from both input and app_state
    columns_to_update <- c("x_column", "y_column", "n_column", "skift_column", "frys_column", "kommentar_column")
    current_selections <- list()

    for (col in columns_to_update) {
      # Priority: input[[col]] > app_state$columns[[col]] > ""
      current_val <- if (!is.null(input[[col]]) && input[[col]] != "") {
        input[[col]]
      } else if (!is.null(isolate(app_state$columns[[col]]))) {
        isolate(app_state$columns[[col]])
      } else {
        ""
      }
      current_selections[[col]] <- current_val
      log_debug(paste("Retaining selection for", col, ":", current_val), .context = "COLUMN_CHOICES_UNIFIED")
    }

    # Update UI controls using centralized service
    tryCatch({
      if (!is.null(ui_service)) {
        ui_service$update_column_choices(choices = col_choices, selected = current_selections)
        log_debug("✅ Used centralized ui_service for column choices with retained selections", .context = "COLUMN_CHOICES_UNIFIED")
      } else {
        # Fallback to direct updates with retained selections
        for (col in columns_to_update) {
          updateSelectizeInput(session, col, choices = col_choices, selected = current_selections[[col]])
        }
        log_debug("✅ Column choices updated with retained selections", .context = "COLUMN_CHOICES_UNIFIED")
      }

    }, error = function(e) {
      log_error("Error updating UI", .context = "COLUMN_CHOICES_UNIFIED")
      log_debug_kv(error_message = e$message, .context = "COLUMN_CHOICES_UNIFIED")
    })
  }
}