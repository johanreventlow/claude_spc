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
  # DUPLICATE PREVENTION: Check if optimized listeners are already active
  if (exists("optimized_listeners_active", envir = app_state) && app_state$optimized_listeners_active) {
    stop("Cannot setup standard listeners while optimized listeners are active. This would cause duplicate execution.")
  }

  # Setting up unified event listeners
  # Mark that standard listeners are active to prevent duplicate optimized listeners
  app_state$standard_listeners_active <- TRUE

  resolve_column_update_reason <- function(context) {
    if (is.null(context)) {
      return("manual")
    }

    ctx <- tolower(context)

    if (grepl("edit|change|modify|column", ctx, ignore.case = FALSE)) {
      return("edit")
    }

    if (grepl("session", ctx, ignore.case = FALSE)) {
      return("session")
    }

    if (grepl("load|upload|file|new", ctx, ignore.case = FALSE)) {
      return("upload")
    }

    "manual"
  }

  # DATA LIFECYCLE EVENTS (CONSOLIDATED - FASE 2.2) ========================

  # Consolidated data update handler - handles both data loading and changes
  shiny::observeEvent(app_state$events$data_updated, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$STATE_MANAGEMENT, {
    # Get data update context for intelligent handling
    update_context <- app_state$last_data_update_context

    # UNIFIED DATA UPDATE LOGIC - combines previous data_loaded + data_changed handlers

    # FASE 3: Unfreeze autodetect system when data is updated
    app_state$columns$auto_detect$frozen_until_next_trigger <- FALSE

    # Context-aware processing based on update type
    if (!is.null(update_context)) {
      context <- update_context$context %||% "general"
      column_update_reason <- resolve_column_update_reason(context)

      if (context == "legacy_data_loaded" || grepl("load|upload|new", context, ignore.case = TRUE)) {
        # Data loading path - trigger auto-detection
        if (!is.null(app_state$data$current_data)) {
          emit$auto_detection_started()
        }

      } else if (context == "legacy_data_changed" || grepl("change|edit|modify", context, ignore.case = TRUE)) {
        # Data change path - update column choices AND trigger plot regeneration
        safe_operation(
          "Update column choices on data change",
          code = {
            update_column_choices_unified(app_state, input, output, session, ui_service, reason = column_update_reason)
          }
        )

        # Trigger plot regeneration when data is edited in table
        emit$navigation_changed()

      } else {
        # General data update - NO autodetect (only update choices)
        safe_operation(
          "Update column choices on data update",
          code = {
            update_column_choices_unified(app_state, input, output, session, ui_service, reason = column_update_reason)
          }
        )
      }
    } else {
      # Fallback: NO autodetect by default (only update choices)
      safe_operation(
        "Update column choices on data update (fallback)",
        code = {
          update_column_choices_unified(app_state, input, output, session, ui_service, reason = "manual")
        }
      )
    }
  })

  # Legacy compatibility observers (for backward compatibility during transition)
  shiny::observeEvent(app_state$events$data_loaded, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$LOW, {
    # Legacy data_loaded observer - mostly handled by data_updated now
    # Only used for specific legacy edge cases that haven't been migrated yet
    log_debug("Legacy data_loaded event fired - consider migrating to data_updated", .context = "EVENT_SYSTEM")
  })

  shiny::observeEvent(app_state$events$data_changed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$LOW, {
    # Legacy data_changed observer - mostly handled by data_updated now
    # Only used for specific legacy edge cases that haven't been migrated yet
    log_debug("Legacy data_changed event fired - consider migrating to data_updated", .context = "EVENT_SYSTEM")
  })

  # Reset auto-default flag for Y-akse når data opdateres
  shiny::observeEvent(app_state$events$data_updated, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$LOWEST, {
    if (!is.null(app_state$ui)) {
      app_state$ui$y_axis_unit_autoset_done <- FALSE
    }
  })

  # AUTO-DETECTION EVENTS
  shiny::observeEvent(app_state$events$auto_detection_started, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$AUTO_DETECT, {
    # Auto-detection started event handler

    # Perform auto-detection using unified engine
    safe_operation(
      "Auto-detection processing",
      code = {
        # NOTE: in_progress state is managed by autodetect_engine itself to prevent conflicts

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
        # Only reset in_progress if autodetect_engine didn't handle it
        if (shiny::isolate(app_state$columns$auto_detect$in_progress)) {
          app_state$columns$auto_detect$in_progress <- FALSE
        }
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

  # UI SYNCHRONIZATION EVENTS (CONSOLIDATED)
  shiny::observeEvent(app_state$events$ui_sync_requested, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$UI_SYNC, {

    # Add extra debugging

    safe_operation(
      "UI synchronization",
      code = {
        # Perform UI synchronization
        sync_ui_with_columns_unified(app_state, input, output, session, ui_service)

        # CONSOLIDATED: Handle general UI updates (from ui_update_needed)
        if (!is.null(ui_service) && !is.null(app_state$data$current_data)) {
          ui_service$update_column_choices()
        }
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

    # Auto-sæt Y-akse enhed efter run chart + N tilgængelighed (kun én gang pr. data load)
    safe_operation(
      "Auto-set y-axis unit after UI sync",
      code = {
        already_set <- isTRUE(shiny::isolate(app_state$ui$y_axis_unit_autoset_done))
        if (!already_set) {
          ct <- get_qic_chart_type(input$chart_type %||% "run")
          # Brug app_state mappings (mere stabile end input under programmatisk sync)
          columns_state <- shiny::isolate(app_state$columns)
          n_val <- tryCatch(shiny::isolate(columns_state$n_column), error = function(...) NULL)
          if (is.null(n_val)) {
            n_val <- tryCatch(shiny::isolate(columns_state$mappings$n_column), error = function(...) NULL)
          }
          n_present <- !is.null(n_val) && nzchar(n_val)
          if (identical(ct, "run")) {
            default_unit <- decide_default_y_axis_ui_type(ct, n_present)
            current_unit <- input$y_axis_unit %||% "count"
            if (!identical(current_unit, default_unit)) {
              safe_programmatic_ui_update(session, app_state, function() {
                shiny::updateSelectizeInput(session, "y_axis_unit", selected = default_unit)
              })
            }
            app_state$ui$y_axis_unit_autoset_done <- TRUE
            log_debug_kv(
              message = "Auto-set y-axis unit",
              chart_type = ct,
              n_present = n_present,
              from = current_unit,
              to = default_unit,
              .context = "[Y_AXIS_UI]"
            )
          }
        }
      },
      fallback = NULL,
      session = session,
      error_type = "processing"
    )

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
    # Phase 4: Track test mode startup event
    if (exists("track_event")) {
      track_event("test_mode_ready", "startup_sequence")
    }

    # Phase 3: Set startup phase and enable race condition prevention
    app_state$test_mode$race_prevention_active <- TRUE
    emit$test_mode_startup_phase_changed("data_ready")

    # FIXED: In test mode, data_loaded event is ignored due to timing (sent before observers setup)
    # Handle autodetect trigger for test scenarios

    # Check if autodetect has not run yet but data is available
    autodetect_completed <- app_state$columns$auto_detect$completed %||% FALSE
    data_available <- !is.null(app_state$data$current_data)

    if (data_available && !autodetect_completed) {
      # Phase 3: Use debounced reactive pattern (following established architecture)
      debounce_delay <- app_state$test_mode$debounce_delay %||% 500

      # Create debounced reactive for test mode autodetect trigger
      debounced_test_mode_trigger <- shiny::debounce(
        shiny::reactive({
          if (app_state$test_mode$race_prevention_active) {
            emit$test_mode_debounced_autodetect()
          }
        }),
        millis = debounce_delay
      )

      # Trigger the debounced reactive immediately to start the delay
      debounced_test_mode_trigger()

    } else if (autodetect_completed) {
      # Autodetect already completed, trigger UI sync
      emit$ui_sync_needed()
    }
  })

  # Phase 3: Test mode startup phase management
  shiny::observeEvent(app_state$events$test_mode_startup_phase_changed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$HIGH, {
    current_phase <- app_state$test_mode$startup_phase

    # Phase 4: Track startup phase transitions
    if (exists("track_event")) {
      track_event("test_mode_startup_phase_changed", paste("phase:", current_phase))
    }

    log_debug_kv(
      message = paste("Startup phase changed to:", current_phase),
      phase = current_phase,
      .context = "[TEST_MODE_STARTUP]"
    )

    # Handle phase transitions
    if (current_phase == "ui_ready") {
      emit$test_mode_startup_phase_changed("complete")
    } else if (current_phase == "complete") {
      # Disable race prevention when startup is complete
      app_state$test_mode$race_prevention_active <- FALSE
      log_info(
        message = "Test mode startup completed - race prevention disabled",
        .context = "[TEST_MODE_STARTUP]"
      )
    }
  })

  # Phase 3: Debounced auto-detection for test mode
  shiny::observeEvent(app_state$events$test_mode_debounced_autodetect, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$AUTO_DETECT, {
    # Only proceed if race prevention is still active
    if (!app_state$test_mode$race_prevention_active) {
      log_debug("Debounced autodetect skipped - race prevention disabled", .context = "[TEST_MODE_STARTUP]")
      return()
    }

    # Trigger autodetect
    emit$auto_detection_started()
    emit$test_mode_startup_phase_changed("ui_ready")
  })

  # SESSION LIFECYCLE EVENTS
  shiny::observeEvent(app_state$events$session_started, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$AUTO_DETECT, {

    # Session start logic
    if (is.null(app_state$data$current_data) || nrow(app_state$data$current_data) == 0) {
      # FASE 3: Session start trigger for name-only detection
      autodetect_engine(
        data = NULL,  # No data available at session start
        trigger_type = "session_start",
        app_state = app_state,
        emit = emit
      )
    } else {
      log_debug("Skipping session_started autodetect - data already available, will be handled by data_loaded event", .context = "AUTO_DETECT_EVENT")
    }
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

  # ERROR HANDLING EVENTS (CONSOLIDATED - FASE 2.1) ========================

  # Unified error event listener - handles all error types with context-aware logic
  shiny::observeEvent(app_state$events$error_occurred, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$STATE_MANAGEMENT, {
    # Get consolidated error context (new system)
    error_context <- app_state$last_error_context

    # Fallback to legacy error info if needed (backward compatibility)
    error_info <- app_state$errors$last_error

    # Centralized error logging with enhanced context
    log_error("Consolidated error event triggered", .context = "ERROR_SYSTEM")

    # Log error details from new context system
    if (!is.null(error_context)) {
      log_debug_kv(
        error_type = error_context$type %||% "unknown",
        error_context = error_context$context %||% "no context",
        error_details = if(!is.null(error_context$details)) paste(names(error_context$details), collapse = ", ") else "none",
        timestamp = as.character(error_context$timestamp %||% Sys.time()),
        session_id = if(!is.null(session)) session$token else "no session",
        .context = "ERROR_SYSTEM"
      )
    } else if (!is.null(error_info)) {
      # Fallback to legacy error info
      log_debug_kv(
        error_type = error_info$type %||% "unknown",
        error_message = error_info$message %||% "no message",
        session_id = if(!is.null(session)) session$token else "no session",
        .context = "ERROR_SYSTEM"
      )
    }

    # Context-aware error handling logic
    if (!is.null(error_context) && !is.null(emit)) {
      error_type <- error_context$type %||% "general"

      # Type-specific error handling
      if (error_type == "processing") {
        # For processing errors, increment recovery attempts
        app_state$errors$recovery_attempts <- app_state$errors$recovery_attempts + 1L

        # Check if it's data processing related
        if (!is.null(error_context$context) && grepl("data|processing|convert|qic", error_context$context, ignore.case = TRUE)) {
          log_debug("Processing error detected - may need data validation", .context = "ERROR_SYSTEM")
        }

      } else if (error_type == "validation") {
        # For validation errors, clear problematic state and increment recovery attempts
        app_state$errors$recovery_attempts <- app_state$errors$recovery_attempts + 1L
        log_debug("Validation error detected - clearing validation state", .context = "ERROR_SYSTEM")

      } else if (error_type == "network") {
        # For network/file errors, log context for retry logic
        if (!is.null(error_context$context) && grepl("file|upload|download|io", error_context$context, ignore.case = TRUE)) {
          log_debug("Network/File I/O error detected", .context = "ERROR_SYSTEM")
        }

      } else if (error_type == "ui") {
        # For UI errors, may need UI sync
        log_debug("UI error detected - may need UI synchronization", .context = "ERROR_SYSTEM")

      } else {
        # General error handling
        log_debug(paste("General error of type:", error_type), "ERROR_SYSTEM")
      }
    }

    # Store error in history
    if (!is.null(app_state$errors)) {
      app_state$errors$error_count <- app_state$errors$error_count + 1L
      app_state$errors$last_error <- list(
        type = if(!is.null(error_context)) error_context$type else (if(!is.null(error_info)) error_info$type else "unknown"),
        context = if(!is.null(error_context)) error_context$context else "consolidated_handler",
        timestamp = Sys.time()
      )
    }
  })

  # Recovery completed event listener
  shiny::observeEvent(app_state$events$recovery_completed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$LOW, {
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
  shiny::observeEvent(app_state$events$form_reset_needed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$LOW, {

    if (!is.null(ui_service)) {
      ui_service$reset_form_fields()
    } else {
    }
  })

  # Form restore needed event listener
  shiny::observeEvent(app_state$events$form_restore_needed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$LOW, {

    # For form restore, we need metadata from app_state
    # This could be triggered by session restore events
    if (!is.null(ui_service) && !is.null(app_state$session$restore_metadata)) {
      ui_service$update_form_fields(app_state$session$restore_metadata)
    } else {
    }
  })

  # NOTE: ui_update_needed functionality consolidated into ui_sync_requested observer above

  # INPUT CHANGE OBSERVERS ===================================================
  # Keep app_state$columns aligned with UI selections when user manually changes dropdowns

  columns_to_observe <- c("x_column", "y_column", "n_column", "skift_column", "frys_column", "kommentar_column")

  normalize_column_input <- function(value) {
    if (is.null(value) || length(value) == 0) {
      return("")
    }

    candidate <- value[[1]]
    if (is.null(candidate) || (is.atomic(candidate) && length(candidate) == 0)) {
      return("")
    }

    candidate_chr <- as.character(candidate)[1]
    if (length(candidate_chr) == 0 || is.na(candidate_chr)) {
      return("")
    }

    if (identical(candidate_chr, "")) {
      return("")
    }

    candidate_chr
  }

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
        app_state$columns[[col]] <- normalize_column_input(new_value)

        # PERFORMANCE METRICS: Track token consumption for monitoring
        shiny::isolate({
          app_state$ui$performance_metrics$tokens_consumed <- app_state$ui$performance_metrics$tokens_consumed + 1L
        })

      #               "- no event emitted"), "TOKEN_DEBUG")
        return()
      }

      # Update app_state to keep it synchronized with UI
      normalized_value <- normalize_column_input(new_value)

      app_state$columns[[col]] <- normalized_value

      cache_key <- paste0(col, "_input")
      if (!is.null(app_state$ui_cache)) {
        app_state$ui_cache[[cache_key]] <- normalized_value
      }

      # Only emit events for user-driven changes (not programmatic updates)
      if (exists("column_choices_changed", envir = as.environment(emit))) {
        emit$column_choices_changed()
      }

    }, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$MEDIUM)
  }

  # OBSERVER: Toggle N (n_column) enabled state based on chart_type selection
  shiny::observeEvent(input$chart_type, {
    safe_operation(
      "Toggle n_column enabled state by chart type",
      code = {
        ct <- input$chart_type %||% "run"
        enabled <- chart_type_requires_denominator(ct)

        if (enabled) {
          shinyjs::enable("n_column")
          shinyjs::hide("n_column_hint")
          shinyjs::hide("n_column_ignore_tt")
        } else {
          shinyjs::disable("n_column")
          shinyjs::show("n_column_hint")
          shinyjs::show("n_column_ignore_tt")
        }

        log_debug_kv(
          message = "Updated n_column enabled state",
          chart_type = ct,
          n_enabled = enabled,
          .context = "[UI_SYNC]"
        )

        # Hvis brugeren vælger en anden diagramtype end run, så sæt passende Y-akse UI-type
        # Ignorér programmatisk ændringer (token-baseret) hvis muligt
        pending_token <- app_state$ui$pending_programmatic_inputs[["chart_type"]]
        if (!is.null(pending_token) && identical(pending_token$value, input$chart_type)) {
          app_state$ui$pending_programmatic_inputs[["chart_type"]] <- NULL
        } else {
          qic_ct <- get_qic_chart_type(ct)
          if (!identical(qic_ct, "run")) {
            desired_ui <- chart_type_to_ui_type(qic_ct)
            current_ui <- input$y_axis_unit %||% "count"
            if (!identical(current_ui, desired_ui)) {
              safe_programmatic_ui_update(session, app_state, function() {
                shiny::updateSelectizeInput(session, "y_axis_unit", selected = desired_ui)
              })
            }
            log_debug_kv(
              message = "Chart type changed; updated y-axis UI type",
              chart_type = qic_ct,
              y_axis_unit = desired_ui,
              .context = "[Y_AXIS_UI]"
            )
          } else {
            # Hvis brugeren skifter tilbage til RUN og der findes en nævner, sæt Y-akse til percent
            columns_state <- shiny::isolate(app_state$columns)
            n_val <- tryCatch(shiny::isolate(columns_state$n_column), error = function(...) NULL)
            if (is.null(n_val)) {
              n_val <- tryCatch(shiny::isolate(columns_state$mappings$n_column), error = function(...) NULL)
            }
            n_present <- !is.null(n_val) && nzchar(n_val)
            if (n_present) {
              current_ui <- input$y_axis_unit %||% "count"
              if (!identical(current_ui, "percent")) {
                safe_programmatic_ui_update(session, app_state, function() {
                  shiny::updateSelectizeInput(session, "y_axis_unit", selected = "percent")
                })
              }
              log_debug_kv(
                message = "Chart type changed to run; updated y-axis UI to percent due to denominator",
                n_present = TRUE,
                .context = "[Y_AXIS_UI]"
              )
            }
          }
        }
      },
      fallback = NULL,
      session = session,
      error_type = "processing"
    )
  }, ignoreInit = FALSE, priority = OBSERVER_PRIORITIES$UI_SYNC)

  # OBSERVER: Auto-vælg korttype baseret på Y-akse UI-type
  shiny::observeEvent(input$y_axis_unit, {
    safe_operation(
      "Auto-select chart type from y-axis UI type",
      code = {
        # Consumér programmatic token hvis dette stammer fra updateSelectizeInput
        pending_token <- app_state$ui$pending_programmatic_inputs[["y_axis_unit"]]
        if (!is.null(pending_token) && identical(pending_token$value, input$y_axis_unit)) {
          app_state$ui$pending_programmatic_inputs[["y_axis_unit"]] <- NULL
          return(invisible(NULL))
        }
        ui_type <- input$y_axis_unit %||% "count"

        # Find y-data og N-tilgængelighed
        y_col <- shiny::isolate(app_state$columns$y_column)
        data <- shiny::isolate(app_state$data$current_data)
        n_points <- if (!is.null(data)) nrow(data) else NA_integer_
        n_present <- !is.null(input$n_column) && nzchar(input$n_column)

        y_vals <- if (!is.null(y_col) && !is.null(data) && y_col %in% names(data)) data[[y_col]] else NULL

        internal_class <- determine_internal_class(ui_type, y_vals, n_present = n_present)
        suggested <- suggest_chart_type(internal_class, n_present = n_present, n_points = n_points)
        # Behold run chart som standard – ændr ikke diagramtype automatisk
        log_debug_kv(
          message = "Y-axis UI type changed; keeping current chart type",
          ui_type = ui_type,
          internal_class = internal_class,
          suggested_chart = suggested,
          current_chart = input$chart_type %||% "run",
          .context = "[Y_AXIS_UI]"
        )

        # Ekstra: vis hjælp hvis PROCENT/RATE uden N
        if (ui_type %in% c("percent", "rate") && !n_present) {
          log_warn("N-kolonne kræves for valgt Y-akse-type", .context = "[Y_AXIS_UI]")
        }
      },
      fallback = NULL,
      session = session,
      error_type = "processing"
    )
  }, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$UI_SYNC)

  # OBSERVER: Når RUN chart og nævner ændres
  shiny::observeEvent(input$n_column, {
    safe_operation(
      "Adjust y-axis when denominator changed in run chart",
      code = {
        # Ignorér programmatisk ændring af n_column
        pending_token <- app_state$ui$pending_programmatic_inputs[["n_column"]]
        if (!is.null(pending_token) && identical(pending_token$value, input$n_column)) {
          app_state$ui$pending_programmatic_inputs[["n_column"]] <- NULL
          return(invisible(NULL))
        }

        ct <- get_qic_chart_type(input$chart_type %||% "run")
        if (identical(ct, "run")) {
          n_present <- !is.null(input$n_column) && nzchar(input$n_column)
          if (!n_present) {
            current_ui <- input$y_axis_unit %||% "count"
            if (!identical(current_ui, "count")) {
              safe_programmatic_ui_update(session, app_state, function() {
                shiny::updateSelectizeInput(session, "y_axis_unit", selected = "count")
              })
            }

            log_debug_kv(
              message = "Denominator cleared in run chart; set y-axis to count",
              chart_type = ct,
              .context = "[Y_AXIS_UI]"
            )
          } else {
            # Nævner valgt i RUN chart → sæt Y-akse til percent som standard
            current_ui <- input$y_axis_unit %||% "count"
            if (!identical(current_ui, "percent")) {
              safe_programmatic_ui_update(session, app_state, function() {
                shiny::updateSelectizeInput(session, "y_axis_unit", selected = "percent")
              })
            }
            log_debug_kv(
              message = "Denominator selected in run chart; set y-axis to percent",
              chart_type = ct,
              .context = "[Y_AXIS_UI]"
            )
          }
        }
      },
      fallback = NULL,
      session = session,
      error_type = "processing"
    )
  }, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$UI_SYNC)

  # PASSIVE TIMING OBSERVER: Monitor system performance without interfering
  # This observer tracks timing metrics for optimization without emitting events
  if (!is.null(app_state$ui)) {
    shiny::observeEvent(app_state$ui$last_programmatic_update, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$LOWEST, {
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
#' @param reason Character string describing why the update is happening
#'
update_column_choices_unified <- function(app_state, input, output, session, ui_service = NULL, reason = "manual") {
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

    normalize_selection_value <- function(value) {
      if (is.null(value)) {
        return(NULL)
      }

      if (length(value) == 0) {
        return("")
      }

      candidate <- value[[1]]
      if (is.null(candidate)) {
        return(NULL)
      }

      candidate_chr <- as.character(candidate)[1]

      if (length(candidate_chr) == 0) {
        return(NULL)
      }

      if (anyNA(candidate_chr)) {
        return("")
      }

      if (identical(candidate_chr, "")) {
        return("")
      }

      candidate_chr
    }

    for (col in columns_to_update) {
      if (identical(reason, "edit")) {
        tryCatch({
          shiny::freezeReactiveValue(input, col)
        }, error = function(...) NULL)
      }

      input_val <- tryCatch(input[[col]], error = function(...) NULL)
      normalized_input <- normalize_selection_value(input_val)

      cache_key <- paste0(col, "_input")
      cache_val_raw <- tryCatch({
        if (!is.null(app_state$ui_cache)) {
          shiny::isolate(app_state$ui_cache[[cache_key]])
        } else {
          NULL
        }
      }, error = function(...) NULL)
      cache_val <- normalize_selection_value(cache_val_raw)

      state_val_raw <- tryCatch(shiny::isolate(app_state$columns[[col]]), error = function(...) NULL)
      state_val <- normalize_selection_value(state_val_raw)

      candidates <- if (identical(reason, "edit")) {
        list(normalized_input, cache_val)
      } else {
        list(normalized_input, cache_val, state_val)
      }

      selected_val <- NULL
      for (candidate in candidates) {
        if (!is.null(candidate)) {
          selected_val <- candidate
          break
        }
      }

      if (is.null(selected_val)) {
        selected_val <- ""
      }

      current_selections[[col]] <- selected_val
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
