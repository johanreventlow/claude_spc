#' Event System Utilities
#'
#' This file contains utilities for the unified reactive event system.
#' It provides centralized event listeners and handlers for the application.
#'
#' ## Quick Navigation
#'
#' **Main Function:** `setup_event_listeners()` - Line ~99
#'
#' **SPRINT 4: Modular Event Registration Functions**
#' - `register_data_lifecycle_events()` - Data loading and changes
#' - `register_autodetect_events()` - Auto-detection processing
#' - `register_ui_sync_events()` - UI synchronization
#' - `register_chart_type_events()` - Chart type and Y-axis logic
#' - `register_navigation_events()` - Navigation and session lifecycle
#'
#' **Supporting Functions:** Helper functions for event processing
#'
#' ## Architecture Note (SPRINT 4 Update)
#'
#' Event listeners are organized into category-specific registration functions
#' for better maintainability, but all are orchestrated by setup_event_listeners()
#' to maintain visibility of event ordering and prevent race conditions.
#'
#' @name utils_event_system
NULL

# ============================================================================
# SPRINT 4: MODULAR EVENT REGISTRATION FUNCTIONS
# ============================================================================
# These helper functions register category-specific observers and return
# a named list of observers for centralized cleanup management.

#' Register Data Lifecycle Events
#'
#' Registers observers for data loading, changes, and updates.
#'
#' @param app_state Centralized app state
#' @param emit Event emission API
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param ui_service UI service for UI updates
#' @param register_observer Function to register observer for cleanup
#'
#' @return Named list of registered observers
#'
#' @details
#' Handles all events related to data lifecycle:
#' - data_updated: Consolidated handler for data_loaded + data_changed
#' - Cache clearing on data updates
#' - Context-aware processing (load vs. edit vs. general)
#' - Y-axis reset on data update
register_data_lifecycle_events <- function(app_state, emit, input, output, session, ui_service, register_observer) {
  observers <- list()

  # Helper function for context resolution
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

  # Consolidated data update handler
  observers$data_updated <- register_observer(
    "data_updated",
    shiny::observeEvent(app_state$events$data_updated,
      ignoreInit = TRUE,
      priority = OBSERVER_PRIORITIES$STATE_MANAGEMENT,
      {
        update_context <- app_state$last_data_update_context

        # SPRINT 4: Clear QIC cache when data changes
        if (!is.null(app_state$cache) && !is.null(app_state$cache$qic)) {
          safe_operation(
            "Clear QIC cache on data update",
            code = {
              app_state$cache$qic$clear()
              log_debug("QIC cache cleared due to data update", .context = "QIC_CACHE")
            },
            fallback = function(e) {
              log_warn(paste("Failed to clear QIC cache:", e$message), .context = "QIC_CACHE")
            }
          )
        }

        # Legacy performance cache clearing
        if (exists("clear_performance_cache") && is.function(clear_performance_cache)) {
          safe_operation(
            "Clear performance cache on data update",
            code = {
              clear_performance_cache()
              log_debug("Performance cache cleared due to data update", .context = "CACHE_INVALIDATION")
            },
            fallback = function(e) {
              log_warn(paste("Failed to clear cache:", e$message), .context = "CACHE_INVALIDATION")
            }
          )
        }

        # Unfreeze autodetect system when data is updated
        app_state$columns$auto_detect$frozen_until_next_trigger <- FALSE

        # Context-aware processing based on update type
        if (!is.null(update_context)) {
          context <- update_context$context %||% "general"
          column_update_reason <- resolve_column_update_reason(context)

          is_table_cells_edit <- identical(context, "table_cells_edited")
          is_load_context <- grepl("load|upload|new", context, ignore.case = TRUE)
          is_change_context <- grepl("change|edit|modify", context, ignore.case = TRUE)

          if (is_load_context) {
            # Data loading path - trigger auto-detection
            if (!is.null(app_state$data$current_data)) {
              emit$auto_detection_started()
            }
          } else if (is_table_cells_edit) {
            emit$navigation_changed()
            emit$visualization_update_needed()
          } else if (is_change_context) {
            # Data change path - update column choices AND trigger plot regeneration
            safe_operation(
              "Update column choices on data change",
              code = {
                update_column_choices_unified(app_state, input, output, session, ui_service, reason = column_update_reason)
              }
            )

            emit$navigation_changed()
            emit$visualization_update_needed()
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
      }
    )
  )

  # Reset auto-default flag for Y-axis when data updates
  observers$data_updated_y_axis_reset <- register_observer(
    "data_updated_y_axis_reset",
    shiny::observeEvent(app_state$events$data_updated,
      ignoreInit = TRUE,
      priority = OBSERVER_PRIORITIES$LOWEST,
      {
        if (!is.null(app_state$ui)) {
          app_state$ui$y_axis_unit_autoset_done <- FALSE
        }
      }
    )
  )

  return(observers)
}

#' Register Auto-Detection Events
#'
#' Registers observers for auto-detection processing.
#'
#' @param app_state Centralized app state
#' @param emit Event emission API
#' @param session Shiny session
#' @param register_observer Function to register observer for cleanup
#'
#' @return Named list of registered observers
#'
#' @details
#' Handles all events related to column auto-detection:
#' - auto_detection_started: Triggers autodetect engine
#' - auto_detection_completed: Updates state, triggers UI sync
register_autodetect_events <- function(app_state, emit, session, register_observer) {
  observers <- list()

  observers$auto_detection_started <- register_observer(
    "auto_detection_started",
    shiny::observeEvent(app_state$events$auto_detection_started,
      ignoreInit = TRUE,
      priority = OBSERVER_PRIORITIES$AUTO_DETECT,
      {
        safe_operation(
          "Auto-detection processing",
          code = {
            if (!is.null(app_state$data$current_data)) {
              # Use unified autodetect engine - data available, so full analysis
              autodetect_engine(
                data = app_state$data$current_data,
                trigger_type = "file_upload",
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
      }
    )
  )

  observers$auto_detection_completed <- register_observer(
    "auto_detection_completed",
    shiny::observeEvent(app_state$events$auto_detection_completed,
      ignoreInit = TRUE,
      priority = OBSERVER_PRIORITIES$AUTO_DETECT,
      {
        # Update state
        app_state$columns$auto_detect$in_progress <- FALSE
        app_state$columns$auto_detect$completed <- TRUE

        # Trigger UI sync if columns were detected
        auto_detect_results <- shiny::isolate(app_state$columns$auto_detect$results)

        if (!is.null(auto_detect_results)) {
          emit$ui_sync_needed()
        }
      }
    )
  )

  return(observers)
}

#' Register UI Sync Events
#'
#' Registers observers for UI synchronization.
#'
#' @param app_state Centralized app state
#' @param emit Event emission API
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param ui_service UI service for UI updates
#' @param register_observer Function to register observer for cleanup
#'
#' @return Named list of registered observers
#'
#' @details
#' Handles UI synchronization events:
#' - ui_sync_requested: Syncs UI controls with detected columns
#' - ui_sync_completed: Marks sync completion, triggers navigation
register_ui_sync_events <- function(app_state, emit, input, output, session, ui_service, register_observer) {
  observers <- list()

  observers$ui_sync_requested <- register_observer(
    "ui_sync_requested",
    shiny::observeEvent(app_state$events$ui_sync_requested,
      ignoreInit = TRUE,
      priority = OBSERVER_PRIORITIES$UI_SYNC,
      {
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
      }
    )
  )

  observers$ui_sync_completed <- register_observer(
    "ui_sync_completed",
    shiny::observeEvent(app_state$events$ui_sync_completed,
      ignoreInit = TRUE,
      priority = OBSERVER_PRIORITIES$UI_SYNC,
      {
        # Update timestamp
        app_state$columns$ui_sync$last_sync_time <- Sys.time()

        # Auto-set Y-axis unit after run chart + N availability (only once per data load)
        safe_operation(
          "Auto-set y-axis unit after UI sync",
          code = {
            already_set <- isTRUE(shiny::isolate(app_state$ui$y_axis_unit_autoset_done))
            if (!already_set) {
              ct <- get_qic_chart_type(input$chart_type %||% "run")
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
      }
    )
  )

  return(observers)
}

#' Register Navigation Events
#'
#' Registers observers for navigation and session lifecycle.
#'
#' @param app_state Centralized app state
#' @param emit Event emission API
#' @param session Shiny session
#' @param register_observer Function to register observer for cleanup
#'
#' @return Named list of registered observers
#'
#' @details
#' Handles navigation, session lifecycle, test mode, and error events:
#' - navigation_changed: Increments navigation trigger
#' - session_started: Session initialization
#' - manual_autodetect_button: User-triggered detection
#' - session_reset: Complete state cleanup
#' - test_mode events: Test mode initialization and startup
#' - error events: Centralized error handling
#' - form events: UI field reset and restore
register_navigation_events <- function(app_state, emit, session, register_observer) {
  observers <- list()

  # Navigation changed
  observers$navigation_changed <- register_observer(
    "navigation_changed",
    shiny::observeEvent(app_state$events$navigation_changed,
      ignoreInit = TRUE,
      priority = OBSERVER_PRIORITIES$STATUS_UPDATES,
      {
        app_state$navigation$trigger <- app_state$navigation$trigger + 1L
      }
    )
  )

  # Session started
  observers$session_started <- register_observer(
    "session_started",
    shiny::observeEvent(app_state$events$session_started,
      ignoreInit = TRUE,
      priority = OBSERVER_PRIORITIES$AUTO_DETECT,
      {
        if (is.null(app_state$data$current_data) || nrow(app_state$data$current_data) == 0) {
          autodetect_engine(
            data = NULL,
            trigger_type = "session_start",
            app_state = app_state,
            emit = emit
          )
        } else {
          log_debug("Skipping session_started autodetect - data already available", .context = "AUTO_DETECT_EVENT")
        }
      }
    )
  )

  # Manual autodetect button
  observers$manual_autodetect_button <- register_observer(
    "manual_autodetect_button",
    shiny::observeEvent(app_state$events$manual_autodetect_button,
      ignoreInit = TRUE,
      priority = OBSERVER_PRIORITIES$AUTO_DETECT,
      {
        autodetect_engine(
          data = app_state$data$current_data,
          trigger_type = "manual",
          app_state = app_state,
          emit = emit
        )
      }
    )
  )

  # Session reset
  observers$session_reset <- register_observer(
    "session_reset",
    shiny::observeEvent(app_state$events$session_reset,
      ignoreInit = TRUE,
      priority = OBSERVER_PRIORITIES$CLEANUP,
      {
        # SPRINT 4: Clear QIC cache on session reset
        if (!is.null(app_state$cache) && !is.null(app_state$cache$qic)) {
          safe_operation(
            "Clear QIC cache on session reset",
            code = {
              app_state$cache$qic$clear()
              log_debug("QIC cache cleared due to session reset", .context = "QIC_CACHE")
            }
          )
        }

        # Clear all caches on session reset
        if (exists("clear_performance_cache") && is.function(clear_performance_cache)) {
          safe_operation(
            "Clear performance cache on session reset",
            code = {
              clear_performance_cache()
              log_debug("Performance cache cleared due to session reset", .context = "CACHE_INVALIDATION")
            }
          )
        }

        # Reset all state to initial values
        app_state$data$current_data <- NULL
        app_state$columns$auto_detect$in_progress <- FALSE
        app_state$columns$auto_detect$completed <- FALSE
        app_state$columns$auto_detect$results <- NULL
        app_state$columns$auto_detect$frozen_until_next_trigger <- FALSE
        app_state$columns$auto_detect$last_run <- NULL
      }
    )
  )

  # Test mode ready
  observers$test_mode_ready <- register_observer(
    "test_mode_ready",
    shiny::observeEvent(app_state$events$test_mode_ready,
      ignoreInit = TRUE,
      priority = OBSERVER_PRIORITIES$AUTO_DETECT,
      {
        if (exists("track_event")) {
          track_event("test_mode_ready", "startup_sequence")
        }

        app_state$test_mode$race_prevention_active <- TRUE
        emit$test_mode_startup_phase_changed("data_ready")

        autodetect_completed <- app_state$columns$auto_detect$completed %||% FALSE
        data_available <- !is.null(app_state$data$current_data)

        if (data_available && !autodetect_completed) {
          debounce_delay <- app_state$test_mode$debounce_delay %||% 500

          debounced_test_mode_trigger <- shiny::debounce(
            shiny::reactive({
              if (app_state$test_mode$race_prevention_active) {
                emit$test_mode_debounced_autodetect()
              }
            }),
            millis = debounce_delay
          )

          debounced_test_mode_trigger()
        } else if (autodetect_completed) {
          emit$ui_sync_needed()
        }
      }
    )
  )

  # Test mode startup phase changed
  observers$test_mode_startup_phase_changed <- register_observer(
    "test_mode_startup_phase_changed",
    shiny::observeEvent(app_state$events$test_mode_startup_phase_changed,
      ignoreInit = TRUE,
      priority = OBSERVER_PRIORITIES$HIGH,
      {
        current_phase <- app_state$test_mode$startup_phase

        if (exists("track_event")) {
          track_event("test_mode_startup_phase_changed", paste("phase:", current_phase))
        }

        log_debug_kv(
          message = paste("Startup phase changed to:", current_phase),
          phase = current_phase,
          .context = "[TEST_MODE_STARTUP]"
        )

        if (current_phase == "ui_ready") {
          emit$test_mode_startup_phase_changed("complete")
        } else if (current_phase == "complete") {
          app_state$test_mode$race_prevention_active <- FALSE
          log_info(
            message = "Test mode startup completed - race prevention disabled",
            .context = "[TEST_MODE_STARTUP]"
          )
        }
      }
    )
  )

  # Test mode debounced autodetect
  observers$test_mode_debounced_autodetect <- register_observer(
    "test_mode_debounced_autodetect",
    shiny::observeEvent(app_state$events$test_mode_debounced_autodetect,
      ignoreInit = TRUE,
      priority = OBSERVER_PRIORITIES$AUTO_DETECT,
      {
        if (!app_state$test_mode$race_prevention_active) {
          log_debug("Debounced autodetect skipped - race prevention disabled", .context = "[TEST_MODE_STARTUP]")
          return()
        }

        emit$auto_detection_started()
        emit$test_mode_startup_phase_changed("ui_ready")
      }
    )
  )

  # Error occurred
  observers$error_occurred <- register_observer(
    "error_occurred",
    shiny::observeEvent(app_state$events$error_occurred,
      ignoreInit = TRUE,
      priority = OBSERVER_PRIORITIES$STATE_MANAGEMENT,
      {
        error_context <- app_state$last_error_context
        error_info <- app_state$errors$last_error

        log_error("Consolidated error event triggered", .context = "ERROR_SYSTEM")

        if (!is.null(error_context)) {
          log_debug_kv(
            error_type = error_context$type %||% "unknown",
            error_context = error_context$context %||% "no context",
            error_details = if (!is.null(error_context$details)) paste(names(error_context$details), collapse = ", ") else "none",
            timestamp = as.character(error_context$timestamp %||% Sys.time()),
            session_id = if (!is.null(session)) sanitize_session_token(session$token) else "no session",
            .context = "ERROR_SYSTEM"
          )
        } else if (!is.null(error_info)) {
          log_debug_kv(
            error_type = error_info$type %||% "unknown",
            error_message = error_info$message %||% "no message",
            session_id = if (!is.null(session)) sanitize_session_token(session$token) else "no session",
            .context = "ERROR_SYSTEM"
          )
        }

        if (!is.null(error_context) && !is.null(emit)) {
          error_type <- error_context$type %||% "general"

          if (error_type == "processing") {
            app_state$errors$recovery_attempts <- app_state$errors$recovery_attempts + 1L
            if (!is.null(error_context$context) && grepl("data|processing|convert|qic", error_context$context, ignore.case = TRUE)) {
              log_debug("Processing error detected - may need data validation", .context = "ERROR_SYSTEM")
            }
          } else if (error_type == "validation") {
            app_state$errors$recovery_attempts <- app_state$errors$recovery_attempts + 1L
            log_debug("Validation error detected - clearing validation state", .context = "ERROR_SYSTEM")
          } else if (error_type == "network") {
            if (!is.null(error_context$context) && grepl("file|upload|download|io", error_context$context, ignore.case = TRUE)) {
              log_debug("Network/File I/O error detected", .context = "ERROR_SYSTEM")
            }
          } else if (error_type == "ui") {
            log_debug("UI error detected - may need UI synchronization", .context = "ERROR_SYSTEM")
          } else {
            log_debug(paste("General error of type:", error_type), "ERROR_SYSTEM")
          }
        }

        if (!is.null(app_state$errors)) {
          app_state$errors$error_count <- app_state$errors$error_count + 1L
          app_state$errors$last_error <- list(
            type = if (!is.null(error_context)) error_context$type else (if (!is.null(error_info)) error_info$type else "unknown"),
            context = if (!is.null(error_context)) error_context$context else "consolidated_handler",
            timestamp = Sys.time()
          )
        }
      }
    )
  )

  # Recovery completed
  observers$recovery_completed <- register_observer(
    "recovery_completed",
    shiny::observeEvent(app_state$events$recovery_completed,
      ignoreInit = TRUE,
      priority = OBSERVER_PRIORITIES$LOW,
      {
        app_state$errors$last_recovery_time <- Sys.time()
        log_info("Error recovery completed", .context = "ERROR_SYSTEM")
        log_debug_kv(
          recovery_time = as.character(Sys.time()),
          session_id = if (!is.null(session)) sanitize_session_token(session$token) else "no session",
          .context = "ERROR_SYSTEM"
        )
      }
    )
  )

  # Form reset needed
  observers$form_reset_needed <- register_observer(
    "form_reset_needed",
    shiny::observeEvent(app_state$events$form_reset_needed,
      ignoreInit = TRUE,
      priority = OBSERVER_PRIORITIES$LOW,
      {
        # NOTE: ui_service not available in this scope - will be handled at call site
      }
    )
  )

  # Form restore needed
  observers$form_restore_needed <- register_observer(
    "form_restore_needed",
    shiny::observeEvent(app_state$events$form_restore_needed,
      ignoreInit = TRUE,
      priority = OBSERVER_PRIORITIES$LOW,
      {
        # NOTE: ui_service not available in this scope - will be handled at call site
      }
    )
  )

  return(observers)
}

#' Register Chart Type Events
#'
#' Registers observers for chart type and Y-axis logic.
#'
#' @param app_state Centralized app state
#' @param emit Event emission API
#' @param input Shiny input
#' @param session Shiny session
#' @param register_observer Function to register observer for cleanup
#'
#' @return Named list of registered observers
#'
#' @details
#' Handles chart type changes and column selection:
#' - Column selection observers (x, y, n, skift, frys, kommentar)
#' - chart_type: Chart type changes with automatic Y-axis adjustment
#' - y_axis_unit: Y-axis unit changes with chart type suggestion
#' - n_column: Denominator changes affecting Y-axis in run charts
register_chart_type_events <- function(app_state, emit, input, session, register_observer) {
  observers <- list()

  # Normalize column input helper
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

  # Column selection observers
  columns_to_observe <- c("x_column", "y_column", "n_column", "skift_column", "frys_column", "kommentar_column")

  for (col in columns_to_observe) {
    observers[[paste0("input_", col)]] <- shiny::observeEvent(input[[col]],
      {
        input_received_time <- Sys.time()
        new_value <- input[[col]]

        # TOKEN CONSUMPTION: Primary loop protection mechanism
        pending_token <- app_state$ui$pending_programmatic_inputs[[col]]

        if (!is.null(pending_token) && pending_token$value == new_value) {
          # CONSUME TOKEN: This is a programmatic input, don't emit event
          app_state$ui$pending_programmatic_inputs[[col]] <- NULL
          app_state$columns[[col]] <- normalize_column_input(new_value)

          shiny::isolate({
            app_state$ui$performance_metrics$tokens_consumed <- app_state$ui$performance_metrics$tokens_consumed + 1L
          })

          return()
        }

        # Update app_state to keep it synchronized with UI
        normalized_value <- normalize_column_input(new_value)
        app_state$columns[[col]] <- normalized_value

        cache_key <- paste0(col, "_input")
        if (!is.null(app_state$ui_cache)) {
          app_state$ui_cache[[cache_key]] <- normalized_value
        }

        # Only emit events for user-driven changes
        if (exists("column_choices_changed", envir = as.environment(emit))) {
          emit$column_choices_changed()
        }
      },
      ignoreInit = TRUE,
      priority = OBSERVER_PRIORITIES$MEDIUM
    )
  }

  # Chart type observer
  observers$chart_type <- register_observer(
    "chart_type",
    shiny::observeEvent(input$chart_type,
      {
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

            # Check for programmatic update token
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
      },
      ignoreInit = FALSE,
      priority = OBSERVER_PRIORITIES$UI_SYNC
    )
  )

  # Y-axis unit observer
  observers$y_axis_unit <- register_observer(
    "y_axis_unit",
    shiny::observeEvent(input$y_axis_unit,
      {
        safe_operation(
          "Auto-select chart type from y-axis UI type",
          code = {
            # Consume programmatic token if from updateSelectizeInput
            pending_token <- app_state$ui$pending_programmatic_inputs[["y_axis_unit"]]
            if (!is.null(pending_token) && identical(pending_token$value, input$y_axis_unit)) {
              app_state$ui$pending_programmatic_inputs[["y_axis_unit"]] <- NULL
              return(invisible(NULL))
            }
            ui_type <- input$y_axis_unit %||% "count"

            y_col <- shiny::isolate(app_state$columns$y_column)
            data <- shiny::isolate(app_state$data$current_data)
            n_points <- if (!is.null(data)) nrow(data) else NA_integer_
            n_present <- !is.null(input$n_column) && nzchar(input$n_column)

            y_vals <- if (!is.null(y_col) && !is.null(data) && y_col %in% names(data)) data[[y_col]] else NULL

            internal_class <- determine_internal_class(ui_type, y_vals, n_present = n_present)
            suggested <- suggest_chart_type(internal_class, n_present = n_present, n_points = n_points)

            log_debug_kv(
              message = "Y-axis UI type changed; keeping current chart type",
              ui_type = ui_type,
              internal_class = internal_class,
              suggested_chart = suggested,
              current_chart = input$chart_type %||% "run",
              .context = "[Y_AXIS_UI]"
            )

            if (ui_type %in% c("percent", "rate") && !n_present) {
              log_warn("N-kolonne kræves for valgt Y-akse-type", .context = "[Y_AXIS_UI]")
            }
          },
          fallback = NULL,
          session = session,
          error_type = "processing"
        )
      },
      ignoreInit = TRUE,
      priority = OBSERVER_PRIORITIES$UI_SYNC
    )
  )

  # N column change observer
  observers$n_column_change <- register_observer(
    "n_column_change",
    shiny::observeEvent(input$n_column,
      {
        safe_operation(
          "Adjust y-axis when denominator changed in run chart",
          code = {
            # Ignore programmatic change of n_column
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
      },
      ignoreInit = TRUE,
      priority = OBSERVER_PRIORITIES$UI_SYNC
    )
  )

  # Passive timing monitor
  if (!is.null(app_state$ui)) {
    observers$timing_monitor <- register_observer(
      "timing_monitor",
      shiny::observeEvent(app_state$ui$last_programmatic_update,
        ignoreInit = TRUE,
        priority = OBSERVER_PRIORITIES$LOWEST,
        {
          current_time <- Sys.time()
          last_update <- shiny::isolate(app_state$ui$last_programmatic_update)

          if (!is.null(last_update)) {
            freeze_state <- shiny::isolate(app_state$columns$auto_detect$frozen_until_next_trigger) %||% FALSE

            autodetect_in_progress <- if (!is.null(app_state$columns)) {
              shiny::isolate(app_state$columns$auto_detect$in_progress) %||% FALSE
            } else {
              FALSE
            }
          }
        }
      )
    )
  }

  return(observers)
}

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
#' @param ui_service UI service for UI updates (optional)
#'
#' @details
#' ## Architectural Philosophy
#'
#' This function consolidates all event-driven reactive patterns in ONE place.
#' This centralization is INTENTIONAL and provides critical benefits:
#'
#' **Benefits of Centralization:**
#' - Event execution order is visible and explicit
#' - Race condition prevention is manageable
#' - Dependency chains are traceable
#' - Priority management is consistent
#' - Debugging is straightforward
#'
#' **Anti-Pattern Warning:**
#' DO NOT split event listeners into separate files by domain.
#' This would break event ordering visibility and make race conditions
#' significantly harder to debug.
#'
#' ## Event Listener Organization
#'
#' The listeners are organized into functional sections:
#'
#' 1. **Data Lifecycle Events** (lines ~62-146)
#'    - data_updated: Consolidated data loading/changes
#'    - Handles cache clearing, autodetect triggering, UI sync
#'
#' 2. **Auto-Detection Events** (lines ~148-201)
#'    - auto_detection_started: Triggers autodetect engine
#'    - auto_detection_completed: Updates state, triggers UI sync
#'
#' 3. **UI Synchronization Events** (lines ~203-274)
#'    - ui_sync_requested: Syncs UI with detected columns
#'    - ui_sync_completed: Triggers navigation updates
#'
#' 4. **Navigation Events** (lines ~276-280)
#'    - navigation_changed: Updates reactive navigation trigger
#'
#' 5. **Test Mode Events** (lines ~282-361)
#'    - test_mode_ready: Test mode initialization
#'    - test_mode_startup_phase_changed: Startup sequencing
#'    - test_mode_debounced_autodetect: Debounced detection
#'
#' 6. **Session Lifecycle Events** (lines ~363-410)
#'    - session_started: Session initialization
#'    - manual_autodetect_button: Manual detection trigger
#'    - session_reset: State cleanup
#'
#' 7. **Error Handling Events** (lines ~412-502)
#'    - error_occurred: Centralized error handling
#'    - recovery_completed: Recovery tracking
#'
#' 8. **UI Update Events** (lines ~504-527)
#'    - form_reset_needed: Form field reset
#'    - form_restore_needed: Session restore
#'
#' 9. **Input Change Observers** (lines ~529-822)
#'    - Column selection observers (x, y, n, etc.)
#'    - Chart type observers
#'    - Y-axis unit observers
#'    - Denominator observers
#'
#' ## Priority System
#'
#' Events use OBSERVER_PRIORITIES for execution order:
#' - STATE_MANAGEMENT: Highest - state updates first
#' - HIGH: Critical operations
#' - AUTO_DETECT: Auto-detection processing
#' - UI_SYNC: UI synchronization
#' - MEDIUM: Standard operations
#' - STATUS_UPDATES: Non-critical updates
#' - LOW: Background tasks
#' - CLEANUP: Lowest - cleanup operations
#' - LOWEST: Passive monitoring
#'
#' All observers use ignoreInit = TRUE to prevent firing at startup
#' unless explicitly designed for initialization (chart_type observer).
#'
setup_event_listeners <- function(app_state, emit, input, output, session, ui_service = NULL) {
  # DUPLICATE PREVENTION: Check if optimized listeners are already active
  if (exists("optimized_listeners_active", envir = app_state) && app_state$optimized_listeners_active) {
    stop("Cannot setup standard listeners while optimized listeners are active. This would cause duplicate execution.")
  }

  # Setting up unified event listeners
  # Mark that standard listeners are active to prevent duplicate optimized listeners
  app_state$standard_listeners_active <- TRUE

  # Observer registry for cleanup on session end
  observer_registry <- list()

  # Helper function to register observers automatically
  register_observer <- function(name, observer) {
    observer_registry[[name]] <<- observer
    return(observer)
  }

  # ============================================================================
  # HELPER FUNCTIONS
  # ============================================================================
  # These helper functions support event processing logic below.
  # They are kept within setup_event_listeners() to maintain closure
  # over app_state, emit, and session variables.

  #' Resolve Column Update Reason
  #'
  #' Determines the reason for a column update based on context string.
  #' Used to provide appropriate logging and behavior branching.
  #'
  #' @param context Context string from data update event
  #' @return One of: "manual", "edit", "session", "upload"
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

  # ============================================================================
  # SPRINT 4: MODULAR EVENT REGISTRATION
  # ============================================================================
  # Register all event categories using helper functions for better organization

  # Note: input, output, ui_service need to be accessible to data lifecycle events
  # Create wrapper that passes these through
  observers <- list()

  # Data lifecycle events (Section 1)
  data_observers <- register_data_lifecycle_events(app_state, emit, input, output, session, ui_service, register_observer)
  observers <- c(observers, data_observers)

  # Auto-detection events (Section 2)
  autodetect_observers <- register_autodetect_events(app_state, emit, session, register_observer)
  observers <- c(observers, autodetect_observers)

  # UI synchronization events (Section 3)
  ui_observers <- register_ui_sync_events(app_state, emit, input, output, session, ui_service, register_observer)
  observers <- c(observers, ui_observers)

  # Navigation and session lifecycle events (Sections 4-8)
  navigation_observers <- register_navigation_events(app_state, emit, session, register_observer)
  observers <- c(observers, navigation_observers)

  # Chart type and input change events (Sections 9-10)
  chart_observers <- register_chart_type_events(app_state, emit, input, session, register_observer)
  observers <- c(observers, chart_observers)

  # Handle form events with ui_service (these need special handling)
  if (!is.null(ui_service)) {
    register_observer(
      "form_reset_with_ui",
      shiny::observeEvent(app_state$events$form_reset_needed,
        ignoreInit = TRUE,
        priority = OBSERVER_PRIORITIES$LOW,
        {
          ui_service$reset_form_fields()
        }
      )
    )

    register_observer(
      "form_restore_with_ui",
      shiny::observeEvent(app_state$events$form_restore_needed,
        ignoreInit = TRUE,
        priority = OBSERVER_PRIORITIES$LOW,
        {
          if (!is.null(app_state$session$restore_metadata)) {
            ui_service$update_form_fields(app_state$session$restore_metadata)
          }
        }
      )
    )
  }

  # ============================================================================
  # END OF MODULAR EVENT REGISTRATION
  # ============================================================================
  # All event listeners are now registered via helper functions.
  # Centralized cleanup is maintained below.


  # ============================================================================
  # OBSERVER CLEANUP ON SESSION END
  # ============================================================================
  # Register cleanup handler to destroy all observers when session ends.
  # This prevents memory leaks from observers holding references to app_state.

  session$onSessionEnded(function() {
    safe_operation(
      "Observer cleanup on session end",
      code = {
        observer_count <- length(observer_registry)
        failed_observers <- character(0)

        log_debug(
          paste("Cleaning up", observer_count, "observers"),
          .context = "EVENT_SYSTEM"
        )

        # Destroy all registered observers with explicit nullification
        for (observer_name in names(observer_registry)) {
          tryCatch(
            {
              if (!is.null(observer_registry[[observer_name]])) {
                observer_registry[[observer_name]]$destroy()
                # CRITICAL: Explicit nullification to release references
                observer_registry[[observer_name]] <- NULL
              }
            },
            error = function(e) {
              failed_observers <<- c(failed_observers, observer_name)
              log_warn(
                paste("Failed to destroy observer:", observer_name, "-", e$message),
                .context = "EVENT_SYSTEM"
              )
            }
          )
        }

        # Verification and reporting
        successful_count <- observer_count - length(failed_observers)

        if (length(failed_observers) > 0) {
          log_warn(
            paste("Observer cleanup incomplete:", length(failed_observers), "failed"),
            .context = "EVENT_SYSTEM"
          )
          log_debug_kv(
            failed_observers = paste(failed_observers, collapse = ", "),
            successful_count = successful_count,
            total_count = observer_count,
            .context = "EVENT_SYSTEM"
          )
        } else {
          log_info(
            paste("Observer cleanup complete: All", observer_count, "observers destroyed"),
            .context = "EVENT_SYSTEM"
          )
        }
      },
      fallback = function(e) {
        log_error(
          paste("Observer cleanup failed:", e$message),
          .context = "EVENT_SYSTEM"
        )
      }
    )
  })

  # Return observer registry for testing/debugging purposes
  invisible(observer_registry)
}

# ============================================================================
# HELPER FUNCTIONS (Outside setup_event_listeners scope)
# ============================================================================
# These functions are called by event listeners but don't need closure
# over app_state/emit/session.

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
