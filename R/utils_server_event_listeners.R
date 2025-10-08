#' Event System Utilities
#'
#' This file contains utilities for the unified reactive event system.
#' It provides centralized event listeners and handlers for the application.
#'
#' ## Quick Navigation
#'
#' **Main Function:** `setup_event_listeners()` - Line ~99
#'
#' **Sections:**
#' - HELPER FUNCTIONS - Line ~109
#' - SECTION 1: Data Lifecycle Events - Line ~145
#' - SECTION 2: Auto-Detection Events - Line ~247
#' - SECTION 3: UI Synchronization Events - Line ~317
#' - SECTION 4: Navigation Events - Line ~404
#' - SECTION 5: Test Mode Events - Line ~422
#' - SECTION 6: Session Lifecycle Events - Line ~519
#' - SECTION 7: Error Handling Events - Line ~588
#' - SECTION 8: UI Update Events - Line ~699
#' - SECTION 9: Input Change Observers - Line ~741
#' - SECTION 10: Passive Monitoring - Line ~1042
#'
#' **Supporting Functions:** Line ~1099+
#'
#' ## Architecture Note
#'
#' This file intentionally consolidates ALL event listeners in one place.
#' DO NOT split into separate files - this would break event ordering
#' visibility and make race conditions harder to debug.
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
  # SECTION 1: DATA LIFECYCLE EVENTS
  # ============================================================================
  # Handles all events related to data loading, changes, and updates.
  # Priority: STATE_MANAGEMENT (highest) for state consistency.
  #
  # Key Events:
  # - data_updated: Consolidated handler for data_loaded + data_changed
  #
  # Event Flow:
  # 1. Data uploaded → emit$data_updated("file_loaded")
  # 2. Clear performance cache
  # 3. Unfreeze autodetect system
  # 4. Context-based processing:
  #    - Load context → trigger auto-detection
  #    - Edit context → update column choices + trigger plot regen
  #    - General → update column choices only

  # Consolidated data update handler - handles both data loading and changes
  shiny::observeEvent(app_state$events$data_updated, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$STATE_MANAGEMENT, {
    # Get data update context for intelligent handling
    update_context <- app_state$last_data_update_context

    # UNIFIED DATA UPDATE LOGIC - combines previous data_loaded + data_changed handlers

    # SPRINT 3: Clear performance cache when data changes
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

    # FASE 3: Unfreeze autodetect system when data is updated
    app_state$columns$auto_detect$frozen_until_next_trigger <- FALSE

    # Context-aware processing based on update type
    if (!is.null(update_context)) {
      context <- update_context$context %||% "general"
      column_update_reason <- resolve_column_update_reason(context)

      is_table_cells_edit <- identical(context, "table_cells_edited")
      # SPRINT 4: Legacy context checks removed - use pattern matching only
      is_load_context <- grepl("load|upload|new", context, ignore.case = TRUE)
      is_change_context <- grepl("change|edit|modify", context, ignore.case = TRUE)

      if (is_load_context) {
        # Data loading path - trigger auto-detection
        if (!is.null(app_state$data$current_data)) {
          emit$auto_detection_started()
        }
      } else if (is_table_cells_edit) {
        emit$navigation_changed()
        emit$visualization_update_needed() # SPRINT 1: Trigger atomic visualization update
      } else if (is_change_context) {
        # Data change path - update column choices AND trigger plot regeneration
        safe_operation(
          "Update column choices on data change",
          code = {
            update_column_choices_unified(app_state, input, output, session, ui_service, reason = column_update_reason)
          }
        )

        # Trigger plot regeneration when data is edited in table
        emit$navigation_changed()
        emit$visualization_update_needed() # SPRINT 1: Trigger atomic visualization update
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

  # SPRINT 4: Legacy compatibility observers removed - all code migrated to data_updated
  # Previously: data_loaded and data_changed observers only logged deprecation warnings

  # Reset auto-default flag for Y-akse når data opdateres
  shiny::observeEvent(app_state$events$data_updated, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$LOWEST, {
    if (!is.null(app_state$ui)) {
      app_state$ui$y_axis_unit_autoset_done <- FALSE
    }
  })

  # ============================================================================
  # SECTION 2: AUTO-DETECTION EVENTS
  # ============================================================================
  # Handles automatic column detection for X-axis, Y-axis, and other columns.
  # Priority: AUTO_DETECT for proper sequencing after data loading.
  #
  # Key Events:
  # - auto_detection_started: Triggers autodetect engine
  # - auto_detection_completed: Updates state, triggers UI sync
  #
  # Event Flow:
  # 1. Data loaded → emit$auto_detection_started()
  # 2. Run autodetect_engine() with appropriate trigger_type
  # 3. Engine detects columns, stores results in app_state
  # 4. emit$auto_detection_completed()
  # 5. Trigger UI sync to update dropdowns
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
            trigger_type = "file_upload", # This event is triggered by data uploads
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

  # ============================================================================
  # SECTION 3: UI SYNCHRONIZATION EVENTS
  # ============================================================================
  # Handles synchronization between app_state columns and UI dropdowns.
  # Priority: UI_SYNC for proper sequencing after auto-detection.
  #
  # Key Events:
  # - ui_sync_requested: Syncs UI controls with detected columns
  # - ui_sync_completed: Marks sync completion, triggers navigation
  #
  # Event Flow:
  # 1. Auto-detection completed → emit$ui_sync_needed()
  # 2. sync_ui_with_columns_unified() updates dropdowns
  # 3. emit$ui_sync_completed()
  # 4. Trigger navigation_changed for plot updates
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

  # ============================================================================
  # SECTION 4: NAVIGATION EVENTS
  # ============================================================================
  # Handles navigation state updates that trigger reactive plot regeneration.
  # Priority: STATUS_UPDATES for non-critical updates.
  #
  # Key Events:
  # - navigation_changed: Increments navigation trigger for reactives
  #
  # Event Flow:
  # 1. UI sync completed / data changed → emit$navigation_changed()
  # 2. Increment app_state$navigation$trigger
  # 3. All eventReactive(app_state$navigation$trigger) components update
  shiny::observeEvent(app_state$events$navigation_changed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$STATUS_UPDATES, {
    # Increment navigation trigger to update all eventReactive components
    app_state$navigation$trigger <- app_state$navigation$trigger + 1L
  })

  # ============================================================================
  # SECTION 5: TEST MODE EVENTS
  # ============================================================================
  # Handles test mode initialization and startup sequencing.
  # Priority: AUTO_DETECT and HIGH for proper test initialization.
  #
  # Key Events:
  # - test_mode_ready: Test mode initialization complete
  # - test_mode_startup_phase_changed: Phase transition tracking
  # - test_mode_debounced_autodetect: Debounced detection trigger
  #
  # Event Flow:
  # 1. Test mode started → emit$test_mode_ready()
  # 2. Enable race prevention
  # 3. Check if autodetect needed
  # 4. Debounce autodetect trigger
  # 5. Phase transitions: data_ready → ui_ready → complete
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

  # ============================================================================
  # SECTION 6: SESSION LIFECYCLE EVENTS
  # ============================================================================
  # Handles session initialization, manual triggers, and cleanup.
  # Priority: AUTO_DETECT for initialization, CLEANUP for reset.
  #
  # Key Events:
  # - session_started: Session initialization
  # - manual_autodetect_button: User-triggered detection
  # - session_reset: Complete state cleanup
  #
  # Event Flow:
  # Session Start:
  # 1. App initialization → emit$session_started()
  # 2. Run name-only autodetect if no data
  # 3. Setup initial state
  #
  # Session Reset:
  # 1. Reset button clicked → emit$session_reset()
  # 2. Clear all caches
  # 3. Reset all state to initial values
  shiny::observeEvent(app_state$events$session_started, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$AUTO_DETECT, {
    # Session start logic
    if (is.null(app_state$data$current_data) || nrow(app_state$data$current_data) == 0) {
      # FASE 3: Session start trigger for name-only detection
      autodetect_engine(
        data = NULL, # No data available at session start
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
      trigger_type = "manual", # This bypasses frozen state check
      app_state = app_state,
      emit = emit
    )
  })

  shiny::observeEvent(app_state$events$session_reset, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$CLEANUP, {
    # SPRINT 3: Clear all caches on session reset
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

    # FASE 3: Reset frozen state
    app_state$columns$auto_detect$frozen_until_next_trigger <- FALSE
    app_state$columns$auto_detect$last_run <- NULL
  })

  # ============================================================================
  # SECTION 7: ERROR HANDLING EVENTS
  # ============================================================================
  # Centralized error handling and recovery tracking.
  # Priority: STATE_MANAGEMENT for error state, LOW for recovery.
  #
  # Key Events:
  # - error_occurred: Unified error handler for all error types
  # - recovery_completed: Error recovery tracking
  #
  # Event Flow:
  # 1. Error detected → emit$error_occurred(type, context, details)
  # 2. Log error with context
  # 3. Type-specific error handling:
  #    - processing: Increment recovery attempts
  #    - validation: Clear validation state
  #    - network: Log for retry logic
  #    - ui: May need UI sync
  # 4. Store error in history
  # 5. If recovery successful → emit$recovery_completed()

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
        error_details = if (!is.null(error_context$details)) paste(names(error_context$details), collapse = ", ") else "none",
        timestamp = as.character(error_context$timestamp %||% Sys.time()),
        session_id = if (!is.null(session)) sanitize_session_token(session$token) else "no session",
        .context = "ERROR_SYSTEM"
      )
    } else if (!is.null(error_info)) {
      # Fallback to legacy error info
      log_debug_kv(
        error_type = error_info$type %||% "unknown",
        error_message = error_info$message %||% "no message",
        session_id = if (!is.null(session)) sanitize_session_token(session$token) else "no session",
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
        type = if (!is.null(error_context)) error_context$type else (if (!is.null(error_info)) error_info$type else "unknown"),
        context = if (!is.null(error_context)) error_context$context else "consolidated_handler",
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
      session_id = if (!is.null(session)) sanitize_session_token(session$token) else "no session",
      .context = "ERROR_SYSTEM"
    )
  })

  # ============================================================================
  # SECTION 8: UI UPDATE EVENTS
  # ============================================================================
  # Handles form field reset and restore operations.
  # Priority: LOW for background UI operations.
  #
  # Key Events:
  # - form_reset_needed: Clear all form fields
  # - form_restore_needed: Restore form from session metadata
  #
  # Event Flow:
  # Form Reset:
  # 1. Reset requested → emit$form_reset_needed()
  # 2. ui_service$reset_form_fields()
  #
  # Form Restore:
  # 1. Session loaded → emit$form_restore_needed()
  # 2. ui_service$update_form_fields(metadata)
  #
  # NOTE: column_choices_changed observer disabled due to UI clearing issue.
  # Event still emitted for tracking, but UI sync handled via ui_sync_needed.

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

  # ============================================================================
  # SECTION 9: INPUT CHANGE OBSERVERS
  # ============================================================================
  # Tracks user input changes and maintains app_state synchronization.
  # Priority: MEDIUM for user-driven changes, UI_SYNC for chart/axis logic.
  #
  # Key Observers:
  # - Column selection observers (x_column, y_column, n_column, etc.)
  # - chart_type: Chart type changes with automatic Y-axis adjustment
  # - y_axis_unit: Y-axis unit changes with chart type suggestion
  # - n_column: Denominator changes affecting Y-axis in run charts
  #
  # Token-Based Loop Prevention:
  # - Programmatic UI updates register tokens before updateSelectizeInput()
  # - Input observers consume tokens to prevent feedback loops
  # - Only user-driven changes (no token) emit events
  #
  # Event Flow:
  # 1. User changes dropdown → input[[col]] updates
  # 2. Check for pending programmatic token
  # 3. If token exists and matches: consume token, update state, SKIP event
  # 4. If no token: user change → update state + emit event
  # 5. Cache normalized value for consistency

  columns_to_observe <- c("x_column", "y_column", "n_column", "skift_column", "frys_column", "kommentar_column")

  #' Normalize Column Input
  #'
  #' Converts input values to consistent string format.
  #' Handles NULL, empty, and edge cases uniformly.
  #'
  #' @param value Input value from Shiny (can be NULL, list, vector, etc.)
  #' @return Normalized string or "" for empty/NULL values
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
    shiny::observeEvent(input[[col]],
      {
        input_received_time <- Sys.time()
        new_value <- input[[col]]

        # DROPDOWN DEBUGGING: Log input change details
        old_value <- shiny::isolate(app_state$columns[[col]]) %||% ""
        #               "to", paste0("'", new_value, "'")), "DROPDOWN_DEBUG")

        # TIMING LOGGING: Calculate time since last programmatic update
        last_update_time <- shiny::isolate(app_state$ui$last_programmatic_update)
        time_since_update <- if (!is.null(last_update_time)) {
          as.numeric(difftime(input_received_time, last_update_time, units = "secs")) * 1000
        } else {
          NA
        }

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
      },
      ignoreInit = TRUE,
      priority = OBSERVER_PRIORITIES$MEDIUM
    )
  }

  # OBSERVER: Toggle N (n_column) enabled state based on chart_type selection
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
    },
    ignoreInit = FALSE,
    priority = OBSERVER_PRIORITIES$UI_SYNC
  )

  # OBSERVER: Auto-vælg korttype baseret på Y-akse UI-type
  shiny::observeEvent(input$y_axis_unit,
    {
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
    },
    ignoreInit = TRUE,
    priority = OBSERVER_PRIORITIES$UI_SYNC
  )

  # OBSERVER: Når RUN chart og nævner ændres
  shiny::observeEvent(input$n_column,
    {
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
    },
    ignoreInit = TRUE,
    priority = OBSERVER_PRIORITIES$UI_SYNC
  )

  # ============================================================================
  # SECTION 10: PASSIVE MONITORING
  # ============================================================================
  # Passive observers that track metrics without interfering with app logic.
  # Priority: LOWEST to ensure they never block critical operations.
  #
  # Key Observers:
  # - last_programmatic_update: Performance timing metrics
  #
  # Purpose:
  # - Track system performance for optimization
  # - Monitor timing between programmatic updates
  # - Freeze-aware metrics collection
  # - No event emission or state modification

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
        } else {
          FALSE
        }

        #               ", autodetect active:", autodetect_in_progress), .context = "TIMING_MONITOR")
      }
    })
  }

  # ============================================================================
  # END OF EVENT LISTENERS SETUP
  # ============================================================================
  # All event listeners are now registered and active.
  #
  # Maintenance Guidelines:
  # 1. Keep ALL event listeners in this file for visibility
  # 2. Use clear section markers when adding new event types
  # 3. Document event flows in section headers
  # 4. Maintain priority order within sections
  # 5. Extract complex LOGIC to helper functions, but keep OBSERVERS here
  # 6. Update the table of contents in function documentation when adding sections
  #
  # For debugging event flows:
  # - All events are visible in one place
  # - Section markers guide navigation
  # - Priority system ensures correct execution order
  # - Helper functions are documented inline
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
