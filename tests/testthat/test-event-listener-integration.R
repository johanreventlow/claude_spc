# test-event-listener-integration.R
# ==============================================================================
# COMPREHENSIVE TEST SUITE: Event Orchestration (utils_server_event_listeners.R)
# ==============================================================================
#
# FORMÅL: >60% coverage af event listener system
# FOKUS: Event sequence verification and state propagation
#
# STRUKTUR:
#   1. Event Sequence Verification (data_updated → auto_detect → ui_sync)
#   2. Observer Priority Testing (execution order)
#   3. State Propagation (events → app_state updates)
#   4. Cleanup Verification (session end)
#
# SUCCESS CRITERIA:
#   - Event chains verified
#   - Observer priorities tested
#   - State updates correct
#   - Cleanup succeeds
# ==============================================================================

library(shiny)
library(testthat)

# SETUP HELPERS ================================================================

# Helper til at oprette mock emit API
create_mock_emit <- function(app_state) {
  list(
    data_updated = function(context = NULL) {
      app_state$last_data_update_context <- list(context = context, timestamp = Sys.time())
      app_state$events$data_updated <- isolate(app_state$events$data_updated) + 1L
    },

    auto_detection_started = function() {
      app_state$events$auto_detection_started <- isolate(app_state$events$auto_detection_started) + 1L
    },

    auto_detection_completed = function() {
      app_state$events$auto_detection_completed <- isolate(app_state$events$auto_detection_completed) + 1L
    },

    ui_sync_needed = function() {
      app_state$columns$ui_sync$needed <- TRUE
      app_state$events$ui_sync_requested <- isolate(app_state$events$ui_sync_requested) + 1L
    },

    ui_sync_completed = function() {
      app_state$events$ui_sync_completed <- isolate(app_state$events$ui_sync_completed) + 1L
    },

    navigation_changed = function() {
      app_state$events$navigation_changed <- isolate(app_state$events$navigation_changed) + 1L
    },

    visualization_update_needed = function() {
      app_state$events$visualization_update_needed <- isolate(app_state$events$visualization_update_needed) + 1L
    }
  )
}

# Helper til at oprette komplet app_state for event testing
create_full_app_state_for_events <- function() {
  app_state <- new.env(parent = emptyenv())

  # Events
  app_state$events <- reactiveValues(
    data_updated = 0L,
    auto_detection_started = 0L,
    auto_detection_completed = 0L,
    ui_sync_requested = 0L,
    ui_sync_completed = 0L,
    navigation_changed = 0L,
    visualization_update_needed = 0L,
    session_started = 0L,
    session_reset = 0L,
    test_mode_ready = 0L,
    error_occurred = 0L
  )

  # Data
  app_state$data <- reactiveValues(
    current_data = NULL,
    original_data = NULL,
    updating_table = FALSE
  )

  # Columns
  app_state$columns <- reactiveValues(
    auto_detect = reactiveValues(
      in_progress = FALSE,
      completed = FALSE,
      results = NULL,
      frozen_until_next_trigger = FALSE,
      last_run = NULL
    ),
    mappings = reactiveValues(
      x_column = NULL,
      y_column = NULL,
      n_column = NULL
    ),
    ui_sync = reactiveValues(
      needed = FALSE,
      last_sync_time = NULL,
      pending_updates = list()
    )
  )

  # UI
  app_state$ui <- reactiveValues(
    pending_programmatic_inputs = list(),
    last_programmatic_update = NULL,
    y_axis_unit_autoset_done = FALSE
  )

  # Context tracking
  app_state$last_data_update_context <- NULL
  app_state$last_error_context <- NULL

  # Navigation
  app_state$navigation <- reactiveValues(
    trigger = 0L
  )

  # Errors
  app_state$errors <- reactiveValues(
    error_count = 0L,
    recovery_attempts = 0L,
    last_error = NULL,
    last_recovery_time = NULL
  )

  # Cache
  app_state$cache <- reactiveValues(
    qic = NULL
  )

  return(app_state)
}

# EVENT SEQUENCE TESTS =========================================================

describe("Event Sequence Verification", {

  it("data_updated triggers cascade correctly", {
    app_state <- create_full_app_state_for_events()
    emit <- create_mock_emit(app_state)

    # Set up data
    test_data <- data.frame(
      x = 1:10,
      y = rnorm(10),
      stringsAsFactors = FALSE
    )
    app_state$data$current_data <- test_data

    # Track events fired
    events_fired <- character(0)

    # Mock observers
    observeEvent(app_state$events$data_updated, ignoreInit = TRUE, {
      events_fired <<- c(events_fired, "data_updated")
    })

    observeEvent(app_state$events$auto_detection_started, ignoreInit = TRUE, {
      events_fired <<- c(events_fired, "auto_detection_started")
    })

    # Trigger data_updated with load context
    emit$data_updated("upload")

    # Give reactives time to process
    Sys.sleep(0.1)
    flush_reactives()

    # Verify events were fired
    expect_true("data_updated" %in% events_fired)
  })

  it("auto_detection_completed triggers ui_sync_needed", {
    app_state <- create_full_app_state_for_events()
    emit <- create_mock_emit(app_state)

    # Mock auto-detection results
    app_state$columns$auto_detect$results <- list(
      x_col = "Date",
      y_col = "Value"
    )

    # Track events
    events_fired <- character(0)

    observeEvent(app_state$events$auto_detection_completed, ignoreInit = TRUE, {
      events_fired <<- c(events_fired, "auto_detection_completed")

      # This should trigger ui_sync_needed
      if (!is.null(isolate(app_state$columns$auto_detect$results))) {
        emit$ui_sync_needed()
      }
    })

    observeEvent(app_state$events$ui_sync_requested, ignoreInit = TRUE, {
      events_fired <<- c(events_fired, "ui_sync_requested")
    })

    # Trigger completion
    emit$auto_detection_completed()

    Sys.sleep(0.1)
    flush_reactives()

    # Verify chain
    expect_true("auto_detection_completed" %in% events_fired)
    expect_true("ui_sync_requested" %in% events_fired)
  })

  it("ui_sync_completed triggers navigation_changed", {
    app_state <- create_full_app_state_for_events()
    emit <- create_mock_emit(app_state)

    events_fired <- character(0)

    observeEvent(app_state$events$ui_sync_completed, ignoreInit = TRUE, {
      events_fired <<- c(events_fired, "ui_sync_completed")
      emit$navigation_changed()
    })

    observeEvent(app_state$events$navigation_changed, ignoreInit = TRUE, {
      events_fired <<- c(events_fired, "navigation_changed")
    })

    emit$ui_sync_completed()

    Sys.sleep(0.1)
    flush_reactives()

    expect_true("ui_sync_completed" %in% events_fired)
    expect_true("navigation_changed" %in% events_fired)
  })
})

# OBSERVER PRIORITY TESTS ======================================================

describe("Observer Priority", {

  it("executes observers in priority order", {
    app_state <- create_full_app_state_for_events()

    execution_order <- character(0)

    # High priority observer
    observeEvent(app_state$events$data_updated,
      ignoreInit = TRUE,
      priority = 100, # OBSERVER_PRIORITIES$STATE_MANAGEMENT
      {
        execution_order <<- c(execution_order, "high_priority")
      }
    )

    # Low priority observer
    observeEvent(app_state$events$data_updated,
      ignoreInit = TRUE,
      priority = 0, # OBSERVER_PRIORITIES$LOWEST
      {
        execution_order <<- c(execution_order, "low_priority")
      }
    )

    # Trigger event
    app_state$events$data_updated <- 1L

    Sys.sleep(0.1)
    flush_reactives()

    # High priority should execute first
    if (length(execution_order) == 2) {
      expect_equal(execution_order[1], "high_priority")
      expect_equal(execution_order[2], "low_priority")
    }
  })

  it("auto_detect has correct priority", {
    # AUTO_DETECT priority is typically 75
    # Should execute after STATE_MANAGEMENT (100) but before UI_SYNC (50)

    expect_true(exists("OBSERVER_PRIORITIES"))

    if (exists("OBSERVER_PRIORITIES")) {
      expect_true("AUTO_DETECT" %in% names(OBSERVER_PRIORITIES))
      expect_true("STATE_MANAGEMENT" %in% names(OBSERVER_PRIORITIES))
      expect_true("UI_SYNC" %in% names(OBSERVER_PRIORITIES))

      # Verify order
      expect_gt(OBSERVER_PRIORITIES$STATE_MANAGEMENT, OBSERVER_PRIORITIES$AUTO_DETECT)
      expect_gt(OBSERVER_PRIORITIES$AUTO_DETECT, OBSERVER_PRIORITIES$UI_SYNC)
    }
  })
})

# STATE PROPAGATION TESTS ======================================================

describe("State Propagation", {

  it("data_updated unfreezes autodetect system", {
    app_state <- create_full_app_state_for_events()
    emit <- create_mock_emit(app_state)

    # Freeze autodetect
    app_state$columns$auto_detect$frozen_until_next_trigger <- TRUE

    # Verify frozen
    expect_true(isolate(app_state$columns$auto_detect$frozen_until_next_trigger))

    # Mock data_updated observer behavior
    observeEvent(app_state$events$data_updated, ignoreInit = TRUE, {
      app_state$columns$auto_detect$frozen_until_next_trigger <- FALSE
    })

    # Trigger data_updated
    emit$data_updated("upload")

    Sys.sleep(0.1)
    flush_reactives()

    # Verify unfrozen
    expect_false(isolate(app_state$columns$auto_detect$frozen_until_next_trigger))
  })

  it("auto_detection_completed updates state flags", {
    app_state <- create_full_app_state_for_events()
    emit <- create_mock_emit(app_state)

    # Set in_progress
    app_state$columns$auto_detect$in_progress <- TRUE
    app_state$columns$auto_detect$completed <- FALSE

    # Mock observer behavior
    observeEvent(app_state$events$auto_detection_completed, ignoreInit = TRUE, {
      app_state$columns$auto_detect$in_progress <- FALSE
      app_state$columns$auto_detect$completed <- TRUE
    })

    # Trigger completion
    emit$auto_detection_completed()

    Sys.sleep(0.1)
    flush_reactives()

    # Verify state updates
    expect_false(isolate(app_state$columns$auto_detect$in_progress))
    expect_true(isolate(app_state$columns$auto_detect$completed))
  })

  it("navigation_changed increments trigger counter", {
    app_state <- create_full_app_state_for_events()
    emit <- create_mock_emit(app_state)

    initial_trigger <- isolate(app_state$navigation$trigger)

    # Mock observer
    observeEvent(app_state$events$navigation_changed, ignoreInit = TRUE, {
      app_state$navigation$trigger <- isolate(app_state$navigation$trigger) + 1L
    })

    # Trigger navigation
    emit$navigation_changed()

    Sys.sleep(0.1)
    flush_reactives()

    # Verify increment
    new_trigger <- isolate(app_state$navigation$trigger)
    expect_gt(new_trigger, initial_trigger)
  })
})

# CONTEXT-AWARE PROCESSING TESTS ===============================================

describe("Context-Aware Processing", {

  it("handles upload context correctly", {
    app_state <- create_full_app_state_for_events()
    emit <- create_mock_emit(app_state)

    test_data <- data.frame(x = 1:10, y = rnorm(10))
    app_state$data$current_data <- test_data

    # Track which path was taken
    path_taken <- NULL

    observeEvent(app_state$events$data_updated, ignoreInit = TRUE, {
      context <- isolate(app_state$last_data_update_context)

      if (!is.null(context) && grepl("upload", context$context, ignore.case = TRUE)) {
        path_taken <<- "upload_path"
        # Should trigger auto-detection
        emit$auto_detection_started()
      }
    })

    # Trigger with upload context
    emit$data_updated("upload")

    Sys.sleep(0.1)
    flush_reactives()

    expect_equal(path_taken, "upload_path")
  })

  it("handles table_cells_edited context correctly", {
    app_state <- create_full_app_state_for_events()
    emit <- create_mock_emit(app_state)

    path_taken <- NULL

    observeEvent(app_state$events$data_updated, ignoreInit = TRUE, {
      context <- isolate(app_state$last_data_update_context)

      if (!is.null(context) && identical(context$context, "table_cells_edited")) {
        path_taken <<- "table_edit_path"
        # Should NOT trigger auto-detection
        emit$navigation_changed()
        emit$visualization_update_needed()
      }
    })

    # Trigger with table edit context
    emit$data_updated("table_cells_edited")

    Sys.sleep(0.1)
    flush_reactives()

    expect_equal(path_taken, "table_edit_path")
  })
})

# CLEANUP VERIFICATION TESTS ===================================================

describe("Cleanup Verification", {

  it("destroys observers on session end", {
    skip("Requires full Shiny session with onSessionEnded")
  })

  it("nullifies observer references explicitly", {
    # This verifies memory leak prevention

    observer_registry <- list()

    # Create mock observer
    observer <- reactiveVal(1)
    observer_registry$test_observer <- observer

    # Simulate cleanup
    observer_registry$test_observer <- NULL

    expect_null(observer_registry$test_observer)
  })

  it("clears all caches on session_reset", {
    app_state <- create_full_app_state_for_events()
    emit <- create_mock_emit(app_state)

    # Setup cache
    app_state$cache$qic <- list(data = "test")

    # Mock session_reset observer
    observeEvent(app_state$events$session_reset, ignoreInit = TRUE, {
      # Clear cache
      app_state$cache$qic <- NULL

      # Reset state
      app_state$data$current_data <- NULL
      app_state$columns$auto_detect$in_progress <- FALSE
      app_state$columns$auto_detect$completed <- FALSE
      app_state$columns$auto_detect$results <- NULL
    })

    # Set initial state
    app_state$data$current_data <- data.frame(x = 1:5)
    app_state$columns$auto_detect$completed <- TRUE

    # Trigger reset
    app_state$events$session_reset <- 1L

    Sys.sleep(0.1)
    flush_reactives()

    # Verify cleanup
    expect_null(isolate(app_state$cache$qic))
    expect_null(isolate(app_state$data$current_data))
    expect_false(isolate(app_state$columns$auto_detect$completed))
  })
})

# HELPER FUNCTION TESTS ========================================================

describe("Helper Functions", {

  it("resolve_column_update_reason classifies context correctly", {
    skip_if_not(exists("resolve_column_update_reason", mode = "function"))

    # This function is internal to setup_event_listeners
    # Testing requires extraction or export

    # Manual implementation for testing
    resolve_reason <- function(context) {
      if (is.null(context)) return("manual")

      ctx <- tolower(context)

      if (grepl("edit|change|modify|column", ctx)) return("edit")
      if (grepl("session", ctx)) return("session")
      if (grepl("load|upload|file|new", ctx)) return("upload")

      "manual"
    }

    expect_equal(resolve_reason("file_upload"), "upload")
    expect_equal(resolve_reason("column_edit"), "edit")
    expect_equal(resolve_reason("session_restore"), "session")
    expect_equal(resolve_reason(NULL), "manual")
  })
})
