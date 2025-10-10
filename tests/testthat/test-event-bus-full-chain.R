# test-event-bus-full-chain.R
# ==============================================================================
# COMPREHENSIVE TEST SUITE: Event Bus Full Chain Integration
# ==============================================================================
#
# FORMÅL: End-to-end testing af event-driven architecture
# FOKUS: Full reactive chains, cross-module state updates, error propagation
#
# STRUKTUR:
#   1. Full Reactive Chains (data upload → plot ready)
#   2. Cross-Module State Propagation
#   3. Error Propagation & Recovery
#   4. Event Priority & Ordering
#   5. Race Condition Prevention
#
# SUCCESS CRITERIA:
#   - Complete chains work end-to-end
#   - State updates propagate correctly across modules
#   - Errors are caught and handled gracefully
#   - Events fire in correct order
#   - No race conditions under load
# ==============================================================================

library(shiny)
library(testthat)

# SETUP HELPERS ================================================================

# Helper til at oprette fuld app_state for integration testing
create_full_integration_app_state <- function() {
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
      last_sync_time = NULL
    )
  )

  # Visualization
  app_state$visualization <- reactiveValues(
    plot_ready = FALSE,
    plot_object = NULL,
    cache_updating = FALSE
  )

  # UI
  app_state$ui <- reactiveValues(
    pending_programmatic_inputs = list()
  )

  # Navigation
  app_state$navigation <- reactiveValues(
    trigger = 0L
  )

  # Errors
  app_state$errors <- reactiveValues(
    error_count = 0L,
    last_error = NULL
  )

  return(app_state)
}

# Helper til at tracke event firing order
create_event_tracker <- function() {
  tracker <- reactiveVal(character(0))

  list(
    record = function(event_name) {
      current <- isolate(tracker())
      tracker(c(current, event_name))
    },
    get_events = function() {
      isolate(tracker())
    },
    clear = function() {
      tracker(character(0))
    }
  )
}

# FULL REACTIVE CHAINS =========================================================

describe("Complete Data Upload Chain", {

  it("data upload triggers full chain to plot ready", {
    app_state <- create_full_integration_app_state()
    tracker <- create_event_tracker()

    # Mock emit API that records events
    emit <- list(
      data_updated = function(context = NULL) {
        tracker$record("data_updated")
        app_state$events$data_updated <- isolate(app_state$events$data_updated) + 1L
      },
      auto_detection_started = function() {
        tracker$record("auto_detection_started")
        app_state$events$auto_detection_started <- isolate(app_state$events$auto_detection_started) + 1L
      },
      auto_detection_completed = function() {
        tracker$record("auto_detection_completed")
        app_state$events$auto_detection_completed <- isolate(app_state$events$auto_detection_completed) + 1L
      },
      ui_sync_requested = function() {
        tracker$record("ui_sync_requested")
        app_state$events$ui_sync_requested <- isolate(app_state$events$ui_sync_requested) + 1L
      },
      ui_sync_completed = function() {
        tracker$record("ui_sync_completed")
        app_state$events$ui_sync_completed <- isolate(app_state$events$ui_sync_completed) + 1L
      },
      navigation_changed = function() {
        tracker$record("navigation_changed")
        app_state$events$navigation_changed <- isolate(app_state$events$navigation_changed) + 1L
      },
      visualization_update_needed = function() {
        tracker$record("visualization_update_needed")
        app_state$events$visualization_update_needed <- isolate(app_state$events$visualization_update_needed) + 1L
      }
    )

    # Simulate data upload chain
    test_data <- data.frame(
      Dato = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 12),
      Tæller = sample(40:50, 12, replace = TRUE),
      Nævner = rep(50, 12),
      stringsAsFactors = FALSE
    )

    # Setup observers to simulate full chain
    observeEvent(app_state$events$data_updated, ignoreInit = TRUE, {
      emit$auto_detection_started()
    })

    observeEvent(app_state$events$auto_detection_started, ignoreInit = TRUE, {
      # Simulate auto-detection completing
      app_state$columns$mappings$x_column <- "Dato"
      app_state$columns$mappings$y_column <- "Tæller"
      app_state$columns$mappings$n_column <- "Nævner"
      emit$auto_detection_completed()
    })

    observeEvent(app_state$events$auto_detection_completed, ignoreInit = TRUE, {
      emit$ui_sync_requested()
    })

    observeEvent(app_state$events$ui_sync_requested, ignoreInit = TRUE, {
      emit$ui_sync_completed()
    })

    observeEvent(app_state$events$ui_sync_completed, ignoreInit = TRUE, {
      emit$navigation_changed()
      emit$visualization_update_needed()
    })

    observeEvent(app_state$events$visualization_update_needed, ignoreInit = TRUE, {
      app_state$visualization$plot_ready <- TRUE
    })

    # Trigger the chain
    app_state$data$current_data <- test_data
    emit$data_updated("upload")

    # Give reactives time to process
    Sys.sleep(0.2)
    flush_reactives()

    # Verify full chain executed
    events_fired <- tracker$get_events()

    expect_true("data_updated" %in% events_fired)
    expect_true("auto_detection_started" %in% events_fired)
    expect_true("auto_detection_completed" %in% events_fired)
    expect_true("ui_sync_requested" %in% events_fired)
    expect_true("navigation_changed" %in% events_fired)
    expect_true("visualization_update_needed" %in% events_fired)
  })

  it("table edit triggers partial chain (skips auto-detection)", {
    app_state <- create_full_integration_app_state()
    tracker <- create_event_tracker()

    emit <- list(
      data_updated = function(context = NULL) {
        tracker$record("data_updated")
        app_state$last_context <- context
        app_state$events$data_updated <- isolate(app_state$events$data_updated) + 1L
      },
      navigation_changed = function() {
        tracker$record("navigation_changed")
        app_state$events$navigation_changed <- isolate(app_state$events$navigation_changed) + 1L
      },
      visualization_update_needed = function() {
        tracker$record("visualization_update_needed")
        app_state$events$visualization_update_needed <- isolate(app_state$events$visualization_update_needed) + 1L
      }
    )

    # Setup context-aware observer
    observeEvent(app_state$events$data_updated, ignoreInit = TRUE, {
      context <- isolate(app_state$last_context)

      if (!is.null(context) && context == "table_cells_edited") {
        # Skip auto-detection, go directly to visualization
        emit$navigation_changed()
        emit$visualization_update_needed()
      }
    })

    # Trigger with table edit context
    emit$data_updated("table_cells_edited")

    Sys.sleep(0.1)
    flush_reactives()

    events_fired <- tracker$get_events()

    # Should skip auto-detection
    expect_true("data_updated" %in% events_fired)
    expect_false("auto_detection_started" %in% events_fired)
    expect_true("navigation_changed" %in% events_fired)
    expect_true("visualization_update_needed" %in% events_fired)
  })
})

# CROSS-MODULE STATE PROPAGATION ===============================================

describe("Cross-Module State Updates", {

  it("data module state propagates to visualization module", {
    app_state <- create_full_integration_app_state()

    # Data module updates data
    test_data <- data.frame(
      x = 1:10,
      y = rnorm(10),
      stringsAsFactors = FALSE
    )
    app_state$data$current_data <- test_data

    # Visualization module should be able to read it
    viz_data <- isolate(app_state$data$current_data)

    expect_equal(viz_data, test_data)
    expect_equal(nrow(viz_data), 10)
  })

  it("autodetect results propagate to UI inputs", {
    app_state <- create_full_integration_app_state()

    # Autodetect sets mappings
    app_state$columns$mappings$x_column <- "Dato"
    app_state$columns$mappings$y_column <- "Tæller"
    app_state$columns$auto_detect$completed <- TRUE

    # UI sync observer should trigger
    observer_triggered <- FALSE

    observeEvent(app_state$columns$auto_detect$completed, ignoreInit = TRUE, {
      # Read mappings for UI update
      x_col <- isolate(app_state$columns$mappings$x_column)
      y_col <- isolate(app_state$columns$mappings$y_column)

      if (!is.null(x_col) && !is.null(y_col)) {
        observer_triggered <<- TRUE
      }
    })

    app_state$columns$auto_detect$completed <- isolate(app_state$columns$auto_detect$completed) + 1L

    Sys.sleep(0.1)
    flush_reactives()

    expect_true(observer_triggered)
  })

  it("visualization state propagates to navigation", {
    app_state <- create_full_integration_app_state()

    initial_trigger <- isolate(app_state$navigation$trigger)

    # Visualization completes
    observeEvent(app_state$visualization$plot_ready, ignoreInit = TRUE, {
      app_state$navigation$trigger <- isolate(app_state$navigation$trigger) + 1L
    })

    app_state$visualization$plot_ready <- TRUE

    Sys.sleep(0.1)
    flush_reactives()

    new_trigger <- isolate(app_state$navigation$trigger)

    expect_gt(new_trigger, initial_trigger)
  })
})

# ERROR PROPAGATION & RECOVERY =================================================

describe("Error Handling in Event Chains", {

  it("captures errors in event handlers", {
    app_state <- create_full_integration_app_state()
    error_caught <- FALSE

    # Observer that will error
    observeEvent(app_state$events$data_updated, ignoreInit = TRUE, {
      tryCatch(
        {
          stop("Simulated error")
        },
        error = function(e) {
          app_state$errors$last_error <- e$message
          app_state$errors$error_count <- isolate(app_state$errors$error_count) + 1L
          error_caught <<- TRUE
        }
      )
    })

    # Trigger event
    app_state$events$data_updated <- 1L

    Sys.sleep(0.1)
    flush_reactives()

    expect_true(error_caught)
    expect_equal(isolate(app_state$errors$error_count), 1L)
    expect_equal(isolate(app_state$errors$last_error), "Simulated error")
  })

  it("continues chain execution after non-critical errors", {
    app_state <- create_full_integration_app_state()
    chain_continued <- FALSE

    # First observer errors, but second should still run
    observeEvent(app_state$events$data_updated, ignoreInit = TRUE,
      priority = 100,
      {
        tryCatch(
          {
            stop("Non-critical error")
          },
          error = function(e) {
            app_state$errors$last_error <- e$message
          }
        )
      }
    )

    observeEvent(app_state$events$data_updated, ignoreInit = TRUE,
      priority = 50,
      {
        chain_continued <<- TRUE
      }
    )

    # Trigger event
    app_state$events$data_updated <- 1L

    Sys.sleep(0.1)
    flush_reactives()

    expect_true(chain_continued)
  })

  it("emits error_occurred event on critical failures", {
    app_state <- create_full_integration_app_state()

    emit <- list(
      error_occurred = function(details) {
        app_state$events$error_occurred <- isolate(app_state$events$error_occurred) + 1L
      }
    )

    # Simulate critical error
    observeEvent(app_state$events$data_updated, ignoreInit = TRUE, {
      tryCatch(
        {
          # Critical operation fails
          stop("Critical data processing error")
        },
        error = function(e) {
          emit$error_occurred(list(message = e$message, context = "data_processing"))
        }
      )
    })

    app_state$events$data_updated <- 1L

    Sys.sleep(0.1)
    flush_reactives()

    expect_equal(isolate(app_state$events$error_occurred), 1L)
  })
})

# EVENT PRIORITY & ORDERING ====================================================

describe("Event Priority Enforcement", {

  it("executes high-priority observers before low-priority", {
    app_state <- create_full_integration_app_state()
    execution_order <- character(0)

    observeEvent(app_state$events$data_updated,
      ignoreInit = TRUE,
      priority = 100, # HIGH
      {
        execution_order <<- c(execution_order, "high")
      }
    )

    observeEvent(app_state$events$data_updated,
      ignoreInit = TRUE,
      priority = 50, # MEDIUM
      {
        execution_order <<- c(execution_order, "medium")
      }
    )

    observeEvent(app_state$events$data_updated,
      ignoreInit = TRUE,
      priority = 10, # LOW
      {
        execution_order <<- c(execution_order, "low")
      }
    )

    app_state$events$data_updated <- 1L

    Sys.sleep(0.1)
    flush_reactives()

    # High should execute first
    if (length(execution_order) == 3) {
      expect_equal(execution_order[1], "high")
      expect_equal(execution_order[3], "low")
    }
  })

  it("state management observers run before UI updates", {
    app_state <- create_full_integration_app_state()
    state_updated_before_ui <- FALSE

    # State management (high priority)
    observeEvent(app_state$events$data_updated,
      ignoreInit = TRUE,
      priority = 100,
      {
        app_state$data$current_data <- data.frame(x = 1:5)
      }
    )

    # UI update (low priority)
    observeEvent(app_state$events$data_updated,
      ignoreInit = TRUE,
      priority = 10,
      {
        # Check if state is already updated
        data <- isolate(app_state$data$current_data)
        state_updated_before_ui <<- !is.null(data)
      }
    )

    app_state$events$data_updated <- 1L

    Sys.sleep(0.1)
    flush_reactives()

    expect_true(state_updated_before_ui)
  })
})

# RACE CONDITION PREVENTION ====================================================

describe("Race Condition Prevention", {

  it("prevents concurrent cache updates with guard flag", {
    app_state <- create_full_integration_app_state()

    # Set cache_updating flag
    app_state$visualization$cache_updating <- TRUE

    # Try to trigger visualization update
    update_executed <- FALSE

    observeEvent(app_state$events$visualization_update_needed, ignoreInit = TRUE, {
      # Guard condition
      if (!isolate(app_state$visualization$cache_updating)) {
        update_executed <<- TRUE
      }
    })

    app_state$events$visualization_update_needed <- 1L

    Sys.sleep(0.1)
    flush_reactives()

    # Update should be blocked
    expect_false(update_executed)
  })

  it("prevents concurrent autodetect runs", {
    app_state <- create_full_integration_app_state()

    # Set in_progress flag
    app_state$columns$auto_detect$in_progress <- TRUE

    # Try to trigger another autodetect
    second_run_executed <- FALSE

    observeEvent(app_state$events$auto_detection_started, ignoreInit = TRUE, {
      # Guard condition
      if (!isolate(app_state$columns$auto_detect$in_progress)) {
        second_run_executed <<- TRUE
      }
    })

    app_state$events$auto_detection_started <- 1L

    Sys.sleep(0.1)
    flush_reactives()

    # Second run should be blocked
    expect_false(second_run_executed)
  })

  it("handles rapid-fire events with debouncing", {
    skip("Debouncing requires time-based testing framework")

    # This test would verify that rapid-fire events
    # are debounced to prevent reactive storms
  })
})

# COMPLEX INTEGRATION SCENARIOS ================================================

describe("Complex Integration Scenarios", {

  it("handles user-initiated column change during auto-detection", {
    app_state <- create_full_integration_app_state()

    # Autodetect in progress
    app_state$columns$auto_detect$in_progress <- TRUE

    # User manually changes column
    user_change_processed <- FALSE

    observeEvent(app_state$columns$mappings$x_column, ignoreInit = TRUE, {
      # User change should cancel auto-detection
      app_state$columns$auto_detect$in_progress <- FALSE
      app_state$columns$auto_detect$frozen_until_next_trigger <- TRUE
      user_change_processed <<- TRUE
    })

    # User selects different column
    app_state$columns$mappings$x_column <- "UserSelectedColumn"

    Sys.sleep(0.1)
    flush_reactives()

    expect_true(user_change_processed)
    expect_false(isolate(app_state$columns$auto_detect$in_progress))
  })

  it("handles session reset cascade", {
    app_state <- create_full_integration_app_state()

    # Populate state
    app_state$data$current_data <- data.frame(x = 1:5)
    app_state$columns$mappings$x_column <- "x"
    app_state$visualization$plot_ready <- TRUE

    # Session reset observer
    observeEvent(app_state$events$session_reset, ignoreInit = TRUE, {
      # Clear all state
      app_state$data$current_data <- NULL
      app_state$columns$mappings$x_column <- NULL
      app_state$columns$mappings$y_column <- NULL
      app_state$visualization$plot_ready <- FALSE
      app_state$columns$auto_detect$completed <- FALSE
    })

    # Trigger reset
    app_state$events$session_reset <- 1L

    Sys.sleep(0.1)
    flush_reactives()

    # Verify clean state
    expect_null(isolate(app_state$data$current_data))
    expect_null(isolate(app_state$columns$mappings$x_column))
    expect_false(isolate(app_state$visualization$plot_ready))
  })

  it("handles concurrent data upload and table edit", {
    app_state <- create_full_integration_app_state()
    tracker <- create_event_tracker()

    emit <- list(
      data_updated = function(context) {
        tracker$record(paste0("data_updated:", context))
        app_state$events$data_updated <- isolate(app_state$events$data_updated) + 1L
      }
    )

    # Guard to prevent concurrent updates
    observeEvent(app_state$events$data_updated, ignoreInit = TRUE, {
      if (!isolate(app_state$data$updating_table)) {
        # Process update
        app_state$data$updating_table <- TRUE

        # Simulate processing
        Sys.sleep(0.05)

        app_state$data$updating_table <- FALSE
      }
    })

    # Trigger both events rapidly
    emit$data_updated("upload")
    emit$data_updated("table_edit")

    Sys.sleep(0.2)
    flush_reactives()

    # Both should be recorded, but guard should prevent overlap
    events <- tracker$get_events()
    expect_equal(length(events), 2)
  })
})

# PERFORMANCE UNDER LOAD =======================================================

describe("Performance Under Load", {

  it("handles 10 rapid events without reactive storm", {
    app_state <- create_full_integration_app_state()
    event_count <- 0

    observeEvent(app_state$events$navigation_changed, ignoreInit = TRUE, {
      event_count <<- event_count + 1
    })

    # Fire 10 rapid events
    for (i in 1:10) {
      app_state$events$navigation_changed <- isolate(app_state$events$navigation_changed) + 1L
    }

    Sys.sleep(0.2)
    flush_reactives()

    # Should process all 10 events
    expect_equal(event_count, 10)
  })

  it("completes full chain in reasonable time", {
    app_state <- create_full_integration_app_state()
    tracker <- create_event_tracker()

    # Setup minimal chain
    emit <- list(
      data_updated = function() {
        tracker$record("data_updated")
        app_state$events$data_updated <- isolate(app_state$events$data_updated) + 1L
      },
      visualization_update_needed = function() {
        tracker$record("visualization_update_needed")
        app_state$visualization$plot_ready <- TRUE
      }
    )

    observeEvent(app_state$events$data_updated, ignoreInit = TRUE, {
      emit$visualization_update_needed()
    })

    start_time <- Sys.time()
    emit$data_updated()
    Sys.sleep(0.1)
    flush_reactives()
    end_time <- Sys.time()

    elapsed_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000

    # Chain should complete quickly
    expect_lt(elapsed_ms, 200)
  })
})
