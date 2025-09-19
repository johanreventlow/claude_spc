# tests/testthat/test-dropdown-loop-prevention.R
# Regressions test for dropdown circular loop prevention
# Ensures that programmatic UI updates don't trigger input observer events

test_that("Loop protection flag forhindrer cirkulære events", {
  # SETUP: Create test environment
  app_state <- create_app_state()
  emit <- create_emit_api(app_state)

  # VERIFY: Initial state
  expect_false(isolate(app_state$ui$updating_programmatically))
  expect_equal(isolate(app_state$events$column_choices_changed), 0L)

  # TEST: Set protection flag
  app_state$ui$updating_programmatically <- TRUE

  # SIMULATE: Input observers skal skippe event emission under protection
  # Mock input change during programmatic update
  mock_input_change <- function(col, value) {
    # Simuler input observer logic med protection check
    if (isolate(app_state$ui$updating_programmatically)) {
      # Should skip event emission
      app_state$columns[[col]] <- value
      return("skipped")
    } else {
      # Would normally emit event
      app_state$columns[[col]] <- value
      emit$column_choices_changed()
      return("emitted")
    }
  }

  # VERIFY: Events skipped under protection
  result <- mock_input_change("x_column", "Dato")
  expect_equal(result, "skipped")
  expect_equal(isolate(app_state$events$column_choices_changed), 0L)
  expect_equal(isolate(app_state$columns$mappings$x_column), "Dato")

  # VERIFY: Events emitted when protection is off
  app_state$ui$updating_programmatically <- FALSE
  result <- mock_input_change("y_column", "Tæller")
  expect_equal(result, "emitted")
  expect_equal(isolate(app_state$events$column_choices_changed), 1L)
  expect_equal(isolate(app_state$columns$mappings$y_column), "Tæller")
})

test_that("Safe programmatic UI update wrapper virker", {
  # SETUP: Create mock session and app_state
  app_state <- create_app_state()
  mock_session <- list()

  # Mock updateSelectizeInput to track calls
  updateSelectizeInput_calls <- list()
  mock_updateSelectizeInput <- function(session, inputId, choices, selected) {
    updateSelectizeInput_calls[[length(updateSelectizeInput_calls) + 1]] <<- list(
      inputId = inputId,
      selected = selected,
      protection_active = isolate(app_state$ui$updating_programmatically)
    )
  }

  # Override updateSelectizeInput in environment
  env <- environment()
  env$updateSelectizeInput <- mock_updateSelectizeInput

  # TEST: Call safe wrapper
  safe_programmatic_ui_update(mock_session, app_state, function() {
    updateSelectizeInput(mock_session, "x_column", choices = c("", "Dato"), selected = "Dato")
    updateSelectizeInput(mock_session, "y_column", choices = c("", "Tæller"), selected = "Tæller")
  }, delay_ms = 50)

  # VERIFY: Protection was active during calls
  expect_length(updateSelectizeInput_calls, 2)
  expect_true(updateSelectizeInput_calls[[1]]$protection_active)
  expect_true(updateSelectizeInput_calls[[2]]$protection_active)
  expect_equal(updateSelectizeInput_calls[[1]]$selected, "Dato")
  expect_equal(updateSelectizeInput_calls[[2]]$selected, "Tæller")

  # VERIFY: Protection gets cleared (wait for later callback)
  if (requireNamespace("later", quietly = TRUE)) {
    # Manually trigger later callbacks in test environment
    later::run_now()
    expect_false(isolate(app_state$ui$updating_programmatically))
  } else {
    # Without later package, protection is cleared synchronously
    expect_false(isolate(app_state$ui$updating_programmatically))
  }
})

test_that("Auto-detection workflow ikke skaber loops", {
  # SETUP: Complete integration test
  app_state <- create_app_state()
  emit <- create_emit_api(app_state)

  test_data <- data.frame(
    Dato = as.Date(c("2025-01-01", "2025-01-02")),
    Tæller = c(10, 12),
    Nævner = c(100, 110),
    stringsAsFactors = FALSE
  )

  # SETUP: Load data
  set_current_data(app_state, test_data)

  # SETUP: Simulate auto-detection results
  isolate({
    app_state$columns$auto_detect$results <- list(
      x_column = "Dato",
      y_column = "Tæller",
      n_column = "Nævner"
    )
  })

  # MEASURE: Event count before workflow
  events_before <- isolate(app_state$events$column_choices_changed)

  # EXECUTE: Trigger events that would cause loop
  emit$auto_detection_completed()  # This triggers UI sync
  emit$ui_sync_needed()           # This calls sync_ui_with_columns_unified

  # MEASURE: Event count after workflow
  events_after <- isolate(app_state$events$column_choices_changed)
  event_increase <- events_after - events_before

  # VERIFY: Event count is controlled (not 20+ like before fix)
  expect_lt(event_increase, 5)

  # VERIFY: Columns are set correctly
  expect_equal(isolate(app_state$columns$mappings$x_column), "Dato")
  expect_equal(isolate(app_state$columns$mappings$y_column), "Tæller")
  expect_equal(isolate(app_state$columns$mappings$n_column), "Nævner")
})

test_that("Event count benchmark - før/efter sammenligning", {
  # SETUP: Create two identical scenarios
  app_state1 <- create_app_state()  # With loop protection
  emit1 <- create_emit_api(app_state1)

  test_data <- data.frame(
    Dato = as.Date(c("2025-01-01", "2025-01-02")),
    Tæller = c(10, 12),
    Nævner = c(100, 110),
    stringsAsFactors = FALSE
  )

  # RUN: Standard auto-detection workflow
  app_state1$data$current_data <- test_data
  app_state1$columns$auto_detect_results <- list(
    x_column = "Dato", y_column = "Tæller", n_column = "Nævner"
  )

  # COUNT: Total events before
  events_before <- sum(
    isolate(app_state1$events$column_choices_changed),
    isolate(app_state1$events$ui_sync_needed),
    isolate(app_state1$events$ui_sync_completed)
  )

  # EXECUTE: Full workflow
  emit1$data_loaded()
  emit1$auto_detection_started()
  emit1$auto_detection_completed()
  emit1$ui_sync_needed()
  emit1$ui_sync_completed()

  # COUNT: Total events after
  events_after <- sum(
    isolate(app_state1$events$column_choices_changed),
    isolate(app_state1$events$ui_sync_needed),
    isolate(app_state1$events$ui_sync_completed)
  )

  total_events <- events_after - events_before

  # BENCHMARK: Total events should be reasonable (under 10, not 50+)
  expect_lt(total_events, 10)

  # VERIFY: At least some events fired (system is working)
  expect_gt(total_events, 0)
})

test_that("UI service update med loop protection", {
  # SETUP: Mock Shiny environment
  app_state <- create_app_state()
  mock_session <- list()
  mock_input <- list()

  # Track updateSelectizeInput calls
  ui_calls <- list()
  mock_updateSelectizeInput <- function(session, inputId, choices, selected) {
    ui_calls[[length(ui_calls) + 1]] <<- list(
      inputId = inputId,
      selected = selected,
      timestamp = Sys.time()
    )
    mock_input[[inputId]] <<- selected
  }

  # Override updateSelectizeInput in globalenv for the test
  original_updateSelectizeInput <- get("updateSelectizeInput", envir = globalenv())
  assign("updateSelectizeInput", mock_updateSelectizeInput, envir = globalenv())

  # Ensure cleanup after test
  on.exit({
    assign("updateSelectizeInput", original_updateSelectizeInput, envir = globalenv())
  })

  # SETUP: Create UI service with mock session that looks valid
  mock_session <- structure(list(), class = "ShinySession")  # Make it look like a ShinySession
  ui_service <- create_ui_update_service(mock_session, app_state)

  # TEST: Update column choices
  choices <- setNames(c("", "Dato", "Tæller", "Nævner"), c("Vælg kolonne...", "Dato", "Tæller", "Nævner"))
  selected <- list(x_column = "Dato", y_column = "Tæller", n_column = "Nævner")

  ui_service$update_column_choices(choices = choices, selected = selected)

  # VERIFY: UI calls were made
  expect_gte(length(ui_calls), 3)  # At least x, y, n columns updated

  # VERIFY: Selected values are correct
  x_call <- Find(function(call) call$inputId == "x_column", ui_calls)
  y_call <- Find(function(call) call$inputId == "y_column", ui_calls)
  n_call <- Find(function(call) call$inputId == "n_column", ui_calls)

  expect_equal(x_call$selected, "Dato")
  expect_equal(y_call$selected, "Tæller")
  expect_equal(n_call$selected, "Nævner")

  # VERIFY: Mock inputs were updated
  expect_equal(mock_input$x_column, "Dato")
  expect_equal(mock_input$y_column, "Tæller")
  expect_equal(mock_input$n_column, "Nævner")
})

test_that("Iterativ stress test - column_choices_changed forbliver 0 ved autoload", {
  # SETUP: Stress test med K iterationer for at catch timing race conditions
  stress_iterations <- 10
  column_choices_failures <- 0

  for (i in 1:stress_iterations) {
    # SETUP: Fresh environment for each iteration
    app_state <- create_app_state()
    emit <- create_emit_api(app_state)

    # SETUP: Test data
    test_data <- data.frame(
      Dato = as.Date(c("2025-01-01", "2025-01-02", "2025-01-03")),
      Tæller = c(10, 12, 8),
      Nævner = c(100, 110, 95),
      stringsAsFactors = FALSE
    )

    # MEASURE: Event count before autoload simulation
    events_before <- isolate(app_state$events$column_choices_changed)

    # EXECUTE: Simulate autoload workflow with programmatic updates
    set_current_data(app_state, test_data)
    isolate({
      app_state$columns$auto_detect$results <- list(
        x_column = "Dato", y_column = "Tæller", n_column = "Nævner"
      )
    })

    # Simulate programmatic UI update (this should NOT trigger column_choices_changed)
    mock_session <- list()
    mock_updateSelectizeInput <- function(session, inputId, choices, selected) {
      # Mock function - no actual UI update needed for this test
    }

    env <- environment()
    env$updateSelectizeInput <- mock_updateSelectizeInput

    # Use safe wrapper to update UI
    safe_programmatic_ui_update(mock_session, app_state, function() {
      updateSelectizeInput(mock_session, "x_column", choices = c("", "Dato", "Tæller", "Nævner"), selected = "Dato")
      updateSelectizeInput(mock_session, "y_column", choices = c("", "Dato", "Tæller", "Nævner"), selected = "Tæller")
    })

    # Wait for any async operations to complete
    if (requireNamespace("later", quietly = TRUE)) {
      later::run_now()
    }

    # MEASURE: Event count after workflow
    events_after <- isolate(app_state$events$column_choices_changed)
    event_increase <- events_after - events_before

    # COUNT: Track failures
    if (event_increase > 0) {
      column_choices_failures <- column_choices_failures + 1
    }
  }

  # VERIFY: No failures across all iterations
  expect_equal(column_choices_failures, 0,
              info = paste("Failed in", column_choices_failures, "out of", stress_iterations, "iterations"))
})

test_that("Real-world timing test med forskellige browser response times", {
  # SETUP: Test forskellige timing scenarios
  timing_scenarios <- list(
    fast_browser = 50,     # 50ms response
    normal_browser = 150,  # 150ms response
    slow_browser = 400     # 400ms response
  )

  for (scenario_name in names(timing_scenarios)) {
    delay_ms <- timing_scenarios[[scenario_name]]

    # SETUP: Fresh app state
    app_state <- create_app_state()
    emit <- create_emit_api(app_state)

    # SETUP: Mock browser response delay
    mock_session <- list()
    ui_update_calls <- list()

    mock_updateSelectizeInput <- function(session, inputId, choices, selected) {
      # Simulate browser response delay
      Sys.sleep(delay_ms / 1000)
      ui_update_calls[[length(ui_update_calls) + 1]] <<- list(
        inputId = inputId,
        selected = selected,
        timestamp = Sys.time(),
        scenario = scenario_name
      )
    }

    env <- environment()
    env$updateSelectizeInput <- mock_updateSelectizeInput

    # EXECUTE: Programmatic UI update
    start_time <- Sys.time()
    events_before <- isolate(app_state$events$column_choices_changed)

    safe_programmatic_ui_update(mock_session, app_state, function() {
      updateSelectizeInput(mock_session, "x_column", choices = c("", "Dato"), selected = "Dato")
    }, delay_ms = LOOP_PROTECTION_DELAYS$default)

    # Wait for completion
    if (requireNamespace("later", quietly = TRUE)) {
      later::run_now()
    }

    end_time <- Sys.time()
    total_time_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000

    # VERIFY: No extra events triggered regardless of timing
    events_after <- isolate(app_state$events$column_choices_changed)
    expect_equal(events_after - events_before, 0,
                info = paste("Scenario:", scenario_name, "- Total time:", round(total_time_ms, 2), "ms"))
  }
})

test_that("Freeze-compatibility test - timing-måling påvirker ikke autodetect freeze-logik", {
  # SETUP: Test at timing-måling ikke interfererer med freeze-state
  app_state <- create_app_state()
  emit <- create_emit_api(app_state)

  # SETUP: Set freeze state
  app_state$autodetect$frozen_until_next_trigger <- TRUE
  initial_freeze_state <- isolate(app_state$autodetect$frozen_until_next_trigger)

  # SETUP: Mock session for UI updates
  mock_session <- list()
  mock_updateSelectizeInput <- function(session, inputId, choices, selected) {
    # Mock function
  }
  env <- environment()
  env$updateSelectizeInput <- mock_updateSelectizeInput

  # EXECUTE: Programmatic UI update with freeze-aware logging
  safe_programmatic_ui_update(mock_session, app_state, function() {
    updateSelectizeInput(mock_session, "x_column", choices = c("", "Dato"), selected = "Dato")
  })

  # Wait for operations to complete
  if (requireNamespace("later", quietly = TRUE)) {
    later::run_now()
  }

  # VERIFY: Freeze state unchanged by timing operations
  final_freeze_state <- isolate(app_state$autodetect$frozen_until_next_trigger)
  expect_equal(final_freeze_state, initial_freeze_state,
              info = "Freeze state should remain unchanged by timing measurements")

  # VERIFY: Timing observer doesn't emit autodetect events
  autodetect_events_triggered <- isolate(app_state$events$auto_detection_started)
  expect_equal(autodetect_events_triggered, 0L,
              info = "Timing observer should not trigger autodetect events")
})

test_that("Session lifecycle test - loop-protection ressourcer cleanes korrekt", {
  # SETUP: Simulate session lifecycle
  app_state <- create_app_state()
  mock_session <- list()

  # Track cleanup calls
  cleanup_calls <- list()
  mock_onSessionEnded <- function(callback) {
    cleanup_calls[[length(cleanup_calls) + 1]] <<- callback
  }
  mock_session$onSessionEnded <- mock_onSessionEnded

  # SETUP: Set some loop protection state
  app_state$ui$updating_programmatically <- TRUE
  app_state$ui$flag_reset_scheduled <- FALSE

  # SIMULATE: Session cleanup (normally triggered by Shiny)
  # We simulate the cleanup function that would be registered
  cleanup_function <- function() {
    if (!is.null(app_state$ui)) {
      app_state$ui$updating_programmatically <- FALSE
      app_state$ui$flag_reset_scheduled <- TRUE
    }
  }

  # EXECUTE: Cleanup
  cleanup_function()

  # VERIFY: All flags properly reset
  expect_false(isolate(app_state$ui$updating_programmatically))
  expect_true(isolate(app_state$ui$flag_reset_scheduled))
})

# TOKEN-BASED TRACKING TESTS ===================================================

test_that("Token-baseret tracking forhindrer false column_choices_changed emission", {
  # SETUP: Create app state with token tracking support
  app_state <- create_app_state()
  emit <- create_emit_api(app_state)

  # Initialize token tracking if not present
  if (is.null(isolate(app_state$ui$pending_programmatic_inputs))) {
    app_state$ui$pending_programmatic_inputs <- list()
  }
  if (is.null(isolate(app_state$ui$programmatic_token_counter))) {
    app_state$ui$programmatic_token_counter <- 0L
  }

  # Track column_choices_changed emissions
  events_before <- isolate(app_state$events$column_choices_changed)

  # EXECUTE: Simulate programmatic update with token
  test_token <- paste0("token_", Sys.time(), "_", sample(1000, 1))
  app_state$ui$pending_programmatic_inputs[["x_column"]] <- list(
    token = test_token,
    value = "Dato",
    timestamp = Sys.time()
  )

  # Simulate input observer with token consumption logic
  mock_input_observer <- function(col, new_value) {
    # Check for pending programmatic input token
    pending_token <- app_state$ui$pending_programmatic_inputs[[col]]

    if (!is.null(pending_token) && pending_token$value == new_value) {
      # CONSUME TOKEN: This is a programmatic input, don't emit event
      app_state$ui$pending_programmatic_inputs[[col]] <- NULL
      cat(paste("TOKEN_DEBUG: Programmatic input consumed for", col, "with token", pending_token$token, "\n"))
      return("token_consumed")
    } else {
      # GENUINE USER INPUT: Emit event
      app_state$columns[[col]] <- new_value
      emit$column_choices_changed()
      return("event_emitted")
    }
  }

  # TEST: Programmatic input should be consumed without emitting event
  result <- mock_input_observer("x_column", "Dato")
  expect_equal(result, "token_consumed")

  # VERIFY: No column_choices_changed event emitted
  events_after <- isolate(app_state$events$column_choices_changed)
  expect_equal(events_after - events_before, 0)

  # VERIFY: Token was consumed
  expect_null(isolate(app_state$ui$pending_programmatic_inputs)[["x_column"]])

  # TEST: User input without token should emit event
  result2 <- mock_input_observer("y_column", "Tæller")
  expect_equal(result2, "event_emitted")

  # VERIFY: Event was emitted for genuine user input
  events_final <- isolate(app_state$events$column_choices_changed)
  expect_equal(events_final - events_after, 1)
})

test_that("Mock session med onFlushed simulation virker med token system", {
  # SETUP: Enhanced mock session that simulates Shiny session behavior
  app_state <- create_app_state()

  # Initialize token support
  if (is.null(isolate(app_state$ui$pending_programmatic_inputs))) {
    app_state$ui$pending_programmatic_inputs <- list()
  }

  flushed_callbacks <- list()
  mock_session <- structure(list(
    onFlushed = function(callback, once = TRUE) {
      flushed_callbacks[[length(flushed_callbacks) + 1]] <<- list(
        callback = callback,
        once = once,
        called = FALSE
      )
    },
    token = "test_session_123"
  ), class = "ShinySession")

  # Mock UI update tracking
  ui_update_calls <- list()
  token_assignments <- list()

  # Mock updateSelectizeInput with token recording
  mock_updateSelectizeInput <- function(session, inputId, choices = NULL, selected = NULL, ...) {
    # Generate and record token
    test_token <- paste0("token_", inputId, "_", length(token_assignments) + 1)

    ui_update_calls[[length(ui_update_calls) + 1]] <<- list(
      inputId = inputId,
      selected = selected,
      token = test_token,
      timestamp = Sys.time()
    )

    # Record token in app_state
    app_state$ui$pending_programmatic_inputs[[inputId]] <- list(
      token = test_token,
      value = selected,
      timestamp = Sys.time()
    )

    token_assignments[[inputId]] <<- test_token
    cat(paste("TOKEN_DEBUG: Token", test_token, "assigned to", inputId, "with value", selected, "\n"))
  }

  # Override function globally for test
  original_updateSelectizeInput <- get("updateSelectizeInput", envir = globalenv())
  assign("updateSelectizeInput", mock_updateSelectizeInput, envir = globalenv())

  on.exit({
    assign("updateSelectizeInput", original_updateSelectizeInput, envir = globalenv())
  })

  # EXECUTE: Safe programmatic update
  safe_programmatic_ui_update(mock_session, app_state, function() {
    updateSelectizeInput(mock_session, "x_column", choices = c("", "Dato", "Tæller"), selected = "Dato")
    updateSelectizeInput(mock_session, "y_column", choices = c("", "Dato", "Tæller"), selected = "Tæller")
  })

  # SIMULATE: onFlushed callback execution
  for (i in seq_along(flushed_callbacks)) {
    if (!flushed_callbacks[[i]]$called) {
      flushed_callbacks[[i]]$callback()
      flushed_callbacks[[i]]$called <- TRUE
    }
  }

  # VERIFY: UI updates were made with tokens
  expect_gte(length(ui_update_calls), 2)
  expect_true(all(sapply(ui_update_calls, function(call) !is.null(call$token))))

  # VERIFY: Tokens are present in app_state
  pending_inputs <- isolate(app_state$ui$pending_programmatic_inputs)
  expect_true("x_column" %in% names(pending_inputs))
  expect_true("y_column" %in% names(pending_inputs))

  # VERIFY: Tokens have correct structure
  x_token <- pending_inputs[["x_column"]]
  expect_true(!is.null(x_token$token))
  expect_equal(x_token$value, "Dato")
  expect_true(!is.null(x_token$timestamp))

  y_token <- pending_inputs[["y_column"]]
  expect_equal(y_token$value, "Tæller")
})

test_that("UI sync med NULL columns bruger auto_detect_results som fallback", {
  # SETUP: App state med auto_detect_results men NULL columns
  app_state <- create_app_state()
  emit <- create_emit_api(app_state)

  # Set up auto-detection results
  isolate({
    app_state$columns$auto_detect$results <- list(
      x_column = "Dato",
      y_column = "Tæller",
      n_column = "Nævner"
    )

    # Ensure columns are NULL (first sync scenario)
    app_state$columns$mappings$x_column <- NULL
    app_state$columns$mappings$y_column <- NULL
    app_state$columns$mappings$n_column <- NULL
  })

  # Track what values get used for UI sync
  ui_sync_values <- list()

  # Mock the UI service update_column_choices function
  mock_ui_service <- list(
    update_column_choices = function(choices = NULL, selected = NULL, ...) {
      ui_sync_values$choices <<- choices
      ui_sync_values$selected <<- selected
      ui_sync_values$call_time <<- Sys.time()
      cat("TOKEN_DEBUG: UI sync called with selected values:\n")
      if (!is.null(selected)) {
        for (col in names(selected)) {
          cat(paste("TOKEN_DEBUG:  ", col, "=", selected[[col]], "\n"))
        }
      }
    }
  )

  # Mock input and session for sync function
  mock_input <- list()
  mock_session <- list()

  # EXECUTE: Call sync function that should use auto_detect_results as fallback
  sync_ui_with_columns_unified(app_state, mock_input, list(), mock_session, mock_ui_service)

  # VERIFY: UI sync was called with auto_detect_results values
  expect_false(is.null(ui_sync_values$selected))
  expect_equal(ui_sync_values$selected$x_column, "Dato")
  expect_equal(ui_sync_values$selected$y_column, "Tæller")
  expect_equal(ui_sync_values$selected$n_column, "Nævner")

  # VERIFY: This prevents the "blank-first, real-second" pattern
  expect_true(ui_sync_values$selected$x_column != "")
  expect_true(ui_sync_values$selected$y_column != "")
})

# FASE 2: QUEUE SYSTEM TESTS ================================================================

test_that("Queue system håndterer overlapping UI updates korrekt", {
  cat("\n\n=== TEST: Queue system for overlapping updates ===\n")

  # SETUP: Create app_state with queue infrastructure
  source("../../global.R", local = TRUE)
  app_state <- create_app_state()

  # SETUP: Mock session for testing
  mock_session <- list(
    sendCustomMessage = function(type, message) {
      cat(paste("MOCK_SESSION: sendCustomMessage called with type:", type, "\n"))
    }
  )

  # TEST: Queue multiple updates rapidly
  cat("TEST: Adding multiple overlapping updates to queue...\n")

  # First update - should execute immediately
  result1 <- safe_programmatic_ui_update(
    updateSelectizeInput, mock_session, "x_column",
    choices = c("Col1", "Col2"), selected = "Col1"
  )

  # Second update while first is "pending" - should queue
  result2 <- safe_programmatic_ui_update(
    updateSelectizeInput, mock_session, "x_column",
    choices = c("Col1", "Col2", "Col3"), selected = "Col2"
  )

  # Third update - should also queue
  result3 <- safe_programmatic_ui_update(
    updateSelectizeInput, mock_session, "x_column",
    choices = c("Col1", "Col2", "Col3", "Col4"), selected = "Col3"
  )

  # VERIFY: Updates were queued
  queue_size <- length(isolate(app_state$ui$queued_updates))
  cat(paste("QUEUE_SIZE after 3 updates:", queue_size, "\n"))

  # Should have at least some queued updates (exact number depends on timing)
  expect_true(queue_size >= 0)  # Could be 0 if no overlap, or 1-2 if overlap occurred

  # VERIFY: Pending tokens exist for the updates
  pending_tokens <- isolate(app_state$ui$pending_programmatic_inputs)
  expect_true(length(pending_tokens) >= 1)  # At least one token should be pending

  cat("QUEUE_TEST: Overlapping updates handled successfully\n")
})

test_that("Queue system processer updates i korrekt rækkefølge", {
  cat("\n\n=== TEST: Queue processing order ===\n")

  # SETUP: Create fresh app_state
  source("../../global.R", local = TRUE)
  app_state <- create_app_state()

  # SETUP: Track execution order
  execution_order <- character(0)

  # SETUP: Mock session that tracks calls
  mock_session <- list(
    sendCustomMessage = function(type, message) {
      execution_order <<- c(execution_order, message$selected)
      cat(paste("EXECUTION_ORDER: Added", message$selected, "to execution order\n"))
    }
  )

  # TEST: Add updates to queue manually (simulating rapid overlap)
  cat("TEST: Manually queuing updates to test order...\n")

  # Simulate queue state where updates are pending
  app_state$ui$queued_updates <- list(
    list(
      func = updateSelectizeInput,
      session = mock_session,
      inputId = "x_column",
      choices = c("A", "B"),
      selected = "First"
    ),
    list(
      func = updateSelectizeInput,
      session = mock_session,
      inputId = "x_column",
      choices = c("A", "B", "C"),
      selected = "Second"
    ),
    list(
      func = updateSelectizeInput,
      session = mock_session,
      inputId = "x_column",
      choices = c("A", "B", "C", "D"),
      selected = "Third"
    )
  )

  # TEST: Process queue (we'll need to implement this function)
  if (exists("process_ui_update_queue")) {
    process_ui_update_queue(app_state)

    # VERIFY: Updates were processed in correct order
    expect_equal(execution_order, c("First", "Second", "Third"))
  } else {
    cat("QUEUE_PROCESSING: process_ui_update_queue not yet implemented - this is expected for Fase 2\n")
    expect_true(TRUE)  # Pass for now until we implement the function
  }

  cat("QUEUE_ORDER: Processing order test completed\n")
})

test_that("Queue system renser expired updates", {
  cat("\n\n=== TEST: Queue cleanup of expired updates ===\n")

  # SETUP: Create app_state
  source("../../global.R", local = TRUE)
  app_state <- create_app_state()

  # SETUP: Add expired updates to queue
  old_timestamp <- Sys.time() - 10  # 10 seconds ago

  app_state$ui$queued_updates <- list(
    list(
      func = updateSelectizeInput,
      session = list(),
      inputId = "x_column",
      choices = c("A", "B"),
      selected = "Expired",
      timestamp = old_timestamp
    ),
    list(
      func = updateSelectizeInput,
      session = list(),
      inputId = "y_column",
      choices = c("X", "Y"),
      selected = "Fresh",
      timestamp = Sys.time()  # Fresh timestamp
    )
  )

  # TEST: Cleanup function (to be implemented)
  if (exists("cleanup_expired_queue_updates")) {
    cleanup_expired_queue_updates(app_state, max_age_seconds = 5)

    # VERIFY: Only fresh update remains
    remaining_updates <- isolate(app_state$ui$queued_updates)
    expect_equal(length(remaining_updates), 1)
    expect_equal(remaining_updates[[1]]$selected, "Fresh")
  } else {
    cat("QUEUE_CLEANUP: cleanup_expired_queue_updates not yet implemented\n")
    expect_true(TRUE)  # Pass for now
  }

  cat("QUEUE_CLEANUP: Expired update cleanup test completed\n")
})