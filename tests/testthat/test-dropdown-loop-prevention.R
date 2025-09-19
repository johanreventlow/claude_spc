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
  expect_equal(isolate(app_state$columns$x_column), "Dato")

  # VERIFY: Events emitted when protection is off
  app_state$ui$updating_programmatically <- FALSE
  result <- mock_input_change("y_column", "Tæller")
  expect_equal(result, "emitted")
  expect_equal(isolate(app_state$events$column_choices_changed), 1L)
  expect_equal(isolate(app_state$columns$y_column), "Tæller")
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
  app_state$data$current_data <- test_data

  # SETUP: Simulate auto-detection results
  app_state$columns$auto_detect_results <- list(
    x_column = "Dato",
    y_column = "Tæller",
    n_column = "Nævner"
  )

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
  expect_equal(isolate(app_state$columns$x_column), "Dato")
  expect_equal(isolate(app_state$columns$y_column), "Tæller")
  expect_equal(isolate(app_state$columns$n_column), "Nævner")
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

  # Override updateSelectizeInput
  env <- environment()
  env$updateSelectizeInput <- mock_updateSelectizeInput

  # SETUP: Create UI service
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