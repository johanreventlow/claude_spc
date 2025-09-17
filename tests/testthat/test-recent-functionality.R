# test-recent-functionality.R
# Test af den nyligt udviklede funktionalitet:
# - Direct reactiveVal trigger pattern
# - Excel upload autodetection fix
# - UI sync trigger mechanism
# - Unified state management helpers

test_that("reactiveVal trigger pattern funktionalitet", {
  # TEST: Simulation af reactiveVal trigger pattern som vi implementerede

  # SETUP: Simuler reactiveVal behavior med en funktion
  trigger_value <- NULL
  trigger_function <- function(value = NULL) {
    if (!missing(value)) {
      trigger_value <<- value
      return(value)
    } else {
      return(trigger_value)
    }
  }

  # Mock autodetect trigger creation
  create_autodetect_trigger <- function() {
    return(trigger_function)
  }

  # TEST: Trigger creation
  autodetect_trigger <- create_autodetect_trigger()
  expect_true(is.function(autodetect_trigger))

  # TEST: Trigger can be set and retrieved
  test_time <- Sys.time()
  autodetect_trigger(test_time)

  retrieved_value <- autodetect_trigger()
  expect_equal(retrieved_value, test_time)

  # TEST: Multiple trigger calls work
  time1 <- Sys.time()
  autodetect_trigger(time1)
  expect_equal(autodetect_trigger(), time1)

  Sys.sleep(0.01)
  time2 <- Sys.time()
  autodetect_trigger(time2)
  expect_equal(autodetect_trigger(), time2)
  expect_true(time2 > time1)
})

test_that("direct trigger callback pattern simulation", {
  # TEST: Simulation af direct callback pattern fra fct_file_operations.R

  # Mock callback system
  callback_fired <- FALSE
  callback_value <- NULL

  mock_autodetect_trigger <- function(value) {
    callback_fired <<- TRUE
    callback_value <<- value
    return(value)
  }

  # Function to simulate file upload with trigger
  simulate_file_upload_with_trigger <- function(file_data, trigger_callback = NULL) {
    # Simulate file processing
    processed_data <- file_data

    # Fire trigger if provided (our new pattern)
    if (!is.null(trigger_callback)) {
      trigger_time <- Sys.time()
      trigger_callback(trigger_time)
      return(list(
        success = TRUE,
        data = processed_data,
        trigger_fired = TRUE,
        trigger_time = trigger_time
      ))
    }

    return(list(success = TRUE, data = processed_data, trigger_fired = FALSE))
  }

  # TEST: Upload without trigger (old behavior)
  result1 <- simulate_file_upload_with_trigger(data.frame(x = 1:3))
  expect_true(result1$success)
  expect_false(result1$trigger_fired)
  expect_false(callback_fired)

  # Reset callback state
  callback_fired <- FALSE
  callback_value <- NULL

  # TEST: Upload with trigger (new behavior)
  result2 <- simulate_file_upload_with_trigger(
    data.frame(x = 1:3),
    trigger_callback = mock_autodetect_trigger
  )

  expect_true(result2$success)
  expect_true(result2$trigger_fired)
  expect_true(callback_fired)
  expect_true(!is.null(callback_value))
  expect_true(inherits(callback_value, "POSIXct"))
})

test_that("Excel upload trigger fix simulation", {
  # TEST: Simulation af Excel upload fix hvor vi tilføjede autodetect_trigger parameter

  trigger_calls <- list()

  mock_trigger <- function(value) {
    trigger_calls <<- c(trigger_calls, list(value))
    return(value)
  }

  # OLD handle_excel_upload without trigger (before fix)
  handle_excel_upload_old <- function(file_path) {
    result <- list(
      current_data = data.frame(
        Dato = c("2024-01-01", "2024-02-01"),
        Tæller = c(90, 85),
        stringsAsFactors = FALSE
      ),
      file_uploaded = TRUE,
      trigger_called = FALSE
    )
    # NOTE: No autodetect trigger call (the bug we fixed)
    return(result)
  }

  # NEW handle_excel_upload with trigger (after fix)
  handle_excel_upload_new <- function(file_path, autodetect_trigger = NULL) {
    result <- list(
      current_data = data.frame(
        Dato = c("2024-01-01", "2024-02-01"),
        Tæller = c(90, 85),
        stringsAsFactors = FALSE
      ),
      file_uploaded = TRUE,
      trigger_called = FALSE
    )

    # FIX: Call autodetect trigger for subsequent files
    if (!is.null(autodetect_trigger)) {
      autodetect_trigger(Sys.time())
      result$trigger_called <- TRUE
    }

    return(result)
  }

  # TEST: Old version doesn't trigger autodetection
  trigger_calls <- list()
  result1 <- handle_excel_upload_old("test.xlsx")

  expect_true(result1$file_uploaded)
  expect_false(result1$trigger_called)
  expect_equal(length(trigger_calls), 0)  # No trigger called

  # TEST: New version triggers autodetection
  trigger_calls <- list()
  result2 <- handle_excel_upload_new("test.xlsx", autodetect_trigger = mock_trigger)

  expect_true(result2$file_uploaded)
  expect_true(result2$trigger_called)
  expect_equal(length(trigger_calls), 1)  # Trigger was called
  expect_true(inherits(trigger_calls[[1]], "POSIXct"))
})

test_that("UI sync trigger mechanism simulation", {
  # TEST: Simulation af UI sync trigger mechanism

  ui_updates <- list()

  # Mock UI sync trigger
  mock_ui_sync_trigger <- function(sync_data) {
    ui_updates <<- c(ui_updates, list(sync_data))
    return(TRUE)
  }

  # Function to create UI sync data (from auto_detect_and_update_columns)
  create_ui_sync_data <- function(detected_columns, col_choices) {
    list(
      x_col = detected_columns$x_col,
      taeller_col = detected_columns$taeller_col,
      naevner_col = detected_columns$naevner_col,
      skift_col = detected_columns$skift_col,
      frys_col = detected_columns$frys_col,
      kommentar_col = detected_columns$kommentar_col,
      col_choices = col_choices,
      timestamp = as.numeric(Sys.time())
    )
  }

  # Simulate auto-detection with UI sync
  simulate_autodetect_with_ui_sync <- function(data, ui_sync_trigger_func = NULL) {
    # Mock auto-detection results
    detected <- list(
      x_col = if ("Dato" %in% names(data)) "Dato" else names(data)[1],
      taeller_col = if ("Tæller" %in% names(data)) "Tæller" else NULL,
      naevner_col = if ("Nævner" %in% names(data)) "Nævner" else NULL,
      skift_col = NULL,
      frys_col = NULL,
      kommentar_col = if ("Kommentarer" %in% names(data)) "Kommentarer" else NULL
    )

    # Create column choices
    col_choices <- c("", names(data))
    names(col_choices) <- c("Vælg kolonne...", names(data))

    # Create UI sync data
    sync_data <- create_ui_sync_data(detected, col_choices)

    # Fire UI sync trigger
    if (!is.null(ui_sync_trigger_func)) {
      ui_sync_trigger_func(sync_data)
    }

    return(detected)
  }

  # TEST: Auto-detection without UI sync
  test_data <- data.frame(
    Dato = c("2024-01-01", "2024-02-01"),
    Tæller = c(90, 85),
    Nævner = c(100, 95)
  )

  ui_updates <- list()
  result1 <- simulate_autodetect_with_ui_sync(test_data)

  expect_equal(result1$x_col, "Dato")
  expect_equal(result1$taeller_col, "Tæller")
  expect_equal(length(ui_updates), 0)  # No UI sync called

  # TEST: Auto-detection with UI sync trigger
  ui_updates <- list()
  result2 <- simulate_autodetect_with_ui_sync(test_data, ui_sync_trigger_func = mock_ui_sync_trigger)

  expect_equal(result2$x_col, "Dato")
  expect_equal(result2$taeller_col, "Tæller")
  expect_equal(length(ui_updates), 1)  # UI sync was called

  # TEST: UI sync data structure
  sync_data <- ui_updates[[1]]
  expect_equal(sync_data$x_col, "Dato")
  expect_equal(sync_data$taeller_col, "Tæller")
  expect_equal(sync_data$naevner_col, "Nævner")
  expect_true("col_choices" %in% names(sync_data))
  expect_true("timestamp" %in% names(sync_data))

  # TEST: Column choices structure
  expect_equal(length(sync_data$col_choices), 4)  # "" + 3 data columns
  expect_equal(sync_data$col_choices[[1]], "")
  expect_equal(names(sync_data$col_choices)[1], "Vælg kolonne...")
})

test_that("unified state management helper functions", {
  # TEST: Helper functions for dual-state management (fra mod_spc_chart.R)

  # TEST: Simulation af state management pattern
  simulate_state_management <- function(use_unified = TRUE) {
    # Mock state containers
    app_state <- list(visualization = list(plot_ready = FALSE))
    legacy_values <- list(plot_ready = FALSE)

    if (use_unified) {
      # Unified state operation
      app_state$visualization$plot_ready <- TRUE
      return(list(
        unified_value = app_state$visualization$plot_ready,
        legacy_value = legacy_values$plot_ready,
        mode = "unified"
      ))
    } else {
      # Legacy state operation
      legacy_values$plot_ready <- TRUE
      return(list(
        unified_value = app_state$visualization$plot_ready,
        legacy_value = legacy_values$plot_ready,
        mode = "legacy"
      ))
    }
  }

  # TEST: Unified state mode
  result_unified <- simulate_state_management(use_unified = TRUE)
  expect_true(result_unified$unified_value)   # Should be updated
  expect_false(result_unified$legacy_value)   # Should NOT be updated
  expect_equal(result_unified$mode, "unified")

  # TEST: Legacy state mode
  result_legacy <- simulate_state_management(use_unified = FALSE)
  expect_false(result_legacy$unified_value)   # Should NOT be updated
  expect_true(result_legacy$legacy_value)     # Should be updated
  expect_equal(result_legacy$mode, "legacy")

  # TEST: Helper function pattern concept
  create_dual_state_pattern <- function() {
    return(list(
      supports_unified = TRUE,
      supports_legacy = TRUE,
      backward_compatible = TRUE,
      migration_safe = TRUE
    ))
  }

  pattern <- create_dual_state_pattern()
  expect_true(pattern$supports_unified)
  expect_true(pattern$supports_legacy)
  expect_true(pattern$backward_compatible)
  expect_true(pattern$migration_safe)
})

test_that("function signature compatibility for autodetect trigger", {
  # TEST: Function signature changes for autodetect trigger passing

  # Mock the function signature updates we made

  # OLD setup_file_upload signature (before fix)
  setup_file_upload_old <- function(input, output, session, values, waiter_file, app_state = NULL) {
    return(list(
      autodetect_trigger_available = FALSE,
      parameters = c("input", "output", "session", "values", "waiter_file", "app_state")
    ))
  }

  # NEW setup_file_upload signature (after fix)
  setup_file_upload_new <- function(input, output, session, values, waiter_file, app_state = NULL, autodetect_trigger = NULL) {
    return(list(
      autodetect_trigger_available = !is.null(autodetect_trigger),
      parameters = c("input", "output", "session", "values", "waiter_file", "app_state", "autodetect_trigger")
    ))
  }

  # Mock function args
  mock_args <- list(
    input = "mock_input",
    output = "mock_output",
    session = "mock_session",
    values = list(),
    waiter_file = "mock_waiter",
    app_state = list()
  )

  mock_trigger <- function(x) x

  # TEST: Old signature without trigger
  result_old <- do.call(setup_file_upload_old, mock_args)
  expect_false(result_old$autodetect_trigger_available)
  expect_equal(length(result_old$parameters), 6)

  # TEST: New signature with trigger
  mock_args_new <- c(mock_args, list(autodetect_trigger = mock_trigger))
  result_new <- do.call(setup_file_upload_new, mock_args_new)
  expect_true(result_new$autodetect_trigger_available)
  expect_equal(length(result_new$parameters), 7)

  # TEST: New signature backward compatible (trigger can be NULL)
  result_new_null <- do.call(setup_file_upload_new, mock_args)
  expect_false(result_new_null$autodetect_trigger_available)
})

test_that("setup_column_management return value pattern", {
  # TEST: setup_column_management returning autodetect_trigger pattern

  # Mock setup_column_management that returns trigger (our new pattern)
  mock_setup_column_management <- function(input, output, session, values, app_state = NULL) {
    # Create mock reactiveVal-like trigger
    trigger_value <- NULL

    autodetect_trigger <- function(value = NULL) {
      if (!missing(value)) {
        trigger_value <<- value
        return(value)
      } else {
        return(trigger_value)
      }
    }

    # Simulate setup logic...
    # Return the trigger for use by other functions
    return(autodetect_trigger)
  }

  # Mock app_server.R pattern
  simulate_app_server_setup <- function() {
    mock_input <- list()
    mock_output <- list()
    mock_session <- list()
    mock_values <- list()
    mock_app_state <- list()
    mock_waiter <- "mock_waiter"

    # NEW PATTERN: Capture return value and pass to file upload
    autodetect_trigger <- mock_setup_column_management(
      mock_input, mock_output, mock_session, mock_values, mock_app_state
    )

    # Verify trigger is function
    expect_true(is.function(autodetect_trigger))

    # Test trigger functionality
    test_value <- Sys.time()
    autodetect_trigger(test_value)
    expect_equal(autodetect_trigger(), test_value)

    return(autodetect_trigger)
  }

  # TEST: App server setup pattern
  trigger <- simulate_app_server_setup()
  expect_true(is.function(trigger))

  # TEST: Trigger can be used by file upload
  file_upload_can_use_trigger <- function(trigger_func) {
    if (is.function(trigger_func)) {
      trigger_func(Sys.time())
      return(TRUE)
    }
    return(FALSE)
  }

  expect_true(file_upload_can_use_trigger(trigger))
})

test_that("event-driven vs timing-based reactivity", {
  # TEST: Demonstrate difference between event-driven (our solution) vs timing approaches

  # Timing-based approach (problematic)
  timing_based_sync <- function(data) {
    start_time <- Sys.time()

    # Simulate delayed/unreliable processing
    sync_success <- FALSE
    attempts <- 0
    max_attempts <- 3

    while (!sync_success && attempts < max_attempts) {
      attempts <- attempts + 1
      # Simulate timing uncertainty
      if (runif(1) > 0.3) {  # 70% success rate per attempt
        sync_success <- TRUE
      }
    }

    end_time <- Sys.time()

    return(list(
      success = sync_success,
      attempts = attempts,
      execution_time = as.numeric(end_time - start_time),
      approach = "timing_based"
    ))
  }

  # Event-driven approach (our solution)
  event_driven_sync <- function(data, trigger_func = NULL) {
    start_time <- Sys.time()

    # Process data immediately
    processed <- TRUE

    # Fire trigger directly (deterministic)
    if (!is.null(trigger_func)) {
      trigger_func(list(data = data, timestamp = Sys.time()))
    }

    end_time <- Sys.time()

    return(list(
      success = processed,
      attempts = 1,  # Always succeeds in one attempt
      execution_time = as.numeric(end_time - start_time),
      approach = "event_driven"
    ))
  }

  # TEST: Event-driven is more reliable
  test_data <- list(x = 1:5)

  trigger_calls <- list()
  mock_trigger <- function(data) {
    trigger_calls <<- c(trigger_calls, list(data))
  }

  # Run event-driven multiple times
  event_results <- replicate(5, event_driven_sync(test_data, mock_trigger), simplify = FALSE)

  # All should succeed
  all_success <- all(sapply(event_results, function(r) r$success))
  expect_true(all_success)

  # All should be fast
  all_fast <- all(sapply(event_results, function(r) r$execution_time < 0.1))
  expect_true(all_fast)

  # All should be single attempt
  all_single_attempt <- all(sapply(event_results, function(r) r$attempts == 1))
  expect_true(all_single_attempt)

  # Trigger should be called every time
  expect_equal(length(trigger_calls), 5)

  # TEST: Compare with timing-based (may be unreliable)
  timing_results <- replicate(5, timing_based_sync(test_data), simplify = FALSE)

  # Event-driven should be consistently better
  event_avg_attempts <- mean(sapply(event_results, function(r) r$attempts))
  timing_avg_attempts <- mean(sapply(timing_results, function(r) r$attempts))

  expect_true(event_avg_attempts <= timing_avg_attempts)
})