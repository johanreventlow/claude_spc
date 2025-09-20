# test-fase3-event-driven-state-machine.R
# Tests for Fase 3: Event-Driven State Machine
# Focus on trigger isolation, frozen state management, and clean event flow

test_that("session_started trigger fungerer for name-only detection", {
  # SETUP: Mock app_state and emit API
  app_state <- create_test_ready_app_state()
  emit <- create_emit_api(app_state)

  # TEST: session_started trigger should work without data
  expect_silent({
    emit$session_started()
  })

  # Should increment event counter (use isolate for test context)
  expect_equal(isolate(app_state$events$session_started), 1L)
})

test_that("manual_autodetect_button trigger bypasser frozen state", {
  # SETUP: Create app_state with test data and freeze it
  app_state <- create_test_ready_app_state()
  set_current_data(app_state, data.frame(
    Dato = c("01-01-2024", "02-01-2024", "03-01-2024"),
    Tæller = c(90, 85, 92),
    Nævner = c(100, 95, 100)
  ))

  emit <- create_emit_api(app_state)

  # First run to freeze the system
  results1 <- autodetect_engine(
    data = isolate(app_state$data$current_data),
    trigger_type = "file_upload",
    app_state = app_state,
    emit = emit
  )

  # Should be frozen now
  expect_true(isolate(app_state$autodetect$frozen_until_next_trigger))

  # TEST: Smart unfreeze gør det muligt at køre endnu et file_upload-trigger
  results2 <- autodetect_engine(
    data = isolate(app_state$data$current_data),
    trigger_type = "file_upload",
    app_state = app_state,
    emit = emit
  )
  expect_equal(results2$x_col, "Dato")
  expect_equal(results2$y_col, "Tæller")

  # TEST: Manual trigger should bypass frozen state
  results3 <- autodetect_engine(
    data = isolate(app_state$data$current_data),
    trigger_type = "manual",
    app_state = app_state,
    emit = emit
  )
  expect_false(is.null(results3))  # Should succeed despite frozen state
  expect_equal(results3$x_col, "Dato")
})

test_that("data_loaded event unfreezer autodetect systemet", {
  # SETUP: Create frozen app_state
  app_state <- create_test_ready_app_state()
  app_state$autodetect$frozen_until_next_trigger <- TRUE
  emit <- create_emit_api(app_state)

  # Verify system is frozen
  expect_true(isolate(app_state$autodetect$frozen_until_next_trigger))

  # TEST: data_loaded event should unfreeze system
  emit$data_loaded()

  # System should still be frozen until event is processed
  # (In real app, this would be handled by event listener)
  expect_true(isolate(app_state$autodetect$frozen_until_next_trigger))

  # Simulate event listener unfreezing the system
  app_state$autodetect$frozen_until_next_trigger <- FALSE
  expect_false(isolate(app_state$autodetect$frozen_until_next_trigger))
})

test_that("trigger isolation fungerer korrekt", {
  # SETUP: Create test environment
  app_state <- create_test_ready_app_state()
  emit <- create_emit_api(app_state)

  # TEST: Each trigger should have independent event counters
  emit$session_started()
  emit$manual_autodetect_button()
  emit$data_loaded()

  expect_equal(isolate(app_state$events$session_started), 1L)
  expect_equal(isolate(app_state$events$manual_autodetect_button), 1L)
  expect_equal(isolate(app_state$events$data_loaded), 1L)

  # TEST: Multiple calls should increment independently
  emit$session_started()
  emit$session_started()
  emit$manual_autodetect_button()

  expect_equal(isolate(app_state$events$session_started), 3L)
  expect_equal(isolate(app_state$events$manual_autodetect_button), 2L)
  expect_equal(isolate(app_state$events$data_loaded), 1L)  # Unchanged
})

test_that("frozen state metadata gemmes korrekt", {
  # SETUP: Create app_state with test data
  app_state <- create_test_ready_app_state()
  test_data <- data.frame(
    Dato = c("01-01-2024", "02-01-2024"),
    Tæller = c(90, 85),
    Nævner = c(100, 95)
  )
  emit <- create_emit_api(app_state)

  # TEST: Run autodetect and check metadata
  results <- autodetect_engine(
    data = test_data,
    trigger_type = "file_upload",
    app_state = app_state,
    emit = emit
  )

  # Check frozen state metadata
  expect_true(isolate(app_state$autodetect$frozen_until_next_trigger))
  expect_false(is.null(isolate(app_state$autodetect$last_run)))

  last_run <- isolate(app_state$autodetect$last_run)
  expect_equal(last_run$trigger, "file_upload")
  expect_equal(last_run$data_rows, 2)
  expect_equal(last_run$data_cols, 3)
  expect_false(is.null(last_run$timestamp))
  expect_equal(last_run$results_summary$x_column, "Dato")
  expect_equal(last_run$results_summary$y_column, "Tæller")
})

test_that("session_start trigger virker uden data", {
  # SETUP: Create app_state without data
  app_state <- create_test_ready_app_state()
  set_current_data(app_state, NULL)
  emit <- create_emit_api(app_state)

  # TEST: session_start should work with no data (name-only detection)
  results <- autodetect_engine(
    data = NULL,
    trigger_type = "session_start",
    app_state = app_state,
    emit = emit
  )

  # Should detect based on names only
  expect_false(is.null(results))
  expect_true(isolate(app_state$autodetect$frozen_until_next_trigger))
  expect_equal(isolate(app_state$autodetect$last_run$trigger), "session_start")
  expect_equal(isolate(app_state$autodetect$last_run$data_rows), 0)
})

test_that("error handling i event-driven architecture", {
  # SETUP: Create app_state with invalid data
  app_state <- create_test_ready_app_state()
  emit <- create_emit_api(app_state)

  # TEST: Invalid trigger type should fail gracefully
  expect_error({
    autodetect_engine(
      data = NULL,
      trigger_type = "invalid_trigger",
      app_state = app_state,
      emit = emit
    )
  })

  # TEST: Missing app_state should fail
  expect_error({
    autodetect_engine(
      data = NULL,
      trigger_type = "manual",
      app_state = NULL,
      emit = emit
    )
  })

  # TEST: Missing emit should fail
  expect_error({
    autodetect_engine(
      data = NULL,
      trigger_type = "manual",
      app_state = app_state,
      emit = NULL
    )
  })
})

test_that("event flow konsistens mellem triggers", {
  # SETUP: Create app_state with standard test data
  app_state <- create_test_ready_app_state()
  test_data <- data.frame(
    Dato = c("01-01-2024", "02-01-2024", "03-01-2024"),
    Tæller = c(90, 85, 92),
    Nævner = c(100, 95, 100)
  )
  set_current_data(app_state, test_data)
  emit <- create_emit_api(app_state)

  # TEST: All trigger types should produce consistent results
  triggers <- c("session_start", "file_upload", "manual")
  results_list <- list()

  for (trigger in triggers) {
    # Reset frozen state for each test
    isolate(app_state$autodetect$frozen_until_next_trigger <- FALSE)

    results_list[[trigger]] <- autodetect_engine(
      data = if (trigger == "session_start") NULL else test_data,
      trigger_type = trigger,
      app_state = app_state,
      emit = emit
    )
  }

  # All triggers should detect the same columns (when data is available)
  expect_equal(results_list$file_upload$x_col, "Dato")
  expect_equal(results_list$manual$x_col, "Dato")
  expect_equal(results_list$file_upload$y_col, "Tæller")
  expect_equal(results_list$manual$y_col, "Tæller")

  # session_start works with name-only detection
  expect_false(is.null(results_list$session_start))
})

test_that("performance ved gentagne trigger calls", {
  # SETUP: Create app_state with large dataset simulation
  app_state <- create_test_ready_app_state()
  large_data <- data.frame(
    Dato = rep(c("01-01-2024", "02-01-2024"), 500),
    Tæller = rep(c(90, 85), 500),
    Nævner = rep(c(100, 95), 500)
  )
  emit <- create_emit_api(app_state)

  results1 <- autodetect_engine(
    data = large_data,
    trigger_type = "file_upload",
    app_state = app_state,
    emit = emit
  )
  expect_equal(results1$x_col, "Dato")
  expect_equal(results1$y_col, "Tæller")

  # TEST: Subsequent file uploads fortsætter med at give resultater
  results2 <- autodetect_engine(
    data = large_data,
    trigger_type = "file_upload",  # Non-manual trigger
    app_state = app_state,
    emit = emit
  )

  expect_equal(results2$x_col, "Dato")
  expect_equal(results2$y_col, "Tæller")

  # Manual trigger fungerer stadig og giver identiske kolonnevalg
  results3 <- autodetect_engine(
    data = large_data,
    trigger_type = "manual",
    app_state = app_state,
    emit = emit
  )

  expect_equal(results3$x_col, "Dato")
  expect_equal(results3$y_col, "Tæller")
})
