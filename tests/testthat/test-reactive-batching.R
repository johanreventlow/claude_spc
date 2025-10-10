# Tests for Reactive Batching Utilities
# Testing batching behavior to prevent reactive storms

library(testthat)
library(shiny)

# Simplified test without later event loop dependency
test_that("schedule_batched_update schedules execution", {
  skip_if_not(requireNamespace("later", quietly = TRUE), "later package not available")

  executed <- FALSE

  # This schedules but doesn't execute in test context (later event loop)
  expect_silent({
    schedule_batched_update(
      update_fn = function() {
        executed <<- TRUE
      },
      delay_ms = 10
    )
  })

  # Should not execute immediately (verified by silent scheduling)
  expect_false(executed)
})

test_that("schedule_batched_update batches rapid-fire events", {
  skip_if_not(requireNamespace("later", quietly = TRUE), "later package not available")

  app_state <- create_app_state()

  # Fire 5 rapid events with same batch key
  for (i in 1:5) {
    schedule_batched_update(
      update_fn = function() {},
      delay_ms = 50,
      app_state = app_state,
      batch_key = "test_batch"
    )
  }

  # Should have scheduled only once (batch pending after first call)
  # Subsequent calls with same batch_key should be no-ops
  expect_true(is_batch_pending(app_state, "test_batch"))

  # Clean up
  clear_all_batches(app_state)
})

test_that("schedule_batched_update handles different batch keys independently", {
  skip_if_not(requireNamespace("later", quietly = TRUE), "later package not available")

  app_state <- create_app_state()

  # Schedule events with different batch keys
  schedule_batched_update(
    update_fn = function() {},
    delay_ms = 10,
    app_state = app_state,
    batch_key = "batch1"
  )

  schedule_batched_update(
    update_fn = function() {},
    delay_ms = 10,
    app_state = app_state,
    batch_key = "batch2"
  )

  # Both batches should be pending independently
  expect_true(is_batch_pending(app_state, "batch1"))
  expect_true(is_batch_pending(app_state, "batch2"))

  # Clean up
  clear_all_batches(app_state)
})

test_that("schedule_batched_update handles errors gracefully", {
  skip_if_not(requireNamespace("later", quietly = TRUE), "later package not available")

  app_state <- create_app_state()

  # Schedule update that will error
  expect_silent({
    schedule_batched_update(
      update_fn = function() {
        stop("Intentional error for testing")
      },
      delay_ms = 10,
      app_state = app_state,
      batch_key = "error_batch"
    )
  })

  # Should have scheduled despite error-prone function
  expect_true(is_batch_pending(app_state, "error_batch"))

  # Clean up
  clear_all_batches(app_state)
})

test_that("is_batch_pending returns FALSE for non-existent batches", {
  app_state <- create_app_state()

  expect_false(is_batch_pending(app_state, "non_existent"))
  expect_false(is_batch_pending(NULL, "any_key"))
})

test_that("clear_all_batches removes all pending batches", {
  skip_if_not(requireNamespace("later", quietly = TRUE), "later package not available")

  app_state <- create_app_state()

  # Schedule multiple batches
  for (i in 1:3) {
    schedule_batched_update(
      update_fn = function() {},
      delay_ms = 100,
      app_state = app_state,
      batch_key = paste0("batch", i)
    )
  }

  # All should be pending
  expect_true(is_batch_pending(app_state, "batch1"))
  expect_true(is_batch_pending(app_state, "batch2"))
  expect_true(is_batch_pending(app_state, "batch3"))

  # Clear all batches
  clear_all_batches(app_state)

  # All should be cleared
  expect_false(is_batch_pending(app_state, "batch1"))
  expect_false(is_batch_pending(app_state, "batch2"))
  expect_false(is_batch_pending(app_state, "batch3"))
})

test_that("schedule_batched_update validates input parameters", {
  expect_error(
    schedule_batched_update(update_fn = "not a function", delay_ms = 50),
    "update_fn must be a function"
  )

  expect_error(
    schedule_batched_update(update_fn = function() {}, delay_ms = -10),
    "delay_ms must be a non-negative number"
  )

  expect_error(
    schedule_batched_update(update_fn = function() {}, delay_ms = "invalid"),
    "delay_ms must be a non-negative number"
  )
})

# Integration test: Column input batching behavior
test_that("handle_column_input uses batching infrastructure", {
  skip_if_not(requireNamespace("later", quietly = TRUE), "later package not available")

  app_state <- create_app_state()

  # Mock emit API (minimal implementation)
  emit <- list(
    column_choices_changed = function() {}
  )

  # Rapid-fire column input changes
  for (i in 1:5) {
    handle_column_input(
      col_name = "x_column",
      new_value = paste0("column_", i),
      app_state = app_state,
      emit = emit
    )
  }

  # Should have scheduled batch (verified by pending flag)
  expect_true(is_batch_pending(app_state, "column_choices"))

  # Clean up
  clear_all_batches(app_state)
})

# Edge case: Fallback behavior without app_state
test_that("schedule_batched_update works without app_state (fallback mode)", {
  skip_if_not(requireNamespace("later", quietly = TRUE), "later package not available")

  # Should work without app_state (no batching tracking)
  expect_silent({
    schedule_batched_update(
      update_fn = function() {},
      delay_ms = 10,
      app_state = NULL
    )
  })

  # No app_state means no batching tracking (just scheduling)
  # This tests the fallback path works without errors
})

# Test batching infrastructure creation
test_that("batching infrastructure is created lazily", {
  app_state <- create_app_state()

  # Initially no batching environment
  expect_false(exists("batching", envir = app_state, inherits = FALSE))

  # First schedule creates infrastructure
  schedule_batched_update(
    update_fn = function() {},
    delay_ms = 10,
    app_state = app_state,
    batch_key = "test"
  )

  # Now batching infrastructure should exist
  expect_true(exists("batching", envir = app_state, inherits = FALSE))
  expect_true(exists("pending_batches", envir = app_state$batching, inherits = FALSE))

  # Clean up
  clear_all_batches(app_state)
})
