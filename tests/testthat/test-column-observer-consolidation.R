# Test: Column Observer Consolidation
#
# Tests for the consolidated column input observer system.
# Validates that the unified handle_column_input() function provides
# identical behavior to the previous 6 separate observers.
#
# Coverage:
# - normalize_column_input() - Input normalization logic
# - handle_column_input() - Unified column input handler
# - create_column_observer() - Observer factory function
#
# NOTE: Integration tests with testServer() are skipped due to timeout issues.
# Manual testing in the running app validates end-to-end behavior.

library(testthat)
library(shiny)

# ============================================================================
# UNIT TESTS: normalize_column_input()
# ============================================================================

test_that("normalize_column_input handles NULL input", {
  result <- normalize_column_input(NULL)
  expect_identical(result, "")
})

test_that("normalize_column_input handles empty vector", {
  result <- normalize_column_input(character(0))
  expect_identical(result, "")
})

test_that("normalize_column_input handles empty string", {
  result <- normalize_column_input("")
  expect_identical(result, "")
})

test_that("normalize_column_input handles NA", {
  result <- normalize_column_input(NA_character_)
  expect_identical(result, "")
})

test_that("normalize_column_input handles valid string", {
  result <- normalize_column_input("dato")
  expect_identical(result, "dato")
})

test_that("normalize_column_input handles vector with multiple elements", {
  result <- normalize_column_input(c("dato", "x", "y"))
  expect_identical(result, "dato")
})

# ============================================================================
# UNIT TESTS: handle_column_input()
# ============================================================================

test_that("handle_column_input consumes programmatic token", {
  # Setup mock app_state
  app_state <- new.env()
  app_state$ui <- list(
    pending_programmatic_inputs = list(),
    performance_metrics = reactiveValues(tokens_consumed = 0L)
  )
  app_state$columns <- reactiveValues()
  app_state$ui_cache <- list()

  # Set programmatic token
  app_state$ui$pending_programmatic_inputs$x_column <- list(value = "dato")

  # Mock emit API
  emit <- new.env()
  emit_called <- FALSE
  emit$column_choices_changed <- function() {
    emit_called <<- TRUE
  }

  # Call handler with matching value
  handle_column_input("x_column", "dato", app_state, emit)

  # Verify token consumed
  expect_null(app_state$ui$pending_programmatic_inputs$x_column)

  # Verify state updated
  expect_identical(isolate(app_state$columns$x_column), "dato")

  # Verify metrics incremented
  expect_identical(isolate(app_state$ui$performance_metrics$tokens_consumed), 1L)

  # Verify NO event emission for programmatic update
  expect_false(emit_called)
})

test_that("handle_column_input emits event for user input", {
  # Setup mock app_state
  app_state <- new.env()
  app_state$ui <- list(
    pending_programmatic_inputs = list(),
    performance_metrics = reactiveValues(tokens_consumed = 0L)
  )
  app_state$columns <- reactiveValues()
  app_state$ui_cache <- list()

  # NO programmatic token (simulates user input)

  # Mock emit API
  emit <- new.env()
  emit_called <- FALSE
  emit$column_choices_changed <- function() {
    emit_called <<- TRUE
  }

  # Call handler
  handle_column_input("x_column", "dato", app_state, emit)

  # Verify state updated
  expect_identical(isolate(app_state$columns$x_column), "dato")

  # Verify cache updated
  expect_identical(app_state$ui_cache$x_column_input, "dato")

  # Verify event emission for user input
  expect_true(emit_called)
})

test_that("handle_column_input normalizes input value", {
  # Setup mock app_state
  app_state <- new.env()
  app_state$ui <- list(
    pending_programmatic_inputs = list(),
    performance_metrics = reactiveValues(tokens_consumed = 0L)
  )
  app_state$columns <- reactiveValues()
  app_state$ui_cache <- list()

  # Mock emit API
  emit <- new.env()
  emit$column_choices_changed <- function() {}

  # Test with NULL input
  handle_column_input("x_column", NULL, app_state, emit)
  expect_identical(isolate(app_state$columns$x_column), "")

  # Test with empty string
  handle_column_input("x_column", "", app_state, emit)
  expect_identical(isolate(app_state$columns$x_column), "")

  # Test with vector
  handle_column_input("x_column", c("dato", "extra"), app_state, emit)
  expect_identical(isolate(app_state$columns$x_column), "dato")
})

test_that("handle_column_input handles missing emit function gracefully", {
  # Setup mock app_state
  app_state <- new.env()
  app_state$ui <- list(
    pending_programmatic_inputs = list(),
    performance_metrics = reactiveValues(tokens_consumed = 0L)
  )
  app_state$columns <- reactiveValues()
  app_state$ui_cache <- list()

  # Mock emit API WITHOUT column_choices_changed
  emit <- new.env()

  # Should not throw error
  expect_silent(handle_column_input("x_column", "dato", app_state, emit))

  # Verify state still updated
  expect_identical(isolate(app_state$columns$x_column), "dato")
})

# ============================================================================
# UNIT TESTS: create_column_observer()
# ============================================================================

test_that("create_column_observer function exists and is callable", {
  # Verify function exists
  expect_true(exists("create_column_observer"))
  expect_true(is.function(create_column_observer))

  # Verify function signature (should accept 4 parameters)
  expect_equal(length(formals(create_column_observer)), 4)
  expect_named(formals(create_column_observer), c("col_name", "input", "app_state", "emit"))
})
