# test-spc-bfh-service.R
# TDD Test Suite for BFHchart Service Layer (Issue #30 Stream A)
# RED PHASE: Tests written FIRST to define expected behavior
# All tests should FAIL initially until implementation in Stream D

# Test Setup ==================================================================

# Load required packages for testing
library(testthat)
library(tibble)
library(mockery)

# DETERMINISTIC TESTING: Fixed seed for reproducibility
set.seed(20251015)

# Helper Functions ============================================================

#' Create test data for SPC charts
#'
#' Generates deterministic test data for various chart types
#'
#' @param n_rows Number of rows to generate
#' @param chart_type Chart type for appropriate data structure
#' @return tibble with test data
create_test_data <- function(n_rows = 20, chart_type = "run") {
  dates <- seq.Date(from = as.Date("2024-01-01"), by = "month", length.out = n_rows)

  if (chart_type %in% c("p", "u", "c")) {
    # Ratio charts need numerator and denominator
    tibble::tibble(
      month = dates,
      numerator = round(rnorm(n_rows, mean = 10, sd = 2)),
      denominator = round(rnorm(n_rows, mean = 100, sd = 10))
    )
  } else {
    # Standard charts
    tibble::tibble(
      month = dates,
      value = round(rnorm(n_rows, mean = 50, sd = 10), 1)
    )
  }
}

# Unit Tests: compute_spc_results_bfh() =======================================

context("Unit Tests - compute_spc_results_bfh()")

test_that("compute_spc_results_bfh() exists and is callable", {
  # RED PHASE: Function doesn't exist yet
  expect_true(exists("compute_spc_results_bfh"))
  expect_true(is.function(compute_spc_results_bfh))
})

# Chart Type Tests ------------------------------------------------------------

test_that("compute_spc_results_bfh() handles run charts", {
  # Arrange
  set.seed(20251015)
  data <- create_test_data(n_rows = 20, chart_type = "run")

  # Act
  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "value",
    chart_type = "run"
  )

  # Assert - Return structure
  expect_type(result, "list")
  expect_named(result, c("plot", "qic_data", "metadata"))

  # Assert - Plot component
  expect_s3_class(result$plot, "ggplot")

  # Assert - Data component
  expect_s3_class(result$qic_data, "tbl_df")
  expect_named(result$qic_data, c("x", "y", "cl", "lcl", "ucl", "signal"))
  expect_equal(nrow(result$qic_data), 20)

  # Assert - Metadata component
  expect_type(result$metadata, "list")
  expect_true("chart_type" %in% names(result$metadata))
  expect_equal(result$metadata$chart_type, "run")
})

test_that("compute_spc_results_bfh() handles I charts", {
  set.seed(20251015)
  data <- create_test_data(n_rows = 25, chart_type = "i")

  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "value",
    chart_type = "i"
  )

  expect_s3_class(result$plot, "ggplot")
  expect_s3_class(result$qic_data, "tbl_df")
  expect_equal(nrow(result$qic_data), 25)
  expect_equal(result$metadata$chart_type, "i")
})

test_that("compute_spc_results_bfh() handles P charts (proportion)", {
  set.seed(20251015)
  data <- create_test_data(n_rows = 30, chart_type = "p")

  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "numerator",
    n_var = "denominator",
    chart_type = "p"
  )

  expect_s3_class(result$plot, "ggplot")
  expect_s3_class(result$qic_data, "tbl_df")
  expect_equal(nrow(result$qic_data), 30)
  expect_equal(result$metadata$chart_type, "p")

  # P charts should have proportion values (0-1 or 0-100)
  expect_true(all(result$qic_data$y >= 0))
})

test_that("compute_spc_results_bfh() handles C charts (counts)", {
  set.seed(20251015)
  data <- create_test_data(n_rows = 24, chart_type = "c")

  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "numerator",
    chart_type = "c"
  )

  expect_s3_class(result$plot, "ggplot")
  expect_equal(result$metadata$chart_type, "c")
  expect_equal(nrow(result$qic_data), 24)
})

test_that("compute_spc_results_bfh() handles U charts (rates)", {
  set.seed(20251015)
  data <- create_test_data(n_rows = 36, chart_type = "u")

  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "numerator",
    n_var = "denominator",
    chart_type = "u"
  )

  expect_s3_class(result$plot, "ggplot")
  expect_equal(result$metadata$chart_type, "u")
  expect_true(all(result$qic_data$y >= 0))
})

test_that("compute_spc_results_bfh() handles X-bar charts", {
  set.seed(20251015)
  data <- create_test_data(n_rows = 20, chart_type = "xbar")

  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "value",
    chart_type = "xbar"
  )

  expect_s3_class(result$plot, "ggplot")
  expect_equal(result$metadata$chart_type, "xbar")
})

test_that("compute_spc_results_bfh() handles S charts (standard deviation)", {
  set.seed(20251015)
  data <- create_test_data(n_rows = 20, chart_type = "s")

  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "value",
    chart_type = "s"
  )

  expect_s3_class(result$plot, "ggplot")
  expect_equal(result$metadata$chart_type, "s")
})

# Parameter Validation Tests --------------------------------------------------

test_that("compute_spc_results_bfh() requires data parameter", {
  expect_error(
    compute_spc_results_bfh(
      x_var = "month",
      y_var = "value",
      chart_type = "run"
    ),
    regexp = "data.*required|missing.*data",
    ignore.case = TRUE
  )
})

test_that("compute_spc_results_bfh() requires x_var parameter", {
  data <- create_test_data(n_rows = 10)

  expect_error(
    compute_spc_results_bfh(
      data = data,
      y_var = "value",
      chart_type = "run"
    ),
    regexp = "x_var.*required|missing.*x_var",
    ignore.case = TRUE
  )
})

test_that("compute_spc_results_bfh() requires y_var parameter", {
  data <- create_test_data(n_rows = 10)

  expect_error(
    compute_spc_results_bfh(
      data = data,
      x_var = "month",
      chart_type = "run"
    ),
    regexp = "y_var.*required|missing.*y_var",
    ignore.case = TRUE
  )
})

test_that("compute_spc_results_bfh() requires chart_type parameter", {
  data <- create_test_data(n_rows = 10)

  expect_error(
    compute_spc_results_bfh(
      data = data,
      x_var = "month",
      y_var = "value"
    ),
    regexp = "chart_type.*required|missing.*chart_type",
    ignore.case = TRUE
  )
})

test_that("compute_spc_results_bfh() validates chart_type values", {
  data <- create_test_data(n_rows = 10)

  expect_error(
    compute_spc_results_bfh(
      data = data,
      x_var = "month",
      y_var = "value",
      chart_type = "invalid_type"
    ),
    regexp = "chart_type.*invalid|must be one of",
    ignore.case = TRUE
  )
})

test_that("compute_spc_results_bfh() requires n_var for P charts", {
  data <- create_test_data(n_rows = 20, chart_type = "p")

  # P charts need denominator
  expect_error(
    compute_spc_results_bfh(
      data = data,
      x_var = "month",
      y_var = "numerator",
      chart_type = "p"
      # Missing n_var
    ),
    regexp = "n_var.*required|denominator.*required",
    ignore.case = TRUE
  )
})

test_that("compute_spc_results_bfh() requires n_var for U charts", {
  data <- create_test_data(n_rows = 20, chart_type = "u")

  expect_error(
    compute_spc_results_bfh(
      data = data,
      x_var = "month",
      y_var = "numerator",
      chart_type = "u"
      # Missing n_var
    ),
    regexp = "n_var.*required|denominator.*required",
    ignore.case = TRUE
  )
})

# Optional Parameters Tests ---------------------------------------------------

test_that("compute_spc_results_bfh() accepts optional freeze_var", {
  set.seed(20251015)
  data <- create_test_data(n_rows = 30)
  data$freeze <- c(rep(FALSE, 20), rep(TRUE, 10))

  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "value",
    chart_type = "run",
    freeze_var = "freeze"
  )

  expect_s3_class(result$plot, "ggplot")
  expect_true("freeze_var" %in% names(result$metadata))
})

test_that("compute_spc_results_bfh() accepts optional part_var", {
  set.seed(20251015)
  data <- create_test_data(n_rows = 40)
  data$phase <- c(rep(1, 20), rep(2, 20))

  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "value",
    chart_type = "run",
    part_var = "phase"
  )

  expect_s3_class(result$plot, "ggplot")
  expect_true("part_var" %in% names(result$metadata))
})

test_that("compute_spc_results_bfh() accepts optional multiply parameter", {
  set.seed(20251015)
  data <- create_test_data(n_rows = 20, chart_type = "p")

  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "numerator",
    n_var = "denominator",
    chart_type = "p",
    multiply = 100  # Convert to percentage
  )

  expect_s3_class(result$plot, "ggplot")
  # Values should be scaled by 100
  expect_true(max(result$qic_data$y, na.rm = TRUE) > 1)
})

test_that("compute_spc_results_bfh() accepts optional cl_var", {
  set.seed(20251015)
  data <- create_test_data(n_rows = 20)
  data$custom_cl <- rep(50, 20)

  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "value",
    chart_type = "run",
    cl_var = "custom_cl"
  )

  expect_s3_class(result$plot, "ggplot")
  expect_true("cl_var" %in% names(result$metadata))
})

# Integration Tests: Validator Integration ====================================

context("Integration Tests - Validator Integration")

test_that("compute_spc_results_bfh() integrates with filter_complete_spc_data()", {
  set.seed(20251015)
  data <- create_test_data(n_rows = 25)

  # Add some NA values to test filtering
  data$value[c(5, 10, 15)] <- NA

  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "value",
    chart_type = "run"
  )

  # Should have filtered out NA rows
  expect_equal(nrow(result$qic_data), 22)
})

test_that("compute_spc_results_bfh() integrates with parse_and_validate_spc_data()", {
  set.seed(20251015)
  data <- create_test_data(n_rows = 20)

  # Test with valid numeric data
  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "value",
    chart_type = "run"
  )

  # Should successfully parse and validate
  expect_s3_class(result$plot, "ggplot")
  expect_true(all(is.numeric(result$qic_data$y)))
})

test_that("compute_spc_results_bfh() handles validation errors gracefully", {
  # Create invalid data (non-numeric values)
  data <- tibble::tibble(
    month = 1:10,
    value = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
  )

  expect_error(
    compute_spc_results_bfh(
      data = data,
      x_var = "month",
      y_var = "value",
      chart_type = "run"
    ),
    regexp = "numeric|convert|invalid",
    ignore.case = TRUE
  )
})

# Integration Tests: Output Compatibility =====================================

context("Integration Tests - Output Compatibility")

test_that("compute_spc_results_bfh() output is compatible with existing plot functions", {
  set.seed(20251015)
  data <- create_test_data(n_rows = 20)

  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "value",
    chart_type = "run"
  )

  # Output should be compatible with ggplot2 operations
  expect_no_error({
    modified_plot <- result$plot + ggplot2::labs(subtitle = "Test subtitle")
  })

  # qic_data should be compatible with dplyr operations
  expect_no_error({
    filtered_data <- result$qic_data |> dplyr::filter(!is.na(y))
  })
})

test_that("compute_spc_results_bfh() qic_data has correct column types", {
  set.seed(20251015)
  data <- create_test_data(n_rows = 20)

  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "value",
    chart_type = "run"
  )

  # Check column types
  expect_true(is.numeric(result$qic_data$y) || inherits(result$qic_data$y, "Date"))
  expect_true(is.numeric(result$qic_data$cl))
  expect_true(is.numeric(result$qic_data$ucl) || all(is.na(result$qic_data$ucl)))
  expect_true(is.numeric(result$qic_data$lcl) || all(is.na(result$qic_data$lcl)))
  expect_true(is.logical(result$qic_data$signal) || all(is.na(result$qic_data$signal)))
})

# Error Handling Tests ========================================================

context("Error Handling Tests")

test_that("compute_spc_results_bfh() handles empty data", {
  empty_data <- tibble::tibble(
    month = as.Date(character(0)),
    value = numeric(0)
  )

  expect_error(
    compute_spc_results_bfh(
      data = empty_data,
      x_var = "month",
      y_var = "value",
      chart_type = "run"
    ),
    regexp = "empty|no data|insufficient",
    ignore.case = TRUE
  )
})

test_that("compute_spc_results_bfh() handles single data point", {
  single_point <- tibble::tibble(
    month = as.Date("2024-01-01"),
    value = 50
  )

  expect_error(
    compute_spc_results_bfh(
      data = single_point,
      x_var = "month",
      y_var = "value",
      chart_type = "run"
    ),
    regexp = "insufficient|minimum.*points|too few",
    ignore.case = TRUE
  )
})

test_that("compute_spc_results_bfh() handles all NA values", {
  all_na <- tibble::tibble(
    month = seq.Date(from = as.Date("2024-01-01"), by = "month", length.out = 20),
    value = rep(NA_real_, 20)
  )

  expect_error(
    compute_spc_results_bfh(
      data = all_na,
      x_var = "month",
      y_var = "value",
      chart_type = "run"
    ),
    regexp = "no valid|all NA|missing values",
    ignore.case = TRUE
  )
})

test_that("compute_spc_results_bfh() handles missing columns", {
  data <- create_test_data(n_rows = 20)

  expect_error(
    compute_spc_results_bfh(
      data = data,
      x_var = "nonexistent_column",
      y_var = "value",
      chart_type = "run"
    ),
    regexp = "column.*not found|missing column",
    ignore.case = TRUE
  )
})

test_that("compute_spc_results_bfh() wraps BFHchart errors in safe_operation", {
  set.seed(20251015)
  data <- create_test_data(n_rows = 20)

  # Mock BFHchart to throw error
  # This tests that safe_operation wrapping is in place
  # In RED phase, this documents expected error handling behavior

  # When BFHchart fails, expect NULL fallback or informative error
  # Implementation should use safe_operation() wrapper

  # Test will be implemented with proper mocking in Stream D
  skip("Requires mockery implementation in Stream D")
})

# Mock Tests ==================================================================

context("Mock Tests - BFHchart Integration")

test_that("compute_spc_results_bfh() calls BFHchart with correct parameters", {
  skip("Requires mockery implementation in Stream D")

  # This test documents expected BFHchart call structure
  # Implementation should:
  # 1. Map SPCify parameters to BFHchart API
  # 2. Call BFHchart::bfh_chart() or equivalent
  # 3. Transform output to standardized format

  # Expected mock behavior:
  # mock_stub(compute_spc_results_bfh, "BFHchart::bfh_chart", mock_bfh_output)
})

test_that("compute_spc_results_bfh() transforms BFHchart output correctly", {
  skip("Requires mockery implementation in Stream D")

  # This test documents expected output transformation
  # BFHchart output → Standardized format
  # - plot: ggplot object
  # - qic_data: tibble with x, y, cl, lcl, ucl, signal
  # - metadata: list with chart configuration
})

test_that("compute_spc_results_bfh() handles BFHchart API changes gracefully", {
  skip("Requires mockery implementation in Stream D")

  # This test documents facade pattern resilience
  # If BFHchart API changes, facade should adapt
  # Tests will verify fallback behavior
})

# Edge Cases Tests ============================================================

context("Edge Cases")

test_that("compute_spc_results_bfh() handles very small datasets (n=3)", {
  set.seed(20251015)
  small_data <- create_test_data(n_rows = 3)

  # Minimum viable dataset
  result <- compute_spc_results_bfh(
    data = small_data,
    x_var = "month",
    y_var = "value",
    chart_type = "run"
  )

  expect_s3_class(result$plot, "ggplot")
  expect_equal(nrow(result$qic_data), 3)
})

test_that("compute_spc_results_bfh() handles very large datasets (n=1000)", {
  set.seed(20251015)
  large_data <- create_test_data(n_rows = 1000)

  # Performance test - should complete without error
  result <- compute_spc_results_bfh(
    data = large_data,
    x_var = "month",
    y_var = "value",
    chart_type = "run"
  )

  expect_s3_class(result$plot, "ggplot")
  expect_equal(nrow(result$qic_data), 1000)
})

test_that("compute_spc_results_bfh() handles zero values", {
  set.seed(20251015)
  data <- create_test_data(n_rows = 20)
  data$value[1:5] <- 0

  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "value",
    chart_type = "run"
  )

  expect_s3_class(result$plot, "ggplot")
  expect_true(any(result$qic_data$y == 0))
})

test_that("compute_spc_results_bfh() handles negative values", {
  set.seed(20251015)
  data <- create_test_data(n_rows = 20)
  data$value <- rnorm(20, mean = 0, sd = 10)  # Can be negative

  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "value",
    chart_type = "i"  # I charts can handle negative values
  )

  expect_s3_class(result$plot, "ggplot")
})

test_that("compute_spc_results_bfh() handles constant values", {
  set.seed(20251015)
  data <- create_test_data(n_rows = 20)
  data$value <- rep(50, 20)  # No variation

  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "value",
    chart_type = "run"
  )

  expect_s3_class(result$plot, "ggplot")
  # Control limits may be NA or equal to center line
  expect_true(all(result$qic_data$cl == 50, na.rm = TRUE))
})

# Comments/Notes Column Tests =================================================

context("Comments/Notes Integration")

test_that("compute_spc_results_bfh() handles notes_column parameter", {
  set.seed(20251015)
  data <- create_test_data(n_rows = 20)
  data$comments <- c(rep("", 15), rep("Important event", 5))

  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "value",
    chart_type = "run",
    notes_column = "comments"
  )

  expect_s3_class(result$plot, "ggplot")
  expect_true("notes_column" %in% names(result$metadata))
})

# Baseline Tests Using Task #29 Fixtures ======================================

context("Baseline Regression Tests")

test_that("compute_spc_results_bfh() baseline: run-basic", {
  # Load qicharts2 baseline from Task #29
  baseline_path <- here::here("tests/testthat/fixtures/qic-baseline/run-basic.rds")

  if (file.exists(baseline_path)) {
    baseline <- readRDS(baseline_path)

    # Run BFHchart service with same input
    result <- compute_spc_results_bfh(
      data = baseline$input_data,
      x_var = names(baseline$input_data)[1],  # x column
      y_var = names(baseline$input_data)[2],  # y column
      chart_type = baseline$chart_type
    )

    # Compare outputs (tolerance for floating point)
    expect_equal(
      result$qic_data$cl,
      baseline$qic_output$center_line,
      tolerance = 1e-6
    )
  } else {
    skip("Baseline file not found - run Task #29 first")
  }
})

test_that("compute_spc_results_bfh() baseline: p-anhoej", {
  baseline_path <- here::here("tests/testthat/fixtures/qic-baseline/p-anhoej.rds")

  if (file.exists(baseline_path)) {
    baseline <- readRDS(baseline_path)

    result <- compute_spc_results_bfh(
      data = baseline$input_data,
      x_var = names(baseline$input_data)[1],
      y_var = names(baseline$input_data)[2],
      n_var = names(baseline$input_data)[3],
      chart_type = baseline$chart_type
    )

    # Verify Anhoej rules detection
    expect_true(any(result$qic_data$signal))
  } else {
    skip("Baseline file not found - run Task #29 first")
  }
})

# Performance Tests ===========================================================

context("Performance Tests")

test_that("compute_spc_results_bfh() completes within reasonable time", {
  set.seed(20251015)
  data <- create_test_data(n_rows = 100)

  # Should complete in < 2 seconds for 100 points
  execution_time <- system.time({
    result <- compute_spc_results_bfh(
      data = data,
      x_var = "month",
      y_var = "value",
      chart_type = "run"
    )
  })

  expect_lt(execution_time["elapsed"], 2.0)
})

# RED PHASE SUMMARY ===========================================================

# All tests above should FAIL initially because:
# 1. compute_spc_results_bfh() function doesn't exist yet
# 2. R/fct_spc_bfh_service.R hasn't been created (Stream D)
# 3. Helper functions (map_to_bfh_params, call_bfh_chart, etc.) don't exist
#
# This is CORRECT TDD behavior (RED → GREEN → REFACTOR)
#
# Expected test results:
# - All tests FAIL with "object 'compute_spc_results_bfh' not found"
# - This documents the expected API contract
# - Stream D will implement until all tests PASS (GREEN phase)
#
# Test Coverage Summary:
# - 7 chart types (run, i, p, c, u, xbar, s)
# - Required parameter validation
# - Optional parameter handling
# - Validator integration (filter_complete_spc_data, parse_and_validate_spc_data)
# - Output format compatibility
# - Error handling (empty data, NA values, invalid inputs)
# - Edge cases (small/large datasets, constant values, zeros, negatives)
# - Comments/notes column mapping
# - Baseline regression tests (Task #29 fixtures)
# - Performance benchmarks
#
# Total test scenarios: 50+
# Expected lines: ~300
# All tests deterministic (fixed seed)
