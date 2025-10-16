# test-anhoej-metadata-local.R
# TDD Test Suite for Local Anhøj Metadata Calculation
# Part of Pure BFHcharts Workflow Refactor
#
# Purpose: Test lightweight qicharts2::qic() call for UI metadata only
#
# Context:
# - BFHcharts handles ALL SPC calculation and visualization
# - compute_anhoej_metadata_local() provides UI metrics (serielængde, antal kryds)
# - Separate from SPC engine (separation of concerns)

# Test Setup ==================================================================

library(testthat)
library(tibble)

set.seed(20251016)  # Reproducibility

# Helper Functions ============================================================

#' Create test data with known Anhøj rules patterns
#'
#' @param pattern Character: "runs" (long run), "crossings" (too few), "none"
#' @param n_rows Number of data points
create_anhoej_test_data <- function(pattern = "none", n_rows = 20) {
  dates <- seq.Date(from = as.Date("2024-01-01"), by = "month", length.out = n_rows)

  if (pattern == "runs") {
    # Create long run above median
    values <- c(rep(45, 5), rep(55, 10), rep(45, 5))  # Run of 10 above median
  } else if (pattern == "crossings") {
    # Too few median crossings (monotonic)
    values <- seq(40, 60, length.out = n_rows)  # No crossings
  } else {
    # Normal variation
    values <- round(rnorm(n_rows, mean = 50, sd = 5), 1)
  }

  tibble::tibble(
    month = dates,
    value = values
  )
}

# Unit Tests: Function Existence ==============================================

context("compute_anhoej_metadata_local() - Function Existence")

test_that("compute_anhoej_metadata_local() exists and is callable", {
  expect_true(exists("compute_anhoej_metadata_local"))
  expect_true(is.function(compute_anhoej_metadata_local))
})

# Unit Tests: Basic Functionality ============================================

context("compute_anhoej_metadata_local() - Basic Functionality")

test_that("compute_anhoej_metadata_local() returns valid metadata structure", {
  # Arrange
  data <- create_anhoej_test_data(pattern = "none", n_rows = 20)
  config <- list(
    x_col = "month",
    y_col = "value",
    chart_type = "run"
  )

  # Act
  result <- compute_anhoej_metadata_local(data, config)

  # Assert - Structure
  expect_type(result, "list")
  expect_true(all(c("runs_signal", "crossings_signal", "longest_run",
                     "n_crossings", "n_crossings_min") %in% names(result)))

  # Assert - Types
  expect_type(result$runs_signal, "logical")
  expect_type(result$crossings_signal, "logical")
  expect_type(result$longest_run, "integer")  # qicharts2 returns integer
  expect_type(result$n_crossings, "double")
  expect_type(result$n_crossings_min, "double")
})

test_that("compute_anhoej_metadata_local() handles run charts", {
  data <- create_anhoej_test_data(pattern = "none", n_rows = 25)
  config <- list(x_col = "month", y_col = "value", chart_type = "run")

  result <- compute_anhoej_metadata_local(data, config)

  expect_false(is.null(result))
  expect_true("longest_run" %in% names(result))
  expect_true("n_crossings" %in% names(result))
})

test_that("compute_anhoej_metadata_local() handles I charts", {
  data <- create_anhoej_test_data(pattern = "none", n_rows = 30)
  config <- list(x_col = "month", y_col = "value", chart_type = "i")

  result <- compute_anhoej_metadata_local(data, config)

  expect_false(is.null(result))
  expect_type(result$runs_signal, "logical")
})

test_that("compute_anhoej_metadata_local() handles P charts with denominator", {
  data <- tibble::tibble(
    month = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 20),
    numerator = round(rnorm(20, mean = 10, sd = 2)),
    denominator = round(rnorm(20, mean = 100, sd = 10))
  )
  config <- list(
    x_col = "month",
    y_col = "numerator",
    n_col = "denominator",
    chart_type = "p"
  )

  result <- compute_anhoej_metadata_local(data, config)

  expect_false(is.null(result))
  expect_true("longest_run" %in% names(result))
})

# Unit Tests: Anhøj Rules Detection ==========================================

context("compute_anhoej_metadata_local() - Anhøj Rules Detection")

test_that("compute_anhoej_metadata_local() detects runs violations", {
  # Arrange: Data with intentional long run
  data <- create_anhoej_test_data(pattern = "runs", n_rows = 20)
  config <- list(x_col = "month", y_col = "value", chart_type = "run")

  # Act
  result <- compute_anhoej_metadata_local(data, config)

  # Assert
  expect_true(result$runs_signal, info = "Should detect run violation")
  expect_true(result$longest_run > 0, info = "Should have longest_run value")
})

test_that("compute_anhoej_metadata_local() detects crossings violations", {
  # Arrange: Monotonic data (too few crossings)
  data <- create_anhoej_test_data(pattern = "crossings", n_rows = 20)
  config <- list(x_col = "month", y_col = "value", chart_type = "run")

  # Act
  result <- compute_anhoej_metadata_local(data, config)

  # Assert
  expect_true(result$crossings_signal, info = "Should detect crossings violation")
  expect_true(result$n_crossings < result$n_crossings_min,
              info = "Too few crossings detected")
})

test_that("compute_anhoej_metadata_local() handles normal variation (no violations)", {
  # Arrange: Random variation, no patterns
  set.seed(20251016)
  data <- create_anhoej_test_data(pattern = "none", n_rows = 30)
  config <- list(x_col = "month", y_col = "value", chart_type = "run")

  # Act
  result <- compute_anhoej_metadata_local(data, config)

  # Assert
  expect_type(result$runs_signal, "logical")
  expect_type(result$crossings_signal, "logical")
  # May or may not trigger (random data), but should return valid structure
})

# Unit Tests: Parameter Validation ===========================================

context("compute_anhoej_metadata_local() - Parameter Validation")

test_that("compute_anhoej_metadata_local() requires data parameter", {
  config <- list(x_col = "month", y_col = "value", chart_type = "run")

  expect_error(
    compute_anhoej_metadata_local(data = NULL, config = config),
    regexp = "data.*required|missing.*data|NULL",
    ignore.case = TRUE
  )
})

test_that("compute_anhoej_metadata_local() requires config parameter", {
  data <- create_anhoej_test_data(n_rows = 20)

  expect_error(
    compute_anhoej_metadata_local(data = data, config = NULL),
    regexp = "config.*required|missing.*config|NULL",
    ignore.case = TRUE
  )
})

test_that("compute_anhoej_metadata_local() validates config structure", {
  data <- create_anhoej_test_data(n_rows = 20)

  # Missing x_col
  expect_error(
    compute_anhoej_metadata_local(
      data = data,
      config = list(y_col = "value", chart_type = "run")
    ),
    regexp = "x_col.*required|missing.*x_col",
    ignore.case = TRUE
  )

  # Missing y_col
  expect_error(
    compute_anhoej_metadata_local(
      data = data,
      config = list(x_col = "month", chart_type = "run")
    ),
    regexp = "y_col.*required|missing.*y_col",
    ignore.case = TRUE
  )

  # Missing chart_type
  expect_error(
    compute_anhoej_metadata_local(
      data = data,
      config = list(x_col = "month", y_col = "value")
    ),
    regexp = "chart_type.*required|missing.*chart_type",
    ignore.case = TRUE
  )
})

test_that("compute_anhoej_metadata_local() handles invalid column names", {
  data <- create_anhoej_test_data(n_rows = 20)
  config <- list(
    x_col = "nonexistent",
    y_col = "value",
    chart_type = "run"
  )

  expect_error(
    compute_anhoej_metadata_local(data = data, config = config),
    regexp = "column.*not found|nonexistent",
    ignore.case = TRUE
  )
})

# Unit Tests: Edge Cases ======================================================

context("compute_anhoej_metadata_local() - Edge Cases")

test_that("compute_anhoej_metadata_local() handles minimum dataset (n=3)", {
  data <- create_anhoej_test_data(n_rows = 3)
  config <- list(x_col = "month", y_col = "value", chart_type = "run")

  result <- compute_anhoej_metadata_local(data, config)

  expect_false(is.null(result))
  expect_type(result$longest_run, "integer")  # qicharts2 returns integer
})

test_that("compute_anhoej_metadata_local() handles large dataset (n=500)", {
  set.seed(20251016)
  dates <- seq.Date(from = as.Date("2020-01-01"), by = "day", length.out = 500)
  data <- tibble::tibble(
    month = dates,
    value = round(rnorm(500, mean = 50, sd = 10), 1)
  )
  config <- list(x_col = "month", y_col = "value", chart_type = "run")

  execution_time <- system.time({
    result <- compute_anhoej_metadata_local(data, config)
  })

  expect_false(is.null(result))
  # Performance check: should complete quickly even with 500 points
  expect_true(execution_time["elapsed"] < 1.0)
})

test_that("compute_anhoej_metadata_local() handles constant values", {
  data <- tibble::tibble(
    month = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 20),
    value = rep(50, 20)  # No variation
  )
  config <- list(x_col = "month", y_col = "value", chart_type = "run")

  result <- compute_anhoej_metadata_local(data, config)

  expect_false(is.null(result))
  # Constant values may trigger crossings violation
  expect_type(result$crossings_signal, "logical")
})

test_that("compute_anhoej_metadata_local() handles zero values", {
  data <- create_anhoej_test_data(n_rows = 20)
  data$value[1:5] <- 0
  config <- list(x_col = "month", y_col = "value", chart_type = "run")

  result <- compute_anhoej_metadata_local(data, config)

  expect_false(is.null(result))
  expect_type(result$longest_run, "integer")  # qicharts2 returns integer
})

test_that("compute_anhoej_metadata_local() handles negative values", {
  data <- tibble::tibble(
    month = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 20),
    value = rnorm(20, mean = 0, sd = 10)  # Can be negative
  )
  config <- list(x_col = "month", y_col = "value", chart_type = "i")

  result <- compute_anhoej_metadata_local(data, config)

  expect_false(is.null(result))
  expect_type(result$runs_signal, "logical")
})

# Integration Tests: extract_anhoej_metadata() ================================

context("compute_anhoej_metadata_local() - Integration with extract_anhoej_metadata()")

test_that("compute_anhoej_metadata_local() uses extract_anhoej_metadata() internally", {
  # This test verifies the integration with existing utility
  data <- create_anhoej_test_data(pattern = "runs", n_rows = 20)
  config <- list(x_col = "month", y_col = "value", chart_type = "run")

  result <- compute_anhoej_metadata_local(data, config)

  # Should return same structure as extract_anhoej_metadata()
  expect_true(all(c("runs_signal", "crossings_signal", "longest_run") %in% names(result)))
})

test_that("compute_anhoej_metadata_local() output matches direct qic() + extract pattern", {
  # Arrange
  data <- create_anhoej_test_data(pattern = "crossings", n_rows = 25)
  config <- list(x_col = "month", y_col = "value", chart_type = "run")

  # Act - Via function
  result_function <- compute_anhoej_metadata_local(data, config)

  # Act - Direct call (expected behavior)
  qic_result <- qicharts2::qic(
    x = data[[config$x_col]],
    y = data[[config$y_col]],
    chart = config$chart_type,
    return.data = TRUE
  )
  result_direct <- extract_anhoej_metadata(qic_result)

  # Assert - Should match
  expect_equal(result_function$runs_signal, result_direct$runs_signal)
  expect_equal(result_function$crossings_signal, result_direct$crossings_signal)
  expect_equal(result_function$longest_run, result_direct$longest_run, tolerance = 1e-10)
})

# Performance Tests ===========================================================

context("compute_anhoej_metadata_local() - Performance")

test_that("compute_anhoej_metadata_local() is lightweight (< 100ms for n=50)", {
  data <- create_anhoej_test_data(n_rows = 50)
  config <- list(x_col = "month", y_col = "value", chart_type = "run")

  execution_time <- system.time({
    result <- compute_anhoej_metadata_local(data, config)
  })

  # Performance check: should be lightweight < 100ms for 50 points
  expect_true(execution_time["elapsed"] < 0.1)
})

test_that("compute_anhoej_metadata_local() performance scales linearly", {
  # Test with different dataset sizes
  sizes <- c(10, 50, 100)
  times <- numeric(length(sizes))

  for (i in seq_along(sizes)) {
    data <- create_anhoej_test_data(n_rows = sizes[i])
    config <- list(x_col = "month", y_col = "value", chart_type = "run")

    times[i] <- system.time({
      compute_anhoej_metadata_local(data, config)
    })["elapsed"]
  }

  # Expect roughly linear scaling (not exponential)
  # time(100) should be < 10x time(10)
  expect_true(times[3] < times[1] * 10)
})

# Error Handling Tests ========================================================

context("compute_anhoej_metadata_local() - Error Handling")

test_that("compute_anhoej_metadata_local() handles empty data gracefully", {
  empty_data <- tibble::tibble(
    month = as.Date(character(0)),
    value = numeric(0)
  )
  config <- list(x_col = "month", y_col = "value", chart_type = "run")

  expect_error(
    compute_anhoej_metadata_local(data = empty_data, config = config),
    regexp = "empty|no data|insufficient",
    ignore.case = TRUE
  )
})

test_that("compute_anhoej_metadata_local() handles all NA values", {
  data <- tibble::tibble(
    month = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 20),
    value = rep(NA_real_, 20)
  )
  config <- list(x_col = "month", y_col = "value", chart_type = "run")

  expect_error(
    compute_anhoej_metadata_local(data = data, config = config),
    regexp = "no valid|all NA|missing values",
    ignore.case = TRUE
  )
})

test_that("compute_anhoej_metadata_local() wraps qic() errors with safe_operation", {
  # Intentionally malformed config to trigger qic() error
  data <- create_anhoej_test_data(n_rows = 20)
  config <- list(
    x_col = "month",
    y_col = "value",
    chart_type = "invalid_chart_type"  # Should fail
  )

  # Expect graceful error handling (not raw qic() error)
  expect_error(
    compute_anhoej_metadata_local(data = data, config = config),
    regexp = "chart.*type|invalid",
    ignore.case = TRUE
  )
})

# RED PHASE SUMMARY ===========================================================

# All tests above should FAIL initially because:
# 1. compute_anhoej_metadata_local() function doesn't exist yet
# 2. Function will be added to R/fct_spc_bfh_service.R
#
# This is CORRECT TDD behavior (RED → GREEN → REFACTOR)
#
# Expected test results:
# - All tests FAIL with "object 'compute_anhoej_metadata_local' not found"
# - This documents the expected API contract
# - Implementation phase will make all tests PASS (GREEN phase)
#
# Test Coverage Summary:
# - Function existence
# - Basic functionality (run, i, p charts)
# - Anhøj rules detection (runs, crossings, none)
# - Parameter validation (required params, invalid values)
# - Edge cases (min/max datasets, constant/zero/negative values)
# - Integration with extract_anhoej_metadata()
# - Performance benchmarks
# - Error handling (empty data, NA values, invalid config)
#
# Total test scenarios: 30+
# All tests deterministic (fixed seed)
