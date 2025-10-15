# test-anhoej-rules.R
# Comprehensive tests for Anhøj rules detection and integration
#
# Test coverage:
# 1. Unit tests for extract_anhoej_metadata()
# 2. Unit tests for validate_anhoej_columns()
# 3. Unit tests for calculate_combined_anhoej_signal()
# 4. Unit tests for format_anhoej_metadata()
# 5. Integration tests with BFHchart service layer
# 6. Validation against qicharts2 baselines

library(testthat)
library(tibble)

# Helper: Load baseline fixture
load_baseline <- function(chart_type, scenario = "basic") {
  fixture_path <- test_path(sprintf(
    "fixtures/qic-baseline/%s-%s.rds",
    chart_type,
    scenario
  ))

  if (!file.exists(fixture_path)) {
    skip(sprintf("Baseline fixture not found: %s", fixture_path))
  }

  readRDS(fixture_path)
}

# ==============================================================================
# Unit Tests: extract_anhoej_metadata()
# ==============================================================================

test_that("extract_anhoej_metadata() extracts runs signal correctly", {
  # Arrange: Create mock qic_data with runs violation
  qic_data <- tibble(
    x = 1:10,
    y = c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19),
    cl = 12,
    runs.signal = c(rep(FALSE, 5), rep(TRUE, 5)),
    n.crossings = 2,
    n.crossings.min = 5,
    longest.run = 8,
    longest.run.max = 6
  )

  # Act
  result <- extract_anhoej_metadata(qic_data)

  # Assert
  expect_false(is.null(result))
  expect_true(result$runs_signal)
  expect_equal(result$longest_run, 8)
})

test_that("extract_anhoej_metadata() detects crossings violation", {
  # Arrange: Too few crossings
  qic_data <- tibble(
    x = 1:20,
    y = runif(20, 8, 12),
    cl = 10,
    runs.signal = rep(FALSE, 20),
    n.crossings = 3,
    n.crossings.min = 8,
    longest.run = 5,
    longest.run.max = 7
  )

  # Act
  result <- extract_anhoej_metadata(qic_data)

  # Assert
  expect_true(result$crossings_signal)
  expect_equal(result$n_crossings, 3)
  expect_equal(result$n_crossings.min, 8)
})

test_that("extract_anhoej_metadata() returns NULL for missing columns", {
  # Arrange: qic_data without required columns
  qic_data <- tibble(
    x = 1:10,
    y = 1:10,
    cl = 5
  )

  # Act & Assert
  expect_warning(result <- extract_anhoej_metadata(qic_data))
  expect_null(result)
})

test_that("extract_anhoej_metadata() handles anhoej.signal column if present", {
  # Arrange
  qic_data <- tibble(
    x = 1:10,
    y = 1:10,
    cl = 5,
    runs.signal = rep(FALSE, 10),
    n.crossings = 5,
    n.crossings.min = 3,
    anhoej.signal = c(rep(FALSE, 5), rep(TRUE, 5))
  )

  # Act
  result <- extract_anhoej_metadata(qic_data)

  # Assert
  expect_equal(result$signal_points, c(rep(FALSE, 5), rep(TRUE, 5)))
})

test_that("extract_anhoej_metadata() handles NA values gracefully", {
  # Arrange
  qic_data <- tibble(
    x = 1:10,
    y = 1:10,
    cl = 5,
    runs.signal = c(rep(NA, 5), rep(TRUE, 5)),
    n.crossings = 5,
    n.crossings.min = 3
  )

  # Act
  result <- extract_anhoej_metadata(qic_data)

  # Assert: NAs should be converted to FALSE
  expect_equal(result$signal_points, c(rep(FALSE, 5), rep(TRUE, 5)))
})

# ==============================================================================
# Unit Tests: validate_anhoej_columns()
# ==============================================================================

test_that("validate_anhoej_columns() returns TRUE for valid qic_data", {
  # Arrange
  qic_data <- tibble(
    runs.signal = c(TRUE, FALSE),
    n.crossings = 5,
    n.crossings.min = 3
  )

  # Act & Assert
  expect_true(validate_anhoej_columns(qic_data))
})

test_that("validate_anhoej_columns() returns FALSE for missing columns", {
  # Arrange
  qic_data <- tibble(
    runs.signal = c(TRUE, FALSE)
    # Missing n.crossings and n.crossings.min
  )

  # Act & Assert
  expect_warning(result <- validate_anhoej_columns(qic_data))
  expect_false(result)
})

test_that("validate_anhoej_columns() validates anhoej.signal if required", {
  # Arrange
  qic_data <- tibble(
    runs.signal = c(TRUE, FALSE),
    n.crossings = 5,
    n.crossings.min = 3
    # Missing anhoej.signal
  )

  # Act & Assert
  expect_warning(result <- validate_anhoej_columns(qic_data, require_signal = TRUE))
  expect_false(result)
})

test_that("validate_anhoej_columns() handles NULL input", {
  # Act & Assert
  expect_warning(result <- validate_anhoej_columns(NULL))
  expect_false(result)
})

# ==============================================================================
# Unit Tests: calculate_combined_anhoej_signal()
# ==============================================================================

test_that("calculate_combined_anhoej_signal() combines runs signals", {
  # Arrange
  qic_data <- tibble(
    runs.signal = c(FALSE, FALSE, TRUE, TRUE, FALSE),
    n.crossings = 5,
    n.crossings.min = 3
  )

  # Act
  result <- calculate_combined_anhoej_signal(qic_data)

  # Assert
  expect_equal(result, c(FALSE, FALSE, TRUE, TRUE, FALSE))
})

test_that("calculate_combined_anhoej_signal() handles missing columns gracefully", {
  # Arrange: Missing required columns
  qic_data <- tibble(
    x = 1:5,
    y = 1:5
  )

  # Act
  result <- calculate_combined_anhoej_signal(qic_data)

  # Assert: Should return all FALSE
  expect_equal(result, rep(FALSE, 5))
})

test_that("calculate_combined_anhoej_signal() converts non-logical to logical", {
  # Arrange
  qic_data <- tibble(
    runs.signal = c(0, 0, 1, 1, 0),  # Numeric instead of logical
    n.crossings = 5,
    n.crossings.min = 3
  )

  # Act
  result <- calculate_combined_anhoej_signal(qic_data)

  # Assert
  expect_type(result, "logical")
  expect_equal(result, c(FALSE, FALSE, TRUE, TRUE, FALSE))
})

# ==============================================================================
# Unit Tests: format_anhoej_metadata()
# ==============================================================================

test_that("format_anhoej_metadata() formats runs violation", {
  # Arrange
  metadata <- list(
    runs_signal = TRUE,
    crossings_signal = FALSE,
    longest_run = 8,
    n_crossings = 5,
    n_crossings_min = 3
  )

  # Act
  result <- format_anhoej_metadata(metadata)

  # Assert
  expect_match(result, "Runs: Ja")
  expect_match(result, "8 punkter")
})

test_that("format_anhoej_metadata() formats no violations", {
  # Arrange
  metadata <- list(
    runs_signal = FALSE,
    crossings_signal = FALSE,
    n_crossings = 5,
    n_crossings_min = 3
  )

  # Act
  result <- format_anhoej_metadata(metadata)

  # Assert
  expect_equal(result, "Ingen Anhøj violations")
})

test_that("format_anhoej_metadata() handles NULL input", {
  # Act
  result <- format_anhoej_metadata(NULL)

  # Assert
  expect_equal(result, "Anhøj metadata ikke tilgængelig")
})

test_that("format_anhoej_metadata() formats crossings violation", {
  # Arrange
  metadata <- list(
    runs_signal = FALSE,
    crossings_signal = TRUE,
    n_crossings = 3,
    n_crossings_min = 8
  )

  # Act
  result <- format_anhoej_metadata(metadata)

  # Assert
  expect_match(result, "Crossings: Ja")
  expect_match(result, "3/8")
})

# ==============================================================================
# Integration Tests: BFHchart Service Layer
# ==============================================================================

test_that("compute_spc_results_bfh() includes Anhøj metadata in output", {
  skip_if_not_installed("BFHcharts")

  # Arrange
  data <- data.frame(
    month = 1:20,
    infections = c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                   5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
  )

  # Act
  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "infections",
    chart_type = "run"
  )

  # Assert
  expect_false(is.null(result))
  expect_true("metadata" %in% names(result))
  expect_true("anhoej_rules" %in% names(result$metadata))

  # Check Anhøj metadata structure
  anhoej <- result$metadata$anhoej_rules
  expect_true("runs_detected" %in% names(anhoej))
  expect_true("crossings_detected" %in% names(anhoej))
  expect_true("longest_run" %in% names(anhoej))
  expect_true("n_crossings" %in% names(anhoej))
  expect_true("n_crossings_min" %in% names(anhoej))
})

test_that("compute_spc_results_bfh() signal column matches BFHchart anhoej.signal", {
  skip_if_not_installed("BFHcharts")

  # Arrange
  data <- data.frame(
    month = 1:20,
    infections = c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                   5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
  )

  # Act
  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "infections",
    chart_type = "run"
  )

  # Assert: signal column should exist and be logical
  expect_true("signal" %in% names(result$qic_data))
  expect_type(result$qic_data$signal, "logical")
})

# ==============================================================================
# Validation Tests: qicharts2 Baseline Comparison
# ==============================================================================

test_that("Run chart: Anhøj rules match qicharts2 baseline - anhoej scenario", {
  skip_if_not_installed("BFHcharts")

  # Arrange
  baseline <- load_baseline("run", "anhoej")

  # Act
  result <- compute_spc_results_bfh(
    data = baseline$input_data,
    x_var = names(baseline$input_data)[1],
    y_var = names(baseline$input_data)[2],
    chart_type = "run"
  )

  # Assert: Anhøj metadata matches baseline
  expect_false(is.null(result$metadata$anhoej_rules))

  # Compare runs detection
  expect_equal(
    result$metadata$anhoej_rules$runs_detected,
    baseline$qic_output$anhoej_rules$runs_signal
  )

  # Compare crossings detection
  # Note: Baseline uses different naming
  expect_equal(
    result$metadata$anhoej_rules$n_crossings,
    baseline$qic_output$anhoej_rules$n_crossings
  )
})

test_that("Run chart: No Anhøj violations in basic scenario", {
  skip_if_not_installed("BFHcharts")

  # Arrange
  baseline <- load_baseline("run", "basic")

  # Act
  result <- compute_spc_results_bfh(
    data = baseline$input_data,
    x_var = names(baseline$input_data)[1],
    y_var = names(baseline$input_data)[2],
    chart_type = "run"
  )

  # Assert: Should have Anhøj metadata even if no violations
  expect_false(is.null(result$metadata$anhoej_rules))
})

test_that("I chart: Anhøj rules detected correctly", {
  skip_if_not_installed("BFHcharts")

  # Arrange
  baseline <- load_baseline("i", "anhoej")

  # Act
  result <- compute_spc_results_bfh(
    data = baseline$input_data,
    x_var = names(baseline$input_data)[1],
    y_var = names(baseline$input_data)[2],
    chart_type = "i"
  )

  # Assert: Anhøj metadata present
  expect_false(is.null(result$metadata$anhoej_rules))
  expect_true("runs_detected" %in% names(result$metadata$anhoej_rules))
})

test_that("P chart: Anhøj rules with denominator", {
  skip_if_not_installed("BFHcharts")

  # Arrange
  baseline <- load_baseline("p", "basic")

  # Act
  result <- compute_spc_results_bfh(
    data = baseline$input_data,
    x_var = names(baseline$input_data)[1],
    y_var = names(baseline$input_data)[2],
    n_var = names(baseline$input_data)[3],
    chart_type = "p"
  )

  # Assert
  expect_false(is.null(result$metadata$anhoej_rules))
})

test_that("C chart: Anhøj rules for count data", {
  skip_if_not_installed("BFHcharts")

  # Arrange
  baseline <- load_baseline("c", "basic")

  # Act
  result <- compute_spc_results_bfh(
    data = baseline$input_data,
    x_var = names(baseline$input_data)[1],
    y_var = names(baseline$input_data)[2],
    chart_type = "c"
  )

  # Assert
  expect_false(is.null(result$metadata$anhoej_rules))
})

test_that("U chart: Anhøj rules for rate data", {
  skip_if_not_installed("BFHcharts")

  # Arrange
  baseline <- load_baseline("u", "basic")

  # Act
  result <- compute_spc_results_bfh(
    data = baseline$input_data,
    x_var = names(baseline$input_data)[1],
    y_var = names(baseline$input_data)[2],
    n_var = names(baseline$input_data)[3],
    chart_type = "u"
  )

  # Assert
  expect_false(is.null(result$metadata$anhoej_rules))
})

# ==============================================================================
# Edge Case Tests
# ==============================================================================

test_that("Anhøj rules handle all points on center line", {
  # Arrange: All points exactly on center line
  qic_data <- tibble(
    x = 1:10,
    y = rep(10, 10),
    cl = 10,
    runs.signal = rep(FALSE, 10),
    n.crossings = 0,
    n.crossings.min = 5
  )

  # Act
  result <- extract_anhoej_metadata(qic_data)

  # Assert: Should detect crossings violation (0 crossings < 5 expected)
  expect_false(result$runs_signal)
  expect_true(result$crossings_signal)
})

test_that("Anhøj rules handle minimal data (3 points)", {
  skip_if_not_installed("BFHcharts")

  # Arrange: Minimal valid SPC data
  data <- data.frame(
    x = 1:3,
    y = c(10, 15, 20)
  )

  # Act
  result <- compute_spc_results_bfh(
    data = data,
    x_var = "x",
    y_var = "y",
    chart_type = "run"
  )

  # Assert: Should compute Anhøj even with minimal data
  expect_false(is.null(result$metadata$anhoej_rules))
})

test_that("Anhøj rules handle freeze period correctly", {
  skip_if_not_installed("BFHcharts")

  # Arrange
  baseline <- load_baseline("run", "freeze")

  # Act
  result <- compute_spc_results_bfh(
    data = baseline$input_data,
    x_var = names(baseline$input_data)[1],
    y_var = names(baseline$input_data)[2],
    chart_type = "run",
    freeze_var = if ("freeze" %in% names(baseline$input_data)) "freeze" else NULL
  )

  # Assert
  expect_false(is.null(result$metadata$anhoej_rules))
  # Freeze should not prevent Anhøj calculation
  expect_true(result$metadata$freeze_applied)
})

# ==============================================================================
# Performance Tests
# ==============================================================================

test_that("extract_anhoej_metadata() performs efficiently on large dataset", {
  # Arrange: Large dataset
  n <- 1000
  qic_data <- tibble(
    x = 1:n,
    y = rnorm(n, 100, 10),
    cl = 100,
    runs.signal = sample(c(TRUE, FALSE), n, replace = TRUE),
    n.crossings = 450,
    n.crossings.min = 400,
    longest.run = 12
  )

  # Act & Assert: Should complete in <100ms
  result <- system.time({
    metadata <- extract_anhoej_metadata(qic_data)
  })["elapsed"]

  expect_lt(result, 0.1)  # Less than 100ms
  expect_false(is.null(metadata))
})
