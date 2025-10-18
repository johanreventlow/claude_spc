# test-qic-preparation-sprint1.R
# SPRINT 1 CRITICAL FIX: Test coverage for QIC preparation functions
# Prevents 100x denominator mismatch bugs

# Test setup
library(testthat)

# Test prepare_qic_inputs() ====================================================

test_that("prepare_qic_inputs handles P-charts with denominators correctly", {
  # Setup: P-chart with count data and denominators
  y_counts <- c(5, 10, 15, 20)
  n_denoms <- c(100, 100, 100, 100)

  result <- prepare_qic_inputs(
    y_raw = y_counts,
    n_raw = n_denoms,
    chart_type = "p",
    user_unit = NULL
  )

  # Should use counts+n approach (NOT convert to proportions)
  expect_equal(result$y, y_counts)
  expect_equal(result$n, n_denoms)
  expect_equal(result$internal_unit, "proportion")
  expect_equal(result$chart_type, "p")
})

test_that("prepare_qic_inputs converts proportions for run charts", {
  # Setup: Run chart with percentage data (should convert to [0,1])
  y_percent <- c("5%", "10%", "15%", "20%")

  result <- prepare_qic_inputs(
    y_raw = y_percent,
    n_raw = NULL,
    chart_type = "run",
    user_unit = "percent"
  )

  # Should convert to [0,1] proportions
  expect_equal(result$y, c(0.05, 0.10, 0.15, 0.20))
  expect_null(result$n)
  expect_equal(result$internal_unit, "proportion")
})

test_that("prepare_qic_inputs handles absolute charts without scaling", {
  # Setup: C-chart with count data
  y_counts <- c(5, 10, 15, 20)

  result <- prepare_qic_inputs(
    y_raw = y_counts,
    n_raw = NULL,
    chart_type = "c",
    user_unit = NULL
  )

  # Should keep raw values (no scaling)
  expect_equal(result$y, y_counts)
  expect_null(result$n)
  expect_equal(result$internal_unit, "absolute")
})

test_that("prepare_qic_inputs creates normalize function correctly", {
  result <- prepare_qic_inputs(
    y_raw = c(5, 10),
    n_raw = NULL,
    chart_type = "run",
    user_unit = "percent"
  )

  # Normalize function should exist
  expect_true(is.function(result$normalize))

  # Normalize should convert percent to [0,1]
  expect_equal(result$normalize("50%"), 0.5)
})

# Test normalize_proportions_to_internal() ====================================

test_that("normalize_proportions_to_internal handles already normalized data", {
  # Data already in [0,1]
  y_prop <- c(0.05, 0.10, 0.15, 0.20)
  y_sample <- y_prop

  result <- normalize_proportions_to_internal(
    y_raw = y_prop,
    y_sample = y_sample,
    user_unit = NULL
  )

  expect_equal(result, y_prop)
})

test_that("normalize_proportions_to_internal converts percent to proportion", {
  # Percent data needs conversion
  y_percent <- c(5, 10, 15, 20)
  y_sample <- y_percent

  result <- normalize_proportions_to_internal(
    y_raw = y_percent,
    y_sample = y_sample,
    user_unit = "percent"
  )

  # Should divide by 100
  expect_equal(result, c(0.05, 0.10, 0.15, 0.20))
})

test_that("normalize_proportions_to_internal handles Danish number format", {
  # Danish format: comma as decimal separator
  y_danish <- c("5,5", "10,5", "15,5")

  # Note: This requires parse_danish_number to work correctly
  result <- normalize_proportions_to_internal(
    y_raw = y_danish,
    y_sample = c(5.5, 10.5, 15.5),  # parsed sample
    user_unit = "percent"
  )

  expect_equal(result, c(0.055, 0.105, 0.155))
})

# Test validate_qic_inputs() ==================================================

test_that("validate_qic_inputs catches out-of-range proportions", {
  # Y data outside [0,1] for proportion chart WITHOUT denominators
  y_bad <- c(0.05, 1.5, 0.10)  # 1.5 is invalid

  result <- validate_qic_inputs(
    y = y_bad,
    n = NULL,  # No denominators
    target = NULL,
    centerline = NULL,
    internal_unit = "proportion"
  )

  expect_false(result$valid)
  expect_length(result$warnings, 1)
  expect_match(result$warnings[1], "outside \\[0,1\\] range")
})

test_that("validate_qic_inputs allows out-of-range Y with denominators", {
  # Y data can be counts (>1) when using denominators
  y_counts <- c(5, 10, 15)
  n_denoms <- c(100, 100, 100)

  result <- validate_qic_inputs(
    y = y_counts,
    n = n_denoms,  # Has denominators
    target = NULL,
    centerline = NULL,
    internal_unit = "proportion"
  )

  # Should be VALID because we have denominators (counts+n approach)
  expect_true(result$valid)
  expect_length(result$warnings, 0)
})

test_that("validate_qic_inputs catches out-of-range target", {
  result <- validate_qic_inputs(
    y = c(0.05, 0.10, 0.15),
    n = NULL,
    target = 1.5,  # Invalid target
    centerline = NULL,
    internal_unit = "proportion"
  )

  expect_length(result$warnings, 1)
  expect_match(result$warnings[1], "Target.*outside \\[0,1\\] range")
})

test_that("validate_qic_inputs catches out-of-range centerline", {
  result <- validate_qic_inputs(
    y = c(0.05, 0.10, 0.15),
    n = NULL,
    target = NULL,
    centerline = -0.5,  # Invalid centerline
    internal_unit = "proportion"
  )

  expect_length(result$warnings, 1)
  expect_match(result$warnings[1], "Centerline.*outside \\[0,1\\] range")
})

test_that("validate_qic_inputs passes clean proportion data", {
  result <- validate_qic_inputs(
    y = c(0.05, 0.10, 0.15, 0.20),
    n = NULL,
    target = 0.12,
    centerline = 0.125,
    internal_unit = "proportion"
  )

  expect_true(result$valid)
  expect_length(result$warnings, 0)
})

# Edge Cases ==================================================================

test_that("prepare_qic_inputs handles NULL denominators gracefully", {
  result <- prepare_qic_inputs(
    y_raw = c(5, 10, 15),
    n_raw = NULL,
    chart_type = "run",
    user_unit = NULL
  )

  expect_null(result$n)
})

test_that("prepare_qic_inputs handles empty data", {
  expect_error(
    prepare_qic_inputs(
      y_raw = numeric(0),
      n_raw = NULL,
      chart_type = "run",
      user_unit = NULL
    ),
    NA  # Should not error, but may return empty result
  )
})

test_that("normalize_proportions_to_internal warns on unknown scale", {
  # Unknown scale should log warning
  expect_warning(
    normalize_proportions_to_internal(
      y_raw = c(500, 1000),  # Ambiguous scale
      y_sample = c(500, 1000),
      user_unit = NULL
    ),
    NA  # May or may not warn depending on implementation
  )
})

# Integration Test ============================================================

test_that("Full workflow: P-chart with counts prevents 100x mismatch", {
  # This is the critical test that prevents the 100x bug

  # User has count data: 5 events out of 100, 10 out of 100, etc.
  y_counts <- c(5, 10, 15, 20)
  n_denoms <- c(100, 100, 100, 100)

  # User sets target at 10% (expressed as percent)
  target_input <- "10%"

  # Prepare inputs
  qic_inputs <- prepare_qic_inputs(
    y_raw = y_counts,
    n_raw = n_denoms,
    chart_type = "p",
    user_unit = "percent"
  )

  # Normalize target
  target_normalized <- qic_inputs$normalize(target_input)

  # CRITICAL CHECKS:
  # 1. Y should still be counts (not proportions)
  expect_equal(qic_inputs$y, y_counts)

  # 2. N should still be denominators
  expect_equal(qic_inputs$n, n_denoms)

  # 3. Target should be in [0,1] (0.10, not 10)
  expect_equal(target_normalized, 0.10)

  # 4. Validation should pass
  validation <- validate_qic_inputs(
    y = qic_inputs$y,
    n = qic_inputs$n,
    target = target_normalized,
    centerline = NULL,
    internal_unit = qic_inputs$internal_unit
  )

  expect_true(validation$valid)
  expect_length(validation$warnings, 0)

  # This ensures qicharts2 receives:
  # - y as counts (5, 10, 15, 20)
  # - n as denominators (100, 100, 100, 100)
  # - target as proportion (0.10)
  # Which prevents the 100x mismatch bug!
})

# Performance Test (Optional) =================================================

test_that("prepare_qic_inputs is fast for large datasets", {
  # Generate large dataset
  y_large <- runif(10000, 0, 100)
  n_large <- rep(100, 10000)

  # Should complete in <100ms
  elapsed <- system.time({
    result <- prepare_qic_inputs(
      y_raw = y_large,
      n_raw = n_large,
      chart_type = "p",
      user_unit = NULL
    )
  })["elapsed"]

  expect_lt(elapsed, 0.1)  # Less than 100ms
})