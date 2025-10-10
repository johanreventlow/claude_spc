# test-y-axis-formatting.R
# Tests for Y-axis formatting utilities
#
# FASE 4 Task 4.2: Y-axis formatting extraction tests
# Tests the consolidated Y-axis formatting functions from utils_y_axis_formatting.R

library(testthat)
library(ggplot2)

# TEST: apply_y_axis_formatting() ============================================

test_that("apply_y_axis_formatting handles all unit types", {
  # Setup: Create base plot
  test_data <- data.frame(x = 1:10, y = seq(0, 100, length.out = 10))
  base_plot <- ggplot(test_data, aes(x = x, y = y)) + geom_point()

  # Test percent formatting
  plot_percent <- apply_y_axis_formatting(base_plot, "percent", test_data)
  expect_s3_class(plot_percent, "ggplot")
  expect_true(length(plot_percent$layers) > 0)

  # Test count formatting
  plot_count <- apply_y_axis_formatting(base_plot, "count", test_data)
  expect_s3_class(plot_count, "ggplot")

  # Test rate formatting
  plot_rate <- apply_y_axis_formatting(base_plot, "rate", test_data)
  expect_s3_class(plot_rate, "ggplot")

  # Test time formatting (requires qic_data structure)
  qic_data <- data.frame(x = 1:10, y = seq(0, 120, length.out = 10))
  plot_time <- apply_y_axis_formatting(base_plot, "time", qic_data)
  expect_s3_class(plot_time, "ggplot")
})

test_that("apply_y_axis_formatting handles invalid inputs gracefully", {
  test_data <- data.frame(x = 1:10, y = 1:10)
  base_plot <- ggplot(test_data, aes(x = x, y = y)) + geom_point()

  # Test NULL y_axis_unit (should default to "count")
  plot_null <- apply_y_axis_formatting(base_plot, NULL, test_data)
  expect_s3_class(plot_null, "ggplot")

  # Test invalid y_axis_unit (should default to "count")
  plot_invalid <- apply_y_axis_formatting(base_plot, "invalid_unit", test_data)
  expect_s3_class(plot_invalid, "ggplot")

  # Test non-ggplot object (should return input unchanged)
  not_a_plot <- list(data = test_data)
  result <- apply_y_axis_formatting(not_a_plot, "percent", test_data)
  expect_equal(result, not_a_plot)
})

# TEST: format_scaled_number() ===============================================

test_that("format_scaled_number formats correctly with Danish notation", {
  # Integer values (no decimals)
  expect_equal(format_scaled_number(1000, 1e3, "K"), "1K")
  expect_equal(format_scaled_number(5000, 1e3, "K"), "5K")
  expect_equal(format_scaled_number(1000000, 1e6, "M"), "1M")
  expect_equal(format_scaled_number(1000000000, 1e9, " mia."), "1 mia.")

  # Decimal values (Danish decimal mark ",")
  expect_equal(format_scaled_number(1500, 1e3, "K"), "1,5K")
  expect_equal(format_scaled_number(2750, 1e3, "K"), "2,8K")
  expect_equal(format_scaled_number(1250000, 1e6, "M"), "1,2M")
  expect_equal(format_scaled_number(1500000000, 1e9, " mia."), "1,5 mia.")
})

# TEST: format_unscaled_number() =============================================

test_that("format_unscaled_number uses Danish notation", {
  # Integer values (with thousand separator ".")
  expect_equal(format_unscaled_number(100), "100")
  expect_equal(format_unscaled_number(1000), "1.000")
  expect_equal(format_unscaled_number(10000), "10.000")
  expect_equal(format_unscaled_number(100000), "100.000")

  # Decimal values (decimal mark "," and thousand separator ".")
  expect_equal(format_unscaled_number(100.5), "100,5")
  expect_equal(format_unscaled_number(1000.75), "1.000,8")
})

# TEST: format_time_with_unit() ==============================================

test_that("format_time_with_unit consolidates duplication correctly", {
  # Minutes formatting
  expect_equal(format_time_with_unit(30, "minutes"), "30 min")
  expect_equal(format_time_with_unit(45.5, "minutes"), "45,5 min")
  expect_equal(format_time_with_unit(59, "minutes"), "59 min")

  # Hours formatting
  expect_equal(format_time_with_unit(60, "hours"), "1 timer")
  expect_equal(format_time_with_unit(90, "hours"), "1,5 timer")
  expect_equal(format_time_with_unit(120, "hours"), "2 timer")
  expect_equal(format_time_with_unit(150, "hours"), "2,5 timer")

  # Days formatting
  expect_equal(format_time_with_unit(1440, "days"), "1 dage")
  expect_equal(format_time_with_unit(2160, "days"), "1,5 dage")
  expect_equal(format_time_with_unit(2880, "days"), "2 dage")
  expect_equal(format_time_with_unit(4320, "days"), "3 dage")
})

test_that("format_time_with_unit handles NA values", {
  expect_true(is.na(format_time_with_unit(NA, "minutes")))
  expect_true(is.na(format_time_with_unit(NA, "hours")))
  expect_true(is.na(format_time_with_unit(NA, "days")))
})

test_that("format_time_with_unit handles edge cases", {
  # Zero values
  expect_equal(format_time_with_unit(0, "minutes"), "0 min")
  expect_equal(format_time_with_unit(0, "hours"), "0 timer")
  expect_equal(format_time_with_unit(0, "days"), "0 dage")

  # Very small decimals
  expect_match(format_time_with_unit(0.1, "minutes"), "^0,1 min$")

  # Large values
  expect_equal(format_time_with_unit(10000, "days"), "6,9 dage")
})

# TEST: format_y_axis_time() =================================================

test_that("format_y_axis_time selects correct unit based on data range", {
  # Minutes range (< 60)
  qic_data_minutes <- data.frame(x = 1:10, y = seq(1, 50, length.out = 10))
  scale_minutes <- format_y_axis_time(qic_data_minutes)
  expect_s3_class(scale_minutes, "ScaleContinuous")

  # Hours range (60-1439)
  qic_data_hours <- data.frame(x = 1:10, y = seq(60, 600, length.out = 10))
  scale_hours <- format_y_axis_time(qic_data_hours)
  expect_s3_class(scale_hours, "ScaleContinuous")

  # Days range (>= 1440)
  qic_data_days <- data.frame(x = 1:10, y = seq(1440, 5000, length.out = 10))
  scale_days <- format_y_axis_time(qic_data_days)
  expect_s3_class(scale_days, "ScaleContinuous")
})

test_that("format_y_axis_time handles missing or invalid data", {
  # NULL qic_data
  scale_null <- format_y_axis_time(NULL)
  expect_s3_class(scale_null, "ScaleContinuous")

  # Missing y column
  qic_data_no_y <- data.frame(x = 1:10, z = 1:10)
  scale_no_y <- format_y_axis_time(qic_data_no_y)
  expect_s3_class(scale_no_y, "ScaleContinuous")
})

# TEST: Integration with ggplot2 =============================================

test_that("Y-axis formatting integrates correctly with ggplot2", {
  # Create test data
  test_data <- data.frame(
    x = 1:20,
    y = c(50, 75, 100, 125, 150, 175, 200, 225, 250, 275,
          300, 325, 350, 375, 400, 425, 450, 475, 500, 525)
  )

  # Create base plot
  base_plot <- ggplot(test_data, aes(x = x, y = y)) +
    geom_line() +
    geom_point()

  # Apply formatting
  formatted_plot <- apply_y_axis_formatting(base_plot, "count", test_data)

  # Verify plot builds without errors
  expect_s3_class(formatted_plot, "ggplot")
  expect_error(ggplot_build(formatted_plot), NA)

  # Verify y-axis scale was added
  scales <- formatted_plot$scales$scales
  has_y_scale <- any(sapply(scales, function(s) "y" %in% s$aesthetics))
  expect_true(has_y_scale)
})

# TEST: Backward Compatibility ===============================================

test_that("Extracted formatting produces identical output to original", {
  # This test ensures the extraction doesn't change behavior
  # We test that the formatting functions produce the same output

  # Test count formatting with K notation
  test_val_k <- 5000
  expect_equal(format_scaled_number(test_val_k, 1e3, "K"), "5K")

  # Test count formatting with M notation
  test_val_m <- 2500000
  expect_equal(format_scaled_number(test_val_m, 1e6, "M"), "2,5M")

  # Test time formatting minutes
  expect_equal(format_time_with_unit(45, "minutes"), "45 min")

  # Test time formatting hours
  expect_equal(format_time_with_unit(90, "hours"), "1,5 timer")
})

# TEST: DRY Principle Validation ============================================

test_that("format_time_with_unit eliminates duplication effectively", {
  # This test validates that the single function replaces 3 duplicated blocks
  # All three time units should use the same formatting logic

  # Test consistent decimal handling across units
  expect_match(format_time_with_unit(30.5, "minutes"), ",")
  expect_match(format_time_with_unit(90, "hours"), ",")
  expect_match(format_time_with_unit(2160, "days"), ",")

  # Test consistent integer handling across units
  expect_equal(format_time_with_unit(30, "minutes"), "30 min")
  expect_equal(format_time_with_unit(120, "hours"), "2 timer")
  expect_equal(format_time_with_unit(2880, "days"), "2 dage")
})
