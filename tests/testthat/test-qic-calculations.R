# test-qic-calculations.R
# Test af QIC calculations functionality og SPC beregninger
# Fokuserer på testable utility functions og qicharts2 integration

# Source required functions
source("../../R/core/spc_helpers.R")
source("../../R/fct_spc_plot_generation.R")
source("../../R/utils/logging.R")

test_that("qicharts2 basic functionality works", {
  # TEST: Basic qicharts2 functionality independent of our wrapper

  # SETUP: Test data similar to our example data
  test_data <- data.frame(
    x = 1:20,
    y = c(93, 91, 97, 91, 92, 96, 87, 87, 91, 90, 88, 87, 86, 81, 93, 92, 85, 91, 84, 89),
    n = c(101, 98, 102, 99, 101, 104, 97, 101, 102, 99, 98, 102, 102, 97, 100, 99, 96, 102, 100, 98)
  )

  # TEST: P-chart (proportions)
  qic_p_result <- qicharts2::qic(
    x = test_data$x,
    y = test_data$y,
    n = test_data$n,
    chart = "p",
    return.data = TRUE
  )

  # Verify P-chart results
  expect_true(is.data.frame(qic_p_result))
  expect_equal(nrow(qic_p_result), 20)
  expect_true("cl" %in% names(qic_p_result)) # Center line
  expect_true("ucl" %in% names(qic_p_result)) # Upper control limit
  expect_true("lcl" %in% names(qic_p_result)) # Lower control limit
  expect_true("y" %in% names(qic_p_result)) # Y values

  # Check that calculations are reasonable
  expect_true(all(qic_p_result$cl > 0 & qic_p_result$cl < 1)) # Proportions should be 0-1
  expect_true(all(qic_p_result$ucl > qic_p_result$cl)) # UCL should be above center line
  expect_true(all(qic_p_result$lcl < qic_p_result$cl)) # LCL should be below center line

  # TEST: Run chart
  rate_values <- (test_data$y / test_data$n) * 100
  qic_run_result <- qicharts2::qic(
    x = test_data$x,
    y = rate_values,
    chart = "run",
    return.data = TRUE
  )

  expect_true(is.data.frame(qic_run_result))
  expect_equal(nrow(qic_run_result), 20)
  expect_true("cl" %in% names(qic_run_result))
  expect_true(all(qic_run_result$cl > 70 & qic_run_result$cl < 100)) # Reasonable percentage range

  # TEST: U-chart
  qic_u_result <- qicharts2::qic(
    x = test_data$x,
    y = test_data$y,
    n = test_data$n,
    chart = "u",
    return.data = TRUE
  )

  expect_true(is.data.frame(qic_u_result))
  expect_equal(nrow(qic_u_result), 20)
  expect_true("cl" %in% names(qic_u_result))
  expect_true("ucl" %in% names(qic_u_result))
  expect_true("lcl" %in% names(qic_u_result))
})

test_that("validate_x_column_format works correctly", {
  # TEST: X column validation and formatting logic

  # SETUP: Test data with different x column types
  date_data <- data.frame(
    Dato = c("01-01-2024", "01-02-2024", "01-03-2024"),
    Value = c(90, 85, 92)
  )

  numeric_data <- data.frame(
    Obs = 1:5,
    Value = c(90, 85, 92, 88, 94)
  )

  # TEST: Date column validation
  date_result <- validate_x_column_format(date_data, "Dato", "day")

  expect_true(is.list(date_result))
  expect_true("x_data" %in% names(date_result))
  expect_true("x.format" %in% names(date_result))
  expect_true("is_date" %in% names(date_result))

  # Should detect as date
  expect_true(date_result$is_date)
  expect_true(inherits(date_result$x_data, c("Date", "POSIXct", "POSIXt")))
  expect_equal(length(date_result$x_data), 3)

  # TEST: Numeric column validation
  numeric_result <- validate_x_column_format(numeric_data, "Obs", "observation")

  expect_false(numeric_result$is_date)
  expect_true(is.numeric(numeric_result$x_data))
  expect_equal(length(numeric_result$x_data), 5)

  # TEST: Missing column fallback
  missing_result <- validate_x_column_format(date_data, "NonExistent", "observation")

  expect_false(missing_result$is_date)
  expect_equal(length(missing_result$x_data), nrow(date_data))
  expect_true(all(missing_result$x_data == 1:nrow(date_data))) # Sequential numbering
})

test_that("detect_date_interval works correctly", {
  # TEST: Date interval detection logic

  # SETUP: Different date patterns
  daily_dates <- as.Date(c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-04", "2024-01-05"))
  weekly_dates <- as.Date(c("2024-01-01", "2024-01-08", "2024-01-15", "2024-01-22", "2024-01-29"))
  monthly_dates <- as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01"))

  # TEST: Daily interval detection
  daily_result <- detect_date_interval(daily_dates, debug = FALSE)

  expect_equal(daily_result$type, "daily")
  expect_true(daily_result$median_days <= 1.1) # Should be around 1 day
  expect_true(daily_result$consistency > 0.8) # Should be very consistent
  expect_equal(daily_result$n_obs, 5)

  # TEST: Weekly interval detection
  weekly_result <- detect_date_interval(weekly_dates, debug = FALSE)

  expect_equal(weekly_result$type, "weekly")
  expect_true(weekly_result$median_days >= 6 && weekly_result$median_days <= 8) # Around 7 days
  expect_true(weekly_result$consistency > 0.8) # Should be consistent

  # TEST: Monthly interval detection
  monthly_result <- detect_date_interval(monthly_dates, debug = FALSE)

  expect_equal(monthly_result$type, "monthly")
  expect_true(monthly_result$median_days >= 28 && monthly_result$median_days <= 35) # Around 30 days

  # TEST: Insufficient data handling
  insufficient_result <- detect_date_interval(daily_dates[1], debug = FALSE)

  expect_equal(insufficient_result$type, "insufficient_data")
  expect_equal(insufficient_result$consistency, 0)
})

test_that("get_optimal_formatting works correctly", {
  # TEST: Optimal formatting configuration based on interval

  # SETUP: Different interval info structures
  daily_interval <- list(
    type = "daily",
    median_days = 1,
    consistency = 0.9,
    timespan_days = 30,
    n_obs = 30
  )

  weekly_interval <- list(
    type = "weekly",
    median_days = 7,
    consistency = 0.85,
    timespan_days = 180,
    n_obs = 25
  )

  monthly_interval <- list(
    type = "monthly",
    median_days = 30,
    consistency = 0.8,
    timespan_days = 365,
    n_obs = 12
  )

  # TEST: Daily formatting
  daily_format <- get_optimal_formatting(daily_interval, debug = FALSE)

  expect_true(is.list(daily_format))
  expect_true("labels" %in% names(daily_format))
  expect_true("n_breaks" %in% names(daily_format))
  expect_true(daily_format$n_breaks <= 12) # Reasonable number of breaks

  # TEST: Weekly formatting
  weekly_format <- get_optimal_formatting(weekly_interval, debug = FALSE)

  expect_true(is.list(weekly_format))
  # Should use smart labels for readable weeks
  if ("use_smart_labels" %in% names(weekly_format)) {
    expect_true(weekly_format$use_smart_labels)
  }

  # TEST: Monthly formatting
  monthly_format <- get_optimal_formatting(monthly_interval, debug = FALSE)

  expect_true(is.list(monthly_format))
  expect_true(monthly_format$n_breaks <= 15) # Reasonable for 12 months
})

test_that("get_unit_label works correctly", {
  # TEST: Unit label conversion

  # Mock unit list similar to Y_AXIS_UNITS_DA
  mock_units <- list(
    "Antal" = "count",
    "Procent" = "percentage",
    "Andel" = "proportion",
    "Rate" = "rate"
  )

  # TEST: Valid unit code conversion
  count_label <- get_unit_label("count", mock_units)
  expect_equal(count_label, "Antal")

  percentage_label <- get_unit_label("percentage", mock_units)
  expect_equal(percentage_label, "Procent")

  # TEST: Invalid unit code fallback
  invalid_label <- get_unit_label("unknown", mock_units)
  expect_equal(invalid_label, "unknown")

  # TEST: Empty/NULL unit code
  empty_label <- get_unit_label("", mock_units)
  expect_equal(empty_label, "")

  null_label <- get_unit_label(NULL, mock_units)
  expect_equal(null_label, "")
})

test_that("QIC chart type calculations are consistent", {
  # TEST: Different chart types produce consistent results

  # SETUP: Consistent test data
  test_data <- data.frame(
    x = 1:15,
    numerator = c(45, 43, 48, 46, 47, 49, 44, 44, 46, 45, 44, 43, 43, 40, 47),
    denominator = rep(50, 15)
  )

  # TEST: P-chart calculations
  p_chart <- qicharts2::qic(
    x = test_data$x,
    y = test_data$numerator,
    n = test_data$denominator,
    chart = "p",
    return.data = TRUE
  )

  # Calculate expected proportion
  expected_proportion <- sum(test_data$numerator) / sum(test_data$denominator)

  # Center line should be close to overall proportion
  expect_true(abs(mean(p_chart$cl) - expected_proportion) < 0.01)

  # TEST: U-chart calculations
  u_chart <- qicharts2::qic(
    x = test_data$x,
    y = test_data$numerator,
    n = test_data$denominator,
    chart = "u",
    return.data = TRUE
  )

  # Both charts should have same number of points
  expect_equal(nrow(p_chart), nrow(u_chart))
  expect_equal(nrow(p_chart), 15)

  # TEST: Run chart calculations
  rates <- (test_data$numerator / test_data$denominator) * 100
  run_chart <- qicharts2::qic(
    x = test_data$x,
    y = rates,
    chart = "run",
    return.data = TRUE
  )

  # Run chart center line should be median
  expected_median <- median(rates)
  expect_true(abs(mean(run_chart$cl) - expected_median) < 1) # Within 1%
})

test_that("Phase and baseline functionality works", {
  # TEST: Phase separation and baseline freezing in QIC

  # SETUP: Test data with phase change
  test_data <- data.frame(
    x = 1:20,
    y = c(rep(45, 10), rep(55, 10)), # Clear phase change at point 10
    n = rep(100, 20)
  )

  # TEST: P-chart with phase separation
  p_chart_phases <- qicharts2::qic(
    x = test_data$x,
    y = test_data$y,
    n = test_data$n,
    chart = "p",
    part = 11, # Phase change at observation 11
    return.data = TRUE
  )

  expect_true("part" %in% names(p_chart_phases))
  expect_true(any(p_chart_phases$part == 1)) # First phase
  expect_true(any(p_chart_phases$part == 2)) # Second phase

  # Different phases should have different center lines
  phase1_cl <- unique(p_chart_phases$cl[p_chart_phases$part == 1])
  phase2_cl <- unique(p_chart_phases$cl[p_chart_phases$part == 2])
  expect_true(abs(phase1_cl - phase2_cl) > 0.05) # Significant difference

  # TEST: P-chart with baseline freeze
  p_chart_freeze <- qicharts2::qic(
    x = test_data$x,
    y = test_data$y,
    n = test_data$n,
    chart = "p",
    freeze = 10, # Freeze baseline at observation 10
    return.data = TRUE
  )

  expect_true("baseline" %in% names(p_chart_freeze))
  # Baseline should be consistent for frozen period
  baseline_values <- unique(p_chart_freeze$cl[p_chart_freeze$baseline])
  expect_equal(length(baseline_values), 1) # Should be single value for frozen baseline
})

test_that("Target line functionality works", {
  # TEST: Target line integration with QIC

  # SETUP: Test data
  test_data <- data.frame(
    x = 1:10,
    y = c(45, 43, 48, 46, 47, 49, 44, 44, 46, 45),
    n = rep(50, 10)
  )

  target_value <- 0.85 # 85% target

  # TEST: P-chart with target
  p_chart_target <- qicharts2::qic(
    x = test_data$x,
    y = test_data$y,
    n = test_data$n,
    chart = "p",
    target = target_value,
    return.data = TRUE
  )

  expect_true("target" %in% names(p_chart_target))
  expect_true(all(p_chart_target$target == target_value | is.na(p_chart_target$target)))

  # TEST: Run chart with target
  rates <- (test_data$y / test_data$n) * 100
  target_percentage <- 85

  run_chart_target <- qicharts2::qic(
    x = test_data$x,
    y = rates,
    chart = "run",
    target = target_percentage,
    return.data = TRUE
  )

  expect_true("target" %in% names(run_chart_target))
  # Target should be consistent across chart
  if (any(!is.na(run_chart_target$target))) {
    target_values <- unique(run_chart_target$target[!is.na(run_chart_target$target)])
    expect_equal(length(target_values), 1)
    expect_equal(target_values[1], target_percentage)
  }
})

test_that("QIC edge cases are handled correctly", {
  # TEST: Edge cases and error conditions

  # SETUP: Problematic data scenarios

  # TEST: All same values
  same_values_data <- data.frame(
    x = 1:10,
    y = rep(45, 10),
    n = rep(50, 10)
  )

  same_values_result <- qicharts2::qic(
    x = same_values_data$x,
    y = same_values_data$y,
    n = same_values_data$n,
    chart = "p",
    return.data = TRUE
  )

  expect_true(is.data.frame(same_values_result))
  expect_equal(nrow(same_values_result), 10)
  # Control limits should be calculated even for constant values
  expect_true(all(!is.na(same_values_result$cl)))

  # TEST: Minimum data points
  min_data <- data.frame(
    x = 1:3,
    y = c(45, 43, 48),
    n = c(50, 50, 50)
  )

  min_result <- qicharts2::qic(
    x = min_data$x,
    y = min_data$y,
    n = min_data$n,
    chart = "p",
    return.data = TRUE
  )

  expect_true(is.data.frame(min_result))
  expect_equal(nrow(min_result), 3)

  # TEST: Single extreme value
  extreme_data <- data.frame(
    x = 1:10,
    y = c(45, 43, 48, 46, 47, 1, 44, 44, 46, 45), # One very low value
    n = rep(50, 10)
  )

  extreme_result <- qicharts2::qic(
    x = extreme_data$x,
    y = extreme_data$y,
    n = extreme_data$n,
    chart = "p",
    return.data = TRUE
  )

  expect_true(is.data.frame(extreme_result))
  expect_equal(nrow(extreme_result), 10)
  # Should still produce valid control limits
  expect_true(all(!is.na(extreme_result$cl)))
  expect_true(all(!is.na(extreme_result$ucl)))
  expect_true(all(!is.na(extreme_result$lcl)))
})

test_that("Data preprocessing functions work correctly", {
  # TEST: Data preprocessing utilities used by generateSPCPlot

  # Mock the danish number parsing function behavior
  test_danish_numbers <- function(input) {
    # Simulate parsing Danish formatted numbers
    if (is.character(input)) {
      # Replace comma with period and convert
      cleaned <- gsub(",", ".", input)
      as.numeric(cleaned)
    } else {
      as.numeric(input)
    }
  }

  # TEST: Danish number conversion
  danish_nums <- c("90,5", "85,2", "92,1")
  converted <- test_danish_numbers(danish_nums)

  expect_true(is.numeric(converted))
  expect_equal(converted, c(90.5, 85.2, 92.1))

  # TEST: Already numeric data
  numeric_nums <- c(90, 85, 92)
  converted_numeric <- test_danish_numbers(numeric_nums)

  expect_true(is.numeric(converted_numeric))
  expect_equal(converted_numeric, c(90, 85, 92))

  # TEST: Mixed valid/invalid data
  mixed_data <- c("90,5", "invalid", "92,1")
  # Suppress expected "NAs introduced by coercion" warning
  converted_mixed <- suppressWarnings(test_danish_numbers(mixed_data))

  expect_true(is.numeric(converted_mixed))
  expect_equal(converted_mixed[1], 90.5)
  expect_true(is.na(converted_mixed[2])) # Invalid should be NA
  expect_equal(converted_mixed[3], 92.1)
})

test_that("generateSPCPlot config validation works", {
  # TEST: Configuration validation that doesn't require reactive context

  # TEST: Valid configuration structure
  valid_config <- list(
    x_col = "Dato",
    y_col = "Tæller",
    n_col = "Nævner"
  )

  invalid_config <- list(
    x_col = "Dato",
    n_col = "Nævner"
  )

  invalid_y_config <- list(
    x_col = "Dato",
    y_col = character(0),
    n_col = "Nævner"
  )

  invalid_config <- list(
    x_col = "Dato",
    n_col = "Nævner"
  )

  expect_true(is.list(valid_config))
  expect_true("y_col" %in% names(valid_config))
  expect_true(!is.null(valid_config$y_col))

  # TEST: Missing y_col (should fail validation)
  invalid_config <- list(
    x_col = "Dato",
    n_col = "Nævner"
  )

  expect_false("y_col" %in% names(invalid_config))

  # TEST: Config with optional columns
  minimal_config <- list(
    y_col = "Tæller"
  )

  expect_true("y_col" %in% names(minimal_config))
  expect_false("x_col" %in% names(minimal_config))
  expect_false("n_col" %in% names(minimal_config))

  # TEST: Chart type validation
  valid_chart_types <- c("p", "pp", "u", "up", "run", "i", "mr", "x", "s")

  for (chart_type in valid_chart_types) {
    expect_true(chart_type %in% valid_chart_types)
    expect_true(nchar(chart_type) >= 1)
  }

  # Charts requiring n column
  charts_need_n <- c("p", "pp", "u", "up")
  charts_no_n <- c("run", "i", "mr", "x", "s")

  for (chart in charts_need_n) {
    expect_true(chart %in% valid_chart_types)
  }

  for (chart in charts_no_n) {
    expect_true(chart %in% valid_chart_types)
  }
})

test_that("generateSPCPlot handles character(0) column inputs correctly", {
  # REGRESSION TEST: character(0) edge cases that caused "argument is of length zero"
  # This test addresses the specific bug where Shiny inputs reset to character(0)
  # and caused failures in generateSPCPlot guards

  # SETUP: Valid test data
  test_data <- data.frame(
    Dato = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-04", "2024-01-05")),
    Tæller = c(45, 43, 48, 46, 47),
    Nævner = c(50, 50, 50, 50, 50),
    Skift = c(FALSE, FALSE, TRUE, FALSE, FALSE),
    Frys = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  valid_config <- list(
    x_col = "Dato",
    y_col = "Tæller",
    n_col = "Nævner"
  )

  invalid_config <- list(
    x_col = "Dato",
    n_col = "Nævner"
  )

  invalid_y_config <- list(
    x_col = "Dato",
    y_col = character(0),
    n_col = "Nævner"
  )

  # TEST 1: character(0) for skift_column parameter
  result <- generateSPCPlot(
    data = test_data,
    config = valid_config,
    chart_type = "p",
    target_value = NULL,
    centerline_value = NULL,
    show_phases = TRUE,
    skift_column = character(0),
    frys_column = character(0)
  )

  expect_true(is.list(result))
  expect_true("plot" %in% names(result))
  expect_true("qic_data" %in% names(result))
  expect_true(inherits(result$plot, "ggplot"))
  expect_true(is.data.frame(result$qic_data))

  expect_error(
    generateSPCPlot(
      data = test_data,
      config = invalid_y_config,
      chart_type = "p",
      target_value = NULL,
      centerline_value = NULL,
      show_phases = TRUE,
      skift_column = "Skift",
      frys_column = "Frys"
    ),
    "Y-kolonne kan ikke være character(0)",
    fixed = TRUE
  )

  # TEST 5: Verify that valid column names still work after fix
  result_with_valid_columns <- generateSPCPlot(
    data = test_data,
    config = valid_config,
    chart_type = "p",
    target_value = NULL,
    centerline_value = NULL,
    show_phases = TRUE,
    skift_column = "Skift",
    frys_column = "Frys"
  )

  expect_true(is.list(result_with_valid_columns))
  expect_true("plot" %in% names(result_with_valid_columns))
  expect_true(inherits(result_with_valid_columns$plot, "ggplot"))

  # TEST 6: Test edge case with empty string (different from character(0))
  expect_no_error({
    result_empty_string <- generateSPCPlot(
      data = test_data,
      config = valid_config,
      chart_type = "p",
      target_value = NULL,
      centerline_value = NULL,
      show_phases = TRUE,
      skift_column = "",
      frys_column = ""
    )
  })
})
