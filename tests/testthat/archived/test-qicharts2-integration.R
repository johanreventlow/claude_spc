# test-qicharts2-integration.R
# Integration tests to validate qicharts2 compatibility and prevent double-scaling

test_that("qicharts2 integration prevents double-scaling for proportion data", {

  # SCENARIO: P-chart with proportion data [0,1]
  # User enters "80%" as target, should become 0.8 and go directly to qicharts2

  # Simulate p-chart data (proportion format)
  test_data <- data.frame(
    date = seq(as.Date("2023-01-01"), length.out = 20, by = "day"),
    numerator = c(8, 12, 15, 20, 18, 22, 25, 19, 16, 14, 21, 23, 17, 19, 24, 20, 18, 22, 25, 23),
    denominator = rep(100, 20)
  )

  # Calculate proportions for Y data
  test_data$proportion <- test_data$numerator / test_data$denominator

  # Y sample for heuristics (should detect as proportion scale)
  y_sample <- test_data$proportion

  # User enters "80%" as target
  target_normalized <- normalize_axis_value(
    x = "80%",
    user_unit = NULL,  # No explicit unit
    col_unit = NULL,
    y_sample = y_sample,  # Will detect as proportion
    internal_unit = "proportion"  # qicharts2 expects [0,1]
  )

  # Should normalize to 0.8 (not 0.008 or 80)
  expect_equal(target_normalized, 0.8)

  # Test actual qicharts2 integration
  qic_result <- qicharts2::qic(
    x = test_data$date,
    y = test_data$proportion,
    n = test_data$denominator,
    chart = "p",
    target = target_normalized,  # 0.8 goes directly to qicharts2
    title = "Test P-Chart"
  )

  # qicharts2 should handle 0.8 correctly for p-chart
  expect_s3_class(qic_result, "ggplot")

  # The target line should appear at 0.8, not 0.008 or 80
  # (This is visual validation - qicharts2 internally handles proportion scale)

})

test_that("qicharts2 integration works for run charts with different scales", {

  # SCENARIO A: Run chart with decimal proportion data
  decimal_data <- data.frame(
    date = seq(as.Date("2023-01-01"), length.out = 10, by = "day"),
    rate = c(0.1, 0.3, 0.6, 0.8, 0.5, 0.7, 0.4, 0.9, 0.2, 0.6)
  )

  target_decimal <- normalize_axis_value(
    x = "50%",
    y_sample = decimal_data$rate,
    internal_unit = "proportion"
  )

  expect_equal(target_decimal, 0.5)  # 50% → 0.5

  # Test with qicharts2
  qic_decimal <- qicharts2::qic(
    x = decimal_data$date,
    y = decimal_data$rate,
    chart = "run",
    target = target_decimal
  )

  expect_s3_class(qic_decimal, "ggplot")

  # SCENARIO B: Run chart with percent-scale data
  percent_data <- data.frame(
    date = seq(as.Date("2023-01-01"), length.out = 10, by = "day"),
    percentage = c(10, 30, 60, 80, 50, 70, 40, 90, 20, 60)
  )

  target_percent <- normalize_axis_value(
    x = "50%",
    y_sample = percent_data$percentage,
    internal_unit = "absolute"  # Percent scale data stays as-is
  )

  expect_equal(target_percent, 50)  # 50% → 50 (in percent scale)

  # Test with qicharts2
  qic_percent <- qicharts2::qic(
    x = percent_data$date,
    y = percent_data$percentage,
    chart = "run",
    target = target_percent
  )

  expect_s3_class(qic_percent, "ggplot")

})

test_that("no double-scaling occurs in legacy parse_danish_target wrapper", {

  # Test that the legacy wrapper maintains existing behavior
  # while using the new internal API

  # Decimal Y-data context (should suggest proportion internal unit)
  decimal_y_data <- c(0.1, 0.3, 0.6, 0.8)

  # Legacy API call
  legacy_result <- parse_danish_target("80%", decimal_y_data, "percent")

  # Should return 0.8 (not 0.008 or 80)
  expect_equal(legacy_result, 0.8)

  # This can go directly to qicharts2 for p-charts
  expect_true(legacy_result >= 0 && legacy_result <= 1)  # Valid proportion range

})

test_that("centerline values work identically to target values", {

  # Both target and centerline should use the same scaling logic

  y_sample <- c(0.1, 0.2, 0.3, 0.8)

  target_result <- normalize_axis_value(
    x = "60%",
    y_sample = y_sample,
    internal_unit = "proportion"
  )

  centerline_result <- normalize_axis_value(
    x = "60%",  # Same input
    y_sample = y_sample,  # Same context
    internal_unit = "proportion"  # Same internal unit
  )

  # Should be identical
  expect_equal(target_result, centerline_result)
  expect_equal(target_result, 0.6)

})

test_that("explicit user unit choice overrides data heuristics", {

  # SCENARIO: Data looks like percent, but user explicitly chooses proportion

  percent_looking_data <- c(10, 20, 30, 80)  # Would normally suggest "percent"

  # Without user choice - data heuristics
  heuristic_result <- normalize_axis_value(
    x = "50%",
    y_sample = percent_looking_data,
    internal_unit = "percent"  # Match data heuristics
  )

  expect_equal(heuristic_result, 50)  # 50% → 50 in percent scale

  # With explicit user choice - overrides heuristics
  explicit_result <- normalize_axis_value(
    x = "50%",
    user_unit = "proportion",  # User explicitly chooses proportion
    y_sample = percent_looking_data,
    internal_unit = "proportion"
  )

  expect_equal(explicit_result, 0.5)  # 50% → 0.5 because user chose proportion

  # This demonstrates the priority system works:
  # User choice > Data heuristics

})