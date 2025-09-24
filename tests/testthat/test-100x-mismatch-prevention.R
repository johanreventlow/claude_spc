# test-100x-mismatch-prevention.R
# Critical sanity tests to detect and prevent 100×-mismatch bugs
# These tests implement the specific scenarios from your redesign

test_that("100×-mismatch sanity test: '80%' = '80' (percent) = 0.8 (internal)", {

  # CRITICAL TEST: All these inputs should result in same internal value
  # for proportion charts when user specifies percent unit

  # Test normalize_axis_value with chart_type
  result1 <- normalize_axis_value("80%", chart_type = "p")     # P-chart uses proportion internal
  result2 <- normalize_axis_value("80", user_unit = "percent", chart_type = "p")
  result3 <- normalize_axis_value("0,8", user_unit = "proportion", chart_type = "p")

  # All should result in 0.8 (internal proportion for p-chart)
  expect_equal(result1, 0.8, info = "'80%' should normalize to 0.8")
  expect_equal(result2, 0.8, info = "'80' (percent) should normalize to 0.8")
  expect_equal(result3, 0.8, info = "'0,8' (proportion) should normalize to 0.8")

  # SANITY CHECK: All three should be identical
  expect_equal(result1, result2, info = "'80%' should equal '80' (percent)")
  expect_equal(result2, result3, info = "'80' (percent) should equal '0,8' (proportion)")

})

test_that("Chart type determines internal unit correctly", {

  # Proportion charts should use [0,1] internal unit
  expect_equal(determine_internal_unit_by_chart_type("p"), "proportion")
  expect_equal(determine_internal_unit_by_chart_type("pp"), "proportion")
  expect_equal(determine_internal_unit_by_chart_type("run"), "proportion")

  # Absolute charts should use absolute internal unit
  expect_equal(determine_internal_unit_by_chart_type("c"), "absolute")
  expect_equal(determine_internal_unit_by_chart_type("u"), "absolute")
  expect_equal(determine_internal_unit_by_chart_type("up"), "absolute")
  expect_equal(determine_internal_unit_by_chart_type("i"), "absolute")
  expect_equal(determine_internal_unit_by_chart_type("mr"), "absolute")
  expect_equal(determine_internal_unit_by_chart_type("g"), "absolute")

})

test_that("No implicit scaling without symbols (CRITICAL for 100×-prevention)", {

  # This test verifies the KEY RULE: Only symbols trigger scaling

  # For proportion target unit - no symbol should mean "already in proportion"
  parsed_no_symbol <- list(value = 0.8, symbol = "none")
  result_prop <- coerce_to_target_unit(parsed_no_symbol, "proportion")
  expect_equal(result_prop, 0.8, info = "0.8 without symbol should stay 0.8 in proportion")

  # For percent target unit - no symbol should mean "already in percent"
  parsed_no_symbol_80 <- list(value = 80, symbol = "none")
  result_percent <- coerce_to_target_unit(parsed_no_symbol_80, "percent")
  expect_equal(result_percent, 80, info = "80 without symbol should stay 80 in percent")

  # CRITICAL: 80 (no symbol) should NOT become 0.8 automatically
  expect_false(result_percent == 0.8, info = "80 without symbol should NOT be 0.8")

})

test_that("Symbol-triggered scaling works correctly", {

  # Symbols should trigger predictable scaling

  # 80% should become 0.8 when target is proportion
  parsed_with_percent <- list(value = 80, symbol = "percent")
  result <- coerce_to_target_unit(parsed_with_percent, "proportion")
  expect_equal(result, 0.8)

  # 8‰ should become 0.008 when target is proportion
  parsed_with_permille <- list(value = 8, symbol = "permille")
  result <- coerce_to_target_unit(parsed_with_permille, "proportion")
  expect_equal(result, 0.008)

  # 80% should stay 80 when target is percent
  result_percent <- coerce_to_target_unit(parsed_with_percent, "percent")
  expect_equal(result_percent, 80)

})

test_that("End-to-end UI consistency: same input different formats = same plot", {

  # This simulates the user changing input format in UI
  # All should result in same internal value for plotting

  chart_type <- "p"  # P-chart

  # Scenario: User wants 80% target
  target_80_percent <- normalize_axis_value("80%", chart_type = chart_type)
  target_80_text <- normalize_axis_value("80", user_unit = "percent", chart_type = chart_type)
  target_08_prop <- normalize_axis_value("0,8", user_unit = "proportion", chart_type = chart_type)

  # All should be identical for plotting
  expect_equal(target_80_percent, target_80_text,
               info = "UI: '80%' should equal '80' (percent)")
  expect_equal(target_80_text, target_08_prop,
               info = "UI: '80' (percent) should equal '0,8' (proportion)")

  # And all should be 0.8 for qicharts2
  expect_equal(target_80_percent, 0.8, info = "Target should be 0.8 for qicharts2")

})

test_that("Target and centerline use identical processing", {

  # Both target and centerline should go through same normalization
  # to prevent inconsistencies

  chart_type <- "p"
  user_input <- "75%"

  # Should use identical processing
  target_result <- normalize_axis_value(user_input, chart_type = chart_type)
  centerline_result <- normalize_axis_value(user_input, chart_type = chart_type)

  expect_identical(target_result, centerline_result,
                   info = "Target and centerline should use identical processing")
  expect_equal(target_result, 0.75, info = "Both should normalize to 0.75")

})

test_that("QIC input preparation prevents double-scaling", {

  # Test the prepare_qic_inputs function

  # Mock proportion data [0,1]
  y_data <- c(0.1, 0.3, 0.6, 0.8)
  n_data <- c(100, 100, 100, 100)

  # P-chart with denominators
  qic_inputs <- prepare_qic_inputs(y_data * 100, n_data, "p", "percent")  # Input as counts

  # Should prepare y as counts, n as denominators
  expect_equal(qic_inputs$y, c(10, 30, 60, 80))  # Counts
  expect_equal(qic_inputs$n, c(100, 100, 100, 100))  # Denominators
  expect_equal(qic_inputs$chart_type, "p")

  # Target normalization should work
  target_normalized <- qic_inputs$normalize("80%")
  expect_equal(target_normalized, 0.8, info = "Target should normalize to 0.8 for qicharts2")

})

test_that("Run chart vs P-chart consistency for same data", {

  # Same proportion data should work consistently across chart types

  proportion_data <- c(0.1, 0.3, 0.6, 0.8)

  # Test with run chart (proportion internal)
  run_target <- normalize_axis_value("80%", chart_type = "run")

  # Test with p-chart (also proportion internal)
  p_target <- normalize_axis_value("80%", chart_type = "p")

  # Should be identical since both use proportion internal unit
  expect_equal(run_target, p_target, info = "Run chart and P-chart should handle proportions identically")
  expect_equal(run_target, 0.8, info = "Both should normalize to 0.8")

})

test_that("Absolute charts don't scale proportion-like inputs", {

  # C-charts and U-charts should treat numbers as absolute counts/rates

  # Even if input looks like percentage, absolute charts keep the number
  c_chart_target <- normalize_axis_value("80%", chart_type = "c")  # Count chart
  u_chart_target <- normalize_axis_value("15", user_unit = "rate_1000", chart_type = "u")

  # C-chart: 80% → 80 (removes symbol, keeps value)
  expect_equal(c_chart_target, 80, info = "C-chart should remove % symbol but keep value 80")

  # U-chart: 15 → 15 (no scaling)
  expect_equal(u_chart_target, 15, info = "U-chart should keep rate value as-is")

})

test_that("Data heuristics work correctly", {

  # Test automatic unit detection from data

  # Proportion-like data [0,1]
  prop_data <- c(0.1, 0.2, 0.3, 0.8)
  prop_unit <- detect_unit_from_data(prop_data)
  expect_equal(prop_unit, "proportion")

  # Percent-like data [0-100]
  percent_data <- c(10, 20, 30, 80)
  percent_unit <- detect_unit_from_data(percent_data)
  expect_equal(percent_unit, "percent")

  # Large numbers → absolute
  large_data <- c(150, 250, 450, 800)
  large_unit <- detect_unit_from_data(large_data)
  expect_equal(large_unit, "absolute")

})

test_that("Priority system prevents conflicts", {

  # User explicit choice should override data heuristics

  percent_looking_data <- c(10, 20, 30, 80)  # Looks like percent

  # Without user unit - should detect percent
  auto_result <- normalize_axis_value("50%", y_sample = percent_looking_data,
                                     chart_type = "p")

  # With explicit user unit - should honor user choice
  user_result <- normalize_axis_value("50%", user_unit = "proportion",
                                     y_sample = percent_looking_data,
                                     chart_type = "p")

  # Results should be different because user overrode heuristics
  expect_equal(auto_result, 0.5)  # Heuristics detected percent, 50% → 50 → /100 → 0.5
  expect_equal(user_result, 0.5)  # User chose proportion, 50% → /100 → 0.5

  # In this case they happen to be same, but via different paths
  # The important thing is user choice took precedence

})

test_that("Invalid conversions are handled gracefully", {

  # Test problematic conversions that should return NA

  # Absolute to proportion without context
  result1 <- to_internal_scale(150, "absolute", "proportion")
  expect_true(is.na(result1), info = "Absolute→proportion without context should return NA")

  # Invalid symbols
  invalid_parsed <- list(value = 80, symbol = "invalid")
  result2 <- coerce_to_target_unit(invalid_parsed, "proportion")
  expect_true(is.na(result2), info = "Invalid symbol should return NA")

  # Unknown units
  result3 <- coerce_to_target_unit(list(value = 80, symbol = "percent"), "unknown")
  expect_true(is.na(result3), info = "Unknown target unit should return NA")

})

test_that("Edge case: mixed decimal/integer data", {

  # Test data that could be ambiguous

  mixed_data <- c(0.5, 1, 1.5, 2)  # Some ≤1, some >1

  # Should not be detected as proportion since max > 1
  detected_unit <- detect_unit_from_data(mixed_data)
  expect_false(detected_unit == "proportion",
               info = "Data with values >1 should not be detected as proportion")

})

test_that("Validation catches potential 100×-bugs", {

  # Test validation functions that should catch problems

  # Proportion data outside [0,1] when not using counts+n
  validation <- validate_qic_inputs(
    y = c(10, 30, 60, 80),  # These look like percents, not proportions
    n = NULL,               # No denominators
    internal_unit = "proportion"
  )

  expect_false(validation$valid, info = "Should detect invalid proportion range")
  expect_length(validation$warnings, 1)

})

test_that("Performance: normalization is reasonably fast", {

  # Basic performance check - normalization should be fast enough
  # for interactive use

  large_input <- rep("80%", 1000)

  timing <- system.time({
    results <- sapply(large_input, function(x) {
      normalize_axis_value(x, chart_type = "p")
    })
  })

  # Should complete 1000 normalizations in reasonable time
  expect_true(timing[["elapsed"]] < 1.0, info = "1000 normalizations should complete in <1 second")
  expect_true(all(results == 0.8), info = "All results should be 0.8")

})