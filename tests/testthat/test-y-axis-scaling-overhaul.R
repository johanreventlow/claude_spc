# test-y-axis-scaling-overhaul.R
# Comprehensive test suite for the new Y-axis scaling API
# Tests the 3-layer architecture: Parsing → Unit Clarification → Conversion

# Load the new functions through global.R
# These tests validate the separation of concerns and deterministic behavior

# =============================================================================
# LAYER 1: PARSING TESTS
# =============================================================================

test_that("parse_number_da correctly parses Danish numbers with symbols", {

  # Basic symbol detection
  expect_equal(parse_number_da("80%"), list(value = 80, symbol = "percent"))
  expect_equal(parse_number_da("8‰"), list(value = 8, symbol = "permille"))
  expect_equal(parse_number_da("80"), list(value = 80, symbol = "none"))

  # Danish comma decimals
  expect_equal(parse_number_da("68,5%"), list(value = 68.5, symbol = "percent"))
  expect_equal(parse_number_da("3,14"), list(value = 3.14, symbol = "none"))
  expect_equal(parse_number_da("0,85"), list(value = 0.85, symbol = "none"))

  # Whitespace handling
  expect_equal(parse_number_da(" 80% "), list(value = 80, symbol = "percent"))
  expect_equal(parse_number_da("80 %"), list(value = 80, symbol = "percent"))

  # Edge cases
  expect_equal(parse_number_da(""), list(value = NA_real_, symbol = "none"))
  expect_equal(parse_number_da(NULL), list(value = numeric(0), symbol = character(0)))
  expect_equal(parse_number_da("80%‰"), list(value = NA_real_, symbol = "invalid"))  # Both symbols

})

test_that("parse_number_da is idempotent", {

  # Same input should give same output on repeated calls
  input1 <- "68,5%"
  result1a <- parse_number_da(input1)
  result1b <- parse_number_da(input1)
  expect_identical(result1a, result1b)

  input2 <- "0,85"
  result2a <- parse_number_da(input2)
  result2b <- parse_number_da(input2)
  expect_identical(result2a, result2b)

})

test_that("parse_number_da handles vectors correctly", {

  # Vector input should return vector output
  inputs <- c("80%", "8‰", "75")
  result <- parse_number_da(inputs)

  expect_equal(result$value, c(80, 8, 75))
  expect_equal(result$symbol, c("percent", "permille", "none"))

})

# =============================================================================
# LAYER 2: UNIT CLARIFICATION TESTS
# =============================================================================

test_that("resolve_y_unit follows correct priority order", {

  # Priority 1: User explicit choice overrides everything
  expect_equal(resolve_y_unit(user_unit = "percent", col_unit = "proportion", y_sample = c(0.1, 0.2)), "percent")

  # Priority 2: Column metadata when no user choice
  expect_equal(resolve_y_unit(user_unit = NULL, col_unit = "permille", y_sample = c(0.1, 0.2)), "permille")

  # Priority 3: Data heuristics when no explicit choices
  decimal_data <- c(0.1, 0.2, 0.3, 0.8)
  expect_equal(resolve_y_unit(user_unit = NULL, col_unit = NULL, y_sample = decimal_data), "proportion")

  percent_data <- c(10, 20, 30, 80)
  expect_equal(resolve_y_unit(user_unit = NULL, col_unit = NULL, y_sample = percent_data), "percent")

  # Priority 4: Fallback to absolute
  expect_equal(resolve_y_unit(user_unit = NULL, col_unit = NULL, y_sample = NULL), "absolute")

})

test_that("detect_unit_from_data uses clear heuristics", {

  # Decimal detection [0,1] with decimals
  decimal_data1 <- c(0.1, 0.3, 0.6, 0.8)
  expect_equal(detect_unit_from_data(decimal_data1), "proportion")

  # Decimal detection [0,1] even with integers (80% in range)
  decimal_data2 <- c(0, 0, 1, 1)
  expect_equal(detect_unit_from_data(decimal_data2), "proportion")

  # Percent detection [0-100] with whole numbers
  percent_data <- c(10, 25, 50, 85)
  expect_equal(detect_unit_from_data(percent_data), "percent")

  # Should NOT detect percent when too many decimals
  mixed_percent <- c(10.5, 25.3, 45.7, 60.2, 85.9)
  expect_equal(detect_unit_from_data(mixed_percent), "absolute")

  # Large numbers → absolute
  large_data <- c(150, 250, 450, 800)
  expect_equal(detect_unit_from_data(large_data), "absolute")

  # Empty/NA data → absolute
  expect_equal(detect_unit_from_data(c()), "absolute")
  expect_equal(detect_unit_from_data(c(NA, NA)), "absolute")

})

# =============================================================================
# LAYER 3A: HARMONIZATION TESTS
# =============================================================================

test_that("coerce_to_target_unit uses deterministic conversion matrix", {

  # TO PROPORTION
  expect_equal(coerce_to_target_unit(list(value = 80, symbol = "percent"), "proportion"), 0.8)
  expect_equal(coerce_to_target_unit(list(value = 8, symbol = "permille"), "proportion"), 0.008)
  expect_equal(coerce_to_target_unit(list(value = 0.8, symbol = "none"), "proportion"), 0.8)  # No implicit scaling!

  # TO PERCENT
  expect_equal(coerce_to_target_unit(list(value = 80, symbol = "percent"), "percent"), 80)
  expect_equal(coerce_to_target_unit(list(value = 80, symbol = "permille"), "percent"), 8)
  expect_equal(coerce_to_target_unit(list(value = 80, symbol = "none"), "percent"), 80)  # No implicit scaling!

  # TO PERMILLE
  expect_equal(coerce_to_target_unit(list(value = 8, symbol = "percent"), "permille"), 80)
  expect_equal(coerce_to_target_unit(list(value = 80, symbol = "permille"), "permille"), 80)
  expect_equal(coerce_to_target_unit(list(value = 80, symbol = "none"), "permille"), 80)  # No implicit scaling!

  # TO ABSOLUTE
  expect_equal(coerce_to_target_unit(list(value = 80, symbol = "percent"), "absolute"), 80)
  expect_equal(coerce_to_target_unit(list(value = 8, symbol = "permille"), "absolute"), 8)
  expect_equal(coerce_to_target_unit(list(value = 50, symbol = "none"), "absolute"), 50)

})

test_that("coerce_to_target_unit handles edge cases", {

  # Invalid input
  expect_true(is.na(coerce_to_target_unit(list(value = NA, symbol = "percent"), "proportion")))
  expect_true(is.na(coerce_to_target_unit(list(value = 80, symbol = "invalid"), "proportion")))

  # Unknown target unit
  expect_true(is.na(coerce_to_target_unit(list(value = 80, symbol = "percent"), "unknown")))

})

# =============================================================================
# LAYER 3B: INTERNAL CONVERSION TESTS
# =============================================================================

test_that("to_internal_scale converts deterministically", {

  # TO PROPORTION (internal canonical for proportional plots)
  expect_equal(to_internal_scale(80, "percent", "proportion"), 0.8)
  expect_equal(to_internal_scale(80, "permille", "proportion"), 0.08)
  expect_equal(to_internal_scale(0.8, "proportion", "proportion"), 0.8)  # Identity

  # TO ABSOLUTE (internal canonical for count plots)
  expect_equal(to_internal_scale(80, "absolute", "absolute"), 80)  # Identity
  expect_equal(to_internal_scale(80, "percent", "absolute"), 80)  # Should this be allowed?

  # Error cases
  expect_true(is.na(to_internal_scale(80, "absolute", "proportion")))  # Can't convert absolute→proportion without context

})

# =============================================================================
# MAIN API TESTS
# =============================================================================

test_that("normalize_axis_value integrates all layers correctly", {

  # SCENARIO A: Input with % symbol, proportion internal unit
  # User types "80%" → should become 0.8 in proportion scale
  result1 <- normalize_axis_value("80%", user_unit = "proportion", internal_unit = "proportion")
  expect_equal(result1, 0.8)

  # SCENARIO B: Input without symbol, percent target unit
  # User types "80" → should become 80 in percent scale (no implicit scaling)
  result2 <- normalize_axis_value("80", user_unit = "percent", internal_unit = "percent")
  expect_equal(result2, 80)

  # SCENARIO C: Data-driven unit resolution
  # No explicit unit, but Y-data suggests proportion scale
  decimal_y_data <- c(0.1, 0.2, 0.3, 0.8)
  result3 <- normalize_axis_value("80%", y_sample = decimal_y_data, internal_unit = "proportion")
  expect_equal(result3, 0.8)  # 80% → proportion 0.8

  # SCENARIO D: Data-driven unit resolution suggests percent
  percent_y_data <- c(10, 20, 30, 80)
  result4 <- normalize_axis_value("80%", y_sample = percent_y_data, internal_unit = "percent")
  expect_equal(result4, 80)  # 80% → percent 80

})

test_that("normalize_axis_value handles edge cases gracefully", {

  # Invalid input
  expect_null(normalize_axis_value(""))
  expect_null(normalize_axis_value("invalid"))
  expect_null(normalize_axis_value("80%‰"))  # Both symbols

  # Valid input but incompatible units
  # Trying to force absolute→proportion without context should fail gracefully
  result <- normalize_axis_value("150", user_unit = "absolute", internal_unit = "proportion")
  expect_true(is.null(result) || !is.na(result))  # Should handle gracefully

})

test_that("normalize_axis_value is idempotent", {

  # Same input should give same output
  input_str <- "68,5%"
  user_unit <- "proportion"
  internal_unit <- "proportion"

  result_a <- normalize_axis_value(input_str, user_unit = user_unit, internal_unit = internal_unit)
  result_b <- normalize_axis_value(input_str, user_unit = user_unit, internal_unit = internal_unit)

  expect_identical(result_a, result_b)

})

# =============================================================================
# VALIDATION TESTS
# =============================================================================

test_that("validate_axis_value enforces range constraints", {

  # Proportion should be [0,1]
  valid_prop <- validate_axis_value(0.8, "proportion")
  expect_true(valid_prop$valid)

  invalid_prop_high <- validate_axis_value(1.5, "proportion")
  expect_false(invalid_prop_high$valid)

  invalid_prop_low <- validate_axis_value(-0.1, "proportion")
  expect_false(invalid_prop_low$valid)

  # Absolute has no constraints (for now)
  absolute_val <- validate_axis_value(150, "absolute")
  expect_true(absolute_val$valid)

})

# =============================================================================
# BACKWARDS COMPATIBILITY TESTS
# =============================================================================

test_that("parse_danish_target maintains backwards compatibility", {

  # These tests should pass to ensure existing code still works
  # Eventually these can be migrated to use normalize_axis_value directly

  # Decimal Y-data context
  decimal_y_data <- c(0.1, 0.3, 0.6, 0.8)
  expect_equal(parse_danish_target("80%", decimal_y_data, "percent"), 0.8)
  expect_equal(parse_danish_target("0.8", decimal_y_data, "percent"), 0.8)

  # Percent Y-data context
  percent_y_data <- c(10, 25, 60, 85)
  expect_equal(parse_danish_target("80%", percent_y_data, "count"), 80)
  expect_equal(parse_danish_target("0.8", percent_y_data, "count"), 80)

  # No Y-data, explicit unit
  expect_equal(parse_danish_target("80%", NULL, "percent"), 80)
  expect_equal(parse_danish_target("0.8", NULL, "percent"), 80)

})

# =============================================================================
# INTEGRATION AND CONSISTENCY TESTS
# =============================================================================

test_that("Key examples from design specification work correctly", {

  # Your examples from the design document:

  # "80%" + target_unit=proportion → 0.8
  result1 <- normalize_axis_value("80%", user_unit = "proportion", internal_unit = "proportion")
  expect_equal(result1, 0.8)

  # "80" (no symbol) + target_unit=percent → 80 (NOT 0.8)
  result2 <- normalize_axis_value("80", user_unit = "percent", internal_unit = "percent")
  expect_equal(result2, 80)

  # "0,8" (no symbol) + target_unit=percent → 0.8 (i.e., 0.8%)
  result3 <- normalize_axis_value("0,8", user_unit = "percent", internal_unit = "percent")
  expect_equal(result3, 0.8)

  # "0,8" + target_unit=proportion → 0.8
  result4 <- normalize_axis_value("0,8", user_unit = "proportion", internal_unit = "proportion")
  expect_equal(result4, 0.8)

  # "8‰" + target_unit=proportion → 0.008
  result5 <- normalize_axis_value("8‰", user_unit = "proportion", internal_unit = "proportion")
  expect_equal(result5, 0.008)

  # "8‰" + target_unit=percent → 0.8
  result6 <- normalize_axis_value("8‰", user_unit = "percent", internal_unit = "percent")
  expect_equal(result6, 0.8)

})

test_that("No double-scaling occurs in typical qicharts2 workflows", {

  # This test ensures that when we have proportion data [0,1]
  # and send it to qicharts2, no additional /100 happens

  # Typical proportion workflow:
  # 1. User enters "80%" for a target in a p-chart context
  # 2. We normalize to internal proportion scale: 0.8
  # 3. This 0.8 should go directly to qicharts2 without further scaling

  proportion_y_data <- c(0.1, 0.2, 0.3, 0.8)  # Typical p-chart data
  target_normalized <- normalize_axis_value("80%", y_sample = proportion_y_data, internal_unit = "proportion")

  expect_equal(target_normalized, 0.8)
  # This 0.8 can now safely go to qicharts2 target parameter
  # No additional /100 should happen in downstream code

})

test_that("Priority system works as designed", {

  # User explicit choice should override data heuristics
  percent_looking_data <- c(10, 20, 30, 80)  # Would normally suggest "percent"

  # But user explicitly chooses "proportion"
  result <- normalize_axis_value("80%", user_unit = "proportion", y_sample = percent_looking_data, internal_unit = "proportion")
  expect_equal(result, 0.8)  # Should honor user choice: 80% → 0.8 proportion

  # Whereas without user choice, data heuristics would suggest percent
  result_heuristic <- normalize_axis_value("80%", y_sample = percent_looking_data, internal_unit = "percent")
  expect_equal(result_heuristic, 80)  # Data suggests percent: 80% → 80 percent

})