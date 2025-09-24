# test-parse-danish-target-unit-conversion.R
# Tests for enhanced parse_danish_target() with explicit y_axis_unit support
# Prioritizes user's explicit unit choice over automatic scale detection

# Functions should be available through global.R loading

test_that("parse_danish_target respects explicit y_axis_unit parameter", {

  # TEST: Procent enhed prioriteres over automatisk detektion

  # SCENARIO: Bruger vælger "percent" som y-akse enhed
  expect_equal(parse_danish_target("80%", NULL, "percent"), 80)
  expect_equal(parse_danish_target("0.8", NULL, "percent"), 80) # decimal → procent
  expect_equal(parse_danish_target("80", NULL, "percent"), 80)  # allerede procent

  # SCENARIO: Bruger vælger "count" som y-akse enhed
  expect_equal(parse_danish_target("80%", NULL, "count"), 80)   # fjern % symbol
  expect_equal(parse_danish_target("80", NULL, "count"), 80)    # behold tal
  expect_equal(parse_danish_target("0.8", NULL, "count"), 0.8)  # behold decimal

  # SCENARIO: Bruger vælger "permille" som y-akse enhed
  expect_equal(parse_danish_target("8‰", NULL, "permille"), 8)   # 8‰ → 8
  expect_equal(parse_danish_target("0.008", NULL, "permille"), 8) # 0.008 → 8
  expect_equal(parse_danish_target("8", NULL, "permille"), 8)    # 8 → 8
})

test_that("parse_danish_target handles rate units correctly", {

  # TEST: Rate enheder behandler input som absolutte tal

  rate_units <- c("rate_1000", "rate_100000")

  for (unit in rate_units) {
    # Fjern % og ‰ symboler men behold numerisk værdi
    expect_equal(parse_danish_target("15%", NULL, unit), 15)
    expect_equal(parse_danish_target("8‰", NULL, unit), 8)
    expect_equal(parse_danish_target("22,5", NULL, unit), 22.5)
    expect_equal(parse_danish_target("100", NULL, unit), 100)
  }
})

test_that("parse_danish_target handles absolute units correctly", {

  # TEST: Absolutte enheder (count, days, hours, etc.) behandles konsistent

  absolute_units <- c("count", "days", "hours", "grams", "kg", "dkk")

  for (unit in absolute_units) {
    # Behold numerisk værdi, fjern kun symboler
    expect_equal(parse_danish_target("50%", NULL, unit), 50)
    expect_equal(parse_danish_target("5‰", NULL, unit), 5)
    expect_equal(parse_danish_target("42", NULL, unit), 42)
    expect_equal(parse_danish_target("12,5", NULL, unit), 12.5)
  }
})

test_that("parse_danish_target fallback logic works without y_axis_unit", {

  # TEST: Automatisk skala-detektion som fallback når ingen enhed specificeret

  # Ingen y_data og ingen unit → simple regler
  expect_equal(parse_danish_target("80%", NULL, NULL), 0.8)     # procent → decimal
  expect_equal(parse_danish_target("8‰", NULL, NULL), 8)       # promille uændret
  expect_equal(parse_danish_target("50", NULL, NULL), 50)      # tal uændret

  # Med y_data men ingen unit → automatisk skala-detektion
  decimal_data <- c(0.1, 0.2, 0.3, 0.8)  # decimal scale
  percent_data <- c(10, 20, 30, 80)       # percent scale
  count_data <- c(100, 200, 300, 800)     # count scale

  # Decimal skala: konverter procent til decimal
  expect_equal(parse_danish_target("80%", decimal_data, NULL), 0.8)
  expect_equal(parse_danish_target("0.6", decimal_data, NULL), 0.6)

  # Procent skala: behandl som procent
  expect_equal(parse_danish_target("80%", percent_data, NULL), 80)
  expect_equal(parse_danish_target("0.8", percent_data, NULL), 80)

  # Count skala: behold tal, fjern symboler
  expect_equal(parse_danish_target("80%", count_data, NULL), 80)
  expect_equal(parse_danish_target("150", count_data, NULL), 150)
})

test_that("parse_danish_target handles edge cases gracefully", {

  # TEST: Edge cases og error handling

  # Tomme/ugyldige inputs
  expect_null(parse_danish_target(NULL, NULL, "percent"))
  expect_null(parse_danish_target("", NULL, "percent"))
  expect_null(parse_danish_target("invalid", NULL, "percent"))
  expect_null(parse_danish_target(NA, NULL, "percent"))

  # Ukendte enheder → fallback til simple regler
  expect_equal(parse_danish_target("80%", NULL, "unknown_unit"), 0.8)
  expect_equal(parse_danish_target("8‰", NULL, "unknown_unit"), 8)
  expect_equal(parse_danish_target("50", NULL, "unknown_unit"), 50)

  # Kombineret symboler (edge case)
  expect_null(parse_danish_target("80%‰", NULL, "percent"))  # invalid format
})

test_that("parse_danish_target preserves Danish number formats", {

  # TEST: Danske komma-decimaler håndteres korrekt

  # Komma-decimaler i forskellige enheder
  expect_equal(parse_danish_target("68,5%", NULL, "percent"), 68.5)
  expect_equal(parse_danish_target("3,14", NULL, "count"), 3.14)
  expect_equal(parse_danish_target("0,85", NULL, "percent"), 85)
  expect_equal(parse_danish_target("12,75‰", NULL, "permille"), 12.75)
})

test_that("convert_by_unit_type helper function works correctly", {

  # TEST: Helper funktionen convert_by_unit_type

  # Test direkte kald til helper-funktionen
  expect_equal(convert_by_unit_type(80, "percent", TRUE, FALSE), 80)    # 80% → 80
  expect_equal(convert_by_unit_type(0.8, "percent", FALSE, FALSE), 80)  # 0.8 → 80
  expect_equal(convert_by_unit_type(8, "permille", TRUE, FALSE), 8)     # 8‰ → 8 (med promille symbol)
  expect_equal(convert_by_unit_type(0.008, "permille", FALSE, FALSE), 8) # 0.008 → 8
  expect_equal(convert_by_unit_type(50, "count", TRUE, FALSE), 50)      # 50% → 50
  expect_equal(convert_by_unit_type(50, "count", FALSE, FALSE), 50)     # 50 → 50
})

test_that("unit conversion priority over scale detection works", {

  # TEST: Eksplicit enhed prioriteres over automatisk detektion

  # SCENARIO: Y-data antyder decimal skala, men bruger vælger procent
  decimal_y_data <- c(0.1, 0.2, 0.3, 0.4)  # ville normalt foreslå decimal
  percent_y_numeric <- parse_danish_number(decimal_y_data)

  # Med eksplicit "percent" unit skal input behandles som procent
  expect_equal(parse_danish_target("80%", percent_y_numeric, "percent"), 80)
  expect_equal(parse_danish_target("0.8", percent_y_numeric, "percent"), 80)

  # SCENARIO: Y-data antyder procent skala, men bruger vælger count
  percent_y_data <- c(10, 20, 30, 40)  # ville normalt foreslå procent
  count_y_numeric <- parse_danish_number(percent_y_data)

  # Med eksplicit "count" unit skal input behandles som absolutte tal
  expect_equal(parse_danish_target("80%", count_y_numeric, "count"), 80)
  expect_equal(parse_danish_target("25", count_y_numeric, "count"), 25)
})