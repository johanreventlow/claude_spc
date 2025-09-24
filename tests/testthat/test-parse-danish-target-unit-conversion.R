# test-parse-danish-target-unit-conversion.R
# Tests for enhanced parse_danish_target() with explicit y_axis_unit support
# Prioritizes user's explicit unit choice over automatic scale detection

# Functions should be available through global.R loading

test_that("parse_danish_target prioritizes Y-data context over explicit y_axis_unit", {

  # TEST: PRIORITY 1 - Y-data skala har højeste prioritet (intelligent kontekst)

  # SCENARIO A: Decimal Y-data skala - input skal tilpasses til 0-1 skala
  decimal_y_data <- c(0.1, 0.3, 0.6, 0.8)

  # Med decimal Y-data skal "80%" blive 0.8 (uanset y_axis_unit)
  expect_equal(parse_danish_target("80%", decimal_y_data, "percent"), 0.8)
  expect_equal(parse_danish_target("80%", decimal_y_data, "count"), 0.8)
  expect_equal(parse_danish_target("80", decimal_y_data, "percent"), 0.8)  # 80 → 0.8
  expect_equal(parse_danish_target("0.8", decimal_y_data, "percent"), 0.8) # bevarer decimal

  # SCENARIO B: Procent Y-data skala - input skal tilpasses til 0-100 skala
  percent_y_data <- c(10, 25, 60, 85)

  # Med procent Y-data skal "80%" blive 80 (uanset y_axis_unit)
  expect_equal(parse_danish_target("80%", percent_y_data, "count"), 80)
  expect_equal(parse_danish_target("80%", percent_y_data, "percent"), 80)
  expect_equal(parse_danish_target("0.8", percent_y_data, "count"), 80)   # 0.8 → 80
  expect_equal(parse_danish_target("80", percent_y_data, "count"), 80)    # bevarer procent

  # SCENARIO C: Integer/rate Y-data skala - fjern symboler men behold værdi
  integer_y_data <- c(150, 250, 450, 800)

  expect_equal(parse_danish_target("80%", integer_y_data, "percent"), 80) # fjern % symbol
  expect_equal(parse_danish_target("80", integer_y_data, "percent"), 80)  # behold værdi
})

test_that("parse_danish_target respects explicit y_axis_unit when no Y-data", {

  # TEST: PRIORITY 2 - Eksplicit y_axis_unit bruges kun når ingen Y-data

  # SCENARIO: Bruger vælger "percent" som y-akse enhed (ingen Y-data)
  expect_equal(parse_danish_target("80%", NULL, "percent"), 80)
  expect_equal(parse_danish_target("0.8", NULL, "percent"), 80) # decimal → procent
  expect_equal(parse_danish_target("80", NULL, "percent"), 80)  # allerede procent

  # SCENARIO: Bruger vælger "count" som y-akse enhed (ingen Y-data)
  expect_equal(parse_danish_target("80%", NULL, "count"), 80)   # fjern % symbol
  expect_equal(parse_danish_target("80", NULL, "count"), 80)    # behold tal
  expect_equal(parse_danish_target("0.8", NULL, "count"), 0.8)  # behold decimal

  # SCENARIO: Bruger vælger "permille" som y-akse enhed (ingen Y-data)
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

test_that("improved detect_y_axis_scale function works correctly", {

  # TEST: Forbedret skala-detektion med bedre decimal vs procent distinction

  # SCENARIO: Decimal skala detection
  pure_decimal <- c(0.1, 0.3, 0.6, 0.8, 0.9)
  expect_equal(detect_y_axis_scale(pure_decimal), "decimal")

  mixed_decimal <- c(0.1, 0.5, 0.8, 1.0)  # Max = 1.0 men mixed
  expect_equal(detect_y_axis_scale(mixed_decimal), "decimal")

  # SCENARIO: Procent skala detection (forbedret kriterier)
  typical_percent <- c(10, 25, 45, 60, 85)  # hele tal i 0-100 range
  expect_equal(detect_y_axis_scale(typical_percent), "percent")

  some_decimals_percent <- c(10.5, 25, 45, 60.2, 85)  # nogle decimaler men stadig procent-range
  expect_equal(detect_y_axis_scale(some_decimals_percent), "integer")  # Falder tilbage til integer

  # SCENARIO: Integer/rate skala detection
  large_numbers <- c(150, 250, 450, 800)
  expect_equal(detect_y_axis_scale(large_numbers), "integer")

  # SCENARIO: Edge cases
  empty_data <- numeric(0)
  expect_equal(detect_y_axis_scale(empty_data), "integer")

  all_na <- c(NA, NA, NA)
  expect_equal(detect_y_axis_scale(all_na), "integer")

  single_value <- c(0.5)
  expect_equal(detect_y_axis_scale(single_value), "decimal")
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

test_that("intelligent context parsing works correctly (Y-data prioritized)", {

  # TEST: Y-data kontekst prioriteres over eksplicit enhed (ny adfærd)

  # SCENARIO: Decimal Y-data skal altid resultere i decimal output
  decimal_y_data <- c(0.1, 0.2, 0.3, 0.4)
  decimal_y_numeric <- parse_danish_number(decimal_y_data)

  # Med decimal Y-data skal input tilpasses til decimal-skala (uanset y_axis_unit)
  expect_equal(parse_danish_target("80%", decimal_y_numeric, "percent"), 0.8)  # intelligent kontekst
  expect_equal(parse_danish_target("0.8", decimal_y_numeric, "percent"), 0.8)  # bevar decimal
  expect_equal(parse_danish_target("80", decimal_y_numeric, "percent"), 0.8)   # konverter til decimal

  # SCENARIO: Procent Y-data skal altid resultere i procent output
  percent_y_data <- c(10, 20, 30, 40)
  percent_y_numeric <- parse_danish_number(percent_y_data)

  # Med procent Y-data skal input tilpasses til procent-skala (uanset y_axis_unit)
  expect_equal(parse_danish_target("80%", percent_y_numeric, "count"), 80)    # intelligent kontekst
  expect_equal(parse_danish_target("0.8", percent_y_numeric, "count"), 80)    # konverter til procent
  expect_equal(parse_danish_target("25", percent_y_numeric, "count"), 25)     # bevar procent
})