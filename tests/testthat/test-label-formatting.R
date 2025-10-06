# test-label-formatting.R
# Tests for utils_label_formatting.R
#
# Verificerer at format_y_value() formaterer konsistent på tværs af enheder

test_that("format_y_value() formaterer count korrekt", {
  # Small values
  expect_equal(format_y_value(5, "count"), "5")
  expect_equal(format_y_value(123, "count"), "123")
  expect_equal(format_y_value(999, "count"), "999")

  # K notation
  expect_equal(format_y_value(1000, "count"), "1K")
  expect_equal(format_y_value(1500, "count"), "1,5K")
  expect_equal(format_y_value(10000, "count"), "10K")

  # M notation
  expect_equal(format_y_value(1000000, "count"), "1M")
  expect_equal(format_y_value(1500000, "count"), "1,5M")

  # Mia notation
  expect_equal(format_y_value(1000000000, "count"), "1 mia.")
  expect_equal(format_y_value(1500000000, "count"), "1,5 mia.")

  # Decimal values < 1000
  expect_equal(format_y_value(12.5, "count"), "12,5")
  expect_equal(format_y_value(100.0, "count"), "100")
})

test_that("format_y_value() formaterer percent korrekt", {
  # Uses scales::label_percent()
  expect_equal(format_y_value(0.5, "percent"), "50%")
  expect_equal(format_y_value(0.123, "percent"), "12%")
  expect_equal(format_y_value(1.0, "percent"), "100%")
})

test_that("format_y_value() formaterer rate korrekt", {
  # Dansk decimal notation
  expect_equal(format_y_value(5, "rate"), "5")
  expect_equal(format_y_value(5.5, "rate"), "5,5")
  expect_equal(format_y_value(123.456, "rate"), "123,5")
})

test_that("format_y_value() formaterer time korrekt", {
  # Minutes (< 60 min)
  y_range_min <- c(0, 50)
  expect_equal(format_y_value(30, "time", y_range_min), "30 min")
  expect_equal(format_y_value(45.5, "time", y_range_min), "45,5 min")

  # Hours (60-1440 min)
  y_range_hours <- c(0, 300)
  expect_equal(format_y_value(120, "time", y_range_hours), "2 timer")
  expect_equal(format_y_value(90, "time", y_range_hours), "1,5 timer")

  # Days (>= 1440 min)
  y_range_days <- c(0, 3000)
  expect_equal(format_y_value(1440, "time", y_range_days), "1 dage")
  expect_equal(format_y_value(2880, "time", y_range_days), "2 dage")
})

test_that("format_y_value() håndterer NA korrekt", {
  expect_true(is.na(format_y_value(NA, "count")))
  expect_true(is.na(format_y_value(NA, "percent")))
  expect_true(is.na(format_y_value(NA, "rate")))
  expect_true(is.na(format_y_value(NA, "time", c(0, 100))))
})

test_that("format_y_value() håndterer ukendte enheder", {
  # Default formatting
  expect_equal(format_y_value(123, "unknown"), "123")
  expect_equal(format_y_value(123.45, "unknown"), "123,5")
})

test_that("format_y_value() håndterer time uden y_range", {
  # Warning og fallback til default
  expect_warning(
    result <- format_y_value(120, "time", NULL),
    "y_range mangler for 'time' unit"
  )
  expect_equal(result, "120")
})

test_that("format_y_value() håndterer ikke-numeriske input", {
  # Warning og konvertering til character
  expect_warning(
    result <- format_y_value("abc", "count"),
    "val skal være numerisk"
  )
  expect_equal(result, "abc")
})
