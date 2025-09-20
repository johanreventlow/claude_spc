# test-constants-architecture.R
# Tests af D) Architecture Improvements - constants og reusable modules

test_that("constants fil indlæser korrekt", {
  # Test at constants fil kan indlæses uden fejl
  expect_true(file.exists("../../R/constants.R"))

  # Source constants filen
  source("../../R/constants.R")

  # Test at hovedkonstanter eksisterer
  expect_true(exists("DEFAULT_PORT"))
  expect_true(exists("TEST_MODE_AUTO_LOAD"))
  expect_true(exists("DEFAULT_ENCODING"))
  expect_true(exists("CSV_SEPARATORS"))
  expect_true(exists("DECIMAL_SEPARATORS"))
})

test_that("application configuration konstanter", {
  source("../../R/constants.R")

  # Test port konstant
  expect_equal(DEFAULT_PORT, 3838)
  expect_type(DEFAULT_PORT, "double")

  # Test encoding konstanter
  expect_equal(DEFAULT_ENCODING, "ISO-8859-1")
  expect_equal(UTF8_ENCODING, "UTF-8")

  # Test boolean konstanter
  expect_type(TEST_MODE_AUTO_LOAD, "logical")
  expect_type(AUTO_RESTORE_ENABLED, "logical")
})

test_that("file processing konstanter", {
  source("../../R/constants.R")

  # Test CSV separators
  expect_type(CSV_SEPARATORS, "list")
  expect_equal(CSV_SEPARATORS$semicolon, ";")
  expect_equal(CSV_SEPARATORS$comma, ",")
  expect_equal(CSV_SEPARATORS$tab, "\t")

  # Test decimal separators
  expect_type(DECIMAL_SEPARATORS, "list")
  expect_equal(DECIMAL_SEPARATORS$comma, ",")
  expect_equal(DECIMAL_SEPARATORS$period, ".")
})

test_that("UI layout konstanter", {
  source("../../R/constants.R")

  # Test UI column widths
  expect_type(UI_COLUMN_WIDTHS, "list")
  expect_equal(UI_COLUMN_WIDTHS$quarter, c(6, 6, 6, 6))
  expect_equal(UI_COLUMN_WIDTHS$half, c(6, 6))
  expect_equal(UI_COLUMN_WIDTHS$thirds, c(4, 4, 4))
  expect_equal(UI_COLUMN_WIDTHS$sidebar, c(3, 9))

  # Test UI heights
  expect_type(UI_HEIGHTS, "list")
  expect_equal(UI_HEIGHTS$logo, "40px")
  expect_equal(UI_HEIGHTS$modal_content, "300px")
  expect_match(UI_HEIGHTS$chart_container, "calc\\(.*\\)")

  # Test UI styles
  expect_type(UI_STYLES, "list")
  expect_match(UI_STYLES$flex_column, "display: flex")
  expect_match(UI_STYLES$scroll_auto, "overflow-y: auto")
})

test_that("data validation konstanter", {
  source("../../R/constants.R")

  # Test SPC konstanter
  expect_equal(MIN_SPC_ROWS, 10)
  expect_equal(RECOMMENDED_SPC_POINTS, 20)
  expect_equal(MAX_MISSING_PERCENT, 20)
  expect_equal(MIN_NUMERIC_PERCENT, 0.8)

  # Test at værdier er logiske
  expect_true(MIN_SPC_ROWS > 0)
  expect_true(RECOMMENDED_SPC_POINTS >= MIN_SPC_ROWS)
  expect_true(MAX_MISSING_PERCENT > 0 && MAX_MISSING_PERCENT <= 100)
  expect_true(MIN_NUMERIC_PERCENT > 0 && MIN_NUMERIC_PERCENT <= 1)
})

test_that("SPC chart konstanter", {
  source("../../R/constants.R")

  # Test chart types
  expect_type(CHART_TYPES_DA, "list")
  expect_type(CHART_TYPES_EN, "list")

  # Test specifikke chart mappings
  expect_equal(CHART_TYPES_DA[["I-kort (Individuelle værdier)"]], "i")
  expect_equal(CHART_TYPES_DA[["P-kort (Andele)"]], "p")
  expect_equal(CHART_TYPES_DA[["C-kort (Tællinger)"]], "c")

  # Test Y-axis units
  expect_type(Y_AXIS_UNITS_DA, "list")
  expect_true("Antal" %in% names(Y_AXIS_UNITS_DA))
  expect_true("Procent" %in% names(Y_AXIS_UNITS_DA))
})

test_that("logging og performance konstanter", {
  source("../../R/constants.R")

  # Test observer priorities
  expect_type(OBSERVER_PRIORITIES, "list")
  expect_true(OBSERVER_PRIORITIES$highest > OBSERVER_PRIORITIES$high)
  expect_true(OBSERVER_PRIORITIES$high > OBSERVER_PRIORITIES$medium)
  expect_true(OBSERVER_PRIORITIES$medium > OBSERVER_PRIORITIES$low)

  # Test log components
  expect_type(LOG_COMPONENTS, "list")
  expect_true("DATA_PROC" %in% names(LOG_COMPONENTS))
  expect_true("AUTO_DETECT" %in% names(LOG_COMPONENTS))
  expect_true("ERROR_HANDLING" %in% names(LOG_COMPONENTS))

  # Test timeouts
  expect_type(OPERATION_TIMEOUTS, "list")
  expect_true(OPERATION_TIMEOUTS$file_read > OPERATION_TIMEOUTS$chart_render)
  expect_true(all(sapply(OPERATION_TIMEOUTS, function(x) x > 0)))

  # Test debounce delays
  expect_type(DEBOUNCE_DELAYS, "list")
  expect_true(all(sapply(DEBOUNCE_DELAYS, function(x) x > 0 && x < 2000)))
})

test_that("konstanter konsistens med eksisterende global.R", {
  # Load constants.R
  source("../../R/constants.R")

  # Test at nye konstanter har fornuftige værdier
  expect_true(TEST_MODE_AUTO_LOAD %in% c(TRUE, FALSE))
  expect_true(DEFAULT_ENCODING %in% c("ISO-8859-1", "UTF-8"))

  # Test at chart types har de forventede keys
  expected_charts <- c("I-kort (Individuelle værdier)", "P-kort (Andele)", "C-kort (Tællinger)")
  expect_true(all(expected_charts %in% names(CHART_TYPES_DA)))

  # Test at UI helpers har fornuftige strukturer
  expect_type(UI_COLUMN_WIDTHS, "list")
  expect_true("quarter" %in% names(UI_COLUMN_WIDTHS))
})

test_that("UI helpers module funktionalitet", {
  # Test at UI helpers fil eksisterer
  expect_true(file.exists("../../R/utils_ui_helpers.R"))

  # Source både constants og UI helpers
  source("../../R/constants.R")
  source("../../R/utils_ui_helpers.R")

  # Test helper funktioner
  expect_true(exists("get_flex_column_style"))
  expect_true(exists("get_scroll_container_style"))
  expect_true(exists("get_column_widths"))
  expect_true(exists("get_ui_height"))

  # Test funktionalitet
  expect_equal(get_column_widths("quarter"), c(6, 6, 6, 6))
  expect_equal(get_column_widths("half"), c(6, 6))
  expect_equal(get_ui_height("logo"), "40px")
  expect_match(get_flex_column_style(), "display: flex")
})

test_that("integration med eksisterende kode", {
  # Test at konstanterne kan bruges i eksisterende funktioner
  source("../../R/constants.R")

  # Test fil funktioner
  if (file.exists("../../R/fct_file_io.R")) {
    source("../../R/fct_file_io.R")

    # Test at readCSVFile kan bruge konstanterne
    expect_true(exists("readCSVFile"))

    # Test default parametre (hvis der er nogen defineret)
    # Dette vil afhænge af hvordan funktionen er defineret
  }

  # Test data validation
  # Test validate_data_structure function (moved to fct_spc_helpers.R)
  expect_true(exists("validate_data_structure"))

  # Test med mock data
  test_data <- data.frame(
    Dato = c("2023-01-01", "2023-01-02", "2023-01-03"),
    Tæller = c(10, 15, 8),
    Nævner = c(100, 120, 90)
  )

  result <- validate_data_structure(test_data)
  expect_type(result, "list")
  expect_true("valid" %in% names(result))
})