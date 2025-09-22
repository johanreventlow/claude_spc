# test-constants-architecture.R
# Tests af D) Architecture Improvements - modular config files og reusable modules

test_that("modular config files load correctly", {
  # Test at de nye modulære config filer eksisterer
  expect_true(file.exists(here::here("R", "config_ui_config.R")))
  expect_true(file.exists(here::here("R", "config_spc_config.R")))
  expect_true(file.exists(here::here("R", "config_system_config.R")))
  expect_true(file.exists(here::here("R", "config_chart_types.R")))

  # Source alle config filer
  source(here::here("R", "config_ui_config.R"))
  source(here::here("R", "config_spc_config.R"))
  source(here::here("R", "config_system_config.R"))
  source(here::here("R", "config_chart_types.R"))

  # Test at hovedkonstanter eksisterer (nu spredt på tværs af config filer)
  expect_true(exists("UI_COLUMN_WIDTHS"))
  expect_true(exists("MIN_SPC_ROWS"))
  expect_true(exists("OBSERVER_PRIORITIES"))
  expect_true(exists("CHART_TYPES_DA"))
})

test_that("system configuration konstanter", {
  source(here::here("R", "config_system_config.R"))

  # Test performance thresholds
  expect_true(exists("PERFORMANCE_THRESHOLDS"))
  expect_type(PERFORMANCE_THRESHOLDS, "list")

  # Test operation timeouts
  expect_true(exists("OPERATION_TIMEOUTS"))
  expect_type(OPERATION_TIMEOUTS, "list")

  # Test observer priorities
  expect_true(exists("OBSERVER_PRIORITIES"))
  expect_type(OBSERVER_PRIORITIES, "list")
})

test_that("SPC configuration konstanter", {
  source(here::here("R", "config_spc_config.R"))

  # Test SPC validation constants
  expect_true(exists("MIN_SPC_ROWS"))
  expect_true(exists("RECOMMENDED_SPC_POINTS"))
  expect_true(exists("MAX_MISSING_PERCENT"))
  expect_true(exists("MIN_NUMERIC_PERCENT"))

  # Test at værdier er logiske
  expect_true(MIN_SPC_ROWS > 0)
  expect_true(RECOMMENDED_SPC_POINTS >= MIN_SPC_ROWS)
  expect_true(MAX_MISSING_PERCENT > 0 && MAX_MISSING_PERCENT <= 100)
  expect_true(MIN_NUMERIC_PERCENT > 0 && MIN_NUMERIC_PERCENT <= 1)
})

test_that("UI layout konstanter", {
  source(here::here("R", "config_ui_config.R"))

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

test_that("chart types konstanter", {
  source(here::here("R", "config_chart_types.R"))

  # Test chart types
  expect_type(CHART_TYPES_DA, "list")
  expect_type(CHART_TYPES_EN, "list")

  # Test specifikke chart mappings
  expect_equal(CHART_TYPES_DA[["I-kort (Individuelle værdier)"]], "i")
  expect_equal(CHART_TYPES_DA[["P-kort (Andele)"]], "p")
  expect_equal(CHART_TYPES_DA[["C-kort (Tællinger)"]], "c")

  # Test at der er sammenhæng mellem DA og EN versioner
  expect_equal(length(CHART_TYPES_DA), length(CHART_TYPES_EN))
})

test_that("Y-axis units konstanter", {
  # Source Y_AXIS_UNITS_DA from its proper location
  source(here::here("R", "config_spc_config.R"))

  # Test Y-axis units structure
  expect_type(Y_AXIS_UNITS_DA, "list")
  expect_true("Antal" %in% names(Y_AXIS_UNITS_DA))
  expect_true("Procent (%)" %in% names(Y_AXIS_UNITS_DA))
  expect_true("Promille (‰)" %in% names(Y_AXIS_UNITS_DA))

  # Test that values are correct runtime codes (not Danish)
  expect_equal(Y_AXIS_UNITS_DA[["Antal"]], "count")
  expect_equal(Y_AXIS_UNITS_DA[["Procent (%)"]], "percent")
  expect_equal(Y_AXIS_UNITS_DA[["Promille (‰)"]], "permille")
})

test_that("logging og performance konstanter", {
  source(here::here("R", "config_system_config.R"))

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
  # Load only the config files needed without hospital branding
  source(here::here("R", "config_chart_types.R"))
  source(here::here("R", "config_ui_config.R"))

  # Test at chart types har de forventede keys
  expected_charts <- c("I-kort (Individuelle værdier)", "P-kort (Andele)", "C-kort (Tællinger)")
  expect_true(all(expected_charts %in% names(CHART_TYPES_DA)))

  # Test at UI helpers har fornuftige strukturer
  expect_type(UI_COLUMN_WIDTHS, "list")
  expect_true("quarter" %in% names(UI_COLUMN_WIDTHS))

  # Test that TEST_MODE_AUTO_LOAD environment variable handling works
  TEST_MODE_AUTO_LOAD <- as.logical(Sys.getenv("TEST_MODE_AUTO_LOAD", "TRUE"))
  expect_type(TEST_MODE_AUTO_LOAD, "logical")
  expect_true(TEST_MODE_AUTO_LOAD %in% c(TRUE, FALSE))
})

test_that("UI helpers module funktionalitet", {
  # Test at UI config filen fungerer
  source(here::here("R", "config_ui_config.R"))

  # Test UI config indlæsning
  expect_true(exists("UI_COLUMN_WIDTHS"))
  expect_true(exists("UI_HEIGHTS"))
  expect_true(exists("UI_STYLES"))

  # Test config funktionalitet
  expect_equal(UI_COLUMN_WIDTHS$quarter, c(6, 6, 6, 6))
  expect_equal(UI_COLUMN_WIDTHS$half, c(6, 6))
  expect_equal(UI_HEIGHTS$logo, "40px")
  expect_match(UI_STYLES$flex_column, "display: flex")
})

test_that("integration med eksisterende kode", {
  # Test fil funktioner - source only needed files
  source("../../R/fct_file_operations.R")
  source("../../R/core_spc_helpers.R")

  # Test fil funktioner - now in R/fct_file_operations.R
  expect_true(exists("handle_csv_upload"))

  # Test SPC helpers - now from R/core_spc_helpers.R
  expect_true(exists("validate_x_column_format"))

  # Test med mock data
  test_data <- data.frame(
    Dato = c("2023-01-01", "2023-01-02", "2023-01-03"),
    Tæller = c(10, 15, 8),
    Nævner = c(100, 120, 90)
  )

  # Test basic validation
  expect_true(is.data.frame(test_data))
  expect_equal(nrow(test_data), 3)
  expect_equal(ncol(test_data), 3)
})
