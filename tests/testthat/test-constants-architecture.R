# test-constants-architecture.R
# Tests af D) Architecture Improvements - modular config files og reusable modules

# Load config consolidation utilities for testing
source(here::here("R", "utils_config_consolidation.R"))

test_that("config registry loads all domains correctly", {
  # Test config registry loading via package-based system
  config_registry <- create_config_registry()

  # Test at alle config domæner kan loades
  charts_config <- config_registry$get("charts")
  expect_type(charts_config, "list")
  expect_true("chart_types_da" %in% names(charts_config))
  expect_true("default_chart" %in% names(charts_config))

  system_config <- config_registry$get("system")
  expect_type(system_config, "list")
  expect_true("observer_priorities" %in% names(system_config))
  expect_true("system_settings" %in% names(system_config))

  ui_config <- config_registry$get("ui")
  expect_type(ui_config, "list")
  expect_true("ui_column_widths" %in% names(ui_config))

  spc_config <- config_registry$get("spc")
  expect_type(spc_config, "list")
  expect_true("spc_settings" %in% names(spc_config))
})

test_that("system configuration via registry", {
  config_registry <- create_config_registry()
  system_config <- config_registry$get("system")

  # Test observer priorities
  expect_true("observer_priorities" %in% names(system_config))
  observer_priorities <- system_config$observer_priorities
  expect_type(observer_priorities, "list")
  expect_true("STATE_MANAGEMENT" %in% names(observer_priorities))
  expect_true("DATA_PROCESSING" %in% names(observer_priorities))

  # Test system settings
  expect_true("system_settings" %in% names(system_config))
  system_settings <- system_config$system_settings
  expect_type(system_settings, "list")
  expect_true("OPERATION_TIMEOUTS" %in% names(system_settings))
  expect_true("PERFORMANCE_THRESHOLDS" %in% names(system_settings))
})

test_that("SPC configuration via registry", {
  config_registry <- create_config_registry()
  spc_config <- config_registry$get("spc")

  # Test SPC settings structure
  expect_true("spc_settings" %in% names(spc_config))
  spc_settings <- spc_config$spc_settings
  expect_type(spc_settings, "list")

  # Test SPC validation constants
  expect_true("MIN_SPC_ROWS" %in% names(spc_settings))
  expect_true("RECOMMENDED_SPC_POINTS" %in% names(spc_settings))
  expect_true("MAX_MISSING_PERCENT" %in% names(spc_settings))
  expect_true("MIN_NUMERIC_PERCENT" %in% names(spc_settings))

  # Test at værdier er logiske
  expect_true(spc_settings$MIN_SPC_ROWS > 0)
  expect_true(spc_settings$RECOMMENDED_SPC_POINTS >= spc_settings$MIN_SPC_ROWS)
  expect_true(spc_settings$MAX_MISSING_PERCENT > 0 && spc_settings$MAX_MISSING_PERCENT <= 100)
  expect_true(spc_settings$MIN_NUMERIC_PERCENT > 0 && spc_settings$MIN_NUMERIC_PERCENT <= 1)
})

test_that("UI layout via registry", {
  config_registry <- create_config_registry()
  ui_config <- config_registry$get("ui")

  # Test UI column widths
  ui_column_widths <- ui_config$ui_column_widths
  expect_type(ui_column_widths, "list")
  expect_equal(ui_column_widths$quarter, c(6, 6, 6, 6))
  expect_equal(ui_column_widths$half, c(6, 6))
  expect_equal(ui_column_widths$thirds, c(4, 4, 4))
  expect_equal(ui_column_widths$sidebar, c(3, 9))

  # Test UI heights
  ui_heights <- ui_config$ui_heights
  expect_type(ui_heights, "list")
  expect_equal(ui_heights$logo, "40px")
  expect_equal(ui_heights$modal_content, "300px")
  expect_match(ui_heights$chart_container, "calc\\(.*\\)")

  # Test UI styles
  ui_styles <- ui_config$ui_styles
  expect_type(ui_styles, "list")
  expect_match(ui_styles$flex_column, "display: flex")
  expect_match(ui_styles$scroll_auto, "overflow-y: auto")
})

test_that("chart types via registry", {
  config_registry <- create_config_registry()
  charts_config <- config_registry$get("charts")

  # Test chart types
  chart_types_da <- charts_config$chart_types_da
  expect_type(chart_types_da, "list")

  # Test specifikke chart mappings
  expect_equal(chart_types_da[["I-kort (Individuelle værdier)"]], "i")
  expect_equal(chart_types_da[["P-kort (Andele)"]], "p")
  expect_equal(chart_types_da[["C-kort (Tællinger)"]], "c")

  # Test default chart
  expect_equal(charts_config$default_chart, "i")
})

test_that("Y-axis units via registry", {
  config_registry <- create_config_registry()
  spc_config <- config_registry$get("spc")

  # Test Y-axis units structure
  y_axis_units_da <- spc_config$spc_settings$Y_AXIS_UNITS_DA
  expect_type(y_axis_units_da, "list")
  expect_true("Antal" %in% names(y_axis_units_da))
  expect_true("Procent (%)" %in% names(y_axis_units_da))
  expect_true("Promille (‰)" %in% names(y_axis_units_da))

  # Test that values are correct runtime codes (not Danish)
  expect_equal(y_axis_units_da[["Antal"]], "count")
  expect_equal(y_axis_units_da[["Procent (%)"]], "percent")
  expect_equal(y_axis_units_da[["Promille (‰)"]], "permille")
})

test_that("logging og performance via registry", {
  config_registry <- create_config_registry()
  system_config <- config_registry$get("system")

  # Test observer priorities
  observer_priorities <- system_config$observer_priorities
  expect_type(observer_priorities, "list")
  expect_true(observer_priorities$highest > observer_priorities$high)
  expect_true(observer_priorities$high > observer_priorities$medium)
  expect_true(observer_priorities$medium > observer_priorities$low)

  # Test log components
  log_components <- system_config$system_settings$LOG_COMPONENTS
  expect_type(log_components, "list")
  expect_true("DATA_PROC" %in% names(log_components))
  expect_true("AUTO_DETECT" %in% names(log_components))
  expect_true("ERROR_HANDLING" %in% names(log_components))

  # Test timeouts
  operation_timeouts <- system_config$system_settings$OPERATION_TIMEOUTS
  expect_type(operation_timeouts, "list")
  expect_true(operation_timeouts$file_read > operation_timeouts$chart_render)
  expect_true(all(sapply(operation_timeouts, function(x) x > 0)))

  # Test debounce delays
  debounce_delays <- system_config$system_settings$DEBOUNCE_DELAYS
  expect_type(debounce_delays, "list")
  expect_true(all(sapply(debounce_delays, function(x) x > 0 && x < 2000)))
})

test_that("konstanter konsistens via registry", {
  config_registry <- create_config_registry()

  # Test chart types consistency
  charts_config <- config_registry$get("charts")
  expected_charts <- c("I-kort (Individuelle værdier)", "P-kort (Andele)", "C-kort (Tællinger)")
  expect_true(all(expected_charts %in% names(charts_config$chart_types_da)))

  # Test at UI helpers har fornuftige strukturer
  ui_config <- config_registry$get("ui")
  expect_type(ui_config$ui_column_widths, "list")
  expect_true("quarter" %in% names(ui_config$ui_column_widths))

  # Test that TEST_MODE_AUTO_LOAD environment variable handling works
  TEST_MODE_AUTO_LOAD <- as.logical(Sys.getenv("TEST_MODE_AUTO_LOAD", "TRUE"))
  expect_type(TEST_MODE_AUTO_LOAD, "logical")
  expect_true(TEST_MODE_AUTO_LOAD %in% c(TRUE, FALSE))
})

test_that("UI helpers module funktionalitet via registry", {
  config_registry <- create_config_registry()
  ui_config <- config_registry$get("ui")

  # Test UI config indlæsning
  expect_true("ui_column_widths" %in% names(ui_config))
  expect_true("ui_heights" %in% names(ui_config))
  expect_true("ui_styles" %in% names(ui_config))

  # Test config funktionalitet
  expect_equal(ui_config$ui_column_widths$quarter, c(6, 6, 6, 6))
  expect_equal(ui_config$ui_column_widths$half, c(6, 6))
  expect_equal(ui_config$ui_heights$logo, "40px")
  expect_match(ui_config$ui_styles$flex_column, "display: flex")
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
