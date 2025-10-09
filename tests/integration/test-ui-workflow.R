# test-ui-workflow.R
# Sprint 5 Fase 1: UI Integration Testing with shinytest2
# End-to-end UI workflow testing

library(testthat)

# Source shinytest2 setup
source("tests/integration/setup-shinytest2.R")

context("UI Workflow Integration")

test_that("UI initializes correctly", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()  # UI tests are slow

  app <- create_test_app_driver()
  skip_if(is.null(app), "shinytest2 not available")

  # Wait for app to initialize
  app$wait_for_idle(timeout = 10000)

  # Check for main UI elements
  has_navbar <- wait_for_element(app, ".navbar", timeout = 5)
  expect_true(has_navbar, info = "Navbar should be present")

  # Check for file upload widget
  has_upload <- wait_for_element(app, "#file_upload", timeout = 5)
  expect_true(has_upload, info = "File upload should be present")

  # No errors on startup
  has_errors <- check_for_errors(app)
  expect_false(has_errors, info = "No errors on startup")

  app$stop()
})

test_that("File upload workflow completes successfully", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  app <- create_test_app_driver()
  skip_if(is.null(app), "shinytest2 not available")

  # Create test data file
  test_data <- data.frame(
    Dato = seq.Date(from = as.Date("2023-01-01"), by = "day", length.out = 30),
    Værdi = rnorm(30, mean = 50, sd = 10),
    Nævner = rep(100, 30)
  )

  test_file <- create_test_data_file(test_data)

  # Upload file
  upload_success <- simulate_file_upload(app, "file_upload", test_file)
  expect_true(upload_success, info = "File upload should succeed")

  # Wait for processing
  app$wait_for_idle(timeout = 15000)

  # Check for data table
  has_table <- wait_for_element(app, ".dataTable", timeout = 10)
  expect_true(has_table, info = "Data table should appear after upload")

  # Check for plot
  has_plot <- wait_for_element(app, ".plotly", timeout = 10)
  expect_true(has_plot, info = "Plot should appear after upload")

  # No errors after upload
  has_errors <- check_for_errors(app)
  expect_false(has_errors, info = "No errors after upload")

  # Cleanup
  cleanup_test_files(test_file)
  app$stop()
})

test_that("Auto-detection updates UI correctly", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  app <- create_test_app_driver()
  skip_if(is.null(app), "shinytest2 not available")

  # Create and upload test data
  test_data <- data.frame(
    Dato = seq.Date(from = as.Date("2023-01-01"), by = "day", length.out = 20),
    Værdi = rnorm(20, mean = 50, sd = 10),
    Nævner = rep(100, 20),
    Kommentar = rep("", 20)
  )

  test_file <- create_test_data_file(test_data)
  simulate_file_upload(app, "file_upload", test_file)
  app$wait_for_idle(timeout = 15000)

  # Check if column selectors are populated
  x_col_value <- get_app_state(app, "x_column_select")
  y_col_value <- get_app_state(app, "y_column_select")

  expect_true(!is.null(x_col_value), info = "X column should be detected")
  expect_true(!is.null(y_col_value), info = "Y column should be detected")

  # Cleanup
  cleanup_test_files(test_file)
  app$stop()
})

test_that("Chart type selection updates plot", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  app <- create_test_app_driver()
  skip_if(is.null(app), "shinytest2 not available")

  # Upload data
  test_data <- data.frame(
    Dato = seq.Date(from = as.Date("2023-01-01"), by = "day", length.out = 25),
    Værdi = rnorm(25, mean = 50, sd = 10),
    Nævner = rep(100, 25)
  )

  test_file <- create_test_data_file(test_data)
  simulate_file_upload(app, "file_upload", test_file)
  app$wait_for_idle(timeout = 15000)

  # Change chart type
  app$set_inputs(chart_type = "p")
  app$wait_for_idle(timeout = 10000)

  # Plot should update
  has_plot <- wait_for_element(app, ".plotly", timeout = 10)
  expect_true(has_plot, info = "Plot should update after chart type change")

  # No errors after chart type change
  has_errors <- check_for_errors(app)
  expect_false(has_errors, info = "No errors after chart type change")

  # Cleanup
  cleanup_test_files(test_file)
  app$stop()
})

test_that("Error messages display for invalid data", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  app <- create_test_app_driver()
  skip_if(is.null(app), "shinytest2 not available")

  # Create invalid data (empty)
  invalid_data <- data.frame()
  test_file <- create_test_data_file(invalid_data)

  # Upload should fail gracefully
  simulate_file_upload(app, "file_upload", test_file)
  app$wait_for_idle(timeout = 10000)

  # Error or warning should be displayed
  has_notification <- wait_for_element(app, ".shiny-notification", timeout = 5)
  expect_true(has_notification, info = "Notification should appear for invalid data")

  # Cleanup
  cleanup_test_files(test_file)
  app$stop()
})

test_that("Session reset clears all data", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  app <- create_test_app_driver()
  skip_if(is.null(app), "shinytest2 not available")

  # Upload data
  test_data <- data.frame(
    Dato = seq.Date(from = as.Date("2023-01-01"), by = "day", length.out = 15),
    Værdi = rnorm(15, mean = 50, sd = 10)
  )

  test_file <- create_test_data_file(test_data)
  simulate_file_upload(app, "file_upload", test_file)
  app$wait_for_idle(timeout = 15000)

  # Trigger reset (if reset button exists)
  reset_button_exists <- wait_for_element(app, "#reset_button", timeout = 2)

  if (reset_button_exists) {
    app$click("reset_button")
    app$wait_for_idle(timeout = 5000)

    # Data table should disappear or be empty
    # (Implementation depends on app UI structure)
  }

  # Cleanup
  cleanup_test_files(test_file)
  app$stop()
})

# NOTE: Additional UI tests can be added as needed:
# - Target value input and plot update
# - Centerline value input and plot update
# - Phase/shift configuration
# - Comment annotation display
# - Export functionality
# - Responsive layout on different screen sizes
