# setup-shinytest2.R
# Sprint 5 Fase 1: shinytest2 Integration Setup
# Configuration and helpers for UI testing with shinytest2

# Check if shinytest2 is available
if (!requireNamespace("shinytest2", quietly = TRUE)) {
  message("shinytest2 not installed. Install with: install.packages('shinytest2')")
  message("UI integration tests will be skipped.")
}

#' Create Shiny App Driver for Testing
#'
#' Creates a shinytest2 AppDriver instance for UI testing.
#'
#' @param app_dir Path to app directory (default: project root)
#' @param timeout Timeout in seconds for app startup (default: 10)
#' @param load_timeout Timeout for individual operations (default: 5)
#'
#' @return AppDriver instance or NULL if shinytest2 not available
#'
#' @examples
#' \dontrun{
#' app <- create_test_app_driver()
#' app$set_inputs(file_upload = "test_data.csv")
#' app$wait_for_idle()
#' app$stop()
#' }
create_test_app_driver <- function(app_dir = ".", timeout = 10, load_timeout = 5) {
  if (!requireNamespace("shinytest2", quietly = TRUE)) {
    message("shinytest2 not available - skipping UI test")
    return(NULL)
  }

  app <- shinytest2::AppDriver$new(
    app_dir = app_dir,
    name = "spc-app-test",
    timeout = timeout * 1000,  # Convert to milliseconds
    load_timeout = load_timeout * 1000
  )

  return(app)
}

#' Wait for App Element
#'
#' Waits for a specific UI element to appear.
#'
#' @param app AppDriver instance
#' @param selector CSS selector for element
#' @param timeout Timeout in seconds (default: 5)
#'
#' @return TRUE if element found, FALSE otherwise
wait_for_element <- function(app, selector, timeout = 5) {
  if (is.null(app)) return(FALSE)

  start_time <- Sys.time()
  while (Sys.time() - start_time < timeout) {
    element <- app$get_html(selector)
    if (!is.null(element) && nchar(element) > 0) {
      return(TRUE)
    }
    Sys.sleep(0.1)
  }

  return(FALSE)
}

#' Simulate File Upload in Test
#'
#' Simulates file upload by setting input value.
#'
#' @param app AppDriver instance
#' @param input_id Input ID for file upload
#' @param file_path Path to test file
#'
#' @return TRUE if successful
simulate_file_upload <- function(app, input_id, file_path) {
  if (is.null(app)) return(FALSE)

  if (!file.exists(file_path)) {
    warning(paste("Test file not found:", file_path))
    return(FALSE)
  }

  app$upload_file(!!input_id := file_path)
  app$wait_for_idle()

  return(TRUE)
}

#' Check for Error Messages
#'
#' Checks if error messages are displayed in the UI.
#'
#' @param app AppDriver instance
#' @param selector CSS selector for error container (default: ".shiny-notification-error")
#'
#' @return TRUE if errors found, FALSE otherwise
check_for_errors <- function(app, selector = ".shiny-notification-error") {
  if (is.null(app)) return(FALSE)

  error_html <- app$get_html(selector)
  return(!is.null(error_html) && nchar(error_html) > 0)
}

#' Get App State Value
#'
#' Retrieves a value from app's reactive state for testing.
#'
#' @param app AppDriver instance
#' @param output_id Output ID to retrieve
#'
#' @return Output value or NULL
get_app_state <- function(app, output_id) {
  if (is.null(app)) return(NULL)

  value <- app$get_value(output = output_id)
  return(value)
}

#' Create Test Data File
#'
#' Creates a temporary test CSV file for upload testing.
#'
#' @param data Data frame to write
#' @param filename Filename (default: "test_data.csv")
#'
#' @return Path to created file
create_test_data_file <- function(data, filename = "test_data.csv") {
  temp_dir <- tempdir()
  file_path <- file.path(temp_dir, filename)

  write.csv(data, file_path, row.names = FALSE, fileEncoding = "UTF-8")

  return(file_path)
}

#' Cleanup Test Files
#'
#' Removes temporary test files created during testing.
#'
#' @param file_paths Character vector of file paths to remove
cleanup_test_files <- function(file_paths) {
  for (path in file_paths) {
    if (file.exists(path)) {
      unlink(path)
    }
  }
}
