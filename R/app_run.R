# run_app.R
# Main app launcher following Golem conventions

#' Run the SPC Shiny Application
#'
#' @param port Port number for the application (default: NULL for auto-assignment)
#' @param launch_browser TRUE for browser, FALSE for no launch, or leave default for environment-aware decision
#' @param options List of application options (golem-style configuration)
#' @param enable_test_mode TRUE/FALSE to force test mode, or NULL for auto-detection based on environment
#' @param log_level Set logging level for the session: "DEBUG", "INFO", "WARN", "ERROR", or NULL for auto-detection
#' @param ... Additional arguments to pass to shiny::shinyApp()
#'
#' @details
#' This function implements golem-inspired patterns for robust Shiny app deployment.
#' It supports environment-aware configuration and graceful fallbacks for different
#' deployment contexts (development, production, testing).
#'
#' Log levels determine what messages are displayed:
#' \describe{
#'   \item{DEBUG}{All messages (most verbose) - ideal for troubleshooting reactive flows}
#'   \item{INFO}{Informational messages and above - good for monitoring app behavior}
#'   \item{WARN}{Warnings and errors only - standard production setting}
#'   \item{ERROR}{Only error messages - minimal logging}
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' run_app()
#'
#' # Development with debug logging
#' run_app(port = 4040, log_level = "DEBUG")
#'
#' # Troubleshooting with detailed logging
#' run_app(log_level = "DEBUG", enable_test_mode = TRUE)
#'
#' # Production with minimal logging
#' run_app(log_level = "WARN", launch_browser = TRUE)
#' }
#'
#' @import shiny
#' @import golem
#' @importFrom golem with_golem_options
#' @export
run_app <- function(port = NULL,
                    launch_browser = NULL,
                    options = list(),
                    enable_test_mode = NULL,
                    log_level = NULL,
                    ...) {

  # Configure logging level
  if (!is.null(log_level)) {
    # Validate and set log level
    valid_levels <- c("DEBUG", "INFO", "WARN", "ERROR")
    log_level <- toupper(trimws(log_level))

    if (log_level %in% valid_levels) {
      Sys.setenv(SPC_LOG_LEVEL = log_level)
      cat(sprintf("[LOG_CONFIG] Log level set to %s for this session\n", log_level))
    } else {
      warning(sprintf("Invalid log level '%s'. Valid options: %s",
                     log_level, paste(valid_levels, collapse = ", ")))
      cat("[LOG_CONFIG] Using default log level\n")
    }
  } else {
    # Auto-detect log level based on context
    if (interactive() || Sys.getenv("GOLEM_CONFIG_ACTIVE", "production") == "development") {
      if (Sys.getenv("SPC_LOG_LEVEL", "") == "") {
        Sys.setenv(SPC_LOG_LEVEL = "INFO")
        cat("[LOG_CONFIG] Auto-detected development environment - using INFO level\n")
      }
    } else {
      if (Sys.getenv("SPC_LOG_LEVEL", "") == "") {
        Sys.setenv(SPC_LOG_LEVEL = "WARN")
        cat("[LOG_CONFIG] Auto-detected production environment - using WARN level\n")
      }
    }
  }

  # Configure test mode based on context
  if (is.null(enable_test_mode)) {
    # Auto-detect: enable test mode in development environments
    enable_test_mode <- interactive() || Sys.getenv("GOLEM_CONFIG_ACTIVE", "production") == "development"
  }

  if (enable_test_mode) {
    # Enable test mode with automatic data loading
    Sys.setenv(TEST_MODE_AUTO_LOAD = "TRUE")
    Sys.setenv(GOLEM_CONFIG_ACTIVE = "development")
    Sys.setenv(TEST_MODE_FILE_PATH = "inst/extdata/spc_exampledata.csv")

    # Update package configuration to ensure it takes effect
    if (exists("get_claudespc_environment", mode = "function")) {
      claudespc_env <- get_claudespc_environment()
      claudespc_env$TEST_MODE_AUTO_LOAD <- TRUE
      claudespc_env$TEST_MODE_FILE_PATH <- "inst/extdata/spc_exampledata.csv"
    }
  }

  # Create the Shiny app using golem pattern
  # Load golem function directly to avoid package conflicts
  if (!exists("with_golem_options")) {
    with_golem_options <- golem::with_golem_options
  }

  app <- with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui,
      server = app_server
    ),
    golem_opts = options
  )

  # Set default browser behavior if not specified
  if (is.null(launch_browser)) {
    launch_browser <- getOption("shiny.launch.browser", TRUE)
  }

  # Run the app with proper port and browser handling
  if (is.null(port)) {
    shiny::runApp(
      app,
      launch.browser = launch_browser,
      ...
    )
  } else {
    shiny::runApp(
      app,
      port = port,
      launch.browser = launch_browser,
      ...
    )
  }
}

