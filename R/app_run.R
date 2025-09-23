# run_app.R
# Main app launcher following Golem conventions

#' Run the SPC Shiny Application
#'
#' @param port Port number for the application (default: NULL for auto-assignment)
#' @param launch_browser TRUE for browser, FALSE for no launch, or leave default for environment-aware decision
#' @param options List of application options (golem-style configuration)
#' @param enable_test_mode TRUE/FALSE to force test mode, or NULL for auto-detection based on environment
#' @param ... Additional arguments to pass to shiny::shinyApp()
#'
#' @details
#' This function implements golem-inspired patterns for robust Shiny app deployment.
#' It supports environment-aware configuration and graceful fallbacks for different
#' deployment contexts (development, production, testing).
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' run_app()
#'
#' # Development with specific options
#' run_app(port = 4040, options = list(test_mode = TRUE))
#'
#' # Production deployment
#' run_app(launch_browser = TRUE, options = list(production_mode = TRUE))
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
                    ...) {

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

