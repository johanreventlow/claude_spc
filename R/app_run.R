# run_app.R
# Main app launcher following Golem conventions

#' Run the SPC Shiny Application
#'
#' @param port Port number for the application (default: NULL for auto-assignment)
#' @param launch_browser TRUE for browser, FALSE for no launch, or leave default for environment-aware decision
#' @param options List of application options (golem-style configuration)
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
                    ...) {

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

  # Run the app with proper port and browser handling
  if (is.null(port)) {
    shiny::runApp(
      app,
      launch.browser = launch_browser %||% TRUE,
      ...
    )
  } else {
    shiny::runApp(
      app,
      port = port,
      launch.browser = launch_browser %||% TRUE,
      ...
    )
  }
}

