# run_app.R
# Main app launcher following Golem conventions

#' Run the SPC Shiny Application
#'
#' @param port Port number for the application (default: NULL for auto-assignment)
#' @param launch_browser TRUE for browser, FALSE for no launch, or leave default for environment-aware decision
#' @param options List of application options (golem-style configuration)
#' @param ... Additional arguments to pass to shinyApp()
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
#' @export
run_app <- function(port = NULL,
                    launch_browser = NULL,
                    options = list(),
                    ...) {
  # Golem-style options management
  set_app_options(options)

  # Ensure global configuration is loaded (needed for package entry point)
  if (!exists("HOSPITAL_NAME", envir = .GlobalEnv) ||
      !exists("my_theme", envir = .GlobalEnv) ||
      !exists("HOSPITAL_LOGO_PATH", envir = .GlobalEnv)) {
    source("global.R", local = FALSE)
  }

  # Server function now loaded globally for better performance
  
  # Setup static resource paths for Golem structure
  shiny::addResourcePath("www", "www")
  
  # Create the Shiny app
  app <- shinyApp(
    ui = app_ui(),
    server = app_server,
    ...
  )

  # Determine launch behavior using golem-style options and environment detection
  browser_option <- if (is.null(launch_browser)) {
    # Check golem-style options first
    option_browser <- get_app_option("browser_launch", NULL)
    if (!is.null(option_browser)) {
      if (option_browser == "rstudio_viewer" && rstudioapi::isAvailable()) {
        tryCatch({
          .rs.invokeShinyWindowViewer
        }, error = function(e) {
          log_debug(paste("RStudio viewer not available, falling back to browser:", e$message), "APP_LAUNCH")
          TRUE
        })
      } else if (option_browser == "browser") {
        TRUE
      } else {
        FALSE
      }
    } else {
      # Fall back to environment-aware detection
      if (exists("is_prod_mode", mode = "function") && is_prod_mode()) {
        # Production: Always use standard browser launch for stability
        TRUE
      } else if (exists("is_dev_mode", mode = "function") && is_dev_mode() && rstudioapi::isAvailable()) {
        # Development: Try RStudio viewer with graceful fallback
        tryCatch({
          .rs.invokeShinyWindowViewer
        }, error = function(e) {
          log_debug(paste("RStudio viewer not available, using browser:", e$message), "APP_LAUNCH")
          TRUE
        })
      } else {
        # Other environments: use standard browser launch
        TRUE
      }
    }
  } else {
    launch_browser
  }

  # Run the app
  if (is.null(port)) {
    runApp(
      app,
      launch.browser = browser_option
    )
  } else {
    runApp(
      app,
      port = port,
      launch.browser = browser_option
    )
  }
}

#' Get the UI function
#' @noRd
app_ui <- function() {
  # UI components now loaded globally for better performance

  # Return the UI structure using consolidated functions
  page_navbar(
    title = tagList(
      img(
        src = HOSPITAL_LOGO_PATH,
        height = "40px",
        style = "margin-right: 10px;",
        onerror = "this.style.display='none'"
      ),
      div("BFH SPC-værktøj", style = "position: absolute; right: 20px; top: 20px; font-weight: bold;")
    ),
    theme = my_theme,
    navbar_options = navbar_options(theme = "light", underline = FALSE),

    # Header-komponenter
    header = create_ui_header(),

    # Sidebar
    sidebar = create_ui_sidebar(),

    # Hovedindhold
    nav_panel(
      title = NULL,
      create_ui_main_content()
    )
  )
}