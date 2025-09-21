# run_app.R
# Main app launcher following Golem conventions

#' Run the SPC Shiny Application
#' 
#' @param port Port number for the application (default: 3838)
#' @param launch_browser TRUE for browser, FALSE for no launch, or leave default for RStudio Viewer
#' @param ... Additional arguments to pass to shinyApp()
#'
#' @export
run_app <- function(#port = 3838,
                    port = NULL,
                    launch_browser = NULL, ...) {
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

  # Determine launch behavior with environment-aware fallback
  browser_option <- if (is.null(launch_browser)) {
    # Check if we're in a development environment where RStudio internals are safe
    is_development <- (
      Sys.getenv("RSTUDIO") == "1" ||                 # RStudio IDE
      Sys.getenv("R_CONFIG_ACTIVE") == "development"  # Explicit development
    )

    # Only use RStudio internals in development environments
    if (is_development && rstudioapi::isAvailable()) {
      # Try RStudio viewer with graceful fallback
      tryCatch({
        .rs.invokeShinyWindowViewer
      }, error = function(e) {
        # If RStudio internals fail, fall back to browser
        TRUE
      })
    } else {
      # Production/test environments: use standard browser launch
      TRUE
    }
  } else {
    launch_browser
  }

  # Run the app
  runApp(
    app,
    # port = port,
    launch.browser = browser_option
  )
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