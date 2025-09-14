# run_app.R
# Main app launcher following Golem conventions

#' Run the SPC Shiny Application
#' 
#' @param port Port number for the application (default: 3838)
#' @param launch_browser Whether to launch browser (default: TRUE)
#' @param ... Additional arguments to pass to shinyApp()
#'
#' @export
run_app <- function(port = 3838, launch_browser = TRUE, ...) {
  # Ensure global configuration is loaded
  if (!exists("HOSPITAL_NAME", envir = .GlobalEnv)) {
    source("global.R", local = FALSE)
  }
  
  # Load server function
  source("R/app_server.R", local = FALSE)
  
  # Setup static resource paths for Golem structure
  shiny::addResourcePath("www", "www")
  
  # Create the Shiny app
  app <- shinyApp(
    ui = app_ui(),
    server = app_server,
    ...
  )
  
  # Run the app
  runApp(
    app,
    port = port,
    launch.browser = launch_browser
  )
}

#' Get the UI function
#' @noRd
app_ui <- function() {
  source("R/app_ui.R", local = TRUE)
  
  # Return the UI structure using consolidated functions
  page_navbar(
    title = tagList(
      img(
        src = utils::URLencode(basename(HOSPITAL_LOGO_PATH)),
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