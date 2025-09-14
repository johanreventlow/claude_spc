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
  
  # Extract the UI definition from the sourced file
  # The app_ui.R contains all UI components concatenated
  # We need to return a proper UI structure
  
  # Since app_ui.R is a concatenated file, we need to source it and 
  # extract the main UI function that should be defined there
  # For now, we'll use the existing ui.R structure
  source("ui.R", local = TRUE)
  return(ui)
}