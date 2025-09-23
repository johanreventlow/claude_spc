# app.R
# Package-based entry point for SPC App

devtools::load_all()

# DEVELOPMENT MODE LOADING ====================================================
# NOTE: Using library(claudespc) creates circular dependency
# Use devtools::load_all() for development or source global.R

# Load global configuration
source("global.R")

# Run the application using package-loaded function
if (exists("run_app")) {
  run_app()
} else {
  # Fallback to basic shiny app loading
  message("run_app() not found, loading basic shiny app")
  shiny::shinyApp(ui = app_ui, server = app_server)
}

