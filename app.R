# app.R
# Development entry point for SPC App

# Clean development environment - remove any potential conflicts
if (exists("app_server") && !is.function(app_server)) rm(app_server)
if (exists("app_ui") && !is.function(app_ui)) rm(app_ui)
if (exists("run_app") && !is.function(run_app)) rm(run_app)

# Use devtools for proper R package development
if (requireNamespace("devtools", quietly = TRUE)) {
  message("Using devtools::load_all() for development")

  # Clear any existing package if loaded to avoid conflicts
  if ("package:SPCify" %in% search()) {
    detach("package:SPCify", unload = TRUE, force = TRUE)
  }

  # Load package from source using devtools - the correct way
  devtools::load_all(
    reset = TRUE,          # Reset namespace to avoid conflicts
    recompile = FALSE,     # No compilation needed for pure R code
    helpers = FALSE        # Don't load testthat helpers
  )

} else {
  # Fallback for environments without devtools
  message("devtools not available - using source-based loading")
  source("global.R")
}

# Run the application
if (exists("run_app")) {
  run_app()
} else if (exists("app_ui") && exists("app_server")) {
  message("Using app_ui and app_server functions")
  shiny::shinyApp(ui = app_ui, server = app_server)
} else {
  stop("Unable to find app functions. Check devtools installation or package structure.")
}

