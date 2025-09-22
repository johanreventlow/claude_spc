# app.R
# Package-based entry point for SPC App

devtools::load_all()

# PACKAGE LOADING =============================================================
# Load the package instead of sourcing files
library(claudespc)

# Run the application using package-loaded function
run_app()

