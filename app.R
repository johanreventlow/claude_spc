# app.R
# Development entry point for SPC App

# Load package
devtools::load_all(reset = TRUE, recompile = FALSE, helpers = FALSE)

# Run app with test mode enabled for development
# run_app(enable_test_mode = TRUE, log_level = "DEBUG")
run_app(enable_test_mode = FALSE, log_level = "INFO")
