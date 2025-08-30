# R/server/server_reactive_values.R
# Initialize reactive values

initialize_reactive_values <- function() {
  reactiveValues(
    current_data = NULL,
    original_data = NULL,
    file_uploaded = FALSE,
    user_started_session = FALSE,  # NEW: Track if user has actively started
    hide_anhoej_rules = FALSE,    # NEW: Flag to force hide Anhøj rules
    
    updating_table = FALSE,
    auto_detect_done = FALSE,
    # Auto-save related values with restore guard
    auto_save_enabled = TRUE,
    last_save_time = NULL,
    restoring_session = FALSE,  # Guard for session restore
    table_version = 0           # Force table re-render counter
  )
}
