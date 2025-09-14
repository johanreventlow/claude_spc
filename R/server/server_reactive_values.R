# server_reactive_values.R
# Initialisering af reaktive værdier til server state management

# Dependencies ----------------------------------------------------------------
# Ingen direkte dependencies - kun shiny reactive system

# REACTIVE VALUES INITIALISERING ==============================================

## Initialiser reaktive værdier
initialize_reactive_values <- function() {
  reactiveValues(
    current_data = NULL,
    original_data = NULL,
    file_uploaded = FALSE,
    user_started_session = FALSE, # NEW: Track if user has actively started
    hide_anhoej_rules = FALSE, # NEW: Flag to force hide Anhøj rules

    updating_table = FALSE,
    table_operation_in_progress = FALSE, # Persistent flag for table operations
    auto_detect_done = FALSE,
    initial_auto_detect_completed = FALSE,
    auto_detect_in_progress = FALSE,
    # Auto-save related values with restore guard
    auto_save_enabled = TRUE,
    last_save_time = NULL,
    restoring_session = FALSE, # Guard for session restore
    table_version = 0 # Force table re-render counter
  )
}
