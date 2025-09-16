# app_server.R  
# Main server function following Golem conventions

#' Main Server Function
#'
#' @param input,output,session Internal Shiny parameters
#' 
#' @noRd
app_server <- function(input, output, session) {
  cat("DEBUG: [APP_SERVER] ===========================================\n")
  cat("DEBUG: [APP_SERVER] Starting main server function\n")
  cat("DEBUG: [APP_SERVER] Session ID:", session$token, "\n")

  # Source all required server components
  cat("DEBUG: [APP_SERVER] Sourcing server components...\n")
  source("R/utils_reactive_state.R", local = TRUE)
  source("R/utils_session_helpers.R", local = TRUE)
  source("R/utils_server_management.R", local = TRUE)
  source("R/fct_data_processing.R", local = TRUE)
  source("R/fct_file_operations.R", local = TRUE)
  source("R/fct_visualization_server.R", local = TRUE)
  source("R/mod_spc_chart.R", local = TRUE)
  cat("DEBUG: [APP_SERVER] ✅ All server components sourced\n")

  # Reaktive Værdier --------------------------------------------------------
  # Initialiser reaktive værdier
  cat("DEBUG: [APP_SERVER] Initializing reactive values...\n")
  values <- initialize_reactive_values()
  cat("DEBUG: [APP_SERVER] ✅ Reactive values initialized\n")

  # PHASE 4: Centraliseret state management (parallel til existing values)
  cat("DEBUG: [APP_SERVER] Initializing centralized app state...\n")
  app_state <- create_app_state()
  cat("DEBUG: [APP_SERVER] ✅ Centralized state initialized\n")

  # Test Tilstand ------------------------------------------------------------
  # TEST MODE: Auto-indlæs eksempel data hvis aktiveret
  cat("DEBUG: [APP_SERVER] Checking TEST_MODE configuration...\n")
  cat("DEBUG: [APP_SERVER] TEST_MODE_AUTO_LOAD:", if(exists("TEST_MODE_AUTO_LOAD")) TEST_MODE_AUTO_LOAD else "UNDEFINED", "\n")

  if (exists("TEST_MODE_AUTO_LOAD") && TEST_MODE_AUTO_LOAD) {
    cat("DEBUG: [TEST_MODE] 🔄 Attempting auto-load with TEST_MODE_AUTO_LOAD =", TEST_MODE_AUTO_LOAD, "\n")
    test_file_path <- if(exists("TEST_MODE_FILE_PATH")) TEST_MODE_FILE_PATH else "UNDEFINED"
    cat("DEBUG: [TEST_MODE] Test file path:", test_file_path, "\n")

    if (exists("TEST_MODE_FILE_PATH") && file.exists(test_file_path)) {
      cat("DEBUG: [TEST_MODE] ✅ Test file found, starting auto-load...\n")
      tryCatch(
        {
          # Bestem hvilken loader der skal bruges baseret på fil-extension
          file_extension <- tools::file_ext(test_file_path)

          if (file_extension %in% c("xlsx", "xls")) {
            # Load Excel file
            test_data <- readxl::read_excel(
              test_file_path,
              sheet = 1, # Læs første sheet
              .name_repair = "minimal"
            )
          } else {
            # Load CSV file using readr::read_csv2 (same as working file upload)
            test_data <- readr::read_csv2(
              test_file_path,
              locale = readr::locale(
                decimal_mark = ",",
                grouping_mark = ".",
                encoding = "ISO-8859-1"
              ),
              show_col_types = FALSE
            )
          }

          # Ensure standard columns are present
          test_data <- ensure_standard_columns(test_data)

          # Set reactive values
          values$current_data <- test_data
          values$original_data <- test_data
          # PHASE 4: Sync to both old and new state management
          values$file_uploaded <- TRUE
          app_state$session$file_uploaded <- TRUE
          # PHASE 4: Sync to both old and new state management
          values$user_started_session <- TRUE # Ensure dataLoaded triggers correctly
          app_state$session$user_started_session <- TRUE
          values$auto_detect_done <- FALSE # Will trigger auto-detect
          values$initial_auto_detect_completed <- FALSE # Reset for new data

          values$hide_anhoej_rules <- FALSE # Show Anhøj rules for real data

          # NOTE: Flag sættes efter setup_column_management() for at undgå race condition

          # Debug output
          cat("TEST MODE: Auto-indlæst fil:", test_file_path, "\n")
          cat("TEST MODE: Data dimensioner:", nrow(test_data), "x", ncol(test_data), "\n")
          cat("TEST MODE: Kolonner:", paste(names(test_data), collapse = ", "), "\n")
        },
        error = function(e) {
          cat("TEST MODE: Fejl ved indlæsning af", test_file_path, ":", e$message, "\n")
        }
      )
    } else {
      cat("TEST MODE: Fil ikke fundet:", test_file_path, "\n")
    }
  }

  # Observer Management ------------------------------------------------------
  # Initialiser observer manager til tracking af alle observers
  obs_manager <- observer_manager()

  # Waiter Konfiguration -----------------------------------------------------
  # Initialiser fil upload waiter
  waiter_file <- waiter::Waiter$new(
    html = WAITER_CONFIG$file_upload$html,
    color = WAITER_CONFIG$file_upload$color
  )

  # Server Setup ------------------------------------------------------------
  # Opsæt alle server-komponenter

  ## Velkomstside interaktioner
  setup_welcome_page_handlers(input, output, session, values, waiter_file)

  ## Session management logik
  setup_session_management(input, output, session, values, waiter_file, app_state)

  ## Fil upload logik
  setup_file_upload(input, output, session, values, waiter_file, app_state)

  ## Data tabel logik
  setup_data_table(input, output, session, values)

  ## Kolonne management logik
  # PHASE 4: Pass centralized state to column management
  setup_column_management(input, output, session, values, app_state)

  ## Visualiserings logik
  visualization <- setup_visualization(input, output, session, values)

  ## Download handlers
  setup_download_handlers(input, output, session, values)

  ## Hjælpe observers
  setup_helper_observers(input, output, session, values, obs_manager, app_state)

  # TEST MODE: Set auto-detect trigger flag AFTER all observers are set up
  if (TEST_MODE_AUTO_LOAD) {
    observe({
      if (!is.null(values$current_data)) {
        cat("TEST MODE: Setting test_mode_auto_detect_ready flag after setup\n")
        timestamp <- Sys.time()

        # PHASE 4: Gradual migration - sync to both old and new state
        values$test_mode_auto_detect_ready <- timestamp
        app_state$test_mode$auto_detect_ready <- timestamp
        cat("DEBUG: [PHASE4] Synced test_mode_auto_detect_ready to both systems\n")
      }
    }) %>% bindEvent(values$current_data, once = TRUE, ignoreNULL = TRUE)
  }

  # Initial UI Setup --------------------------------------------------------
  # Sæt standard chart_type når appen starter
  observe({
    cat("DEBUG: [APP_SERVER] Setting initial chart_type to 'run'\n")
    updateSelectizeInput(session, "chart_type", selected = "run")
    cat("DEBUG: [APP_SERVER] ✅ Initial chart_type set\n")
  }) %>%
    bindEvent(TRUE, once = TRUE)

  # Session Cleanup ---------------------------------------------------------
  # Opsæt cleanup af observers og ressourcer når session lukker
  setup_session_cleanup(session, list(
    function() {
      # Cleanup alle observers
      obs_manager$cleanup_all()
    },
    function() {
      # Cleanup waiter
      if (exists("waiter_file") && !is.null(waiter_file)) {
        waiter_file$hide()
      }
    },
    function() {
      # Log session statistics
      log_error(paste("Session afsluttet - Observer count:", obs_manager$count()),
        level = "info"
      )
    }
  ))
}