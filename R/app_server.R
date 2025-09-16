# app_server.R  
# Main server function following Golem conventions

#' Main Server Function
#'
#' @param input,output,session Internal Shiny parameters
#' 
#' @noRd
app_server <- function(input, output, session) {
  log_debug("===========================================", "APP_SERVER")
  log_debug("Starting main server function", "APP_SERVER")
  log_debug(paste("Session ID:", session$token), "APP_SERVER")

  # Source all required server components
  log_debug("Sourcing server components...", "APP_SERVER")
  source("R/utils_reactive_state.R", local = TRUE)
  source("R/utils_session_helpers.R", local = TRUE)
  source("R/utils_server_management.R", local = TRUE)
  source("R/fct_data_processing.R", local = TRUE)
  source("R/fct_file_operations.R", local = TRUE)
  source("R/fct_visualization_server.R", local = TRUE)
  source("R/mod_spc_chart.R", local = TRUE)
  log_debug("✅ All server components sourced", "APP_SERVER")

  # Reaktive Værdier --------------------------------------------------------
  # Initialiser reaktive værdier
  log_debug("Initializing reactive values...", "APP_SERVER")
  values <- initialize_reactive_values()
  log_debug("✅ Reactive values initialized", "APP_SERVER")

  # PHASE 4: Centraliseret state management (parallel til existing values)
  log_debug("Initializing centralized app state...", "APP_SERVER")
  app_state <- create_app_state()
  log_debug("✅ Centralized state initialized", "APP_SERVER")

  # Test Tilstand ------------------------------------------------------------
  # TEST MODE: Auto-indlæs eksempel data hvis aktiveret
  log_debug("Checking TEST_MODE configuration...", "APP_SERVER")
  log_debug(paste("TEST_MODE_AUTO_LOAD:", if(exists("TEST_MODE_AUTO_LOAD")) TEST_MODE_AUTO_LOAD else "UNDEFINED"), "APP_SERVER")

  if (exists("TEST_MODE_AUTO_LOAD") && TEST_MODE_AUTO_LOAD) {
    log_debug(paste("🔄 Attempting auto-load with TEST_MODE_AUTO_LOAD =", TEST_MODE_AUTO_LOAD), "TEST_MODE")
    test_file_path <- if(exists("TEST_MODE_FILE_PATH")) TEST_MODE_FILE_PATH else "UNDEFINED"
    log_debug(paste("Test file path:", test_file_path), "TEST_MODE")

    if (exists("TEST_MODE_FILE_PATH") && file.exists(test_file_path)) {
      log_debug("✅ Test file found, starting auto-load...", "TEST_MODE")
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
                encoding = DEFAULT_ENCODING
              ),
              show_col_types = FALSE
            )
          }

          # Ensure standard columns are present
          test_data <- ensure_standard_columns(test_data)

          # Set reactive values
          values$current_data <- test_data
          # PHASE 4: Sync original_data to both old and new state management
          values$original_data <- test_data
          if (exists("use_centralized_state") && use_centralized_state && exists("app_state")) {
            app_state$data$original_data <- test_data
          }
          # PHASE 4: Sync to both old and new state management
          app_state$data$current_data <- test_data
          # PHASE 4: original_data er nu tilføjet til centralized state schema
          values$file_uploaded <- TRUE
          app_state$session$file_uploaded <- TRUE
          # PHASE 4: Sync to both old and new state management
          values$user_started_session <- TRUE # Ensure dataLoaded triggers correctly
          app_state$session$user_started_session <- TRUE
          # PHASE 4: Sync to both old and new state management
          values$auto_detect_done <- FALSE # Will trigger auto-detect
          app_state$columns$auto_detect$completed <- FALSE
          values$initial_auto_detect_completed <- FALSE # Reset for new data

          # PHASE 4: Sync to both old and new state management
          values$hide_anhoej_rules <- FALSE # Show Anhøj rules for real data
          app_state$ui$hide_anhoej_rules <- FALSE

          # NOTE: Flag sættes efter setup_column_management() for at undgå race condition

          # Debug output
          log_info(paste("Auto-indlæst fil:", test_file_path), "TEST_MODE")
          log_info(paste("Data dimensioner:", nrow(test_data), "x", ncol(test_data)), "TEST_MODE")
          log_info(paste("Kolonner:", paste(names(test_data), collapse = ", ")), "TEST_MODE")
        },
        error = function(e) {
          log_error(paste("Fejl ved indlæsning af", test_file_path, ":", e$message), "TEST_MODE")
        }
      )
    } else {
      log_warn(paste("Fil ikke fundet:", test_file_path), "TEST_MODE")
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
      # PHASE 4: Check both old and new state management for current_data
      current_data_check <- if (exists("use_centralized_state") && use_centralized_state && exists("app_state")) {
        app_state$data$current_data
      } else {
        values$current_data
      }

      if (!is.null(current_data_check)) {
        log_debug("Setting test_mode_auto_detect_ready flag after setup", "TEST_MODE")
        timestamp <- Sys.time()

        # PHASE 4: Gradual migration - sync to both old and new state
        values$test_mode_auto_detect_ready <- timestamp
        app_state$test_mode$auto_detect_ready <- timestamp
        log_debug("Synced test_mode_auto_detect_ready to both systems", "PHASE4")
      }
    }) %>% bindEvent({
      # PHASE 4: Check both old and new state management for current_data
      if (exists("use_centralized_state") && use_centralized_state && exists("app_state")) {
        app_state$data$current_data
      } else {
        values$current_data
      }
    }, once = TRUE, ignoreNULL = TRUE)
  }

  # Initial UI Setup --------------------------------------------------------
  # Sæt standard chart_type når appen starter
  observe({
    log_debug("Setting initial chart_type to 'run'", "APP_SERVER")
    updateSelectizeInput(session, "chart_type", selected = "run")
    log_debug("✅ Initial chart_type set", "APP_SERVER")
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