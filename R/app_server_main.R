# app_server.R
# Main server function following Golem conventions

#' Hash session token for secure logging
#' @param token Session token to hash
#' @return First 8 characters of SHA256 hash for logging identification
hash_session_token <- function(token) {
  if (is.null(token) || !is.character(token)) {
    return("unknown")
  }
  # Use first 8 chars of SHA256 hash for secure but identifiable logging
  substr(digest::sha1(token), 1, 8)
}

#' Main Server Function
#'
#' @param input,output,session Internal Shiny parameters
#'
#' @export
main_app_server <- function(input, output, session) {
  # Get session token and hash it for secure logging
  session_token <- session$token %||% paste0("session_", Sys.time(), "_", sample(1000:9999, 1))
  hashed_token <- hash_session_token(session_token)

  # Log server initialization with session details (using hashed token for security)
  log_debug_kv(
    message = "SPC App server initialization started",
    session_id = hashed_token,
    client_data = if (exists("clientData", envir = session)) length(session$clientData) else 0,
    .context = "APP_SERVER"
  )

  # Initialize advanced debug system
  initialize_advanced_debug(enable_history = TRUE, max_history_entries = 1000)

  # Start session lifecycle debugging (using hashed token)
  session_debugger <- debug_session_lifecycle(hashed_token, session)
  session_debugger$event("server_initialization")

  log_debug(paste("Server starting - Session ID:", hashed_token), .context = "APP_SERVER")

  debug_log("SPC App server initialization started", "SESSION_LIFECYCLE",
    level = "INFO", session_id = hashed_token
  )

  # SPRINT 1 REFACTORING: Initialize app infrastructure (extracted to helper)
  infrastructure <- initialize_app_infrastructure(session, hashed_token, session_debugger)
  app_state <- infrastructure$app_state
  emit <- infrastructure$emit
  ui_service <- infrastructure$ui_service

  # FASE 5: Memory management setup
  log_debug("Setting up memory management...", .context = "APP_SERVER")
  setup_session_cleanup(session, app_state)
  log_debug("Memory management configured", .context = "APP_SERVER")

  # SPRINT 1 REFACTORING: Setup background tasks (extracted to helper)
  setup_background_tasks(session, app_state, emit)

  # Test Tilstand ------------------------------------------------------------
  # TEST MODE: Auto-indlæs eksempel data hvis aktiveret
  test_mode_auto_load <- get_test_mode_auto_load()

  debug_log("Checking TEST_MODE configuration", "SESSION_LIFECYCLE",
    level = "TRACE",
    context = list(
      TEST_MODE_AUTO_LOAD = test_mode_auto_load
    ),
    session_id = hashed_token
  )

  if (test_mode_auto_load) {
    # Phase 3: Initialize test mode optimization settings
    claudespc_env <- get_claudespc_environment()
    app_state$test_mode$debounce_delay <- claudespc_env$TEST_MODE_STARTUP_DEBOUNCE_MS %||% 500
    app_state$test_mode$lazy_plot_generation <- claudespc_env$TEST_MODE_LAZY_PLOT_GENERATION %||% TRUE
    app_state$test_mode$autoload_completed <- FALSE

    # Phase 4: Memory tracking (now handled by setup_background_tasks)
    # Note: track_memory_usage() moved to profiling utilities with session parameter

    log_debug_kv(
      message = "Test mode optimization configured",
      debounce_delay = shiny::isolate(app_state$test_mode$debounce_delay),
      lazy_plot_generation = shiny::isolate(app_state$test_mode$lazy_plot_generation),
      .context = "[TEST_MODE_STARTUP]"
    )

    test_file_path <- get_test_mode_file_path()

    session$onFlushed(function() {
      if (isTRUE(shiny::isolate(app_state$test_mode$autoload_completed))) {
        log_debug_kv(
          message = "Skipping duplicate test data autoload",
          session_id = hashed_token,
          .context = "[TEST_MODE_STARTUP]"
        )
        return(invisible(NULL))
      }

      shiny::isolate(app_state$test_mode$autoload_completed <- TRUE)

      # Start workflow tracer for auto-load process
      autoload_tracer <- debug_workflow_tracer("test_mode_auto_load", app_state, hashed_token)

      if (!is.null(test_file_path) && file.exists(test_file_path)) {
        autoload_tracer$step("file_validation_complete")

        safe_operation(
          "Test mode auto-load data",
          code = {
            autoload_tracer$step("data_loading_started")
            log_debug_kv(
              message = "Starting test data autoload after UI flush",
              session_id = hashed_token,
              file = test_file_path,
              .context = "[TEST_MODE_STARTUP]"
            )

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
            autoload_tracer$step("data_processing_complete")

            # Set reactive values using dual-state sync
            app_state$data$original_data <- test_data
            # Unified state: Set data using sync helper for compatibility
            set_current_data(app_state, test_data)

            # Emit event to trigger downstream effects
            emit$data_updated("test_data_loaded")
            # Set session flags
            app_state$session$file_uploaded <- TRUE
            app_state$session$user_started_session <- TRUE
            # Reset auto-detection state
            shiny::isolate(app_state$columns$auto_detect$completed <- FALSE)
            # Legacy assignments removed - managed by unified state
            app_state$ui$hide_anhoej_rules <- FALSE

            autoload_tracer$step("state_synchronization_complete")

            # Take state snapshot after auto-load
            debug_state_snapshot("after_test_data_autoload", app_state, session_id = hashed_token)

            # NOTE: Flag sættes efter setup_column_management() for at undgå race condition

            # Debug output
            log_info(paste("Auto-indlæst fil:", test_file_path), .context = "TEST_MODE")
            log_info(paste("Data dimensioner:", nrow(test_data), "x", ncol(test_data)), .context = "TEST_MODE")
            log_info(paste("Kolonner:", paste(names(test_data), collapse = ", ")), .context = "TEST_MODE")

            autoload_tracer$step("test_data_autoload_complete")
          },
          fallback = function(e) {
            log_error(paste("Fejl ved indlæsning af", test_file_path, ":", e$message), .context = "TEST_MODE")
          },
          error_type = "processing"
        )
      } else {
        log_warn(paste("Fil ikke fundet:", test_file_path), .context = "TEST_MODE")
      }

      invisible(NULL)
    }, once = TRUE)
  }



  # Observer Management ------------------------------------------------------
  # Initialiser observer manager til tracking af alle observers
  obs_manager <- observer_manager()


  # Server Setup ------------------------------------------------------------
  # Opsæt alle server-komponenter

  ## Velkomstside interaktioner
  setup_welcome_page_handlers(input, output, session, app_state, emit, ui_service)

  ## Session management logik
  setup_session_management(input, output, session, app_state, emit, ui_service)

  ## Fil upload logik
  setup_file_upload(input, output, session, app_state, emit, ui_service)

  ## Data tabel logik
  setup_data_table(input, output, session, app_state, emit)

  ## Hjælpe observers (IMPORTANT: Must be set up before visualization for unified navigation)
  setup_helper_observers(input, output, session, obs_manager, app_state)

  ## Kolonne management logik
  # Pass centralized state to column management via unified event system
  setup_column_management(input, output, session, app_state, emit)

  ## Visualiserings logik
  visualization <- setup_visualization(input, output, session, app_state)

  ## Download handlers
  # setup_download_handlers(input, output, session, app_state, visualization)

  session_debugger$event("server_setup_complete")
  debug_log("All server components setup completed", "SESSION_LIFECYCLE", level = "INFO", session_id = hashed_token)

  # FASE 3: Emit session_started event for name-only detection
  shiny::observeEvent(shiny::reactive(TRUE),
    {
      emit$session_started()
    },
    once = TRUE,
    ignoreInit = FALSE
  )

  # TEST MODE: Emit test_mode_ready event AFTER all observers are set up
  if (test_mode_auto_load) {
    shiny::observe({
      # Unified state: Use centralized state as primary data source
      current_data_check <- app_state$data$current_data

      if (!is.null(current_data_check)) {
        emit$test_mode_ready()
      }
    }) |> bindEvent(
      {
        # Unified state: Use centralized state for reactive triggers
        app_state$data$current_data
      },
      once = TRUE,
      ignoreNULL = TRUE
    )
  }

  # Initial UI Setup --------------------------------------------------------
  # Sæt standard chart_type når appen starter
  shiny::observe({
    shiny::updateSelectizeInput(session, "chart_type", selected = "run")
  }) |> bindEvent(TRUE, once = TRUE)

  # Session Cleanup ---------------------------------------------------------
  # Additional cleanup når session lukker
  session$onSessionEnded(function() {
    session_debugger$event("session_cleanup_started")
    debug_log("Session cleanup initiated", "SESSION_LIFECYCLE", level = "INFO", session_id = hashed_token)

    # Stop background tasks immediately
    if (!is.null(app_state$infrastructure)) {
      app_state$infrastructure$session_active <- FALSE
      app_state$infrastructure$background_tasks_active <- FALSE
    }
    if (!is.null(app_state$session)) {
      app_state$session$cleanup_initiated <- TRUE
    }

    # Cleanup alle observers
    obs_manager$cleanup_all()

    # LOOP PROTECTION CLEANUP: Ensure all flags are cleared and no dangling callbacks
    safe_operation(
      "Clear loop protection flags during session cleanup",
      code = {
        if (!is.null(app_state$ui)) {
          app_state$ui$updating_programmatically <- FALSE
          app_state$ui$flag_reset_scheduled <- TRUE
        }
      },
      fallback = function(e) {
        log_error(paste("Session cleanup: Could not clear loop protection flags:", e$message), .context = "SESSION_CLEANUP")
      },
      error_type = "processing"
    )


    # Complete session lifecycle debugging
    session_lifecycle_result <- session_debugger$complete()

    # Log session statistics
    log_info(paste("Session ended - Observer count:", obs_manager$count()), .context = "APP_SERVER")
    debug_log("Session ended successfully", "SESSION_LIFECYCLE",
      level = "INFO",
      context = list(
        session_duration = round(session_lifecycle_result$total_duration, 3),
        events_tracked = length(session_lifecycle_result$events)
      ),
      session_id = hashed_token
    )
  })
}
