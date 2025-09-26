# app_server.R  
# Main server function following Golem conventions

#' Main Server Function
#'
#' @param input,output,session Internal Shiny parameters
#'
#' @export
main_app_server <- function(input, output, session) {
  log_info("SPC App server initialization started", "APP_SERVER")

  # Initialize advanced debug system
  initialize_advanced_debug(enable_history = TRUE, max_history_entries = 1000)

  # Start session lifecycle debugging
  session_debugger <- debug_session_lifecycle(session$token, session)
  session_debugger$event("server_initialization")

  log_debug(paste("Server starting - Session ID:", session$token), "APP_SERVER")

  debug_log("SPC App server initialization started", "SESSION_LIFECYCLE",
            level = "INFO", session_id = session$token)

  # Server components now loaded globally in global.R for better performance

  # Centralized state management using unified app_state architecture
  app_state <- create_app_state()
  session_debugger$event("centralized_state_initialized")

  # EVENT SYSTEM: Initialize reactive event bus
  emit <- create_emit_api(app_state)
  log_debug("Event system initialized", "APP_SERVER")

  # UI SERVICE: Initialize centralized UI update service
  ui_service <- create_ui_update_service(session, app_state)
  log_debug("UI update service initialized", "APP_SERVER")


  # SHINYLOGS: Setup advanced web-based logging (if enabled)
  if (should_enable_shinylogs()) {
    setup_shinylogs(
      enable_tracking = TRUE,
      enable_errors = TRUE,
      enable_performances = TRUE,
      log_directory = "logs/"
    )
    initialize_shinylogs_tracking(
      session = session,
      app_name = "SPC_Analysis_Tool"
    )
    # integrate_shinylogs_with_logging(session)  # Disabled - causes conflicts
    log_info("shinylogs advanced logging activated", "APP_SERVER")
  }

  # EVENT SYSTEM: Set up reactive event listeners AFTER shinylogs setup

  # SESSION FLAG: Prevent duplicate event listener registration
  # Initialize event listeners setup flag in infrastructure state to prevent double registration
  safe_operation(
    "Initialize event listeners setup flag",
    code = {
      if (is.null(app_state$infrastructure$event_listeners_setup)) {
        app_state$infrastructure$event_listeners_setup <- FALSE
      }
    },
    fallback = function(e) {
      log_error(paste("ERROR initializing event_listeners_setup flag:", e$message), "APP_SERVER")
    },
    error_type = "processing"
  )

  safe_operation(
    "Setup event listeners",
    code = {
      setup_event_listeners(app_state, emit, input, output, session, ui_service)
      app_state$infrastructure$event_listeners_setup <- TRUE  # SUCCESS: Mark as completed
    },
    fallback = function(e) {
      log_error(paste("ERROR in setup_event_listeners:", e$message), "APP_SERVER")
      print(paste("Full error details:", e))
    },
    error_type = "processing"
  )

  # Emergency observer removed - event listeners setup now reliable

  # Take initial state snapshot - delay to avoid reactive context issues
  shiny::observeEvent(shiny::reactive(TRUE), {
    shiny::isolate({
      initial_snapshot <- debug_state_snapshot("app_initialization", app_state, session_id = session$token)
    })
  }, once = TRUE, priority = OBSERVER_PRIORITIES$LOW, ignoreInit = FALSE)

  # FASE 5: Memory management setup
  log_debug("Line 150 executed - about to setup memory management", "DEBUG")
  log_debug("Setting up memory management...", "APP_SERVER")
  setup_session_cleanup(session, app_state)
  log_debug("Memory management configured", "APP_SERVER")

  # FASE 4: AUTOMATIC BACKGROUND CLEANUP - Schedule periodic system maintenance
  log_debug("Setting up automatic background cleanup scheduling...", "APP_SERVER")

  # Schedule regular comprehensive cleanup every 5 minutes
  if (requireNamespace("later", quietly = TRUE)) {
    cleanup_interval_minutes <- 5
    later::later(function() {
      shiny::withReactiveDomain(session, {
        # Recursive cleanup scheduler
        schedule_periodic_cleanup <- function() {
        # Check if session is still active before continuing
        session_check <- !app_state$infrastructure$session_active || !app_state$infrastructure$background_tasks_active
        if (session_check) {
          log_debug("Stopping periodic cleanup - session ended", "BACKGROUND_CLEANUP")
          return()
        }

        log_debug("Running scheduled comprehensive system cleanup", "BACKGROUND_CLEANUP")
        safe_operation(
          "Scheduled system cleanup",
          code = {
            comprehensive_system_cleanup(app_state)
            log_debug("Scheduled cleanup completed successfully", "BACKGROUND_CLEANUP")
          },
          fallback = NULL,
          session = session,
          error_type = "processing",
          emit = emit,
          app_state = app_state
        )

        # Schedule next cleanup only if session is still active
        should_continue <- app_state$infrastructure$session_active && app_state$infrastructure$background_tasks_active
        if (should_continue) {
          later::later(schedule_periodic_cleanup, delay = cleanup_interval_minutes * 60)
        }
      }

        # Start the periodic cleanup cycle
        schedule_periodic_cleanup()
      })
    }, delay = cleanup_interval_minutes * 60)  # Initial delay

    log_debug(paste("Background cleanup scheduled every", cleanup_interval_minutes, "minutes"), "APP_SERVER")
  } else {
    log_warn("later package not available - background cleanup disabled", "APP_SERVER")
  }

  # FASE 4: PERFORMANCE MONITORING INTEGRATION - Schedule periodic reporting
  if (requireNamespace("later", quietly = TRUE)) {
    report_interval_minutes <- 15
    later::later(function() {
      shiny::withReactiveDomain(session, {
        # Recursive performance reporting
        schedule_periodic_reporting <- function() {
        # Check if session is still active before continuing
        session_check <- !app_state$infrastructure$session_active || !app_state$infrastructure$background_tasks_active
        if (session_check) {
          return()
        }
        safe_operation(
          "Performance report generation",
          code = {
            report <- get_performance_report(app_state)
            log_debug(report$formatted_text, "PERFORMANCE_MONITOR")

            # Check if system needs attention
            if (report$health_status == "WARNING") {
              log_warn(paste("System health WARNING - Queue:", report$queue_utilization_pct, "% | Tokens:", report$token_utilization_pct, "%"), "PERFORMANCE_MONITOR")
            }
          },
          fallback = NULL,
          session = session,
          error_type = "processing",
          emit = emit,
          app_state = app_state
        )

        # Schedule next report only if session is still active
        should_continue <- app_state$infrastructure$session_active && app_state$infrastructure$background_tasks_active
        if (should_continue) {
          later::later(schedule_periodic_reporting, delay = report_interval_minutes * 60)
        }
      }

        # Start the periodic reporting cycle
        schedule_periodic_reporting()
      })
    }, delay = report_interval_minutes * 60)  # Initial delay

    log_debug(paste("Performance monitoring scheduled every", report_interval_minutes, "minutes"), "APP_SERVER")
  }

  # Test Tilstand ------------------------------------------------------------
  # TEST MODE: Auto-indlæs eksempel data hvis aktiveret
  test_mode_auto_load <- get_test_mode_auto_load()

  debug_log("Checking TEST_MODE configuration", "SESSION_LIFECYCLE", level = "TRACE",
            context = list(
              TEST_MODE_AUTO_LOAD = test_mode_auto_load
            ),
            session_id = session$token)

  if (test_mode_auto_load) {
    # Phase 3: Initialize test mode optimization settings
    claudespc_env <- get_claudespc_environment()
    app_state$test_mode$debounce_delay <- claudespc_env$TEST_MODE_STARTUP_DEBOUNCE_MS %||% 500
    app_state$test_mode$lazy_plot_generation <- claudespc_env$TEST_MODE_LAZY_PLOT_GENERATION %||% TRUE
    app_state$test_mode$autoload_completed <- FALSE

    # Phase 4: Track memory usage during test mode setup
    if (exists("track_memory_usage")) {
      track_memory_usage("test_mode_setup")
    }

    log_debug(
      component = "[TEST_MODE_STARTUP]",
      message = "Test mode optimization configured",
      details = list(
        debounce_delay = app_state$test_mode$debounce_delay,
        lazy_plot_generation = app_state$test_mode$lazy_plot_generation
      )
    )

    test_file_path <- get_test_mode_file_path()

    session$onFlushed(function() {
      if (isTRUE(shiny::isolate(app_state$test_mode$autoload_completed))) {
        log_debug(
          component = "[TEST_MODE_STARTUP]",
          message = "Skipping duplicate test data autoload",
          details = list(session_id = session$token)
        )
        return(invisible(NULL))
      }

      shiny::isolate(app_state$test_mode$autoload_completed <- TRUE)

      # Start workflow tracer for auto-load process
      autoload_tracer <- debug_workflow_tracer("test_mode_auto_load", app_state, session$token)

      if (!is.null(test_file_path) && file.exists(test_file_path)) {
        autoload_tracer$step("file_validation_complete")

        safe_operation(
          "Test mode auto-load data",
          code = {
            autoload_tracer$step("data_loading_started")
            log_debug(
              component = "[TEST_MODE_STARTUP]",
              message = "Starting test data autoload after UI flush",
              details = list(session_id = session$token, file = test_file_path)
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
            emit$data_loaded()
            # Set session flags
            app_state$session$file_uploaded <- TRUE
            app_state$session$user_started_session <- TRUE
            # Reset auto-detection state
            shiny::isolate(app_state$columns$auto_detect$completed <- FALSE)
            # Legacy assignments removed - managed by unified state
            app_state$ui$hide_anhoej_rules <- FALSE

            autoload_tracer$step("state_synchronization_complete")

            # Take state snapshot after auto-load
            debug_state_snapshot("after_test_data_autoload", app_state, session_id = session$token)

            # NOTE: Flag sættes efter setup_column_management() for at undgå race condition

            # Debug output
            log_info(paste("Auto-indlæst fil:", test_file_path), "TEST_MODE")
            log_info(paste("Data dimensioner:", nrow(test_data), "x", ncol(test_data)), "TEST_MODE")
            log_info(paste("Kolonner:", paste(names(test_data), collapse = ", ")), "TEST_MODE")

            autoload_tracer$step("test_data_autoload_complete")
          },
          fallback = function(e) {
            log_error(paste("Fejl ved indlæsning af", test_file_path, ":", e$message), "TEST_MODE")
          },
          error_type = "processing"
        )
      } else {
        log_warn(paste("Fil ikke fundet:", test_file_path), "TEST_MODE")
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
  debug_log("All server components setup completed", "SESSION_LIFECYCLE", level = "INFO", session_id = session$token)

  # FASE 3: Emit session_started event for name-only detection
  shiny::observeEvent(shiny::reactive(TRUE), {
    emit$session_started()
  }, once = TRUE, ignoreInit = FALSE)

  # TEST MODE: Emit test_mode_ready event AFTER all observers are set up
  if (test_mode_auto_load) {
    shiny::observe({
      # Unified state: Use centralized state as primary data source
      current_data_check <- app_state$data$current_data

      if (!is.null(current_data_check)) {
        emit$test_mode_ready()
      }
    }) |> bindEvent({
      # Unified state: Use centralized state for reactive triggers
      app_state$data$current_data
    }, once = TRUE, ignoreNULL = TRUE)
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
    debug_log("Session cleanup initiated", "SESSION_LIFECYCLE", level = "INFO", session_id = session$token)

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
        log_error(paste("Session cleanup: Could not clear loop protection flags:", e$message), "SESSION_CLEANUP")
      },
      error_type = "processing"
    )


    # Complete session lifecycle debugging
    session_lifecycle_result <- session_debugger$complete()

    # Log session statistics
    log_info(paste("Session ended - Observer count:", obs_manager$count()), "APP_SERVER")
    debug_log("Session ended successfully", "SESSION_LIFECYCLE", level = "INFO",
              context = list(
                session_duration = round(session_lifecycle_result$total_duration, 3),
                events_tracked = length(session_lifecycle_result$events)
              ),
              session_id = session$token)
  })
}
