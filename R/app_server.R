# app_server.R  
# Main server function following Golem conventions

#' Main Server Function
#'
#' @param input,output,session Internal Shiny parameters
#' 
#' @noRd
app_server <- function(input, output, session) {
  log_info("🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀", "APP_SERVER")
  log_info("🚀🚀🚀 APP_SERVER FUNCTION CALLED!!! SESSION START!!! 🚀🚀🚀", "APP_SERVER")
  log_info("🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀", "APP_SERVER")
  log_debug("🚀🚀🚀 APP_SERVER FUNCTION CALLED 🚀🚀🚀", "APP_SERVER")
  log_debug("===========================================", "APP_SERVER")

  # Initialize advanced debug system
  initialize_advanced_debug(enable_history = TRUE, max_history_entries = 1000)

  # Start session lifecycle debugging
  session_debugger <- debug_session_lifecycle(session$token, session)
  session_debugger$event("server_initialization")

  log_debug("===========================================", "APP_SERVER")
  log_debug("Starting main server function", "APP_SERVER")
  log_debug(paste("Session ID:", session$token), "APP_SERVER")

  debug_log("SPC App server initialization started", "SESSION_LIFECYCLE",
            level = "INFO", session_id = session$token)

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

  # PHASE 4: Legacy reactive values system removed - using unified app_state only

  # PHASE 4: Centraliseret state management (parallel til existing values)
  log_debug("Initializing centralized app state...", "APP_SERVER")
  debug_log("Creating centralized app_state", "SESSION_LIFECYCLE", level = "TRACE", session_id = session$token)
  app_state <- create_app_state()
  log_debug("⭐⭐⭐ create_app_state() COMPLETED SUCCESSFULLY ⭐⭐⭐", "APP_SERVER")
  log_debug("✅ Centralized state initialized", "APP_SERVER")
  session_debugger$event("centralized_state_initialized")
  log_debug("⭐⭐⭐ session_debugger event COMPLETED ⭐⭐⭐", "APP_SERVER")

  # EVENT SYSTEM: Initialize reactive event bus
  log_debug("==========================================", "APP_SERVER")
  log_debug("Creating event emit API...", "APP_SERVER")
  log_debug("About to create emit API", "APP_SERVER")
  emit <- create_emit_api(app_state)
  log_debug("✅ Emit API created successfully", "APP_SERVER")
  log_debug("✅ Event system initialized", "APP_SERVER")

  # UI SERVICE: Initialize centralized UI update service
  log_debug("Creating UI update service...", "APP_SERVER")
  log_debug("About to create UI service", "APP_SERVER")
  ui_service <- create_ui_update_service(session, app_state)
  log_debug("✅ UI service created successfully", "APP_SERVER")
  log_debug("✅ UI update service initialized", "APP_SERVER")


  # SHINYLOGS: Setup advanced web-based logging (if enabled)
  if (should_enable_shinylogs()) {
    log_debug("Setting up shinylogs advanced logging...", "APP_SERVER")
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
    log_info("✅ shinylogs advanced logging activated", "APP_SERVER")
  } else {
    log_debug("shinylogs disabled via environment variable", "APP_SERVER")
  }

  # EVENT SYSTEM: Set up reactive event listeners AFTER shinylogs setup
  log_debug("Line 94 executed - about to start event system setup", "DEBUG")
  tryCatch({
    log_debug("🔧 Starting event system setup...", "APP_SERVER")
  }, error = function(e) {
    log_error(paste("ERROR in log_debug at line 95:", e$message), "APP_SERVER")
  })

  # SESSION FLAG: Prevent duplicate event listener registration
  log_debug("🔧 Checking app_state$system exists...", "APP_SERVER")

  # Initialize event listeners setup flag in app_state to prevent double registration
  tryCatch({
    log_debug("🔧 Checking event_listeners_setup flag...", "APP_SERVER")
    if (is.null(isolate(app_state$system$event_listeners_setup))) {
      log_debug("🔧 Setting event_listeners_setup = FALSE", "APP_SERVER")
      app_state$system$event_listeners_setup <- FALSE
    }
    log_debug("🔧 Event listeners setup flag initialized successfully", "APP_SERVER")
  }, error = function(e) {
    log_debug(paste("❌ ERROR initializing event_listeners_setup flag:", e$message), "APP_SERVER")
    print(e)
  })

  log_debug("==========================================", "APP_SERVER")
  log_debug("About to set up event listeners AFTER shinylogs", "APP_SERVER")
  log_debug("Setting up event listeners with all required dependencies...", "APP_SERVER")
  tryCatch({
    log_debug("About to call setup_event_listeners...", "DEBUG")
    setup_event_listeners(app_state, emit, input, output, session, ui_service)
    log_debug("setup_event_listeners call completed", "DEBUG")
    app_state$system$event_listeners_setup <- TRUE  # SUCCESS: Mark as completed
    log_debug("Event listeners setup flag set to TRUE", "DEBUG")
  }, error = function(e) {
    log_error(paste("ERROR in setup_event_listeners:", e$message), "APP_SERVER")
    print(paste("Full error details:", e))
  })

  # Emergency observer removed - event listeners setup now reliable

  # Take initial state snapshot - delay to avoid reactive context issues
  observeEvent(reactive(TRUE), {
    isolate({
      initial_snapshot <- debug_state_snapshot("app_initialization", app_state, session_id = session$token)
    })
  }, once = TRUE, priority = OBSERVER_PRIORITIES$LOW, ignoreInit = FALSE)

  # FASE 5: Memory management setup
  log_debug("Line 150 executed - about to setup memory management", "DEBUG")
  log_debug("Setting up memory management...", "APP_SERVER")
  setup_session_cleanup(session, app_state)
  log_debug("✅ Memory management configured", "APP_SERVER")

  # FASE 4: AUTOMATIC BACKGROUND CLEANUP - Schedule periodic system maintenance
  log_debug("Setting up automatic background cleanup scheduling...", "APP_SERVER")

  # Schedule regular comprehensive cleanup every 5 minutes
  if (requireNamespace("later", quietly = TRUE)) {
    cleanup_interval_minutes <- 5
    later::later(function() {
      # Recursive cleanup scheduler
      schedule_periodic_cleanup <- function() {
        # Check if session is still active before continuing
        if (!app_state$session$lifecycle$session_active || !app_state$session$lifecycle$background_tasks_active) {
          log_debug("🧹 Stopping periodic cleanup - session ended", "BACKGROUND_CLEANUP")
          return()
        }

        log_debug("🧹 Running scheduled comprehensive system cleanup", "BACKGROUND_CLEANUP")
        tryCatch({
          comprehensive_system_cleanup(app_state)
          log_debug("✅ Scheduled cleanup completed successfully", "BACKGROUND_CLEANUP")
        }, error = function(e) {
          log_error(paste("Background cleanup error:", e$message), "BACKGROUND_CLEANUP")
        })

        # Schedule next cleanup only if session is still active
        if (app_state$session$lifecycle$session_active && app_state$session$lifecycle$background_tasks_active) {
          later::later(schedule_periodic_cleanup, delay = cleanup_interval_minutes * 60)
        }
      }

      # Start the periodic cleanup cycle
      schedule_periodic_cleanup()
    }, delay = cleanup_interval_minutes * 60)  # Initial delay

    log_debug(paste("✅ Background cleanup scheduled every", cleanup_interval_minutes, "minutes"), "APP_SERVER")
  } else {
    log_warn("later package not available - background cleanup disabled", "APP_SERVER")
  }

  # FASE 4: PERFORMANCE MONITORING INTEGRATION - Schedule periodic reporting
  if (requireNamespace("later", quietly = TRUE)) {
    report_interval_minutes <- 15
    later::later(function() {
      # Recursive performance reporting
      schedule_periodic_reporting <- function() {
        # Check if session is still active before continuing
        if (!app_state$session$lifecycle$session_active || !app_state$session$lifecycle$background_tasks_active) {
          log_debug("📊 Stopping periodic reporting - session ended", "PERFORMANCE_MONITOR")
          return()
        }

        log_debug("📊 Generating periodic performance report", "PERFORMANCE_MONITOR")
        tryCatch({
          report <- get_performance_report(app_state)
          log_debug("=== PERIODIC PERFORMANCE REPORT ===", "PERFORMANCE_MONITOR")
          log_debug(report$formatted_text, "PERFORMANCE_MONITOR")

          # Check if system needs attention
          if (report$health_status == "WARNING") {
            log_warn(paste("System health WARNING - Queue:", report$queue_utilization_pct, "% | Tokens:", report$token_utilization_pct, "%"), "PERFORMANCE_MONITOR")
          } else if (report$health_status == "CAUTION") {
            log_debug(paste("System health CAUTION - Queue:", report$queue_utilization_pct, "% | Tokens:", report$token_utilization_pct, "%"), "PERFORMANCE_MONITOR")
          }

        }, error = function(e) {
          log_error(paste("Performance reporting error:", e$message), "PERFORMANCE_MONITOR")
        })

        # Schedule next report only if session is still active
        if (app_state$session$lifecycle$session_active && app_state$session$lifecycle$background_tasks_active) {
          later::later(schedule_periodic_reporting, delay = report_interval_minutes * 60)
        }
      }

      # Start the periodic reporting cycle
      schedule_periodic_reporting()
    }, delay = report_interval_minutes * 60)  # Initial delay

    log_debug(paste("✅ Performance monitoring scheduled every", report_interval_minutes, "minutes"), "APP_SERVER")
  }

  # Test Tilstand ------------------------------------------------------------
  # TEST MODE: Auto-indlæs eksempel data hvis aktiveret
  log_debug("Checking TEST_MODE configuration...", "APP_SERVER")
  log_debug(paste("TEST_MODE_AUTO_LOAD:", if(exists("TEST_MODE_AUTO_LOAD")) TEST_MODE_AUTO_LOAD else "UNDEFINED"), "APP_SERVER")

  debug_log("Checking TEST_MODE configuration", "SESSION_LIFECYCLE", level = "TRACE",
            context = list(
              TEST_MODE_AUTO_LOAD = if(exists("TEST_MODE_AUTO_LOAD")) TEST_MODE_AUTO_LOAD else "UNDEFINED"
            ),
            session_id = session$token)

  if (exists("TEST_MODE_AUTO_LOAD") && TEST_MODE_AUTO_LOAD) {
    # Start workflow tracer for auto-load process
    autoload_tracer <- debug_workflow_tracer("test_mode_auto_load", app_state, session$token)
    log_debug(paste("🔄 Attempting auto-load with TEST_MODE_AUTO_LOAD =", TEST_MODE_AUTO_LOAD), "TEST_MODE")
    test_file_path <- if(exists("TEST_MODE_FILE_PATH")) TEST_MODE_FILE_PATH else "UNDEFINED"
    log_debug(paste("Test file path:", test_file_path), "TEST_MODE")

    if (exists("TEST_MODE_FILE_PATH") && file.exists(test_file_path)) {
      log_debug("✅ Test file found, starting auto-load...", "TEST_MODE")
      autoload_tracer$step("file_validation_complete")

      tryCatch(
        {
          autoload_tracer$step("data_loading_started")
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

          # Set reactive values - PHASE 4: Unified state only
          app_state$data$original_data <- test_data
          # Unified state: Set data and flags in both legacy and centralized state
          app_state$data$current_data <- test_data

          # Emit event to trigger downstream effects
          emit$data_loaded()
          # PHASE 4B: Unified state assignment only
          app_state$session$file_uploaded <- TRUE
          # PHASE 4B: Unified state assignment only
          app_state$session$user_started_session <- TRUE
          # PHASE 4B: Unified state assignment only
          app_state$columns$auto_detect$completed <- FALSE
          # PHASE 4B: Legacy assignment removed - managed by unified state
          # PHASE 4B: Unified state assignment only
          app_state$ui$hide_anhoej_rules <- FALSE

          autoload_tracer$step("state_synchronization_complete")

          # Take state snapshot after auto-load
          debug_state_snapshot("after_test_data_autoload", app_state, session_id = session$token)

          # NOTE: Flag sættes efter setup_column_management() for at undgå race condition

          # Debug output
          log_info(paste("Auto-indlæst fil:", test_file_path), "TEST_MODE")
          log_info(paste("Data dimensioner:", nrow(test_data), "x", ncol(test_data)), "TEST_MODE")
          log_info(paste("Kolonner:", paste(names(test_data), collapse = ", ")), "TEST_MODE")

          autoload_tracer$complete("test_data_autoload_complete")
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
  setup_welcome_page_handlers(input, output, session, waiter_file, app_state, emit, ui_service)

  ## Session management logik
  setup_session_management(input, output, session, waiter_file, app_state, emit, ui_service)

  ## Fil upload logik
  setup_file_upload(input, output, session, waiter_file, app_state, emit, ui_service)

  ## Data tabel logik
  setup_data_table(input, output, session, app_state, emit)

  ## Hjælpe observers (IMPORTANT: Must be set up before visualization for unified navigation)
  setup_helper_observers(input, output, session, obs_manager, app_state)

  ## Kolonne management logik
  # PHASE 4: Pass centralized state to column management - now uses unified event system
  setup_column_management(input, output, session, app_state, emit)
  log_debug("Column management setup completed with unified event system", .context = "APP_SERVER")

  ## Visualiserings logik
  visualization <- setup_visualization(input, output, session, app_state)

  ## Download handlers (REMOVED - to be reimplemented later)
  # setup_download_handlers(input, output, session, app_state, visualization)

  session_debugger$event("server_setup_complete")
  debug_log("All server components setup completed", "SESSION_LIFECYCLE", level = "INFO", session_id = session$token)

  # FASE 3: Emit session_started event for name-only detection
  observeEvent(reactive(TRUE), {
    log_debug("Session started, emitting session_started event", "SESSION_LIFECYCLE")
    emit$session_started()
    log_debug("✅ Session started event emitted", "SESSION_LIFECYCLE")
  }, once = TRUE, ignoreInit = FALSE)

  # TEST MODE: Emit test_mode_ready event AFTER all observers are set up
  if (TEST_MODE_AUTO_LOAD) {
    observe({
      # Unified state: Use centralized state as primary data source
      current_data_check <- app_state$data$current_data

      if (!is.null(current_data_check)) {
        log_debug("Test data loaded, emitting test_mode_ready event", "TEST_MODE")
        emit$test_mode_ready()
        log_debug("✅ Test mode ready event emitted", "TEST_MODE")
      }
    }) %>% bindEvent({
      # Unified state: Use centralized state for reactive triggers
      app_state$data$current_data
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
  # Additional cleanup når session lukker
  session$onSessionEnded(function() {
    session_debugger$event("session_cleanup_started")
    debug_log("Session cleanup initiated", "SESSION_LIFECYCLE", level = "INFO", session_id = session$token)

    # Stop background tasks immediately
    if (!is.null(app_state$session$lifecycle)) {
      app_state$session$lifecycle$session_active <- FALSE
      app_state$session$lifecycle$background_tasks_active <- FALSE
      app_state$session$lifecycle$cleanup_initiated <- TRUE
      log_debug("🔄 Background tasks stopped", "SESSION_LIFECYCLE")
    }

    # Cleanup alle observers
    obs_manager$cleanup_all()

    # LOOP PROTECTION CLEANUP: Ensure all flags are cleared and no dangling callbacks
    tryCatch({
      if (!is.null(app_state$ui)) {
        app_state$ui$updating_programmatically <- FALSE
        app_state$ui$flag_reset_scheduled <- TRUE
        log_debug("LOOP_PROTECTION: Flags cleared during session cleanup", .context = "SESSION_CLEANUP")
      }
    }, error = function(e) {
      log_debug(paste("Session cleanup: Could not clear loop protection flags:", e$message), .context = "SESSION_CLEANUP")
    })

    # Cleanup waiter
    if (exists("waiter_file") && !is.null(waiter_file)) {
      waiter_file$hide()
    }

    # Complete session lifecycle debugging
    session_lifecycle_result <- session_debugger$complete()

    # Log session statistics
    log_info(paste("Session afsluttet - Observer count:", obs_manager$count()), "APP_SERVER")
    debug_log("Session ended successfully", "SESSION_LIFECYCLE", level = "INFO",
              context = list(
                session_duration = round(session_lifecycle_result$total_duration, 3),
                events_tracked = length(session_lifecycle_result$events)
              ),
              session_id = session$token)
  })
}