# utils_server_initialization.R
# Server initialization helper functions
# Extracted from app_server_main.R for better maintainability (Sprint 1)

#' Initialize App Infrastructure
#'
#' Sets up core app infrastructure including state management, event system,
#' UI service, and logging.
#'
#' @param session Shiny session object
#' @param hashed_token Hashed session token for logging
#' @param session_debugger Session debugger object
#'
#' @return List with app_state, emit, ui_service components
#' @export
initialize_app_infrastructure <- function(session, hashed_token, session_debugger) {
  log_debug("Initializing app infrastructure", .context = "APP_INIT")

  # Centralized state management using unified app_state architecture
  app_state <- create_app_state()
  session_debugger$event("centralized_state_initialized")

  # EVENT SYSTEM: Initialize reactive event bus
  emit <- create_emit_api(app_state)
  log_debug("Event system initialized", .context = "APP_INIT")

  # UI SERVICE: Initialize centralized UI update service
  ui_service <- create_ui_update_service(session, app_state)
  log_debug("UI update service initialized", .context = "APP_INIT")

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
    log_debug_kv(
      message = "shinylogs advanced logging activated",
      log_directory = "logs/",
      session_id = hashed_token,
      .context = "APP_INIT"
    )
  }

  # EVENT SYSTEM: Set up reactive event listeners AFTER shinylogs setup
  # SESSION FLAG: Prevent duplicate event listener registration
  safe_operation(
    "Initialize event listeners setup flag",
    code = {
      if (is.null(app_state$infrastructure$event_listeners_setup)) {
        app_state$infrastructure$event_listeners_setup <- FALSE
      }
    },
    fallback = function(e) {
      log_error(
        paste("ERROR initializing event_listeners_setup flag:", e$message),
        .context = "APP_INIT"
      )
    },
    error_type = "processing"
  )

  safe_operation(
    "Setup event listeners",
    code = {
      setup_event_listeners(app_state, emit, input = session$input, output = session$output, session, ui_service)
      app_state$infrastructure$event_listeners_setup <- TRUE
    },
    fallback = function(e) {
      log_error(paste("ERROR in setup_event_listeners:", e$message), .context = "APP_INIT")
    },
    error_type = "processing"
  )

  # Take initial state snapshot
  shiny::observeEvent(
    shiny::reactive(TRUE),
    {
      shiny::isolate({
        initial_snapshot <- debug_state_snapshot("app_initialization", app_state, session_id = hashed_token)
      })
    },
    once = TRUE,
    priority = OBSERVER_PRIORITIES$LOW,
    ignoreInit = FALSE
  )

  log_debug("App infrastructure initialized", .context = "APP_INIT")

  return(list(
    app_state = app_state,
    emit = emit,
    ui_service = ui_service
  ))
}

#' Setup Background Tasks
#'
#' Configures periodic background maintenance tasks including cleanup and
#' performance monitoring.
#'
#' @param session Shiny session object
#' @param app_state Centralized app state
#' @param emit Event emission API
#'
#' @export
setup_background_tasks <- function(session, app_state, emit) {
  log_debug("Setting up background tasks", .context = "BACKGROUND_TASKS")

  # AUTOMATIC BACKGROUND CLEANUP - Schedule periodic system maintenance
  if (requireNamespace("later", quietly = TRUE)) {
    cleanup_interval_minutes <- 5

    later::later(
      function() {
        shiny::withReactiveDomain(session, {
          # Recursive cleanup scheduler
          schedule_periodic_cleanup <- function() {
            # Check if session is still active
            session_check <- !app_state$infrastructure$session_active ||
              !app_state$infrastructure$background_tasks_active
            if (session_check) {
              log_debug("Stopping periodic cleanup - session ended", .context = "BACKGROUND_CLEANUP")
              return()
            }

            log_debug("Running scheduled comprehensive system cleanup", .context = "BACKGROUND_CLEANUP")
            safe_operation(
              "Scheduled system cleanup",
              code = {
                comprehensive_system_cleanup(app_state)
                log_debug("Scheduled cleanup completed successfully", .context = "BACKGROUND_CLEANUP")
              },
              fallback = NULL,
              session = session,
              error_type = "processing",
              emit = emit,
              app_state = app_state
            )

            # Schedule next cleanup only if session is still active
            should_continue <- app_state$infrastructure$session_active &&
              app_state$infrastructure$background_tasks_active
            if (should_continue) {
              later::later(schedule_periodic_cleanup, delay = cleanup_interval_minutes * 60)
            }
          }

          # Start the periodic cleanup cycle
          schedule_periodic_cleanup()
        })
      },
      delay = cleanup_interval_minutes * 60
    )

    log_debug(
      paste("Background cleanup scheduled every", cleanup_interval_minutes, "minutes"),
      .context = "BACKGROUND_TASKS"
    )
  } else {
    log_warn("later package not available - background cleanup disabled", .context = "BACKGROUND_TASKS")
  }

  # PERFORMANCE MONITORING INTEGRATION - Schedule periodic reporting
  if (requireNamespace("later", quietly = TRUE)) {
    report_interval_minutes <- 15

    later::later(
      function() {
        shiny::withReactiveDomain(session, {
          # Recursive performance reporting
          schedule_periodic_reporting <- function() {
            # Check if session is still active
            session_check <- !app_state$infrastructure$session_active ||
              !app_state$infrastructure$background_tasks_active
            if (session_check) {
              return()
            }

            safe_operation(
              "Performance report generation",
              code = {
                report <- get_performance_report(app_state)
                log_debug(report$formatted_text, .context = "PERFORMANCE_MONITOR")

                # Check if system needs attention
                if (report$health_status == "WARNING") {
                  log_warn(
                    paste(
                      "System health WARNING - Queue:", report$queue_utilization_pct,
                      "% | Tokens:", report$token_utilization_pct, "%"
                    ),
                    .context = "PERFORMANCE_MONITOR"
                  )
                }
              },
              fallback = NULL,
              session = session,
              error_type = "processing",
              emit = emit,
              app_state = app_state
            )

            # Schedule next report only if session is still active
            should_continue <- app_state$infrastructure$session_active &&
              app_state$infrastructure$background_tasks_active
            if (should_continue) {
              later::later(schedule_periodic_reporting, delay = report_interval_minutes * 60)
            }
          }

          # Start the periodic reporting cycle
          schedule_periodic_reporting()
        })
      },
      delay = report_interval_minutes * 60
    )

    log_debug(
      paste("Performance monitoring scheduled every", report_interval_minutes, "minutes"),
      .context = "BACKGROUND_TASKS"
    )
  }

  log_debug("Background tasks configured", .context = "BACKGROUND_TASKS")
}

#' Initialize Test Mode
#'
#' Sets up test mode with auto-loading of example data if enabled.
#'
#' @param app_state Centralized app state
#' @param emit Event emission API
#' @param session Shiny session object
#' @param hashed_token Hashed session token for logging
#' @param session_debugger Session debugger object
#'
#' @export
initialize_test_mode <- function(app_state, emit, session, hashed_token, session_debugger) {
  # TEST MODE: Auto-indlæs eksempel data hvis aktiveret
  test_mode_auto_load <- get_test_mode_auto_load()

  debug_log(
    "Checking TEST_MODE configuration",
    "SESSION_LIFECYCLE",
    level = "INFO",
    session_id = hashed_token,
    details = list(auto_load = test_mode_auto_load)
  )

  if (test_mode_auto_load) {
    log_debug("TEST MODE: Auto-loading example data enabled", .context = "TEST_MODE")

    # Schedule test data loading after app initialization completes
    shiny::observeEvent(
      shiny::reactive(TRUE),
      {
        log_debug("TEST MODE: Preparing to load example data", .context = "TEST_MODE")

        # Use later() to defer loading until all observers are ready
        # This prevents race conditions with auto-detection system
        startup_delay_ms <- if (exists("TEST_MODE_STARTUP_DEBOUNCE_MS") &&
          !is.null(TEST_MODE_STARTUP_DEBOUNCE_MS)) {
          TEST_MODE_STARTUP_DEBOUNCE_MS
        } else {
          300
        }

        later::later(
          function() {
            shiny::withReactiveDomain(session, {
              safe_operation(
                "Load test mode example data",
                code = {
                  log_debug("TEST MODE: Loading example data now", .context = "TEST_MODE")

                  # Load SPC example data
                  example_file <- system.file("R/data/spc_data_with_extra_columns.csv", package = "SPCify")
                  if (nchar(example_file) == 0 || !file.exists(example_file)) {
                    example_file <- "R/data/spc_data_with_extra_columns.csv"
                  }

                  if (file.exists(example_file)) {
                    file_data <- readr::read_csv(example_file,
                      show_col_types = FALSE,
                      locale = readr::locale(encoding = "UTF-8")
                    )

                    attr(file_data, "file_name") <- "spc_data_with_extra_columns.csv"

                    # Store data in app_state and emit event
                    app_state$data$current_data <- file_data
                    app_state$data$original_data <- file_data
                    app_state$session$file_uploaded <- TRUE
                    app_state$session$file_name <- "spc_data_with_extra_columns.csv"

                    emit$data_updated(context = "test_mode_auto_load")

                    log_debug(
                      paste("TEST MODE: Example data loaded successfully:", nrow(file_data), "rows"),
                      .context = "TEST_MODE"
                    )

                    session_debugger$event("test_data_loaded")
                  } else {
                    log_warn(
                      paste("TEST MODE: Example file not found:", example_file),
                      .context = "TEST_MODE"
                    )
                  }
                },
                fallback = function(e) {
                  log_error(
                    paste("TEST MODE: Failed to load example data:", e$message),
                    .context = "TEST_MODE"
                  )
                },
                error_type = "file_operations"
              )
            })
          },
          delay = startup_delay_ms / 1000
        )
      },
      once = TRUE,
      ignoreInit = FALSE,
      priority = OBSERVER_PRIORITIES$LOW
    )
  }

  # Emit test_mode_ready event AFTER all observers are set up
  later::later(
    function() {
      shiny::withReactiveDomain(session, {
        safe_operation(
          "Emit test_mode_ready event",
          code = {
            emit$test_mode_ready()
            log_debug("TEST MODE: test_mode_ready event emitted", .context = "TEST_MODE")
          },
          fallback = function(e) {
            log_error(
              paste("Failed to emit test_mode_ready event:", e$message),
              .context = "TEST_MODE"
            )
          },
          error_type = "processing"
        )
      })
    },
    delay = 1.5
  )

  log_debug("Test mode initialized", .context = "TEST_MODE")
}
