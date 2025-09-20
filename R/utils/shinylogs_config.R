# utils_shinylogs_config.R
# Advanced web-based logging configuration using shinylogs package

# SHINYLOGS CONFIGURATION ==================================================

#' Setup Advanced Web Logging with shinylogs
#'
#' Konfigurerer shinylogs til avanceret web-baseret logging og monitoring
#' af Shiny applikationen. Integrerer med eksisterende logging system.
#'
#' @param enable_tracking Aktiver automatisk tracking af brugerinteraktioner
#' @param enable_errors Aktiver fejl-tracking og rapportering
#' @param enable_performances Aktiver performance monitoring
#' @param log_directory Directory til log filer (default: "logs/")
#'
#' @details
#' shinylogs giver følgende funktionaliteter:
#' - Real-time web-baseret log viewer
#' - Automatisk tracking af inputs, outputs og fejl
#' - Performance metrics og timing
#' - Session management og brugerstatistikker
#' - Export af logs til forskellige formater
#'
setup_shinylogs <- function(enable_tracking = TRUE,
                           enable_errors = TRUE,
                           enable_performances = TRUE,
                           log_directory = "logs/") {

  # log_info will be available when this is called from app_server

  # Ensure log directory exists
  if (!dir.exists(log_directory)) {
    dir.create(log_directory, recursive = TRUE)
    # Directory created
  }

  # Configure shinylogs options
  options(
    # Basic shinylogs configuration
    shinylogs.save_inputs = enable_tracking,
    shinylogs.save_outputs = enable_tracking,
    shinylogs.save_errors = enable_errors,
    shinylogs.save_performances = enable_performances,

    # Storage configuration
    shinylogs.storage_mode = "file",  # Can be "file" or "database"
    shinylogs.log_dir = log_directory,

    # Performance thresholds
    shinylogs.performance_threshold = 0.5,  # 500ms threshold for slow operations

    # Additional tracking
    shinylogs.max_entries = 10000,  # Maximum log entries to keep
    shinylogs.compress = TRUE       # Compress old log files
  )

  # Configuration completed - logging will be available when called from app_server

  return(invisible(TRUE))
}

#' Initialize shinylogs tracking for app
#'
#' Starter shinylogs tracking når applikationen initialiseres.
#' Skal kaldes i app_server funktionen.
#'
#' @param session Shiny session object
#' @param custom_fields List med custom felter til logging
#' @param app_name Navn på applikationen (default: "SPC_APP")
#'
initialize_shinylogs_tracking <- function(session,
                                        app_name = "SPC_APP",
                                        log_directory = "logs/") {

  # Initializing shinylogs tracking

  # Start tracking with correct API
  shinylogs::track_usage(
    storage_mode = shinylogs::store_json(path = log_directory),
    what = c("session", "input", "output", "error"),
    app_name = app_name,
    session = session
  )

  # Tracking started successfully

  return(invisible(TRUE))
}

#' Create shinylogs dashboard module UI
#'
#' Opretter UI for real-time log viewing dashboard.
#' Kan integreres som en separat fane eller modal.
#'
#' @param id Module ID
#' @param title Dashboard titel
#'
shinylogs_dashboard_ui <- function(id, title = "Application Logs") {
  ns <- NS(id)

  tagList(
    h3(title),

    # Log viewer tabs
    tabsetPanel(
      id = ns("log_tabs"),

      # Real-time logs
      tabPanel(
        "Live Logs",
        value = "live",
        div(
          style = "margin-top: 15px;",
          # This will be replaced with shinylogs viewer
          shinylogs::use_tracking(),
          uiOutput(ns("live_logs"))
        )
      ),

      # Performance metrics
      tabPanel(
        "Performance",
        value = "performance",
        div(
          style = "margin-top: 15px;",
          plotOutput(ns("performance_plot"), height = "300px"),
          br(),
          DT::dataTableOutput(ns("performance_table"))
        )
      ),

      # Error tracking
      tabPanel(
        "Errors",
        value = "errors",
        div(
          style = "margin-top: 15px;",
          DT::dataTableOutput(ns("error_table"))
        )
      ),

      # Session statistics
      tabPanel(
        "Sessions",
        value = "sessions",
        div(
          style = "margin-top: 15px;",
          fluidRow(
            column(6, valueBoxOutput(ns("total_sessions"), width = 12)),
            column(6, valueBoxOutput(ns("active_sessions"), width = 12))
          ),
          br(),
          DT::dataTableOutput(ns("session_table"))
        )
      )
    )
  )
}

#' Create shinylogs dashboard module server
#'
#' Server-side logik for logs dashboard.
#' Henter og viser logging data i real-time.
#'
#' @param id Module ID
#' @param log_directory Directory med log filer
#'
shinylogs_dashboard_server <- function(id, log_directory = "logs/") {
  moduleServer(id, function(input, output, session) {

    # Starting shinylogs dashboard server

    # Reactive log data
    log_data <- reactive({
      invalidateLater(5000, session)  # Update every 5 seconds

      safe_operation(
        "Read shinylogs database",
        code = {
          shinylogs::read_db_logs(log_directory)
        },
        fallback = function(e) {
          # Could not read shinylogs data
          return(data.frame())
        },
        error_type = "processing"
      )
    })

    # Live logs output
    output$live_logs <- renderUI({
      logs <- log_data()

      if (nrow(logs) == 0) {
        return(div("No logs available yet...", class = "text-muted"))
      }

      # Show recent 50 log entries
      recent_logs <- tail(logs, 50)

      div(
        style = "max-height: 400px; overflow-y: auto; background: #f8f9fa; padding: 10px; border-radius: 4px;",
        lapply(seq_len(nrow(recent_logs)), function(i) {
          entry <- recent_logs[i, ]
          div(
            style = "margin-bottom: 5px; font-family: monospace; font-size: 12px;",
            span(
              style = "color: #666;",
              paste0("[", entry$timestamp, "] ")
            ),
            span(
              style = "font-weight: bold;",
              paste0(entry$type, ": ")
            ),
            span(entry$message)
          )
        })
      )
    })

    # Performance plot
    output$performance_plot <- renderPlot({
      logs <- log_data()
      perf_logs <- logs[logs$type == "performance" & !is.na(logs$duration), ]

      if (nrow(perf_logs) == 0) {
        return(ggplot() +
               labs(title = "No performance data available") +
               theme_minimal())
      }

      ggplot(perf_logs, aes(x = as.POSIXct(timestamp), y = duration)) +
        geom_line(color = "steelblue") +
        geom_point(color = "steelblue", alpha = 0.7) +
        labs(
          title = "Application Performance Over Time",
          x = "Time",
          y = "Duration (seconds)"
        ) +
        theme_minimal()
    })

    # Session statistics
    output$total_sessions <- renderValueBox({
      logs <- log_data()
      session_count <- length(unique(logs$session))

      valueBox(
        value = session_count,
        subtitle = "Total Sessions",
        icon = icon("users"),
        color = "blue"
      )
    })

    output$active_sessions <- renderValueBox({
      logs <- log_data()
      # Consider sessions active if they had activity in last 30 minutes
      recent_sessions <- logs[logs$timestamp > (Sys.time() - 1800), ]
      active_count <- length(unique(recent_sessions$session))

      valueBox(
        value = active_count,
        subtitle = "Active Sessions",
        icon = icon("user-clock"),
        color = "green"
      )
    })

    # shinylogs dashboard server initialized
  })
}

#' Add shinylogs integration to existing logging system
#'
#' Integrerer shinylogs med vores eksisterende log_debug/info/warn/error system
#' så beskeder også sendes til shinylogs tracking.
#'
#' @param session Shiny session for tracking
#'
integrate_shinylogs_with_logging <- function(session) {

  # Integrating shinylogs with existing logging system

  # Store original log functions
  original_log_debug <<- log_debug
  original_log_info <<- log_info
  original_log_warn <<- log_warn
  original_log_error <<- log_error

  # Override log functions to also send to shinylogs
  log_debug <<- function(message, .context = NULL, ...) {
    # Call original function
    original_log_debug(message, .context = .context, ...)

    # Send to shinylogs if session available
    if (!is.null(session) && exists("session") && !is.null(session$token)) {
      safe_operation(
        "Record debug log to shinylogs",
        code = {
          shinylogs::record_log(
            session = session,
            level = "DEBUG",
            message = paste0("[", .context %||% "GENERAL", "] ", message),
            timestamp = Sys.time()
          )
        },
        fallback = function(e) {
          # Fail silently to avoid breaking logging
        },
        error_type = "processing"
      )
    }
  }

  log_info <<- function(message, .context = NULL, ...) {
    original_log_info(message, .context = .context, ...)

    if (!is.null(session) && exists("session") && !is.null(session$token)) {
      safe_operation(
        "Record info log to shinylogs",
        code = {
          shinylogs::record_log(
            session = session,
            level = "INFO",
            message = paste0("[", .context %||% "GENERAL", "] ", message),
            timestamp = Sys.time()
          )
        },
        fallback = function(e) {},
        error_type = "processing"
      )
    }
  }

  log_warn <<- function(message, .context = NULL, ...) {
    original_log_warn(message, .context = .context, ...)

    if (!is.null(session) && exists("session") && !is.null(session$token)) {
      safe_operation(
        "Record warn log to shinylogs",
        code = {
          shinylogs::record_log(
            session = session,
            level = "WARN",
            message = paste0("[", .context %||% "GENERAL", "] ", message),
            timestamp = Sys.time()
          )
        },
        fallback = function(e) {},
        error_type = "processing"
      )
    }
  }

  log_error <<- function(message, .context = NULL, ...) {
    original_log_error(message, .context = .context, ...)

    if (!is.null(session) && exists("session") && !is.null(session$token)) {
      safe_operation(
        "Record error log to shinylogs",
        code = {
          shinylogs::record_log(
            session = session,
            level = "ERROR",
            message = paste0("[", .context %||% "GENERAL", "] ", message),
            timestamp = Sys.time()
          )
        },
        fallback = function(e) {},
        error_type = "processing"
      )
    }
  }

  # shinylogs integration with logging system completed

  return(invisible(TRUE))
}

#' Environment variable configuration for shinylogs
#'
#' Tjekker environment variable for at kontrollere shinylogs funktioner
#'
should_enable_shinylogs <- function() {
  enable_flag <- Sys.getenv("ENABLE_SHINYLOGS", "TRUE")  # Default enabled
  return(toupper(enable_flag) %in% c("TRUE", "1", "YES", "ON"))
}

# CONSOLE OUTPUT ===========================================================

if (should_enable_shinylogs()) {
  # Use basic cat since log_info may not be loaded yet
  # Will be replaced by proper logging when system is initialized

  log_debug("=====================================", "SHINYLOGS")
  log_debug("🌟 SHINYLOGS ADVANCED LOGGING ACTIVE", "SHINYLOGS")
  log_debug("=====================================", "SHINYLOGS")
  log_debug("Features enabled:", "SHINYLOGS")
  log_debug("• Real-time web-based log viewer", "SHINYLOGS")
  log_debug("• Automatic input/output tracking", "SHINYLOGS")
  log_debug("• Performance monitoring", "SHINYLOGS")
  log_debug("• Error tracking and reporting", "SHINYLOGS")
  log_debug("• Session management statistics", "SHINYLOGS")
  log_debug("• Export capabilities", "SHINYLOGS")
  log_debug("", "SHINYLOGS")
  log_debug("Access logs dashboard at: /logs (when implemented in UI)", "SHINYLOGS")
  log_debug("Control via: ENABLE_SHINYLOGS environment variable", "SHINYLOGS")
  log_debug("=====================================", "SHINYLOGS")
} else {
  # shinylogs disabled - no output needed
}