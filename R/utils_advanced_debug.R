# utils_advanced_debug.R
# Advanced debug infrastructure for comprehensive workflow tracing
# Created: 2025-09-17
# Purpose: End-to-end debug visibility for complex Shiny state management

# Dependencies ----------------------------------------------------------------
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required for state hashing")
}

# CONSTANTS ===================================================================

## Debug Log Categories
DEBUG_CATEGORIES <- list(
  SESSION_LIFECYCLE = "SESSION_LIFECYCLE",    # Session init/cleanup
  FILE_UPLOAD_FLOW = "FILE_UPLOAD_FLOW",      # Upload → processing → storage
  AUTO_DETECT_FLOW = "AUTO_DETECT_FLOW",      # Detection → UI sync
  STATE_TRANSITION = "STATE_TRANSITION",      # State changes
  UI_SYNC_FLOW = "UI_SYNC_FLOW",             # UI update operations
  NAVIGATION_FLOW = "NAVIGATION_FLOW",        # Welcome → main app
  PERFORMANCE = "PERFORMANCE",                # Timing metrics
  ERROR_HANDLING = "ERROR_HANDLING",          # Error context
  WORKFLOW_TRACE = "WORKFLOW_TRACE"           # End-to-end operation tracking
)

## Debug Levels
DEBUG_LEVELS <- list(
  TRACE = 1,     # Detailed step-by-step operations
  DEBUG = 2,     # General debug information
  INFO = 3,      # Important state changes
  WARN = 4,      # Potential issues
  ERROR = 5      # Error conditions
)

# CORE DEBUG UTILITIES ========================================================

#' Enhanced Debug Logger
#'
#' Comprehensive logging med category, level og context information
#'
#' @param message Log message
#' @param category Debug category from DEBUG_CATEGORIES
#' @param level Debug level from DEBUG_LEVELS (default: DEBUG)
#' @param context Additional context information (optional)
#' @param session_id Shiny session ID (optional)
#' @param timestamp Custom timestamp (optional, defaults to current)
#'
#' @examples
#' debug_log("File upload started", "FILE_UPLOAD_FLOW", level = "INFO")
#' debug_log("Auto-detect failed", "AUTO_DETECT_FLOW", level = "ERROR",
#'           context = list(file_size = 1024, columns = c("A", "B")))
debug_log <- function(message, category, level = "DEBUG", context = NULL,
                      session_id = NULL, timestamp = NULL) {

  # Validate inputs
  if (!category %in% unlist(DEBUG_CATEGORIES)) {
    category <- "UNKNOWN"
  }

  if (!level %in% names(DEBUG_LEVELS)) {
    level <- "DEBUG"
  }

  # Generate timestamp if not provided
  if (is.null(timestamp)) {
    timestamp <- Sys.time()
  }

  # Format timestamp
  timestamp_str <- format(timestamp, "%H:%M:%S.%f")

  # Build log entry
  log_parts <- list(
    timestamp = paste0("[", timestamp_str, "]"),
    level = paste0(level, ":"),
    category = paste0("[", category, "]"),
    message = message
  )

  # Add session ID if provided
  if (!is.null(session_id)) {
    log_parts$session <- paste0("(Session: ", substr(session_id, 1, 8), ")")
  }

  # Add context if provided
  if (!is.null(context)) {
    context_str <- ""
    if (is.list(context)) {
      context_items <- sapply(names(context), function(name) {
        value <- context[[name]]
        if (is.vector(value) && length(value) > 1) {
          paste0(name, "=[", paste(value, collapse = ","), "]")
        } else {
          paste0(name, "=", value)
        }
      })
      context_str <- paste0(" |", paste(context_items, collapse = " "))
    }
    log_parts$context <- context_str
  }

  # Output log entry
  log_entry <- paste(log_parts, collapse = " ")
  cat(log_entry, "\n")

  # Store in global debug history if enabled
  if (exists("GLOBAL_DEBUG_HISTORY", envir = .GlobalEnv)) {
    .GlobalEnv$GLOBAL_DEBUG_HISTORY <- append(.GlobalEnv$GLOBAL_DEBUG_HISTORY,
                                               list(list(
                                                 timestamp = timestamp,
                                                 level = level,
                                                 category = category,
                                                 message = message,
                                                 context = context,
                                                 session_id = session_id
                                               )))
  }
}

#' State Snapshot Utility
#'
#' Creates detailed snapshot af app_state for debugging og comparison
#'
#' @param checkpoint_name Name of the checkpoint for identification
#' @param app_state Current app state object
#' @param include_hash Include state hash for change detection (default: TRUE)
#' @param include_data_summary Include data summary info (default: TRUE)
#' @param session_id Shiny session ID (optional)
#'
#' @return List med snapshot information
#'
#' @examples
#' snapshot <- debug_state_snapshot("before_upload", app_state)
#' debug_state_snapshot("after_upload", app_state)
debug_state_snapshot <- function(checkpoint_name, app_state, include_hash = TRUE,
                                  include_data_summary = TRUE, session_id = NULL) {

  debug_log(paste("Taking state snapshot:", checkpoint_name),
            "STATE_TRANSITION", level = "TRACE", session_id = session_id)

  snapshot <- list(
    checkpoint = checkpoint_name,
    timestamp = Sys.time(),
    session_id = session_id
  )

  if (!is.null(app_state)) {
    # Basic state information
    snapshot$state_available <- TRUE

    # State hash for change detection
    if (include_hash) {
      snapshot$state_hash <- digest::digest(app_state, algo = "md5")
    }

    # Data summary - safe reactive access
    if (include_data_summary && !is.null(app_state$data)) {
      data_summary <- list()

      # Safe access to reactive values
      current_data <- safe_operation(
        "Get current data for debug snapshot",
        code = {
          if (shiny::isRunning()) {
            shiny::isolate(app_state$data$current_data)
          } else {
            NULL
          }
        },
        fallback = function(e) NULL,
        error_type = "processing"
      )

      if (!is.null(current_data)) {
        data_summary$current_data <- list(
          rows = nrow(current_data),
          cols = ncol(current_data),
          col_names = names(current_data)
        )
      }

      original_data <- safe_operation(
        "Get original data for debug snapshot",
        code = {
          if (shiny::isRunning()) {
            shiny::isolate(app_state$data$original_data)
          } else {
            NULL
          }
        },
        fallback = function(e) NULL,
        error_type = "processing"
      )

      if (!is.null(original_data)) {
        data_summary$original_data <- list(
          rows = nrow(original_data),
          cols = ncol(original_data)
        )
      }

      snapshot$data_summary <- data_summary
    }

    # Session state summary - safe reactive access
    if (!is.null(app_state$session)) {
      session_state <- safe_operation(
        "Get session state for debug snapshot",
        code = {
          if (shiny::isRunning()) {
            list(
              file_uploaded = shiny::isolate(app_state$session$file_uploaded %||% FALSE),
              user_started_session = shiny::isolate(app_state$session$user_started_session %||% FALSE),
              auto_save_enabled = shiny::isolate(app_state$session$auto_save_enabled %||% TRUE)
            )
          } else {
            list(
              file_uploaded = FALSE,
              user_started_session = FALSE,
              auto_save_enabled = TRUE
            )
          }
        },
        fallback = function(e) {
          list(
            file_uploaded = FALSE,
            user_started_session = FALSE,
            auto_save_enabled = TRUE
          )
        },
        error_type = "processing"
      )
      snapshot$session_state <- session_state
    }

    # Column management state - safe reactive access
    if (!is.null(app_state$columns)) {
      column_state <- safe_operation(
        "Get column state for debug snapshot",
        code = {
          if (shiny::isRunning()) {
            list(
              auto_detect_completed = shiny::isolate(app_state$columns$auto_detect$completed %||% FALSE),
              auto_detect_in_progress = shiny::isolate(app_state$columns$auto_detect$in_progress %||% FALSE)
            )
          } else {
            list(
              auto_detect_completed = FALSE,
              auto_detect_in_progress = FALSE
            )
          }
        },
        fallback = function(e) {
          list(
            auto_detect_completed = FALSE,
            auto_detect_in_progress = FALSE
          )
        },
        error_type = "processing"
      )
      snapshot$column_state <- column_state
    }

    # UI state - safe reactive access
    if (!is.null(app_state$ui)) {
      ui_state <- safe_operation(
        "Get UI state for debug snapshot",
        code = {
          if (shiny::isRunning()) {
            list(
              welcome_screen_active = shiny::isolate(app_state$ui$welcome_screen_active %||% TRUE),
              current_chart_type = shiny::isolate(app_state$ui$current_chart_type %||% "unknown")
            )
          } else {
            list(
              welcome_screen_active = TRUE,
              current_chart_type = "unknown"
            )
          }
        },
        fallback = function(e) {
          list(
            welcome_screen_active = TRUE,
            current_chart_type = "unknown"
          )
        },
        error_type = "processing"
      )
      snapshot$ui_state <- ui_state
    }

  } else {
    snapshot$state_available <- FALSE
    debug_log("State snapshot warning: app_state is NULL",
              "STATE_TRANSITION", level = "WARN", session_id = session_id)
  }

  # Log snapshot summary
  context <- list(
    checkpoint = checkpoint_name,
    state_available = snapshot$state_available
  )

  if (snapshot$state_available && include_hash) {
    context$state_hash <- substr(snapshot$state_hash, 1, 8)
  }

  debug_log("State snapshot completed", "STATE_TRANSITION", level = "TRACE",
            context = context, session_id = session_id)

  return(snapshot)
}

#' Performance Timer Utility
#'
#' High-precision timing for workflow operations
#'
#' @param operation_name Name of the operation being timed
#' @param session_id Shiny session ID (optional)
#'
#' @return Timer object med checkpoint og completion methods
#'
#' @examples
#' timer <- debug_performance_timer("file_upload_workflow")
#' # ... upload operations ...
#' timer$checkpoint("upload_complete")
#' # ... processing operations ...
#' timer$complete("workflow_complete")
debug_performance_timer <- function(operation_name, session_id = NULL) {

  start_time <- Sys.time()
  checkpoints <- list()

  debug_log(paste("Performance timer started:", operation_name),
            "PERFORMANCE", level = "TRACE", session_id = session_id)

  timer <- list(
    operation = operation_name,
    start_time = start_time,
    session_id = session_id,

    # Add checkpoint during operation
    checkpoint = function(checkpoint_name) {
      checkpoint_time <- Sys.time()
      elapsed <- as.numeric(difftime(checkpoint_time, start_time, units = "secs"))

      checkpoints[[checkpoint_name]] <<- list(
        time = checkpoint_time,
        elapsed = elapsed
      )

      debug_log(paste("Checkpoint:", checkpoint_name), "PERFORMANCE",
                level = "TRACE",
                context = list(elapsed_seconds = round(elapsed, 3)),
                session_id = session_id)

      return(elapsed)
    },

    # Complete timing and log summary
    complete = function(final_checkpoint_name = "complete") {
      end_time <- Sys.time()
      total_elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

      checkpoints[[final_checkpoint_name]] <<- list(
        time = end_time,
        elapsed = total_elapsed
      )

      # Log performance summary
      context <- list(
        operation = operation_name,
        total_seconds = round(total_elapsed, 3),
        checkpoints = length(checkpoints)
      )

      debug_log(paste("Performance timer completed:", operation_name),
                "PERFORMANCE", level = "INFO", context = context,
                session_id = session_id)

      return(list(
        operation = operation_name,
        total_elapsed = total_elapsed,
        checkpoints = checkpoints,
        start_time = start_time,
        end_time = end_time
      ))
    },

    # Get current elapsed time
    elapsed = function() {
      as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    }
  )

  class(timer) <- "debug_performance_timer"
  return(timer)
}

#' Workflow Tracer
#'
#' End-to-end workflow tracking med state snapshots og performance metrics
#'
#' @param workflow_name Name of the workflow being traced
#' @param app_state App state object for snapshots
#' @param session_id Shiny session ID (optional)
#'
#' @return Workflow tracer object
#'
#' @examples
#' tracer <- debug_workflow_tracer("file_upload_to_visualization", app_state)
#' tracer$step("upload_started")
#' # ... upload operations ...
#' tracer$step("auto_detect_triggered")
#' # ... auto-detect operations ...
#' tracer$complete("visualization_ready")
debug_workflow_tracer <- function(workflow_name, app_state = NULL, session_id = NULL) {

  start_time <- Sys.time()
  steps <- list()
  snapshots <- list()

  debug_log(paste("Workflow trace started:", workflow_name),
            "WORKFLOW_TRACE", level = "INFO", session_id = session_id)

  # Take initial snapshot
  if (!is.null(app_state)) {
    initial_snapshot <- debug_state_snapshot(
      paste0(workflow_name, "_start"),
      app_state,
      session_id = session_id
    )
    snapshots[["start"]] <- initial_snapshot
  }

  tracer <- list(
    workflow = workflow_name,
    start_time = start_time,
    session_id = session_id,

    # Add workflow step
    step = function(step_name, context = NULL) {
      step_time <- Sys.time()
      elapsed <- as.numeric(difftime(step_time, start_time, units = "secs"))

      step_info <- list(
        name = step_name,
        time = step_time,
        elapsed = elapsed,
        context = context
      )

      steps[[step_name]] <<- step_info

      # Take state snapshot if app_state available
      if (!is.null(app_state)) {
        snapshot <- debug_state_snapshot(
          paste0(workflow_name, "_", step_name),
          app_state,
          session_id = session_id
        )
        snapshots[[step_name]] <<- snapshot
      }

      debug_log(paste("Workflow step:", step_name), "WORKFLOW_TRACE",
                level = "TRACE",
                context = list(elapsed_seconds = round(elapsed, 3)),
                session_id = session_id)

      return(elapsed)
    },

    # Complete workflow tracing
    complete = function(final_step_name = "complete", summary = TRUE) {
      end_time <- Sys.time()
      total_elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

      # Add final step
      final_step <- list(
        name = final_step_name,
        time = end_time,
        elapsed = total_elapsed
      )
      steps[[final_step_name]] <<- final_step

      # Take final snapshot
      if (!is.null(app_state)) {
        final_snapshot <- debug_state_snapshot(
          paste0(workflow_name, "_", final_step_name),
          app_state,
          session_id = session_id
        )
        snapshots[[final_step_name]] <<- final_snapshot
      }

      # Log workflow summary
      if (summary) {
        step_names <- names(steps)
        context <- list(
          workflow = workflow_name,
          total_seconds = round(total_elapsed, 3),
          steps = length(steps),
          step_sequence = paste(step_names, collapse = " → ")
        )

        debug_log(paste("Workflow trace completed:", workflow_name),
                  "WORKFLOW_TRACE", level = "INFO", context = context,
                  session_id = session_id)
      }

      return(list(
        workflow = workflow_name,
        total_elapsed = total_elapsed,
        steps = steps,
        snapshots = snapshots,
        start_time = start_time,
        end_time = end_time
      ))
    },

    # Get current workflow status
    status = function() {
      current_time <- Sys.time()
      elapsed <- as.numeric(difftime(current_time, start_time, units = "secs"))

      return(list(
        workflow = workflow_name,
        elapsed = elapsed,
        steps_completed = length(steps),
        last_step = if (length(steps) > 0) names(steps)[length(steps)] else "none"
      ))
    }
  )

  class(tracer) <- "debug_workflow_tracer"
  return(tracer)
}

#' Session Lifecycle Debugger
#'
#' Tracks complete session from initialization to cleanup
#'
#' @param session_id Shiny session ID
#' @param session_object Shiny session object (optional)
#'
#' @return Session debugger object
debug_session_lifecycle <- function(session_id, session_object = NULL) {

  lifecycle_start <- Sys.time()
  events <- list()

  debug_log("Session lifecycle debugging started", "SESSION_LIFECYCLE",
            level = "INFO", session_id = session_id)

  debugger <- list(
    session_id = session_id,
    start_time = lifecycle_start,

    # Log lifecycle event
    event = function(event_name, context = NULL) {
      event_time <- Sys.time()
      elapsed <- as.numeric(difftime(event_time, lifecycle_start, units = "secs"))

      event_info <- list(
        name = event_name,
        time = event_time,
        elapsed = elapsed,
        context = context
      )

      events[[event_name]] <<- event_info

      debug_log(paste("Session event:", event_name), "SESSION_LIFECYCLE",
                level = "TRACE",
                context = list(elapsed_seconds = round(elapsed, 3)),
                session_id = session_id)
    },

    # Complete lifecycle debugging
    complete = function() {
      end_time <- Sys.time()
      total_elapsed <- as.numeric(difftime(end_time, lifecycle_start, units = "secs"))

      context <- list(
        session_duration_seconds = round(total_elapsed, 3),
        events_tracked = length(events)
      )

      debug_log("Session lifecycle debugging completed", "SESSION_LIFECYCLE",
                level = "INFO", context = context, session_id = session_id)

      return(list(
        session_id = session_id,
        total_duration = total_elapsed,
        events = events,
        start_time = lifecycle_start,
        end_time = end_time
      ))
    }
  )

  class(debugger) <- "debug_session_lifecycle"
  return(debugger)
}

# INITIALIZATION ==============================================================

#' Initialize Advanced Debug System
#'
#' Sets up global debug infrastructure
#'
#' @param enable_history Enable global debug history storage (default: TRUE)
#' @param max_history_entries Maximum number of debug entries to store (default: 1000)
initialize_advanced_debug <- function(enable_history = TRUE, max_history_entries = 1000) {

  if (enable_history) {
    .GlobalEnv$GLOBAL_DEBUG_HISTORY <- list()
    .GlobalEnv$DEBUG_MAX_HISTORY <- max_history_entries
  }

  debug_log("Advanced debug system initialized", "SESSION_LIFECYCLE", level = "INFO")

  log_info("=== ADVANCED DEBUG SYSTEM ACTIVE ===", "ADVANCED_DEBUG")
  log_info("Available utilities:", "ADVANCED_DEBUG")
  log_info("- debug_log()              Enhanced logging med categories", "ADVANCED_DEBUG")
  log_info("- debug_state_snapshot()   State inspection og comparison", "ADVANCED_DEBUG")
  log_info("- debug_performance_timer() High-precision operation timing", "ADVANCED_DEBUG")
  log_info("- debug_workflow_tracer()   End-to-end workflow tracking", "ADVANCED_DEBUG")
  log_info("- debug_session_lifecycle() Session creation → cleanup tracking", "ADVANCED_DEBUG")
  log_info("=====================================", "ADVANCED_DEBUG")
}

# NULL-safe operator for backward compatibility
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}