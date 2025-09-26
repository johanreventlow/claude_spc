# R/utils_performance_monitoring.R
# Performance Monitoring System for SPC App Startup
# Provides comprehensive metrics for tracking QIC calls, events and timing

# ENHANCED PERFORMANCE MONITORING ==============================================
# Building on existing functionality with detailed tracking capabilities

#' Reset QIC call counters
#'
#' Resets global counters used to track QIC calculation calls
#'
#' @export
reset_qic_counters <- function() {
  if (exists("qic_call_counter", envir = .GlobalEnv)) {
    rm("qic_call_counter", envir = .GlobalEnv)
  }
  if (exists("actual_qic_call_counter", envir = .GlobalEnv)) {
    rm("actual_qic_call_counter", envir = .GlobalEnv)
  }
  log_debug("QIC performance counters reset", .context = "PERFORMANCE_MONITOR")
}

#' Get current QIC call counts
#'
#' Returns the current state of QIC calculation counters
#'
#' @return List with generateSPCPlot_calls and actual_qic_calls counts
#' @export
get_qic_call_counts <- function() {
  generateSPCPlot_calls <- if (exists("qic_call_counter", envir = .GlobalEnv)) {
    get("qic_call_counter", envir = .GlobalEnv)
  } else {
    0
  }

  actual_qic_calls <- if (exists("actual_qic_call_counter", envir = .GlobalEnv)) {
    get("actual_qic_call_counter", envir = .GlobalEnv)
  } else {
    0
  }

  result <- list(
    generateSPCPlot_calls = generateSPCPlot_calls,
    actual_qic_calls = actual_qic_calls
  )

  log_debug(paste("Current QIC call counts:",
                  "generateSPCPlot:", generateSPCPlot_calls,
                  "actual qic():", actual_qic_calls), "PERFORMANCE_MONITOR")

  return(result)
}

#' Monitor app startup performance
#'
#' Measures QIC calculations during app startup sequence
#'
#' @param timeout_seconds Maximum time to wait for startup (default: 30)
#' @return List with startup metrics
#' @export
monitor_startup_performance <- function(timeout_seconds = 30) {
  log_info("Starting app startup performance monitoring", .context = "PERFORMANCE_MONITOR")

  # Reset counters
  reset_qic_counters()

  start_time <- Sys.time()
  initial_counts <- get_qic_call_counts()

  # Monitor for specified timeout
  Sys.sleep(timeout_seconds)

  end_time <- Sys.time()
  final_counts <- get_qic_call_counts()

  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

  result <- list(
    duration_seconds = duration,
    initial_generateSPCPlot_calls = initial_counts$generateSPCPlot_calls,
    final_generateSPCPlot_calls = final_counts$generateSPCPlot_calls,
    total_generateSPCPlot_calls = final_counts$generateSPCPlot_calls - initial_counts$generateSPCPlot_calls,
    initial_actual_qic_calls = initial_counts$actual_qic_calls,
    final_actual_qic_calls = final_counts$actual_qic_calls,
    total_actual_qic_calls = final_counts$actual_qic_calls - initial_counts$actual_qic_calls,
    calls_per_second_generateSPCPlot = (final_counts$generateSPCPlot_calls - initial_counts$generateSPCPlot_calls) / duration,
    calls_per_second_actual_qic = (final_counts$actual_qic_calls - initial_counts$actual_qic_calls) / duration
  )

  log_info(paste("Startup performance results:",
                 "Duration:", round(duration, 2), "sec",
                 "generateSPCPlot calls:", result$total_generateSPCPlot_calls,
                 "Actual qic() calls:", result$total_actual_qic_calls,
                 "Rate:", round(result$calls_per_second_generateSPCPlot, 2), "generateSPCPlot/sec,",
                 round(result$calls_per_second_actual_qic, 2), "qic()/sec"), "PERFORMANCE_MONITOR")

  return(result)
}

# ENHANCED STARTUP METRICS ===================================================

# Global performance state
.startup_metrics <- new.env(parent = emptyenv())

#' Initialize startup performance monitoring
#'
#' Resets all counters and starts timing for a new startup sequence
#'
#' @export
init_startup_metrics <- function() {
  .startup_metrics$start_time <- Sys.time()
  .startup_metrics$phase_times <- list()
  .startup_metrics$qic_calls <- 0
  .startup_metrics$generateSPCPlot_calls <- 0
  .startup_metrics$events_fired <- list()
  .startup_metrics$event_sequence <- list()
  .startup_metrics$startup_phase <- "initializing"

  # Phase 4: Add memory usage tracking
  .startup_metrics$memory_snapshots <- list()
  .startup_metrics$initial_memory_mb <- round(as.numeric(object.size(search())) / 1024^2, 2)

  log_debug("Enhanced startup metrics initialized", .context = "PERFORMANCE_MONITORING")
}

#' Track memory usage at specific points
#'
#' @param context Character. Context where memory is being tracked
#' @export
track_memory_usage <- function(context = "unknown") {
  if (!exists("start_time", envir = .startup_metrics)) {
    init_startup_metrics()
  }

  # Get current memory usage
  current_memory_mb <- round(as.numeric(object.size(search())) / 1024^2, 2)

  memory_info <- list(
    context = context,
    timestamp = Sys.time(),
    memory_mb = current_memory_mb,
    memory_diff_mb = current_memory_mb - .startup_metrics$initial_memory_mb,
    time_since_start = as.numeric(difftime(Sys.time(), .startup_metrics$start_time, units = "secs"))
  )

  if (!exists("memory_snapshots", envir = .startup_metrics)) {
    .startup_metrics$memory_snapshots <- list()
  }

  .startup_metrics$memory_snapshots[[length(.startup_metrics$memory_snapshots) + 1]] <- memory_info

  log_debug(paste("Memory tracked:", context, "-", current_memory_mb, "MB"),
           .context = "PERFORMANCE_MONITORING")
}

#' Track QIC function call with context
#'
#' @param context Character. Context where QIC was called
#' @param details List. Additional context information
#' @export
track_qic_call <- function(context = "unknown", details = list()) {
  if (!exists("start_time", envir = .startup_metrics)) {
    init_startup_metrics()
  }

  .startup_metrics$qic_calls <- .startup_metrics$qic_calls + 1

  call_info <- list(
    call_number = .startup_metrics$qic_calls,
    timestamp = Sys.time(),
    context = context,
    details = details,
    time_since_start = as.numeric(difftime(Sys.time(), .startup_metrics$start_time, units = "secs"))
  )

  if (!exists("qic_call_details", envir = .startup_metrics)) {
    .startup_metrics$qic_call_details <- list()
  }

  .startup_metrics$qic_call_details[[length(.startup_metrics$qic_call_details) + 1]] <- call_info

  log_debug(paste("QIC call", .startup_metrics$qic_calls, "tracked - context:", context),
           "PERFORMANCE_MONITORING")
}

#' Track generateSPCPlot function call
#'
#' @param context Character. Context where generateSPCPlot was called
#' @param details List. Additional context information
#' @export
track_generateSPCPlot_call <- function(context = "unknown", details = list()) {
  if (!exists("start_time", envir = .startup_metrics)) {
    init_startup_metrics()
  }

  .startup_metrics$generateSPCPlot_calls <- .startup_metrics$generateSPCPlot_calls + 1

  call_info <- list(
    call_number = .startup_metrics$generateSPCPlot_calls,
    timestamp = Sys.time(),
    context = context,
    details = details,
    time_since_start = as.numeric(difftime(Sys.time(), .startup_metrics$start_time, units = "secs"))
  )

  if (!exists("generateSPCPlot_call_details", envir = .startup_metrics)) {
    .startup_metrics$generateSPCPlot_call_details <- list()
  }

  .startup_metrics$generateSPCPlot_call_details[[length(.startup_metrics$generateSPCPlot_call_details) + 1]] <- call_info

  log_debug(paste("generateSPCPlot call", .startup_metrics$generateSPCPlot_calls, "tracked - context:", context),
           "PERFORMANCE_MONITORING")
}

#' Track event firing
#'
#' @param event_name Character. Name of the event
#' @param context Character. Context where event was fired
#' @export
track_event <- function(event_name, context = "unknown") {
  if (!exists("start_time", envir = .startup_metrics)) {
    init_startup_metrics()
  }

  event_info <- list(
    event = event_name,
    timestamp = Sys.time(),
    context = context,
    time_since_start = as.numeric(difftime(Sys.time(), .startup_metrics$start_time, units = "secs"))
  )

  # Add to event sequence
  .startup_metrics$event_sequence[[length(.startup_metrics$event_sequence) + 1]] <- event_info

  # Count occurrences per event type
  if (!exists("events_fired", envir = .startup_metrics)) {
    .startup_metrics$events_fired <- list()
  }

  if (is.null(.startup_metrics$events_fired[[event_name]])) {
    .startup_metrics$events_fired[[event_name]] <- 0
  }

  .startup_metrics$events_fired[[event_name]] <- .startup_metrics$events_fired[[event_name]] + 1

  log_debug(paste("Event tracked:", event_name), "PERFORMANCE_MONITORING")
}

#' Set startup phase
#'
#' @param phase Character. Current startup phase
#' @export
set_startup_phase <- function(phase) {
  if (!exists("start_time", envir = .startup_metrics)) {
    init_startup_metrics()
  }

  current_time <- Sys.time()
  time_since_start <- as.numeric(difftime(current_time, .startup_metrics$start_time, units = "secs"))

  .startup_metrics$phase_times[[phase]] <- list(
    start_time = current_time,
    time_since_start = time_since_start
  )

  .startup_metrics$startup_phase <- phase

  log_debug(paste("Startup phase set to:", phase, "at", round(time_since_start, 3), "seconds"),
           "PERFORMANCE_MONITORING")
}

#' Get enhanced startup metrics
#'
#' @return List with all performance metrics
#' @export
get_enhanced_startup_metrics <- function() {
  if (!exists("start_time", envir = .startup_metrics)) {
    return(list(
      error = "Enhanced metrics not initialized - call init_startup_metrics() first"
    ))
  }

  current_time <- Sys.time()
  total_duration <- as.numeric(difftime(current_time, .startup_metrics$start_time, units = "secs"))

  result <- list(
    # Basic counters
    qic_calls = .startup_metrics$qic_calls,
    generateSPCPlot_calls = .startup_metrics$generateSPCPlot_calls,

    # Timing
    start_time = .startup_metrics$start_time,
    current_time = current_time,
    total_duration_seconds = total_duration,

    # Current state
    current_phase = .startup_metrics$startup_phase,

    # Events
    events_fired = as.list(.startup_metrics$events_fired),
    event_sequence = .startup_metrics$event_sequence,

    # Phase timings
    phase_times = .startup_metrics$phase_times,

    # Detailed call info
    qic_call_details = if(exists("qic_call_details", envir = .startup_metrics)) .startup_metrics$qic_call_details else list(),
    generateSPCPlot_call_details = if(exists("generateSPCPlot_call_details", envir = .startup_metrics)) .startup_metrics$generateSPCPlot_call_details else list()
  )

  return(result)
}

#' Print enhanced startup performance summary
#'
#' @export
print_enhanced_startup_summary <- function() {
  metrics <- get_enhanced_startup_metrics()

  if (!is.null(metrics$error)) {
    cat("ERROR:", metrics$error, "\n")
    return(invisible())
  }

  cat("=== ENHANCED STARTUP PERFORMANCE SUMMARY ===\n")
  cat("Total Duration:", round(metrics$total_duration_seconds, 3), "seconds\n")
  cat("Current Phase:", metrics$current_phase, "\n\n")

  cat("FUNCTION CALLS:\n")
  cat("  QIC calls:", metrics$qic_calls, "\n")
  cat("  generateSPCPlot calls:", metrics$generateSPCPlot_calls, "\n\n")

  # Phase 4: Memory usage summary
  if (!is.null(.startup_metrics$memory_snapshots) && length(.startup_metrics$memory_snapshots) > 0) {
    cat("MEMORY USAGE:\n")
    cat("  Initial memory:", .startup_metrics$initial_memory_mb, "MB\n")
    latest_memory <- .startup_metrics$memory_snapshots[[length(.startup_metrics$memory_snapshots)]]
    cat("  Current memory:", latest_memory$memory_mb, "MB\n")
    cat("  Memory increase:", latest_memory$memory_diff_mb, "MB\n\n")
  }

  cat("EVENTS FIRED:\n")
  if (length(metrics$events_fired) > 0) {
    for (event_name in names(metrics$events_fired)) {
      cat("  ", event_name, ":", metrics$events_fired[[event_name]], "\n")
    }
  } else {
    cat("  No events recorded\n")
  }

  cat("\nEVENT SEQUENCE:\n")
  if (length(metrics$event_sequence) > 0) {
    for (i in seq_along(metrics$event_sequence)) {
      event <- metrics$event_sequence[[i]]
      cat(sprintf("  %2d. %s (%.3fs) - %s\n",
                  i, event$event, event$time_since_start, event$context))
    }
  } else {
    cat("  No event sequence recorded\n")
  }

  # Performance assessment
  cat("\n=== PERFORMANCE ASSESSMENT ===\n")
  if (metrics$qic_calls <= 3) {
    cat("✅ QIC calls: EXCELLENT (≤3)\n")
  } else if (metrics$qic_calls <= 5) {
    cat("⚡ QIC calls: ACCEPTABLE (4-5)\n")
  } else {
    cat("⚠️  QIC calls: NEEDS OPTIMIZATION (>5)\n")
  }

  if (metrics$total_duration_seconds <= 2) {
    cat("✅ Startup time: FAST (≤2s)\n")
  } else if (metrics$total_duration_seconds <= 5) {
    cat("⚡ Startup time: ACCEPTABLE (2-5s)\n")
  } else {
    cat("⚠️  Startup time: SLOW (>5s)\n")
  }
}