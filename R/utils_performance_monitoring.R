# Performance Monitoring Utilities
# Functions for measuring app startup performance and QIC calculation frequency

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
  log_debug("QIC performance counters reset", "PERFORMANCE_MONITOR")
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
  log_info("Starting app startup performance monitoring", "PERFORMANCE_MONITOR")

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