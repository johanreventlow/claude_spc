# Microbenchmark Performance Utilities
# Advanced statistical benchmarking for SPC App critical functions
#
# This module provides comprehensive benchmarking capabilities using
# the microbenchmark package for statistical analysis of performance.

#' Safely evaluate expressions in benchmark context
#' Secure wrapper for eval() that validates input and provides controlled execution
#' @param expr Expression, call, or function to evaluate
#' @param operation_name Name of the operation for logging
#' @return Result of expression evaluation
#' @noRd
safe_eval_benchmark <- function(expr, operation_name = "unknown") {
  # Enhanced input validation
  if (is.null(expr)) {
    log_error(paste("NULL expression in benchmark:", operation_name), "[SECURITY]")
    stop("NULL expression not allowed in benchmarks")
  }

  # Type validation - only allow safe expression types
  valid_types <- c("expression", "call", "function", "symbol", "language")
  expr_type <- typeof(expr)

  if (!expr_type %in% valid_types) {
    log_error("Invalid expression type in benchmark", .context = "[SECURITY]")
    log_debug_kv(
      operation = operation_name,
      type = expr_type,
      class = paste(class(expr), collapse = ", "),
      .context = "[SECURITY]"
    )
    stop(paste("Invalid expression type for benchmarking:", expr_type))
  }

  # For functions, validate environment
  if (is.function(expr)) {
    expr_env <- environment(expr)
    if (!identical(expr_env, globalenv()) &&
      !identical(expr_env, baseenv()) &&
      !identical(expr_env, .GlobalEnv)) {
      log_warn("Non-standard environment in benchmark function", .context = "[SECURITY]")
      log_debug_kv(
        operation = operation_name,
        .context = "[SECURITY]"
      )
    }
  }

  # Safe execution with error handling
  tryCatch(
    {
      eval(expr)
    },
    error = function(e) {
      log_error(paste("Benchmark execution failed:", e$message), .context = "[BENCHMARK]")
      log_debug_kv(
        operation = operation_name,
        .context = "[BENCHMARK]"
      )
      stop("Benchmark execution failed safely")
    }
  )
}

#' Microbenchmark Wrapper for SPC App Functions
#'
#' Provides statistical benchmarking with integrated logging and reporting.
#' Uses microbenchmark package for precise timing measurements with multiple
#' iterations and statistical analysis.
#'
#' @param expr Expression to benchmark (can be a single expression or list)
#' @param times Number of iterations (default: 100)
#' @param operation_name Descriptive name for the operation
#' @param log_results Whether to log results (default: TRUE)
#' @param return_full_results Return full microbenchmark object (default: FALSE)
#' @param capture_result Capture and return the result of the last expression evaluation (default: FALSE)
#'
#' @return List with summary statistics or full microbenchmark results
#' @examples
#' \dontrun{
#' # Benchmark autodetect engine
#' results <- benchmark_spc_operation(
#'   {
#'     autodetect_engine(test_data, "manual", app_state, emit)
#'   },
#'   operation_name = "autodetect_engine"
#' )
#'
#' # Compare multiple implementations
#' results <- benchmark_spc_operation(list(
#'   old_implementation = {
#'     old_function()
#'   },
#'   new_implementation = {
#'     new_function()
#'   }
#' ), operation_name = "implementation_comparison")
#' }
#' @export
benchmark_spc_operation <- function(expr, times = 100, operation_name = "unknown_operation",
                                    log_results = TRUE, return_full_results = FALSE,
                                    capture_result = FALSE) {
  # Check if microbenchmark is available
  if (!requireNamespace("microbenchmark", quietly = TRUE)) {
    log_warn("microbenchmark package not available - falling back to basic timing",
      .context = "MICROBENCHMARK"
    )

    # Fallback to existing measure_reactive_performance
    if (exists("measure_reactive_performance")) {
      return(measure_reactive_performance(expr, operation_name))
    } else {
      start_time <- Sys.time()
      # SECURITY: Use safe evaluation wrapper
      result <- safe_eval_benchmark(expr, operation_name)
      execution_time <- as.numeric(Sys.time() - start_time)

      result_data <- list(
        mean_time = execution_time,
        operation = operation_name,
        fallback = TRUE
      )

      if (capture_result) {
        result_data$captured_result <- result
      }

      return(result_data)
    }
  }

  log_debug(paste("Starting microbenchmark for:", operation_name))

  # Execute microbenchmark
  tryCatch(
    {
      mb_results <- microbenchmark::microbenchmark(
        expr,
        times = times,
        unit = "ms"
      )

      # Extract summary statistics
      summary_stats <- summary(mb_results)

      # Create standardized results structure
      results <- list(
        operation = operation_name,
        times = times,
        min_ms = summary_stats$min,
        q1_ms = summary_stats$lq,
        median_ms = summary_stats$median,
        mean_ms = summary_stats$mean,
        q3_ms = summary_stats$uq,
        max_ms = summary_stats$max,
        timestamp = Sys.time(),
        unit = "milliseconds"
      )

      # Add full results if requested
      if (return_full_results) {
        results$full_benchmark <- mb_results
        results$summary_table <- summary_stats
      }

      # Capture result if requested
      if (capture_result) {
        # SECURITY: Use safe evaluation wrapper
        results$captured_result <- safe_eval_benchmark(expr, operation_name)
      }

      # Log results if requested
      if (log_results) {
        log_performance_results(results)
      }

      return(results)
    },
    error = function(e) {
      log_error(paste("Microbenchmark failed for", operation_name, ":", e$message))

      # Fallback to basic timing
      start_time <- Sys.time()
      # SECURITY: Use safe evaluation wrapper
      result <- safe_eval_benchmark(expr, operation_name)
      execution_time <- as.numeric(Sys.time() - start_time) * 1000 # Convert to ms

      error_result <- list(
        operation = operation_name,
        mean_ms = execution_time,
        error = e$message,
        fallback = TRUE
      )

      if (capture_result) {
        error_result$captured_result <- result
      }

      return(error_result)
    }
  )
}

#' Log Performance Results
#'
#' Logs microbenchmark results in structured format for analysis.
#'
#' @param results Results from benchmark_spc_operation
#' @param warn_threshold Warning threshold in milliseconds (default: 500ms)
log_performance_results <- function(results, warn_threshold = 500) {
  if (is.null(results$median_ms)) {
    # Fallback format
    log_debug(paste(
      "BENCHMARK:", results$operation, "-",
      round(results$mean_ms, 2), "ms (fallback)"
    ))
    return()
  }

  # Structured logging for full results
  log_debug_kv(
    operation = results$operation,
    min_ms = round(results$min_ms, 2),
    median_ms = round(results$median_ms, 2),
    mean_ms = round(results$mean_ms, 2),
    max_ms = round(results$max_ms, 2),
    iterations = results$times
  )

  # Warning for slow operations
  if (results$median_ms > warn_threshold) {
    log_warn(
      paste(
        "SLOW OPERATION:", results$operation, "median:",
        round(results$median_ms, 2), "ms"
      ),
      "PERFORMANCE_WARNING"
    )
  }

  # Info level summary for important operations
  if (grepl("autodetect|qic|plot", results$operation, ignore.case = TRUE)) {
    log_info(
      paste(
        "BENCHMARK COMPLETE:", results$operation,
        "median:", round(results$median_ms, 2), "ms",
        "range:", round(results$min_ms, 2), "-", round(results$max_ms, 2), "ms"
      ),
      "PERFORMANCE_BENCHMARK"
    )
  }
}

#' Benchmark Autodetect Engine Performance
#'
#' Specialized benchmarking for autodetect engine with different trigger types.
#' Tests performance across multiple scenarios and data sizes.
#'
#' @param data_list List of data frames to test (default: generates test data)
#' @param trigger_types Vector of trigger types to test (default: common types)
#' @param app_state App state object
#' @param emit Emit functions object
#' @param times Number of iterations per test (default: 10 for expensive operations)
#'
#' @return Data frame with comparative benchmark results
#' @export
benchmark_autodetect_comprehensive <- function(data_list = NULL,
                                               trigger_types = c("file_upload", "manual"),
                                               app_state, emit, times = 10) {
  # Generate test data if not provided
  if (is.null(data_list)) {
    data_list <- list(
      small = data.frame(
        Dato = seq(as.Date("2024-01-01"), by = "day", length.out = 30),
        Taeller = sample(90:110, 30, replace = TRUE),
        Naevner = sample(95:105, 30, replace = TRUE)
      ),
      medium = data.frame(
        Dato = seq(as.Date("2024-01-01"), by = "day", length.out = 365),
        Taeller = sample(90:110, 365, replace = TRUE),
        Naevner = sample(95:105, 365, replace = TRUE)
      ),
      large = data.frame(
        Dato = seq(as.Date("2020-01-01"), by = "day", length.out = 1460), # 4 years
        Taeller = sample(90:110, 1460, replace = TRUE),
        Naevner = sample(95:105, 1460, replace = TRUE)
      )
    )
  }

  results_list <- list()

  log_debug_kv(
    message = "Starting comprehensive autodetect benchmarking",
    data_sets = length(data_list),
    trigger_types = length(trigger_types),
    .context = "PERFORMANCE_BENCHMARK"
  )

  for (data_name in names(data_list)) {
    for (trigger_type in trigger_types) {
      operation_name <- paste0("autodetect_", data_name, "_", trigger_type)
      data <- data_list[[data_name]]

      log_debug(paste("Benchmarking:", operation_name, "with", nrow(data), "rows"), )

      # Reset autodetect state between tests
      if (!is.null(app_state)) {
        shiny::isolate({
          app_state$columns$auto_detect$in_progress <- FALSE
          app_state$columns$auto_detect$completed <- FALSE
          app_state$columns$auto_detect$frozen_until_next_trigger <- FALSE
        })
      }

      # Benchmark the operation
      result <- benchmark_spc_operation(
        expr = {
          if (exists("autodetect_engine")) {
            autodetect_engine(data, trigger_type, app_state, emit)
          } else {
            # Fallback for testing environments
            Sys.sleep(0.001) # Simulate minimal work
            list(x_col = "Dato", y_col = "Taeller")
          }
        },
        times = times,
        operation_name = operation_name,
        log_results = FALSE # We'll log manually
      )

      # Add context information
      result$data_size <- nrow(data)
      result$data_name <- data_name
      result$trigger_type <- trigger_type

      results_list[[operation_name]] <- result
    }
  }

  # Convert to data frame for analysis
  results_df <- do.call(rbind, lapply(results_list, function(r) {
    data.frame(
      operation = r$operation,
      data_size = r$data_size %||% 0,
      data_name = r$data_name %||% "unknown",
      trigger_type = r$trigger_type %||% "unknown",
      median_ms = r$median_ms %||% r$mean_ms %||% 0,
      mean_ms = r$mean_ms %||% 0,
      min_ms = r$min_ms %||% 0,
      max_ms = r$max_ms %||% 0,
      stringsAsFactors = FALSE
    )
  }))

  # Log summary
  log_info(
    paste("Autodetect benchmarking complete:", nrow(results_df), "test scenarios"),
  )

  # Log performance summary by data size
  if (nrow(results_df) > 0) {
    size_summary <- aggregate(median_ms ~ data_name, results_df, mean)
    for (i in seq_len(nrow(size_summary))) {
      log_debug_kv(
        data_size = size_summary$data_name[i],
        avg_median_ms = round(size_summary$median_ms[i], 2),
      )
    }
  }

  return(results_df)
}

#' Benchmark QIC Plot Generation Performance
#'
#' Specialized benchmarking for QIC plot generation with different chart types and data sizes.
#'
#' @param data_list List of data frames to test
#' @param chart_types Vector of chart types (default: common types)
#' @param times Number of iterations (default: 5 for plot generation)
#'
#' @return Data frame with QIC benchmark results
#' @export
benchmark_qic_generation <- function(data_list = NULL,
                                     chart_types = c("run", "p", "u"),
                                     times = 5) {
  # Generate test data if not provided
  if (is.null(data_list)) {
    data_list <- list(
      small = data.frame(
        Dato = seq(as.Date("2024-01-01"), by = "month", length.out = 12),
        Taeller = sample(50:150, 12),
        Naevner = sample(100:200, 12)
      ),
      medium = data.frame(
        Dato = seq(as.Date("2022-01-01"), by = "month", length.out = 36),
        Taeller = sample(50:150, 36),
        Naevner = sample(100:200, 36)
      )
    )
  }

  results_list <- list()

  log_debug_kv(
    message = "Starting QIC plot generation benchmarking",
    data_sets = length(data_list),
    chart_types = length(c("run", "p", "c")),
    .context = "PERFORMANCE_BENCHMARK"
  )

  for (data_name in names(data_list)) {
    for (chart_type in chart_types) {
      operation_name <- paste0("qic_", chart_type, "_", data_name)
      data <- data_list[[data_name]]

      # Benchmark QIC generation if function exists
      result <- benchmark_spc_operation(
        expr = {
          if (exists("generateSPCPlot") && requireNamespace("qicharts2", quietly = TRUE)) {
            # Create minimal config for testing
            config <- list(
              x_col = "Dato",
              y_col = "Taeller",
              n_col = if (chart_type %in% c("p", "u")) "Naevner" else NULL
            )

            generateSPCPlot(
              data = data,
              config = config,
              chart_type = chart_type,
              target_value = NULL,
              centerline_value = NULL,
              show_phases = FALSE,
              skift_column = NULL,
              frys_column = NULL,
              kommentar_column = NULL
            )
          } else {
            # Fallback simulation
            Sys.sleep(runif(1, 0.01, 0.05)) # Simulate plot generation time
            ggplot2::ggplot() +
              ggplot2::geom_point()
          }
        },
        times = times,
        operation_name = operation_name,
        log_results = FALSE
      )

      # Add context
      result$data_size <- nrow(data)
      result$data_name <- data_name
      result$chart_type <- chart_type

      results_list[[operation_name]] <- result
    }
  }

  # Convert to analysis format
  results_df <- do.call(rbind, lapply(results_list, function(r) {
    data.frame(
      operation = r$operation,
      data_size = r$data_size %||% 0,
      data_name = r$data_name %||% "unknown",
      chart_type = r$chart_type %||% "unknown",
      median_ms = r$median_ms %||% r$mean_ms %||% 0,
      mean_ms = r$mean_ms %||% 0,
      min_ms = r$min_ms %||% 0,
      max_ms = r$max_ms %||% 0,
      stringsAsFactors = FALSE
    )
  }))

  # Log summary
  log_info(
    paste("QIC benchmarking complete:", nrow(results_df), "chart type scenarios"),
  )

  return(results_df)
}

#' Performance Comparison Analysis
#'
#' Compares benchmark results and identifies performance regressions or improvements.
#'
#' @param baseline_results Previous benchmark results
#' @param current_results Current benchmark results
#' @param regression_threshold Threshold for detecting regressions (default: 1.2 = 20% slower)
#'
#' @return List with comparison analysis
#' @export
analyze_performance_comparison <- function(baseline_results, current_results, regression_threshold = 1.2) {
  if (!is.data.frame(baseline_results) || !is.data.frame(current_results)) {
    log_warn("Performance comparison requires data frame inputs", .context = "MICROBENCHMARK")
    return(NULL)
  }

  # Match operations between baseline and current
  common_ops <- intersect(baseline_results$operation, current_results$operation)

  if (length(common_ops) == 0) {
    log_warn("No common operations found for performance comparison", .context = "MICROBENCHMARK")
    return(list(message = "No common operations"))
  }

  comparisons <- list()
  regressions <- list()
  improvements <- list()

  for (op in common_ops) {
    baseline_median <- baseline_results[baseline_results$operation == op, "median_ms"][1]
    current_median <- current_results[current_results$operation == op, "median_ms"][1]

    if (is.na(baseline_median) || is.na(current_median)) next

    ratio <- current_median / baseline_median

    comparison <- list(
      operation = op,
      baseline_median = baseline_median,
      current_median = current_median,
      ratio = ratio,
      change_percent = (ratio - 1) * 100,
      change_ms = current_median - baseline_median
    )

    comparisons[[op]] <- comparison

    # Detect regressions
    if (ratio >= regression_threshold) {
      regressions[[op]] <- comparison
      log_warn(
        paste(
          "PERFORMANCE REGRESSION:", op,
          sprintf(
            "%.1f%% slower (%.2f -> %.2f ms)",
            comparison$change_percent, baseline_median, current_median
          )
        ),
      )
    }

    # Detect improvements (>10% faster)
    if (ratio <= 0.9) {
      improvements[[op]] <- comparison
      log_info(
        paste(
          "PERFORMANCE IMPROVEMENT:", op,
          sprintf(
            "%.1f%% faster (%.2f -> %.2f ms)",
            abs(comparison$change_percent), baseline_median, current_median
          )
        ),
      )
    }
  }

  summary_stats <- list(
    total_comparisons = length(comparisons),
    regressions_count = length(regressions),
    improvements_count = length(improvements),
    stable_count = length(comparisons) - length(regressions) - length(improvements)
  )

  log_info(
    paste(
      "Performance comparison complete:",
      summary_stats$total_comparisons, "operations,",
      summary_stats$regressions_count, "regressions,",
      summary_stats$improvements_count, "improvements"
    ),
  )

  return(list(
    comparisons = comparisons,
    regressions = regressions,
    improvements = improvements,
    summary = summary_stats
  ))
}

#' Export Benchmark Results
#'
#' Exports benchmark results to CSV for external analysis and reporting.
#'
#' @param results Benchmark results (data frame or list)
#' @param filename Output filename (optional - will generate timestamp-based name)
#' @param include_metadata Include system metadata (default: TRUE)
#'
#' @return Path to exported file
#' @export
export_benchmark_results <- function(results, filename = NULL, include_metadata = TRUE) {
  if (is.null(filename)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- paste0("benchmark_results_", timestamp, ".csv")
  }

  # Convert list to data frame if needed
  if (is.list(results) && !is.data.frame(results)) {
    if ("comparisons" %in% names(results)) {
      # Handle comparison results
      results_df <- do.call(rbind, lapply(results$comparisons, function(comp) {
        data.frame(
          operation = comp$operation,
          baseline_median = comp$baseline_median,
          current_median = comp$current_median,
          change_percent = comp$change_percent,
          stringsAsFactors = FALSE
        )
      }))
    } else {
      log_error("Cannot convert benchmark results to data frame", .context = "MICROBENCHMARK")
      return(NULL)
    }
  } else {
    results_df <- results
  }

  # Add metadata if requested
  if (include_metadata && is.data.frame(results_df)) {
    results_df$benchmark_timestamp <- Sys.time()
    results_df$r_version <- R.version.string
    results_df$platform <- Sys.info()["sysname"]
  }

  # Export to CSV
  tryCatch(
    {
      if (requireNamespace("readr", quietly = TRUE)) {
        readr::write_csv(results_df, filename)
      } else {
        write.csv(results_df, filename, row.names = FALSE)
      }

      log_info(paste("Benchmark results exported to:", filename))
      return(filename)
    },
    error = function(e) {
      log_error(paste("Failed to export benchmark results:", e$message))
      return(NULL)
    }
  )
}
