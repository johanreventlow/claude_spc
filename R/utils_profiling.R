# utils_profiling.R
# Performance profiling and benchmarking utilities
# Sprint 3 Fase 2 - Profiling infrastructure

#' Profile Reactive Expression
#'
#' Wrapper til profvis profiling af reactive expressions.
#' Giver detaljeret performance analysis af reactive execution.
#'
#' @param expr Reactive expression at profile
#' @param label Profile label for identification
#' @param interval Sampling interval i sekunder (default: 0.005 = 5ms)
#'
#' @return Profvis object med profiling resultater
#'
#' @details
#' Bruger profvis til at identificere performance bottlenecks i reactive expressions.
#' Profiling data inkluderer:
#' - Function call stacks
#' - Time spent per function
#' - Memory allocations
#' - Line-by-line execution times
#'
#' @examples
#' \dontrun{
#' # Profile en expensive reactive computation
#' result <- profile_reactive(
#'   reactive({
#'     data %>%
#'       filter(condition) %>%
#'       summarise(metric = mean(value))
#'   }),
#'   label = "data_aggregation"
#' )
#'
#' # View profiling results
#' print(result)
#' }
#'
#' @export
profile_reactive <- function(expr, label = "reactive", interval = 0.005) {
  if (requireNamespace("profvis", quietly = TRUE)) {
    log_debug_kv(
      message = "Starting reactive profiling",
      label = label,
      interval = interval,
      .context = "PROFILING"
    )

    result <- profvis::profvis(
      {
        expr()
      },
      interval = interval
    )

    log_debug_kv(
      message = "Profiling completed",
      label = label,
      .context = "PROFILING"
    )

    return(result)
  } else {
    log_warn(
      "profvis not available - install with: install.packages('profvis')",
      .context = "PROFILING"
    )
    # Execute without profiling
    return(expr())
  }
}

#' Benchmark Reactive Performance
#'
#' Sammenligner performance af forskellige reactive implementations.
#' Bruger bench::mark() til præcis timing og memory measurements.
#'
#' @param ... Named reactive expressions at benchmarke
#' @param times Number of iterations (default: 10)
#' @param check Whether to check results are identical (default: FALSE)
#'
#' @return bench::mark() resultat med performance metrics
#'
#' @details
#' Benchmarking metrics inkluderer:
#' - Median execution time
#' - Memory allocations
#' - Garbage collection counts
#' - Iterations per second
#'
#' @examples
#' \dontrun{
#' # Sammenlign to implementations
#' benchmark_result <- benchmark_reactives(
#'   base_r = reactive({
#'     data[data$value > 10, ]
#'   }),
#'   tidyverse = reactive({
#'     data %>% filter(value > 10)
#'   }),
#'   times = 20
#' )
#'
#' # View benchmark results
#' print(benchmark_result)
#' summary(benchmark_result)
#' }
#'
#' @export
benchmark_reactives <- function(..., times = 10, check = FALSE) {
  if (requireNamespace("bench", quietly = TRUE)) {
    expressions <- list(...)

    log_debug_kv(
      message = "Starting reactive benchmarking",
      expressions_count = length(expressions),
      iterations = times,
      .context = "BENCHMARKING"
    )

    result <- bench::mark(
      ...,
      iterations = times,
      check = check
    )

    log_info(
      paste(
        "Benchmarking completed -",
        length(expressions), "expressions,",
        times, "iterations each"
      ),
      .context = "BENCHMARKING"
    )

    return(result)
  } else {
    log_warn(
      "bench not available - install with: install.packages('bench')",
      .context = "BENCHMARKING"
    )
    return(NULL)
  }
}

#' Track Memory Usage Over Time
#'
#' Periodic memory usage tracking for leak detection og performance monitoring.
#' Samples memory every interval_seconds og gemmer i reactive log.
#'
#' @param session Shiny session object
#' @param interval_seconds Sampling interval (default: 60)
#' @param max_samples Maximum samples at gemme (default: 100)
#'
#' @return ReactiveVal med memory log data.frame
#'
#' @details
#' Memory tracking registrerer:
#' - Timestamp for hver sample
#' - Total memory usage i MB
#' - Memory change siden sidste sample
#' - Session activity status
#'
#' Stops automatisk når session ender.
#'
#' @examples
#' \dontrun{
#' # Start memory tracking i server function
#' memory_log <- track_memory_usage(
#'   session = session,
#'   interval_seconds = 30
#' )
#'
#' # Check memory trends
#' observeEvent(input$show_memory, {
#'   current_log <- memory_log()
#'   print(tail(current_log, 10))
#'
#'   # Check for memory leaks
#'   if (nrow(current_log) > 5) {
#'     recent_trend <- lm(
#'       memory_mb ~ timestamp,
#'       data = tail(current_log, 5)
#'     )
#'     if (coef(recent_trend)[2] > 5) {
#'       warning("Potential memory leak detected")
#'     }
#'   }
#' })
#' }
#'
#' @export
track_memory_usage <- function(session, interval_seconds = 60, max_samples = 100) {
  # Initialize reactive log
  memory_log <- shiny::reactiveVal(data.frame(
    timestamp = numeric(0),
    memory_mb = numeric(0),
    change_mb = numeric(0),
    stringsAsFactors = FALSE
  ))

  log_debug_kv(
    message = "Starting memory tracking",
    interval_seconds = interval_seconds,
    max_samples = max_samples,
    .context = "MEMORY_TRACKING"
  )

  # Recursive memory tracking function
  schedule_memory_sample <- function() {
    # Check if session is still active via tryCatch
    session_active <- tryCatch(
      {
        !is.null(session) && inherits(session, "ShinySession")
      },
      error = function(e) FALSE
    )

    if (!session_active) {
      log_debug("Memory tracking stopped - session closed", .context = "MEMORY_TRACKING")
      return()
    }

    # Sample current memory
    current_mem <- tryCatch(
      {
        # Use pryr if available, fallback to gc()
        if (requireNamespace("pryr", quietly = TRUE)) {
          as.numeric(pryr::mem_used()) / 1024^2
        } else {
          sum(gc()[, 2]) # Total memory from gc()
        }
      },
      error = function(e) {
        log_warn(
          paste("Memory sampling failed:", e$message),
          .context = "MEMORY_TRACKING"
        )
        return(NA_real_)
      }
    )

    if (!is.na(current_mem)) {
      # Get current log
      current_log <- shiny::isolate(memory_log())

      # Calculate change from previous sample
      change_mb <- if (nrow(current_log) > 0) {
        current_mem - tail(current_log$memory_mb, 1)
      } else {
        0
      }

      # Add new sample
      new_entry <- data.frame(
        timestamp = as.numeric(Sys.time()),
        memory_mb = current_mem,
        change_mb = change_mb,
        stringsAsFactors = FALSE
      )

      updated_log <- rbind(current_log, new_entry)

      # Keep only max_samples most recent
      if (nrow(updated_log) > max_samples) {
        updated_log <- tail(updated_log, max_samples)
      }

      # Update reactive log
      memory_log(updated_log)

      # Log significant changes
      if (abs(change_mb) > 10) {
        log_debug_kv(
          message = "Significant memory change detected",
          current_mb = round(current_mem, 2),
          change_mb = round(change_mb, 2),
          .context = "MEMORY_TRACKING"
        )
      }
    }

    # Schedule next sample
    if (requireNamespace("later", quietly = TRUE)) {
      later::later(schedule_memory_sample, delay = interval_seconds)
    }
  }

  # Start tracking
  schedule_memory_sample()

  return(memory_log)
}

#' Get Memory Usage Summary
#'
#' Genererer summary statistik fra memory tracking log.
#'
#' @param memory_log ReactiveVal fra track_memory_usage()
#'
#' @return List med memory statistics
#'
#' @details
#' Summary inkluderer:
#' - Current memory usage
#' - Min/max/mean memory
#' - Total change over tracking period
#' - Trend direction (increasing/stable/decreasing)
#'
#' @examples
#' \dontrun{
#' memory_log <- track_memory_usage(session)
#'
#' # Get summary statistics
#' summary <- get_memory_summary(memory_log)
#' print(summary$current_mb)
#' print(summary$trend)
#' }
#'
#' @export
get_memory_summary <- function(memory_log) {
  log_data <- shiny::isolate(memory_log())

  if (nrow(log_data) == 0) {
    return(list(
      status = "no_data",
      current_mb = NA,
      min_mb = NA,
      max_mb = NA,
      mean_mb = NA,
      total_change_mb = NA,
      trend = "unknown",
      samples_count = 0
    ))
  }

  # Calculate statistics
  current_mb <- tail(log_data$memory_mb, 1)
  min_mb <- min(log_data$memory_mb)
  max_mb <- max(log_data$memory_mb)
  mean_mb <- mean(log_data$memory_mb)
  total_change_mb <- current_mb - log_data$memory_mb[1]

  # Determine trend via simple linear regression
  trend <- if (nrow(log_data) >= 3) {
    trend_model <- lm(memory_mb ~ timestamp, data = log_data)
    slope <- coef(trend_model)[2]

    if (slope > 1) {
      "increasing"
    } else if (slope < -1) {
      "decreasing"
    } else {
      "stable"
    }
  } else {
    "insufficient_data"
  }

  return(list(
    status = "ok",
    current_mb = round(current_mb, 2),
    min_mb = round(min_mb, 2),
    max_mb = round(max_mb, 2),
    mean_mb = round(mean_mb, 2),
    total_change_mb = round(total_change_mb, 2),
    trend = trend,
    samples_count = nrow(log_data),
    duration_minutes = round((tail(log_data$timestamp, 1) - log_data$timestamp[1]) / 60, 1)
  ))
}

#' Profile Data Processing Pipeline
#'
#' Specialiseret profiling for data processing workflows.
#' Måler performance på hvert trin i pipeline.
#'
#' @param pipeline_steps Named list af processing functions
#' @param input_data Data at processe gennem pipeline
#'
#' @return Data.frame med timing for hvert trin
#'
#' @examples
#' \dontrun{
#' # Profile en data processing pipeline
#' pipeline_profile <- profile_data_pipeline(
#'   pipeline_steps = list(
#'     load = function(d) readr::read_csv(d),
#'     clean = function(d) d %>% filter(!is.na(value)),
#'     transform = function(d) d %>% mutate(scaled = scale(value)),
#'     aggregate = function(d) {
#'       d %>%
#'         group_by(category) %>%
#'         summarise(mean = mean(scaled))
#'     }
#'   ),
#'   input_data = "data.csv"
#' )
#'
#' print(pipeline_profile)
#' }
#'
#' @export
profile_data_pipeline <- function(pipeline_steps, input_data) {
  results <- data.frame(
    step = character(0),
    time_ms = numeric(0),
    memory_mb = numeric(0),
    output_rows = numeric(0),
    stringsAsFactors = FALSE
  )

  current_data <- input_data

  for (step_name in names(pipeline_steps)) {
    step_func <- pipeline_steps[[step_name]]

    # Measure performance
    start_time <- Sys.time()
    start_mem <- if (requireNamespace("pryr", quietly = TRUE)) {
      as.numeric(pryr::mem_used()) / 1024^2
    } else {
      NA_real_
    }

    # Execute step
    current_data <- tryCatch(
      step_func(current_data),
      error = function(e) {
        log_error(
          paste("Pipeline step", step_name, "failed:", e$message),
          .context = "PIPELINE_PROFILING"
        )
        return(NULL)
      }
    )

    # Calculate metrics
    end_time <- Sys.time()
    time_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000

    end_mem <- if (requireNamespace("pryr", quietly = TRUE)) {
      as.numeric(pryr::mem_used()) / 1024^2
    } else {
      NA_real_
    }

    memory_mb <- if (!is.na(start_mem) && !is.na(end_mem)) {
      end_mem - start_mem
    } else {
      NA_real_
    }

    output_rows <- if (is.data.frame(current_data)) {
      nrow(current_data)
    } else {
      NA_integer_
    }

    # Record results
    results <- rbind(results, data.frame(
      step = step_name,
      time_ms = round(time_ms, 2),
      memory_mb = round(memory_mb, 2),
      output_rows = output_rows,
      stringsAsFactors = FALSE
    ))

    log_debug_kv(
      message = paste("Pipeline step completed:", step_name),
      time_ms = round(time_ms, 2),
      memory_mb = round(memory_mb, 2),
      .context = "PIPELINE_PROFILING"
    )

    # Stop if step failed
    if (is.null(current_data)) {
      break
    }
  }

  return(results)
}
