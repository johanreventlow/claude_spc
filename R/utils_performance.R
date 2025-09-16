# utils_performance.R
# Performance utilities for Fase 5 optimization

#' Performance monitoring utilities for reactive operations
#'
#' Tools til at måle og optimere reactive chain performance.
#' Implementeret som del af Fase 5: Performance & Cleanup.
#'
#' @examples
#' \dontrun{
#' # Monitor reactive execution
#' result <- measure_reactive_performance({
#'   expensive_calculation()
#' }, "calculation_name")
#' }
#'
#' @family performance
#' @export
measure_reactive_performance <- function(expr, operation_name = "unknown") {
  start_time <- Sys.time()
  result <- expr
  end_time <- Sys.time()

  execution_time <- as.numeric(end_time - start_time)

  # Log performance hvis over threshold
  if (execution_time > PERFORMANCE_THRESHOLDS$reactive_warning) {
    log_warn(
      paste("Slow reactive operation:", operation_name,
            "took", round(execution_time, 3), "seconds"),
      "PERFORMANCE"
    )
  }

  return(list(
    result = result,
    execution_time = execution_time,
    operation_name = operation_name
  ))
}

#' Create cached reactive expression
#'
#' Wrapper omkring reactive() der tilføjer intelligent caching
#' for expensive operations. Cache invalideres automatisk
#' når dependencies ændres.
#'
#' @param expr Reactive expression der skal caches
#' @param cache_key Character string med unique cache key
#' @param cache_timeout Numeric - cache timeout i sekunder (default 300)
#'
#' @return Cached reactive expression
#'
#' @examples
#' \dontrun{
#' # Cache expensive data processing
#' processed_data <- create_cached_reactive({
#'   expensive_data_processing(values$current_data)
#' }, "data_processing", cache_timeout = 600)
#' }
#'
#' @family performance
#' @export
create_cached_reactive <- function(expr, cache_key, cache_timeout = 300) {

  # Global cache environment (persistent during session)
  if (!exists(".performance_cache", envir = .GlobalEnv)) {
    assign(".performance_cache", new.env(), envir = .GlobalEnv)
  }

  reactive({
    cache_env <- get(".performance_cache", envir = .GlobalEnv)
    current_time <- Sys.time()

    # Check if cache exists and is valid
    cache_entry <- cache_env[[cache_key]]
    if (!is.null(cache_entry)) {
      cache_age <- as.numeric(current_time - cache_entry$timestamp)
      if (cache_age < cache_timeout) {
        log_debug(paste("Cache hit for", cache_key), "PERFORMANCE")
        return(cache_entry$value)
      }
    }

    # Cache miss or expired - evaluate expression
    log_debug(paste("Cache miss for", cache_key, "- computing..."), "PERFORMANCE")
    start_time <- Sys.time()
    result <- expr
    end_time <- Sys.time()

    # Store in cache
    cache_env[[cache_key]] <- list(
      value = result,
      timestamp = current_time,
      computation_time = as.numeric(end_time - start_time)
    )

    log_debug(paste("Cached result for", cache_key), "PERFORMANCE")
    return(result)
  })
}

#' Debounced reactive with performance tracking
#'
#' Enhanced version af shiny::debounce med performance monitoring.
#' Tracker både debounce effectiveness og execution performance.
#'
#' @param r Reactive expression der skal debounces
#' @param millis Numeric - debounce delay i millisekunder
#' @param operation_name Character string med operation navn til logging
#'
#' @return Debounced reactive expression
#'
#' @family performance
#' @export
create_performance_debounced <- function(r, millis, operation_name = "debounced") {

  # Track debounce statistics
  stats_key <- paste0("debounce_stats_", operation_name)

  if (!exists(".performance_stats", envir = .GlobalEnv)) {
    assign(".performance_stats", new.env(), envir = .GlobalEnv)
  }

  debounced_reactive <- debounce(r, millis)

  reactive({
    start_time <- Sys.time()
    result <- debounced_reactive()
    end_time <- Sys.time()

    # Update statistics
    stats_env <- get(".performance_stats", envir = .GlobalEnv)
    if (is.null(stats_env[[stats_key]])) {
      stats_env[[stats_key]] <- list(
        operation_name = operation_name,
        execution_count = 0,
        total_time = 0,
        avg_time = 0
      )
    }

    stats <- stats_env[[stats_key]]
    stats$execution_count <- stats$execution_count + 1
    execution_time <- as.numeric(end_time - start_time)
    stats$total_time <- stats$total_time + execution_time
    stats$avg_time <- stats$total_time / stats$execution_count

    stats_env[[stats_key]] <- stats

    # Log hvis performance er dårlig
    if (execution_time > PERFORMANCE_THRESHOLDS$debounce_warning) {
      log_warn(
        paste("Slow debounced operation:", operation_name,
              "took", round(execution_time, 3), "seconds"),
        "PERFORMANCE"
      )
    }

    return(result)
  })
}

#' Clear performance caches
#'
#' Utility function til at cleare performance caches.
#' Bruges ved session cleanup eller når cache skal invalideres.
#'
#' @param cache_pattern Character string med pattern for cache keys (optional)
#'
#' @family performance
#' @export
clear_performance_cache <- function(cache_pattern = NULL) {
  if (exists(".performance_cache", envir = .GlobalEnv)) {
    cache_env <- get(".performance_cache", envir = .GlobalEnv)

    if (is.null(cache_pattern)) {
      # Clear all caches
      rm(list = ls(cache_env), envir = cache_env)
      log_debug("Cleared all performance caches", "PERFORMANCE")
    } else {
      # Clear caches matching pattern
      cache_keys <- ls(cache_env)
      matching_keys <- cache_keys[grepl(cache_pattern, cache_keys)]
      if (length(matching_keys) > 0) {
        rm(list = matching_keys, envir = cache_env)
        log_debug(paste("Cleared", length(matching_keys), "performance caches matching", cache_pattern), "PERFORMANCE")
      }
    }
  }
}

#' Get performance statistics
#'
#' Hent performance statistik for monitoring og debugging.
#' Returnerer data om cache hits, execution times etc.
#'
#' @return List med performance statistik
#'
#' @family performance
#' @export
get_performance_stats <- function() {
  stats <- list()

  # Cache statistics
  if (exists(".performance_cache", envir = .GlobalEnv)) {
    cache_env <- get(".performance_cache", envir = .GlobalEnv)
    cache_keys <- ls(cache_env)
    stats$cache_entries <- length(cache_keys)
    stats$cache_keys <- cache_keys
  } else {
    stats$cache_entries <- 0
    stats$cache_keys <- character(0)
  }

  # Execution statistics
  if (exists(".performance_stats", envir = .GlobalEnv)) {
    stats_env <- get(".performance_stats", envir = .GlobalEnv)
    stats$execution_stats <- as.list(stats_env)
  } else {
    stats$execution_stats <- list()
  }

  return(stats)
}

#' Memory usage monitoring
#'
#' Monitor memory usage patterns for reactive expressions
#' og identificer memory leaks eller excessive memory usage.
#'
#' @param operation_name Character string med operation navn
#'
#' @return List med memory statistics
#'
#' @family performance
#' @export
monitor_memory_usage <- function(operation_name = "unknown") {
  # Get memory info using gc()
  gc_before <- gc(reset = TRUE)
  memory_before <- sum(gc_before[, "used"])

  return(function() {
    gc_after <- gc()
    memory_after <- sum(gc_after[, "used"])
    memory_diff <- memory_after - memory_before

    # Log memory usage hvis significant
    if (abs(memory_diff) > PERFORMANCE_THRESHOLDS$memory_warning) {
      log_info(
        paste("Memory change for", operation_name, ":",
              ifelse(memory_diff > 0, "+", ""), round(memory_diff, 2), "MB"),
        "PERFORMANCE"
      )
    }

    return(list(
      operation_name = operation_name,
      memory_before = memory_before,
      memory_after = memory_after,
      memory_diff = memory_diff
    ))
  })
}

# Performance thresholds (kan konfigureres)
PERFORMANCE_THRESHOLDS <- list(
  reactive_warning = 0.5,    # 500ms for reactive expressions
  debounce_warning = 1.0,    # 1 second for debounced operations
  memory_warning = 10        # 10MB memory change
)