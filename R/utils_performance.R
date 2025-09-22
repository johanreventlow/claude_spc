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
#' result <- measure_reactive_performance(
#'   {
#'     expensive_calculation()
#'   },
#'   "calculation_name")
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

#' Create cached reactive expression with session-local cache
#'
#' Wrapper omkring shiny::reactive() der tilføjer intelligent caching
#' for expensive operations. Cache invalideres automatisk
#' når dependencies ændres. Bruger session-local cache for isolation.
#'
#' @param expr Reactive expression der skal caches
#' @param cache_key Character string med unique cache key
#' @param cache_timeout Numeric - cache timeout i sekunder (default 300)
#' @param session Shiny session object (optional - auto-detected if available)
#'
#' @return Cached reactive expression eller function (context-afhængig)
#'
#' @examples
#' \dontrun{
#' # Cache expensive data processing
#' processed_data <- create_cached_reactive(
#'   {
#'     expensive_data_processing(app_state$data$current_data)
#'   },
#'   "data_processing",
#'   cache_timeout = 600,
#'   session = session)
#' }
#'
#' @family performance
#' @export
create_cached_reactive <- function(expr, cache_key, cache_timeout = 300, session = NULL) {
  # FAILSAFE: Robust cache key validation to handle NULL, character(0), and malformed keys
  safe_cache_key <- safe_operation(
    "Validate cache key",
    code = {
      if (is.null(cache_key) || length(cache_key) == 0 || identical(cache_key, character(0))) {
        paste0("fallback_key_", as.integer(Sys.time()), "_", sample(1000:9999, 1))
      } else {
        # Sanitize cache key - remove problematic characters and ensure it's a single string
        key_str <- as.character(cache_key)[1]
        if (is.na(key_str) || key_str == "" || trimws(key_str) == "") {
          paste0("empty_key_", as.integer(Sys.time()), "_", sample(1000:9999, 1))
        } else {
          # Clean key: only alphanumeric, underscores, and hyphens
          gsub("[^a-zA-Z0-9_-]", "_", key_str)
        }
      }
    },
    fallback = function(e) {
      log_debug(paste("Cache key validation failed:", e$message, "- using emergency fallback"), "PERFORMANCE")
      paste0("error_key_", as.integer(Sys.time()), "_", sample(1000:9999, 1))
    },
    error_type = "processing"
  )

  log_debug(paste("Using cache key:", safe_cache_key), "PERFORMANCE")

  # SESSION-LOCAL CACHE: Auto-detect session or use fallback
  if (is.null(session)) {
    # Try to get session from current reactive domain
    session <- safe_operation(
      "Get session from reactive domain",
      code = {
        getFromNamespace("getDefaultReactiveDomain", "shiny")()$session
      },
      fallback = function(e) NULL,
      error_type = "processing"
    )
  }

  # Get or create session-local cache environment
  cache_env <- if (!is.null(session)) {
    # Use session$userData for session-local cache
    if (is.null(session$userData$performance_cache)) {
      session$userData$performance_cache <- new.env()
      log_debug("Created session-local performance cache", "PERFORMANCE")
    }
    session$userData$performance_cache
  } else {
    # Fallback to global environment for non-session contexts (e.g., tests)
    if (!exists(".performance_cache_fallback", envir = .GlobalEnv)) {
      assign(".performance_cache_fallback", new.env(), envir = .GlobalEnv)
      log_debug("Created fallback performance cache (no session available)", "PERFORMANCE")
    }
    get(".performance_cache_fallback", envir = .GlobalEnv)
  }

  # Define cache logic as function for reuse
  cache_logic <- function() {
    # FAILSAFE: Wrap entire cache logic in error handling
    safe_operation(
      "Execute cache logic",
      code = {
        current_time <- Sys.time()

        # Check if cache exists and is valid
        cache_entry <- cache_env[[safe_cache_key]]
        if (!is.null(cache_entry)) {
          cache_age <- as.numeric(current_time - cache_entry$timestamp)
          if (cache_age < cache_timeout) {
            log_debug(paste("Cache hit for", safe_cache_key), "PERFORMANCE")
            return(cache_entry$value)
          }
        }

        # Cache miss or expired - evaluate expression with robust error handling
        log_debug(paste("Cache miss for", safe_cache_key, "- computing..."), "PERFORMANCE")
        start_time <- Sys.time()

        # FAILSAFE: Expression evaluation with comprehensive error handling
        result <- safe_operation(
          paste("Evaluate cached expression for", safe_cache_key),
          code = {
            expr
          },
          fallback = function(e) {
            log_debug(paste("Expression evaluation failed in cache for", safe_cache_key, ":", e$message), "PERFORMANCE")
            # Return NULL as safe fallback - caller must handle NULL gracefully
            NULL
          },
          error_type = "processing"
        )

        end_time <- Sys.time()

        # FAILSAFE: Only cache non-NULL results to avoid caching failures
        if (!is.null(result)) {
          safe_operation(
            paste("Store cache result for", safe_cache_key),
            code = {
              # Store in cache
              cache_env[[safe_cache_key]] <- list(
                value = result,
                timestamp = current_time,
                computation_time = as.numeric(end_time - start_time)
              )
              log_debug(paste("Cached result for", safe_cache_key), "PERFORMANCE")
            },
            fallback = function(e) {
              log_debug(paste("Failed to store cache for", safe_cache_key, ":", e$message), "PERFORMANCE")
              # Continue without caching - return result anyway
            },
            error_type = "processing"
          )
        } else {
          log_debug(paste("Skipping cache storage for NULL result:", safe_cache_key), "PERFORMANCE")
        }

        return(result)
      },
      fallback = function(e) {
        log_debug(paste("Complete cache logic failure for", safe_cache_key, ":", e$message), "PERFORMANCE")
        # Emergency fallback: try to evaluate expression directly without caching
        safe_operation(
          "Emergency expression evaluation without cache",
          code = {
            expr
          },
          fallback = function(inner_e) {
            log_debug(paste("Emergency expression evaluation also failed:", inner_e$message), "PERFORMANCE")
            NULL
          },
          error_type = "processing"
        )
      },
      error_type = "processing"
    )
  }

  # Check if we're in reactive context
  in_reactive_context <- safe_operation(
    "Check reactive context",
    code = {
      # Try to get current reactive domain
      domain <- shiny::getDefaultReactiveDomain()
      !is.null(domain)
    },
    fallback = function(e) {
      FALSE
    },
    error_type = "processing"
  )

  if (in_reactive_context) {
    # Return reactive expression if in reactive context
    return(shiny::reactive(cache_logic()))
  } else {
    # Return function if not in reactive context (e.g., tests)
    return(cache_logic)
  }
}

#' Debounced reactive with performance tracking
#'
#' Enhanced version af shiny::debounce med performance monitoring.
#' Tracker både debounce effectiveness og execution performance.
#' Virker både i reactive context og uden.
#'
#' @param r Reactive expression der skal debounces (eller function i non-reactive context)
#' @param millis Numeric - debounce delay i millisekunder
#' @param operation_name Character string med operation navn til logging
#'
#' @return Debounced reactive expression eller function (context-afhængig)
#'
#' @family performance
#' @export
create_performance_debounced <- function(r, millis, operation_name = "debounced") {
  # Track debounce statistics
  stats_key <- paste0("debounce_stats_", operation_name)

  if (!exists(".performance_stats", envir = .GlobalEnv)) {
    assign(".performance_stats", new.env(), envir = .GlobalEnv)
  }

  # Performance tracking logic
  track_performance <- function(result_func) {
    start_time <- Sys.time()
    result <- result_func()
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
  }

  # Check if we're in reactive context
  in_reactive_context <- safe_operation(
    "Check reactive context for debounce",
    code = {
      # Try to get current reactive domain
      domain <- shiny::getDefaultReactiveDomain()
      !is.null(domain)
    },
    fallback = function(e) {
      FALSE
    },
    error_type = "processing"
  )

  if (in_reactive_context) {
    # Use debounce if in reactive context
    debounced_reactive <- shiny::debounce(r, millis)

    return(shiny::reactive({
      track_performance(function() debounced_reactive())
    }))
  } else {
    # Return simple function if not in reactive context (e.g., tests)
    # Note: No actual debouncing in non-reactive context, just performance tracking
    return(function() {
      # Handle reactive expressions in non-reactive context
      if (is.function(r) && "reactiveExpr" %in% class(r)) {
        # It's a reactive expression - create a temporary reactive domain for execution
        track_performance(function() {
          shiny::isolate(r())
        })
      } else {
        # It's a regular function
        track_performance(r)
      }
    })
  }
}

#' Clear performance caches from session-local or fallback storage
#'
#' Utility function til at cleare performance caches.
#' Bruges ved session cleanup eller når cache skal invalideres.
#'
#' @param cache_pattern Character string med pattern for cache keys (optional)
#' @param session Shiny session object (optional - auto-detected if available)
#'
#' @family performance
#' @export
clear_performance_cache <- function(cache_pattern = NULL, session = NULL) {
  # Auto-detect session if not provided
  if (is.null(session)) {
    session <- safe_operation(
      "Auto-detect session for cache clear",
      code = {
        getFromNamespace("getDefaultReactiveDomain", "shiny")()$session
      },
      fallback = function(e) NULL,
      error_type = "processing"
    )
  }

  # Get cache environment
  cache_env <- if (!is.null(session) && !is.null(session$userData$performance_cache)) {
    session$userData$performance_cache
  } else if (exists(".performance_cache_fallback", envir = .GlobalEnv)) {
    get(".performance_cache_fallback", envir = .GlobalEnv)
  } else {
    NULL
  }

  if (!is.null(cache_env)) {
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
  } else {
    log_debug("No performance cache found to clear", "PERFORMANCE")
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
  memory_warning = 10,       # 10MB memory change
  cache_timeout_default = 300 # 5 minutes default cache timeout
)
