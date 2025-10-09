#' Performance Caching Utilities
#'
#' Caching system til at forbedre performance af expensive operations,
#' specielt auto-detection og data processing. Implementeret som del af
#' performance optimization efter tidyverse migration code review.
#'
#' @name performance_caching
NULL

# Global cache environment for session-wide caching
.performance_cache <- new.env(parent = emptyenv())

#' Create Cached Reactive
#'
#' Wrapper around reactive expressions med caching til expensive operations.
#' Implementerer memoization med digest-based cache keys.
#'
#' @param reactive_expr Reactive expression at cache
#' @param cache_key Character string eller function der genererer cache key
#' @param cache_timeout Timeout i sekunder (default: CACHE_CONFIG$default_timeout_seconds)
#' @param cache_size_limit Maximum antal cache entries (default: 50)
#'
#' @return Cached reactive expression
#'
#' @examples
#' # Auto-detection caching
#' cached_autodetect <- create_cached_reactive(
#'   {
#'     detect_columns_full_analysis(data, app_state)
#'   },
#'   "autodetect",
#'   cache_timeout = CACHE_CONFIG$extended_timeout_seconds
#' )
#'
#' # Data-specific caching
#' cached_processing <- create_cached_reactive(
#'   {
#'     expensive_data_processing(data)
#'   },
#'   function() paste0("processing_", digest::digest(data)),
#'   CACHE_CONFIG$default_timeout_seconds
#' )
#'
#' @export
create_cached_reactive <- function(reactive_expr, cache_key, cache_timeout = CACHE_CONFIG$default_timeout_seconds, cache_size_limit = CACHE_CONFIG$size_limit_entries) {
  # FIX BUG #3: Capture expression lazily using substitute()
  # This allows reactive dependencies to trigger re-evaluation
  expr_call <- substitute(reactive_expr)
  expr_env <- parent.frame()

  # Create evaluation function that runs inside reactive context
  expr_fn <- if (is.function(reactive_expr)) {
    # Already a function - use directly
    reactive_expr
  } else {
    # Expression or code block - wrap in function for lazy evaluation
    eval(call("function", pairlist(), expr_call), expr_env)
  }

  # Convert cache_key to function if it's a string
  key_func <- if (is.function(cache_key)) {
    cache_key
  } else {
    function() as.character(cache_key)
  }

  return(reactive({
    # Generate actual cache key
    actual_key <- key_func()

    # Check if cached result exists and is fresh
    cached_result <- get_cached_result(actual_key)

    if (!is.null(cached_result)) {
      log_debug(
        "Cache hit - returning cached result",
        .context = "PERFORMANCE_CACHE"
      )
      log_debug_kv(cache_key = actual_key, .context = "PERFORMANCE_CACHE")
      return(cached_result$value)
    }

    # Cache miss - compute new result
    log_debug(
      "Cache miss - computing new result",
      .context = "PERFORMANCE_CACHE"
    )
    log_debug_kv(cache_key = actual_key, .context = "PERFORMANCE_CACHE")

    # FIX BUG #3: Execute lazily via expr_fn() instead of evaluating reactive_expr directly
    start_time <- Sys.time()
    result <- expr_fn()
    computation_time <- as.numeric(Sys.time() - start_time)

    # Store result in cache
    cache_result(actual_key, result, cache_timeout)

    # Clean cache if needed
    manage_cache_size(cache_size_limit)

    log_debug(
      "Result cached successfully",
      .context = "PERFORMANCE_CACHE"
    )
    log_debug_kv(
      cache_key = actual_key,
      computation_time = computation_time,
      .context = "PERFORMANCE_CACHE"
    )

    return(result)
  }))
}

#' Generate Data-Based Cache Key
#'
#' Genererer cache key baseret på data content ved hjælp af digest.
#' Sikrer at cache keys ændres når data ændres.
#'
#' @param data Data object (data.frame, list, etc.)
#' @param prefix Cache key prefix for identification
#' @param include_names Include column/element names i cache key
#'
#' @return Character string med cache key
#'
#' @examples
#' key <- generate_data_cache_key(my_data, "autodetect")
#' key_detailed <- generate_data_cache_key(my_data, "processing", TRUE)
#'
#' @export
generate_data_cache_key <- function(data, prefix = "data", include_names = FALSE) {
  if (is.null(data) || length(data) == 0) {
    return(paste0(prefix, "_empty"))
  }

  # Basic data digest
  data_digest <- digest::digest(data, algo = "md5")

  # Include structure information for better cache invalidation
  structure_info <- paste0(
    class(data)[1], "_",
    if (is.data.frame(data)) paste0(nrow(data), "x", ncol(data)) else length(data)
  )

  # Include names if requested (for column-dependent operations)
  names_part <- if (include_names && !is.null(names(data))) {
    digest::digest(names(data), algo = "md5")
  } else {
    ""
  }

  cache_key <- paste0(prefix, "_", structure_info, "_", data_digest, "_", names_part)

  # Ensure key is not too long
  if (nchar(cache_key) > 200) {
    cache_key <- paste0(prefix, "_", digest::digest(cache_key, algo = "md5"))
  }

  return(cache_key)
}

#' Cache Auto-Detection Results
#'
#' Specialiseret caching for auto-detection operations med intelligent
#' cache invalidation baseret på data changes og column structure.
#'
#' @param data Data at analysere
#' @param app_state App state object
#' @param force_refresh Force cache refresh (default: FALSE)
#'
#' @return Cached auto-detection results
#'
#' @examples
#' results <- cache_auto_detection_results(data, app_state)
#' fresh_results <- cache_auto_detection_results(data, app_state, TRUE)
#'
#' @export
cache_auto_detection_results <- function(data, app_state, force_refresh = FALSE) {
  # Generate comprehensive cache key for auto-detection
  cache_key <- generate_data_cache_key(data, "autodetect", include_names = TRUE)

  if (!force_refresh) {
    cached_result <- get_cached_result(cache_key)
    if (!is.null(cached_result)) {
      log_debug(
        "Auto-detection cache hit",
        list(cache_key = cache_key, data_dims = dim(data)),
        .context = "AUTO_DETECT_CACHE"
      )
      return(cached_result$value)
    }
  }

  # Cache miss or forced refresh - perform auto-detection
  log_debug(
    "Auto-detection cache miss - running analysis",
    list(cache_key = cache_key, force_refresh = force_refresh),
    .context = "AUTO_DETECT_CACHE"
  )

  start_time <- Sys.time()

  # Use existing auto-detection logic (fra fct_autodetect_unified.R)
  results <- detect_columns_full_analysis(data, app_state)

  computation_time <- as.numeric(Sys.time() - start_time)

  # Cache results for 30 minutes (auto-detection is expensive, longer cache reduces duplicates)
  cache_result(cache_key, results, timeout_seconds = 1800)

  log_info(
    "Auto-detection completed and cached",
    .context = "AUTO_DETECT_CACHE"
  )
  log_debug_kv(
    cache_key = cache_key,
    computation_time = computation_time,
    results_count = length(results),
    .context = "AUTO_DETECT_CACHE"
  )

  return(results)
}

#' Cache Management Functions
#'

#' Get Cached Result
#'
#' Henter cached result hvis det eksisterer og ikke er expired.
#'
#' @param cache_key Character string med cache key
#'
#' @return Cached result eller NULL hvis ikke fundet/expired
#'
get_cached_result <- function(cache_key) {
  if (!exists(cache_key, envir = .performance_cache)) {
    return(NULL)
  }

  cached_entry <- get(cache_key, envir = .performance_cache)

  # Check if expired
  if (Sys.time() > cached_entry$expires_at) {
    rm(list = cache_key, envir = .performance_cache)
    log_debug_kv(
      message = "Cache entry expired and removed",
      cache_key = cache_key,
      .context = "[PERFORMANCE_CACHE]"
    )
    return(NULL)
  }

  # Update access time for LRU management
  cached_entry$last_access <- Sys.time()
  assign(cache_key, cached_entry, envir = .performance_cache)

  return(cached_entry)
}

#' Cache Result
#'
#' Gemmer result i cache med expiration time.
#'
#' @param cache_key Character string med cache key
#' @param value Value at cache
#' @param timeout_seconds Timeout i sekunder
#'
cache_result <- function(cache_key, value, timeout_seconds) {
  cached_entry <- list(
    value = value,
    created_at = Sys.time(),
    expires_at = Sys.time() + timeout_seconds,
    last_access = Sys.time(),
    size_estimate = object.size(value)
  )

  assign(cache_key, cached_entry, envir = .performance_cache)
}

#' Manage Cache Size
#'
#' Håndterer cache size ved at fjerne gamle entries (LRU eviction).
#'
#' @param max_entries Maximum antal entries i cache
#'
manage_cache_size <- function(max_entries) {
  cache_keys <- ls(envir = .performance_cache)

  if (length(cache_keys) <= max_entries) {
    return()
  }

  # Get all cache entries med access times
  cache_entries <- purrr::map(cache_keys, ~ {
    entry <- get(.x, envir = .performance_cache)
    list(
      key = .x,
      last_access = entry$last_access,
      size_estimate = entry$size_estimate
    )
  })

  # Sort by last access time (LRU)
  sorted_entries <- cache_entries |>
    purrr::map_dfr(~ tibble::tibble(
      key = .x$key,
      last_access = .x$last_access,
      size_estimate = as.numeric(.x$size_estimate)
    )) |>
    dplyr::arrange(last_access)

  # Remove oldest entries
  entries_to_remove <- nrow(sorted_entries) - max_entries
  if (entries_to_remove > 0) {
    keys_to_remove <- sorted_entries$key[1:entries_to_remove]

    rm(list = keys_to_remove, envir = .performance_cache)

    log_debug_kv(
      message = "Cache size managed - removed old entries",
      removed_count = entries_to_remove,
      remaining_entries = length(cache_keys) - entries_to_remove,
      .context = "[PERFORMANCE_CACHE]"
    )
  }
}

#' Clear Performance Cache
#'
#' Rydder hele performance cache. Bruges ved session cleanup
#' og efter store data changes.
#'
#' @param pattern Optional regex pattern til at rydde specific keys
#'
#' @examples
#' clear_performance_cache() # Clear alt
#' clear_performance_cache("autodetect_.*") # Clear kun autodetect cache
#'
#' @export
clear_performance_cache <- function(pattern = NULL) {
  cache_keys <- ls(envir = .performance_cache)

  if (!is.null(pattern)) {
    cache_keys <- cache_keys[grepl(pattern, cache_keys)]
  }

  if (length(cache_keys) > 0) {
    rm(list = cache_keys, envir = .performance_cache)

    log_debug_kv(
      message = "Performance cache cleared",
      pattern = pattern %||% "all",
      cleared_count = length(cache_keys),
      .context = "[PERFORMANCE_CACHE]"
    )
  }
}

#' Cache Performance Statistics
#'
#' Returnerer statistikker omkring cache performance til monitoring.
#'
#' @return List med cache statistikker
#'
#' @examples
#' stats <- get_cache_stats()
#' print(stats)
#'
#' @export
get_cache_stats <- function() {
  cache_keys <- ls(envir = .performance_cache)

  if (length(cache_keys) == 0) {
    return(list(
      total_entries = 0,
      total_size_mb = 0,
      oldest_entry = NULL,
      newest_entry = NULL
    ))
  }

  # Collect cache metadata
  cache_metadata <- purrr::map(cache_keys, ~ {
    entry <- get(.x, envir = .performance_cache)
    list(
      key = .x,
      created_at = entry$created_at,
      last_access = entry$last_access,
      size_estimate = as.numeric(entry$size_estimate)
    )
  })

  total_size <- sum(purrr::map_dbl(cache_metadata, ~ .x$size_estimate))
  access_times <- purrr::map(cache_metadata, ~ .x$last_access)

  return(list(
    total_entries = length(cache_keys),
    total_size_mb = round(total_size / (1024 * 1024), 2),
    oldest_entry = min(purrr::map(access_times, as.POSIXct)),
    newest_entry = max(purrr::map(access_times, as.POSIXct)),
    keys = cache_keys
  ))
}

#' Create Performance-Debounced Reactive
#'
#' Kombinerer caching med debouncing for optimal performance på
#' hyppigt-opdaterede reactive expressions.
#'
#' @param reactive_expr Reactive expression
#' @param cache_key Cache key (string eller function)
#' @param debounce_millis Debounce delay i millisekunder
#' @param cache_timeout Cache timeout i sekunder (default: CACHE_CONFIG$default_timeout_seconds)
#'
#' @return Debounced og cached reactive expression
#'
#' @examples
#' optimized_reactive <- create_performance_debounced(
#'   reactive({
#'     expensive_computation(input$data)
#'   }),
#'   "computation",
#'   millis = 500,
#'   cache_timeout = CACHE_CONFIG$default_timeout_seconds
#' )
#'
#' @export
create_performance_debounced <- function(reactive_expr, cache_key, debounce_millis = 500, cache_timeout = CACHE_CONFIG$default_timeout_seconds) {
  # First apply caching
  cached_reactive <- create_cached_reactive(reactive_expr, cache_key, cache_timeout)

  # Then apply debouncing
  debounced_reactive <- shiny::debounce(cached_reactive, debounce_millis)

  return(debounced_reactive)
}
