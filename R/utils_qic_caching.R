# utils_qic_caching.R
# QIC Result Caching
# Sprint 4 Fase 2 - Memoization for expensive QIC calculations

#' Get or Initialize QIC Cache
#'
#' Lazily initializes QIC cache if not already created.
#' This avoids dependency issues with CACHE_CONFIG during app_state initialization.
#'
#' @param app_state Application state object
#' @return QIC cache object
#'
#' @details
#' The QIC cache is created lazily to avoid circular dependencies.
#' CACHE_CONFIG must be loaded before first cache access.
#'
#' @examples
#' \dontrun{
#' qic_cache <- get_or_init_qic_cache(app_state)
#' }
#'
#' @export
get_or_init_qic_cache <- function(app_state) {
  if (is.null(app_state$cache$qic)) {
    app_state$cache$qic <- create_qic_cache()
  }
  return(app_state$cache$qic)
}

#' Create QIC Result Cache
#'
#' Memoization for expensive QIC calculations.
#' Cache key based on data content + parameters.
#'
#' @return List with cache operations (get, set, clear, size)
#'
#' @details
#' Creates an environment-based cache for QIC calculation results.
#' Each cache entry includes:
#' - value: The cached QIC result
#' - created_at: Timestamp when cached
#' - expires_at: Expiration timestamp
#'
#' Cache entries expire after timeout to prevent stale data.
#'
#' @examples
#' \dontrun{
#' # Create cache instance
#' qic_cache <- create_qic_cache()
#'
#' # Check cache size
#' qic_cache$size()
#'
#' # Clear all entries
#' qic_cache$clear()
#' }
#'
#' @export
create_qic_cache <- function() {
  cache <- new.env(parent = emptyenv())

  list(
    get = function(key) {
      if (exists(key, envir = cache)) {
        entry <- get(key, envir = cache)
        if (Sys.time() < entry$expires_at) {
          return(entry$value)
        } else {
          # Entry expired - remove it
          rm(list = key, envir = cache)
        }
      }
      return(NULL)
    },
    set = function(key, value, timeout = CACHE_CONFIG$default_timeout_seconds) {
      entry <- list(
        value = value,
        created_at = Sys.time(),
        expires_at = Sys.time() + timeout
      )
      assign(key, entry, envir = cache)
    },
    clear = function() {
      rm(list = ls(envir = cache), envir = cache)
    },
    size = function() {
      length(ls(envir = cache))
    }
  )
}

#' Generate QIC Cache Key
#'
#' Creates unique key based on data + parameters.
#'
#' @param data Data used for QIC
#' @param params QIC parameters (chart type, x, y, etc.)
#'
#' @return Character string cache key
#'
#' @details
#' Uses MD5 digests of data and parameters to create a unique key.
#' Same data + same parameters = same key.
#'
#' @examples
#' \dontrun{
#' # Generate cache key
#' params <- list(
#'   x = "Dato",
#'   y = "Værdi",
#'   chart = "run"
#' )
#' key <- generate_qic_cache_key(my_data, params)
#' }
#'
#' @export
generate_qic_cache_key <- function(data, params) {
  # Combine data digest + parameter digest
  data_digest <- digest::digest(data, algo = "md5")
  param_digest <- digest::digest(params, algo = "md5")

  paste0("qic_", data_digest, "_", param_digest)
}

#' Cached QIC Wrapper
#'
#' Wraps qic() calls with caching.
#'
#' @param data Data for QIC
#' @param x X variable
#' @param y Y variable
#' @param chart Chart type
#' @param ... Additional qic() parameters
#' @param .cache QIC cache object (created by create_qic_cache())
#' @param .cache_timeout Cache timeout in seconds (default: 300)
#'
#' @return QIC result (from cache or freshly computed)
#'
#' @details
#' Performance optimization for QIC calculations:
#' - First call: Computes and caches result
#' - Subsequent calls with same data+params: Returns cached result
#' - Cache invalidation: Automatic after timeout
#'
#' Logs cache hit/miss for monitoring.
#'
#' @examples
#' \dontrun{
#' # Create cache
#' qic_cache <- create_qic_cache()
#'
#' # Use cached_qic instead of qic()
#' result <- cached_qic(
#'   data = my_data,
#'   x = "Dato",
#'   y = "Værdi",
#'   chart = "run",
#'   .cache = qic_cache
#' )
#'
#' # Second call with same parameters hits cache
#' result2 <- cached_qic(
#'   data = my_data,
#'   x = "Dato",
#'   y = "Værdi",
#'   chart = "run",
#'   .cache = qic_cache
#' )
#' }
#'
#' @export
cached_qic <- function(data, x, y, chart, ...,
                       .cache = NULL,
                       .cache_timeout = CACHE_CONFIG$default_timeout_seconds) {
  # If no cache provided, call qic directly
  if (is.null(.cache)) {
    return(qicharts2::qic(
      data = data,
      x = x,
      y = y,
      chart = chart,
      ...
    ))
  }

  # Generate cache key
  params <- list(x = x, y = y, chart = chart, extra = list(...))
  cache_key <- generate_qic_cache_key(data, params)

  # Check cache
  cached_result <- .cache$get(cache_key)
  if (!is.null(cached_result)) {
    log_debug_kv(
      message = "QIC cache hit",
      cache_key = substr(cache_key, 1, 20),
      .context = "QIC_CACHE"
    )
    return(cached_result)
  }

  # Cache miss - compute
  log_debug_kv(
    message = "QIC cache miss - computing",
    cache_key = substr(cache_key, 1, 20),
    .context = "QIC_CACHE"
  )

  start_time <- Sys.time()
  result <- qicharts2::qic(
    data = data,
    x = x,
    y = y,
    chart = chart,
    ...
  )
  computation_time <- as.numeric(Sys.time() - start_time) * 1000

  # Cache result
  .cache$set(cache_key, result, timeout = .cache_timeout)

  log_debug_kv(
    message = "QIC result cached",
    cache_key = substr(cache_key, 1, 20),
    computation_time_ms = round(computation_time, 2),
    .context = "QIC_CACHE"
  )

  return(result)
}

#' Get QIC Cache Statistics
#'
#' Returns statistics about cache usage.
#'
#' @param cache QIC cache object
#'
#' @return List with cache statistics
#'
#' @details
#' Useful for monitoring cache effectiveness:
#' - size: Number of cached entries
#' - Can be extended with hit/miss counters
#'
#' @examples
#' \dontrun{
#' stats <- get_qic_cache_stats(qic_cache)
#' cat("Cache size:", stats$size, "\n")
#' }
#'
#' @export
get_qic_cache_stats <- function(cache) {
  list(
    size = cache$size()
  )
}
