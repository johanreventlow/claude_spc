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
#' @param max_size Maximum number of cache entries (default: 50)
#'
#' @return List with cache operations (get, set, clear, size, stats)
#'
#' @details
#' Creates an environment-based cache for QIC calculation results.
#' Each cache entry includes:
#' - value: The cached QIC result
#' - created_at: Timestamp when cached
#' - expires_at: Expiration timestamp
#' - access_count: Number of times accessed
#' - last_accessed: Last access timestamp
#'
#' Cache features:
#' - Automatic expiration after timeout
#' - LRU eviction when max_size reached
#' - Hit/miss metrics collection
#'
#' @examples
#' \dontrun{
#' # Create cache instance
#' qic_cache <- create_qic_cache(max_size = 100)
#'
#' # Check cache size
#' qic_cache$size()
#'
#' # Get cache statistics
#' qic_cache$stats()
#'
#' # Clear all entries
#' qic_cache$clear()
#' }
#'
#' @export
create_qic_cache <- function(max_size = 50) {
  cache <- new.env(parent = emptyenv())
  metrics <- list(
    hits = 0,
    misses = 0,
    evictions = 0
  )

  list(
    get = function(key) {
      if (exists(key, envir = cache)) {
        entry <- get(key, envir = cache)
        if (Sys.time() < entry$expires_at) {
          # Update access metrics
          entry$access_count <- entry$access_count + 1
          entry$last_accessed <- Sys.time()
          assign(key, entry, envir = cache)

          metrics$hits <<- metrics$hits + 1
          return(entry$value)
        } else {
          # Entry expired - remove it
          rm(list = key, envir = cache)
        }
      }
      metrics$misses <<- metrics$misses + 1
      return(NULL)
    },
    set = function(key, value, timeout = CACHE_CONFIG$default_timeout_seconds) {
      # Check if cache is full
      current_size <- length(ls(envir = cache))
      if (current_size >= max_size && !exists(key, envir = cache)) {
        # Evict least recently used entry
        cache_keys <- ls(envir = cache)
        if (length(cache_keys) > 0) {
          # Get last_accessed times for all entries
          access_times <- sapply(cache_keys, function(k) {
            entry <- get(k, envir = cache)
            entry$last_accessed
          })

          # Find least recently used
          lru_key <- cache_keys[which.min(access_times)]
          rm(list = lru_key, envir = cache)
          metrics$evictions <<- metrics$evictions + 1

          log_debug_kv(
            message = "QIC cache eviction (LRU)",
            evicted_key = substr(lru_key, 1, 20),
            cache_size = current_size,
            .context = "QIC_CACHE"
          )
        }
      }

      entry <- list(
        value = value,
        created_at = Sys.time(),
        expires_at = Sys.time() + timeout,
        access_count = 0,
        last_accessed = Sys.time()
      )
      assign(key, entry, envir = cache)
    },
    clear = function() {
      rm(list = ls(envir = cache), envir = cache)
      metrics$hits <<- 0
      metrics$misses <<- 0
      metrics$evictions <<- 0
    },
    size = function() {
      length(ls(envir = cache))
    },
    stats = function() {
      total_requests <- metrics$hits + metrics$misses
      hit_rate <- if (total_requests > 0) {
        round(metrics$hits / total_requests * 100, 2)
      } else {
        0
      }

      list(
        size = length(ls(envir = cache)),
        max_size = max_size,
        hits = metrics$hits,
        misses = metrics$misses,
        evictions = metrics$evictions,
        hit_rate_percent = hit_rate,
        total_requests = total_requests
      )
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

# NOTE: cached_qic() function removed in Sprint 5 Fase 2
# Caching is now handled via log_qic_call_wrapper() in utils_qic_debug_logging.R
# This provides better integration with the existing call chain and debug logging

#' Get QIC Cache Statistics
#'
#' Returns comprehensive statistics about cache usage.
#'
#' @param cache QIC cache object
#'
#' @return List with cache statistics:
#'   - size: Current number of cached entries
#'   - max_size: Maximum cache capacity
#'   - hits: Number of cache hits
#'   - misses: Number of cache misses
#'   - evictions: Number of LRU evictions
#'   - hit_rate_percent: Cache hit rate percentage
#'   - total_requests: Total cache requests
#'
#' @details
#' Useful for monitoring cache effectiveness and performance.
#' High hit rate (>70%) indicates effective caching.
#' Frequent evictions may suggest increasing max_size.
#'
#' @examples
#' \dontrun{
#' stats <- get_qic_cache_stats(qic_cache)
#' cat("Cache size:", stats$size, "/", stats$max_size, "\n")
#' cat("Hit rate:", stats$hit_rate_percent, "%\n")
#' cat("Evictions:", stats$evictions, "\n")
#' }
#'
#' @export
get_qic_cache_stats <- function(cache) {
  cache$stats()
}
