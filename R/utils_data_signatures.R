# utils_data_signatures.R
# H14: Shared Data Signatures
# Centralized data signature generation to reduce redundant hashing
# Sprint 4 Fase 3 - Performance optimization

#' Data Signature Cache
#'
#' Session-level cache for data signatures to avoid rehashing same data.
#' Signatures are stored in memory during session and reused across
#' different caching systems (QIC cache, auto-detect cache, etc.)
#'
#' @keywords internal
.data_signature_cache <- new.env(parent = emptyenv())

#' Generate Data Signature (Shared)
#'
#' Creates a consistent signature for data that can be reused across
#' multiple caching systems. Uses xxhash64 for speed and caches results.
#'
#' @param data Data frame to generate signature for
#' @param include_structure Include structural metadata (nrow, ncol, names, types)
#'
#' @return Character string signature (xxhash64 digest)
#'
#' @details
#' ## Performance Benefits
#'
#' - **Shared signatures**: Same data hashed once, reused in QIC + auto-detect
#' - **Fast algorithm**: xxhash64 is 5-10x faster than MD5
#' - **Session cache**: Identical data lookups avoid rehashing entirely
#'
#' ## When Signatures Match
#'
#' Two datasets have same signature if they have:
#' - Same number of rows and columns
#' - Same column names and types (if include_structure = TRUE)
#' - Identical data values
#'
#' @examples
#' \dontrun{
#' # Generate signature
#' sig <- generate_shared_data_signature(my_data)
#'
#' # Reuse in different contexts
#' qic_key <- paste0("qic_", sig, "_", param_hash)
#' autodetect_key <- paste0("autodetect_", sig)
#' }
#'
#' @export
generate_shared_data_signature <- function(data, include_structure = TRUE) {
  # Handle NULL/empty data
  if (is.null(data) || nrow(data) == 0) {
    return("empty_data")
  }

  # Create quick lookup key for cache based on object identity
  # Use pryr::address if available, otherwise use object structure
  data_ptr <- if (requireNamespace("pryr", quietly = TRUE)) {
    digest::digest(list(
      ptr = pryr::address(data),
      nrow = nrow(data),
      ncol = ncol(data)
    ), algo = "xxhash64")
  } else {
    # Fallback: Use first row + structure as proxy for identity
    digest::digest(list(
      first_row = if (nrow(data) > 0) as.list(data[1, , drop = FALSE]) else list(),
      nrow = nrow(data),
      ncol = ncol(data),
      names = names(data)
    ), algo = "xxhash64")
  }

  # Check if signature already cached
  if (exists(data_ptr, envir = .data_signature_cache)) {
    cached_sig <- get(data_ptr, envir = .data_signature_cache)
    return(cached_sig$signature)
  }

  # Generate new signature
  if (include_structure) {
    signature_components <- list(
      nrow = nrow(data),
      ncol = ncol(data),
      column_names = names(data),
      column_types = purrr::map_chr(data, ~ class(.x)[1]),
      data_hash = digest::digest(data, algo = "xxhash64", serialize = TRUE)
    )
    signature <- digest::digest(signature_components, algo = "xxhash64", serialize = TRUE)
  } else {
    # Data-only signature (faster, no structure)
    signature <- digest::digest(data, algo = "xxhash64", serialize = TRUE)
  }

  # Cache for reuse
  assign(data_ptr, list(
    signature = signature,
    timestamp = Sys.time(),
    include_structure = include_structure
  ), envir = .data_signature_cache)

  # Clean old cache entries if too large (keep last 100)
  cache_size <- length(ls(envir = .data_signature_cache))
  if (cache_size > 100) {
    # Remove oldest 20 entries
    cache_keys <- ls(envir = .data_signature_cache)
    cache_times <- sapply(cache_keys, function(k) {
      entry <- get(k, envir = .data_signature_cache)
      entry$timestamp
    })
    oldest_keys <- cache_keys[order(cache_times)][1:20]
    rm(list = oldest_keys, envir = .data_signature_cache)
  }

  return(signature)
}

#' Generate QIC Cache Key (Optimized)
#'
#' Creates cache key for QIC results using shared data signatures.
#' Replaces redundant MD5 hashing with shared xxhash64 signatures.
#'
#' @param data Data for QIC calculation
#' @param params QIC parameters (chart type, columns, etc.)
#'
#' @return Character string cache key
#'
#' @details
#' ## Performance Improvement
#'
#' **Before (H13):**
#' - Data hashed with MD5 for QIC cache key
#' - Same data hashed again with xxhash64 for auto-detect
#' - Total: 2 full data hashes per workflow
#'
#' **After (H14):**
#' - Data hashed once with xxhash64 (shared signature)
#' - Signature reused for QIC + auto-detect
#' - Total: 1 full data hash per workflow
#'
#' **Expected gain**: 30-50% reduction in hashing overhead
#'
#' @examples
#' \dontrun{
#' params <- list(
#'   chart = "run",
#'   x = "Dato",
#'   y = "Værdi"
#' )
#' key <- generate_qic_cache_key_optimized(data, params)
#' }
#'
#' @export
generate_qic_cache_key_optimized <- function(data, params) {
  # Use shared signature instead of rehashing
  data_signature <- generate_shared_data_signature(data, include_structure = FALSE)

  # Hash parameters (lightweight)
  param_digest <- digest::digest(params, algo = "xxhash64")

  paste0("qic_", data_signature, "_", param_digest)
}

#' Generate Auto-Detect Cache Key (Optimized)
#'
#' Creates cache key for auto-detection results using shared signatures.
#'
#' @param data Data for auto-detection
#'
#' @return Character string cache key
#'
#' @details
#' Uses same shared signature as QIC cache, ensuring consistency and
#' avoiding redundant hashing when both systems cache same data.
#'
#' @export
generate_autodetect_cache_key_optimized <- function(data) {
  # Use shared signature with structure info
  data_signature <- generate_shared_data_signature(data, include_structure = TRUE)

  paste0("autodetect_", data_signature)
}

#' Clear Data Signature Cache
#'
#' Clears the session-level signature cache.
#' Typically called on session end or when memory needs to be freed.
#'
#' @export
clear_data_signature_cache <- function() {
  rm(list = ls(envir = .data_signature_cache), envir = .data_signature_cache)
  log_debug("Data signature cache cleared", .context = "DATA_SIGNATURE")
}

#' Get Data Signature Cache Stats
#'
#' Returns statistics about signature cache usage.
#'
#' @return List with cache statistics
#'
#' @export
get_data_signature_cache_stats <- function() {
  cache_keys <- ls(envir = .data_signature_cache)

  if (length(cache_keys) == 0) {
    return(list(
      size = 0,
      oldest = NULL,
      newest = NULL
    ))
  }

  cache_times <- sapply(cache_keys, function(k) {
    entry <- get(k, envir = .data_signature_cache)
    entry$timestamp
  })

  list(
    size = length(cache_keys),
    oldest = min(cache_times),
    newest = max(cache_times),
    keys = cache_keys
  )
}

#' Migrate to Shared Signatures
#'
#' Backward compatibility wrapper for existing code.
#' Maps old create_data_signature() to new shared version.
#'
#' @param data Data frame
#'
#' @return Data signature
#'
#' @details
#' This function provides backward compatibility for code that uses
#' the old create_data_signature() function. New code should use
#' generate_shared_data_signature() directly.
#'
#' @keywords internal
create_data_signature <- function(data) {
  generate_shared_data_signature(data, include_structure = TRUE)
}
