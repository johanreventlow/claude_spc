# utils_startup_cache.R
# Startup cache system for static artifacts to improve boot performance

#' Startup cache configuration
#' @description
#' Configuration for caching static artifacts during startup to reduce
#' repeated computations and file reads on subsequent application starts.
#'
#' @details
#' Cached artifacts:
#' - Hospital branding configuration (colors, logos, text)
#' - Observer priorities configuration
#' - Chart types configuration
#' - System configuration snapshot
#' - SPC configuration defaults
#'
#' Cache location: Uses temporary directory to avoid persistence issues
#' Cache TTL: 1 hour (configurable)
STARTUP_CACHE_CONFIG <- list(
  cache_dir = file.path(tempdir(), "spc_startup_cache"),
  cache_ttl_seconds = 3600, # 1 hour
  max_cache_size_mb = 10,   # Maximum cache size

  # Artifacts to cache
  artifacts = list(
    hospital_branding = list(
      file = "hospital_branding.rds",
      generator = function() get_hospital_branding_config(),
      ttl_seconds = 7200 # Branding changes rarely, cache longer
    ),
    observer_priorities = list(
      file = "observer_priorities.rds",
      generator = function() get_observer_priorities_config(),
      ttl_seconds = 3600
    ),
    chart_types = list(
      file = "chart_types.rds",
      generator = function() get_chart_types_config(),
      ttl_seconds = 3600
    ),
    system_config = list(
      file = "system_config.rds",
      generator = function() get_system_config_snapshot(),
      ttl_seconds = 1800 # Config changes more frequently
    )
  )
)

#' Initialize startup cache directory
#' @description
#' Create cache directory if it doesn't exist and clean up old cache files
#'
#' @return TRUE if successful, FALSE otherwise
#' @export
init_startup_cache <- function() {
  cache_dir <- STARTUP_CACHE_CONFIG$cache_dir

  safe_operation(
    operation_name = "Initialize startup cache",
    code = {
      # Create cache directory
      if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE)
        log_debug(paste("Created cache directory:", cache_dir), "STARTUP_CACHE")
      }

      # Clean up old cache files
      cleanup_old_cache()

      log_info(paste("Startup cache initialized at:", cache_dir), "STARTUP_CACHE")
      return(TRUE)
    },
    fallback = function(e) {
      log_warn(paste("Failed to initialize startup cache:", e$message), "STARTUP_CACHE")
      return(FALSE)
    }
  )
}

#' Clean up old cache files
#' @description
#' Remove cache files that are older than their TTL or if cache size exceeds limit
#'
#' @export
cleanup_old_cache <- function() {
  cache_dir <- STARTUP_CACHE_CONFIG$cache_dir

  if (!dir.exists(cache_dir)) {
    return()
  }

  safe_operation(
    operation_name = "Cache cleanup",
    code = {
      cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)

      removed_count <- 0
      total_size_mb <- 0

      for (file_path in cache_files) {
        file_info <- file.info(file_path)
        file_age_seconds <- as.numeric(Sys.time() - file_info$mtime)
        file_size_mb <- file_info$size / (1024 * 1024)

        total_size_mb <- total_size_mb + file_size_mb

        # Check if file is too old
        artifact_name <- tools::file_path_sans_ext(basename(file_path))
        ttl <- STARTUP_CACHE_CONFIG$artifacts[[artifact_name]]$ttl_seconds %||%
               STARTUP_CACHE_CONFIG$cache_ttl_seconds

        if (file_age_seconds > ttl) {
          unlink(file_path)
          removed_count <- removed_count + 1
          log_debug(paste("Removed expired cache file:", basename(file_path)), "STARTUP_CACHE")
        }
      }

      # Check total cache size
      if (total_size_mb > STARTUP_CACHE_CONFIG$max_cache_size_mb) {
        # Remove oldest files first
        remaining_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
        if (length(remaining_files) > 0) {
          file_times <- file.info(remaining_files)$mtime
          oldest_files <- remaining_files[order(file_times)]

          for (old_file in oldest_files) {
            unlink(old_file)
            removed_count <- removed_count + 1
            log_debug(paste("Removed cache file due to size limit:", basename(old_file)), "STARTUP_CACHE")

            # Recalculate size
            remaining_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
            if (length(remaining_files) == 0) break

            total_size_mb <- sum(file.info(remaining_files)$size) / (1024 * 1024)
            if (total_size_mb <= STARTUP_CACHE_CONFIG$max_cache_size_mb) break
          }
        }
      }

      if (removed_count > 0) {
        log_info(paste("Cache cleanup complete:", removed_count, "files removed"), "STARTUP_CACHE")
      }
    },
    fallback = function(e) {
      log_warn(paste("Cache cleanup failed:", e$message), "STARTUP_CACHE")
    }
  )
}

#' Cache startup data
#' @description
#' Cache all configured static artifacts for faster subsequent startups
#'
#' @return List of successfully cached artifacts
#' @export
cache_startup_data <- function() {
  if (!init_startup_cache()) {
    log_warn("Cannot cache startup data - cache initialization failed", "STARTUP_CACHE")
    return(character(0))
  }

  cached_artifacts <- character(0)
  cache_dir <- STARTUP_CACHE_CONFIG$cache_dir

  log_info("Starting startup data caching", "STARTUP_CACHE")

  for (artifact_name in names(STARTUP_CACHE_CONFIG$artifacts)) {
    artifact_config <- STARTUP_CACHE_CONFIG$artifacts[[artifact_name]]
    cache_file <- file.path(cache_dir, artifact_config$file)

    should_cache <- TRUE

    # Check if cache file exists and is still valid
    if (file.exists(cache_file)) {
      file_info <- file.info(cache_file)
      file_age_seconds <- as.numeric(Sys.time() - file_info$mtime)
      ttl <- artifact_config$ttl_seconds %||% STARTUP_CACHE_CONFIG$cache_ttl_seconds

      if (file_age_seconds < ttl) {
        should_cache <- FALSE
        log_debug(paste("Cache file still valid:", artifact_name), "STARTUP_CACHE")
      }
    }

    if (should_cache) {
      cached <- safe_operation(
        operation_name = paste("Cache artifact:", artifact_name),
        code = {
          if (exists(deparse(substitute(artifact_config$generator)), mode = "function")) {
            data <- artifact_config$generator()
            saveRDS(data, cache_file)
            log_debug(paste("Cached artifact:", artifact_name, "to", basename(cache_file)), "STARTUP_CACHE")
            return(TRUE)
          } else {
            log_debug(paste("Generator function not available for:", artifact_name), "STARTUP_CACHE")
            return(FALSE)
          }
        },
        fallback = function(e) {
          log_warn(paste("Failed to cache", artifact_name, ":", e$message), "STARTUP_CACHE")
          return(FALSE)
        }
      )

      if (cached) {
        cached_artifacts <- c(cached_artifacts, artifact_name)
      }
    }
  }

  if (length(cached_artifacts) > 0) {
    log_info(paste("Cached artifacts:", paste(cached_artifacts, collapse = ", ")), "STARTUP_CACHE")
  } else {
    log_info("No new artifacts cached (all up-to-date)", "STARTUP_CACHE")
  }

  return(cached_artifacts)
}

#' Load cached startup data
#' @description
#' Load cached static artifacts to speed up startup
#'
#' @return List of successfully loaded artifacts with their data
#' @export
load_cached_startup_data <- function() {
  cache_dir <- STARTUP_CACHE_CONFIG$cache_dir

  if (!dir.exists(cache_dir)) {
    log_debug("No startup cache directory found", "STARTUP_CACHE")
    return(list())
  }

  loaded_data <- list()

  for (artifact_name in names(STARTUP_CACHE_CONFIG$artifacts)) {
    artifact_config <- STARTUP_CACHE_CONFIG$artifacts[[artifact_name]]
    cache_file <- file.path(cache_dir, artifact_config$file)

    if (file.exists(cache_file)) {
      # Check if cache is still valid
      file_info <- file.info(cache_file)
      file_age_seconds <- as.numeric(Sys.time() - file_info$mtime)
      ttl <- artifact_config$ttl_seconds %||% STARTUP_CACHE_CONFIG$cache_ttl_seconds

      if (file_age_seconds < ttl) {
        data <- safe_operation(
          operation_name = paste("Load cached artifact:", artifact_name),
          code = {
            cached_data <- readRDS(cache_file)
            log_debug(paste("Loaded cached artifact:", artifact_name), "STARTUP_CACHE")
            return(cached_data)
          },
          fallback = function(e) {
            log_warn(paste("Failed to load cached", artifact_name, ":", e$message), "STARTUP_CACHE")
            return(NULL)
          }
        )

        if (!is.null(data)) {
          loaded_data[[artifact_name]] <- data
        }
      } else {
        log_debug(paste("Cache expired for:", artifact_name), "STARTUP_CACHE")
        # Remove expired cache file
        unlink(cache_file)
      }
    }
  }

  if (length(loaded_data) > 0) {
    log_info(paste("Loaded cached artifacts:", paste(names(loaded_data), collapse = ", ")), "STARTUP_CACHE")
  }

  return(loaded_data)
}

#' Get startup cache status
#' @description
#' Get information about the current startup cache state
#'
#' @return List with cache status information
#' @export
get_startup_cache_status <- function() {
  cache_dir <- STARTUP_CACHE_CONFIG$cache_dir

  status <- list(
    cache_enabled = dir.exists(cache_dir),
    cache_dir = cache_dir,
    artifacts = list()
  )

  if (status$cache_enabled) {
    cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
    total_size_bytes <- 0

    for (artifact_name in names(STARTUP_CACHE_CONFIG$artifacts)) {
      artifact_config <- STARTUP_CACHE_CONFIG$artifacts[[artifact_name]]
      cache_file <- file.path(cache_dir, artifact_config$file)

      if (file.exists(cache_file)) {
        file_info <- file.info(cache_file)
        file_age_seconds <- as.numeric(Sys.time() - file_info$mtime)
        ttl <- artifact_config$ttl_seconds %||% STARTUP_CACHE_CONFIG$cache_ttl_seconds

        status$artifacts[[artifact_name]] <- list(
          cached = TRUE,
          file_size_bytes = file_info$size,
          age_seconds = file_age_seconds,
          ttl_seconds = ttl,
          valid = file_age_seconds < ttl,
          last_modified = file_info$mtime
        )

        total_size_bytes <- total_size_bytes + file_info$size
      } else {
        status$artifacts[[artifact_name]] <- list(
          cached = FALSE,
          file_size_bytes = 0,
          age_seconds = NA,
          ttl_seconds = artifact_config$ttl_seconds %||% STARTUP_CACHE_CONFIG$cache_ttl_seconds,
          valid = FALSE,
          last_modified = NA
        )
      }
    }

    status$total_size_mb <- total_size_bytes / (1024 * 1024)
    status$total_files <- length(cache_files)
  }

  return(status)
}