# utils_cache_generators.R
# Generator functions for startup cache artifacts

#' Get hospital branding configuration for caching
#'
#' Extract all hospital branding data that can be cached for faster startup
#'
#' @return List with hospital branding configuration
#' @export
get_hospital_branding_config <- function() {
  safe_operation(
    operation_name = "Generate hospital branding cache",
    code = {
      branding_config <- list()

      # Hospital colors
      if (exists("get_hospital_colors", mode = "function")) {
        branding_config$colors <- get_hospital_colors()
      } else {
        branding_config$colors <- list(
          primary = "#375a7f",
          secondary = "#6c757d",
          success = "#28a745",
          warning = "#ffc107",
          danger = "#dc3545",
          accent = "#FF6B35"
        )
      }

      # Hospital name and metadata
      if (exists("get_hospital_name", mode = "function")) {
        branding_config$name <- get_hospital_name()
      } else {
        branding_config$name <- "SPCify SPC"
      }

      # Cache timestamp
      branding_config$generated_at <- Sys.time()
      branding_config$version <- "1.0.0"

      log_debug("Generated hospital branding cache data", .context = "CACHE_GENERATOR")
      return(branding_config)
    },
    fallback = function(e) {
      log_warn(paste("Failed to generate hospital branding cache:", e$message), "CACHE_GENERATOR")
      return(list(
        colors = list(primary = "#375a7f", secondary = "#6c757d"),
        name = "SPCify SPC",
        generated_at = Sys.time(),
        version = "1.0.0"
      ))
    }
  )
}

#' Get observer priorities configuration for caching
#'
#' Extract observer priorities that can be cached
#'
#' @return List with observer priorities
#' @export
get_observer_priorities_config <- function() {
  safe_operation(
    operation_name = "Generate observer priorities cache",
    code = {
      priorities <- list()

      # Get observer priorities if they exist
      if (exists("OBSERVER_PRIORITIES", envir = .GlobalEnv)) {
        priorities <- get("OBSERVER_PRIORITIES", envir = .GlobalEnv)
      } else {
        # Default priorities
        priorities <- list(
          STATE_MANAGEMENT = 1000L,
          HIGH = 900L,
          DATA_PROCESSING = 800L,
          UI_UPDATES = 700L,
          MEDIUM = 500L,
          LOW = 300L,
          CLEANUP = 100L
        )
      }

      priorities$generated_at <- Sys.time()
      priorities$version <- "1.0.0"

      log_debug("Generated observer priorities cache data", .context = "CACHE_GENERATOR")
      return(priorities)
    },
    fallback = function(e) {
      log_warn(paste("Failed to generate observer priorities cache:", e$message), "CACHE_GENERATOR")
      return(list(
        HIGH = 900L,
        MEDIUM = 500L,
        LOW = 300L,
        generated_at = Sys.time(),
        version = "1.0.0"
      ))
    }
  )
}

#' Get chart types configuration for caching
#'
#' Extract chart types configuration that can be cached
#'
#' @return List with chart types configuration
#' @export
get_chart_types_config <- function() {
  safe_operation(
    operation_name = "Generate chart types cache",
    code = {
      chart_types <- list()

      # Standard SPC chart types
      chart_types$spc_types <- list(
        "I chart" = list(
          description = "Individual measurements chart",
          requires = c("y_column"),
          optional = c("x_column")
        ),
        "MR chart" = list(
          description = "Moving range chart",
          requires = c("y_column"),
          optional = c("x_column")
        ),
        "Xbar chart" = list(
          description = "Average chart",
          requires = c("y_column", "n_column"),
          optional = c("x_column")
        ),
        "S chart" = list(
          description = "Standard deviation chart",
          requires = c("y_column", "n_column"),
          optional = c("x_column")
        ),
        "P chart" = list(
          description = "Proportion chart",
          requires = c("y_column", "n_column"),
          optional = c("x_column")
        ),
        "C chart" = list(
          description = "Count chart",
          requires = c("y_column"),
          optional = c("x_column")
        ),
        "U chart" = list(
          description = "Rate chart",
          requires = c("y_column", "n_column"),
          optional = c("x_column")
        )
      )

      chart_types$generated_at <- Sys.time()
      chart_types$version <- "1.0.0"

      log_debug("Generated chart types cache data", .context = "CACHE_GENERATOR")
      return(chart_types)
    },
    fallback = function(e) {
      log_warn(paste("Failed to generate chart types cache:", e$message), "CACHE_GENERATOR")
      return(list(
        spc_types = list("I chart" = list(description = "Individual measurements chart")),
        generated_at = Sys.time(),
        version = "1.0.0"
      ))
    }
  )
}

#' Get system configuration snapshot for caching
#' Create a snapshot of system configuration that can be cached
#'
#' @return List with system configuration
#' @export
get_system_config_snapshot <- function() {
  safe_operation(
    operation_name = "Generate system config cache",
    code = {
      config <- list()

      # Environment detection
      config$environment <- detect_golem_environment()

      # Log level
      config$log_level <- Sys.getenv("SPC_LOG_LEVEL", "INFO")

      # Key environment variables
      config$env_vars <- list(
        golem_config_active = Sys.getenv("GOLEM_CONFIG_ACTIVE", ""),
        test_mode_auto_load = Sys.getenv("TEST_MODE_AUTO_LOAD", "FALSE"),
        spc_debug_mode = Sys.getenv("SPC_DEBUG_MODE", "FALSE"),
        spc_source_loading = Sys.getenv("SPC_SOURCE_LOADING", "FALSE")
      )

      # Runtime flags
      config$runtime_flags <- list(
        interactive_session = interactive(),
        package_mode = !"SPC_SOURCE_LOADING" %in% names(Sys.getenv()) ||
                      Sys.getenv("SPC_SOURCE_LOADING", "FALSE") == "FALSE"
      )

      config$generated_at <- Sys.time()
      config$version <- "1.0.0"

      log_debug("Generated system config cache data", .context = "CACHE_GENERATOR")
      return(config)
    },
    fallback = function(e) {
      log_warn(paste("Failed to generate system config cache:", e$message), "CACHE_GENERATOR")
      return(list(
        environment = "production",
        log_level = "INFO",
        generated_at = Sys.time(),
        version = "1.0.0"
      ))
    }
  )
}