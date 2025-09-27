# run_app.R
# Main app launcher following Golem conventions

#' Configure logging level from YAML configuration
#'
#' @description
#' Sets SPC_LOG_LEVEL from golem-config.yml as single source of truth.
#' Respects explicit log_level parameter override.
#'
#' @param log_level Optional explicit log level override
#' @noRd
configure_logging_from_yaml <- function(log_level = NULL) {
  # If explicit log_level provided, validate and use it
  if (!is.null(log_level)) {
    valid_levels <- c("DEBUG", "INFO", "WARN", "ERROR")
    log_level <- toupper(trimws(log_level))

    if (log_level %in% valid_levels) {
      Sys.setenv(SPC_LOG_LEVEL = log_level)
      message(sprintf("[LOG_CONFIG] Explicit log level set to %s", log_level))
      return()
    } else {
      message(sprintf("[LOG_CONFIG] Invalid log level '%s'. Valid: %s",
                     log_level, paste(valid_levels, collapse = ", ")))
      # Continue to YAML-based config
    }
  }

  # Get current environment config
  current_config <- Sys.getenv("GOLEM_CONFIG_ACTIVE", "default")

  # Read logging level from YAML
  tryCatch({
    if (exists("get_golem_config", mode = "function")) {
      yaml_log_level <- get_golem_config("logging")$level

      if (!is.null(yaml_log_level) && yaml_log_level != "") {
        Sys.setenv(SPC_LOG_LEVEL = yaml_log_level)
        message(sprintf("[LOG_CONFIG] Log level '%s' from YAML config '%s'",
                       yaml_log_level, current_config))
        return()
      }
    }
  }, error = function(e) {
    message(sprintf("[LOG_CONFIG] Could not read YAML config: %s", e$message))
  })

  # Fallback to environment-based defaults
  default_level <- if (current_config %in% c("development", "testing")) "DEBUG" else "WARN"

  if (Sys.getenv("SPC_LOG_LEVEL", "") == "") {
    Sys.setenv(SPC_LOG_LEVEL = default_level)
    message(sprintf("[LOG_CONFIG] Fallback log level '%s' for config '%s'",
                   default_level, current_config))
  }
}

#' Configure application environment and test mode
#'
#' @description
#' Centralized configuration of application environment and test mode settings.
#' Uses golem config as single source of truth instead of scattered environment variable setting.
#'
#' @param enable_test_mode Boolean or NULL for auto-detection
#' @param golem_options List of additional golem options
#' @noRd
configure_app_environment <- function(enable_test_mode = NULL, golem_options = list()) {
  # Auto-detect test mode if not specified
  if (is.null(enable_test_mode)) {
    current_config <- Sys.getenv("GOLEM_CONFIG_ACTIVE", "production")
    enable_test_mode <- interactive() || current_config == "development"
  }

  enable_test_mode <- isTRUE(enable_test_mode)

  # Set appropriate golem configuration profile
  config_profile <- if (enable_test_mode) "development" else "production"

  # Set GOLEM_CONFIG_ACTIVE as single source of configuration
  Sys.setenv(GOLEM_CONFIG_ACTIVE = config_profile)

  # Store in options for app-level access
  options(spc.test_mode = enable_test_mode)

  # Configure package environment if available
  if (exists("get_claudespc_environment", mode = "function")) {
    claudespc_env <- get_claudespc_environment()
    claudespc_env$TEST_MODE_AUTO_LOAD <- enable_test_mode
    claudespc_env$TEST_MODE_FILE_PATH <- if (enable_test_mode) {
      "inst/extdata/spc_exampledata.csv"
    } else {
      NULL
    }
  }

  # Apply configuration via golem config instead of direct env vars
  if (enable_test_mode) {
    # Use golem config values for test mode
    tryCatch({
      if (exists("golem::get_golem_config", mode = "function")) {
        test_config <- golem::get_golem_config("testing", config = config_profile)
        if (!is.null(test_config$auto_load_test_data)) {
          Sys.setenv(TEST_MODE_AUTO_LOAD = ifelse(test_config$auto_load_test_data, "TRUE", "FALSE"))
        }
        if (!is.null(test_config$test_data_file)) {
          Sys.setenv(TEST_MODE_FILE_PATH = test_config$test_data_file)
        }
      } else {
        # Fallback to direct settings if golem config not available
        Sys.setenv(TEST_MODE_AUTO_LOAD = "TRUE")
        Sys.setenv(TEST_MODE_FILE_PATH = "inst/extdata/spc_exampledata.csv")
      }
    }, error = function(e) {
      # Fallback configuration
      Sys.setenv(TEST_MODE_AUTO_LOAD = "TRUE")
      Sys.setenv(TEST_MODE_FILE_PATH = "inst/extdata/spc_exampledata.csv")
    })
  } else {
    Sys.setenv(TEST_MODE_AUTO_LOAD = "FALSE")
    Sys.unsetenv("TEST_MODE_FILE_PATH")
  }

  log_debug_kv(
    config_profile = config_profile,
    test_mode = enable_test_mode,
    .context = "APP_CONFIG"
  )

  invisible(list(
    config_profile = config_profile,
    test_mode = enable_test_mode
  ))
}

#' Initialize startup performance optimizations
#'
#' @description
#' Initialize startup cache and lazy loading systems at run_app() level.
#' This ensures performance optimizations work in production regardless of loading mode.
#'
#' @noRd
initialize_startup_performance_optimizations <- function() {
  safe_operation(
    operation_name = "Initialize startup performance optimizations",
    code = {
      # Initialize startup cache system if available
      if (exists("load_cached_startup_data", mode = "function") &&
          exists("cache_startup_data", mode = "function")) {

        # Load cached static data if available
        cached_data <- load_cached_startup_data()
        if (length(cached_data) > 0) {
          log_debug_kv(
            key = "startup_cache_loaded",
            value = paste(names(cached_data), collapse = ", "),
            .context = "STARTUP_OPTIMIZATION"
          )

          # Apply cached data to global variables for backward compatibility
          if ("hospital_branding" %in% names(cached_data) && exists("get_claudespc_environment", mode = "function")) {
            claudespc_env <- get_claudespc_environment()
            claudespc_env$HOSPITAL_COLORS <- cached_data$hospital_branding$colors
            claudespc_env$HOSPITAL_NAME <- cached_data$hospital_branding$name
          }
          if ("observer_priorities" %in% names(cached_data)) {
            if (exists("get_claudespc_environment", mode = "function")) {
              claudespc_env <- get_claudespc_environment()
              claudespc_env$OBSERVER_PRIORITIES <- cached_data$observer_priorities
            }
          }
        }

        # Cache current data for next startup (async, non-blocking)
        cache_startup_data()
      }

      # Initialize lazy loading system if available
      if (exists("lazy_load_modules", mode = "function")) {
        loaded_modules <- lazy_load_modules()
        if (length(loaded_modules) > 0) {
          log_debug_kv(
            key = "lazy_loaded_modules",
            value = paste(loaded_modules, collapse = ", "),
            .context = "STARTUP_OPTIMIZATION"
          )
        }
      }

      return(TRUE)
    },
    fallback = function(e) {
      # Non-critical failure - log but don't block app startup
      log_debug_kv(
        key = "startup_optimization_failed",
        value = e$message,
        .context = "STARTUP_OPTIMIZATION"
      )
      return(FALSE)
    }
  )

  invisible()
}

#' Run the SPC Shiny Application
#'
#' @param port Port number for the application (default: NULL for auto-assignment)
#' @param launch_browser TRUE for browser, FALSE for no launch, or leave default for environment-aware decision
#' @param options List of application options (golem-style configuration)
#' @param enable_test_mode TRUE/FALSE to force test mode, or NULL for auto-detection based on environment
#' @param log_level Set logging level for the session: "DEBUG", "INFO", "WARN", "ERROR", or NULL for auto-detection
#' @param ... Additional arguments to pass to shiny::shinyApp()
#'
#' @details
#' This function implements golem-inspired patterns for robust Shiny app deployment.
#' It supports environment-aware configuration and graceful fallbacks for different
#' deployment contexts (development, production, testing).
#'
#' Log levels determine what messages are displayed:
#' \describe{
#'   \item{DEBUG}{All messages (most verbose) - ideal for troubleshooting reactive flows}
#'   \item{INFO}{Informational messages and above - good for monitoring app behavior}
#'   \item{WARN}{Warnings and errors only - standard production setting}
#'   \item{ERROR}{Only error messages - minimal logging}
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' run_app()
#'
#' # Development with debug logging
#' run_app(port = 4040, log_level = "DEBUG")
#'
#' # Troubleshooting with detailed logging
#' run_app(log_level = "DEBUG", enable_test_mode = TRUE)
#'
#' # Production with minimal logging
#' run_app(log_level = "WARN", launch_browser = TRUE)
#' }
#'
#' @import shiny
#' @import golem
#' @importFrom golem with_golem_options
#' @export
run_app <- function(port = NULL,
                    launch_browser = NULL,
                    options = list(),
                    enable_test_mode = NULL,
                    log_level = NULL,
                    ...) {

  # Initialize startup performance optimizations early
  initialize_startup_performance_optimizations()

  # Configure logging level using YAML as single source of truth
  configure_logging_from_yaml(log_level)

  # Configure application environment and test mode using unified configuration
  configure_app_environment(enable_test_mode, options)

  # Create the Shiny app using golem pattern
  # Load golem function directly to avoid package conflicts
  if (!exists("with_golem_options")) {
    with_golem_options <- golem::with_golem_options
  }

  app <- with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui,
      server = app_server
    ),
    golem_opts = options
  )

  # Set default browser behavior if not specified
  if (is.null(launch_browser)) {
    launch_browser <- getOption("shiny.launch.browser", TRUE)
  }

  # Run the app with proper port and browser handling
  if (is.null(port)) {
    shiny::runApp(
      app,
      launch.browser = launch_browser,
      ...
    )
  } else {
    shiny::runApp(
      app,
      port = port,
      launch.browser = launch_browser,
      ...
    )
  }
}

