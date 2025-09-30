# golem_utils.R
# Golem-inspired utility functions for robust Shiny app management
# Fase 3.4: Proper golem patterns implementation with YAML configuration support

#' Null-coalescing operator
#'
#' Returns the left-hand side if it's not NULL, otherwise returns the right-hand side
#'
#' @param lhs Left-hand side value
#' @param rhs Right-hand side value (fallback)
#' @return lhs if not NULL, otherwise rhs
#' @export
`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) lhs else rhs
}

#' Set Application Options (Golem-style)
#'
#' Centralized options management following golem patterns. Allows runtime
#' configuration of app behavior without modifying global state directly.
#'
#' @param options List of options to set
#' @return Invisibly returns the updated options
#' @export
#'
#' @examples
#' \dontrun{
#' # Set development options
#' set_app_options(list(
#'   test_mode = TRUE,
#'   debug_level = "DEBUG",
#'   auto_load_data = TRUE
#' ))
#'
#' # Set production options
#' set_app_options(list(
#'   production_mode = TRUE,
#'   debug_level = "ERROR",
#'   auto_restore = TRUE
#' ))
#' }
set_app_options <- function(options = list()) {
  if (length(options) == 0) {
    return(invisible(NULL))
  }


  # Default app options
  default_options <- list(
    production_mode = FALSE,
    test_mode = FALSE,
    debug_level = "INFO",
    auto_load_data = NULL,
    auto_restore = NULL,
    browser_launch = NULL,
    port = NULL
  )

  # Merge with provided options
  merged_options <- utils::modifyList(default_options, options)

  # Set options using R's standard options mechanism
  for (option_name in names(merged_options)) {
    option_key <- paste0("claudespc.", option_name)
    option_list <- list()
    option_list[[option_key]] <- merged_options[[option_name]]
    do.call("options", option_list)
  }

  return(invisible(merged_options))
}

#' Get Application Option (Golem-style)
#'
#' Retrieve application options with fallback to defaults.
#'
#' @param option_name Name of the option to retrieve
#' @param default Default value if option is not set
#' @return Option value or default
#' @export
#'
#' @examples
#' \dontrun{
#' # Get with default
#' debug_level <- get_app_option("debug_level", "INFO")
#'
#' # Check if in test mode
#' is_test_mode <- get_app_option("test_mode", FALSE)
#' }
get_app_option <- function(option_name, default = NULL) {
  option_key <- paste0("claudespc.", option_name)
  return(getOption(option_key, default))
}

#' Check if Application is in Development Mode
#'
#' Golem-style development mode detection combining explicit options
#' with environment detection.
#'
#' @return Boolean indicating development mode
#' @export
is_dev_mode <- function() {
  # Check explicit option first
  explicit_dev <- get_app_option("production_mode", NULL)
  if (!is.null(explicit_dev)) {
    return(!explicit_dev)  # dev mode is opposite of production mode
  }

  # Fall back to environment detection
  if (exists("detect_development_environment", mode = "function")) {
    return(detect_development_environment())
  }

  # Ultimate fallback
  return(interactive())
}

#' Check if Application is in Production Mode
#'
#' Golem-style production mode detection.
#'
#' @return Boolean indicating production mode
#' @export
is_prod_mode <- function() {
  # Check explicit option first
  explicit_prod <- get_app_option("production_mode", NULL)
  if (!is.null(explicit_prod)) {
    return(explicit_prod)
  }

  # Fall back to environment detection
  if (exists("detect_production_environment", mode = "function")) {
    return(detect_production_environment())
  }

  # Ultimate fallback - if not interactive, assume production
  return(!interactive())
}

#' Development Application Runner (Golem-style)
#'
#' Enhanced development runner med debugging og development-specific settings.
#' Equivalent til golem::run_dev() men tilpasset vores arkitektur.
#'
#' @param port Development port (default: 4040)
#' @param host Host address (default: 127.0.0.1 for development)
#' @param reload Enable hot reload (currently not implemented)
#' @param debug_level Debug level for development
#' @param ... Additional arguments passed to run_app()
#' @return Shiny app object
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic development run
#' run_dev()
#'
#' # Development with specific settings
#' run_dev(port = 5050, debug_level = "DEBUG")
#' }
run_dev <- function(port = 4040,
                    host = "127.0.0.1",
                    reload = FALSE,
                    debug_level = "DEBUG",
                    ...) {

  log_debug("Starting development mode application", .context = "DEV_MODE")

  # Set development-specific options
  dev_options <- list(
    production_mode = FALSE,
    test_mode = TRUE,
    debug_level = debug_level,
    auto_load_data = TRUE,
    auto_restore = FALSE,
    browser_launch = "rstudio_viewer"
  )


  # Set environment for enhanced debugging
  Sys.setenv(SHINY_DEBUG_MODE = "TRUE")
  Sys.setenv(SPC_LOG_LEVEL = debug_level)

  if (reload) {
    warning("Hot reload is not yet implemented in run_dev()")
  }

  # Run app with development settings
  app_result <- run_app(
    port = port,
    launch_browser = NULL,  # Will use environment-aware detection
    options = dev_options,
    ...
  )

  return(app_result)
}

#' Production Application Runner (Golem-style)
#'
#' Production-ready app runner med optimerede settings for deployment.
#'
#' @param port Production port (default: NULL for system assignment)
#' @param host Host address (default: 0.0.0.0 for production)
#' @param ... Additional arguments passed to run_app()
#' @return Shiny app object
#' @export
run_prod <- function(port = NULL,
                     host = "0.0.0.0",
                     ...) {

  log_debug("Starting production mode application", .context = "PROD_MODE")

  # Set production-specific options
  prod_options <- list(
    production_mode = TRUE,
    test_mode = FALSE,
    debug_level = "ERROR",
    auto_load_data = FALSE,
    auto_restore = TRUE,
    browser_launch = TRUE
  )


  # Set environment for production
  Sys.setenv(SHINY_DEBUG_MODE = "FALSE")
  Sys.setenv(SPC_LOG_LEVEL = "ERROR")

  # Run app with production settings
  app_result <- run_app(
    port = port,
    launch_browser = TRUE,  # Always launch browser in production
    options = prod_options,
    ...
  )

  return(app_result)
}

#' Get Application Information (Golem-style)
#'
#' Retrieve information about the current application configuration.
#'
#' @return List with application information
#' @export
get_app_info <- function() {
  # Try to get version from DESCRIPTION file or default
  version <- tryCatch({
    if (file.exists("DESCRIPTION")) {
      desc <- read.dcf("DESCRIPTION")
      desc[1, "Version"]
    } else {
      "0.1.0"  # Default version
    }
  }, error = function(e) {
    "0.1.0"  # Fallback version
  })

  info <- list(
    package_name = "SPCify",
    version = version,
    mode = if (is_prod_mode()) "production" else "development",
    options = list()
  )

  # Get all claudespc options
  all_options <- options()
  claudespc_options <- all_options[grepl("^claudespc\\.", names(all_options))]
  names(claudespc_options) <- gsub("^claudespc\\.", "", names(claudespc_options))
  info$options <- claudespc_options

  return(info)
}

#' Application Resource Path Setup (Golem-style)
#'
#' Setup static resource paths following golem conventions.
#'
#' @param path Path to add as resource
#' @param prefix Prefix for the resource path
#' @return Invisibly returns TRUE if successful
add_resource_path <- function(path = "www", prefix = "www") {
  # Input validation
  if (is.null(prefix) || length(prefix) == 0 || prefix == "" || is.na(prefix)) {
    if (exists("log_warn")) {
      log_warn("Invalid prefix provided to add_resource_path", .context = "RESOURCE_PATHS")
    } else {
      warning("Invalid prefix provided to add_resource_path")
    }
    return(invisible(FALSE))
  }

  if (is.null(path) || length(path) == 0 || path == "" || is.na(path)) {
    if (exists("log_warn")) {
      log_warn("Invalid path provided to add_resource_path", .context = "RESOURCE_PATHS")
    } else {
      warning("Invalid path provided to add_resource_path")
    }
    return(invisible(FALSE))
  }

  # Use system.file() for packaged apps, fallback for development
  if (path == "www") {
    www_path <- system.file("app", "www", package = "SPCify")
    if (www_path == "") {
      # Development mode fallbacks
      possible_paths <- c(
        file.path("inst", "app", "www"),
        file.path("www"),
        file.path("app", "www")
      )

      www_path <- NULL
      for (possible_path in possible_paths) {
        if (dir.exists(possible_path)) {
          www_path <- possible_path
          break
        }
      }

      if (is.null(www_path)) {
        if (exists("log_debug")) {
          log_debug("No www directory found in development mode", .context = "RESOURCE_PATHS")
        }
        return(invisible(FALSE))
      }
    }
    path <- www_path
  }

  if (dir.exists(path)) {
    # Additional validation for shiny::addResourcePath requirements
    tryCatch({
      shiny::addResourcePath(prefix, path)
      if (exists("log_debug")) {
        log_debug(paste("Added resource path:", prefix, "->", path), "RESOURCE_PATHS")
      }
      return(invisible(TRUE))
    }, error = function(e) {
      if (exists("log_warn")) {
        log_warn(paste("Failed to add resource path:", e$message), "RESOURCE_PATHS")
      } else {
        warning(paste("Failed to add resource path:", e$message))
      }
      return(invisible(FALSE))
    })
  } else {
    if (exists("log_debug")) {
      log_debug(paste("Resource path not found:", path), "RESOURCE_PATHS")
    }
    return(invisible(FALSE))
  }
}

#' Favicon Setup (Golem-style)
#'
#' Setup application favicon following golem patterns.
#'
#' @param path Path to favicon file
#' @return HTML tags for favicon
favicon <- function(path = "www/favicon.ico") {
  # For packaged apps, adjust favicon path
  if (path == "www/favicon.ico") {
    favicon_path <- system.file("app", "www", "favicon.ico", package = "SPCify")
    if (favicon_path == "") {
      favicon_path <- file.path("inst", "app", "www", "favicon.ico")
    }
    if (file.exists(favicon_path)) {
      path <- "www/favicon.ico"  # Keep relative path for href
    }
  }

  if (file.exists(path) || grepl("^www/", path)) {
    return(shiny::tags$head(shiny::tags$link(rel = "icon", href = path)))
  } else {
    log_debug(paste("Favicon not found:", path), .context = "FAVICON")
    return(NULL)
  }
}

# CONFIGURATION SUPPORT ======================================================
# Note: Golem configuration is handled via config::get() in app_config.R
# This avoids duplicate YAML readers and ensures consistent config loading

#' Detect Golem Environment
#'
#' Detect current deployment environment following golem conventions.
#'
#' @return String indicating environment (development, production, testing, default)
detect_golem_environment <- function() {
  # Check GOLEM_CONFIG_ACTIVE first (primary environment variable)
  golem_config <- Sys.getenv("GOLEM_CONFIG_ACTIVE", "")
  if (golem_config != "") {
    mapped_env <- switch(golem_config,
      "development" = "development",
      "dev" = "development",
      "production" = "production",
      "prod" = "production",
      "testing" = "testing",
      "test" = "testing",
      "default"  # Fallback
    )
    log_debug(paste("Environment detected from GOLEM_CONFIG_ACTIVE:", mapped_env), .context = "GOLEM_ENV")
    return(mapped_env)
  }

  # Map R_CONFIG_ACTIVE to GOLEM_CONFIG_ACTIVE for backward compatibility
  r_config <- Sys.getenv("R_CONFIG_ACTIVE", "")
  if (r_config != "") {
    mapped_env <- switch(r_config,
      "development" = "development",
      "dev" = "development",
      "production" = "production",
      "prod" = "production",
      "testing" = "testing",
      "test" = "testing",
      "default"  # Fallback
    )
    # Set GOLEM_CONFIG_ACTIVE based on R_CONFIG_ACTIVE for consistency
    Sys.setenv(GOLEM_CONFIG_ACTIVE = mapped_env)
    log_debug(paste("Environment mapped from R_CONFIG_ACTIVE to GOLEM_CONFIG_ACTIVE:", mapped_env), .context = "GOLEM_ENV")
    return(mapped_env)
  }

  # Check application mode
  if (exists("is_prod_mode", mode = "function") && is_prod_mode()) {
    return("production")
  }

  if (exists("is_dev_mode", mode = "function") && is_dev_mode()) {
    return("development")
  }

  # Check for testing environment
  if (any(c("testthat", "test") %in% search())) {
    return("testing")
  }

  # Interactive session implies development
  if (interactive()) {
    return("development")
  }

  # Default fallback
  log_debug("No specific environment detected, using default", .context = "GOLEM_ENV")
  return("default")
}

#' Get Fallback Golem Configuration
#'
#' Provide fallback configuration when YAML loading fails.
#'
#' @param env Environment name
#' @return List with basic configuration
get_fallback_golem_config <- function(env) {
  log_debug(paste("Using fallback golem configuration for:", env), .context = "GOLEM_FALLBACK")

  # Basic fallback configuration
  base_config <- list(
    golem_name = "claudeSPC",
    golem_version = "1.0.0",
    app_prod = FALSE,

    environment = list(
      type = env,
      is_development = (env == "development"),
      is_production = (env == "production"),
      is_testing = (env == "testing")
    ),

    logging = list(
      level = "INFO",
      enable_debug_mode = (env %in% c("development", "testing"))
    ),

    testing = list(
      auto_load_test_data = (env == "development"),
      test_data_file = if (env == "development") "inst/extdata/spc_exampledata.csv" else NULL
    ),

    session = list(
      auto_restore_session = (env == "production")
    )
  )

  return(base_config)
}

#' Apply Golem Configuration to Runtime
#'
#' Apply loaded golem configuration til application runtime.
#' Bridges between YAML config og eksisterende systems.
#'
#' @param golem_config Configuration from get_golem_config()
#' @return Invisible TRUE
#' @export
apply_golem_config <- function(golem_config) {
  log_debug("Applying golem configuration to runtime", .context = "GOLEM_APPLY")

  if (is.null(golem_config)) {
    log_debug("No golem configuration to apply", .context = "GOLEM_APPLY")
    return(invisible(FALSE))
  }

  # Apply app options
  if (!is.null(golem_config$environment)) {
    app_options <- list(
      production_mode = golem_config$environment$is_production %||% FALSE,
      test_mode = golem_config$testing$auto_load_test_data %||% FALSE,
      debug_level = golem_config$logging$level %||% "INFO"
    )
    set_app_options(app_options)
  }

  # Apply environment variables
  if (!is.null(golem_config$logging)) {
    if (!is.null(golem_config$logging$level)) {
      Sys.setenv(SPC_LOG_LEVEL = golem_config$logging$level)
    }

    if (!is.null(golem_config$logging$enable_debug_mode)) {
      Sys.setenv(SHINY_DEBUG_MODE = if (golem_config$logging$enable_debug_mode) "TRUE" else "FALSE")
    }
  }

  # Apply global variables for backward compatibility
  if (!is.null(golem_config$testing$auto_load_test_data)) {
    assign("TEST_MODE_AUTO_LOAD", golem_config$testing$auto_load_test_data, envir = .GlobalEnv)
  }

  if (!is.null(golem_config$session$auto_restore_session)) {
    assign("AUTO_RESTORE_ENABLED", golem_config$session$auto_restore_session, envir = .GlobalEnv)
  }

  log_debug("Golem configuration applied to runtime", .context = "GOLEM_APPLY")
  return(invisible(TRUE))
}

#' Get Current Golem Configuration Summary
#'
#' Get human-readable summary af current golem configuration.
#'
#' @return Character vector with configuration summary
#' @export
get_golem_config_summary <- function() {
  env <- detect_golem_environment()

  # Use fallback configuration since detailed config is handled via config::get()
  config <- get_fallback_golem_config(env)

  if (is.null(config)) {
    return("Golem configuration not available")
  }

  summary_lines <- c(
    paste("Golem App:", config$golem_name %||% "claudeSPC"),
    paste("Version:", config$golem_version %||% "1.0.0"),
    paste("Environment:", config$environment$type %||% env),
    paste("Production Mode:", config$app_prod %||% FALSE),
    paste("Debug Mode:", config$logging$enable_debug_mode %||% FALSE),
    paste("Log Level:", config$logging$level %||% "INFO"),
    paste("Auto Load Test Data:", config$testing$auto_load_test_data %||% FALSE),
    paste("Auto Restore Session:", config$session$auto_restore_session %||% FALSE)
  )

  return(summary_lines)
}

# HELPER FUNCTIONS ============================================================

#' Null-coalescing operator
#'
# Null coalescing operator is defined in utils_logging.R