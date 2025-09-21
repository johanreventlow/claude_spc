# golem_utils.R
# Golem-inspired utility functions for robust Shiny app management
# Fase 3.4: Proper golem patterns implementation

#' Set Application Options (Golem-style)
#'
#' @description
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

  log_debug(paste("Setting app options:", length(options), "options provided"), "GOLEM_OPTIONS")

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

  log_debug(paste("✅ App options set successfully:", paste(names(merged_options), collapse = ", ")), "GOLEM_OPTIONS")
  return(invisible(merged_options))
}

#' Get Application Option (Golem-style)
#'
#' @description
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
#' @description
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
#' @description
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
#' @description
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

  log_debug("Starting development mode application", "DEV_MODE")

  # Set development-specific options
  dev_options <- list(
    production_mode = FALSE,
    test_mode = TRUE,
    debug_level = debug_level,
    auto_load_data = TRUE,
    auto_restore = FALSE,
    browser_launch = "rstudio_viewer"
  )

  log_debug(paste("Development options:", paste(names(dev_options), collapse = ", ")), "DEV_MODE")

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

  log_debug("✅ Development application started successfully", "DEV_MODE")
  return(app_result)
}

#' Production Application Runner (Golem-style)
#'
#' @description
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

  log_debug("Starting production mode application", "PROD_MODE")

  # Set production-specific options
  prod_options <- list(
    production_mode = TRUE,
    test_mode = FALSE,
    debug_level = "ERROR",
    auto_load_data = FALSE,
    auto_restore = TRUE,
    browser_launch = TRUE
  )

  log_debug(paste("Production options:", paste(names(prod_options), collapse = ", ")), "PROD_MODE")

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

  log_debug("✅ Production application started successfully", "PROD_MODE")
  return(app_result)
}

#' Get Application Information (Golem-style)
#'
#' @description
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
    package_name = "claudespc",
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
#' @description
#' Setup static resource paths following golem conventions.
#'
#' @param path Path to add as resource
#' @param prefix Prefix for the resource path
#' @return Invisibly returns TRUE if successful
add_resource_path <- function(path = "www", prefix = "www") {
  if (dir.exists(path)) {
    shiny::addResourcePath(prefix, path)
    log_debug(paste("✅ Added resource path:", prefix, "->", path), "RESOURCE_PATHS")
    return(invisible(TRUE))
  } else {
    log_debug(paste("Resource path not found:", path), "RESOURCE_PATHS")
    return(invisible(FALSE))
  }
}

#' Favicon Setup (Golem-style)
#'
#' @description
#' Setup application favicon following golem patterns.
#'
#' @param path Path to favicon file
#' @return HTML tags for favicon
favicon <- function(path = "www/favicon.ico") {
  if (file.exists(path)) {
    return(shiny::tags$head(shiny::tags$link(rel = "icon", href = path)))
  } else {
    log_debug(paste("Favicon not found:", path), "FAVICON")
    return(NULL)
  }
}