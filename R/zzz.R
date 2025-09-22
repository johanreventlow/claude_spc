#' Package initialization and loading
#'
#' This file handles package initialization when claudespc is loaded.
#' It replaces the global.R source() chain with proper package loading.
#'
#' @param libname Library name (not used)
#' @param pkgname Package name (should be "claudespc")
#'
#' @importFrom utils packageVersion
#' @noRd

# Package-level environment for storing configuration and variables
# This prevents pollution of .GlobalEnv
.claudespc_env <- NULL

#' Get or create the claudespc package environment
#'
#' @description
#' Returns the package-level environment used for storing configuration
#' and global variables. Creates it if it doesn't exist.
#'
#' @return Environment containing package-level variables
#' @noRd
get_claudespc_environment <- function() {
  if (is.null(.claudespc_env)) {
    .claudespc_env <<- new.env(parent = emptyenv())
  }
  return(.claudespc_env)
}

#' Get configuration value from package environment
#'
#' @param key Configuration key to retrieve
#' @param default Default value if key not found
#' @return Configuration value or default
#' @export
get_package_config <- function(key, default = NULL) {
  claudespc_env <- get_claudespc_environment()
  if (exists(key, envir = claudespc_env)) {
    return(get(key, envir = claudespc_env))
  } else {
    return(default)
  }
}

#' Get the complete runtime configuration
#'
#' @return Runtime configuration list or NULL
#' @export
get_runtime_config <- function() {
  get_package_config("runtime_config", default = NULL)
}

#' Get test mode auto load setting
#'
#' @return Boolean indicating if test mode auto load is enabled
#' @export
get_test_mode_auto_load <- function() {
  get_package_config("TEST_MODE_AUTO_LOAD", default = FALSE)
}

#' Get auto restore enabled setting
#'
#' @return Boolean indicating if auto restore is enabled
#' @export
get_auto_restore_enabled <- function() {
  get_package_config("AUTO_RESTORE_ENABLED", default = FALSE)
}

#' Get hospital name from package environment
#'
#' @return Hospital name string
#' @export
get_package_hospital_name <- function() {
  get_package_config("HOSPITAL_NAME", default = "Unknown Hospital")
}

#' Get hospital theme from package environment
#'
#' @return Bootstrap theme object
#' @export
get_package_theme <- function() {
  get_package_config("my_theme", default = NULL)
}

#' Get test mode file path
#'
#' @return Test file path string
#' @export
get_test_mode_file_path <- function() {
  get_package_config("TEST_MODE_FILE_PATH", default = NULL)
}
.onLoad <- function(libname, pkgname) {

  # Initialize package-level configuration
  initialize_package_globals()

  # Set up logging
  initialize_logging_system()

  # Load runtime configuration using the full implementation
  setup_package_runtime_config()

  # Set up resource paths for static files
  setup_resource_paths()

  invisible()
}

#' Initialize package-level global variables
#'
#' @noRd
initialize_package_globals <- function() {

  # Initialize branding configuration using safe getters
  initialize_branding()

  # Store branding in package environment instead of polluting .GlobalEnv
  claudespc_env <- get_claudespc_environment()
  claudespc_env$HOSPITAL_NAME <- get_hospital_name()
  claudespc_env$HOSPITAL_LOGO_PATH <- get_hospital_logo_path()
  claudespc_env$my_theme <- get_bootstrap_theme()
  claudespc_env$HOSPITAL_COLORS <- get_hospital_colors()
  claudespc_env$HOSPITAL_THEME <- get_hospital_ggplot_theme()

  # NOTE: Global environment exposure removed as part of legacy cleanup
  # All consumers should now use package getters:
  # - get_package_hospital_name() instead of HOSPITAL_NAME
  # - get_package_theme() instead of my_theme
  # - get_package_config("HOSPITAL_LOGO_PATH") instead of HOSPITAL_LOGO_PATH
  # - get_package_config("HOSPITAL_COLORS") instead of HOSPITAL_COLORS
}

#' Initialize logging system
#'
#' @noRd
initialize_logging_system <- function() {
  # Set default log level if not set
  if (!nzchar(Sys.getenv("SPC_LOG_LEVEL", ""))) {
    Sys.setenv(SPC_LOG_LEVEL = "INFO")
  }
  # Logging functions (log_debug, log_info, etc.) handle their own
  # availability checks and fallbacks, so no additional setup needed
}


#' Setup package runtime configuration (called during .onLoad)
#'
#' @noRd
setup_package_runtime_config <- function() {
  # Call the full initialize_runtime_config() implementation
  # and store result in package environment instead of .GlobalEnv
  config <- initialize_runtime_config()

  if (!is.null(config)) {
    # Store config in package environment
    claudespc_env <- get_claudespc_environment()
    claudespc_env$runtime_config <- config

    # Set up performance-related globals in package environment too
    if (!is.null(config$testing)) {
      claudespc_env$TEST_MODE_AUTO_LOAD <- config$testing$auto_load_enabled %||% FALSE
    }
    if (!is.null(config$development)) {
      claudespc_env$AUTO_RESTORE_ENABLED <- config$development$auto_restore_enabled %||% FALSE
    }
  }

  invisible()
}

#' Set up resource paths for static files
#'
#' @noRd
setup_resource_paths <- function() {
  # Resource paths will be set up in golem_add_external_resources()
  # when the app starts
  invisible()
}

#' Package unload cleanup
#'
#' @param libpath Library path (not used)
#' @noRd
.onUnload <- function(libpath) {
  # Clean up any package-level resources
  if (exists("claudespc_globals", envir = parent.env(environment()))) {
    rm("claudespc_globals", envir = parent.env(environment()))
  }

  # Global environment cleanup no longer needed since globals are not exposed

  invisible()
}
