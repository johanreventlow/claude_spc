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
.onLoad <- function(libname, pkgname) {

  # Initialize package-level configuration
  initialize_package_globals()

  # Set up logging
  initialize_logging_system()

  # Load runtime configuration
  initialize_runtime_config()

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

  # Make branding available in global environment for backward compatibility
  # These now use the safe getters instead of hardcoded values
  assign("HOSPITAL_NAME", get_hospital_name(), envir = .GlobalEnv)
  assign("HOSPITAL_LOGO_PATH", get_hospital_logo_path(), envir = .GlobalEnv)
  assign("my_theme", get_bootstrap_theme(), envir = .GlobalEnv)

  # Also make hospital colors available for backward compatibility
  assign("HOSPITAL_COLORS", get_hospital_colors(), envir = .GlobalEnv)
  assign("HOSPITAL_THEME", get_hospital_ggplot_theme(), envir = .GlobalEnv)
}

#' Initialize logging system
#'
#' @noRd
initialize_logging_system <- function() {
  # Set default log level if not set
  if (!nzchar(Sys.getenv("SPC_LOG_LEVEL", ""))) {
    Sys.setenv(SPC_LOG_LEVEL = "INFO")
  }

  # Initialize any logging infrastructure here
  # (Most logging functions are defined in their respective files)
}

#' Initialize runtime configuration
#'
#' @noRd
initialize_runtime_config <- function() {
  # This will be called when the package loads
  # Runtime config initialization will happen in app startup
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

  # Remove globals from global environment
  if (exists("HOSPITAL_NAME", envir = .GlobalEnv)) {
    rm("HOSPITAL_NAME", envir = .GlobalEnv)
  }
  if (exists("HOSPITAL_LOGO_PATH", envir = .GlobalEnv)) {
    rm("HOSPITAL_LOGO_PATH", envir = .GlobalEnv)
  }
  if (exists("my_theme", envir = .GlobalEnv)) {
    rm("my_theme", envir = .GlobalEnv)
  }

  invisible()
}