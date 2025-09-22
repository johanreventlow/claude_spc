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

  # Create package environment for globals
  if (!exists("claudespc_globals", envir = parent.env(environment()))) {
    claudespc_globals <- new.env(parent = emptyenv())
    assign("claudespc_globals", claudespc_globals, envir = parent.env(environment()))
  }

  # Initialize hospital branding (from hospital_branding.R)
  claudespc_globals$HOSPITAL_NAME <- "Bornholms Hospital"
  claudespc_globals$HOSPITAL_LOGO_PATH <- "www/hospital_logo.png"

  # Initialize theme (basic version - full theme will be loaded by UI)
  claudespc_globals$my_theme <- bslib::bs_theme(
    version = 5,
    preset = "bootstrap"
  )

  # Make globals available in global environment for backward compatibility
  assign("HOSPITAL_NAME", claudespc_globals$HOSPITAL_NAME, envir = .GlobalEnv)
  assign("HOSPITAL_LOGO_PATH", claudespc_globals$HOSPITAL_LOGO_PATH, envir = .GlobalEnv)
  assign("my_theme", claudespc_globals$my_theme, envir = .GlobalEnv)
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