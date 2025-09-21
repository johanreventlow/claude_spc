# app_config.R
# Centralized Application Configuration Management
# Fase 3.2: Modular initialization component

#' Initialize Complete Application Configuration
#'
#' @description
#' Centraliseret funktion til at sætte alle app-konfigurationer på en kontrolleret måde.
#' Erstatter spread konfiguration i global.R med centraliseret setup.
#'
#' @return List containing all configuration values
#' @export
initialize_app_config <- function() {
  log_debug("Starting centralized app configuration", "APP_CONFIG")

  config <- list()

  # Environment Detection and Core Settings
  config$environment <- setup_environment_config()

  # Feature Flags based on Environment
  config$features <- setup_feature_flags(config$environment)

  # Performance and Resource Settings
  config$performance <- setup_performance_config(config$environment)

  # Logging Configuration
  config$logging <- setup_logging_config(config$environment)

  # Development Tools Configuration
  config$development <- setup_development_config(config$environment)

  log_debug("✅ App configuration completed successfully", "APP_CONFIG")
  return(config)
}

#' Environment Detection and Setup
#'
#' @description
#' Robust environment detection that replaces global environment variables
#' with structured configuration management.
#'
#' @return List with environment configuration
setup_environment_config <- function() {
  log_debug("Detecting deployment environment", "ENV_DETECTION")

  config <- list()

  # Primary environment detection
  config$is_production <- detect_production_environment()
  config$is_development <- detect_development_environment()
  config$is_testing <- detect_testing_environment()

  # Deployment context
  config$deployment_type <- determine_deployment_type()

  # Environment variables to respect
  config$explicit_vars <- list(
    test_mode = Sys.getenv("TEST_MODE_AUTO_LOAD", ""),
    log_level = Sys.getenv("SPC_LOG_LEVEL", ""),
    debug_mode = Sys.getenv("SHINY_DEBUG_MODE", ""),
    config_active = Sys.getenv("R_CONFIG_ACTIVE", "")
  )

  log_debug(paste("Environment detected as:", config$deployment_type), "ENV_DETECTION")
  return(config)
}

#' Feature Flags Setup
#'
#' @description
#' Centralized feature flag management med environment-aware defaults.
#' Erstatter scattered boolean variables i global.R.
#'
#' @param env_config Environment configuration from setup_environment_config()
#' @return List with feature flags
setup_feature_flags <- function(env_config) {
  log_debug("Setting up feature flags", "FEATURE_FLAGS")

  flags <- list()

  # TEST_MODE_AUTO_LOAD - Smart environment-aware default
  flags$test_mode_auto_load <- determine_test_mode_setting(env_config)

  # AUTO_RESTORE_ENABLED - Environment-aware session restoration
  flags$auto_restore_enabled <- determine_auto_restore_setting(env_config)

  # SHINY_DEBUG_MODE - Enhanced debugging for development
  flags$debug_mode_enabled <- determine_debug_mode_setting(env_config)

  # Future feature flags can be added here
  flags$enhanced_logging_enabled <- env_config$is_development
  flags$performance_monitoring_enabled <- !env_config$is_production

  log_debug(paste("Test mode auto load:", flags$test_mode_auto_load), "FEATURE_FLAGS")
  log_debug(paste("Auto restore enabled:", flags$auto_restore_enabled), "FEATURE_FLAGS")
  log_debug(paste("Debug mode enabled:", flags$debug_mode_enabled), "FEATURE_FLAGS")

  return(flags)
}

#' Performance Configuration Setup
#'
#' @description
#' Environment-aware performance settings for optimal resource usage.
#'
#' @param env_config Environment configuration
#' @return List with performance settings
setup_performance_config <- function(env_config) {
  log_debug("Setting up performance configuration", "PERFORMANCE_CONFIG")

  perf <- list()

  # Memory management settings
  perf$aggressive_cleanup <- env_config$is_production
  perf$observer_cleanup_on_session_end <- TRUE
  perf$reactive_debounce_default <- if (env_config$is_development) 500 else 1000

  # Performance monitoring
  perf$memory_monitoring_enabled <- env_config$is_development
  perf$timing_monitoring_enabled <- env_config$is_development

  # Resource limits
  perf$max_file_size_mb <- if (env_config$is_production) 50 else 100
  perf$max_observers_per_session <- if (env_config$is_production) 100 else 200

  return(perf)
}

#' Logging Configuration Setup
#'
#' @description
#' Centralized logging configuration med environment-aware levels.
#'
#' @param env_config Environment configuration
#' @return List with logging settings
setup_logging_config <- function(env_config) {
  log_debug("Setting up logging configuration", "LOGGING_CONFIG")

  logging <- list()

  # Log levels based on environment
  if (env_config$explicit_vars$log_level != "") {
    # Respect explicit setting
    logging$level <- env_config$explicit_vars$log_level
  } else if (env_config$is_development) {
    # Development: verbose logging
    logging$level <- "DEBUG"
  } else if (env_config$is_testing) {
    # Testing: moderate logging
    logging$level <- "INFO"
  } else {
    # Production: minimal logging
    logging$level <- "WARN"
  }

  # Advanced logging features
  logging$web_logging_enabled <- env_config$is_development
  logging$session_tracking_enabled <- TRUE
  logging$error_tracking_enabled <- TRUE

  # Set the environment variable for the logging system
  Sys.setenv(SPC_LOG_LEVEL = logging$level)

  log_debug(paste("Log level set to:", logging$level), "LOGGING_CONFIG")
  return(logging)
}

#' Development Tools Configuration
#'
#' @description
#' Development-specific settings og test data configuration.
#'
#' @param env_config Environment configuration
#' @return List with development settings
setup_development_config <- function(env_config) {
  dev <- list()

  # Test data settings
  dev$test_file_path <- "R/data/spc_exampledata.csv"  # Default test file

  # Alternative test files for different scenarios
  dev$alternative_test_files <- list(
    infection_data = "R/data/test_infection.csv",
    example_data_1 = "R/data/spc_exampledata1.csv",
    excel_data = "R/data/SPC_test_data_forskellige.xlsx"
  )

  # Development utilities
  dev$hot_reload_enabled <- env_config$is_development
  dev$debug_ui_enabled <- env_config$is_development
  dev$enhanced_error_display <- env_config$is_development

  return(dev)
}

# HELPER FUNCTIONS FOR ENVIRONMENT DETECTION =================================

#' Detect Production Environment
#'
#' @description
#' Identify production deployment contexts where safe defaults are required.
#'
#' @return Boolean indicating production environment
detect_production_environment <- function() {
  # Known production environment indicators
  production_indicators <- c(
    Sys.getenv("SHINY_SERVER_VERSION") != "",     # Shiny Server
    Sys.getenv("CONNECT_SERVER") != "",           # Posit Connect
    Sys.getenv("SHINYAPPS_SERVER") != "",         # shinyapps.io
    Sys.getenv("R_CONFIG_ACTIVE") == "production" # Explicit production
  )

  return(any(production_indicators))
}

#' Detect Development Environment
#'
#' @description
#' Identify development contexts where convenience features can be enabled.
#'
#' @return Boolean indicating development environment
detect_development_environment <- function() {
  # Known development environment indicators
  development_indicators <- c(
    Sys.getenv("RSTUDIO") == "1",                 # RStudio IDE
    interactive(),                                # Interactive R session
    Sys.getenv("R_CONFIG_ACTIVE") == "development" # Explicit development
  )

  return(any(development_indicators))
}

#' Detect Testing Environment
#'
#' @description
#' Identify test execution contexts.
#'
#' @return Boolean indicating testing environment
detect_testing_environment <- function() {
  # Testing environment indicators
  testing_indicators <- c(
    Sys.getenv("TESTTHAT") == "true",
    Sys.getenv("R_CONFIG_ACTIVE") == "test",
    grepl("testthat", getwd(), ignore.case = TRUE)
  )

  return(any(testing_indicators))
}

#' Determine Deployment Type
#'
#' @description
#' Classify the current deployment type for configuration purposes.
#'
#' @return String indicating deployment type
determine_deployment_type <- function() {
  if (detect_production_environment()) {
    return("production")
  } else if (detect_development_environment()) {
    return("development")
  } else if (detect_testing_environment()) {
    return("testing")
  } else {
    return("unknown")
  }
}

# HELPER FUNCTIONS FOR FEATURE FLAGS =====================================

#' Determine Test Mode Setting
#'
#' @description
#' Smart logic for TEST_MODE_AUTO_LOAD setting med environment awareness.
#' Replaces the detect_environment() function from global.R.
#'
#' @param env_config Environment configuration
#' @return Boolean for test mode setting
determine_test_mode_setting <- function(env_config) {
  # Respect explicit environment variable first
  if (env_config$explicit_vars$test_mode != "") {
    return(as.logical(env_config$explicit_vars$test_mode))
  }

  # Environment-based defaults
  if (env_config$is_production) {
    return(FALSE)  # Safe default for production
  } else if (env_config$is_development) {
    return(TRUE)   # Convenient default for development
  } else if (env_config$is_testing) {
    return(FALSE)  # Controlled environment for tests
  } else {
    return(FALSE)  # Safe default for unknown contexts
  }
}

#' Determine Auto Restore Setting
#'
#' @description
#' Environment-aware auto restore configuration.
#'
#' @param env_config Environment configuration
#' @return Boolean for auto restore setting
determine_auto_restore_setting <- function(env_config) {
  # Auto restore typically disabled in development for clean testing
  # Enabled in production for user convenience
  if (env_config$is_development) {
    return(FALSE)  # Clean state for development
  } else if (env_config$is_production) {
    return(TRUE)   # User convenience in production
  } else {
    return(FALSE)  # Safe default
  }
}

#' Determine Debug Mode Setting
#'
#' @description
#' Environment-aware debug mode configuration.
#'
#' @param env_config Environment configuration
#' @return Boolean for debug mode setting
determine_debug_mode_setting <- function(env_config) {
  # Respect explicit setting first
  if (env_config$explicit_vars$debug_mode != "") {
    return(env_config$explicit_vars$debug_mode == "TRUE")
  }

  # Environment-based defaults
  return(env_config$is_development)  # Only enable in development by default
}