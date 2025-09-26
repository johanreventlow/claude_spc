# app_runtime_config.R
# Centralized runtime configuration management
# Fase 4.1: Consolidate scattered constants and integrate with golem-style options

#' Initialize Complete Runtime Configuration
#'
#' @description
#' Centraliseret runtime konfiguration som erstatter spredte konstanter i global.R
#' og integrerer med golem-style options og environment profiles for environment-aware configuration.
#'
#' @param override_options Optional list to override default configuration
#' @param use_environment_profiles Whether to use new environment profiles (default: TRUE)
#' @return List with complete runtime configuration
#' @export
initialize_runtime_config <- function(override_options = list(), use_environment_profiles = TRUE) {
  # Note: No logging during package initialization to avoid dependency issues

  # Choose configuration approach - for now use legacy approach only
  if (use_environment_profiles && exists("get_environment_profile", mode = "function")) {
    # log_debug( "Using environment profiles for configuration", .context = "RUNTIME_CONFIG")

    # Environment detection
    environment_type <- determine_environment_type_from_context(override_options)
    # log_debug( paste("Detected environment type:", environment_type), .context = "RUNTIME_CONFIG")

    # Get environment-specific profile
    profile <- get_environment_profile(environment_type, override_options)

    # Apply profile to runtime and environment variables
    apply_environment_profile(profile)

    # Convert profile to legacy format for backward compatibility
    config <- convert_profile_to_legacy_config(profile)

  } else {
    # log_debug( "Using legacy configuration approach", .context = "RUNTIME_CONFIG")

    # Legacy configuration structure
    config <- list()

    # Development and testing configuration
    config$development <- setup_development_config(override_options)

    # Environment detection and features
    config$environment <- setup_environment_features(override_options)

    # Logging and debugging configuration
    config$logging <- setup_logging_features(override_options)

    # Test mode and data configuration
    config$testing <- setup_testing_config(override_options)

    # Performance and timeout configuration
    config$performance <- setup_performance_config(override_options)

    # Apply configuration to global variables for backward compatibility
    apply_runtime_config(config)
  }

  # log_debug("Runtime configuration initialized successfully", .context = "RUNTIME_CONFIG")
  return(config)
}

#' Setup Development Configuration
#'
#' @description
#' Development-specific settings med environment-aware defaults.
#'
#' @param override_options Optional override values
#' @return List with development configuration
setup_development_config <- function(override_options = list()) {
  dev_config <- list()

  # Auto restore settings - environment aware
  dev_config$auto_restore_enabled <- determine_auto_restore_setting(override_options)

  # Default development port
  dev_config$default_port <- get_override_or_default(override_options, "default_port", 3838)

  # Development-specific features
  dev_config$hot_reload_enabled <- get_override_or_default(override_options, "hot_reload", FALSE)
  dev_config$enhanced_debugging <- get_override_or_default(override_options, "enhanced_debug", NULL)

  return(dev_config)
}

#' Setup Environment Features
#'
#' @description
#' Environment detection og feature flags for miljøafhængig adfærd.
#' Uses new environment profile detection system.
#'
#' @param override_options Optional override values
#' @return List with environment configuration
setup_environment_features <- function(override_options = list()) {
  env_config <- list()

  # Use new environment detection system
  if (exists("detect_environment_from_context", mode = "function")) {
    environment_type <- detect_environment_from_context()

    env_config$is_development <- (environment_type == "development")
    env_config$is_production <- (environment_type == "production")
    env_config$is_testing <- (environment_type == "testing")
  } else {
    # Fallback to legacy detection
    if (exists("detect_development_environment", mode = "function")) {
      env_config$is_development <- detect_development_environment()
      env_config$is_production <- detect_production_environment()
      env_config$is_testing <- detect_testing_environment()
    } else {
      # Ultimate fallback
      env_config$is_development <- interactive()
      env_config$is_production <- !interactive()
      env_config$is_testing <- FALSE
    }
  }

  # Environment-based feature flags
  env_config$environment_type <- determine_environment_type(env_config)

  return(env_config)
}

#' Setup Logging Features
#'
#' @description
#' Centraliseret logging configuration med environment-aware levels.
#'
#' @param override_options Optional override values
#' @return List with logging configuration
setup_logging_features <- function(override_options = list()) {
  logging_config <- list()

  # Debug mode detection
  logging_config$debug_mode_enabled <- determine_debug_mode_setting(override_options)

  # Log level determination
  logging_config$log_level <- determine_log_level(override_options)

  # Enhanced debugging features
  logging_config$enhanced_debugging <- logging_config$debug_mode_enabled
  logging_config$reactive_logging <- logging_config$debug_mode_enabled

  return(logging_config)
}

#' Setup Testing Configuration
#'
#' @description
#' Test mode og eksempeldata konfiguration.
#'
#' @param override_options Optional override values
#' @return List with testing configuration
setup_testing_config <- function(override_options = list()) {
  testing_config <- list()

  # Test mode auto load - environment aware
  testing_config$auto_load_enabled <- determine_test_mode_setting(override_options)

  # Test data file configuration
  testing_config$test_file_path <- get_override_or_default(
    override_options,
    "test_file_path",
    "inst/extdata/spc_exampledata.csv"
  )

  # Alternative test files
  testing_config$alternative_files <- list(
    infection_data = "R/data/test_infection.csv",
    example_data_1 = "R/data/spc_exampledata1.csv",
    excel_data = "R/data/SPC_test_data_forskellige.xlsx"
  )

  # Phase 3: Test mode optimization settings
  testing_golem_config <- tryCatch({
    golem_config <- golem::get_current_config()
    golem_config$testing %||% list()
  }, error = function(e) {
    # Fallback if golem config is not available
    list()
  })

  testing_config$startup_debounce_ms <- testing_golem_config$startup_debounce_ms %||% 500
  testing_config$lazy_plot_generation <- testing_golem_config$lazy_plot_generation %||% TRUE
  testing_config$auto_detection_delay_ms <- testing_golem_config$auto_detection_delay_ms %||% 250
  testing_config$race_condition_prevention <- testing_golem_config$race_condition_prevention %||% TRUE

  return(testing_config)
}

#' Setup Performance Configuration
#'
#' @description
#' Performance og timeout settings med environment-aware optimizations.
#'
#' @param override_options Optional override values
#' @return List with performance configuration
setup_performance_config <- function(override_options = list()) {
  perf_config <- list()

  # Inherit existing performance constants
  if (exists("PERFORMANCE_THRESHOLDS")) {
    perf_config$thresholds <- PERFORMANCE_THRESHOLDS
  }

  if (exists("OPERATION_TIMEOUTS")) {
    perf_config$timeouts <- OPERATION_TIMEOUTS
  }

  if (exists("DEBOUNCE_DELAYS")) {
    perf_config$debounce <- DEBOUNCE_DELAYS
  }

  # Environment-specific performance adjustments
  env_type <- get_override_or_default(override_options, "environment_type", "development")

  if (env_type == "production") {
    # Production: More conservative timeouts
    perf_config$debounce$input_change <- 500  # Slower for stability
    perf_config$thresholds$reactive_warning <- 1.0  # More tolerance
  } else if (env_type == "development") {
    # Development: Faster response for productivity
    perf_config$debounce$input_change <- 200  # Faster feedback
    perf_config$thresholds$reactive_warning <- 0.3  # Stricter monitoring
  }

  return(perf_config)
}

#' Apply Runtime Configuration to Global Variables
#'
#' @description
#' Sæt globale variabler for backward compatibility med eksisterende kode.
#' Denne funktion sikrer at eksisterende kode der forventer globale konstanter
#' stadig fungerer efter centralisering.
#'
#' @param config Complete runtime configuration
#' @return Invisible TRUE
apply_runtime_config <- function(config) {
  # log_debug("Applying runtime configuration to global variables", .context = "CONFIG_APPLY")

  # Set configuration in package environment (not .GlobalEnv)
  claudespc_env <- get_claudespc_environment()

  if (!is.null(config$development$auto_restore_enabled)) {
    claudespc_env$AUTO_RESTORE_ENABLED <- config$development$auto_restore_enabled
  }

  if (!is.null(config$testing$auto_load_enabled)) {
    claudespc_env$TEST_MODE_AUTO_LOAD <- config$testing$auto_load_enabled
  }

  if (!is.null(config$testing$test_file_path)) {
    claudespc_env$TEST_MODE_FILE_PATH <- config$testing$test_file_path
  }

  # SHINY_DEBUG_MODE now managed by environment profiles

  # Set environment variables for logging system
  if (!is.null(config$logging$log_level)) {
    Sys.setenv(SPC_LOG_LEVEL = config$logging$log_level)
  }

  # Environment variables set by environment profiles, not here

  # log_debug("Runtime configuration applied to global variables", .context = "CONFIG_APPLY")
  return(invisible(TRUE))
}

# HELPER FUNCTIONS FOR CONFIGURATION LOGIC ===================================

#' Get Override Value or Default
#'
#' @description
#' Utility function to get override value fra options eller return default.
#'
#' @param override_options Override options list
#' @param key Configuration key
#' @param default Default value
#' @return Override value or default
get_override_or_default <- function(override_options, key, default) {
  if (!is.null(override_options[[key]])) {
    return(override_options[[key]])
  }

  # Check golem-style options
  golem_option <- get_app_option(key, NULL)
  if (!is.null(golem_option)) {
    return(golem_option)
  }

  return(default)
}

#' Determine Auto Restore Setting
#'
#' @description
#' Environment-aware auto restore configuration.
#'
#' @param override_options Override options
#' @return Boolean for auto restore setting
determine_auto_restore_setting <- function(override_options) {
  # Check explicit override first
  explicit_setting <- get_override_or_default(override_options, "auto_restore", NULL)
  if (!is.null(explicit_setting)) {
    return(explicit_setting)
  }

  # Environment-based defaults
  if (exists("is_prod_mode", mode = "function") && is_prod_mode()) {
    return(TRUE)   # Enable in production for user convenience
  } else if (exists("is_dev_mode", mode = "function") && is_dev_mode()) {
    return(FALSE)  # Disable in development for clean testing
  } else {
    return(FALSE)  # Safe default
  }
}

#' Determine Test Mode Setting
#'
#' @description
#' Environment-aware test mode configuration.
#'
#' @param override_options Override options
#' @return Boolean for test mode setting
determine_test_mode_setting <- function(override_options) {
  # Check explicit override first
  explicit_setting <- get_override_or_default(override_options, "test_mode", NULL)
  if (!is.null(explicit_setting)) {
    return(explicit_setting)
  }

  # Check explicit environment variable
  env_var <- Sys.getenv("TEST_MODE_AUTO_LOAD", "")
  if (env_var != "") {
    return(as.logical(env_var))
  }

  # Use existing detect_environment() function if available
  if (exists("detect_environment", mode = "function")) {
    return(detect_environment())
  }

  # Fallback to environment-based logic
  if (exists("is_prod_mode", mode = "function") && is_prod_mode()) {
    return(FALSE)  # Safe default for production
  } else if (exists("is_dev_mode", mode = "function") && is_dev_mode()) {
    return(TRUE)   # Convenient default for development
  } else {
    return(FALSE)  # Safe default for unknown contexts
  }
}

#' Determine Debug Mode Setting
#'
#' @description
#' Environment-aware debug mode configuration.
#'
#' @param override_options Override options
#' @return Boolean for debug mode setting
determine_debug_mode_setting <- function(override_options) {
  # Check explicit override first
  explicit_setting <- get_override_or_default(override_options, "debug_mode", NULL)
  if (!is.null(explicit_setting)) {
    return(explicit_setting)
  }

  # Check environment variable
  env_debug <- Sys.getenv("SHINY_DEBUG_MODE", "FALSE")
  if (env_debug == "TRUE") {
    return(TRUE)
  }

  # Environment-based defaults
  if (exists("is_dev_mode", mode = "function")) {
    return(is_dev_mode())  # Enable in development by default
  } else {
    return(interactive())  # Fallback to interactive session detection
  }
}

#' Determine Log Level
#'
#' @description
#' Environment-aware log level determination.
#'
#' @param override_options Override options
#' @return Log level string
determine_log_level <- function(override_options) {
  # Check explicit override first
  explicit_level <- get_override_or_default(override_options, "log_level", NULL)
  if (!is.null(explicit_level)) {
    return(explicit_level)
  }

  # Check environment variable
  env_level <- Sys.getenv("SPC_LOG_LEVEL", "")
  if (env_level != "") {
    return(env_level)
  }

  # Environment-based defaults
  if (exists("is_prod_mode", mode = "function") && is_prod_mode()) {
    return("ERROR")  # Minimal logging in production
  } else if (exists("is_dev_mode", mode = "function") && is_dev_mode()) {
    return("DEBUG")  # Verbose logging in development
  } else {
    return("INFO")   # Moderate logging for unknown contexts
  }
}

#' Determine Environment Type
#'
#' @description
#' Determine textual environment type fra boolean flags.
#'
#' @param env_config Environment configuration
#' @return String indicating environment type
determine_environment_type <- function(env_config) {
  if (env_config$is_production) {
    return("production")
  } else if (env_config$is_development) {
    return("development")
  } else if (env_config$is_testing) {
    return("testing")
  } else {
    return("unknown")
  }
}

#' Determine Environment Type from Context
#'
#' @description
#' Determine environment type using available context og override options.
#'
#' @param override_options Override options
#' @return String indicating environment type
determine_environment_type_from_context <- function(override_options = list()) {
  # Check explicit override first
  if (!is.null(override_options$environment_type)) {
    return(override_options$environment_type)
  }

  # Check existing environment configuration if available
  if (exists("runtime_config") && !is.null(runtime_config$environment)) {
    return(runtime_config$environment$environment_type)
  }

  # Use existing detection logic
  env_config <- setup_environment_features(override_options)
  return(determine_environment_type(env_config))
}

#' Convert Environment Profile to Legacy Config Format
#'
#' @description
#' Convert environment profile til legacy configuration format for backward compatibility.
#'
#' @param profile Environment profile from get_environment_profile()
#' @return Configuration in legacy format
convert_profile_to_legacy_config <- function(profile) {
  # log_debug("Converting environment profile to legacy config format", .context = "CONFIG_CONVERT")

  config <- list(
    # Development configuration
    development = list(
      auto_restore_enabled = profile$session$auto_restore_session,
      default_port = 3838,  # Default value
      hot_reload_enabled = profile$ui$enable_hot_reload,
      enhanced_debugging = profile$logging$enable_debug_mode
    ),

    # Environment features
    environment = list(
      type = profile$environment$type,
      is_development = profile$environment$is_development,
      is_production = profile$environment$is_production,
      is_testing = profile$environment$is_testing,
      environment_type = profile$environment$type
    ),

    # Logging configuration
    logging = list(
      debug_mode_enabled = profile$logging$enable_debug_mode,
      log_level = profile$logging$level,
      enhanced_debugging = profile$logging$enable_debug_mode,
      reactive_logging = profile$logging$log_reactive_contexts
    ),

    # Testing configuration
    testing = list(
      auto_load_enabled = profile$testing$auto_load_test_data,
      test_file_path = profile$testing$test_data_file,
      alternative_files = list(
        infection_data = "R/data/test_infection.csv",
        example_data_1 = "R/data/spc_exampledata1.csv",
        excel_data = "R/data/SPC_test_data_forskellige.xlsx"
      )
    ),

    # Performance configuration
    performance = list(
      thresholds = list(
        reactive_warning = if (profile$environment$is_production) 1.0 else 0.3
      ),
      timeouts = list(),
      debounce = list(
        input_change = profile$performance$debounce_input_ms
      )
    )
  )

  # log_debug("Environment profile converted to legacy format", .context = "CONFIG_CONVERT")
  return(config)
}