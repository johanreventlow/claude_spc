# environment_profiles.R
# Environment-specific configuration profiles
# Fase 4.2: DEV/PROD/TEST specific settings with security awareness

#' Get Environment-Specific Configuration Profile
#'
#' @description
#' Returnerer en komplet konfigurationsprofil baseret på deployment environment.
#' Sikrer korrekte og sikre defaults for hvert miljø.
#'
#' @param environment_type Environment type ("development", "production", "testing", "unknown")
#' @param override_options Optional overrides for specific settings
#' @return List with environment-specific configuration
#' @export
get_environment_profile <- function(environment_type = NULL, override_options = list()) {
  # Auto-detect environment if not specified
  if (is.null(environment_type)) {
    if (exists("runtime_config") && !is.null(runtime_config$environment$environment_type)) {
      environment_type <- runtime_config$environment$environment_type
    } else {
      # Use smart environment detection
      environment_type <- detect_environment_from_context()
    }
  }

  log_debug(paste("Loading configuration profile for environment:", environment_type), "ENV_PROFILES")

  # Get base profile
  profile <- switch(environment_type,
    "development" = get_development_profile(),
    "production" = get_production_profile(),
    "testing" = get_testing_profile(),
    "unknown" = get_safe_default_profile()
  )

  # Apply any overrides
  if (length(override_options) > 0) {
    profile <- merge_configuration_overrides(profile, override_options)
  }

  log_debug(paste("✅ Environment profile loaded:", environment_type), "ENV_PROFILES")
  return(profile)
}

#' Development Environment Profile
#'
#' @description
#' Development-optimeret konfiguration med enhanced debugging og convenience features.
#' Optimeret for developer productivity.
#'
#' @return List with development configuration
get_development_profile <- function() {
  log_debug("Loading development environment profile", "DEV_PROFILE")

  profile <- list(
    # Environment identification
    environment = list(
      type = "development",
      is_development = TRUE,
      is_production = FALSE,
      is_testing = FALSE
    ),

    # Security settings (relaxed for development)
    security = list(
      require_https = FALSE,
      allow_debug_endpoints = TRUE,
      session_timeout_minutes = 480,  # 8 hours for long development sessions
      csrf_protection = FALSE,
      content_security_policy = "permissive"
    ),

    # Performance settings (optimized for fast feedback)
    performance = list(
      enable_caching = TRUE,
      cache_timeout_seconds = 60,     # Short cache for rapid iteration
      debounce_input_ms = 200,        # Fast response
      max_file_size_mb = 100,         # Generous limits for development
      enable_profiling = TRUE,
      log_slow_operations = TRUE,
      performance_monitoring = TRUE
    ),

    # Logging and debugging (verbose for troubleshooting)
    logging = list(
      level = "DEBUG",
      enable_debug_mode = TRUE,
      log_reactive_contexts = TRUE,
      log_performance_metrics = TRUE,
      console_logging = TRUE,
      file_logging = FALSE,           # Console is fine for development
      capture_warnings = TRUE,
      capture_errors = TRUE
    ),

    # Testing and data (convenient defaults)
    testing = list(
      auto_load_test_data = TRUE,
      test_data_file = "R/data/spc_exampledata.csv",
      enable_test_endpoints = TRUE,
      mock_external_services = FALSE,
      validate_test_data = TRUE
    ),

    # UI and UX (enhanced for development)
    ui = list(
      enable_hot_reload = FALSE,      # Not implemented yet
      show_debug_panel = TRUE,
      enable_dev_tools = TRUE,
      browser_launch = "rstudio_viewer",
      show_performance_metrics = TRUE
    ),

    # Session management (convenient for development)
    session = list(
      auto_restore_session = FALSE,   # Clean slate for testing
      save_session_on_exit = FALSE,
      enable_session_debugging = TRUE,
      cleanup_on_disconnect = TRUE
    )
  )

  log_debug("✅ Development profile configured", "DEV_PROFILE")
  return(profile)
}

#' Production Environment Profile
#'
#' @description
#' Production-optimeret konfiguration med sikkerhed og stabilitet i fokus.
#' Conservative defaults og robust error handling.
#'
#' @return List with production configuration
get_production_profile <- function() {
  log_debug("Loading production environment profile", "PROD_PROFILE")

  profile <- list(
    # Environment identification
    environment = list(
      type = "production",
      is_development = FALSE,
      is_production = TRUE,
      is_testing = FALSE
    ),

    # Security settings (strict for production)
    security = list(
      require_https = TRUE,
      allow_debug_endpoints = FALSE,
      session_timeout_minutes = 60,    # 1 hour for security
      csrf_protection = TRUE,
      content_security_policy = "strict",
      hide_error_details = TRUE
    ),

    # Performance settings (optimized for stability)
    performance = list(
      enable_caching = TRUE,
      cache_timeout_seconds = 1800,    # 30 minutes cache
      debounce_input_ms = 500,         # More conservative for stability
      max_file_size_mb = 50,           # Reasonable limits
      enable_profiling = FALSE,        # Disable for performance
      log_slow_operations = TRUE,
      performance_monitoring = FALSE   # Minimal overhead
    ),

    # Logging and debugging (minimal for production)
    logging = list(
      level = "ERROR",
      enable_debug_mode = FALSE,
      log_reactive_contexts = FALSE,
      log_performance_metrics = FALSE,
      console_logging = FALSE,
      file_logging = TRUE,             # Log to files in production
      capture_warnings = TRUE,
      capture_errors = TRUE
    ),

    # Testing and data (disabled in production)
    testing = list(
      auto_load_test_data = FALSE,     # NEVER auto-load in production
      test_data_file = NULL,
      enable_test_endpoints = FALSE,
      mock_external_services = FALSE,
      validate_test_data = FALSE
    ),

    # UI and UX (minimal for production)
    ui = list(
      enable_hot_reload = FALSE,
      show_debug_panel = FALSE,
      enable_dev_tools = FALSE,
      browser_launch = "browser",
      show_performance_metrics = FALSE
    ),

    # Session management (user-friendly for production)
    session = list(
      auto_restore_session = TRUE,     # User convenience
      save_session_on_exit = TRUE,
      enable_session_debugging = FALSE,
      cleanup_on_disconnect = FALSE    # Preserve user work
    )
  )

  log_debug("✅ Production profile configured", "PROD_PROFILE")
  return(profile)
}

#' Testing Environment Profile
#'
#' @description
#' Testing-optimeret konfiguration med controlled conditions og reproducibility.
#' Balancerer debugging capabilities med test isolation.
#'
#' @return List with testing configuration
get_testing_profile <- function() {
  log_debug("Loading testing environment profile", "TEST_PROFILE")

  profile <- list(
    # Environment identification
    environment = list(
      type = "testing",
      is_development = FALSE,
      is_production = FALSE,
      is_testing = TRUE
    ),

    # Security settings (moderate for testing)
    security = list(
      require_https = FALSE,
      allow_debug_endpoints = TRUE,
      session_timeout_minutes = 120,   # 2 hours for test sessions
      csrf_protection = FALSE,         # Simplified for testing
      content_security_policy = "moderate"
    ),

    # Performance settings (optimized for test reliability)
    performance = list(
      enable_caching = FALSE,          # Disable for test consistency
      cache_timeout_seconds = 0,
      debounce_input_ms = 100,         # Fast for automated tests
      max_file_size_mb = 25,           # Reasonable test limits
      enable_profiling = TRUE,         # Good for test analysis
      log_slow_operations = TRUE,
      performance_monitoring = TRUE
    ),

    # Logging and debugging (verbose for test troubleshooting)
    logging = list(
      level = "INFO",
      enable_debug_mode = TRUE,
      log_reactive_contexts = TRUE,
      log_performance_metrics = TRUE,
      console_logging = TRUE,
      file_logging = FALSE,           # Console logging for tests
      capture_warnings = TRUE,
      capture_errors = TRUE
    ),

    # Testing and data (controlled test environment)
    testing = list(
      auto_load_test_data = FALSE,     # Explicit control in tests
      test_data_file = "R/data/spc_exampledata.csv",
      enable_test_endpoints = TRUE,
      mock_external_services = TRUE,   # Isolated testing
      validate_test_data = TRUE
    ),

    # UI and UX (minimal for testing)
    ui = list(
      enable_hot_reload = FALSE,
      show_debug_panel = TRUE,
      enable_dev_tools = TRUE,
      browser_launch = FALSE,          # No browser for automated tests
      show_performance_metrics = TRUE
    ),

    # Session management (clean for testing)
    session = list(
      auto_restore_session = FALSE,    # Clean state for each test
      save_session_on_exit = FALSE,
      enable_session_debugging = TRUE,
      cleanup_on_disconnect = TRUE     # Clean up after tests
    )
  )

  log_debug("✅ Testing profile configured", "TEST_PROFILE")
  return(profile)
}

#' Safe Default Environment Profile
#'
#' @description
#' Conservative fallback configuration for unknown environments.
#' Prioriterer sikkerhed og stabilitet over convenience.
#'
#' @return List with safe default configuration
get_safe_default_profile <- function() {
  log_debug("Loading safe default environment profile", "SAFE_PROFILE")

  profile <- list(
    # Environment identification
    environment = list(
      type = "unknown",
      is_development = FALSE,
      is_production = FALSE,
      is_testing = FALSE
    ),

    # Security settings (strict by default)
    security = list(
      require_https = FALSE,           # Can't assume HTTPS availability
      allow_debug_endpoints = FALSE,
      session_timeout_minutes = 30,    # Conservative timeout
      csrf_protection = TRUE,
      content_security_policy = "strict"
    ),

    # Performance settings (conservative defaults)
    performance = list(
      enable_caching = TRUE,
      cache_timeout_seconds = 600,     # 10 minutes
      debounce_input_ms = 800,         # Conservative debouncing
      max_file_size_mb = 25,           # Safe limits
      enable_profiling = FALSE,
      log_slow_operations = TRUE,
      performance_monitoring = FALSE
    ),

    # Logging and debugging (moderate logging)
    logging = list(
      level = "WARN",
      enable_debug_mode = FALSE,
      log_reactive_contexts = FALSE,
      log_performance_metrics = FALSE,
      console_logging = TRUE,
      file_logging = FALSE,
      capture_warnings = TRUE,
      capture_errors = TRUE
    ),

    # Testing and data (basic test support for unknown environment)
    testing = list(
      auto_load_test_data = FALSE,     # Safe default
      test_data_file = "R/data/spc_exampledata.csv",  # Enable basic test support
      enable_test_endpoints = FALSE,
      mock_external_services = FALSE,
      validate_test_data = FALSE
    ),

    # UI and UX (basic functionality)
    ui = list(
      enable_hot_reload = FALSE,
      show_debug_panel = FALSE,
      enable_dev_tools = FALSE,
      browser_launch = "browser",
      show_performance_metrics = FALSE
    ),

    # Session management (conservative)
    session = list(
      auto_restore_session = FALSE,    # Don't assume user wants this
      save_session_on_exit = FALSE,
      enable_session_debugging = FALSE,
      cleanup_on_disconnect = TRUE
    )
  )

  log_debug("✅ Safe default profile configured", "SAFE_PROFILE")
  return(profile)
}

#' Merge Configuration Overrides
#'
#' @description
#' Merge user-provided overrides med environment profile.
#' Allows for runtime customization af environment-specific settings.
#'
#' @param base_profile Base environment profile
#' @param overrides User-provided override values
#' @return Merged configuration profile
merge_configuration_overrides <- function(base_profile, overrides) {
  log_debug("Merging configuration overrides", "CONFIG_MERGE")

  # Use modifyList for deep merging
  merged_profile <- utils::modifyList(base_profile, overrides, keep.null = TRUE)

  log_debug(paste("✅ Configuration overrides merged:", length(overrides), "overrides applied"), "CONFIG_MERGE")
  return(merged_profile)
}

#' Apply Environment Profile to Runtime
#'
#' @description
#' Anvend environment profile til runtime configuration og global variables.
#' Bridges mellem environment profiles og eksisterende configuration system.
#'
#' @param profile Environment profile from get_environment_profile()
#' @return Invisible TRUE
#' @export
apply_environment_profile <- function(profile) {
  log_debug(paste("Applying environment profile:", profile$environment$type), "PROFILE_APPLY")

  # Apply logging configuration
  if (!is.null(profile$logging$level)) {
    Sys.setenv(SPC_LOG_LEVEL = profile$logging$level)
  }

  if (!is.null(profile$logging$enable_debug_mode)) {
    Sys.setenv(SHINY_DEBUG_MODE = if (profile$logging$enable_debug_mode) "TRUE" else "FALSE")
  }

  # Apply session configuration
  if (!is.null(profile$session$auto_restore_session)) {
    assign("AUTO_RESTORE_ENABLED", profile$session$auto_restore_session, envir = .GlobalEnv)
  }

  # Apply testing configuration
  if (!is.null(profile$testing$auto_load_test_data)) {
    assign("TEST_MODE_AUTO_LOAD", profile$testing$auto_load_test_data, envir = .GlobalEnv)
  }

  if (!is.null(profile$testing$test_data_file)) {
    assign("TEST_MODE_FILE_PATH", profile$testing$test_data_file, envir = .GlobalEnv)
  }

  # Apply debugging configuration
  if (!is.null(profile$logging$enable_debug_mode)) {
    assign("SHINY_DEBUG_MODE", profile$logging$enable_debug_mode, envir = .GlobalEnv)
  }

  log_debug("✅ Environment profile applied to runtime", "PROFILE_APPLY")
  return(invisible(TRUE))
}

#' Get Current Environment Profile Summary
#'
#' @description
#' Get a human-readable summary af den aktuelle environment profile.
#'
#' @return Character vector with profile summary
#' @export
get_environment_summary <- function() {
  if (!exists("runtime_config") || is.null(runtime_config$environment)) {
    return("Environment configuration not initialized")
  }

  env_type <- runtime_config$environment$environment_type
  profile <- get_environment_profile(env_type)

  summary_lines <- c(
    paste("Environment Type:", profile$environment$type),
    paste("Security Level:", if (profile$security$csrf_protection) "High" else "Standard"),
    paste("Logging Level:", profile$logging$level),
    paste("Debug Mode:", if (profile$logging$enable_debug_mode) "Enabled" else "Disabled"),
    paste("Test Data Auto-Load:", if (profile$testing$auto_load_test_data) "Enabled" else "Disabled"),
    paste("Session Auto-Restore:", if (profile$session$auto_restore_session) "Enabled" else "Disabled"),
    paste("Performance Monitoring:", if (profile$performance$performance_monitoring) "Enabled" else "Disabled")
  )

  return(summary_lines)
}

#' Detect Environment from Context
#'
#' @description
#' Smart environment detection baseret på system context og environment variables.
#' Bruges som fallback når explicit environment type ikke er angivet.
#'
#' @return String indicating detected environment type
detect_environment_from_context <- function() {
  log_debug("Detecting environment from context", "ENV_DETECTION")

  # Check explicit environment variables first
  env_var <- Sys.getenv("R_CONFIG_ACTIVE", "")
  if (env_var != "") {
    log_debug(paste("R_CONFIG_ACTIVE detected:", env_var), "ENV_DETECTION")
    # Map common config names to our environment types
    mapped_env <- switch(env_var,
      "development" = "development",
      "dev" = "development",
      "production" = "production",
      "prod" = "production",
      "testing" = "testing",
      "test" = "testing",
      "unknown"  # Default for unrecognized values
    )
    return(mapped_env)
  }

  # Check debug mode indicators
  debug_mode <- Sys.getenv("SHINY_DEBUG_MODE", "FALSE")
  test_mode <- Sys.getenv("TEST_MODE_AUTO_LOAD", "")

  if (debug_mode == "TRUE" || test_mode == "TRUE") {
    log_debug("Debug/test mode indicators detected - environment: development", "ENV_DETECTION")
    return("development")
  }

  # Check if running interactively (development indicator)
  if (interactive()) {
    log_debug("Interactive session detected - environment: development", "ENV_DETECTION")
    return("development")
  }

  # Check for common production indicators
  if (nzchar(Sys.getenv("SHINY_PORT")) || nzchar(Sys.getenv("PORT"))) {
    log_debug("Production port indicators detected - environment: production", "ENV_DETECTION")
    return("production")
  }

  # Check for testing indicators
  if (any(c("testthat", "test") %in% search()) || exists(".Random.seed") && exists("test_that")) {
    log_debug("Testing framework detected - environment: testing", "ENV_DETECTION")
    return("testing")
  }

  # Default fallback
  log_debug("No specific environment indicators - defaulting to unknown", "ENV_DETECTION")
  return("unknown")
}