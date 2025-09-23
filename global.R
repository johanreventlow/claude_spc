# PACKAGE-BASED GLOBAL CONFIGURATION ================================================
# Replaces source()-based loading with package-based approach

# DEVELOPMENT MODE LOADING --------------------------------
# Always use source-based loading for development
# NOTE: Package self-loading (library(claudespc)) creates circular dependency and should be avoided
message("Loading development configuration...")

# Load core functions via sourcing in dependency order
source_files <- c(
  # Core utilities first
  "R/utils_logging.R",
  "R/utils_error_handling.R",
  "R/utils_performance.R",
  "R/golem_utils.R",
  "R/zzz.R",

  # Configuration
  "R/config_system_config.R",
  "R/config_hospital_branding.R",
  "R/config_observer_priorities.R",
  "R/config_chart_types.R",
  "R/app_config.R",
  "R/app_runtime_config.R",

  # App initialization
  "R/app_dependencies.R",
  "R/app_initialization.R"
)

for (file in source_files) {
  if (file.exists(file)) {
    source(file, local = FALSE)
  }
}

message("✓ claudespc package loaded")
message("✓ Global configuration loaded successfully")

# BACKWARD COMPATIBILITY LAYER --------------------------------
# Essential functions for backward compatibility - now provided by package

# Enhanced debugging utilities - simplified for package use
get_debug_mode_status <- function() {
  # Simplified version that works with package loading
  if (exists("runtime_config") && !is.null(runtime_config$logging$debug_mode_enabled)) {
    return(runtime_config$logging$debug_mode_enabled)
  }
  env_debug <- Sys.getenv("SHINY_DEBUG_MODE", "FALSE")
  return(env_debug == "TRUE")
}

# Legacy environment detection
detect_environment <- function() {
  env_var <- Sys.getenv("TEST_MODE_AUTO_LOAD", "FALSE")
  if (env_var != "") {
    normalized_env_var <- tolower(trimws(env_var))
    true_values <- c("true", "t", "1", "yes", "y", "on")
    return(normalized_env_var %in% true_values)
  }
  return(FALSE)
}

# RUNTIME CONFIGURATION --------------------------------
# Initialize runtime configuration - now handled by package loading
if (exists("initialize_runtime_config", mode = "function")) {
  runtime_config <- initialize_runtime_config()
} else {
  # Fallback basic config
  runtime_config <- list(
    logging = list(debug_mode_enabled = FALSE),
    testing = list(auto_load_enabled = FALSE)
  )
}

# DEVELOPMENT LOG LEVEL
if (!nzchar(Sys.getenv("SPC_LOG_LEVEL", ""))) {
  Sys.setenv(SPC_LOG_LEVEL = "INFO")
}

# PACKAGE INITIALIZATION COMPLETE --------------------------------
message("✓ Global configuration loaded successfully")
if (exists("HOSPITAL_NAME")) {
  message(paste("✓ Hospital branding loaded:", HOSPITAL_NAME))
} else {
  message("⚠ Hospital branding not available - check package installation")
}
