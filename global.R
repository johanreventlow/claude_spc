# PACKAGE-BASED GLOBAL CONFIGURATION ================================================
# Replaces source()-based loading with package-based approach

# DEVELOPMENT MODE LOADING --------------------------------
# Always use source-based loading for development
# NOTE: Package self-loading creates circular dependency and should be avoided
# Clear any existing SPCify package conflicts first
if (exists("SPCify") && "package:SPCify" %in% search()) {
  detach("package:SPCify", unload = TRUE, force = TRUE)
}

message("Loading development configuration...")

# Load core functions via sourcing in dependency order
source_files <- c(
  # Core utilities first
  "R/utils_logging.R",
  "R/utils_error_handling.R",
  "R/utils_performance.R",
  "R/utils_performance_monitoring.R",
  "R/utils_dependency_injection.R",
  "R/utils_advanced_debug.R",
  "R/utils_shinylogs_config.R",
  "R/utils_memory_management.R",
  "R/utils_danish_locale.R",
  "R/golem_utils.R",
  "R/zzz.R",

  # Configuration
  "R/config_system_config.R",
  "R/config_hospital_branding.R",
  "R/config_branding_getters.R",
  "R/config_observer_priorities.R",
  "R/config_chart_types.R",
  "R/config_spc_config.R",
  "R/app_config.R",
  "R/app_runtime_config.R",
  "R/state_management.R",

  # App initialization
  "R/app_dependencies.R",
  "R/app_initialization.R",
  "R/app_run.R",

  # UI components
  "R/config_ui_config.R",
  "R/ui_utils_ui_components.R",
  "R/ui_utils_ui_updates.R",
  "R/ui_app_ui.R",

  # Shiny modules
  "R/modules_mod_spc_chart_ui.R",
  "R/modules_mod_spc_chart_server.R",

  # Server components
  "R/server_observer_manager.R",
  "R/server_utils_server_management.R",
  "R/app_server_main.R",
  "R/server_utils_session_helpers.R",
  "R/server_utils_event_system.R",
  "R/fct_visualization_server.R"
)

for (file in source_files) {
  if (file.exists(file)) {
    source(file, local = FALSE)
  }
}

message("✓ claudespc package loaded")
message("✓ Global configuration loaded successfully")

# Initialize critical global variables for development
if (exists("get_hospital_colors")) {
  HOSPITAL_COLORS <- get_hospital_colors()
  message("✓ Hospital branding loaded: ", ifelse(exists("get_hospital_name") && !is.null(get_hospital_name()), get_hospital_name(), "Default Hospital"), " SPC")
} else {
  # Fallback hospital colors
  HOSPITAL_COLORS <- list(
    primary = "#375a7f",
    secondary = "#6c757d",
    success = "#28a745",
    warning = "#ffc107",
    danger = "#dc3545",
    accent = "#FF6B35"
  )
  message("⚠ Hospital branding not available - using fallback colors")
}

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
