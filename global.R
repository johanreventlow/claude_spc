# PACKAGE-BASED GLOBAL CONFIGURATION ================================================
# Replaces source()-based loading with package-based approach

# PACKAGE LOADING --------------------------------
# For packaged deployment, load the package instead of sourcing individual files
if (requireNamespace("claudespc", quietly = TRUE)) {
  # Package is installed - load it
  library(claudespc)
  message("✓ claudespc package loaded")
} else {
  # Development mode - fall back to original global_packaged.R approach
  warning("claudespc package not found - falling back to source-based loading")
  if (file.exists("dev/global_packaged.R")) {
    source("dev/global_packaged.R")
  } else {
    stop("Neither claudespc package nor dev/global_packaged.R found")
  }
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
