# PACKAGED GLOBAL CONFIGURATION ================================================
# New packaged version of global.R - replaces source() chain with package loading

# PACKAGE LOADING --------------------------------
# Load the claudespc package which contains all functionality
library(claudespc)

# BACKWARD COMPATIBILITY --------------------------------
# Ensure critical functions are available in global environment for existing code

# Import key functions that may be called directly from global scope
if (!exists("create_app_state")) {
  create_app_state <- claudespc:::create_app_state
}

if (!exists("create_emit_api")) {
  create_emit_api <- claudespc:::create_emit_api
}

if (!exists("safe_operation")) {
  safe_operation <- claudespc:::safe_operation
}

if (!exists("log_debug")) {
  log_debug <- claudespc:::log_debug
}

if (!exists("log_info")) {
  log_info <- claudespc:::log_info
}

if (!exists("log_warn")) {
  log_warn <- claudespc:::log_warn
}

if (!exists("log_error")) {
  log_error <- claudespc:::log_error
}

# UI COMPONENTS --------------------------------
# Import UI functions from subdirectories using ::: notation
if (!exists("create_ui_sidebar")) {
  create_ui_sidebar <- claudespc:::create_ui_sidebar
}

if (!exists("create_ui_header")) {
  create_ui_header <- claudespc:::create_ui_header
}

if (!exists("create_ui_main_content")) {
  create_ui_main_content <- claudespc:::create_ui_main_content
}

if (!exists("create_welcome_page")) {
  create_welcome_page <- claudespc:::create_welcome_page
}

# RUNTIME CONFIGURATION --------------------------------
# Initialize runtime configuration (now handled by package .onLoad)
# But we may need to call it explicitly in some contexts
if (exists("initialize_runtime_config", mode = "function")) {
  runtime_config <- initialize_runtime_config()
} else {
  # Fallback basic config
  runtime_config <- list(
    logging = list(debug_mode_enabled = FALSE),
    testing = list(auto_load_enabled = FALSE)
  )
}

# ENVIRONMENT DETECTION --------------------------------
# Simplified environment detection (main logic moved to package)
detect_environment <- function() {
  env_var <- Sys.getenv("TEST_MODE_AUTO_LOAD", "FALSE")
  if (env_var != "") {
    normalized_env_var <- tolower(trimws(env_var))
    true_values <- c("true", "t", "1", "yes", "y", "on")
    return(normalized_env_var %in% true_values)
  }
  return(FALSE)
}

# LEGACY FUNCTION COMPATIBILITY --------------------------------
# Keep essential legacy functions for backward compatibility

# Observer manager (simplified version)
observer_manager <- function() {
  observers <- list()
  list(
    add = function(observer, name = NULL) {
      id <- if (is.null(name)) length(observers) + 1 else name
      observers[[id]] <<- observer
      return(id)
    },
    remove = function(id) {
      if (id %in% names(observers)) {
        if (!is.null(observers[[id]]$destroy)) {
          observers[[id]]$destroy()
        }
        observers[[id]] <<- NULL
      }
    },
    cleanup_all = function() {
      for (id in names(observers)) {
        if (!is.null(observers[[id]]$destroy)) {
          tryCatch(
            observers[[id]]$destroy(),
            error = function(e) {
              if (exists("log_error")) {
                log_error(paste("Observer cleanup fejl for", id, ":", e$message), "OBSERVER_MGMT")
              }
            }
          )
        }
      }
      observers <<- list()
    },
    count = function() length(observers)
  )
}

# Debounced reactive helper
create_debounced_reactive <- function(reactive_expr, millis = 1000) {
  return(shiny::debounce(reactive_expr, millis = millis))
}

# DEVELOPMENT LOG LEVEL
if (!nzchar(Sys.getenv("SPC_LOG_LEVEL", ""))) {
  Sys.setenv(SPC_LOG_LEVEL = "INFO")
}

# PACKAGE INITIALIZATION STATUS --------------------------------
message("✓ claudespc package loaded successfully")
message("✓ Global configuration initialized")
if (exists("HOSPITAL_NAME") && exists("my_theme")) {
  message("✓ UI components available")
} else {
  warning("⚠ Some UI components may not be initialized - package may need reinstalling")
}