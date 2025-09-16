# utils_logging.R
# Konfigurerbart logging system til SPC App

# Log levels
LOG_LEVELS <- list(
  DEBUG = 1,
  INFO = 2,
  WARN = 3,
  ERROR = 4
)

# Get log level from environment or default to INFO
get_log_level <- function() {
  env_level <- Sys.getenv("SPC_LOG_LEVEL", "INFO")
  level_num <- LOG_LEVELS[[toupper(env_level)]]
  if (is.null(level_num)) {
    return(LOG_LEVELS$INFO)
  }
  return(level_num)
}

# Main logging function
log_msg <- function(message, level = "INFO", component = NULL) {
  current_level <- get_log_level()
  msg_level <- LOG_LEVELS[[toupper(level)]]

  if (is.null(msg_level) || msg_level < current_level) {
    return(invisible(NULL))
  }

  timestamp <- format(Sys.time(), "%H:%M:%S")
  component_str <- if (!is.null(component)) paste0("[", component, "] ") else ""

  cat(sprintf("[%s] %s: %s%s\n",
              timestamp,
              toupper(level),
              component_str,
              message))
}

# Convenience functions
log_debug <- function(message, component = NULL) {
  log_msg(message, "DEBUG", component)
}

log_info <- function(message, component = NULL) {
  log_msg(message, "INFO", component)
}

log_warn <- function(message, component = NULL) {
  log_msg(message, "WARN", component)
}

log_error <- function(message, component = NULL) {
  log_msg(message, "ERROR", component)
}