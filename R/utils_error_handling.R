# utils_error_handling.R
# Centralized error handling utilities for SPC App

#' Safe operation wrapper with error handling
#'
#' @description
#' Provides a standardized way to execute code with error handling and
#' fallback behavior. Designed to prevent crashes in reactive contexts
#' and provide graceful degradation.
#'
#' @param operation_name Character string describing the operation for logging
#' @param code Expression or code block to execute safely
#' @param fallback Default value to return if operation fails. Default is NULL.
#' @param session Shiny session object for user notifications (optional)
#' @param show_user Logical, whether to show error to user (default FALSE)
#' @param error_type Character string categorizing the error type for logging
#'
#' @return Result of code execution, or fallback value if error occurs
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' result <- safe_operation(
#'   "Data processing",
#'   code = {
#'     process_data(input_data)
#'   },
#'   fallback = empty_data()
#' )
#'
#' # With session and user notification
#' result <- safe_operation(
#'   "File upload",
#'   code = {
#'     read.csv(file_path)
#'   },
#'   fallback = NULL,
#'   session = session,
#'   show_user = TRUE
#' )
#' }
safe_operation <- function(operation_name, code, fallback = NULL, session = NULL, show_user = FALSE, error_type = "general", ...) {
  tryCatch(
    {
      force(code)
    },
    error = function(e) {
      # Basic error message construction
      error_msg <- paste(operation_name, "fejlede:", e$message)

      # Try to use structured logging if available, fallback to basic cat
      if (exists("log_error", mode = "function")) {
        tryCatch(
          {
            log_error(error_msg, paste0("ERROR_HANDLING_", toupper(error_type)))
          },
          error = function(log_err) {
            # Fallback to basic R messaging if logging fails
            cat("[ERROR]", format(Sys.time(), "%H:%M:%S"), ":", error_msg, "\n")
          })
      } else {
        # Basic fallback logging without dependencies
        cat("[ERROR]", format(Sys.time(), "%H:%M:%S"), ":", error_msg, "\n")
      }

      # User notification if session provided and requested
      if (!is.null(session) && show_user) {
        tryCatch(
          {
            shiny::showNotification(
              paste("Fejl:", operation_name),
              type = "error",
              duration = 5
            )
          },
          error = function(notification_err) {
            # Silent failure for notifications to avoid cascading errors
          })
      }

      return(fallback)
    })
}

#' Validate required objects exist before operation
#'
#' @description
#' Helper function to check that required objects/variables exist
#' before attempting operations. Prevents common "object not found" errors.
#'
#' @param ... Named arguments where names are variable names and values are environments to check
#' @param error_message Custom error message if validation fails
#'
#' @return TRUE if all objects exist, throws error otherwise
#' @export
#'
#' @examples
#' \dontrun{
#' validate_exists(
#'   data = environment(),
#'   session = environment(),
#'   error_message = "Required objects missing for data processing"
#' )
#' }
validate_exists <- function(..., error_message = "Required objects not found") {
  args <- list(...)

  for (var_name in names(args)) {
    env <- args[[var_name]]
    if (!exists(var_name, envir = env)) {
      stop(paste(error_message, "- missing:", var_name))
    }
  }

  return(TRUE)
}

#' Safe environment variable retrieval
#'
#' @description
#' Safely retrieve environment variables with fallback values
#' and type conversion.
#'
#' @param var_name Environment variable name
#' @param default Default value if variable not set
#' @param type Expected type: "character", "logical", "numeric"
#'
#' @return Environment variable value converted to specified type, or default
#' @export
#'
#' @examples
#' safe_getenv("DEBUG_MODE", FALSE, "logical")
#' safe_getenv("MAX_ROWS", 1000, "numeric")
safe_getenv <- function(var_name, default = "", type = "character") {
  value <- Sys.getenv(var_name, unset = default)

  safe_operation(
    paste("Environment variable conversion:", var_name),
    code = {
      switch(type,
        "character" = as.character(value),
        "logical" = {
          if (is.logical(default)) {
            if (value == "") return(default)
            return(as.logical(value))
          }
          as.character(value)
        },
        "numeric" = {
          if (value == "") return(default)
          as.numeric(value)
        },
        as.character(value)
      )
    },
    fallback = default,
    error_type = "configuration"
  )
}
