# utils_error_handling.R
# Centralized error handling utilities for SPC App

#' Safe operation wrapper with error handling
#'
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
#' @export
safe_operation <- function(operation_name, code, fallback = NULL, session = NULL, show_user = FALSE, error_type = "general", ...) {
  tryCatch(
    {
      force(code)
    },
    error = function(e) {
      # Basic error message construction
      error_msg <- paste(operation_name, "fejlede:", e$message)

      # Try centralized logging with multi-level fallback strategy
      logging_succeeded <- FALSE

      # LEVEL 1: Try structured log_error() if available
      if (exists("log_error", mode = "function")) {
        tryCatch(
          {
            log_error(
              message = error_msg,
              .context = paste0("ERROR_HANDLING_", toupper(error_type)),
              details = list(
                operation = operation_name,
                error_class = class(e)[1],
                error_message = e$message
              )
            )
            logging_succeeded <- TRUE
          },
          error = function(log_err) {
            # Fall through to next level if log_error fails
            logging_succeeded <<- FALSE
          }
        )
      }

      # LEVEL 2: Try basic log_msg() if log_error failed/unavailable
      if (!logging_succeeded && exists("log_msg", mode = "function")) {
        tryCatch(
          {
            log_msg(
              message = error_msg,
              level = "ERROR",
              component = paste0("ERROR_HANDLING_", toupper(error_type))
            )
            logging_succeeded <- TRUE
          },
          error = function(log_err) {
            # Fall through to final fallback
            logging_succeeded <<- FALSE
          }
        )
      }

      # LEVEL 3: Absolute fallback - minimal output only if all logging failed
      if (!logging_succeeded) {
        # Use minimal structured format matching log_msg pattern
        # This should rarely happen in production
        tryCatch(
          {
            # Use message() for last-resort fallback (standard R approach)
            message(sprintf(
              "[ERROR_HANDLING_%s] %s",
              toupper(error_type),
              error_msg
            ))
          },
          error = function(final_err) {
            # Absolute last resort - completely silent failure to avoid cascade
            # Do nothing - error already occurred, don't make it worse
          }
        )
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
          }
        )
      }

      # Handle fallback execution based on type
      if (is.function(fallback)) {
        # Call fallback function with error parameter
        return(fallback(e))
      } else {
        # Return fallback value directly
        return(fallback)
      }
    }
  )
}

#' Validate required objects exist before operation
#'
#' Helper function to check that required objects/variables exist
#' before attempting operations. Prevents common "object not found" errors.
#'
#' @param ... Named arguments where names are variable names and values are environments to check
#' @param error_message Custom error message if validation fails
#'
#' @return TRUE if all objects exist, throws error otherwise
#' @examples
#' \dontrun{
#' validate_exists(
#'   data = environment(),
#'   session = environment(),
#'   error_message = "Required objects missing for data processing"
#' )
#' }
#' @export
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
#' Safely retrieve environment variables with fallback values
#' and type conversion.
#'
#' @param var_name Environment variable name
#' @param default Default value if variable not set
#' @param type Expected type: "character", "logical", "numeric"
#'
#' @return Environment variable value converted to specified type, or default
#' @examples
#' safe_getenv("DEBUG_MODE", FALSE, "logical")
#' safe_getenv("MAX_ROWS", 1000, "numeric")
#' @export
safe_getenv <- function(var_name, default = "", type = "character") {
  value <- Sys.getenv(var_name, unset = default)

  safe_operation(
    paste("Environment variable conversion:", var_name),
    code = {
      switch(type,
        "character" = as.character(value),
        "logical" = {
          if (is.logical(default)) {
            if (value == "") {
              return(default)
            }
            return(as.logical(value))
          }
          as.character(value)
        },
        "numeric" = {
          if (value == "") {
            return(default)
          }
          as.numeric(value)
        },
        as.character(value)
      )
    },
    fallback = default,
    error_type = "configuration"
  )
}
