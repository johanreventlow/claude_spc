# utils_validation_guards.R
# Centraliserede validation guard functions for NULL checks og data validation
# Reducerer 685+ is.null() checks til genbrug patterns

#' Validate Data or Return Fallback
#'
#' Konsolideret guard for data validation med fallback.
#' Erstatter mønstret: if (!is.null(data) && nrow(data) > 0) { ... }
#'
#' @param data Data object to validate
#' @param fallback Value to return if validation fails (default: NULL)
#' @param min_rows Minimum required rows (default: 1)
#' @param min_cols Minimum required columns (default: 1)
#' @return Data if valid, otherwise fallback
#' @export
#'
#' @examples
#' # Basic usage
#' validated_data <- validate_data_or_return(input_data, fallback = data.frame())
#'
#' # Custom thresholds
#' validated_data <- validate_data_or_return(input_data, min_rows = 10, min_cols = 3)
validate_data_or_return <- function(data,
                                    fallback = NULL,
                                    min_rows = 1,
                                    min_cols = 1) {
  if (is.null(data)) {
    return(fallback)
  }

  if (!is.data.frame(data) && !inherits(data, "tbl_df")) {
    return(fallback)
  }

  if (nrow(data) < min_rows || ncol(data) < min_cols) {
    return(fallback)
  }

  return(data)
}

#' Value or Default (NULL Coalescing)
#'
#' Konsolideret guard for NULL coalescing med type checking.
#' Erstatter mønstret: value %||% default eller if (!is.null(value)) value else default
#'
#' @param value Value to check
#' @param default Default value if NULL (default: "")
#' @param allowed_types Character vector of allowed types (optional)
#' @return Value if not NULL and valid type, otherwise default
#' @export
#'
#' @examples
#' # Basic usage
#' safe_value <- value_or_default(input$field, default = "")
#'
#' # With type checking
#' safe_numeric <- value_or_default(input$number, default = 0, allowed_types = "numeric")
value_or_default <- function(value,
                             default = "",
                             allowed_types = NULL) {
  if (is.null(value)) {
    return(default)
  }

  # Empty character check
  if (is.character(value) && length(value) == 1 && trimws(value) == "") {
    return(default)
  }

  # Empty vector check
  if (length(value) == 0 || identical(value, character(0))) {
    return(default)
  }

  # Type validation if specified
  if (!is.null(allowed_types)) {
    value_type <- class(value)[1]
    if (!value_type %in% allowed_types) {
      return(default)
    }
  }

  return(value)
}

#' Validate Column Exists
#'
#' Konsolideret guard for kolonne validering.
#' Erstatter mønstret: if (!is.null(col) && col %in% names(data)) { ... }
#'
#' @param data Data frame to check
#' @param column_name Column name to validate
#' @param return_column If TRUE, return column data; if FALSE, return logical (default: FALSE)
#' @param fallback Value to return if validation fails (default: NULL for column data, FALSE for logical)
#' @return Column data, logical, or fallback depending on parameters
#' @export
#'
#' @examples
#' # Check if column exists
#' has_column <- validate_column_exists(data, "Dato")
#'
#' # Get column data if exists
#' column_data <- validate_column_exists(data, "Dato", return_column = TRUE, fallback = numeric(0))
validate_column_exists <- function(data,
                                   column_name,
                                   return_column = FALSE,
                                   fallback = NULL) {
  # Input validation
  if (is.null(data) || is.null(column_name)) {
    return(if (return_column) fallback else FALSE)
  }

  if (!is.data.frame(data) && !inherits(data, "tbl_df")) {
    return(if (return_column) fallback else FALSE)
  }

  # Empty string check
  if (is.character(column_name) && trimws(column_name) == "") {
    return(if (return_column) fallback else FALSE)
  }

  # Column existence check
  column_exists <- column_name %in% names(data)

  if (return_column) {
    return(if (column_exists) data[[column_name]] else fallback)
  } else {
    return(column_exists)
  }
}

#' Validate Function Exists
#'
#' Konsolideret guard for function existence check.
#' Erstatter mønstret: if (exists("func_name", mode = "function") && !is.null(func_name)) { ... }
#'
#' @param func_name Function name as string
#' @param envir Environment to search (default: .GlobalEnv)
#' @return Logical indicating if function exists and is callable
#' @export
#'
#' @examples
#' if (validate_function_exists("emit$data_loaded")) {
#'   emit$data_loaded()
#' }
validate_function_exists <- function(func_name, envir = .GlobalEnv) {
  if (is.null(func_name) || !is.character(func_name) || trimws(func_name) == "") {
    return(FALSE)
  }

  return(exists(func_name, mode = "function", envir = envir))
}

#' Validate Config Value
#'
#' Konsolideret guard for config value extraction med validation.
#' Erstatter mønstret: if (!is.null(config$field) && config$field != "") { ... }
#'
#' @param config Configuration object (list or environment)
#' @param field Field name to extract
#' @param default Default value if validation fails
#' @param allow_empty If FALSE, treats empty strings as invalid (default: FALSE)
#' @return Config value if valid, otherwise default
#' @export
#'
#' @examples
#' x_col <- validate_config_value(config, "x_col", default = "Dato")
#' y_unit <- validate_config_value(config, "y_axis_unit", default = "count")
validate_config_value <- function(config,
                                  field,
                                  default = NULL,
                                  allow_empty = FALSE) {
  # Input validation
  if (is.null(config) || is.null(field)) {
    return(default)
  }

  # Extract value
  value <- if (is.environment(config)) {
    config[[field]]
  } else if (is.list(config)) {
    config[[field]]
  } else {
    return(default)
  }

  # NULL check
  if (is.null(value)) {
    return(default)
  }

  # Empty check for character values
  if (!allow_empty && is.character(value) && trimws(value) == "") {
    return(default)
  }

  # Empty vector check
  if (length(value) == 0 || identical(value, character(0))) {
    return(default)
  }

  return(value)
}

#' Validate Reactive Value
#'
#' Konsolideret guard for reactive value extraction med safety.
#' Erstatter mønstret: shiny::isolate(app_state$section$field) %||% default
#'
#' @param reactive_value Reactive value or reactiveValues object
#' @param field Field name (optional for single reactive values)
#' @param default Default value if NULL or error
#' @param use_isolate If TRUE, wraps in shiny::isolate() (default: TRUE)
#' @return Reactive value if accessible, otherwise default
#' @export
#'
#' @examples
#' # Single reactive value
#' current_data <- validate_reactive_value(app_state$data$current_data, default = NULL)
#'
#' # Nested field
#' frozen_state <- validate_reactive_value(
#'   app_state$columns$auto_detect,
#'   field = "frozen_until_next_trigger",
#'   default = FALSE
#' )
validate_reactive_value <- function(reactive_value,
                                    field = NULL,
                                    default = NULL,
                                    use_isolate = TRUE) {
  safe_operation(
    operation_name = "Validate reactive value",
    code = {
      # Extract value with optional isolation
      value <- if (use_isolate) {
        shiny::isolate({
          if (!is.null(field)) {
            reactive_value[[field]]
          } else {
            reactive_value
          }
        })
      } else {
        if (!is.null(field)) {
          reactive_value[[field]]
        } else {
          reactive_value
        }
      }

      # Return value or default
      if (is.null(value)) default else value
    },
    fallback = default,
    error_type = "reactive_validation"
  )
}

#' Validate State Transition
#'
#' Konsolideret guard for state transition validation med logging.
#' Erstatter scattered state checks med centraliseret validation.
#'
#' @param app_state Application state object
#' @param checks Named list of state checks (each should return logical)
#' @param operation_name Operation name for logging
#' @param allow_proceed If TRUE, proceeds even if checks fail (default: FALSE)
#' @return List with valid (logical) and failed_checks (character vector)
#' @export
#'
#' @examples
#' validation <- validate_state_transition(
#'   app_state,
#'   checks = list(
#'     not_updating = !app_state$data$updating_table,
#'     has_data = !is.null(app_state$data$current_data),
#'     auto_detect_ready = !app_state$columns$auto_detect$in_progress
#'   ),
#'   operation_name = "Start auto-detection"
#' )
#'
#' if (validation$valid) {
#'   # Proceed with operation
#' }
validate_state_transition <- function(app_state,
                                      checks,
                                      operation_name = "State transition",
                                      allow_proceed = FALSE) {
  failed_checks <- character(0)

  # Evaluate each check
  for (check_name in names(checks)) {
    check_result <- tryCatch(
      {
        isTRUE(checks[[check_name]])
      },
      error = function(e) {
        log_warn(
          paste("State check failed:", check_name, "-", e$message),
          "STATE_VALIDATION"
        )
        FALSE
      }
    )

    if (!check_result) {
      failed_checks <- c(failed_checks, check_name)
    }
  }

  # Log if checks failed
  if (length(failed_checks) > 0) {
    log_debug_kv(
      operation = operation_name,
      failed_checks = paste(failed_checks, collapse = ", "),
      allow_proceed = allow_proceed,
      .context = "STATE_VALIDATION"
    )
  }

  return(list(
    valid = length(failed_checks) == 0 || allow_proceed,
    failed_checks = failed_checks
  ))
}

# NULL coalescing operator (%||%) er allerede defineret i utils_logging.R
# Denne fil udvider med mere specifik validation logic
