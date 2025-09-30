# R/utils_qic_preparation.R
# QICHarts2 Input Preparation System
# Prevents 100×-mismatch by ensuring consistent input preparation for qicharts2

# QIC INPUT PREPARATION =======================================================

#' Prepare consistent inputs for qicharts2::qic() calls
#'
#' This function eliminates 100×-mismatch by ensuring all inputs to qicharts2
#' are prepared consistently, regardless of chart type or user input format.
#'
#' Key principle: Either use counts+n OR use proportions[0,1], never mix both.
#'
#' @param y_raw Raw Y data (could be counts, rates, proportions)
#' @param n_raw Raw N data (denominators, NULL for non-ratio charts)
#' @param chart_type Character. QIC chart type (e.g., "p", "run", "c")
#' @param user_unit Character. User's explicit unit choice
#' @return List with prepared qic inputs and normalization function
#' @export
prepare_qic_inputs <- function(y_raw, n_raw = NULL, chart_type, user_unit = NULL) {

  internal_unit <- determine_internal_unit_by_chart_type(chart_type)

  log_debug("Preparing QIC inputs for chart type:", chart_type, "internal unit:", internal_unit, .context = "QIC_PREPARATION")

  # Create normalization function for target/centerline values
  normalize_fn <- function(x) {
    normalize_axis_value(
      x = x,
      user_unit = user_unit,
      chart_type = chart_type  # This will determine internal_unit automatically
    )
  }

  # Prepare Y data based on chart type and internal unit
  if (internal_unit == "proportion") {

    if (!is.null(n_raw) && chart_type %in% c("p", "pp")) {
      # P-charts with denominators: Use counts + n (let qicharts2 calculate proportions)
      y_prepared <- y_raw  # Keep as counts
      n_prepared <- n_raw  # Keep denominators
      log_debug("P-chart with denominators: using counts+n approach", .context = "QIC_PREPARATION")

    } else {
      # Run charts with rates, or P-charts without denominators: Convert to [0,1]
      y_sample <- parse_danish_number(y_raw)
      y_prepared <- normalize_proportions_to_internal(y_raw, y_sample, user_unit)
      n_prepared <- NULL  # No denominators
      log_debug("Proportion chart without denominators: converting to [0,1]", .context = "QIC_PREPARATION")
    }

  } else if (internal_unit == "absolute") {
    # Absolute charts (c, u, i, mr, g): Use raw values, no scaling
    y_prepared <- parse_danish_number(y_raw)  # Parse but don't scale
    n_prepared <- n_raw  # Keep denominators if any
    log_debug("Absolute chart: using raw values without scaling", .context = "QIC_PREPARATION")

  } else {
    log_error(paste("Unknown internal unit:", internal_unit), "QIC_PREPARATION")
    stop("Unknown internal unit: ", internal_unit)
  }

  return(list(
    y = y_prepared,
    n = n_prepared,
    normalize = normalize_fn,
    internal_unit = internal_unit,
    chart_type = chart_type
  ))
}

#' Normalize proportions to internal [0,1] scale
#'
#' Helper function to convert proportion-like data to consistent [0,1] scale
#'
#' @param y_raw Raw Y data
#' @param y_sample Parsed numeric sample for heuristics
#' @param user_unit User's unit preference
#' @return Numeric vector in [0,1] scale
#' @keywords internal
normalize_proportions_to_internal <- function(y_raw, y_sample, user_unit) {

  # Determine if data is already in [0,1] or needs conversion
  scale_type <- detect_unit_from_data(y_sample)

  if (scale_type == "proportion") {
    # Already in [0,1], parse and return
    return(parse_danish_number(y_raw))

  } else if (scale_type == "percent") {
    # Convert from percent scale to [0,1]
    parsed_values <- parse_danish_number(y_raw)
    return(parsed_values / 100)

  } else {
    # Unknown scale - use user unit to guide conversion
    if (!is.null(user_unit) && user_unit == "percent") {
      parsed_values <- parse_danish_number(y_raw)
      return(parsed_values / 100)
    } else {
      # Default: assume already in correct scale
      log_warn(paste("Unknown data scale, assuming correct internal scale"), "QIC_PREPARATION")
      return(parse_danish_number(y_raw))
    }
  }
}

# DISPLAY SCALING HELPERS =====================================================

# QIC CALL WRAPPER ============================================================

#' Create complete qicharts2::qic() call with consistent inputs
#'
#' This is the main entry point that prevents 100×-mismatch by ensuring
#' all inputs are prepared consistently before calling qicharts2::qic().
#'
#' @param data Data.frame with the data
#' @param x_col Character. X column name (usually dates)
#' @param y_col Character. Y column name
#' @param n_col Character. N column name (optional)
#' @param chart_type Character. QIC chart type
#' @param user_unit Character. User's unit preference
#' @param target_input Character. Target value input from UI
#' @param centerline_input Character. Centerline value input from UI
#' @param title Character. Plot title
#' @param ... Additional arguments passed to qicharts2::qic()
#' @return qicharts2 plot object
#' @export
create_qic_plot_safe <- function(data, x_col, y_col, n_col = NULL, chart_type,
                                 user_unit = NULL, target_input = NULL,
                                 centerline_input = NULL, title = NULL, ...) {

  # Extract data
  x_data <- data[[x_col]]
  y_data <- data[[y_col]]
  n_data <- if (!is.null(n_col) && n_col %in% names(data)) data[[n_col]] else NULL

  # Prepare all inputs consistently
  qic_inputs <- prepare_qic_inputs(y_data, n_data, chart_type, user_unit)

  # Prepare target and centerline values
  target_value <- if (!is.null(target_input)) qic_inputs$normalize(target_input) else NULL
  centerline_value <- if (!is.null(centerline_input)) qic_inputs$normalize(centerline_input) else NULL

  # Build qic arguments
  qic_args <- list(
    x = x_data,
    y = qic_inputs$y,
    chart = chart_type,
    title = title %||% paste("SPC Chart:", toupper(chart_type))
  )

  # Add n if available
  if (!is.null(qic_inputs$n)) {
    qic_args$n <- qic_inputs$n
  }

  # Add target and centerline
  if (!is.null(target_value)) {
    qic_args$target <- target_value
  }
  if (!is.null(centerline_value)) {
    qic_args$cl <- centerline_value
  }

  # Add additional arguments
  extra_args <- list(...)
  qic_args <- c(qic_args, extra_args)

  # Log the prepared inputs for debugging
  log_debug("QIC call prepared:", "chart_type:", chart_type, "internal_unit:", qic_inputs$internal_unit,
           "target:", target_value, "centerline:", centerline_value, .context = "QIC_PREPARATION")

  # PERFORMANCE MONITORING: Track actual qic() function calls
  if (!exists("actual_qic_call_counter", envir = .GlobalEnv)) {
    assign("actual_qic_call_counter", 0, envir = .GlobalEnv)
  }
  assign("actual_qic_call_counter", get("actual_qic_call_counter", envir = .GlobalEnv) + 1, envir = .GlobalEnv)

  current_time <- Sys.time()
  qic_call_number <- get("actual_qic_call_counter", envir = .GlobalEnv)
  log_debug(paste("ACTUAL qic() CALL #", qic_call_number, "at", format(current_time, "%H:%M:%S.%OS3")), "QIC_CALL_DEBUG")

  # Enhanced tracking - track this QIC call with context
  if (exists("track_qic_call", mode = "function")) {
    safe_operation("Track QIC call", {
      track_qic_call(
        context = "create_qic_plot_safe",
        details = list(
          chart_type = chart_type,
          has_target = !is.null(target_value),
          has_centerline = !is.null(centerline_value),
          internal_unit = qic_inputs$internal_unit,
          call_number = qic_call_number
        )
      )
    }, fallback = NULL)
  }

  # Call qicharts2::qic() with prepared inputs and debug logging
  log_qic_call_wrapper(qic_args, "create_qic_plot_safe", qic_call_number)
}

# VALIDATION HELPERS ==========================================================

#' Validate qic inputs for consistency
#'
#' Performs sanity checks to catch 100×-mismatch before it reaches qicharts2
#'
#' @param y Prepared Y data
#' @param n Prepared N data (optional)
#' @param target Prepared target value (optional)
#' @param centerline Prepared centerline value (optional)
#' @param internal_unit Internal unit being used
#' @return List with validation results and warnings
#' @keywords internal
validate_qic_inputs <- function(y, n = NULL, target = NULL, centerline = NULL, internal_unit) {

  warnings <- character(0)
  valid <- TRUE

  if (internal_unit == "proportion") {
    # Check Y data is in [0,1] range
    if (!is.null(y) && (any(y < 0, na.rm = TRUE) || any(y > 1, na.rm = TRUE))) {
      if (is.null(n)) {  # Only warn if not using counts+n
        warnings <- c(warnings, paste("Y data outside [0,1] range for proportion chart"))
        valid <- FALSE
      }
    }

    # Check target and centerline are in [0,1] range
    if (!is.null(target) && (target < 0 || target > 1)) {
      warnings <- c(warnings, paste("Target value", target, "outside [0,1] range"))
    }
    if (!is.null(centerline) && (centerline < 0 || centerline > 1)) {
      warnings <- c(warnings, paste("Centerline value", centerline, "outside [0,1] range"))
    }
  }

  return(list(
    valid = valid,
    warnings = warnings
  ))
}
