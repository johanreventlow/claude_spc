# R/utils_y_axis_scaling.R
# Robust Y-axis scaling system with separated concerns
# Implements the 3-layer architecture: Parsing → Unit Clarification → Conversion

# CORE CONSTANTS ==============================================================

#' Valid unit types for Y-axis scaling
#' @keywords internal
VALID_UNITS <- c("proportion", "percent", "permille", "absolute")

#' Internal canonical units by plot type
#' @keywords internal
INTERNAL_UNITS_BY_PLOTTYPE <- list(
  proportions = "proportion",  # p-charts, run charts for rates [0,1]
  absolute = "absolute"        # c/u-charts, count run charts
)

# LAYER 1: PARSING ============================================================

#' Parse Danish number with symbol detection
#'
#' Pure parsing function that extracts numeric value and symbol without scaling.
#' Idempotent - calling multiple times returns same result.
#'
#' @param x Character string to parse (e.g., "80%", "8‰", "0,5")
#' @return List with elements:
#'   - value: Numeric value as written (80% → 80, not 0.8)
#'   - symbol: One of "percent", "permille", "none"
#' @export
parse_number_da <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(list(value = numeric(0), symbol = character(0)))
  }

  if (length(x) > 1) {
    results <- purrr::map(x, parse_number_da)
    return(list(
      value = purrr::map_dbl(results, "value"),
      symbol = purrr::map_chr(results, "symbol")
    ))
  }

  # Convert to character and trim
  x <- trimws(as.character(x))

  if (is.na(x) || x == "") {
    return(list(value = NA_real_, symbol = "none"))
  }

  # Detect symbols
  has_percent <- grepl("%", x)
  has_permille <- grepl("‰", x)

  # Invalid: both symbols present
  if (has_percent && has_permille) {
    return(list(value = NA_real_, symbol = "invalid"))
  }

  # Determine symbol type
  symbol <- if (has_percent) "percent" else if (has_permille) "permille" else "none"

  # Clean number: remove symbols, spaces, handle comma decimal
  x_clean <- gsub("[%‰\\s]", "", x)
  x_normalized <- gsub(",", ".", x_clean)

  # Parse numeric value
  numeric_value <- suppressWarnings(as.numeric(x_normalized))

  return(list(
    value = numeric_value,
    symbol = symbol
  ))
}

# LAYER 2: UNIT CLARIFICATION ================================================

#' Resolve target unit for Y-axis using priority system
#'
#' Determines which unit to work in based on explicit priorities.
#' 1. User-specified unit (explicit choice)
#' 2. Column metadata unit (if available)
#' 3. Data heuristics (with visible decision logging)
#'
#' @param user_unit Character. Explicit unit choice from UI
#' @param col_unit Character. Unit from column metadata (optional)
#' @param y_sample Numeric vector. Sample of Y data for heuristics
#' @return Character. One of "proportion", "percent", "permille", "absolute"
#' @export
resolve_y_unit <- function(user_unit = NULL, col_unit = NULL, y_sample = NULL) {

  # Priority 1: User explicit choice
  if (!is.null(user_unit) && user_unit %in% VALID_UNITS) {
    log_debug("Y-axis unit resolved via user choice:", user_unit, .context = "Y_AXIS_SCALING")
    return(user_unit)
  }

  # Priority 2: Column metadata
  if (!is.null(col_unit) && col_unit %in% VALID_UNITS) {
    log_debug("Y-axis unit resolved via column metadata:", col_unit, .context = "Y_AXIS_SCALING")
    return(col_unit)
  }

  # Priority 3: Data heuristics
  if (!is.null(y_sample) && length(y_sample) > 0) {
    heuristic_unit <- detect_unit_from_data(y_sample)
    log_debug("Y-axis unit resolved via data heuristics:", heuristic_unit, "n_observations:", length(y_sample), .context = "Y_AXIS_SCALING")
    return(heuristic_unit)
  }

  # Fallback: absolute
  log_debug("Y-axis unit fallback to absolute", .context = "Y_AXIS_SCALING")
  return("absolute")
}

#' Detect unit from data using clear heuristics
#' @param y_data Numeric vector
#' @return Character unit type
#' @keywords internal
detect_unit_from_data <- function(y_data) {
  if (is.null(y_data) || length(y_data) == 0) return("absolute")

  y_clean <- y_data[!is.na(y_data)]
  if (length(y_clean) == 0) return("absolute")

  max_val <- max(y_clean)
  min_val <- min(y_clean)

  # Rule 1: Proportion detection [0,1]
  if (max_val <= 1.0) {
    # Strong indicators for proportion
    has_decimals <- any(y_clean != floor(y_clean))
    values_in_range <- sum(y_clean >= 0 & y_clean <= 1) / length(y_clean)

    if (has_decimals || values_in_range >= 0.8) {
      return("proportion")
    }
  }

  # Rule 2: Percent detection [0-100+]
  if (min_val >= 0 && max_val <= 200) {
    percent_range_count <- sum(y_clean >= 0 & y_clean <= 100)
    whole_number_ratio <- sum(y_clean == floor(y_clean)) / length(y_clean)

    # Requires: ≥80% in [0-100] range AND ≥70% whole numbers
    if (percent_range_count / length(y_clean) >= 0.8 && whole_number_ratio >= 0.7) {
      return("percent")
    }
  }

  # Rule 3: Default to absolute
  return("absolute")
}

# LAYER 3A: HARMONIZATION TO TARGET UNIT ====================================

#' Convert parsed input to target unit using deterministic matrix
#'
#' @param parsed List from parse_number_da() with value and symbol
#' @param target_unit Character. Target unit to convert to
#' @return Numeric value in target unit
#' @export
coerce_to_target_unit <- function(parsed, target_unit) {

  if (is.na(parsed$value) || parsed$symbol == "invalid") {
    return(NA_real_)
  }

  value <- parsed$value
  symbol <- parsed$symbol

  # Conversion matrix: from symbol × to target_unit
  if (target_unit == "proportion") {

    if (symbol == "percent") {
      return(value / 100)  # 80% → 0.8
    } else if (symbol == "permille") {
      return(value / 1000)  # 8‰ → 0.008
    } else if (symbol == "none") {
      # No implicit scaling - treat as already in target unit
      return(value)  # 0.8 → 0.8
    }

  } else if (target_unit == "percent") {

    if (symbol == "percent") {
      return(value)  # 80% → 80
    } else if (symbol == "permille") {
      return(value / 10)  # 80‰ → 8
    } else if (symbol == "none") {
      # No implicit scaling - treat as already in target unit
      return(value)  # 80 → 80
    }

  } else if (target_unit == "permille") {

    if (symbol == "percent") {
      return(value * 10)  # 8% → 80‰
    } else if (symbol == "permille") {
      return(value)  # 80‰ → 80
    } else if (symbol == "none") {
      # No implicit scaling - treat as already in target unit
      return(value)  # 80 → 80
    }

  } else if (target_unit == "absolute") {

    # For absolute units, remove symbols but keep numeric value
    return(value)  # 80% → 80, 80‰ → 80, 80 → 80

  } else {
    # Unknown target unit
    log_error(
      paste("Unknown target unit in coerce_to_target_unit:", target_unit, "symbol:", symbol, "value:", value),
      "Y_AXIS_SCALING"
    )
    return(NA_real_)
  }
}

# LAYER 3B: CONVERSION TO INTERNAL CANONICAL ================================

#' Convert from target unit to internal canonical unit
#'
#' @param value_in_target_unit Numeric value in target unit
#' @param target_unit Character. The unit value is currently in
#' @param internal_unit Character. Internal canonical unit (e.g., "proportion")
#' @return Numeric value in internal unit
#' @export
to_internal_scale <- function(value_in_target_unit, target_unit, internal_unit) {

  if (is.na(value_in_target_unit)) return(NA_real_)

  # Same unit - no conversion needed
  if (target_unit == internal_unit) {
    return(value_in_target_unit)
  }

  # Deterministic conversion rules
  if (internal_unit == "proportion") {

    if (target_unit == "percent") {
      return(value_in_target_unit / 100)  # 80 → 0.8
    } else if (target_unit == "permille") {
      return(value_in_target_unit / 1000)  # 80 → 0.08
    } else if (target_unit == "absolute") {
      log_error(
        paste("Cannot convert absolute to proportion without context - target:", target_unit, "internal:", internal_unit),
        "Y_AXIS_SCALING"
      )
      return(NA_real_)
    }

  } else if (internal_unit == "absolute") {

    # Absolute internal - no scaling
    return(value_in_target_unit)

  } else {
    log_error(
      paste("Unknown internal unit in to_internal_scale - target:", target_unit, "internal:", internal_unit),
      "Y_AXIS_SCALING"
    )
    return(NA_real_)
  }
}

# MAIN API FUNCTION ===========================================================

#' Normalize axis value using complete 3-layer architecture
#'
#' Main entry point that combines parsing, unit resolution, and conversion.
#' This replaces parse_danish_target() with cleaner separation of concerns.
#'
#' @param x Character string input (e.g., "80%", "0,8", "25‰")
#' @param user_unit Character. Explicit user unit choice (priority 1)
#' @param col_unit Character. Column metadata unit (priority 2)
#' @param y_sample Numeric. Sample of Y data for heuristics (priority 3)
#' @param internal_unit Character. Internal canonical unit for this plot type
#' @return Numeric value in internal unit, ready for qicharts2
#' @export
normalize_axis_value <- function(x, user_unit = NULL, col_unit = NULL,
                                 y_sample = NULL, internal_unit = "proportion") {

  # Layer 1: Parse input
  parsed <- parse_number_da(x)

  if (is.na(parsed$value) || parsed$symbol == "invalid") {
    log_debug("Invalid input in normalize_axis_value:", x, "parsed_value:", parsed$value, "symbol:", parsed$symbol, .context = "Y_AXIS_SCALING")
    return(NULL)
  }

  # Layer 2: Resolve target unit
  target_unit <- resolve_y_unit(user_unit, col_unit, y_sample)

  # Layer 3A: Harmonize to target unit
  value_in_target <- coerce_to_target_unit(parsed, target_unit)

  if (is.na(value_in_target)) {
    log_debug("Failed conversion to target unit:", x, "target_unit:", target_unit, "parsed_value:", parsed$value, "symbol:", parsed$symbol, .context = "Y_AXIS_SCALING")
    return(NULL)
  }

  # Layer 3B: Convert to internal canonical
  internal_value <- to_internal_scale(value_in_target, target_unit, internal_unit)

  if (is.na(internal_value)) {
    log_debug("Failed conversion to internal unit:", x, "target_unit:", target_unit, "internal_unit:", internal_unit, .context = "Y_AXIS_SCALING")
    return(NULL)
  }

  # Validation
  validation_result <- validate_axis_value(internal_value, internal_unit)
  if (!validation_result$valid) {
    log_warn(paste("Validation warning:", validation_result$message, "input:", x, "value:", internal_value, "unit:", internal_unit), "Y_AXIS_SCALING")
  }

  log_debug("Successfully normalized axis value:", x, "->", internal_value, "parsed:", parsed$value, "symbol:", parsed$symbol, "target_unit:", target_unit, .context = "Y_AXIS_SCALING")

  return(internal_value)
}

# VALIDATION ==================================================================

#' Validate normalized axis value
#' @param value Numeric value to validate
#' @param internal_unit Character. Expected internal unit
#' @return List with valid (logical) and message (character)
#' @keywords internal
validate_axis_value <- function(value, internal_unit) {

  if (internal_unit == "proportion") {
    if (value < 0 || value > 1) {
      return(list(
        valid = FALSE,
        message = paste("Proportion value", value, "outside expected [0,1] range")
      ))
    }
  }

  # Other validations could be added here
  return(list(valid = TRUE, message = "OK"))
}

# BACKWARDS COMPATIBILITY ====================================================

#' Legacy wrapper for parse_danish_target
#'
#' Maintains backwards compatibility while transitioning to new API.
#' This function should be replaced with normalize_axis_value() calls.
#'
#' @param target_input Character. Input string
#' @param y_data Numeric vector. Y data for context
#' @param y_axis_unit Character. Unit specification
#' @return Numeric value (legacy format)
#' @export
parse_danish_target <- function(target_input, y_data = NULL, y_axis_unit = NULL) {

  # Determine internal unit based on y_data characteristics
  # For now, use proportion as default (matches most existing behavior)
  internal_unit <- if (!is.null(y_data)) {
    detected_scale <- detect_unit_from_data(y_data)
    if (detected_scale == "percent") "percent" else "proportion"
  } else {
    "proportion"
  }

  result <- normalize_axis_value(
    x = target_input,
    user_unit = y_axis_unit,
    col_unit = NULL,
    y_sample = y_data,
    internal_unit = internal_unit
  )

  # Legacy behavior expects specific return format
  if (is.null(result)) return(NULL)

  return(result)
}