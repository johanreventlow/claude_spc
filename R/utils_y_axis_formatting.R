# utils_y_axis_formatting.R
# Y-Axis Formatting Utilities for SPC Plots
#
# FASE 4 Task 4.2: Y-axis formatting extraction (Week 11-12)
# Consolidates Y-axis formatting logic from fct_spc_plot_generation.R
# Priority 1: LOW RISK, HIGH VALUE refactoring
#
# Benefits:
# - Reduces generateSPCPlot() by ~60 lines
# - DRY principle: Consolidates duplicated time formatting (~30 lines)
# - Easier to unit test formatting logic independently
# - Potential reuse in other plotting functions

# Y-AXIS FORMATTING MAIN FUNCTION ============================================

#' Apply Y-Axis Formatting to SPC Plot
#'
#' Applies unit-specific Y-axis formatting to an SPC ggplot object.
#' This function consolidates all Y-axis formatting logic for different
#' unit types (percent, count, rate, time).
#'
#' @param plot ggplot object to which formatting will be applied
#' @param y_axis_unit Character string indicating unit type:
#'   - "percent": Percentage values (0-100%)
#'   - "count": Count values with intelligent K/M/mia. notation
#'   - "rate": Rate values with decimal formatting
#'   - "time": Time values (minutes/hours/days)
#' @param qic_data Data frame with qic data (used for time range calculation)
#'
#' @return Modified ggplot object with appropriate y-axis scale
#'
#' @details
#' This function replaces inline Y-axis formatting from generateSPCPlot()
#' (lines 1159-1285). It provides:
#' - Consistent expansion (mult = c(.25, .25)) across all unit types
#' - Danish number formatting (decimal.mark = ",", big.mark = ".")
#' - Intelligent scaling (K for thousands, M for millions, etc.)
#' - Context-aware time formatting (minutes/hours/days)
#'
#' @examples
#' \dontrun{
#' plot <- ggplot(qic_data, aes(x = x, y = y)) +
#'   geom_point()
#' plot <- apply_y_axis_formatting(plot, "percent", qic_data)
#' }
#'
#' @export
apply_y_axis_formatting <- function(plot, y_axis_unit = "count", qic_data = NULL) {
  # Validate inputs
  if (!inherits(plot, "ggplot")) {
    log_warn("apply_y_axis_formatting: plot is not a ggplot object", .context = "Y_AXIS_FORMAT")
    return(plot)
  }

  if (is.null(y_axis_unit) || !is.character(y_axis_unit)) {
    log_warn("apply_y_axis_formatting: invalid y_axis_unit, defaulting to 'count'", .context = "Y_AXIS_FORMAT")
    y_axis_unit <- "count"
  }

  # Apply unit-specific formatting
  if (y_axis_unit == "percent") {
    return(plot + format_y_axis_percent())
  } else if (y_axis_unit == "count") {
    return(plot + format_y_axis_count())
  } else if (y_axis_unit == "rate") {
    return(plot + format_y_axis_rate())
  } else if (y_axis_unit == "time") {
    return(plot + format_y_axis_time(qic_data))
  }

  # Default: no special formatting (use ggplot2 defaults)
  return(plot)
}

# UNIT-SPECIFIC FORMATTING FUNCTIONS =========================================

#' Format Y-Axis for Percentage Data
#'
#' @return ggplot2 scale_y_continuous layer for percentage formatting
#' @keywords internal
format_y_axis_percent <- function() {
  # Percent formatting with % suffix
  # Data from qic is in decimal form (0.9 for 90%), scale = 100 converts to percentage
  # Danish formatting: decimal.mark = "," (85,5 %), big.mark = "." (not used for %)
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(.25, .25)),
    labels = scales::label_percent()
  )
}

#' Format Y-Axis for Count Data with Intelligent K/M Notation
#'
#' @return ggplot2 scale_y_continuous layer for count formatting
#' @keywords internal
format_y_axis_count <- function() {
  # Count formatting with intelligent K/M notation
  # K starts at 1.000+ for correct notation (K = 1.000, not 10.000)
  # Trade-off: loses thousand separator for 1.000-9.999 range
  # Only shows decimals if present (50K vs 50,5K)

  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(.25, .25)),
    labels = function(x) {
      # TIDYVERSE: purrr::map_chr + dplyr::case_when replaces sapply + nested if-else
      x |>
        purrr::map_chr(~ {
          dplyr::case_when(
            is.na(.x) ~ NA_character_,
            abs(.x) >= 1e9 ~ format_scaled_number(.x, 1e9, " mia."),
            abs(.x) >= 1e6 ~ format_scaled_number(.x, 1e6, "M"),
            abs(.x) >= 1e3 ~ format_scaled_number(.x, 1e3, "K"),
            TRUE ~ format_unscaled_number(.x)
          )
        })
    }
  )
}

#' Format Y-Axis for Rate Data
#'
#' @return ggplot2 scale_y_continuous layer for rate formatting
#' @keywords internal
format_y_axis_rate <- function() {
  # Rate formatting (only shows decimals if present)
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(.25, .25)),
    labels = function(x) {
      ifelse(x == round(x),
        format(round(x), decimal.mark = ","),
        format(x, decimal.mark = ",", nsmall = 1)
      )
    }
  )
}

#' Format Y-Axis for Time Data (Minutes/Hours/Days)
#'
#' @param qic_data Data frame with qic data containing y column (time values in minutes)
#'
#' @return ggplot2 scale_y_continuous layer for time formatting
#' @keywords internal
format_y_axis_time <- function(qic_data) {
  # Intelligent time formatting based on data range (input: minutes)
  # Only shows decimals if present

  if (is.null(qic_data) || !"y" %in% names(qic_data)) {
    log_warn("format_y_axis_time: missing qic_data or y column, using default formatting", .context = "Y_AXIS_FORMAT")
    return(ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(.25, .25))))
  }

  y_range <- range(qic_data$y, na.rm = TRUE)
  max_minutes <- max(y_range, na.rm = TRUE)

  # Determine appropriate time unit based on max value
  if (max_minutes < 60) {
    time_unit <- "minutes"
  } else if (max_minutes < 1440) {
    time_unit <- "hours"
  } else {
    time_unit <- "days"
  }

  # Return scale with consolidated formatting function
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(.25, .25)),
    labels = function(x) {
      sapply(x, function(val) format_time_with_unit(val, time_unit))
    }
  )
}

# HELPER FUNCTIONS ===========================================================

#' Format Scaled Number with Suffix (K, M, mia.)
#'
#' Helper function for count formatting with intelligent scaling.
#' Only shows decimals if the scaled value has decimal places.
#'
#' @param val Numeric value to format
#' @param scale Scale factor (1e3 for K, 1e6 for M, 1e9 for mia.)
#' @param suffix Suffix string ("K", "M", " mia.")
#'
#' @return Formatted string with Danish decimal notation
#' @keywords internal
format_scaled_number <- function(val, scale, suffix) {
  scaled <- val / scale
  if (scaled == round(scaled)) {
    paste0(round(scaled), suffix)
  } else {
    paste0(format(scaled, decimal.mark = ",", nsmall = 1), suffix)
  }
}

#' Format Unscaled Number with Danish Notation
#'
#' Helper function for count formatting without scaling.
#' Uses Danish thousand separator (.) and decimal mark (,).
#'
#' @param val Numeric value to format
#'
#' @return Formatted string with Danish number notation
#' @keywords internal
format_unscaled_number <- function(val) {
  if (val == round(val)) {
    format(round(val), big.mark = ".", decimal.mark = ",")
  } else {
    format(val, big.mark = ".", decimal.mark = ",", nsmall = 1)
  }
}

#' Format Time Value with Unit (CONSOLIDATES DUPLICATION)
#'
#' Consolidated time formatting function that replaces 3 duplicated
#' code blocks (minutes/hours/days) from generateSPCPlot().
#'
#' This function reduces ~30 lines of duplicated formatting logic
#' to a single reusable function following DRY principles.
#'
#' @param val_minutes Numeric time value in minutes
#' @param time_unit Character string: "minutes", "hours", or "days"
#'
#' @return Formatted string with appropriate time unit (e.g., "30 min", "1,5 timer", "2 dage")
#' @keywords internal
format_time_with_unit <- function(val_minutes, time_unit) {
  # Handle NA values
  if (is.na(val_minutes)) {
    return(NA_character_)
  }

  # Scale value to appropriate unit
  scaled <- switch(time_unit,
    minutes = val_minutes,
    hours = val_minutes / 60,
    days = val_minutes / 1440,
    val_minutes # fallback
  )

  # Get Danish unit label
  unit_label <- switch(time_unit,
    minutes = " min",
    hours = " timer",
    days = " dage",
    " min" # fallback
  )

  # Format with or without decimals (only show if present)
  if (scaled == round(scaled)) {
    paste0(round(scaled), unit_label)
  } else {
    paste0(format(scaled, decimal.mark = ",", nsmall = 1), unit_label)
  }
}
