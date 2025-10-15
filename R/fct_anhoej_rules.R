# fct_anhoej_rules.R
# Anhøj Rules Detection and Metadata Extraction
#
# Dette modul implementerer funktioner til at arbejde med Anhøj rules i SPC charts.
# BFHchart pakken beregner allerede Anhøj rules (runs test og crossings test),
# så disse funktioner ekstraherer og standardiserer output fra BFHchart.
#
# Anhøj Rules:
# 1. Runs test: ≥8 consecutive points above or below center line
# 2. Crossings test: Too few median crossings (statistical threshold)
#
# Design princip: BFHchart er primary source, vi standardiserer kun output

#' Extract Anhøj Rules Metadata from BFHchart Output
#'
#' Extracts standardized Anhøj rules metadata from BFHchart qic_data.
#' BFHchart already computes Anhøj rules (runs and crossings tests), so this
#' function simply extracts and formats the results into a consistent structure
#' matching qicharts2 conventions.
#'
#' @details
#' **BFHchart Anhøj Columns:**
#' - `runs.signal`: Logical indicating if runs test triggered (≥8 consecutive points)
#' - `longest.run`: Length of longest run above or below center line
#' - `longest.run.max`: Maximum acceptable run length
#' - `n.crossings`: Actual number of median crossings observed
#' - `n.crossings.min`: Minimum expected crossings (statistical threshold)
#' - `anhoej.signal`: Combined signal (TRUE if runs OR crossings violation)
#'
#' **Output Standardization:**
#' This function extracts per-point signals and aggregates to overall metadata
#' matching the baseline fixture structure from Task #29.
#'
#' **Per-Phase Handling:**
#' If data contains multiple phases (part column), Anhøj rules are applied
#' per-phase by BFHchart. This function preserves per-point signals and
#' returns overall status.
#'
#' @param qic_data data.frame or tibble. BFHchart qic_data with Anhøj columns.
#'   Must contain at least: `runs.signal`, `n.crossings`, `n.crossings.min`.
#'   Optional: `longest.run`, `longest.run.max`, `anhoej.signal`.
#'
#' @return list with Anhøj rules metadata:
#'   \describe{
#'     \item{runs_signal}{logical. TRUE if any runs violation detected.}
#'     \item{crossings_signal}{logical. TRUE if crossings violation detected.}
#'     \item{n_crossings}{numeric. Actual number of crossings (first value if per-phase).}
#'     \item{n_crossings_min}{numeric. Minimum expected crossings (first value if per-phase).}
#'     \item{longest_run}{integer. Length of longest run (if available).}
#'     \item{longest_run_max}{numeric. Maximum acceptable run (if available).}
#'     \item{signal_points}{logical vector. Per-point combined Anhøj signal.}
#'   }
#'   Returns NULL if required columns missing (with warning).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract from BFHchart result
#' bfh_result <- BFHcharts::create_spc_chart(data, x, y, chart_type = "run")
#' qic_data <- bfh_result$data
#' anhoej_meta <- extract_anhoej_metadata(qic_data)
#'
#' # Check if rules triggered
#' if (anhoej_meta$runs_signal) {
#'   message("Runs violation detected!")
#' }
#' if (anhoej_meta$crossings_signal) {
#'   message("Too few crossings detected!")
#' }
#'
#' # Access per-point signals
#' signal_indices <- which(anhoej_meta$signal_points)
#' }
#'
#' @seealso
#' \code{\link{validate_anhoej_columns}} for input validation
#' \code{\link{compute_spc_results_bfh}} for integration in service layer
extract_anhoej_metadata <- function(qic_data) {
  # 1. Validate input
  if (is.null(qic_data) || !is.data.frame(qic_data)) {
    warning("qic_data must be a data.frame or tibble")
    return(NULL)
  }

  # 2. Check for required Anhøj columns
  required_cols <- c("runs.signal", "n.crossings", "n.crossings.min")
  missing_cols <- setdiff(required_cols, names(qic_data))

  if (length(missing_cols) > 0) {
    warning(paste(
      "Missing required Anhøj columns:",
      paste(missing_cols, collapse = ", ")
    ))
    return(NULL)
  }

  # 3. Extract runs signal (aggregate: TRUE if any point signals)
  runs_signal <- any(qic_data$runs.signal, na.rm = TRUE)

  # 4. Extract crossings information
  # Note: BFHchart returns per-phase crossings, we take first value as overall
  # (consistent with qicharts2 baseline structure)
  n_crossings <- qic_data$n.crossings[1]
  n_crossings_min <- qic_data$n.crossings.min[1]

  # Crossings signal: TRUE if n.crossings < n.crossings.min
  crossings_signal <- !is.na(n_crossings) && !is.na(n_crossings_min) &&
    n_crossings < n_crossings_min

  # 5. Extract longest run information (if available)
  longest_run <- if ("longest.run" %in% names(qic_data)) {
    qic_data$longest.run[1]
  } else {
    NA_integer_
  }

  longest_run_max <- if ("longest.run.max" %in% names(qic_data)) {
    qic_data$longest.run.max[1]
  } else {
    NA_real_
  }

  # 6. Extract per-point combined signal
  # Use anhoej.signal if available, otherwise compute from components
  if ("anhoej.signal" %in% names(qic_data)) {
    signal_points <- qic_data$anhoej.signal
  } else {
    # Fallback: combine runs and crossings signals
    signal_points <- qic_data$runs.signal
  }

  # Ensure logical type and handle NAs
  signal_points <- as.logical(signal_points)
  signal_points[is.na(signal_points)] <- FALSE

  # 7. Build metadata list matching baseline structure
  metadata <- list(
    runs_signal = runs_signal,
    crossings_signal = crossings_signal,
    n_crossings = n_crossings,
    n_crossings_min = n_crossings_min,
    longest_run = longest_run,
    longest_run_max = longest_run_max,
    signal_points = signal_points
  )

  return(metadata)
}


#' Validate Anhøj Columns in QIC Data
#'
#' Internal helper to validate that required Anhøj columns are present in
#' qic_data from BFHchart. Used for defensive programming and early error detection.
#'
#' @param qic_data data.frame. BFHchart qic_data to validate.
#' @param require_signal logical. If TRUE, require `anhoej.signal` column. Default FALSE.
#'
#' @return logical. TRUE if all required columns present, FALSE otherwise.
#'   Also issues warnings for missing columns.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' if (!validate_anhoej_columns(qic_data)) {
#'   stop("Invalid qic_data: missing Anhøj columns")
#' }
#' }
validate_anhoej_columns <- function(qic_data, require_signal = FALSE) {
  if (is.null(qic_data) || !is.data.frame(qic_data)) {
    warning("qic_data must be a data.frame")
    return(FALSE)
  }

  # Required columns for Anhøj metadata extraction
  required_cols <- c("runs.signal", "n.crossings", "n.crossings.min")

  if (require_signal) {
    required_cols <- c(required_cols, "anhoej.signal")
  }

  missing_cols <- setdiff(required_cols, names(qic_data))

  if (length(missing_cols) > 0) {
    warning(paste(
      "Missing Anhøj columns:",
      paste(missing_cols, collapse = ", ")
    ))
    return(FALSE)
  }

  return(TRUE)
}


#' Calculate Combined Anhøj Signal from Components
#'
#' Computes combined Anhøj signal (runs OR crossings) from individual components.
#' This is a fallback function used when BFHchart doesn't provide `anhoej.signal`
#' column directly.
#'
#' @details
#' **Signal Logic:**
#' - Per-point signal: TRUE if runs.signal is TRUE at that point
#' - Overall signal: TRUE if runs_signal OR crossings_signal
#'
#' **Crossings Signal:**
#' Crossings test is chart-wide, not per-point. If n.crossings < n.crossings.min,
#' all points are considered to have a crossings signal (but typically only runs
#' are highlighted per-point).
#'
#' @param qic_data data.frame. BFHchart qic_data with runs and crossings columns.
#'
#' @return logical vector. Per-point combined Anhøj signal.
#'   Returns vector of FALSE if validation fails.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' signal <- calculate_combined_anhoej_signal(qic_data)
#' qic_data$signal <- signal
#' }
calculate_combined_anhoej_signal <- function(qic_data) {
  # Validate input
  if (!validate_anhoej_columns(qic_data, require_signal = FALSE)) {
    # Return FALSE vector if validation fails
    return(rep(FALSE, nrow(qic_data)))
  }

  # Start with runs.signal
  signal <- qic_data$runs.signal

  # Check crossings
  n_crossings <- qic_data$n.crossings[1]
  n_crossings_min <- qic_data$n.crossings.min[1]

  crossings_signal <- !is.na(n_crossings) && !is.na(n_crossings_min) &&
    n_crossings < n_crossings_min

  # If crossings signal triggered, could highlight all points
  # But qicharts2 only highlights runs violations per-point
  # So we keep signal = runs.signal for per-point display

  # Ensure logical type and handle NAs
  signal <- as.logical(signal)
  signal[is.na(signal)] <- FALSE

  return(signal)
}


#' Format Anhøj Metadata for Display
#'
#' Formats Anhøj rules metadata into human-readable Danish text for logging
#' or UI display. Useful for debugging and user notifications.
#'
#' @param anhoej_meta list. Anhøj metadata from `extract_anhoej_metadata()`.
#'
#' @return character. Formatted Danish text describing Anhøj rule violations.
#'   Example: "Runs: Ja (8 punkter), Crossings: Nej (9/13)"
#'   Returns "Ingen Anhøj violations" if no signals detected.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' anhoej_meta <- extract_anhoej_metadata(qic_data)
#' message(format_anhoej_metadata(anhoej_meta))
#' # Output: "Runs: Ja (8 punkter), Crossings: Nej (9/13)"
#' }
format_anhoej_metadata <- function(anhoej_meta) {
  if (is.null(anhoej_meta)) {
    return("Anhøj metadata ikke tilgængelig")
  }

  # Runs text
  runs_text <- if (anhoej_meta$runs_signal) {
    if (!is.na(anhoej_meta$longest_run)) {
      sprintf("Ja (%d punkter)", anhoej_meta$longest_run)
    } else {
      "Ja"
    }
  } else {
    "Nej"
  }

  # Crossings text
  crossings_text <- if (anhoej_meta$crossings_signal) {
    sprintf("Ja (%d/%d)", anhoej_meta$n_crossings, anhoej_meta$n_crossings_min)
  } else {
    sprintf("Nej (%d/%d)", anhoej_meta$n_crossings, anhoej_meta$n_crossings_min)
  }

  # Overall signal
  if (!anhoej_meta$runs_signal && !anhoej_meta$crossings_signal) {
    return("Ingen Anhøj violations")
  }

  # Combine
  result <- sprintf("Runs: %s, Crossings: %s", runs_text, crossings_text)

  return(result)
}
