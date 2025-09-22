# fct_spc_helpers.R
# SPC helper functions: date formatting, validation, preprocessing
# Extracted from fct_spc_calculations.R for better maintainability

# Dependencies ----------------------------------------------------------------

# HJÆLPEFUNKTIONER ============================================================

#' Konverter enheds-kode til dansk label
#'
#' @param unit_code Character. Kode for organisatorisk enhed
#' @param unit_list Named list. Mapping mellem enheder og koder
#' @return Character. Dansk label for enheden
#' @family spc_helpers
#' @export
get_unit_label <- function(unit_code, unit_list) {
  if (is.null(unit_code) || unit_code == "") {
    return("")
  }

  # Find dansk navn baseret på værdi
  unit_names <- names(unit_list)[unit_list == unit_code]
  if (length(unit_names) > 0) {
    return(unit_names[1])
  }

  # Fallback til koden selv
  return(unit_code)
}

#' Valider og formater X-akse data til SPC charts
#'
#' Intelligent validering og formatering af X-akse data, med automatisk
#' detektion af dato formater og optimeret visning baseret på data interval.
#'
#' @param x_data Vector. Rå X-akse data (datoer, tal eller tekst)
#' @return List med formateret data og metadata
#' @details
#' Validering proces:
#' \enumerate{
#'   \item Tjek om x_col eksisterer i data
#'   \item Forsøg dato parsing for forskellige formater
#'   \item Beregn optimal date interval hvis dato
#'   \item Generer formaterings string for qicharts2
#'   \item Fallback til numerisk sekvensnummerering
#' }
#'
#' Understøttede dato formater:
#' \itemize{
#'   \item "dd-mm-yyyy" (dansk standard)
#'   \item "yyyy-mm-dd" (ISO)
#'   \item "mm/dd/yyyy" (amerikansk)
#'   \item Automatisk locale detection
#' }
#'
#' @return List med formateret X-akse data:
#' \describe{
#'   \item{x_data}{Formateret X-akse værdier}
#'   \item{x.format}{qicharts2 formaterings string eller NULL}
#'   \item{is_date}{Logical - om data er datoer}
#'   \item{interval_info}{Liste med interval statistik (kun datoer)}
#' }
#'
#' @examples
#' \dontrun{
#' # Dato data
#' data <- data.frame(
#'   Dato = c("01-01-2024", "01-02-2024", "01-03-2024"),
#'   Værdi = c(95, 92, 98)
#' )
#' result <- validate_x_column_format(data, "Dato", "day")
#'
#' # Numerisk data
#' data_num <- data.frame(Obs = 1:10, Værdi = rnorm(10))
#' result <- validate_x_column_format(data_num, "Obs", "observation")
#' }
#'
#' @seealso \code{\link{detect_date_interval}}, \code{\link{get_optimal_formatting}}
validate_x_column_format <- function(data, x_col, x_axis_unit = "observation") {
  # Return default hvis ingen x-kolonne
  if (is.null(x_col) || !x_col %in% names(data)) {
    return(list(
      x_data = 1:nrow(data),
      x.format = NULL,
      is_date = FALSE
    ))
  }

  x_data <- data[[x_col]]

  # Tjek om data allerede er Date/POSIXct
  if (inherits(x_data, c("Date", "POSIXct", "POSIXt"))) {
    # Data er allerede formateret som dato/tid
    x_format <- get_x_format_string(x_axis_unit)
    return(list(
      x_data = x_data,
      x.format = x_format,
      is_date = TRUE
    ))
  }

  # Forsøg intelligent date detection med lubridate
  if (is.character(x_data) || is.factor(x_data)) {
    char_data <- as.character(x_data)[!is.na(x_data)]

    if (length(char_data) > 0) {
      # Test sample til date detection
      test_sample <- char_data[1:min(5, length(char_data))]

      # FØRST: Test danske dato-formater direkte (mest almindelige)
      danish_parsed <- suppressWarnings(lubridate::dmy(char_data))
      danish_success_rate <- sum(!is.na(danish_parsed)) / length(danish_parsed)

      if (danish_success_rate >= 0.7) {
        # Danske datoer fungerer - konverter til POSIXct for konsistens med qicharts2
        x_data_converted <- as.POSIXct(danish_parsed)
        x_format <- get_x_format_string(x_axis_unit)


        return(list(
          x_data = x_data_converted,
          x.format = x_format,
          is_date = TRUE
        ))
      }

      # FALLBACK: Brug lubridate guess_formats for andre formater (med error handling)
      safe_operation(
        "Parse dates using lubridate guess_formats",
        code = {
          guessed_formats <- suppressWarnings(
            lubridate::guess_formats(test_sample, c("ymd", "dmy", "mdy", "dby", "dmY", "Ymd", "mdY"))
          )

          if (!is.null(guessed_formats) && length(guessed_formats) > 0) {
            # Filtrer ugyldige formater (undgå "n" format problem)
            valid_formats <- guessed_formats[!grepl("^n$|Unknown", guessed_formats)]

            if (length(valid_formats) > 0) {
              # Test konvertering med guessed formats
              parsed_dates <- suppressWarnings(
                lubridate::parse_date_time(char_data, orders = valid_formats, quiet = TRUE)
              )

              if (!is.null(parsed_dates)) {
                success_rate <- sum(!is.na(parsed_dates)) / length(parsed_dates)

                if (success_rate >= 0.7) { # 70% success rate threshold
                  # Konverter til Date objekter
                  x_data_converted <- as.Date(parsed_dates)
                  x_format <- get_x_format_string(x_axis_unit)

                  return(list(
                    x_data = x_data_converted,
                    x.format = x_format,
                    is_date = TRUE
                  ))
                }
              }
            }
          }
        },
        fallback = function(e) {
          # Skip denne parsing metode hvis den fejler
        },
        error_type = "processing"
      )
    }
  }

  # Numerisk data eller tekst der ikke kunne parses som datoer
  if (is.numeric(x_data)) {
    return(list(
      x_data = x_data,
      x.format = NULL,
      is_date = FALSE
    ))
  } else {
    # Fallback til observation nummer
    return(list(
      x_data = 1:length(x_data),
      x.format = NULL,
      is_date = FALSE
    ))
  }
}

## Simpel formatering baseret på x_axis_unit
get_x_format_string <- function(x_axis_unit) {
  switch(x_axis_unit,
    "date" = "%Y-%m-%d",
    "month" = "%b %Y",
    "year" = "%Y",
    "week" = "Uge %W",
    "hour" = "%H:%M",
    "%Y-%m-%d" # default
  )
}

## Intelligent dato-interval detektion
detect_date_interval <- function(dates, debug = TRUE) {
  insufficient_response <- function(n_obs) {
    list(
      type = "insufficient_data",
      median_days = NA_real_,
      consistency = 0,
      timespan_days = 0,
      n_obs = n_obs
    )
  }

  if (length(dates) < 2) {
    return(insufficient_response(length(dates)))
  }

  # Sorter datoer og beregn intervaller
  sorted_dates <- sort(dates[!is.na(dates)])
  if (length(sorted_dates) < 2) {
    return(insufficient_response(length(sorted_dates)))
  }

  # Beregn forskelle mellem konsekutive datoer (i dage)
  intervals <- as.numeric(diff(sorted_dates))

  if (length(intervals) == 0) {
    return(insufficient_response(length(sorted_dates)))
  }

  median_interval <- median(intervals, na.rm = TRUE)
  interval_variance <- var(intervals, na.rm = TRUE)
  consistency <- 1 - (sqrt(interval_variance) / median_interval) # Høj værdi = konsistent
  consistency <- max(0, min(1, consistency)) # Klamp til 0-1

  timespan_days <- as.numeric(max(sorted_dates) - min(sorted_dates))

  # Klassificer interval type baseret på median
  interval_type <- if (median_interval <= 1) {
    "daily"
  } else if (median_interval <= 10) {
    "weekly"
  } else if (median_interval <= 40) {
    "monthly"
  } else if (median_interval <= 120) {
    "quarterly"
  } else if (median_interval <= 400) {
    "yearly"
  } else {
    "irregular"
  }

  # Debug info available in interval_info list
  return(list(
    type = interval_type,
    median_days = median_interval,
    consistency = consistency,
    timespan_days = timespan_days,
    n_obs = length(sorted_dates)
  ))
}

## Optimal formatering baseret på interval og antal observationer
get_optimal_formatting <- function(interval_info, debug = TRUE) {
  interval_type <- interval_info$type
  n_obs <- interval_info$n_obs
  timespan_days <- interval_info$timespan_days

  # Formatering matrix baseret på interval type og antal observationer
  config <- switch(interval_type,
    daily = {
      if (n_obs < 30) {
        list(labels = "%d %b", breaks = "1 week", n_breaks = 8)
      } else if (n_obs < 90) {
        list(labels = "%b %Y", breaks = "2 weeks", n_breaks = 10)
      } else {
        list(labels = "%b %Y", breaks = "1 month", n_breaks = 12)
      }
    },
    weekly = {
      if (n_obs <= 36) {
        # Intelligent uge-formatering med scales::label_date_short()
        list(
          use_smart_labels = TRUE,
          labels = scales::label_date_short(),
          n_breaks = min(n_obs, 24) # Max 15 breaks for læsbarhed
        )
      } else {
        # For mange uger - skift til månedlig visning
        list(
          use_smart_labels = FALSE,
          labels = "%b %Y",
          breaks = "1 month",
          n_breaks = 12
        )
      }
    },
    monthly = {
      if (n_obs < 12) {
        # Intelligent måneds-formatering med scales::label_date_short()
        list(
          use_smart_labels = TRUE,
          labels = scales::label_date_short(
            format = c("%Y", "%b"), # År først, så måneder
            sep = "\n"
          ),
          breaks = "1 month",
          n_breaks = n_obs
        )
      } else if (n_obs < 40) {
        # Intelligent måneds-formatering med scales::label_date_short()
        list(
          use_smart_labels = TRUE,
          labels = scales::label_date_short(),
          breaks = "3 months",
          n_breaks = 8
        )
      } else {
        # For mange måneder - skift til årlig visning med smart labels
        list(
          use_smart_labels = TRUE,
          labels = scales::label_date_short(
            format = c("%Y", "", ""), # Kun år
            sep = ""
          ),
          breaks = "6 months",
          n_breaks = 10
        )
      }
    },
    quarterly = {
      list(labels = "Q%q %Y", breaks = "3 months", n_breaks = 8)
    },
    yearly = {
      list(labels = "%Y", breaks = "1 year", n_breaks = min(n_obs, 10))
    },
    # Default/irregular
    {
      # Tilpas til tidsspan
      if (timespan_days < 100) {
        list(labels = "%d %b %Y", breaks = "2 weeks", n_breaks = 8)
      } else if (timespan_days < 730) {
        list(labels = "%b %Y", breaks = "2 months", n_breaks = 10)
      } else {
        list(labels = "%Y", breaks = "1 year", n_breaks = 12)
      }
    }
  )

  # Debug info available in config list
  return(config)
}


# SPC PLOT GENERERING =========================================================

#' Process chart title for SPC plot
#'
#' @param chart_title_reactive Reactive function for chart title
#' @param config Chart configuration with y_col
#' @return Character string with processed title
process_chart_title <- function(chart_title_reactive, config) {

  custom_title <- safe_operation(
    "Process chart title reactive",
    code = {
      if (!is.null(chart_title_reactive) && is.function(chart_title_reactive)) {
        title <- chart_title_reactive()
        if (!is.null(title) && title != "" && title != "SPC Analyse") {
          title
        } else {
          NULL
        }
      } else {
        NULL
      }
    },
    fallback = function(e) {
      log_error(paste("ERROR in chart_title_reactive:", e$message), "SPC_CALC_DEBUG")
      NULL
    },
    error_type = "processing"
  )


  if (!is.null(custom_title)) {
    custom_title
  } else {
    paste("SPC Chart -", config$y_col)
  }
}

#' Validate input parameters for SPC plot generation
#'
#' @param data Input data frame
#' @param config Chart configuration
validate_spc_inputs <- function(data, config) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    stop("Ingen gyldig data at visualisere")
  }

  if (is.null(config$y_col) || length(config$y_col) == 0 || identical(config$y_col, character(0))) {
    stop("Y-kolonne kan ikke være character(0)")
  }

  y_col <- config$y_col[[1]]

  if (!y_col %in% names(data)) {
    stop("Y-kolonne ikke fundet i data")
  }
}

#' Comprehensive Data Structure Validation for SPC Analysis
#'
#' @description
#' Performs comprehensive validation of uploaded data to ensure it is suitable
#' for SPC (Statistical Process Control) analysis. Integrated from validateDataStructure.
#'
#' @param data Data frame with uploaded data to validate
#' @return List with validation results containing valid, errors, warnings, potential columns
#'
#' @export
validate_data_structure <- function(data) {
  errors <- character(0)
  warnings <- character(0)

  # Check if data exists and has content
  if (is.null(data) || nrow(data) == 0) {
    errors <- c(errors, "Ingen data fundet i filen")
    return(list(valid = FALSE, errors = errors, warnings = warnings))
  }

  if (ncol(data) < 2) {
    errors <- c(errors, "Data skal have mindst 2 kolonner (tid og værdi)")
  }

  # Check for potential time/date columns using tidyverse approach
  empty_columns <- data |>
    purrr::map_lgl(~ all(is.na(.x))) |>
    purrr::keep(isTRUE) |>
    names()

  # Add warnings for empty columns
  if (length(empty_columns) > 0) {
    warnings <- c(warnings, purrr::map_chr(empty_columns, ~ paste("Kolonne", .x, "er helt tom")))
  }

  # Find potential date columns
  potential_date_cols <- data |>
    purrr::imap_chr(~ {
      if (!all(is.na(.x))) {
        char_data <- as.character(.x)[!is.na(.x)]
        if (length(char_data) > 0 &&
            any(grepl("\\d{4}-\\d{2}-\\d{2}|\\d{2}/\\d{2}/\\d{4}|\\d{2}-\\d{2}-\\d{4}", char_data))) {
          return(.y)
        }
      }
      return(NA_character_)
    }) |>
    purrr::discard(is.na)

  # Find potential numeric columns
  potential_numeric_cols <- data |>
    purrr::imap_chr(~ {
      if (!all(is.na(.x))) {
        if (is.numeric(.x) ||
            sum(!is.na(parse_danish_number(.x))) > length(.x) * MIN_NUMERIC_PERCENT) {
          return(.y)
        }
      }
      return(NA_character_)
    }) |>
    purrr::discard(is.na)

  # Validate minimum requirements
  if (length(potential_numeric_cols) == 0) {
    errors <- c(errors, "Ingen numeriske kolonner fundet - mindst én numerisk kolonne er påkrævet for SPC analyse")
  }

  if (length(potential_date_cols) == 0 && nrow(data) > 1) {
    warnings <- c(warnings, "Ingen dato-kolonner fundet - overvej at tilføje tidsstempel for tidsserie-analyse")
  }

  # Check data size
  if (nrow(data) < MIN_SPC_ROWS) {
    warnings <- c(warnings, paste("Kun", nrow(data), "datapunkter fundet - SPC analyse er mest pålidelig med mindst", RECOMMENDED_SPC_POINTS, "punkter"))
  }

  # Check for missing values
  missing_pct <- round(sum(is.na(data)) / (nrow(data) * ncol(data)) * 100, 1)
  if (missing_pct > MAX_MISSING_PERCENT) {
    warnings <- c(warnings, paste("Høj andel manglende værdier:", missing_pct, "% - dette kan påvirke analyse-kvaliteten"))
  }

  # Return validation results
  return(list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings,
    potential_date_cols = potential_date_cols,
    potential_numeric_cols = potential_numeric_cols,
    summary = list(
      rows = nrow(data),
      cols = ncol(data),
      missing_pct = missing_pct
    )
  ))
}

#' Process phase and freeze configuration
#'
#' @param data Input data frame
#' @param show_phases Logical indicating if phases should be shown
#' @param skift_column Column name for phase shifts
#' @param frys_column Column name for baseline freeze
#' @return List with part_positions and freeze_position
process_phase_freeze_config <- function(data, show_phases, skift_column, frys_column) {

  part_positions <- NULL
  if (show_phases && !is.null(skift_column)) {
    # DEFENSIVE: Check for character(0) before using %in%
    if (length(skift_column) == 0 || identical(skift_column, character(0))) {
    } else if (skift_column %in% names(data)) {
      skift_data <- data[[skift_column]]

      # Convert to logical if needed
      if (!is.logical(skift_data)) {
        skift_data <- as.logical(skift_data)
      }

      # Get positions where TRUE values occur (these are where new phases start)
      skift_points <- which(skift_data == TRUE)
      if (length(skift_points) > 0) {
        # qic() expects integer vector of positions where new phases start
        part_positions <- sort(skift_points)
      }
    }
  }

  # Handle baseline freeze from selected Frys column

  freeze_position <- NULL
  if (!is.null(frys_column)) {
    # DEFENSIVE: Check for character(0) before using %in%
    if (length(frys_column) == 0 || identical(frys_column, character(0))) {
      # Skip processing for invalid column reference
    } else if (frys_column %in% names(data)) {
      frys_data <- data[[frys_column]]

      # Convert to logical if needed
      if (!is.logical(frys_data)) {
        frys_data <- as.logical(frys_data)
      }

      # Get positions where TRUE values occur (baseline freeze points)
      frys_points <- which(frys_data == TRUE)

      if (length(frys_points) > 0) {
        # Use the last TRUE position as freeze point (baseline up to this point)
        freeze_position <- max(frys_points)
      }
    }
  }

  # Results ready for qic() function

  list(
    part_positions = part_positions,
    freeze_position = freeze_position
  )
}

# CHART VALIDATION ============================================================

## Validering af Data til Chart Generering
# Tjekker data kvalitet og kompatibilitet med valgt chart type
validateDataForChart <- function(data, config, chart_type) {
  warnings <- character(0)

  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    warnings <- c(warnings, "Ingen data tilgængelig")
    return(list(valid = FALSE, warnings = warnings))
  }

  if (is.null(config$y_col) || !config$y_col %in% names(data)) {
    warnings <- c(warnings, "Ingen numerisk kolonne fundet til Y-akse")
    return(list(valid = FALSE, warnings = warnings))
  }

  if (chart_type %in% c("p", "pp", "u", "up")) {
    if (is.null(config$n_col) || !config$n_col %in% names(data)) {
      warnings <- c(warnings, paste("Chart type", chart_type, "kræver en nævner-kolonne"))
      return(list(valid = FALSE, warnings = warnings))
    }
  }

  # Check for missing values
  y_data <- data[[config$y_col]]
  if (all(is.na(y_data))) {
    warnings <- c(warnings, "Alle værdier i Y-kolonnen er tomme")
    return(list(valid = FALSE, warnings = warnings))
  }

  # Skift column validation handled by qicharts2::qic() internally

  if (nrow(data) < 8) {
    warnings <- c(warnings, paste("Kun", nrow(data), "datapunkter - SPC analyse er mest pålidelig med mindst 15-20 punkter"))
  }

  return(list(valid = TRUE, warnings = warnings))
}

## Generér SPC plot med tilpasset styling
