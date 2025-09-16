# data_validation.R
# Modul til validering af data til SPC analyse

# Dependencies ----------------------------------------------------------------
# Bruger funktioner fra danish_numbers.R

# VALIDERING FUNKTIONER =======================================================

#' Validér datastruktur til SPC analyse
#'
#' @description
#' Udfører omfattende validering af uploaded data for at sikre at det
#' er egnet til SPC (Statistical Process Control) analyse. Checker
#' datastruktur, kolonne-typer, data-kvalitet og giver anbefalinger.
#'
#' @param data Data frame med uploadede data der skal valideres
#'
#' @return Liste med validerings resultater indeholdende:
#' \describe{
#'   \item{valid}{Logisk værdi - TRUE hvis data er grundlæggende validt}
#'   \item{errors}{Karakter vektor med kritiske fejl der forhindrer analyse}
#'   \item{warnings}{Karakter vektor med advarsler og anbefalinger}
#'   \item{potential_date_cols}{Navne på kolonner der ser ud til at indeholde datoer}
#'   \item{potential_numeric_cols}{Navne på numeriske kolonner egnet til SPC}
#'   \item{summary}{Liste med grundlæggende data statistikker}
#' }
#' @export
#'
#' @details
#' Funktionen udfører følgende valideringer:
#' \itemize{
#'   \item Checker at data eksisterer og har indhold
#'   \item Verificerer minimum antal kolonner (mindst 2)
#'   \item Identificerer potentielle dato/tid kolonner
#'   \item Finder numeriske kolonner (inkl. danske tal formater)
#'   \item Vurderer data størrelse og anbefaler minimum antal punkter
#'   \item Analyserer missing values og advarer ved høje procenter
#' }
#'
#' @examples
#' # Valider eksempel data
#' test_data <- data.frame(
#'   Dato = c("2023-01-01", "2023-01-02", "2023-01-03"),
#'   Tæller = c(10, 15, 12),
#'   Nævner = c(100, 120, 110)
#' )
#' result <- validateDataStructure(test_data)
#' print(result$valid)  # TRUE
#' print(result$potential_numeric_cols)  # "Tæller", "Nævner"
validateDataStructure <- function(data) {
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

  # Check for potential time/date columns
  potential_date_cols <- character(0)
  potential_numeric_cols <- character(0)

  for (col_name in names(data)) {
    col_data <- data[[col_name]]

    # Skip completely empty columns
    if (all(is.na(col_data))) {
      warnings <- c(warnings, paste("Kolonne", col_name, "er helt tom"))
      next
    }

    # Check for potential date columns
    char_data <- as.character(col_data)[!is.na(col_data)]
    if (length(char_data) > 0) {
      if (any(grepl("\\d{4}-\\d{2}-\\d{2}|\\d{2}/\\d{2}/\\d{4}|\\d{2}-\\d{2}-\\d{4}", char_data))) {
        potential_date_cols <- c(potential_date_cols, col_name)
      }
    }

    # Check for numeric columns (handle Danish decimal separator)
    if (is.numeric(col_data) ||
      sum(!is.na(parse_danish_number(col_data))) > length(col_data) * 0.8) {
      potential_numeric_cols <- c(potential_numeric_cols, col_name)
    }
  }

  # Validate minimum requirements
  if (length(potential_numeric_cols) == 0) {
    errors <- c(errors, "Ingen numeriske kolonner fundet - mindst én numerisk kolonne er påkrævet for SPC analyse")
  }

  if (length(potential_date_cols) == 0 && nrow(data) > 1) {
    warnings <- c(warnings, "Ingen dato-kolonner fundet - overvej at tilføje tidsstempel for tidsserie-analyse")
  }

  # Check data size
  if (nrow(data) < 10) {
    warnings <- c(warnings, paste("Kun", nrow(data), "datapunkter fundet - SPC analyse er mest pålidelig med mindst 15-20 punkter"))
  }

  # Check for missing values
  missing_pct <- round(sum(is.na(data)) / (nrow(data) * ncol(data)) * 100, 1)
  if (missing_pct > 20) {
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
