# utils_anhoej_results.R
# Hjælpefunktioner til opdatering af Anhøj resultater (serielængde og kryds)

#' Opdater Anhøj resultater ud fra nye QIC-beregninger
#'
#' Regler:
#' - Hvis nye beregninger indeholder gyldige metrics (longest_run eller n_crossings),
#'   så opdateres resultaterne altid og markeres som gyldige
#' - Hvis metrics er NA og centerline (baseline) er ændret, opdateres til NA-resultater
#'   (vi bevarer ikke gamle værdier — UI skal afspejle den nye tilstand)
#' - Hvis metrics er NA og centerline ikke er ændret, og der fandtes gyldige værdier før,
#'   bevares de gamle værdier (for at undgå midlertidig flimmer under beregning)
#'
#' @param previous list. Forrige `anhoej_results` (kan være NULL ved første kørsel)
#' @param qic_results list. Nye beregnede metrics fra QIC-kald (kan indeholde NA)
#' @param centerline_changed logical. TRUE hvis centerline/baseline er ændret (inkl. ryddet)
#' @return list. Opdateret `anhoej_results`
#' @keywords internal
update_anhoej_results <- function(previous, qic_results, centerline_changed = FALSE) {
  # Defensive input checks
  if (is.null(qic_results) || !is.list(qic_results)) {
    return(previous)
  }

  has_metrics <- (!is.null(qic_results$longest_run) && !is.na(qic_results$longest_run)) ||
                 (!is.null(qic_results$n_crossings) && !is.na(qic_results$n_crossings))

  # 1) Gyldige metrics → altid opdater
  if (isTRUE(has_metrics)) {
    qic_results$has_valid_data <- TRUE
    return(qic_results)
  }

  # 2) Ingen metrics og centerline ændret → opdater til NA-resultater (ikke bevar gamle)
  if (isTRUE(centerline_changed)) {
    qic_results$has_valid_data <- FALSE
    if (is.null(qic_results$message)) {
      qic_results$message <- "Ingen run-metrics tilgængelige"
    }
    return(qic_results)
  }

  # 3) Ingen metrics og centerline uændret → bevar tidligere gyldige
  if (!is.null(previous) && is.list(previous) && isTRUE(previous$has_valid_data)) {
    return(previous)
  }

  # 4) Fallback: ingen tidligere gyldige værdier – returnér NA-resultaterne
  qic_results$has_valid_data <- FALSE
  if (is.null(qic_results$message)) {
    qic_results$message <- "Ingen run-metrics tilgængelige"
  }
  return(qic_results)
}

