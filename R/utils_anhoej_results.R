# utils_anhoej_results.R
# Hjælpefunktioner til opdatering af Anhøj resultater (serielængde og kryds)

#' Filtrer qic_data til seneste part hvis skift er aktivt
#'
#' Når brugeren har aktiveret "Skift" (parts/phases), skal beregninger
#' kun baseres på den seneste part, ikke hele datasættet.
#'
#' @param qic_data data.frame. QIC data med part kolonne
#' @param show_phases logical. TRUE hvis parts/skift er aktivt
#' @return data.frame. Filtreret til seneste part eller hele datasæt
#' @keywords internal
filter_latest_part <- function(qic_data, show_phases = FALSE) {
  # Hvis ingen parts eller show_phases er FALSE, returner hele datasættet
  if (!isTRUE(show_phases) || is.null(qic_data) || !"part" %in% names(qic_data)) {
    return(qic_data)
  }

  # Find seneste part
  latest_part <- max(qic_data$part, na.rm = TRUE)

  # Returner kun data fra seneste part
  qic_data[qic_data$part == latest_part & !is.na(qic_data$part), ]
}

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
#' @param qic_data data.frame. QIC data (bruges til part filtrering)
#' @param show_phases logical. TRUE hvis parts/skift er aktivt
#' @return list. Opdateret `anhoej_results`
#' @keywords internal
update_anhoej_results <- function(previous, qic_results, centerline_changed = FALSE,
                                   qic_data = NULL, show_phases = FALSE) {
  # Defensive input checks
  if (is.null(qic_results) || !is.list(qic_results)) {
    return(previous)
  }

  # Filtrer til seneste part hvis skift er aktivt
  if (!is.null(qic_data) && isTRUE(show_phases)) {
    qic_data_filtered <- filter_latest_part(qic_data, show_phases)

    # Genberegn metrics OG signals baseret på filtreret data
    if (!is.null(qic_data_filtered) && nrow(qic_data_filtered) > 0) {
      # Opdater longest_run og n_crossings fra filtreret data hvis tilgængelig
      if ("longest.run" %in% names(qic_data_filtered)) {
        qic_results$longest_run <- max(qic_data_filtered$longest.run, na.rm = TRUE)
      }
      if ("longest.run.max" %in% names(qic_data_filtered)) {
        qic_results$longest_run_max <- max(qic_data_filtered$longest.run.max, na.rm = TRUE)
      }
      if ("n.crossings" %in% names(qic_data_filtered)) {
        qic_results$n_crossings <- max(qic_data_filtered$n.crossings, na.rm = TRUE)
      }
      if ("n.crossings.min" %in% names(qic_data_filtered)) {
        qic_results$n_crossings_min <- max(qic_data_filtered$n.crossings.min, na.rm = TRUE)
      }

      # Genberegn runs_signal baseret på filtreret data
      if ("runs.signal" %in% names(qic_data_filtered)) {
        qic_results$runs_signal <- any(qic_data_filtered$runs.signal, na.rm = TRUE)
      }

      # Genberegn crossings_signal baseret på filtreret data
      if ("n.crossings" %in% names(qic_data_filtered) && "n.crossings.min" %in% names(qic_data_filtered)) {
        n_cross <- max(qic_data_filtered$n.crossings, na.rm = TRUE)
        n_cross_min <- max(qic_data_filtered$n.crossings.min, na.rm = TRUE)
        qic_results$crossings_signal <- !is.na(n_cross) && !is.na(n_cross_min) && n_cross < n_cross_min
      }
    }
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

