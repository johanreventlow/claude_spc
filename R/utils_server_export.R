# utils_server_export.R
# Export helper utilities for server-side operations
# Provides data extraction and formatting for PDF/PowerPoint/PNG exports

# EXTRACT SPC STATISTICS ======================================================

#' Extract SPC Statistics from App State
#'
#' Udtræk Anhøj rules statistikker fra app_state til brug i export funktioner.
#' Returnerer named list med expected og actual værdier for runs, crossings, outliers.
#'
#' @param app_state Reactive values. Global app state med visualization data.
#'
#' @return List med SPC statistikker eller NULL hvis ikke tilgængelige.
#'   Liste indeholder: runs_expected, runs_actual, crossings_expected,
#'   crossings_actual, outliers_expected, outliers_actual
#'
#' @examples
#' \dontrun{
#' spc_stats <- extract_spc_statistics(app_state)
#' if (!is.null(spc_stats)) {
#'   print(paste("Runs:", spc_stats$runs_actual))
#' }
#' }
#'
#' @family export_helpers
#' @export
extract_spc_statistics <- function(app_state) {
  safe_operation(
    operation_name = "Extract SPC statistics",
    code = {
      # Validér app_state eksisterer
      if (is.null(app_state)) {
        log_warn(
          component = "[EXPORT]",
          message = "app_state er NULL - kan ikke udtrække SPC statistikker"
        )
        return(NULL)
      }

      # Hent Anhøj results fra visualization state
      anhoej <- app_state$visualization$anhoej_results

      if (is.null(anhoej)) {
        log_warn(
          component = "[EXPORT]",
          message = "Ingen Anhøj results tilgængelige"
        )
        return(NULL)
      }

      # Check om vi har valid data
      if (!isTRUE(anhoej$has_valid_data)) {
        log_debug(
          component = "[EXPORT]",
          message = "Anhøj results ikke valid endnu",
          details = list(message = anhoej$message %||% "Ingen besked")
        )
        return(NULL)
      }

      # Udtræk statistikker
      stats <- list(
        runs_expected = anhoej$longest_run_max,
        runs_actual = anhoej$longest_run,
        crossings_expected = anhoej$n_crossings_min,
        crossings_actual = anhoej$n_crossings,
        outliers_expected = 0, # Anhøj rules forventer 0 outliers normalt
        outliers_actual = anhoej$out_of_control_count %||% 0
      )

      log_debug(
        component = "[EXPORT]",
        message = "SPC statistikker udtrukket succesfuldt",
        details = list(
          runs = sprintf("%s/%s", stats$runs_actual, stats$runs_expected),
          crossings = sprintf("%s/%s", stats$crossings_actual, stats$crossings_expected),
          outliers = stats$outliers_actual
        )
      )

      return(stats)
    },
    fallback = function(e) {
      log_error(
        component = "[EXPORT]",
        message = "Fejl ved udtrækning af SPC statistikker",
        details = list(error = e$message)
      )
      return(NULL)
    },
    error_type = "processing"
  )
}

# GENERATE DETAILS STRING =====================================================

#' Generate Details String for Export
#'
#' Generér detalje-streng med periode info, gennemsnit og nuværende niveau
#' til brug i PDF/PowerPoint exports.
#'
#' @param app_state Reactive values. Global app state med data og columns.
#' @param format Character. Format af output ("short" eller "full"). Default: "full".
#'
#' @return Character string med detaljer eller NULL hvis data ikke tilgængelig.
#'
#' @examples
#' \dontrun{
#' details <- generate_details_string(app_state)
#' # Returns: "Periode: jan. 2024 – dec. 2024 • Gns.: 42.5 • Seneste: 45.2"
#' }
#'
#' @family export_helpers
#' @export
generate_details_string <- function(app_state, format = c("full", "short")) {
  format <- match.arg(format)

  safe_operation(
    operation_name = "Generate details string",
    code = {
      # Validér app_state og data
      if (is.null(app_state) || is.null(app_state$data$current_data)) {
        log_warn(
          component = "[EXPORT]",
          message = "Ingen data tilgængelig for details string"
        )
        return(NULL)
      }

      data <- app_state$data$current_data
      x_col <- app_state$columns$mappings$x_column
      y_col <- app_state$columns$mappings$y_column

      if (is.null(x_col) || is.null(y_col)) {
        log_warn(
          component = "[EXPORT]",
          message = "x eller y kolonne ikke mappet"
        )
        return(NULL)
      }

      # Udtræk værdier
      x_values <- data[[x_col]]
      y_values <- data[[y_col]]

      # Validér at vi har data
      if (length(y_values) == 0) {
        return(NULL)
      }

      # Beregn statistikker
      mean_value <- mean(y_values, na.rm = TRUE)
      latest_value <- tail(y_values[!is.na(y_values)], 1)
      n_observations <- sum(!is.na(y_values))

      # Periode info (hvis x er dato)
      period_str <- NULL
      if (inherits(x_values, "Date") || inherits(x_values, "POSIXt")) {
        valid_dates <- x_values[!is.na(x_values)]
        if (length(valid_dates) > 0) {
          start_date <- min(valid_dates)
          end_date <- max(valid_dates)

          # Dansk dato formatering
          start_fmt <- format(start_date, "%b. %Y")
          end_fmt <- format(end_date, "%b. %Y")

          period_str <- sprintf("Periode: %s – %s", start_fmt, end_fmt)
        }
      }

      # Byg details string
      parts <- c()

      if (!is.null(period_str)) {
        parts <- c(parts, period_str)
      }

      parts <- c(parts, sprintf("Antal obs.: %d", n_observations))

      if (format == "full") {
        parts <- c(parts, sprintf("Gennemsnit: %.1f", mean_value))

        if (length(latest_value) > 0) {
          parts <- c(parts, sprintf("Seneste: %.1f", latest_value))
        }
      }

      details <- paste(parts, collapse = " • ")

      log_debug(
        component = "[EXPORT]",
        message = "Details string genereret",
        details = list(
          format = format,
          n_obs = n_observations,
          output = details
        )
      )

      return(details)
    },
    fallback = function(e) {
      log_error(
        component = "[EXPORT]",
        message = "Fejl ved generering af details string",
        details = list(error = e$message)
      )
      return(NULL)
    },
    error_type = "processing"
  )
}

# CHECK QUARTO AVAILABILITY ===================================================

#' Check if Quarto is Available
#'
#' Tjekker om Quarto CLI er tilgængelig på systemet.
#' Bruges til at vise/skjule PDF export option baseret på Quarto tilgængelighed.
#'
#' @return Logical. TRUE hvis Quarto er tilgængelig, FALSE ellers.
#'
#' @examples
#' \dontrun{
#' if (quarto_available()) {
#'   # Enable PDF export
#' } else {
#'   # Show message to install Quarto
#' }
#' }
#'
#' @family export_helpers
#' @export
quarto_available <- function() {
  quarto_path <- Sys.which("quarto")
  available <- nzchar(quarto_path)

  # Fallback: Check for RStudio bundled Quarto (macOS)
  if (!available && file.exists("/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto")) {
    available <- TRUE
  }

  # Fallback: Check for RStudio bundled Quarto (Windows)
  if (!available && .Platform$OS.type == "windows") {
    rstudio_quarto <- file.path(
      Sys.getenv("PROGRAMFILES"),
      "RStudio/resources/app/quarto/bin/quarto.cmd"
    )
    if (file.exists(rstudio_quarto)) {
      available <- TRUE
    }
  }

  if (!available) {
    log_debug(
      component = "[EXPORT]",
      message = "Quarto CLI ikke fundet - PDF export ikke tilgængelig"
    )
  }

  return(available)
}

# GET HOSPITAL NAME ===========================================================

#' Get Hospital Name for Export
#'
#' Henter hospital navn fra branding config til brug i exports.
#' Fallback til default hvis ikke tilgængeligt.
#'
#' @return Character. Hospital navn.
#'
#' @examples
#' \dontrun{
#' hospital <- get_hospital_name_for_export()
#' }
#'
#' @family export_helpers
#' @export
get_hospital_name_for_export <- function() {
  # Prøv at hente fra branding config
  if (exists("get_hospital_name") && is.function(get_hospital_name)) {
    name <- tryCatch(
      get_hospital_name(),
      error = function(e) NULL
    )

    if (!is.null(name) && nchar(name) > 0) {
      return(name)
    }
  }

  # Fallback til global variabel hvis den eksisterer
  if (exists("HOSPITAL_NAME", envir = .GlobalEnv)) {
    name <- get("HOSPITAL_NAME", envir = .GlobalEnv)
    if (!is.null(name) && nchar(name) > 0) {
      return(name)
    }
  }

  # Final fallback
  return("Bispebjerg og Frederiksberg Hospital")
}

# GENERATE PDF PREVIEW ========================================================

#' Generate PDF Preview Image
#'
#' Kompilerer Typst til PDF og konverterer første side til PNG til preview.
#' Bruges af export module til at vise PDF layout preview.
#'
#' @param plot_object ggplot2 object. SPC chart til embedding i PDF.
#' @param metadata List. PDF metadata (hospital, department, title, analysis, etc.).
#' @param spc_statistics List. SPC statistikker (runs, crossings, outliers).
#' @param dpi Numeric. DPI for PNG rendering (default: 150).
#'
#' @return Character path til PNG preview fil eller NULL ved fejl.
#'
#' @details
#' Funktionen:
#' 1. Genererer temp PDF via \code{export_spc_to_typst_pdf()}
#' 2. Konverterer første side til PNG via \code{pdftools::pdf_render_page()}
#' 3. Returnerer path til PNG fil (i temp directory)
#'
#' PNG filen er midlertidig og vil blive slettet når R session afsluttes.
#'
#' @examples
#' \dontrun{
#' plot <- ggplot2::qplot(1:10, 1:10)
#' metadata <- list(
#'   hospital = "Test Hospital",
#'   department = "Test Dept",
#'   title = "Test Chart",
#'   analysis = "Test analysis",
#'   details = "Test details",
#'   data_definition = NULL,
#'   author = "Test Author",
#'   date = Sys.Date()
#' )
#' spc_stats <- list(
#'   runs_expected = 12, runs_actual = 10,
#'   crossings_expected = 16, crossings_actual = 14,
#'   outliers_expected = 0, outliers_actual = 2
#' )
#'
#' preview_path <- generate_pdf_preview(plot, metadata, spc_stats)
#' if (!is.null(preview_path)) {
#'   # Display preview image
#' }
#' }
#'
#' @family export_helpers
#' @export
generate_pdf_preview <- function(plot_object,
                                 metadata,
                                 spc_statistics,
                                 dpi = 150) {
  safe_operation(
    operation_name = "Generate PDF preview",
    code = {
      # Validér inputs
      if (is.null(plot_object)) {
        log_warn(
          component = "[EXPORT]",
          message = "Ingen plot object til PDF preview"
        )
        return(NULL)
      }

      # Check Quarto availability
      if (!quarto_available()) {
        log_warn(
          component = "[EXPORT]",
          message = "Quarto ikke tilgængelig - PDF preview kan ikke genereres"
        )
        return(NULL)
      }

      # Generer temp PDF
      temp_pdf <- tempfile(fileext = ".pdf")

      log_debug(
        component = "[EXPORT]",
        message = "Genererer temp PDF til preview",
        details = list(temp_pdf = temp_pdf)
      )

      # Brug eksisterende Typst export
      export_spc_to_typst_pdf(
        plot_object = plot_object,
        metadata = metadata,
        spc_statistics = spc_statistics,
        output_path = temp_pdf
      )

      # Validér PDF blev genereret
      if (!file.exists(temp_pdf)) {
        log_error(
          component = "[EXPORT]",
          message = "Temp PDF blev ikke genereret"
        )
        return(NULL)
      }

      # Konverter første side til PNG
      temp_png <- tempfile(fileext = ".png")

      log_debug(
        component = "[EXPORT]",
        message = "Konverterer PDF til PNG preview",
        details = list(
          pdf = temp_pdf,
          png = temp_png,
          dpi = dpi
        )
      )

      pdftools::pdf_render_page(
        pdf = temp_pdf,
        page = 1,
        dpi = dpi,
        file = temp_png
      )

      # Validér PNG blev genereret
      if (!file.exists(temp_png)) {
        log_error(
          component = "[EXPORT]",
          message = "PNG preview blev ikke genereret"
        )
        return(NULL)
      }

      # Cleanup temp PDF (behold kun PNG)
      unlink(temp_pdf)

      log_info(
        component = "[EXPORT]",
        message = "PDF preview genereret succesfuldt",
        details = list(
          png = temp_png,
          size_kb = round(file.size(temp_png) / 1024, 1)
        )
      )

      return(temp_png)
    },
    fallback = function(e) {
      log_error(
        component = "[EXPORT]",
        message = "PDF preview generation failed",
        details = list(error = e$message)
      )
      return(NULL)
    },
    error_type = "processing"
  )
}
