# utils_font_registration.R
# Font registration utilities for ensuring Roboto Medium is available across all systems

#' Register Roboto Medium Font
#'
#' Registrerer den bundlede Roboto Medium font med systemfonts pakken.
#' Dette sikrer at fonten er tilgængelig på alle systemer, uanset om
#' Roboto er installeret lokalt.
#'
#' Funktionen køres automatisk ved app-start for at sikre font-tilgængelighed.
#'
#' @return NULL (invisible). Registrerer font som sideeffekt.
#'
#' @details
#' Roboto Medium font er bundlet i inst/fonts/ mappen og licenseret under
#' Apache License 2.0 (se inst/fonts/LICENSE).
#'
#' Font-registrering håndteres af systemfonts pakken, som er cross-platform
#' kompatibel (Windows, macOS, Linux).
#'
#' @examples
#' \dontrun{
#' # Typisk kaldt fra global.R eller app startup
#' register_roboto_font()
#' }
#'
#' @family font_registration
#' @export
register_roboto_font <- function() {
  # Guard: Only register once per session
  # Check if already registered to prevent duplicate registrations
  if (exists(".roboto_registered", envir = .GlobalEnv) &&
    isTRUE(get(".roboto_registered", envir = .GlobalEnv))) {
    return(invisible(NULL))
  }

  # Check if systemfonts package is available
  if (!requireNamespace("systemfonts", quietly = TRUE)) {
    log_warn(
      component = "[FONT_REGISTRATION]",
      message = "systemfonts package ikke tilgængelig - font-registrering sprunget over",
      details = list(fallback = "System vil forsøge at bruge lokalt installeret Roboto")
    )
    return(invisible(NULL))
  }

  # Definer font-sti (udvikling vs. installeret pakke)
  font_path <- tryCatch(
    {
      # Forsøg at finde font i installeret pakke
      system.file("fonts", "Roboto-Medium.ttf", package = "SPCify")
    },
    error = function(e) {
      # Fallback til relativ sti i udvikling
      file.path("inst", "fonts", "Roboto-Medium.ttf")
    }
  )

  # Verificer at font-filen eksisterer
  if (!file.exists(font_path) || font_path == "") {
    log_error(
      component = "[FONT_REGISTRATION]",
      message = "Roboto-Medium.ttf font-fil ikke fundet",
      details = list(
        expected_path = font_path,
        working_directory = getwd(),
        fallback = "System vil forsøge at bruge lokalt installeret Roboto"
      )
    )
    return(invisible(NULL))
  }

  # Registrer font med systemfonts
  safe_operation(
    "Registrer Roboto Medium font",
    code = {
      # Register font family with full font path
      systemfonts::register_font(
        name = "Roboto Medium",
        plain = font_path
      )

      log_info(
        component = "[FONT_REGISTRATION]",
        message = "Roboto Medium font registreret succesfuldt",
        details = list(
          font_path = font_path,
          font_size_kb = round(file.size(font_path) / 1024, 1)
        )
      )

      # Mark as registered to prevent duplicate registrations
      assign(".roboto_registered", TRUE, envir = .GlobalEnv)
    },
    fallback = function(e) {
      log_warn(
        component = "[FONT_REGISTRATION]",
        message = "Font-registrering fejlede - fortsætter med system-standard",
        details = list(
          error = e$message,
          fallback = "System vil forsøge at bruge lokalt installeret Roboto"
        )
      )
    },
    error_type = "processing"
  )

  return(invisible(NULL))
}

#' Check if Roboto Medium Font is Available
#'
#' Tjekker om Roboto Medium fonten er tilgængelig i systemet.
#' Nyttigt for diagnostik og fejlfinding.
#'
#' @return Logical. TRUE hvis fonten er tilgængelig, FALSE ellers.
#'
#' @examples
#' \dontrun{
#' if (is_roboto_available()) {
#'   message("Roboto Medium er tilgængelig")
#' }
#' }
#'
#' @family font_registration
#' @export
is_roboto_available <- function() {
  if (!requireNamespace("systemfonts", quietly = TRUE)) {
    return(FALSE)
  }

  # Check if font is registered
  tryCatch(
    {
      available_fonts <- systemfonts::system_fonts()
      any(grepl("Roboto.*Medium", available_fonts$family, ignore.case = TRUE))
    },
    error = function(e) {
      FALSE
    }
  )
}

#' Get Font Registration Status
#'
#' Returnerer detaljeret status om font-registrering.
#' Nyttigt til debugging og system-diagnostik.
#'
#' @return Named list med font-status information
#'
#' @examples
#' \dontrun{
#' status <- get_font_status()
#' print(status)
#' }
#'
#' @family font_registration
#' @export
get_font_status <- function() {
  # Check if systemfonts is available
  systemfonts_available <- requireNamespace("systemfonts", quietly = TRUE)

  # Check font file existence
  font_path <- system.file("fonts", "Roboto-Medium.ttf", package = "SPCify")
  font_exists <- file.exists(font_path) && font_path != ""

  # Check if Roboto is available
  roboto_available <- is_roboto_available()

  list(
    systemfonts_installed = systemfonts_available,
    font_file_exists = font_exists,
    font_file_path = if (font_exists) font_path else NA_character_,
    roboto_registered = roboto_available,
    status = if (roboto_available) "OK" else "WARNING: Roboto Medium not available"
  )
}
