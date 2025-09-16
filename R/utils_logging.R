# utils_logging.R
# Konfigurerbart logging system til SPC App

#' Log levels liste til SPC App logging system
#'
#' @description
#' Definerer numeriske værdier for forskellige log niveauer til brug i det
#' konfigurerede logging system. Lavere værdier betyder højere prioritet.
#'
#' @details
#' Log levels:
#' - DEBUG (1): Detaljeret debug information
#' - INFO (2): Generel information
#' - WARN (3): Advarsler
#' - ERROR (4): Fejl beskeder
#'
#' @format Liste med 4 elementer
#' @export
LOG_LEVELS <- list(
  DEBUG = 1,
  INFO = 2,
  WARN = 3,
  ERROR = 4
)

#' Hent aktuel log level fra environment variable
#'
#' @description
#' Læser SPC_LOG_LEVEL environment variablen og returnerer den tilsvarende
#' numeriske værdi. Falder tilbage til INFO hvis variablen ikke er sat eller
#' indeholder en ugyldig værdi.
#'
#' @return Numerisk værdi svarende til log level (se LOG_LEVELS)
#' @export
#'
#' @examples
#' # Standard opførsel (INFO level)
#' get_log_level()
#'
#' # Med DEBUG environment variable
#' Sys.setenv(SPC_LOG_LEVEL = "DEBUG")
#' get_log_level()  # Returns 1
get_log_level <- function() {
  env_level <- Sys.getenv("SPC_LOG_LEVEL", "INFO")
  level_num <- LOG_LEVELS[[toupper(env_level)]]
  if (is.null(level_num)) {
    return(LOG_LEVELS$INFO)
  }
  return(level_num)
}

#' Primær logging funktion med konfigureret level filtering
#'
#' @description
#' Central logging funktion der håndterer alle log beskeder med automatisk
#' level filtering baseret på SPC_LOG_LEVEL environment variabel.
#'
#' @param message Besked der skal logges (karakter string)
#' @param level Log level som string. Gyldige værdier: "DEBUG", "INFO", "WARN", "ERROR"
#' @param component Valgfri komponent tag for organisering (f.eks. "AUTO_DETECT", "FILE_UPLOAD")
#'
#' @return Returnerer invisible(NULL). Beskeder skrives til console hvis level er tilstrækkelig høj
#' @export
#'
#' @details
#' Funktionen checker den aktuelle log level (fra SPC_LOG_LEVEL environment variable)
#' og viser kun beskeder hvis deres level er høj nok. Beskeder formateres med
#' timestamp, level og valgfri komponent tag.
#'
#' @examples
#' # Grundlæggende brug
#' log_msg("System startet", "INFO")
#'
#' # Med komponent tag
#' log_msg("Data læst succesfuldt", "INFO", "FILE_UPLOAD")
#'
#' # Debug besked (kun vist hvis SPC_LOG_LEVEL=DEBUG)
#' log_msg("Processerer række 42", "DEBUG", "DATA_PROC")
log_msg <- function(message, level = "INFO", component = NULL) {
  current_level <- get_log_level()
  msg_level <- LOG_LEVELS[[toupper(level)]]

  if (is.null(msg_level) || msg_level < current_level) {
    return(invisible(NULL))
  }

  timestamp <- format(Sys.time(), "%H:%M:%S")
  component_str <- if (!is.null(component)) paste0("[", component, "] ") else ""

  cat(sprintf("[%s] %s: %s%s\n",
              timestamp,
              toupper(level),
              component_str,
              message))
}

#' Log debug besked (kun vist ved DEBUG log level)
#'
#' @description Convenience funktion til logging af debug beskeder.
#'
#' @param message Debug besked der skal logges
#' @param component Valgfri komponent tag (f.eks. "AUTO_DETECT")
#' @export
#'
#' @examples
#' # Sæt debug level først
#' Sys.setenv(SPC_LOG_LEVEL = "DEBUG")
#' log_debug("Processerer data række", "DATA_PROC")
log_debug <- function(message, component = NULL) {
  log_msg(message, "DEBUG", component)
}

#' Log information besked
#'
#' @description Convenience funktion til logging af information beskeder.
#'
#' @param message Information besked der skal logges
#' @param component Valgfri komponent tag (f.eks. "FILE_UPLOAD")
#' @export
#'
#' @examples
#' log_info("Fil uploaded succesfuldt", "FILE_UPLOAD")
log_info <- function(message, component = NULL) {
  log_msg(message, "INFO", component)
}

#' Log warning besked
#'
#' @description Convenience funktion til logging af advarsels beskeder.
#'
#' @param message Warning besked der skal logges
#' @param component Valgfri komponent tag (f.eks. "DATA_VALIDATION")
#' @export
#'
#' @examples
#' log_warn("Manglende data i kolonne", "DATA_VALIDATION")
log_warn <- function(message, component = NULL) {
  log_msg(message, "WARN", component)
}

#' Log error besked
#'
#' @description Convenience funktion til logging af fejl beskeder.
#'
#' @param message Error besked der skal logges
#' @param component Valgfri komponent tag (f.eks. "ERROR_HANDLING")
#' @export
#'
#' @examples
#' log_error("Kunne ikke læse fil", "FILE_UPLOAD")
log_error <- function(message, component = NULL) {
  log_msg(message, "ERROR", component)
}