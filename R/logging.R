# utils_logging.R
# Konfigurerbart logging system til SPC App (Shiny-sikkert)

#' Log levels liste til SPC App logging system
#'
#' @description
#' Definerer numeriske værdier for forskellige log-niveauer til brug i det
#' konfigurerede logging-system. **Lavere tal betyder højere prioritet.**
#'
#' @details
#' Log levels:
#' - DEBUG (1): Detaljeret debug-information
#' - INFO  (2): Generel information
#' - WARN  (3): Advarsler
#' - ERROR (4): Fejlbeskeder
#'
#' @format Liste med 4 elementer
#' @export
LOG_LEVELS <- list(
  DEBUG = 1L,
  INFO  = 2L,
  WARN  = 3L,
  ERROR = 4L
)

# intern hjælper (ikke-eksporteret)
.level_name <- function(x) {
  inv <- setNames(names(LOG_LEVELS), unlist(LOG_LEVELS, use.names = FALSE))
  inv[as.character(x)] %||% "INFO"
}

# intern hjælper (ikke-eksporteret)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

#' Hent aktuel log level fra environment variabel
#'
#' @description
#' Læser `SPC_LOG_LEVEL` fra environment og returnerer tilsvarende numeriske værdi.
#' Understøtter både navne (f.eks. `"DEBUG"`) og tal (f.eks. `"1"`).
#' Fald tilbage til `INFO` ved ugyldig værdi.
#'
#' @return Heltalsværdi svarende til log-niveau (se `LOG_LEVELS`)
#' @export
#'
#' @examples
#' get_log_level()
#' Sys.setenv(SPC_LOG_LEVEL = "DEBUG")
#' get_log_level()
#' Sys.setenv(SPC_LOG_LEVEL = "1")
#' get_log_level()
get_log_level <- function() {
  env_raw <- Sys.getenv("SPC_LOG_LEVEL", "INFO")
  env_val <- trimws(toupper(as.character(env_raw)))

  lvl_num <-
    if (nzchar(env_val) && !is.na(suppressWarnings(as.integer(env_val)))) {
      as.integer(env_val)
    } else {
      LOG_LEVELS[[env_val]]
    }

  if (is.null(lvl_num) || is.na(lvl_num)) LOG_LEVELS$INFO else lvl_num
}

# intern hjælper (ikke-eksporteret)
.should_log <- function(level_chr) {
  lvl <- LOG_LEVELS[[toupper(level_chr)]]
  if (is.null(lvl)) return(FALSE)
  cur <- get_log_level()
  lvl >= cur
}

# intern hjælper (ikke-eksporteret)
.safe_format <- function(x) {
  # Direct tryCatch to avoid circular dependency with safe_operation
  tryCatch(
    {
      if (is.null(x)) return("NULL")
      if (is.character(x)) return(paste(x, collapse = " "))
      if (is.atomic(x)) {
        if (length(x) > 10) {
          return(paste0(
            paste(utils::head(x, 10), collapse = " "),
            " … (n=", length(x), ")"
          ))
        } else {
          return(paste(x, collapse = " "))
        }
      }
      if (is.data.frame(x)) {
        return(sprintf("<data.frame: %d x %d> cols=[%s%s]",
          nrow(x), ncol(x),
          paste(utils::head(names(x), 6), collapse = ", "),
          if (ncol(x) > 6) ", …" else ""))
      }
      if (is.list(x)) {
        nms <- names(x) %||% rep("", length(x))
        shown <- utils::head(seq_along(x), 6)
        keys <- ifelse(nchar(nms[shown]) > 0, nms[shown], shown)
        return(sprintf("<list: %d> [%s%s]",
          length(x),
          paste(keys, collapse = ", "),
          if (length(x) > 6) ", …" else ""))
      }
      paste(capture.output(utils::str(x, max.level = 1, vec.len = 10, give.attr = FALSE)),
        collapse = " ")
    },
    error = function(e) {
      "<FORMAT_ERROR>"
    })
}

# intern hjælper (ikke-eksporteret)
.safe_collapse <- function(args_list) {
  parts <- lapply(args_list, .safe_format)
  paste(unlist(parts, use.names = FALSE), collapse = " ")
}

# intern hjælper (ikke-eksporteret)
.component_or_fallback <- function(x) x %||% "UNSPECIFIED"

# intern hjælper (ikke-eksporteret)
.timestamp <- function() format(Sys.time(), "%H:%M:%S")

#' Primær logging-funktion med level-filtering
#'
#' @description
#' Central logging-funktion der håndterer alle log-beskeder med automatisk
#' level-filtering baseret på `SPC_LOG_LEVEL`.
#'
#' @param message Besked (karakter/string) der skal logges
#' @param level Log-niveau som string. Gyldige værdier: `"DEBUG"`, `"INFO"`, `"WARN"`, `"ERROR"`
#' @param component Valgfrit komponent-tag for organisering (f.eks. `"AUTO_DETECT"`, `"FILE_UPLOAD"`)
#'
#' @return `invisible(NULL)`. Beskeder skrives til konsol hvis niveauet tillader det.
#' @export
#'
#' @examples
#' log_msg("System startet", "INFO")
#' log_msg("Data læst", "INFO", "FILE_UPLOAD")
#' Sys.setenv(SPC_LOG_LEVEL = "DEBUG")
#' log_msg("Detaljer", "DEBUG", "DATA_PROC")
log_msg <- function(message, level = "INFO", component = NULL) {
  if (!.should_log(level)) return(invisible(NULL))

  ts <- .timestamp()
  comp <- .component_or_fallback(component)
  comp_str <- if (!is.null(component)) paste0("[", comp, "] ") else ""

  cat(sprintf("[%s] %s: %s%s\n",
    ts,
    toupper(level),
    comp_str,
    as.character(message)))
  invisible(NULL)
}

#' Log debug-besked (variadisk og Shiny-sikker)
#'
#' @description
#' Convenience-funktion til logging af DEBUG-beskeder.
#' Accepterer vilkårligt antal argumenter (`...`) og formaterer dem robust
#' (tåler lister, data.frames m.m.) uden at crashe i Shiny renderers.
#'
#' @param ... Variable argumenter der sammenkædes til en debug-besked
#' @param .context Valgfri kontekst-tag (f.eks. `"RENDER_PLOT"`, `"AUTO_DETECT"`)
#'
#' @return `invisible(NULL)`.
#' @export
#'
#' @examples
#' log_debug("Status:", TRUE, .context = "RENDER_PLOT")
#' log_debug("Række:", 42, list(a = 1), .context = "DATA_PROC")
log_debug <- function(..., .context = NULL) {
  if (!.should_log("DEBUG")) return(invisible(NULL))

  component <- .component_or_fallback(.context)

  # Direct tryCatch to avoid circular dependency with safe_operation
  tryCatch(
    {
      msg <- .safe_collapse(list(...))
      if (exists("log_msg", mode = "function")) {
        log_msg(msg, "DEBUG", component = component)
      } else {
        cat(sprintf("[%s] DEBUG: [%s] %s\n", .timestamp(), component, msg))
      }
    },
    error = function(e) {
      # Fejlsikker fallback – må ALDRIG vælte Shiny-renderers
      try(message("[LOGGING_ERROR] Could not format debug message"), silent = TRUE)
    })

  invisible(NULL)
}

#' Log information-besked
#'
#' @description
#' Convenience-funktion til logging af INFO-beskeder.
#'
#' @param message Besked der skal logges
#' @param component Valgfri komponent-tag (f.eks. `"FILE_UPLOAD"`)
#'
#' @return `invisible(NULL)`.
#' @export
#'
#' @examples
#' log_info("Fil uploaded succesfuldt", "FILE_UPLOAD")
log_info <- function(message, component = NULL) {
  log_msg(message, "INFO", component)
}

#' Log warning-besked
#'
#' @description
#' Convenience-funktion til logging af WARN-beskeder.
#'
#' @param message Besked der skal logges
#' @param component Valgfri komponent-tag (f.eks. `"DATA_VALIDATION"`)
#'
#' @return `invisible(NULL)`.
#' @export
#'
#' @examples
#' log_warn("Manglende data i kolonne", "DATA_VALIDATION")
log_warn <- function(message, component = NULL) {
  log_msg(message, "WARN", component)
}

#' Log error-besked
#'
#' @description
#' Convenience-funktion til logging af ERROR-beskeder. Accepterer også en
#' `condition` direkte (beskeden udtrækkes med `conditionMessage()`).
#'
#' @param message Besked eller condition der skal logges
#' @param component Valgfri komponent-tag (f.eks. `"ERROR_HANDLING"`)
#'
#' @return `invisible(NULL)`.
#' @export
#'
#' @examples
#' log_error("Kunne ikke læse fil", "FILE_UPLOAD")
#' \dontrun{
#' tryCatch(stop("Boom"), error = function(e) log_error(e, "PIPELINE"))
#' }
log_error <- function(message, component = NULL) {
  msg <- if (inherits(message, "condition")) conditionMessage(message) else message
  log_msg(msg, "ERROR", component)
}

#' Log afgrænsede debug-blokke (start/stop)
#'
#' @description
#' Helper-funktion til logging af visuelt afgrænsede debug-blokke
#' med separatorlinjer. Erstatter hardcodede separatorer i koden.
#'
#' @param context Kontekst-tag for blokken (f.eks. `"COLUMN_MGMT"`, `"AUTO_DETECT"`)
#' @param action Beskrivelse af handlingen (f.eks. `"Starting column detection"`)
#' @param type Type af markering: `"start"`, `"stop"`, eller `"both"` (default `"start"`)
#'
#' @return `invisible(NULL)`.
#' @export
#'
#' @examples
#' log_debug_block("COLUMN_MGMT", "Starting column detection")
#' # ... kode ...
#' log_debug_block("COLUMN_MGMT", "Column detection completed", type = "stop")
log_debug_block <- function(context, action, type = "start") {
  if (!.should_log("DEBUG")) return(invisible(NULL))

  sep <- paste(rep("=", 50), collapse = "")

  ctx <- .component_or_fallback(context)
  t <- match.arg(type, choices = c("start", "stop", "both"))

  if (t %in% c("start", "both")) {
    log_debug(sep, .context = ctx)
    log_debug(action, .context = ctx)
  }
  if (t %in% c("stop", "both")) {
    log_debug(paste0(action, " - completed"), .context = ctx)
    log_debug(sep, .context = ctx)
  }

  invisible(NULL)
}

#' Log strukturerede key-value par (kompakt)
#'
#' @description
#' Helper-funktion til logging af strukturerede key-value data.
#' Understøtter både navngivne `...`-argumenter og en liste via `.list_data`.
#' Værdier formatteres robust (tåler komplekse objekter) uden at crashe i Shiny.
#'
#' @param ... Navngivne argumenter der logges som key-value (`navn = værdi`)
#' @param .context Kontekst-tag (f.eks. `"DATA_PROC"`, `"AUTO_DETECT"`)
#' @param .list_data Valgfri liste med key-value data
#'
#' @return `invisible(NULL)`.
#' @export
#'
#' @examples
#' log_debug_kv(trigger_value = 1, status = "active", .context = "DATA_TABLE")
#' log_debug_kv(.list_data = list(rows = 100, cols = 5), .context = "DATA_PROC")
log_debug_kv <- function(..., .context = NULL, .list_data = NULL) {
  if (!.should_log("DEBUG")) return(invisible(NULL))

  ctx <- .component_or_fallback(.context)

  dots <- list(...)
  if (length(dots) > 0) {
    nms <- names(dots) %||% rep("", length(dots))
    for (i in seq_along(dots)) {
      key <- nms[[i]] %||% paste0("..", i)
      val <- .safe_format(dots[[i]])
      log_debug(paste0(key, ": ", val), .context = ctx)
    }
  }

  if (!is.null(.list_data) && is.list(.list_data)) {
    nms <- names(.list_data) %||% rep("", length(.list_data))
    for (i in seq_along(.list_data)) {
      key <- nms[[i]] %||% paste0("..", i)
      val <- .safe_format(.list_data[[i]])
      log_debug(paste0(key, ": ", val), .context = ctx)
    }
  }

  invisible(NULL)
}

#' Convenience functions for common log level configurations
#'
#' @description
#' Helper functions to easily switch between development and production
#' log level configurations. These set the `SPC_LOG_LEVEL` environment
#' variable for the current R session.
#'
#' @return invisible(NULL). The environment variable is set as a side effect.
#' @export
#'
#' @examples
#' set_log_level_development()  # Enables all DEBUG messages
#' set_log_level_production()   # Only WARN and ERROR in production
#' set_log_level_quiet()        # Only ERROR messages
set_log_level_development <- function() {
  Sys.setenv(SPC_LOG_LEVEL = "DEBUG")
  cat("[LOG_CONFIG] Log level set to DEBUG (development mode)\n")
  invisible(NULL)
}

#' @rdname set_log_level_development
#' @export
set_log_level_production <- function() {
  Sys.setenv(SPC_LOG_LEVEL = "WARN")
  cat("[LOG_CONFIG] Log level set to WARN (production mode)\n")
  invisible(NULL)
}

#' @rdname set_log_level_development
#' @export
set_log_level_quiet <- function() {
  Sys.setenv(SPC_LOG_LEVEL = "ERROR")
  cat("[LOG_CONFIG] Log level set to ERROR (quiet mode)\n")
  invisible(NULL)
}

#' @rdname set_log_level_development
#' @export
set_log_level_info <- function() {
  Sys.setenv(SPC_LOG_LEVEL = "INFO")
  cat("[LOG_CONFIG] Log level set to INFO (standard mode)\n")
  invisible(NULL)
}
