# GLOBAL KONFIGURATIONSFIL ================================================

# BIBLIOTEKER OG DEPENDENCIES --------------------------------

library(shiny)
library(bslib) # Til page_navbar, card, sidebar etc.
library(qicharts2)
library(excelR) # Til Excel-lignende redigerbare tabeller
library(dplyr)
library(ggplot2)
library(ggrepel) # Til kommentar labels på plots
library(stringi)
library(readr)
library(readxl)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(zoo) # Til rullende gennemsnit i beregnede felter
library(scales) # Til procent-formatering i plots
library(rlang) # Til %||% operatoren
library(lubridate)
library(openxlsx) # Til Excel export funktionalitet
library(yaml) # Til læsning af brand.yml
library(shinylogs) # Advanced web-based logging and monitoring
library(later) # Til forsinket udførelse

# SOURCE UTILITIES --------------------------------
# Robust sourcing that works from different working directories
source_from_base <- function(relative_path) {
  # Multiple strategies to find base directory
  base_dir <- getwd()

  # If we're in tests/testthat, go up two levels
  if (basename(base_dir) == "testthat" && basename(dirname(base_dir)) == "tests") {
    base_dir <- dirname(dirname(base_dir))
  }

  # Try direct path from current working dir
  full_path <- file.path(base_dir, relative_path)
  if (file.exists(full_path)) {
    source(full_path)
    return(invisible(TRUE))
  }

  # If that fails and we're potentially in a test environment, try going up directories
  if (grepl("tests?", base_dir, ignore.case = TRUE)) {
    parent_dir <- dirname(base_dir)
    full_path_parent <- file.path(parent_dir, relative_path)
    if (file.exists(full_path_parent)) {
      source(full_path_parent)
      return(invisible(TRUE))
    }
  }

  # Final fallback - try relative path as is
  if (file.exists(relative_path)) {
    source(relative_path)
    return(invisible(TRUE))
  }

  # If nothing works, give descriptive error
  stop(paste("Could not find file:", relative_path, "from directory:", base_dir))
}

source_from_base("R/utils/logging.R")
source_from_base("R/utils/dependency_injection.R")  # Dependency injection framework
# DELETED: utils_app_setup.R - dependencies moved to DESCRIPTION
source_from_base("R/utils/shinylogs_config.R")  # Advanced web-based logging with shinylogs
source_from_base("R/utils/advanced_debug.R")  # Advanced debug infrastructure
source_from_base("R/utils/end_to_end_debug.R")  # PHASE 8: Enhanced debugging for comprehensive testing
# constants.R removed - all constants now in modular config files

# CONFIGURATION - modularized from global.R
source_from_base("R/config/hospital_branding.R")
source_from_base("R/config/chart_types.R")
source_from_base("R/config/observer_priorities.R")
source_from_base("R/config/state_management.R")
source_from_base("R/config/ui_config.R")
source_from_base("R/config/spc_config.R")
source_from_base("R/config/system_config.R")

# CORE FUNCTIONS - moved from runtime to global initialization
# REMOVED: R/fct_chart_helpers.R - functions moved to fct_spc_helpers.R and fct_spc_plot_generation.R
source_from_base("R/core/spc_helpers.R")
source_from_base("R/fct_spc_plot_generation.R")
source_from_base("R/core/file_io.R")
source_from_base("R/core/autodetect_helpers.R")
# REMOVED: R/fct_data_validation.R - functionality integrated into R/fct_spc_helpers.R
# DELETED: utils_local_storage_js.R - functionality moved to www/local-storage.js
source_from_base("R/utils/local_storage.R")

# SERVER COMPONENTS - moved from runtime to global initialization
source_from_base("R/server/utils_session_helpers.R")
source_from_base("R/server/utils_server_management.R")
source_from_base("R/server/utils_column_management.R")
source_from_base("R/fct_file_operations.R")
source_from_base("R/fct_visualization_server.R")
source_from_base("R/modules/mod_spc_chart_server.R")

# PERFORMANCE OPTIMIZATIONS - reactive consolidation og caching
source_from_base("R/server/performance_helpers.R")
source_from_base("R/server/performance_optimizations.R")
source_from_base("R/server/plot_optimizations.R")

# UI COMPONENTS - moved from runtime to global initialization
source_from_base("R/modules/mod_spc_chart_ui.R")
# DELETED: mod_session_storage.R - functionality integrated into session helpers
# DELETED: mod_data_upload.R - functionality integrated into file upload system

# MAIN APP COMPONENTS - moved from runtime to global initialization
source_from_base("R/server/app_server.R")
source_from_base("R/ui/app_ui.R")
source_from_base("R/run_app.R")

# ENHANCED DEBUGGING UTILITIES --------------------------------
# Enhanced reactive context logging for Shiny fejlidentifikation
log_reactive_context <- function(message, component = "REACTIVE", reactive_name = NULL) {
  if (SHINY_DEBUG_MODE) {
    context_info <- ""
    if (!is.null(reactive_name)) {
      context_info <- paste0(" [", reactive_name, "]")
    }

    # Attempt to get reactive context info
    tryCatch({
      is_reactive_context <- isolate({ TRUE })
      context_info <- paste0(context_info, " (in reactive)")
    }, error = function(e) {
      context_info <- paste0(context_info, " (outside reactive)")
    })

    log_debug(paste0(message, context_info), component)
  }
}

# State consistency validator for dual-state debugging
validate_state_consistency <- function(values, app_state) {
  if (!SHINY_DEBUG_MODE) return(TRUE)

  inconsistencies <- c()

  # Check current_data sync
  if (exists("app_state") && !is.null(app_state) && !is.null(app_state$data)) {
    if (!is.null(values$current_data) && !is.null(app_state$data$current_data)) {
      if (!identical(values$current_data, app_state$data$current_data)) {
        inconsistencies <- c(inconsistencies, "current_data mismatch")
      }
    }
  }

  # Check file_uploaded sync
  if (exists("app_state") && !is.null(app_state) && !is.null(app_state$session)) {
    if (!is.null(values$file_uploaded) && !is.null(app_state$session$file_uploaded)) {
      if (values$file_uploaded != app_state$session$file_uploaded) {
        inconsistencies <- c(inconsistencies, "file_uploaded mismatch")
      }
    }
  }

  if (length(inconsistencies) > 0) {
    log_debug(paste("State inconsistencies found:", paste(inconsistencies, collapse = ", ")), "STATE_VALIDATOR")
  }

  return(length(inconsistencies) == 0)
}

# UDVIKLINGSINDSTILLINGER --------------------------------

## Testmodus -----
# TEST MODE: Auto-indlæs eksempeldata til qic() fejlfinding
# Smart environment detection for sikker default-adfærd

# Detect deployment environment
detect_environment <- function() {
  # Check for explicit environment variable first
  env_var <- Sys.getenv("TEST_MODE_AUTO_LOAD", "")
  if (env_var != "") {
    return(as.logical(env_var))
  }

  # Production environments (safe default: FALSE)
  if (Sys.getenv("SHINY_SERVER_VERSION") != "" ||     # Shiny Server
      Sys.getenv("CONNECT_SERVER") != "" ||           # Posit Connect
      Sys.getenv("SHINYAPPS_SERVER") != "" ||         # shinyapps.io
      Sys.getenv("R_CONFIG_ACTIVE") == "production") { # Explicit production
    return(FALSE)
  }

  # Development environments (convenient default: TRUE)
  if (Sys.getenv("RSTUDIO") == "1" ||                 # RStudio IDE
      interactive() ||                                # Interactive R session
      Sys.getenv("R_CONFIG_ACTIVE") == "development") { # Explicit development
    return(TRUE)
  }

  # Test environments and unknown contexts (safe default: FALSE)
  return(FALSE)
}

TEST_MODE_AUTO_LOAD <- detect_environment()

# Specificer hvilken fil der skal indlæses automatisk i test mode
# Filsti skal være relativ til app root-mappen
# TEST_MODE_FILE_PATH <- "R/data/spc_exampledata1.csv"

# Alternative test filer (udkommenterede):
TEST_MODE_FILE_PATH <- "R/data/spc_exampledata.csv"
# TEST_MODE_FILE_PATH <- "R/data/test_infection.csv"
# TEST_MODE_FILE_PATH <- "R/data/SPC_test_data_forskellige.xlsx"

## Auto-gendannelse -----
# AUTO-RESTORE: Gendan automatisk tidligere sessioner
# Sæt til FALSE under udvikling, TRUE til produktion
AUTO_RESTORE_ENABLED <- FALSE

## Enhanced debugging til Shiny-kontekst fejl -----
# SHINY_DEBUG_MODE: Enhanced debugging for fejlidentifikation
SHINY_DEBUG_MODE <- Sys.getenv("SHINY_DEBUG_MODE", "FALSE") == "TRUE"

## Tabeltype -----
# TABLE TYPE: Bruger excelR til Excel-lignende redigerbare tabeller

# HJÆLPEFUNKTIONER --------------------------------

source_from_base("R/utils/danish_locale.R")
source_from_base("R/ui/utils_ui_helpers.R")
source_from_base("R/ui/utils_ui_components.R")
source_from_base("R/ui/utils_ui_updates.R")
source_from_base("R/server/utils_event_system.R")
source_from_base("R/server/utils_server_management.R")
source_from_base("R/utils/performance.R")
source_from_base("R/utils/memory_management.R")

# UNIFIED AUTODETECT ENGINE
source_from_base("R/fct_autodetect_unified.R")    # Main unified autodetect engine
# autodetect helpers now loaded in core section above

# Hospital branding and themes moved to R/config/hospital_branding.R

# Chart types moved to R/config/chart_types.R

## Akseenhedsvalg -----

# Y_AXIS_UNITS_DA now defined in R/config/spc_config.R to avoid duplication

# DATABEHANDLING ================================

## Standardkolonner hjælpefunktion -----
# Sikrer at kun nødvendige kontrol-kolonner er til stede
ensure_standard_columns <- function(data) {
  # Kun Skift og Frys kolonner tilføjes automatisk (nødvendige for SPC kontrol)
  required_cols <- c("Skift", "Frys")

  # Tilføj kun påkrævede kontrol-kolonner
  for (col in required_cols) {
    if (!col %in% names(data)) {
      data[[col]] <- FALSE
    }
  }

  # Organiser kolonner: Skift og Frys først, derefter brugerens originale data
  user_cols <- setdiff(names(data), required_cols)
  final_order <- c(required_cols, user_cols)

  # Returner data med korrekt kolonnerækkefølge
  return(data[, final_order, drop = FALSE])
}

# VALIDERINGSFUNKTIONER ================================

## Numerisk kolonnevalidering -----
validate_numeric_column <- function(data, column_name) {
  if (!column_name %in% names(data)) {
    return(paste("Kolonne", column_name, "ikke fundet"))
  }
  if (!is.numeric(data[[column_name]])) {
    return(paste("Kolonne", column_name, "skal være numerisk"))
  }
  return(NULL)
}

# Observer priorities moved to R/config/observer_priorities.R

# FEJLHÅNDTERING HJÆLPEFUNKTIONER ================================

## Centraliseret error logging - DEPRECATED - bruge R/utils_logging.R i stedet

## Robust date parsing med standardiseret fejlhåndtering
safe_date_parse <- function(data, locale = "da_DK.UTF-8", operation_name = "date parsing") {
  tryCatch(
    {
      result <- suppressWarnings(lubridate::dmy(data, locale = locale))
      success_rate <- sum(!is.na(result)) / length(result)

      list(
        data = result,
        success_rate = success_rate,
        success = success_rate > 0.7,
        parsed_count = sum(!is.na(result)),
        total_count = length(result)
      )
    },
    error = function(e) {
      log_error(paste(operation_name, "fejlede:", e$message), "DATE_PARSING")
      list(
        data = rep(as.POSIXct(NA), length(data)),
        success_rate = 0,
        success = FALSE,
        parsed_count = 0,
        total_count = length(data)
      )
    }
  )
}

#' Sikker udførelse af kritiske operationer med fejlhåndtering
#'
#' @description
#' Standard wrapper til kritiske operationer der kræver robust fejlhåndtering.
#' Fanger fejl, logger dem med det nye logging system og returnerer fallback værdi.
#'
#' @param operation_name Beskrivende navn på operationen (bruges i fejl logs)
#' @param code R kode der skal udføres sikkert
#' @param fallback Værdi der returneres hvis operationen fejler (default: NULL)
#' @param session Shiny session objekt (ikke implementeret endnu)
#' @param show_user Om fejl skal vises til bruger (ikke implementeret endnu)
#'
#' @return Resultat af code hvis succesfuld, ellers fallback værdi
#' @export
#'
#' @details
#' Funktionen bruger tryCatch til at fange fejl og logger automatisk fejl
#' med ERROR level til logging systemet med "ERROR_HANDLING" komponent.
#'
#' @examples
#' # Sikker udførelse af potentielt farlig operation
#' result <- safe_operation(
#'   "Division operation",
#'   code = 10 / 0,
#'   fallback = NA_real_
#' )
#'
#' # Sikker fil læsning
#' data <- safe_operation(
#'   "Læs CSV fil",
#'   code = read.csv("ikke_eksisterende_fil.csv"),
#'   fallback = data.frame()
#' )
safe_operation <- function(operation_name, code,
                          fallback = NULL,
                          session = NULL,
                          show_user = FALSE,
                          error_type = "general",
                          emit = NULL,
                          app_state = NULL) {
  tryCatch(
    {
      code
    },
    error = function(e) {
      # Log error
      log_error(
        paste(operation_name, "fejlede:", e$message),
        "ERROR_HANDLING"
      )

      # Update error state if app_state available
      if (!is.null(app_state)) {
        error_details <- list(
          operation = operation_name,
          message = e$message,
          type = error_type,
          timestamp = Sys.time(),
          session_id = if(!is.null(session)) session$token else NULL
        )

        # Update error state
        app_state$errors$last_error <- error_details
        app_state$errors$error_count <- app_state$errors$error_count + 1L

        # Add to error history (keep max 10)
        current_history <- app_state$errors$error_history
        if (length(current_history) >= 10) {
          current_history <- current_history[2:10]  # Remove oldest
        }
        app_state$errors$error_history <- c(current_history, list(error_details))
      }

      # Emit error event if emit API available
      if (!is.null(emit)) {
        # Choose specific error event based on type
        if (error_type == "validation") {
          emit$validation_error()
        } else if (error_type == "processing") {
          emit$processing_error()
        } else if (error_type == "network") {
          emit$network_error()
        } else {
          emit$error_occurred()
        }
      }

      # User feedback via unified pattern
      if (show_user && !is.null(session)) {
        showNotification(
          paste("Fejl:", operation_name),
          type = "error",
          duration = 5
        )
      }

      if (is.function(fallback)) {
        return(fallback(e))
      }
      return(fallback)
    }
  )
}

# DEVELOPMENT LOG LEVEL - Sæt kun automatisk niveau hvis ikke allerede defineret
if (!nzchar(Sys.getenv("SPC_LOG_LEVEL", ""))) {
  Sys.setenv(SPC_LOG_LEVEL = "INFO")
}

# REACTIVE PERFORMANCE HJÆLPEFUNKTIONER =============================

## Debounced reactive for expensive operationer - bruger Shiny native debounce
create_debounced_reactive <- function(reactive_expr, millis = 1000) {
  # Brug Shiny's built-in debounce() som følger best practices
  # Dette eliminerer behovet for manual timer management
  return(debounce(reactive_expr, millis = millis))
}

# NOTE: setup_session_cleanup moved to R/utils_memory_management.R for Fase 5

## Observer manager til tracking og cleanup
observer_manager <- function() {
  observers <- list()

  list(
    add = function(observer, name = NULL) {
      id <- if (is.null(name)) length(observers) + 1 else name
      observers[[id]] <<- observer
      return(id)
    },
    remove = function(id) {
      if (id %in% names(observers)) {
        if (!is.null(observers[[id]]$destroy)) {
          observers[[id]]$destroy()
        }
        observers[[id]] <<- NULL
      }
    },
    cleanup_all = function() {
      for (id in names(observers)) {
        if (!is.null(observers[[id]]$destroy)) {
          tryCatch(
            {
              observers[[id]]$destroy()
            },
            error = function(e) {
              log_error(paste("Observer cleanup fejl for", id, ":", e$message), "OBSERVER_MGMT")
            }
          )
        }
      }
      observers <<- list()
    },
    count = function() length(observers)
  )
}

# State management moved to R/config/state_management.R
