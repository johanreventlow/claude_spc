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
library(waiter)
library(yaml) # Til læsning af brand.yml
library(later) # Til forsinket udførelse

# SOURCE UTILITIES --------------------------------
source("R/utils_logging.R")
source("R/utils_advanced_debug.R")  # Advanced debug infrastructure
source("R/constants.R")

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
# Sæt til FALSE for at deaktivere auto-indlæsning og vende tilbage til normal brugerstyret dataindlæsning
TEST_MODE_AUTO_LOAD <- FALSE

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

source("R/utils_danish_locale.R")
source("R/utils_ui_helpers.R")
source("R/utils_ui_components.R")
source("R/utils_server_management.R")
source("R/utils_performance.R")
source("R/utils_memory_management.R")

# HOSPITAL BRANDING ================================

## Brand.yml konfiguration -----
# Læs brand.yml konfiguration
brand_config <- yaml::read_yaml("_brand.yml")

# Opret tema baseret på brand.yml (auto-opdaget som _brand.yml)
my_theme <- bs_theme(brand = "_brand.yml")

## Hospital information -----
HOSPITAL_NAME <- brand_config$meta$name
HOSPITAL_LOGO_PATH <- brand_config$logo$image

## Hospital farver -----
# Hent ALLE farver fra brand.yml (via bs_theme)
HOSPITAL_COLORS <- list(
  primary = brand_config$color$palette$primary,
  secondary = brand_config$color$palette$secondary,
  accent = brand_config$color$palette$accent, # Fra brand.yml palette
  success = brand_config$color$palette$success,
  warning = brand_config$color$palette$warning,
  danger = brand_config$color$palette$danger,
  info = brand_config$color$palette$info,
  light = brand_config$color$palette$light,
  dark = brand_config$color$palette$dark,
  hospitalblue = brand_config$color$palette$hospitalblue,
  darkgrey = brand_config$color$palette$darkgrey,
  lightgrey = brand_config$color$palette$lightgrey,
  mediumgrey = brand_config$color$palette$mediumgrey,
  regionhblue = brand_config$color$palette$regionhblue
)

# WAITER KONFIGURATION ================================

## Hospital branding til loading screens -----
WAITER_CONFIG <- list(
  # App start loading - FULDSKÆRM OVERLAY
  app_start = list(
    html = tagList(
      div(
        style = "text-align: center; padding: 50px;",
        img(src = basename(HOSPITAL_LOGO_PATH), height = "80px", style = "margin-bottom: 30px;"),
        h2(paste(HOSPITAL_NAME, "SPC App"),
          style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 300; margin-bottom: 20px;")
        ),
        h4("Indlæser...",
          style = paste("color:", HOSPITAL_COLORS$secondary, "; font-weight: 300; margin-bottom: 30px;")
        ),
        waiter::spin_6(), # Hospital-venligt spinner
        br(), br(), br(),
        p("Vent venligst mens appen initialiseres",
          style = paste("color:", HOSPITAL_COLORS$secondary, "; font-size: 0.9rem;")
        )
      )
    )
  ),

  # File upload loading
  file_upload = list(
    html = tagList(
      h4("Behandler fil...", style = paste("color:", HOSPITAL_COLORS$primary)),
      br(),
      waiter::spin_3(),
      br(), br(),
      p("Læser og validerer data", style = paste("color:", HOSPITAL_COLORS$secondary))
    ),
    color = "rgba(255,255,255,0.9)"
  ),

  # Plot generation loading
  plot_generation = list(
    html = tagList(
      h4("Genererer SPC graf...", style = paste("color:", HOSPITAL_COLORS$primary)),
      br(),
      waiter::spin_4(),
      br(), br(),
      p("Anvender Anhøj-regler og beregner kontrolgrænser",
        style = paste("color:", HOSPITAL_COLORS$secondary, "; font-size: 0.9rem;")
      )
    ),
    color = "rgba(248,249,250,0.95)"
  )
)

# GRAFIK TEMAER ================================

## ggplot2 hospital tema -----
HOSPITAL_THEME <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(color = HOSPITAL_COLORS$primary, size = 14, face = "bold"),
      plot.subtitle = element_text(color = HOSPITAL_COLORS$secondary, size = 12),
      axis.title = element_text(color = HOSPITAL_COLORS$dark, size = 11),
      axis.text = element_text(color = HOSPITAL_COLORS$dark, size = 10),
      legend.title = element_text(color = HOSPITAL_COLORS$dark, size = 11),
      legend.text = element_text(color = HOSPITAL_COLORS$dark, size = 10),
      panel.grid.major = element_line(color = HOSPITAL_COLORS$light),
      panel.grid.minor = element_line(color = HOSPITAL_COLORS$light),
      strip.text = element_text(color = HOSPITAL_COLORS$primary, face = "bold")
    )
}

## Standard footer til alle grafer -----
create_plot_footer <- function(afdeling = "", data_kilde = "", dato = Sys.Date()) {
  paste0(
    HOSPITAL_NAME,
    if (afdeling != "") paste0(" - ", afdeling) else "",
    " | Datakilde: ", data_kilde,
    " | Genereret: ", format(dato, "%d-%m-%Y"),
    " | SPC analyse med Anhøj-regler"
  )
}

# DIAGRAM TYPER ================================

## Dansk oversættelse af chart typer -----
CHART_TYPES_DA <- list(
  "Seriediagram med SPC (Run Chart)" = "run",
  "I-kort (Individuelle værdier)" = "i",
  "MR-kort (Moving Range)" = "mr",
  "P-kort (Andele)" = "p",
  "P'-kort (Andele, standardiseret)" = "pp",
  "U-kort (Rater)" = "u",
  "U'-kort (Rater, standardiseret)" = "up",
  "C-kort (Tællinger)" = "c",
  "G-kort (Tid mellem hændelser)" = "g"
)

## Omvendt mapping til engelske koder -----
CHART_TYPES_EN <- list(
  "run" = "run",
  "i" = "i",
  "mr" = "mr",
  "p" = "p",
  "pp" = "pp",
  "u" = "u",
  "up" = "up",
  "c" = "c",
  "g" = "g"
)

## Hjælpefunktion til konvertering -----
# Konverter danske displaynavne til engelske qic-koder
get_qic_chart_type <- function(danish_selection) {
  if (is.null(danish_selection) || danish_selection == "") {
    return("run") # standard
  }

  # Hvis det allerede er en engelsk kode, returner som-den-er
  if (danish_selection %in% unlist(CHART_TYPES_EN)) {
    return(danish_selection)
  }

  # Slå op i dansk-til-engelsk mapping
  english_code <- CHART_TYPES_DA[[danish_selection]]
  if (!is.null(english_code)) {
    return(english_code)
  }

  # Særlig håndtering for eksakte match
  if (danish_selection == "Seriediagram med SPC (Run Chart)") {
    return("run")
  }
  if (danish_selection == "P-kort (Andele)") {
    return("p")
  }

  # Fallback
  return("run")
}

## Akseenhedsvalg -----

# Y-akse enheder (værdier)
Y_AXIS_UNITS_DA <- list(
  "Antal" = "count",
  "Procent (%)" = "percent",
  "Promille (‰)" = "permille",
  "Rate pr. 1000" = "rate_1000",
  "Rate pr. 100.000" = "rate_100000",
  "Dage" = "days",
  "Timer" = "hours",
  "Gram" = "grams",
  "Kilogram" = "kg",
  "Kroner" = "dkk"
)

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

# OBSERVER MANAGEMENT ================================

## Observer Priorities -----
# Phase 3: Prioriteret observer eksekveringsrækkefølge
OBSERVER_PRIORITIES <- list(
  # Høj prioritet - kritisk state management
  STATE_MANAGEMENT = 1000,

  # Medium prioritet - data processing
  AUTO_DETECT = 800,
  DATA_PROCESSING = 700,

  # Lav prioritet - UI updates og visuel feedback
  UI_SYNC = 500,
  PLOT_GENERATION = 400,
  STATUS_UPDATES = 300,

  # Meget lav prioritet - cleanup og logging
  CLEANUP = 100,
  LOGGING = 50
)

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
safe_operation <- function(operation_name, code, fallback = NULL, session = NULL, show_user = FALSE) {
  tryCatch(
    {
      code
    },
    error = function(e) {
      log_error(
        paste(operation_name, "fejlede:", e$message),
        "ERROR_HANDLING"
      )
      return(fallback)
    }
  )
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

# CENTRALISERET STATE MANAGEMENT - FASE 4 ================================

## App State Schema -----
#' Opret centraliseret applikations state struktur (Phase 4)
#'
#' @description
#' Opretter det centraliserede state management schema for SPC App.
#' Dette implementerer Phase 4 arkitekturen med single source of truth
#' for al applikations tilstand.
#'
#' @return Liste med følgende hovedsektioner:
#' \describe{
#'   \item{data}{Data management state (current_data, original_data, fil info, flags)}
#'   \item{columns}{Kolonne management (auto-detect, mappings, UI sync)}
#'   \item{test_mode}{Test mode konfiguration}
#'   \item{session}{Session management (save state, file upload status)}
#'   \item{ui}{UI preferencer og indstillinger}
#' }
#' @export
#'
#' @details
#' Centraliseret state structure:
#'
#' **Data Management:**
#' - current_data: Det aktive dataset
#' - original_data: Backup af oprindelige data
#' - updating_table: Flag for table operation status
#' - table_version: Versioning for optimistic updates
#'
#' **Column Management:**
#' - auto_detect: Auto-detection progress og resultater
#' - mappings: X/Y/N/CL kolonne mappings for SPC
#' - ui_sync: UI synchronization state
#'
#' **Session Management:**
#' - auto_save_enabled: Automatisk save funktionalitet
#' - file_uploaded: Track fil upload status
#' - user_started_session: Bruger session state
#'
#' @examples
#' # Opret standard app state
#' app_state <- create_app_state()
#'
#' # Tjek state struktur
#' names(app_state)  # "data", "columns", "test_mode", "session", "ui"
#'
#' # Opdater data
#' app_state$data$current_data <- data.frame(Dato = Sys.Date(), Værdi = 10)
#' app_state$columns$mappings$x_column <- "Dato"
#'
#' @seealso
#' - ARCHITECTURE_OVERVIEW.md for Phase 4 detaljer
#' - test-phase4-centralized-state.R for eksempler
create_app_state <- function() {
  # Create environment-based state for by-reference sharing
  # CRITICAL FIX: Environment passes by reference, solving scope isolation
  app_state <- new.env(parent = emptyenv())

  cat("DEBUG: [CREATE_APP_STATE] Environment created with address:", capture.output(print(app_state)), "\n")

  # Data Management
  app_state$data <- list(
    current_data = NULL,
    original_data = NULL,
    file_info = NULL,
    updating_table = FALSE,
    table_operation_in_progress = FALSE,
    table_operation_cleanup_needed = FALSE,
    table_version = 0
  )

  # Column Management - centraliseret
  app_state$columns <- list(
    # Auto-detection state
    auto_detect = list(
      in_progress = FALSE,
      completed = FALSE,
      trigger = NULL,
      results = NULL
    ),

    # Column mappings
    mappings = list(
      x_column = NULL,
      y_column = NULL,
      n_column = NULL,
      cl_column = NULL
    ),

    # UI sync state
    ui_sync = list(
      needed = NULL,
      last_sync_time = NULL
    )
  )

  # Test Mode
  app_state$test_mode <- list(
    auto_detect_ready = FALSE
  )

  # Session Management
  app_state$session <- list(
    auto_save_enabled = TRUE,
    restoring_session = FALSE,
    file_uploaded = FALSE,
    user_started_session = FALSE,
    last_save_time = NULL,
    file_name = NULL
  )

  # UI State
  app_state$ui <- list(
    hide_anhoej_rules = FALSE
  )

  return(app_state)
}

## Dato kolonnevalidering -----
validate_date_column <- function(data, column_name) {
  if (!column_name %in% names(data)) {
    return(paste("Kolonne", column_name, "ikke fundet"))
  }
  # Forsøg at konvertere til dato hvis det ikke allerede er det
  tryCatch(
    {
      as.Date(data[[column_name]])
      return(NULL)
    },
    error = function(e) {
      return(paste("Kolonne", column_name, "kunne ikke konverteres til dato"))
    }
  )
}
