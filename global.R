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

source_from_base("R/utils_logging.R")
# source_from_base("R/utils_app_setup.R")  # App setup - temporarily disabled due to logging dependency issue
source_from_base("R/utils_shinylogs_config.R")  # Advanced web-based logging with shinylogs
source_from_base("R/utils_advanced_debug.R")  # Advanced debug infrastructure
source_from_base("R/utils_end_to_end_debug.R")  # PHASE 8: Enhanced debugging for comprehensive testing
source_from_base("R/constants.R")

# CORE FUNCTIONS - moved from runtime to global initialization
source_from_base("R/fct_chart_helpers.R")
source_from_base("R/fct_spc_helpers.R")
source_from_base("R/fct_spc_plot_generation.R")
source_from_base("R/fct_file_io.R")
source_from_base("R/fct_data_validation.R")
# source_from_base("R/utils_local_storage_js.R")  # Moved to www/local-storage.js
source_from_base("R/utils_local_storage.R")

# SERVER COMPONENTS - moved from runtime to global initialization
source_from_base("R/utils_session_helpers.R")
source_from_base("R/utils_server_management.R")
source_from_base("R/fct_data_processing.R")
source_from_base("R/fct_file_operations.R")
source_from_base("R/fct_visualization_server.R")
source_from_base("R/mod_spc_chart_server.R")

# UI COMPONENTS - moved from runtime to global initialization
source_from_base("R/mod_spc_chart_ui.R")
source_from_base("R/mod_session_storage.R")
source_from_base("R/mod_data_upload.R")

# MAIN APP COMPONENTS - moved from runtime to global initialization
source_from_base("R/app_server.R")
source_from_base("R/app_ui.R")

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
# Respekter environment variable hvis sat, ellers default til FALSE
TEST_MODE_AUTO_LOAD <- as.logical(Sys.getenv("TEST_MODE_AUTO_LOAD", "TRUE"))

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

source_from_base("R/utils_danish_locale.R")
source_from_base("R/utils_ui_helpers.R")
source_from_base("R/utils_ui_components.R")
source_from_base("R/utils_ui_updates.R")
source_from_base("R/utils_event_system.R")
source_from_base("R/utils_server_management.R")
source_from_base("R/utils_performance.R")
source_from_base("R/utils_memory_management.R")

# UNIFIED AUTODETECT ENGINE
source_from_base("R/fct_autodetect_unified.R")    # Main unified autodetect engine
source_from_base("R/fct_autodetect_helpers.R")    # Supporting functions for robust detection

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
# Enhanced with wider gaps to prevent race conditions
OBSERVER_PRIORITIES <- list(
  # Høj prioritet - kritisk state management
  STATE_MANAGEMENT = 2000,    # Critical state operations

  # Medium prioritet - data processing
  AUTO_DETECT = 1500,         # Auto-detection logic
  DATA_PROCESSING = 1250,     # Data operations

  # Lav prioritet - UI updates og visuel feedback
  UI_SYNC = 750,              # UI synchronization
  PLOT_GENERATION = 600,      # Plot rendering
  STATUS_UPDATES = 500,       # Status indicators

  # Meget lav prioritet - cleanup og logging
  CLEANUP = 200,              # Cleanup operations
  LOGGING = 100               # Monitoring and logging
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

# DEVELOPMENT LOG LEVEL - Activate all DEBUG messages during development
set_log_level_development()

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

  log_debug(paste("Environment created with address:", capture.output(print(app_state))), "CREATE_APP_STATE")

  # REACTIVE EVENT BUS: Central event system for all triggers
  # STREAMLINED EVENT-BUS: Reduced noise, consolidated events
  app_state$events <- reactiveValues(
    # DATA-LIVSCYKLUS ------------------------------------------------------
    data_loaded = 0L,                  # Triggeres når ny data er indlæst eller uploadet
    data_changed = 0L,                 # Triggeres ved eksplicit dataændringer

    # KOLONNEDETEKTION -----------------------------------------------------
    auto_detection_started = 0L,
    auto_detection_completed = 0L,
    columns_detected = 0L,

    # UI SYNKRONISERING ----------------------------------------------------
    ui_sync_needed = 0L,
    ui_sync_completed = 0L,
    ui_update_needed = 0L,
    column_choices_changed = 0L,
    navigation_changed = 0L,

    # SESSION LIVSCYKLUS ---------------------------------------------------
    session_started = 0L,
    session_reset = 0L,
    manual_autodetect_button = 0L,
    test_mode_ready = 0L,

    # FEJL- OG GENOPRETTELSESHÅNDTERING -----------------------------------
    error_occurred = 0L,
    validation_error = 0L,
    processing_error = 0L,
    network_error = 0L,
    recovery_completed = 0L,

    # FORM- OG STATEHÅNDTERING --------------------------------------------
    form_reset_needed = 0L,
    form_restore_needed = 0L,
    form_update_needed = 0L
  )

  # Data Management - Simplified unified structure
  app_state$data <- reactiveValues(
    # Core data
    current_data = NULL,
    original_data = NULL,
    processed_data = NULL,
    backup_data = NULL,

    # File metadata
    file_info = NULL,
    file_path = NULL,
    file_encoding = NULL,
    import_settings = NULL,

    # Table operations
    updating_table = FALSE,
    table_operation_in_progress = FALSE,
    table_operation_cleanup_needed = FALSE,
    table_version = 0,
    last_update_time = NULL
  )

  # Column Management - Hierarchical structure with sub-objects
  app_state$columns <- reactiveValues(
    # Auto-detection sub-system
    auto_detect = reactiveValues(
      in_progress = FALSE,
      completed = FALSE,
      results = NULL,
      trigger = NULL,
      last_run = NULL,
      frozen_until_next_trigger = FALSE
    ),

    # Column mappings sub-system
    mappings = reactiveValues(
      x_column = NULL,
      y_column = NULL,
      n_column = NULL,
      cl_column = NULL,
      skift_column = NULL,
      frys_column = NULL,
      kommentar_column = NULL
    ),

    # UI synchronization sub-system
    ui_sync = reactiveValues(
      needed = FALSE,
      last_sync_time = NULL,
      pending_updates = list()
    )
  )

  # Session Management - Simplified unified structure
  app_state$session <- reactiveValues(
    # State management
    auto_save_enabled = TRUE,
    restoring_session = FALSE,
    file_uploaded = FALSE,
    user_started_session = FALSE,

    # File tracking
    file_name = NULL,
    file_path = NULL,
    last_modified = NULL,
    file_size = NULL,

    # Time tracking
    last_save_time = NULL,
    session_start_time = NULL,
    last_activity_time = NULL,

    # Lifecycle tracking
    session_active = TRUE,
    cleanup_initiated = FALSE,
    background_tasks_active = TRUE
  )

  # Test Mode Management
  app_state$test_mode <- reactiveValues(
    enabled = FALSE,
    auto_load = FALSE,
    file_path = NULL,
    data_loaded = FALSE
  )

  # UI State - Convert to reactiveValues for consistency
  app_state$ui <- reactiveValues(
    hide_anhoej_rules = FALSE,
    updating_programmatically = FALSE,    # LOOP PROTECTION: Flag to prevent circular events during UI updates
    last_programmatic_update = NULL,      # LOOP PROTECTION: Timestamp of last programmatic update
    flag_reset_scheduled = FALSE,         # LOOP PROTECTION: Single-reset guarantee flag

    # TOKEN-BASED TRACKING: Advanced loop protection infrastructure
    pending_programmatic_inputs = list(), # TOKEN TRACKING: Map of inputId -> {token, value, timestamp}
    programmatic_token_counter = 0L,      # TOKEN GENERATION: Counter for unique token generation
    queued_updates = list(),              # QUEUE SYSTEM: Queued UI updates for overlapping calls

    # FASE 3: PERFORMANCE MONITORING - Track system performance metrics
    performance_metrics = list(
      total_updates = 0L,                 # METRIC: Total UI updates processed
      queued_updates = 0L,                # METRIC: Total updates that were queued
      tokens_consumed = 0L,               # METRIC: Total tokens consumed
      queue_max_size = 0L,                # METRIC: Maximum queue size reached
      avg_update_duration_ms = 0.0,       # METRIC: Average UI update duration
      last_performance_reset = Sys.time() # METRIC: When metrics were last reset
    ),

    # FASE 3: MEMORY MANAGEMENT - Control memory usage and cleanup
    memory_limits = list(
      max_queue_size = 50L,               # LIMIT: Maximum queue entries allowed
      max_pending_tokens = 100L,          # LIMIT: Maximum pending tokens
      token_cleanup_interval_sec = 300L,  # CLEANUP: Clean tokens every 5 minutes
      performance_reset_interval_sec = 3600L  # RESET: Reset metrics every hour
    )
  )

  # Autodetect State - Unified autodetect engine state management
  app_state$autodetect <- reactiveValues(
    frozen_until_next_trigger = FALSE,
    last_run = NULL,
    in_progress = FALSE,
    completed = FALSE
  )

  # Navigation State - For eventReactive patterns
  app_state$navigation <- reactiveValues(
    trigger = 0  # Counter for triggering navigation-dependent reactives
  )

  # Visualization State - Convert to reactiveValues for consistency
  app_state$visualization <- reactiveValues(
    plot_ready = FALSE,
    plot_warnings = character(0),
    anhoej_results = NULL,
    is_computing = FALSE,
    plot_object = NULL
  )

  # Error State - Convert to reactiveValues for consistency
  app_state$errors <- reactiveValues(
    last_error = NULL,           # Last error details
    error_count = 0L,           # Total error count
    error_history = list(),     # Recent error history (max 10)
    recovery_attempts = 0L,     # Number of recovery attempts
    last_recovery_time = NULL   # Timestamp of last recovery
  )

  # System Management - Infrastructure flags and state
  app_state$system <- reactiveValues(
    event_listeners_setup = FALSE     # Prevent double registration of event listeners
  )

  return(app_state)
}

#' Simplified State Access Helpers
#'
#' Helper functions for consistent state access patterns
#'
#' @param app_state The app state object
#' @param value The value to set
#'

# Data helper (simplified)
set_current_data <- function(app_state, value) {
  isolate({
    app_state$data$current_data <- value
    log_debug(paste("Data set with", if(is.null(value)) "NULL" else paste(nrow(value), "rows")), "STATE_MANAGEMENT")
  })
}

# Original data helper (simplified)
set_original_data <- function(app_state, value) {
  isolate({
    app_state$data$original_data <- value
    log_debug(paste("Original data set with", if(is.null(value)) "NULL" else paste(nrow(value), "rows")), "STATE_MANAGEMENT")
  })
}

# Get data helper (simplified)
get_current_data <- function(app_state) {
  isolate({
    return(app_state$data$current_data)
  })
}

#' Create Event Emit API
#'
#' Creates a clean API for emitting events to the app_state event bus.
#' This function returns a list of emit functions that can be called
#' to trigger specific events throughout the application.
#'
#' @param app_state The app state object with reactive event bus
#'
#' @return List of emit functions
#'
#' @details
#' Each emit function increments the corresponding event counter in
#' app_state$events, triggering any observeEvent() listeners.
#' Using isolate() ensures the emit functions don't create
#' unintended reactive dependencies.
#'
#' @examples
#' \dontrun{
#' emit <- create_emit_api(app_state)
#' emit$data_loaded()  # Triggers observeEvent(app_state$events$data_loaded, ...)
#' }
create_emit_api <- function(app_state) {
  list(
    # Data lifecycle events
    data_loaded = function() {
      isolate({
        app_state$events$data_loaded <- app_state$events$data_loaded + 1L
      })
    },

    data_changed = function() {
      isolate({
        app_state$events$data_changed <- app_state$events$data_changed + 1L
      })
    },

    # Column detection events
    auto_detection_started = function() {
      isolate({
        app_state$events$auto_detection_started <- app_state$events$auto_detection_started + 1L
      })
    },

    auto_detection_completed = function() {
      isolate({
        app_state$events$auto_detection_completed <- app_state$events$auto_detection_completed + 1L
      })
    },

    columns_detected = function() {
      isolate({
        app_state$events$columns_detected <- app_state$events$columns_detected + 1L
      })
    },

    # UI synchronization events
    ui_sync_needed = function() {
      isolate({
        app_state$events$ui_sync_needed <- app_state$events$ui_sync_needed + 1L
      })
    },

    ui_sync_completed = function() {
      isolate({
        app_state$events$ui_sync_completed <- app_state$events$ui_sync_completed + 1L
      })
    },

    # Navigation events
    navigation_changed = function() {
      isolate({
        app_state$events$navigation_changed <- app_state$events$navigation_changed + 1L
      })
    },

    # Session lifecycle events
    session_started = function() {
      isolate({
        app_state$events$session_started <- app_state$events$session_started + 1L
      })
    },

    session_reset = function() {
      isolate({
        app_state$events$session_reset <- app_state$events$session_reset + 1L
      })
    },

    manual_autodetect_button = function() {
      isolate({
        app_state$events$manual_autodetect_button <- app_state$events$manual_autodetect_button + 1L
      })
    },

    test_mode_ready = function() {
      isolate({
        app_state$events$test_mode_ready <- app_state$events$test_mode_ready + 1L
      })
    },

    # Error handling events
    error_occurred = function() {
      isolate({
        app_state$events$error_occurred <- app_state$events$error_occurred + 1L
      })
    },

    validation_error = function() {
      isolate({
        app_state$events$validation_error <- app_state$events$validation_error + 1L
      })
    },

    processing_error = function() {
      isolate({
        app_state$events$processing_error <- app_state$events$processing_error + 1L
      })
    },

    network_error = function() {
      isolate({
        app_state$events$network_error <- app_state$events$network_error + 1L
      })
    },

    recovery_completed = function() {
      isolate({
        app_state$events$recovery_completed <- app_state$events$recovery_completed + 1L
      })
    },

    # UI update events
    ui_update_needed = function() {
      isolate({
        app_state$events$ui_update_needed <- app_state$events$ui_update_needed + 1L
      })
    },

    column_choices_changed = function() {
      isolate({
        app_state$events$column_choices_changed <- app_state$events$column_choices_changed + 1L
      })
    },

    form_reset_needed = function() {
      isolate({
        app_state$events$form_reset_needed <- app_state$events$form_reset_needed + 1L
      })
    },

    form_restore_needed = function() {
      isolate({
        app_state$events$form_restore_needed <- app_state$events$form_restore_needed + 1L
      })
    }
  )
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
