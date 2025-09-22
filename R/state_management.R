# State Management Configuration
# Extracted from global.R for better modularity

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
#' @export
create_app_state <- function() {
  # Create environment-based state for by-reference sharing
  # CRITICAL FIX: Environment passes by reference, solving scope isolation
  app_state <- new.env(parent = emptyenv())

  # log_debug(paste("Environment created with address:", capture.output(print(app_state))), "CREATE_APP_STATE")

  # REACTIVE EVENT BUS: Central event system for all triggers
  # STREAMLINED EVENT-BUS: Reduced noise, consolidated events
  app_state$events <- shiny::reactiveValues(
    # DATA-LIVSCYKLUS ------------------------------------------------------
    data_loaded = 0L,                  # Triggeres når ny data er indlæst eller uploadet
    data_changed = 0L,                 # Triggeres ved eksplicit dataændringer

    # KOLONNEDETEKTION -----------------------------------------------------
    auto_detection_started = 0L,
    auto_detection_completed = 0L,

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
  app_state$data <- shiny::reactiveValues(
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
  app_state$columns <- shiny::reactiveValues(
    # Auto-detection sub-system
    auto_detect = shiny::reactiveValues(
      in_progress = FALSE,
      completed = FALSE,
      results = NULL,
      trigger = NULL,
      last_run = NULL,
      frozen_until_next_trigger = FALSE
    ),

    # Column mappings sub-system
    mappings = shiny::reactiveValues(
      x_column = NULL,
      y_column = NULL,
      n_column = NULL,
      cl_column = NULL,
      skift_column = NULL,
      frys_column = NULL,
      kommentar_column = NULL
    ),

    # UI synchronization sub-system
    ui_sync = shiny::reactiveValues(
      needed = FALSE,
      last_sync_time = NULL,
      pending_updates = list()
    )
  )

  # Session Management - Simplified unified structure
  app_state$session <- shiny::reactiveValues(
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
  app_state$test_mode <- shiny::reactiveValues(
    enabled = FALSE,
    auto_load = FALSE,
    file_path = NULL,
    data_loaded = FALSE
  )

  # UI State - Convert to reactiveValues for consistency
  app_state$ui <- shiny::reactiveValues(
    hide_anhoej_rules = FALSE,
    updating_programmatically = FALSE,    # LOOP PROTECTION: Flag to prevent circular events during UI updates
    last_programmatic_update = NULL,      # LOOP PROTECTION: Timestamp of last programmatic update
    flag_reset_scheduled = FALSE,         # LOOP PROTECTION: Single-reset guarantee flag

    # TOKEN-BASED TRACKING: Advanced loop protection infrastructure
    pending_programmatic_inputs = list(), # TOKEN TRACKING: Map of inputId -> {token, value, timestamp}
    programmatic_token_counter = 0L,      # TOKEN GENERATION: Counter for unique token generation
    queued_updates = list(),              # QUEUE SYSTEM: Queued UI updates for overlapping calls
    queue_processing = FALSE,             # QUEUE SYSTEM: Flag to prevent multiple processors

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

  # NOTE: Autodetect state consolidated into app_state$columns$auto_detect
  # This eliminates state duplication and maintenance burden

  # Navigation State - For eventReactive patterns
  app_state$navigation <- shiny::reactiveValues(
    trigger = 0  # Counter for triggering navigation-dependent reactives
  )

  # Visualization State - Convert to reactiveValues for consistency
  app_state$visualization <- shiny::reactiveValues(
    plot_ready = FALSE,
    plot_warnings = character(0),
    anhoej_results = NULL,
    is_computing = FALSE,
    plot_object = NULL
  )

  # Error State - Convert to reactiveValues for consistency
  app_state$errors <- shiny::reactiveValues(
    last_error = NULL,           # Last error details
    error_count = 0L,           # Total error count
    error_history = list(),     # Recent error history (max 10)
    recovery_attempts = 0L,     # Number of recovery attempts
    last_recovery_time = NULL   # Timestamp of last recovery
  )

  # Infrastructure Management - Non-reactive system state for background tasks
  # NOTE: Using regular environment instead of reactiveValues for infrastructure
  # flags that need to be accessed from later::later() callbacks without reactive context
  app_state$infrastructure <- new.env(parent = emptyenv())
  app_state$infrastructure$event_listeners_setup <- FALSE     # Prevent double registration of event listeners
  app_state$infrastructure$session_active <- TRUE            # Session lifecycle tracking
  app_state$infrastructure$background_tasks_active <- TRUE   # Background task control

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
#' @export
set_current_data <- function(app_state, value) {
  shiny::isolate({
    app_state$data$current_data <- value
    # log_debug(paste("Data set with", if(is.null(value)) "NULL" else paste(nrow(value), "rows")), "STATE_MANAGEMENT")
  })
}

# Original data helper (simplified)
set_original_data <- function(app_state, value) {
  shiny::isolate({
    app_state$data$original_data <- value
    # log_debug(paste("Original data set with", if(is.null(value)) "NULL" else paste(nrow(value), "rows")), "STATE_MANAGEMENT")
  })
}

# Get data helper (simplified)
get_current_data <- function(app_state) {
  shiny::isolate({
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
#' app_state$events, triggering any shiny::observeEvent() listeners.
#' Using shiny::isolate() ensures the emit functions don't create
#' unintended reactive dependencies.
#'
#' @examples
#' \dontrun{
#' emit <- create_emit_api(app_state)
#' emit$data_loaded()  # Triggers shiny::observeEvent(app_state$events$data_loaded, ...)
#' }
#' @export
create_emit_api <- function(app_state) {
  list(
    # Data lifecycle events
    data_loaded = function() {
      shiny::isolate({
        app_state$events$data_loaded <- app_state$events$data_loaded + 1L
      })
    },

    data_changed = function() {
      shiny::isolate({
        app_state$events$data_changed <- app_state$events$data_changed + 1L
      })
    },

    # Column detection events
    auto_detection_started = function() {
      shiny::isolate({
        app_state$events$auto_detection_started <- app_state$events$auto_detection_started + 1L
      })
    },

    auto_detection_completed = function() {
      shiny::isolate({
        app_state$events$auto_detection_completed <- app_state$events$auto_detection_completed + 1L
      })
    },


    # UI synchronization events
    ui_sync_needed = function() {
      shiny::isolate({
        app_state$events$ui_sync_needed <- app_state$events$ui_sync_needed + 1L
      })
    },

    ui_sync_completed = function() {
      shiny::isolate({
        app_state$events$ui_sync_completed <- app_state$events$ui_sync_completed + 1L
      })
    },

    # Navigation events
    navigation_changed = function() {
      shiny::isolate({
        app_state$events$navigation_changed <- app_state$events$navigation_changed + 1L
      })
    },

    # Session lifecycle events
    session_started = function() {
      shiny::isolate({
        app_state$events$session_started <- app_state$events$session_started + 1L
      })
    },

    session_reset = function() {
      shiny::isolate({
        app_state$events$session_reset <- app_state$events$session_reset + 1L
      })
    },

    manual_autodetect_button = function() {
      shiny::isolate({
        app_state$events$manual_autodetect_button <- app_state$events$manual_autodetect_button + 1L
      })
    },

    test_mode_ready = function() {
      shiny::isolate({
        app_state$events$test_mode_ready <- app_state$events$test_mode_ready + 1L
      })
    },

    # Error handling events
    error_occurred = function() {
      shiny::isolate({
        app_state$events$error_occurred <- app_state$events$error_occurred + 1L
      })
    },

    validation_error = function() {
      shiny::isolate({
        app_state$events$validation_error <- app_state$events$validation_error + 1L
      })
    },

    processing_error = function() {
      shiny::isolate({
        app_state$events$processing_error <- app_state$events$processing_error + 1L
      })
    },

    network_error = function() {
      shiny::isolate({
        app_state$events$network_error <- app_state$events$network_error + 1L
      })
    },

    recovery_completed = function() {
      shiny::isolate({
        app_state$events$recovery_completed <- app_state$events$recovery_completed + 1L
      })
    },

    # UI update events
    ui_update_needed = function() {
      shiny::isolate({
        app_state$events$ui_update_needed <- app_state$events$ui_update_needed + 1L
      })
    },

    column_choices_changed = function() {
      shiny::isolate({
        app_state$events$column_choices_changed <- app_state$events$column_choices_changed + 1L
      })
    },

    form_reset_needed = function() {
      shiny::isolate({
        app_state$events$form_reset_needed <- app_state$events$form_reset_needed + 1L
      })
    },

    form_restore_needed = function() {
      shiny::isolate({
        app_state$events$form_restore_needed <- app_state$events$form_restore_needed + 1L
      })
    }
  )
}

## Dato kolonnevalidering -----
#' @export
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
