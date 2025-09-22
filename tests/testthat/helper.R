# helper.R
# Test setup og fælles funktioner

# Only load shinytest2 if available to avoid breaking tests
if (requireNamespace("shinytest2", quietly = TRUE)) {
  library(shinytest2)
} else {
  # Graceful fallback - define dummy functions for shinytest2 dependencies
  AppDriver <- function(...) {
    skip("shinytest2 not available")
  }
}

library(testthat)

# Alias Shiny core functions for tests uden global library()
isolate <- shiny::isolate
reactive <- shiny::reactive
reactiveValues <- shiny::reactiveValues
debounce <- shiny::debounce
updateSelectizeInput <- shiny::updateSelectizeInput
req <- shiny::req

# Source app komponenter for test
# Naviger til projekt rod og load global.R
project_root <- here::here()

# Sørg for at lokale pakkeinstallationer i .Rlibs opdages først
local_lib <- file.path(project_root, ".Rlibs")
if (dir.exists(local_lib)) {
  .libPaths(c(local_lib, .libPaths()))
}

# Skift working directory midlertidigt til project root for at loade global.R korrekt
old_wd <- getwd()
on.exit(setwd(old_wd))
setwd(project_root)

if (file.exists("global.R")) {
  source("global.R", local = FALSE)
}

# Load server helper functions specifically for tests
if (file.exists("R/utils_session_helpers.R")) {
  source("R/utils_session_helpers.R", local = FALSE)
}

# Test data setup
create_test_data <- function() {
  data.frame(
    Skift = rep(FALSE, 10),
    Frys = rep(FALSE, 10), 
    Dato = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 10),
    Tæller = c(90, 85, 92, 88, 94, 91, 87, 93, 89, 95),
    Nævner = c(100, 95, 100, 98, 102, 99, 96, 101, 97, 103),
    stringsAsFactors = FALSE
  )
}

# Helper funktion til at vente på app ready state
wait_for_app_ready <- function(app, timeout = 10) {
  Sys.sleep(2) # Basic wait for app initialization
  TRUE
}

# Test helper: Minimal app_state uden Shiny reaktivitet
create_test_app_state <- function() {
  state <- new.env(parent = emptyenv())

  # Minimal event struktur til unit tests uden fuld Shiny state
  state$events <- new.env(parent = emptyenv())

  state$autodetect <- new.env(parent = emptyenv())
  state$autodetect$frozen_until_next_trigger <- FALSE
  state$autodetect$last_run <- NULL

  state$columns <- new.env(parent = emptyenv())
  state$columns$mappings <- new.env(parent = emptyenv())
  state$columns$mappings$x_column <- NULL
  state$columns$mappings$y_column <- NULL
  state$columns$mappings$n_column <- NULL
  state$columns$mappings$skift_column <- NULL
  state$columns$mappings$frys_column <- NULL
  state$columns$mappings$kommentar_column <- NULL

  state$columns$auto_detect <- new.env(parent = emptyenv())
  state$columns$auto_detect$in_progress <- FALSE
  state$columns$auto_detect$completed <- FALSE
  state$columns$auto_detect$results <- NULL

  state$data <- new.env(parent = emptyenv())
  state$data$current_data <- NULL
  state$data$original_data <- NULL

  state$ui <- new.env(parent = emptyenv())
  state$ui$updating_programmatically <- FALSE
  state$ui$flag_reset_scheduled <- FALSE
  state$ui$queued_updates <- list()
  state$ui$queue_processing <- FALSE
  state$ui$pending_programmatic_inputs <- list()
  state$ui$memory_limits <- list(
    max_queue_size = 5L,
    max_pending_tokens = 100L
  )
  state$ui$programmatic_token_counter <- 0L
  state$ui$performance_metrics <- list(
    queued_updates = 0L,
    queue_max_size = 0L,
    tokens_consumed = 0L,
    total_updates = 0L,
    avg_update_duration_ms = 0
  )

  state
}

# Hjælpefunktion til at sikre at alle nødvendige event-tællere findes
ensure_event_counters <- function(app_state, required_events = NULL) {
  if (is.null(required_events)) {
    required_events <- c(
      "data_loaded", "data_changed",
      "auto_detection_started", "auto_detection_completed",
      "ui_sync_needed", "ui_sync_completed", "ui_update_needed",
      "navigation_changed",
      "session_started", "session_reset", "manual_autodetect_button",
      "test_mode_ready",
      "error_occurred", "validation_error", "processing_error",
      "network_error", "recovery_completed",
      "column_choices_changed",
      "form_reset_needed", "form_restore_needed", "form_update_needed"
    )
  }

  for (event_name in required_events) {
    current_value <- tryCatch(
      isolate(app_state$events[[event_name]]),
      error = function(...) NULL
    )

    if (is.null(current_value)) {
      app_state$events[[event_name]] <- 0L
    }
  }

  app_state
}

# Fuldt initialiseret app_state til integrationstests
create_test_ready_app_state <- function() {
  app_state <- create_app_state()
  ensure_event_counters(app_state)
  app_state
}
