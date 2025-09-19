# helper.R
# Test setup og fælles funktioner

library(shiny)
library(shinytest2)
library(testthat)

# Source app komponenter for test
# Naviger til projekt rod og load global.R
project_root <- here::here()

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
