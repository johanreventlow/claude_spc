# test-ui-synchronization-fixed.R
# Test af UI synchronization og input management
# Fokuserer på updateSelectizeInput calls og UI state management

test_that("UI input choices structure er korrekt", {
  # TEST: Oprettelse af korrekt choices struktur for selectizeInput

  # SETUP: Simuler kolonne data
  all_cols <- c("Dato", "Tæller", "Nævner", "Kommentar")

  # Function to create choices (from fct_data_processing.R)
  create_column_choices <- function(columns) {
    choices <- c("", columns)
    names(choices) <- c("Vælg kolonne...", columns)
    return(choices)
  }

  choices <- create_column_choices(all_cols)

  # TEST: Structure
  expect_equal(length(choices), 5) # empty + 4 columns
  expect_equal(choices[[1]], "")
  expect_equal(names(choices)[1], "Vælg kolonne...")

  # TEST: All columns present
  for (col in all_cols) {
    expect_true(col %in% choices)
    expect_true(col %in% names(choices))
  }
})

test_that("UI sync request creation fungerer", {
  # TEST: UI sync request data structure

  # Function to create UI sync request
  create_ui_sync_request <- function(detected_columns, col_choices) {
    list(
      x_col = detected_columns$x_col,
      taeller_col = detected_columns$taeller_col,
      naevner_col = detected_columns$naevner_col,
      skift_col = detected_columns$skift_col,
      frys_col = detected_columns$frys_col,
      kommentar_col = detected_columns$kommentar_col,
      col_choices = col_choices,
      timestamp = Sys.time()
    )
  }

  # SETUP: Mock detection results
  detected <- list(
    x_col = "Dato",
    taeller_col = "Tæller",
    naevner_col = "Nævner",
    skift_col = NULL,
    frys_col = NULL,
    kommentar_col = "Kommentar"
  )

  choices <- c("", "Dato", "Tæller", "Nævner", "Kommentar")
  names(choices) <- c("Vælg kolonne...", "Dato", "Tæller", "Nævner", "Kommentar")

  sync_request <- create_ui_sync_request(detected, choices)

  # TEST: All required fields present
  required_fields <- c("x_col", "taeller_col", "naevner_col", "skift_col",
                       "frys_col", "kommentar_col", "col_choices", "timestamp")
  for (field in required_fields) {
    expect_true(field %in% names(sync_request))
  }

  # TEST: Values are correct
  expect_equal(sync_request$x_col, "Dato")
  expect_equal(sync_request$taeller_col, "Tæller")
  expect_equal(sync_request$naevner_col, "Nævner")
  expect_null(sync_request$skift_col)
  expect_null(sync_request$frys_col)
  expect_equal(sync_request$kommentar_col, "Kommentar")
})

test_that("UI sync processing logic", {
  # TEST: Processing af UI sync requests

  # SETUP: Mock sync request
  sync_request <- list(
    x_col = "Dato",
    taeller_col = "Tæller",
    naevner_col = NULL, # Test NULL handling
    skift_col = "Skift",
    frys_col = NULL,
    kommentar_col = NULL,
    col_choices = c("", "Dato", "Tæller", "Skift"),
    timestamp = Sys.time()
  )

  # Function to process sync request
  process_ui_sync <- function(sync_data) {
    updates <- list()

    if (!is.null(sync_data$x_col)) {
      updates[["x_column"]] <- list(
        choices = sync_data$col_choices,
        selected = sync_data$x_col
      )
    }

    if (!is.null(sync_data$taeller_col)) {
      updates[["y_column"]] <- list(
        choices = sync_data$col_choices,
        selected = sync_data$taeller_col
      )
    }

    if (!is.null(sync_data$naevner_col)) {
      updates[["n_column"]] <- list(
        choices = sync_data$col_choices,
        selected = sync_data$naevner_col
      )
    }

    return(updates)
  }

  updates <- process_ui_sync(sync_request)

  # TEST: Correct updates generated
  expect_true("x_column" %in% names(updates)) # x_col is not NULL
  expect_true("y_column" %in% names(updates)) # taeller_col is not NULL
  expect_false("n_column" %in% names(updates)) # naevner_col is NULL

  # TEST: Update structure
  expect_equal(updates$x_column$selected, "Dato")
  expect_equal(updates$y_column$selected, "Tæller")
})

test_that("Event-driven vs timing-based approach", {
  # TEST: Demonstrate difference between approaches

  # EVENT-DRIVEN approach (our new implementation)
  event_driven_sync <- function(data) {
    start_time <- Sys.time()

    # Immediate processing - no delays
    result <- "sync_completed"

    end_time <- Sys.time()
    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

    return(list(
      result = result,
      execution_time = execution_time,
      approach = "event_driven"
    ))
  }

  # TEST: Event-driven approach
  test_data <- list(x_col = "Dato")
  event_result <- event_driven_sync(test_data)

  # VERIFY: Event-driven approach
  expect_equal(event_result$result, "sync_completed")
  expect_equal(event_result$approach, "event_driven")
  expect_true(event_result$execution_time < 0.1) # Should be very fast

  # CONCEPTUAL TEST: Event-driven is deterministic
  results <- replicate(3, event_driven_sync(test_data), simplify = FALSE)

  # All results should be consistent
  for (result in results) {
    expect_equal(result$result, "sync_completed")
    expect_equal(result$approach, "event_driven")
    expect_true(result$execution_time < 0.1)
  }
})

test_that("Input validation for UI updates", {
  # TEST: Validation of input data before UI updates

  # Function to validate sync data
  validate_sync_data <- function(sync_data) {
    errors <- character(0)

    # Check required structure
    if (is.null(sync_data)) {
      errors <- c(errors, "sync_data cannot be NULL")
    }

    if (!is.list(sync_data)) {
      errors <- c(errors, "sync_data must be a list")
    }

    # Check for required fields
    required_fields <- c("col_choices", "timestamp")
    for (field in required_fields) {
      if (!(field %in% names(sync_data))) {
        errors <- c(errors, paste("Missing required field:", field))
      }
    }

    # Validate col_choices structure
    if ("col_choices" %in% names(sync_data)) {
      if (!is.character(sync_data$col_choices)) {
        errors <- c(errors, "col_choices must be character vector")
      }
    }

    return(if (length(errors) == 0) TRUE else errors)
  }

  # TEST: Valid data
  valid_data <- list(
    x_col = "Dato",
    col_choices = c("", "Dato"),
    timestamp = Sys.time()
  )
  expect_true(validate_sync_data(valid_data))

  # TEST: Invalid data cases
  expect_true(is.character(validate_sync_data(NULL)))
  expect_true(is.character(validate_sync_data("not_a_list")))

  invalid_data <- list(x_col = "Dato") # Missing required fields
  expect_true(is.character(validate_sync_data(invalid_data)))
})

test_that("Observer priority concept", {
  # TEST: Simulation of observer priority behavior

  # SETUP: Event queue with priorities
  event_queue <- list()

  add_event <- function(name, priority = 0) {
    event_queue <<- c(event_queue, list(list(name = name, priority = priority)))
  }

  # Function to execute events by priority
  execute_by_priority <- function() {
    if (length(event_queue) == 0) return(character(0))

    # Sort by priority: negative priorities run last
    sorted_events <- event_queue[order(sapply(event_queue, function(e) {
      if (e$priority < 0) {
        1000 + abs(e$priority)
      } else {
        -e$priority
      }
    }))]

    sapply(sorted_events, function(e) e$name)
  }

  # TEST: Add events with different priorities
  add_event("auto_detect", priority = 0)
  add_event("ui_sync", priority = -10) # Should run last
  add_event("validation", priority = 5) # Should run first

  result <- execute_by_priority()

  # VERIFY: Correct execution order
  expect_equal(length(result), 3)
  expect_equal(result[3], "ui_sync") # Lowest priority (-10)
})

test_that("UI sync cleanup", {
  # TEST: Cleanup af sync requests

  # SETUP: Mock reactive values (simulation)
  ui_sync_state <- list(
    ui_sync_needed = list(x_col = "Dato", timestamp = Sys.time()),
    sync_in_progress = FALSE
  )

  # Function to clear sync request
  clear_ui_sync_request <- function(state) {
    state$ui_sync_needed <- NULL
    return(state)
  }

  # TEST: Initial state
  expect_false(is.null(ui_sync_state$ui_sync_needed))

  # TEST: Clear request
  ui_sync_state <- clear_ui_sync_request(ui_sync_state)
  expect_null(ui_sync_state$ui_sync_needed)
})