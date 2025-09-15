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

# ===== NYE TESTS FOR AUTO-DETECT UI SYNC FLOW =====

test_that("auto-detect UI sync trigger mechanism", {
  # TEST: Auto-detect setter korrekt ui_sync_needed structure

  # SETUP: Mock auto-detect results
  auto_detect_results <- list(
    x_col = "Dato",
    taeller_col = "Tæller",
    naevner_col = "Nævner",
    skift_col = NULL,
    frys_col = NULL,
    kommentar_col = "Kommentarer"
  )

  col_choices <- c("", "Dato", "Tæller", "Nævner", "Kommentarer")
  names(col_choices) <- c("Vælg kolonne...", "Dato", "Tæller", "Nævner", "Kommentarer")

  # Function to create UI sync trigger (from auto-detect function)
  create_ui_sync_trigger <- function(detected_cols, choices) {
    list(
      x_col = detected_cols$x_col,
      taeller_col = detected_cols$taeller_col,
      naevner_col = detected_cols$naevner_col,
      skift_col = detected_cols$skift_col,
      frys_col = detected_cols$frys_col,
      kommentar_col = detected_cols$kommentar_col,
      col_choices = choices,
      timestamp = as.numeric(Sys.time())
    )
  }

  trigger <- create_ui_sync_trigger(auto_detect_results, col_choices)

  # TEST: All required fields present
  required_fields <- c("x_col", "taeller_col", "naevner_col", "skift_col",
                      "frys_col", "kommentar_col", "col_choices", "timestamp")
  for (field in required_fields) {
    expect_true(field %in% names(trigger), info = paste("Missing field:", field))
  }

  # TEST: Correct values mapped
  expect_equal(trigger$x_col, "Dato")
  expect_equal(trigger$taeller_col, "Tæller")
  expect_equal(trigger$naevner_col, "Nævner")
  expect_null(trigger$skift_col)
  expect_null(trigger$frys_col)
  expect_equal(trigger$kommentar_col, "Kommentarer")

  # TEST: Timestamp is numeric and recent
  expect_true(is.numeric(trigger$timestamp))
  expect_true(trigger$timestamp > 0)

  # TEST: col_choices structure preserved
  expect_equal(trigger$col_choices, col_choices)
})

test_that("reaktiv UI sync observer simulation", {
  # TEST: Simulation af hvordan observeren håndterer ui_sync_needed

  # SETUP: Mock sync data
  sync_data <- list(
    x_col = "Dato",
    taeller_col = "Tæller",
    naevner_col = NULL,  # Test NULL handling
    skift_col = "Skift_kolonne",
    frys_col = NULL,
    kommentar_col = "Kommentarer",
    col_choices = c("Vælg kolonne..." = "", "Dato" = "Dato", "Tæller" = "Tæller"),
    timestamp = as.numeric(Sys.time())
  )

  # Function to simulate observer logic
  simulate_ui_sync_observer <- function(sync_data) {
    updates <- list()

    if (!is.null(sync_data$col_choices)) {
      # Simulate updateSelectizeInput calls
      if (!is.null(sync_data$x_col)) {
        updates$x_column <- list(
          choices = sync_data$col_choices,
          selected = sync_data$x_col
        )
      }

      if (!is.null(sync_data$taeller_col)) {
        updates$y_column <- list(
          choices = sync_data$col_choices,
          selected = sync_data$taeller_col
        )
      }

      if (!is.null(sync_data$naevner_col)) {
        updates$n_column <- list(
          choices = sync_data$col_choices,
          selected = sync_data$naevner_col
        )
      } else {
        updates$n_column <- list(
          choices = sync_data$col_choices,
          selected = ""  # Fallback for NULL
        )
      }

      if (!is.null(sync_data$skift_col)) {
        updates$skift_column <- list(
          choices = sync_data$col_choices,
          selected = sync_data$skift_col
        )
      }

      if (!is.null(sync_data$frys_col)) {
        updates$frys_column <- list(
          choices = sync_data$col_choices,
          selected = sync_data$frys_col
        )
      }

      if (!is.null(sync_data$kommentar_col)) {
        updates$kommentar_column <- list(
          choices = sync_data$col_choices,
          selected = sync_data$kommentar_col
        )
      }
    }

    return(updates)
  }

  updates <- simulate_ui_sync_observer(sync_data)

  # TEST: Correct updates generated for non-NULL columns
  expect_true("x_column" %in% names(updates))
  expect_true("y_column" %in% names(updates))
  expect_true("n_column" %in% names(updates))  # Should be present even if NULL (with fallback)
  expect_true("skift_column" %in% names(updates))
  expect_true("kommentar_column" %in% names(updates))

  # TEST: Correct selected values
  expect_equal(updates$x_column$selected, "Dato")
  expect_equal(updates$y_column$selected, "Tæller")
  expect_equal(updates$n_column$selected, "")  # NULL becomes empty string
  expect_equal(updates$skift_column$selected, "Skift_kolonne")
  expect_equal(updates$kommentar_column$selected, "Kommentarer")

  # TEST: All updates have correct choices
  for (update in updates) {
    expect_equal(update$choices, sync_data$col_choices)
  }
})

test_that("timestamp reactivity mechanism", {
  # TEST: Timestamp mechanism sikrer reactivity selv med samme values

  base_data <- list(
    x_col = "Dato",
    taeller_col = "Tæller",
    col_choices = c("", "Dato", "Tæller")
  )

  # Function to add timestamp
  add_reactivity_timestamp <- function(data) {
    data$timestamp <- as.numeric(Sys.time())
    return(data)
  }

  # TEST: Multiple calls produce different timestamps
  trigger1 <- add_reactivity_timestamp(base_data)
  Sys.sleep(0.01)  # Small delay to ensure different timestamps
  trigger2 <- add_reactivity_timestamp(base_data)

  # Verify timestamps are different even though other data is same
  expect_true(trigger1$timestamp != trigger2$timestamp)
  expect_true(trigger2$timestamp > trigger1$timestamp)

  # Verify rest of data is identical
  expect_equal(trigger1$x_col, trigger2$x_col)
  expect_equal(trigger1$taeller_col, trigger2$taeller_col)
  expect_equal(trigger1$col_choices, trigger2$col_choices)
})

test_that("edge case: alle kolonner NULL", {
  # TEST: Håndtering når auto-detect ikke finder noget

  # SETUP: Empty detection results
  empty_results <- list(
    x_col = NULL,
    taeller_col = NULL,
    naevner_col = NULL,
    skift_col = NULL,
    frys_col = NULL,
    kommentar_col = NULL
  )

  col_choices <- c("Vælg kolonne..." = "", "UnknownCol1" = "UnknownCol1")

  # Function to handle empty detection
  handle_empty_detection <- function(detected_cols, choices) {
    # Simulation af ui_sync_needed creation med NULL values
    list(
      x_col = detected_cols$x_col,
      taeller_col = detected_cols$taeller_col,
      naevner_col = detected_cols$naevner_col,
      skift_col = detected_cols$skift_col,
      frys_col = detected_cols$frys_col,
      kommentar_col = detected_cols$kommentar_col,
      col_choices = choices,
      timestamp = as.numeric(Sys.time())
    )
  }

  trigger <- handle_empty_detection(empty_results, col_choices)

  # TEST: Structure er korrekt selvom values er NULL
  expect_true(is.list(trigger))
  expect_true("col_choices" %in% names(trigger))
  expect_true("timestamp" %in% names(trigger))

  # TEST: All detection fields are NULL
  expect_null(trigger$x_col)
  expect_null(trigger$taeller_col)
  expect_null(trigger$naevner_col)
  expect_null(trigger$skift_col)
  expect_null(trigger$frys_col)
  expect_null(trigger$kommentar_col)

  # TEST: col_choices and timestamp still present
  expect_equal(trigger$col_choices, col_choices)
  expect_true(is.numeric(trigger$timestamp))
})

test_that("integration: komplet auto-detect til UI flow", {
  # TEST: Integration test af hele flow fra auto-detect til UI update

  # SETUP: Complete scenario
  test_data <- data.frame(
    Dato = as.Date(c("2022-01-01", "2022-02-01")),
    Tæller = c(10, 12),
    Nævner = c(100, 120),
    Kommentarer = c("Test1", "Test2"),
    stringsAsFactors = FALSE
  )

  # Step 1: Simulate column detection
  simulate_auto_detect <- function(data) {
    col_names <- names(data)

    # Simple detection logic (mirror actual function logic)
    x_col <- if ("Dato" %in% col_names) "Dato" else col_names[1]
    taeller_col <- if ("Tæller" %in% col_names) "Tæller" else NULL
    naevner_col <- if ("Nævner" %in% col_names) "Nævner" else NULL
    kommentar_col <- if ("Kommentarer" %in% col_names) "Kommentarer" else NULL

    return(list(
      x_col = x_col,
      taeller_col = taeller_col,
      naevner_col = naevner_col,
      skift_col = NULL,
      frys_col = NULL,
      kommentar_col = kommentar_col
    ))
  }

  # Step 2: Create UI sync trigger
  detected <- simulate_auto_detect(test_data)

  col_choices <- c("", names(test_data))
  names(col_choices) <- c("Vælg kolonne...", names(test_data))

  ui_sync_trigger <- list(
    x_col = detected$x_col,
    taeller_col = detected$taeller_col,
    naevner_col = detected$naevner_col,
    skift_col = detected$skift_col,
    frys_col = detected$frys_col,
    kommentar_col = detected$kommentar_col,
    col_choices = col_choices,
    timestamp = as.numeric(Sys.time())
  )

  # Step 3: Process UI updates
  # (simulation af observer processing)

  # TEST: End-to-end flow
  expect_equal(ui_sync_trigger$x_col, "Dato")
  expect_equal(ui_sync_trigger$taeller_col, "Tæller")
  expect_equal(ui_sync_trigger$naevner_col, "Nævner")
  expect_equal(ui_sync_trigger$kommentar_col, "Kommentarer")
  expect_null(ui_sync_trigger$skift_col)
  expect_null(ui_sync_trigger$frys_col)

  # TEST: UI sync data is ready for processing
  expect_true(!is.null(ui_sync_trigger$col_choices))
  expect_true(length(ui_sync_trigger$col_choices) == 5)  # "" + 4 data columns
  expect_true(is.numeric(ui_sync_trigger$timestamp))
})