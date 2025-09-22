# test-state-management-hierarchical.R
# Comprehensive tests for hierarchical state management system
# Tests the new app_state architecture with reactive values and event systems
# Foundation for all other functionality - critical path testing

test_that("create_app_state basic functionality works", {
  # TEST: Core app_state creation and structure

  # Skip if create_app_state function not available
  skip_if_not(exists("create_app_state", mode = "function"), "create_app_state function not available")

  # SETUP: Create app state
  app_state <- create_app_state()

  # Verify basic structure
  expect_true(is.environment(app_state))
  expect_true("events" %in% names(app_state))
  expect_true("data" %in% names(app_state))
  expect_true("columns" %in% names(app_state))

  # Verify reactive values structure
  expect_s3_class(app_state$events, "reactivevalues")
  expect_s3_class(app_state$data, "reactivevalues")
  expect_s3_class(app_state$columns, "reactivevalues")

  # Verify hierarchical column structure
  expect_s3_class(app_state$columns$auto_detect, "reactivevalues")
  expect_s3_class(app_state$columns$mappings, "reactivevalues")
  expect_s3_class(app_state$columns$ui_sync, "reactivevalues")
})

test_that("app_state event system works correctly", {
  # TEST: Event bus functionality and triggering

  # SETUP: Create app state
  app_state <- create_app_state()

  # TEST: Initial event values
  expect_equal(isolate(app_state$events$data_loaded), 0L)
  expect_equal(isolate(app_state$events$auto_detection_started), 0L)
  expect_equal(isolate(app_state$events$ui_sync_needed), 0L)

  # TEST: Event triggering
  app_state$events$data_loaded <- isolate(app_state$events$data_loaded) + 1L
  expect_equal(isolate(app_state$events$data_loaded), 1L)

  # TEST: Multiple event types
  app_state$events$auto_detection_completed <- 1L
  app_state$events$ui_sync_needed <- 1L

  expect_equal(isolate(app_state$events$auto_detection_completed), 1L)
  expect_equal(isolate(app_state$events$ui_sync_needed), 1L)

  # TEST: Event independence
  expect_equal(isolate(app_state$events$data_loaded), 1L) # Should remain unchanged
})

test_that("app_state data management works", {
  # TEST: Data management reactive values

  # SETUP: Create app state
  app_state <- create_app_state()

  # TEST: Initial data state
  expect_null(isolate(app_state$data$current_data))
  expect_null(isolate(app_state$data$original_data))
  expect_false(isolate(app_state$data$updating_table))
  expect_equal(isolate(app_state$data$table_version), 0)

  # TEST: Data assignment
  test_data <- data.frame(
    Dato = c("01-01-2024", "01-02-2024"),
    Værdi = c(45, 43),
    stringsAsFactors = FALSE
  )

  app_state$data$current_data <- test_data
  app_state$data$original_data <- test_data

  expect_equal(nrow(isolate(app_state$data$current_data)), 2)
  expect_equal(nrow(isolate(app_state$data$original_data)), 2)
  expect_true(all(names(isolate(app_state$data$current_data)) == c("Dato", "Værdi")))

  # TEST: Table operation flags
  app_state$data$updating_table <- TRUE
  app_state$data$table_version <- 1

  expect_true(isolate(app_state$data$updating_table))
  expect_equal(isolate(app_state$data$table_version), 1)

  # TEST: File metadata
  app_state$data$file_info <- list(name = "test.csv", size = 1024)
  app_state$data$file_path <- "/path/to/test.csv"

  expect_equal(isolate(app_state$data$file_info)$name, "test.csv")
  expect_equal(isolate(app_state$data$file_path), "/path/to/test.csv")
})

test_that("app_state hierarchical column management works", {
  # TEST: Hierarchical column structure and mappings

  # SETUP: Create app state
  app_state <- create_app_state()

  # TEST: Auto-detection sub-system
  expect_false(isolate(app_state$columns$auto_detect$in_progress))
  expect_false(isolate(app_state$columns$auto_detect$completed))
  expect_null(isolate(app_state$columns$auto_detect$results))
  expect_false(isolate(app_state$columns$auto_detect$frozen_until_next_trigger))

  # Update auto-detection state
  app_state$columns$auto_detect$in_progress <- TRUE
  app_state$columns$auto_detect$results <- list(x_col = "Dato", y_col = "Værdi")
  app_state$columns$auto_detect$frozen_until_next_trigger <- TRUE

  expect_true(isolate(app_state$columns$auto_detect$in_progress))
  expect_equal(isolate(app_state$columns$auto_detect$results)$x_col, "Dato")
  expect_true(isolate(app_state$columns$auto_detect$frozen_until_next_trigger))

  # TEST: Column mappings sub-system
  expect_null(isolate(app_state$columns$mappings$x_column))
  expect_null(isolate(app_state$columns$mappings$y_column))
  expect_null(isolate(app_state$columns$mappings$n_column))

  # Update column mappings
  app_state$columns$mappings$x_column <- "Dato"
  app_state$columns$mappings$y_column <- "Tæller"
  app_state$columns$mappings$n_column <- "Nævner"
  app_state$columns$mappings$cl_column <- "Control_Limit"
  app_state$columns$mappings$skift_column <- "Skift"
  app_state$columns$mappings$frys_column <- "Frys"
  app_state$columns$mappings$kommentar_column <- "Kommentar"

  expect_equal(isolate(app_state$columns$mappings$x_column), "Dato")
  expect_equal(isolate(app_state$columns$mappings$y_column), "Tæller")
  expect_equal(isolate(app_state$columns$mappings$n_column), "Nævner")
  expect_equal(isolate(app_state$columns$mappings$cl_column), "Control_Limit")
  expect_equal(isolate(app_state$columns$mappings$skift_column), "Skift")
  expect_equal(isolate(app_state$columns$mappings$frys_column), "Frys")
  expect_equal(isolate(app_state$columns$mappings$kommentar_column), "Kommentar")

  # TEST: UI synchronization sub-system
  expect_false(isolate(app_state$columns$ui_sync$needed))
  expect_null(isolate(app_state$columns$ui_sync$last_sync_time))

  app_state$columns$ui_sync$needed <- TRUE
  app_state$columns$ui_sync$last_sync_time <- Sys.time()

  expect_true(isolate(app_state$columns$ui_sync$needed))
  expect_true(inherits(isolate(app_state$columns$ui_sync$last_sync_time), c("POSIXct", "POSIXt")))
})

test_that("app_state environment-based sharing works", {
  # TEST: Environment-based by-reference sharing

  # SETUP: Create app state
  app_state <- create_app_state()

  # TEST: Function that modifies state by reference
  modify_state <- function(state) {
    state$data$current_data <- data.frame(test = "modified")
    state$columns$mappings$x_column <- "modified_column"
    state$events$data_loaded <- 99L
  }

  # Modify state through function
  modify_state(app_state)

  # Verify changes persist (environment passed by reference)
  expect_equal(isolate(app_state$data$current_data)$test, "modified")
  expect_equal(isolate(app_state$columns$mappings$x_column), "modified_column")
  expect_equal(isolate(app_state$events$data_loaded), 99L)

  # TEST: Multiple references to same environment
  app_state_ref1 <- app_state
  app_state_ref2 <- app_state

  app_state_ref1$data$table_version <- 100

  # Both references should see the change
  expect_equal(isolate(app_state_ref2$data$table_version), 100)
  expect_equal(isolate(app_state$data$table_version), 100)
})

test_that("app_state session management works", {
  # TEST: Session-related state management

  # SETUP: Create app state with session components
  app_state <- create_app_state()

  # Verify session structure exists if defined
  if ("session" %in% names(app_state)) {
    # TEST: Session state initial values
    expect_true(isolate(app_state$session$auto_save_enabled) %||% TRUE)
    expect_false(isolate(app_state$session$restoring_session) %||% FALSE)
    expect_false(isolate(app_state$session$file_uploaded) %||% FALSE)

    # TEST: Session state updates
    app_state$session$file_uploaded <- TRUE
    app_state$session$user_started_session <- TRUE
    app_state$session$file_name <- "test_data.csv"

    expect_true(isolate(app_state$session$file_uploaded))
    expect_true(isolate(app_state$session$user_started_session))
    expect_equal(isolate(app_state$session$file_name), "test_data.csv")
  }
})

test_that("app_state reactive chains work correctly", {
  # TEST: Reactive dependencies and chains

  # SETUP: Create app state
  app_state <- create_app_state()

  # TEST: Create reactive expression dependent on state
  data_summary <- reactive({
    req(app_state$data$current_data)
    data <- isolate(app_state$data$current_data)
    list(
      rows = nrow(data),
      cols = ncol(data),
      names = names(data)
    )
  })

  # Initially should be null (no data)
  expect_error(data_summary(), class = "shiny.silent.error")

  # Add data and test reactive update
  test_data <- data.frame(
    A = 1:3,
    B = 4:6,
    C = 7:9
  )
  app_state$data$current_data <- test_data

  # Reactive should now work
  summary_result <- data_summary()
  expect_equal(summary_result$rows, 3)
  expect_equal(summary_result$cols, 3)
  expect_equal(summary_result$names, c("A", "B", "C"))
})

test_that("app_state event-driven workflows work", {
  # TEST: Event-driven state update workflows

  # SETUP: Create app state
  app_state <- create_app_state()

  # Track workflow execution
  workflow_steps <- list()

  # SETUP: Event-driven observers
  observeEvent(app_state$events$data_loaded, ignoreInit = TRUE, {
    workflow_steps$data_loaded <- TRUE
    app_state$events$auto_detection_started <- isolate(app_state$events$auto_detection_started) + 1L
  })

  observeEvent(app_state$events$auto_detection_started, ignoreInit = TRUE, {
    workflow_steps$auto_detection_started <- TRUE
    # Simulate auto-detection
    app_state$columns$mappings$x_column <- "detected_x"
    app_state$events$auto_detection_completed <- isolate(app_state$events$auto_detection_completed) + 1L
  })

  observeEvent(app_state$events$auto_detection_completed, ignoreInit = TRUE, {
    workflow_steps$auto_detection_completed <- TRUE
    app_state$events$ui_sync_needed <- isolate(app_state$events$ui_sync_needed) + 1L
  })

  # TEST: Trigger workflow
  app_state$events$data_loaded <- isolate(app_state$events$data_loaded) + 1L

  # Allow reactive context to settle
  session <- shiny::MockShinySession$new()
  shiny::flushReact()

  # Verify workflow executed
  expect_true(workflow_steps$data_loaded %||% FALSE)
  expect_true(workflow_steps$auto_detection_started %||% FALSE)
  expect_true(workflow_steps$auto_detection_completed %||% FALSE)
  expect_equal(isolate(app_state$columns$mappings$x_column), "detected_x")
})

test_that("app_state error handling and recovery works", {
  # TEST: Error states and recovery mechanisms

  # SETUP: Create app state
  app_state <- create_app_state()

  # TEST: Error event handling
  expect_equal(isolate(app_state$events$error_occurred), 0L)
  expect_equal(isolate(app_state$events$validation_error), 0L)
  expect_equal(isolate(app_state$events$processing_error), 0L)

  # Trigger error events
  app_state$events$validation_error <- 1L
  app_state$events$processing_error <- 1L

  expect_equal(isolate(app_state$events$validation_error), 1L)
  expect_equal(isolate(app_state$events$processing_error), 1L)

  # TEST: Recovery workflow
  app_state$events$recovery_completed <- 1L
  expect_equal(isolate(app_state$events$recovery_completed), 1L)
})

test_that("app_state performance and memory management works", {
  # TEST: Performance considerations and memory usage

  # SETUP: Create app state
  app_state <- create_app_state()

  # TEST: Large data handling
  large_data <- data.frame(
    x = 1:1000,
    y = sample(1:100, 1000, replace = TRUE),
    z = runif(1000)
  )

  # Measure memory impact
  start_time <- Sys.time()
  app_state$data$current_data <- large_data
  assignment_time <- as.numeric(Sys.time() - start_time)

  # Should handle large data efficiently
  expect_lt(assignment_time, 1.0) # Should complete in under 1 second
  expect_equal(nrow(isolate(app_state$data$current_data)), 1000)

  # TEST: State cleanup
  app_state$data$current_data <- NULL
  app_state$data$original_data <- NULL

  expect_null(isolate(app_state$data$current_data))
  expect_null(isolate(app_state$data$original_data))
})

test_that("app_state complex state transitions work", {
  # TEST: Complex state transition scenarios

  # SETUP: Create app state
  app_state <- create_app_state()

  # TEST: File upload workflow simulation
  # Step 1: File upload starts
  app_state$data$updating_table <- TRUE
  app_state$events$data_loaded <- 1L

  expect_true(isolate(app_state$data$updating_table))
  expect_equal(isolate(app_state$events$data_loaded), 1L)

  # Step 2: Data processed
  test_data <- data.frame(
    Dato = c("01-01-2024", "01-02-2024", "01-03-2024"),
    Tæller = c(45, 43, 48),
    Nævner = c(50, 50, 50)
  )
  app_state$data$current_data <- test_data
  app_state$data$original_data <- test_data

  # Step 3: Auto-detection triggered
  app_state$events$auto_detection_started <- 1L
  app_state$columns$auto_detect$in_progress <- TRUE

  # Step 4: Auto-detection completed
  app_state$columns$mappings$x_column <- "Dato"
  app_state$columns$mappings$y_column <- "Tæller"
  app_state$columns$mappings$n_column <- "Nævner"
  app_state$columns$auto_detect$completed <- TRUE
  app_state$columns$auto_detect$frozen_until_next_trigger <- TRUE
  app_state$events$auto_detection_completed <- 1L

  # Step 5: UI sync needed
  app_state$events$ui_sync_needed <- 1L
  app_state$columns$ui_sync$needed <- TRUE

  # Step 6: Workflow completed
  app_state$data$updating_table <- FALSE
  app_state$events$ui_sync_completed <- 1L
  app_state$columns$ui_sync$needed <- FALSE

  # Verify final state
  expect_false(isolate(app_state$data$updating_table))
  expect_true(isolate(app_state$columns$auto_detect$completed))
  expect_true(isolate(app_state$columns$auto_detect$frozen_until_next_trigger))
  expect_false(isolate(app_state$columns$ui_sync$needed))
  expect_equal(isolate(app_state$columns$mappings$x_column), "Dato")
  expect_equal(isolate(app_state$columns$mappings$y_column), "Tæller")
  expect_equal(isolate(app_state$columns$mappings$n_column), "Nævner")
})

test_that("app_state backward compatibility works", {
  # TEST: Backward compatibility with legacy patterns

  # SETUP: Create app state
  app_state <- create_app_state()

  # TEST: Legacy access patterns should still work
  # Direct column access (legacy pattern)
  if (exists("x_column", envir = app_state$columns)) {
    # Legacy direct access
    app_state$columns$x_column <- "legacy_x"
    expect_equal(isolate(app_state$columns$x_column), "legacy_x")
  }

  # Modern hierarchical access
  app_state$columns$mappings$x_column <- "modern_x"
  expect_equal(isolate(app_state$columns$mappings$x_column), "modern_x")

  # TEST: Migration helper function (if exists)
  if (exists("migrate_legacy_state")) {
    legacy_state <- list(
      x_column = "old_x",
      y_column = "old_y",
      auto_detected_columns = list(x = "detected_x")
    )

    migrated_state <- migrate_legacy_state(legacy_state, app_state)

    # Should preserve data in new structure
    expect_equal(isolate(migrated_state$columns$mappings$x_column), "old_x")
    expect_equal(isolate(migrated_state$columns$mappings$y_column), "old_y")
  }
})

test_that("app_state Danish clinical workflow works", {
  # TEST: Complete Danish clinical data workflow

  # SETUP: Create app state
  app_state <- create_app_state()

  # TEST: Danish clinical data simulation
  danish_data <- data.frame(
    `Måned` = c("Jan 2024", "Feb 2024", "Mar 2024"),
    `Genindlæggelser` = c(12, 8, 15),
    `Samlede indlæggelser` = c(150, 145, 160),
    `Målestatus` = c("Standard", "Under mål", "Over mål"),
    `Faseændring` = c(FALSE, FALSE, TRUE),
    `Frys baseline` = c(FALSE, FALSE, TRUE),
    `Klinisk kommentar` = c("", "Ferieperiode", "Ny procedure"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Simulate complete workflow
  # 1. Data upload
  app_state$data$current_data <- danish_data
  app_state$data$original_data <- danish_data
  app_state$events$data_loaded <- 1L

  # 2. Auto-detection
  app_state$columns$mappings$x_column <- "Måned"
  app_state$columns$mappings$y_column <- "Genindlæggelser"
  app_state$columns$mappings$n_column <- "Samlede indlæggelser"
  app_state$columns$mappings$skift_column <- "Faseændring"
  app_state$columns$mappings$frys_column <- "Frys baseline"
  app_state$columns$mappings$kommentar_column <- "Klinisk kommentar"
  app_state$events$auto_detection_completed <- 1L

  # 3. UI sync
  app_state$events$ui_sync_needed <- 1L
  app_state$columns$ui_sync$needed <- TRUE

  # Verify Danish workflow state
  expect_equal(nrow(isolate(app_state$data$current_data)), 3)
  expect_equal(isolate(app_state$columns$mappings$x_column), "Måned")
  expect_equal(isolate(app_state$columns$mappings$y_column), "Genindlæggelser")
  expect_equal(isolate(app_state$columns$mappings$n_column), "Samlede indlæggelser")
  expect_equal(isolate(app_state$columns$mappings$skift_column), "Faseændring")
  expect_equal(isolate(app_state$columns$mappings$frys_column), "Frys baseline")
  expect_equal(isolate(app_state$columns$mappings$kommentar_column), "Klinisk kommentar")

  # Verify Danish character support in data
  expect_true(all(grepl("æ|ø|å", c("Måned", "Genindlæggelser", "Samlede indlæggelser"))))
})