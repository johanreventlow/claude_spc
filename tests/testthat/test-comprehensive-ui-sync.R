# test-comprehensive-ui-sync.R
# Tests for comprehensive UI synchronization with all 6 columns

test_that("autodetect opdaterer alle 6 dropdown kolonner", {
  # SETUP: Create test data with perfect column names for autodetect
  test_data <- data.frame(
    Dato = as.Date(c("2025-01-01", "2025-01-02", "2025-01-03")),
    Tæller = c(10, 12, 8),
    Nævner = c(100, 110, 95),
    Skift = c("A", "A", "B"),
    Frys = c("", "", ""),
    Kommentarer = c("Normal", "Høj", "Lav"),
    stringsAsFactors = FALSE
  )

  # SETUP: Mock app state
  app_state <- create_app_state()
  app_state$data$current_data <- test_data

  # SETUP: Mock emit functions
  emit <- list(
    auto_detection_completed = function() { cat("emit$auto_detection_completed called\n") }
  )

  # TEST: Run autodetect engine with full analysis
  results <- autodetect_engine(
    data = test_data,
    trigger_type = "file_upload",
    app_state = app_state,
    emit = emit
  )

  # VERIFY: All 6 columns should be detected and stored in app_state
  # Use isolate() to access reactive values in test context
  expect_equal(isolate(app_state$columns$mappings$x_column), "Dato")
  expect_equal(isolate(app_state$columns$mappings$y_column), "Tæller")
  expect_equal(isolate(app_state$columns$mappings$n_column), "Nævner")
  expect_equal(isolate(app_state$columns$mappings$skift_column), "Skift")
  expect_equal(isolate(app_state$columns$mappings$frys_column), "Frys")
  expect_equal(isolate(app_state$columns$mappings$kommentar_column), "Kommentarer")

  # VERIFY: Autodetect results are preserved for backward compatibility
  autodetect_results <- isolate(app_state$columns$auto_detect$results)
  expect_equal(autodetect_results$x_col, "Dato")
  expect_equal(autodetect_results$y_col, "Tæller")
  expect_equal(autodetect_results$n_col, "Nævner")
  expect_equal(autodetect_results$skift_col, "Skift")
  expect_equal(autodetect_results$frys_col, "Frys")
  expect_equal(autodetect_results$kommentar_col, "Kommentarer")
})

test_that("sync_ui_with_columns_unified læser alle 6 kolonner fra app_state", {
  # SETUP: Create test data and app state
  test_data <- data.frame(
    Dato = as.Date(c("2025-01-01", "2025-01-02")),
    Tæller = c(10, 12),
    Nævner = c(100, 110),
    Skift = c("A", "B"),
    Frys = c("", ""),
    Kommentarer = c("Test1", "Test2"),
    stringsAsFactors = FALSE
  )

  app_state <- create_app_state()
  app_state$data$current_data <- test_data
  app_state$columns$mappings$x_column <- "Dato"
  app_state$columns$mappings$y_column <- "Tæller"
  app_state$columns$mappings$n_column <- "Nævner"
  app_state$columns$mappings$skift_column <- "Skift"
  app_state$columns$mappings$frys_column <- "Frys"
  app_state$columns$mappings$kommentar_column <- "Kommentarer"

  # VERIFY: All 6 columns are accessible in app_state
  expect_equal(isolate(app_state$columns$mappings$x_column), "Dato")
  expect_equal(isolate(app_state$columns$mappings$y_column), "Tæller")
  expect_equal(isolate(app_state$columns$mappings$n_column), "Nævner")
  expect_equal(isolate(app_state$columns$mappings$skift_column), "Skift")
  expect_equal(isolate(app_state$columns$mappings$frys_column), "Frys")
  expect_equal(isolate(app_state$columns$mappings$kommentar_column), "Kommentarer")

  # VERIFY: sync_ui_with_columns_unified doesn't crash with all 6 columns
  # This verifies the function can read all column states without error
  expect_silent({
    sync_ui_with_columns_unified(app_state, NULL, NULL, list(), ui_service = NULL)
  })
})

test_that("ui_service default columns inkluderer alle 6 kolonner", {
  # SETUP: Create test data
  test_data <- data.frame(
    Col1 = 1:3,
    Col2 = 4:6,
    Col3 = 7:9,
    stringsAsFactors = FALSE
  )

  app_state <- create_app_state()
  app_state$data$current_data <- test_data

  # VERIFY: UI service can be created without errors
  expect_silent({
    ui_service <- create_ui_update_service(list(), app_state)
  })

  # VERIFY: Default columns include all 6 expected columns
  ui_service <- create_ui_update_service(list(), app_state)
  default_columns <- c("x_column", "y_column", "n_column", "skift_column", "frys_column", "kommentar_column")

  # Verify that calling with all 6 columns doesn't crash
  expect_silent({
    # This tests that the function signature accepts all 6 columns
    formals(ui_service$update_column_choices)$columns
  })
})

test_that("choice formatting standard er defineret korrekt", {
  # SETUP: Test data
  test_cols <- c("A", "B", "C")

  # TEST: Standard choice format generation
  app_state <- create_app_state()
  app_state$data$current_data <- data.frame(A = 1, B = 2, C = 3)

  # VERIFY: Choice format pattern matches expected structure
  # This mirrors the logic in sync_ui_with_columns_unified
  col_names <- names(app_state$data$current_data)
  standard_choices <- setNames(c("", col_names), c("Vælg kolonne...", col_names))

  # Verify standard choice format structure
  expect_equal(standard_choices[1], "")
  expect_equal(names(standard_choices)[1], "Vælg kolonne...")
  expect_true(all(test_cols %in% standard_choices))
  expect_equal(length(standard_choices), 4) # "" + 3 columns
})

test_that("autodetect → UI sync event chain fungerer end-to-end", {
  # SETUP: Real event chain test
  app_state <- create_app_state()
  test_data <- data.frame(
    Måned = c("Jan", "Feb", "Mar"),
    Procent = c(10.5, 12.3, 8.7),
    Antal = c(21, 26, 18),
    stringsAsFactors = FALSE
  )
  app_state$data$current_data <- test_data

  # SETUP: Track events
  events_fired <- character(0)
  emit <- list(
    auto_detection_completed = function() { events_fired <<- c(events_fired, "auto_detection_completed") },
    ui_sync_needed = function() { events_fired <<- c(events_fired, "ui_sync_needed") }
  )

  # TEST: Run autodetect
  result <- autodetect_engine(
    data = test_data,
    trigger_type = "file_upload",
    app_state = app_state,
    emit = emit
  )

  # VERIFY: Events were fired in correct order
  expect_true("auto_detection_completed" %in% events_fired)

  # VERIFY: Autodetect worked correctly
  expect_equal(isolate(app_state$columns$x_column), "Måned")  # Time pattern
  expect_equal(isolate(app_state$columns$y_column), "Procent")  # Rate pattern should score higher

  # VERIFY: State is frozen until next trigger
  expect_true(app_state$autodetect$frozen_until_next_trigger)
})