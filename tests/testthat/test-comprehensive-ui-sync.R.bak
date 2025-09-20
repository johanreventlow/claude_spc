# test-comprehensive-ui-sync.R
# Opdaterede tests for unified autodetect og kolonnemapping

make_emit_recorder <- function() {
  calls <- character(0)
  emit <- list(
    auto_detection_completed = function() {
      calls <<- c(calls, "auto_detection_completed")
    },
    ui_sync_needed = function() {
      calls <<- c(calls, "ui_sync_needed")
    },
    data_loaded = function() {
      calls <<- c(calls, "data_loaded")
    }
  )
  list(emit = emit, calls = function() calls)
}

test_that("autodetect_engine kortlægger alle kernekolonner ved filupload", {
  app_state <- create_test_app_state()
  recorder <- make_emit_recorder()

  test_data <- data.frame(
    Dato = as.Date(c("2025-01-01", "2025-01-02", "2025-01-03")),
    Tæller = c(10, 12, 8),
    Nævner = c(100, 110, 95),
    Skift = c("A", "A", "B"),
    Frys = c("", "", ""),
    Kommentarer = c("Normal", "Høj", "Lav"),
    stringsAsFactors = FALSE
  )

  app_state$data$current_data <- test_data

  result <- autodetect_engine(
    data = test_data,
    trigger_type = "file_upload",
    app_state = app_state,
    emit = recorder$emit
  )

  expect_type(result, "list")
  expect_equal(result$x_col, "Dato")
  expect_equal(result$y_col, "Tæller")
  expect_equal(result$n_col, "Nævner")
  expect_equal(result$skift_col, "Skift")
  expect_equal(result$frys_col, "Frys")
  expect_equal(result$kommentar_col, "Kommentarer")

  # Unified state skal være ajour
  expect_equal(app_state$columns$mappings$x_column, "Dato")
  expect_equal(app_state$columns$mappings$y_column, "Tæller")
  expect_equal(app_state$columns$mappings$n_column, "Nævner")
  expect_equal(app_state$columns$mappings$skift_column, "Skift")
  expect_equal(app_state$columns$mappings$frys_column, "Frys")
  expect_equal(app_state$columns$mappings$kommentar_column, "Kommentarer")

  expect_true(app_state$autodetect$frozen_until_next_trigger)
  expect_equal(recorder$calls(), "auto_detection_completed")
})

test_that("autodetect_engine smart-unfreezer kører filupload trods frossen tilstand", {
  app_state <- create_test_app_state()
  recorder <- make_emit_recorder()

  app_state$autodetect$frozen_until_next_trigger <- TRUE

  upload_data <- data.frame(
    Dato = as.Date(c("2025-03-01", "2025-03-02")),
    Tæller = c(4, 6),
    Nævner = c(40, 42),
    stringsAsFactors = FALSE
  )

  result <- autodetect_engine(
    data = upload_data,
    trigger_type = "file_upload",
    app_state = app_state,
    emit = recorder$emit
  )

  expect_equal(result$x_col, "Dato")
  expect_equal(result$y_col, "Tæller")
  expect_equal(recorder$calls(), "auto_detection_completed")
  expect_equal(app_state$columns$mappings$x_column, "Dato")
})

test_that("manual trigger overstyrer frossen tilstand", {
  app_state <- create_test_app_state()
  recorder <- make_emit_recorder()

  app_state$autodetect$frozen_until_next_trigger <- TRUE
  minimal_data <- data.frame(
    Dato = as.Date(c("2025-02-01", "2025-02-02")),
    Tæller = c(5, 9),
    Nævner = c(50, 60),
    stringsAsFactors = FALSE
  )

  result <- autodetect_engine(
    data = minimal_data,
    trigger_type = "manual",
    app_state = app_state,
    emit = recorder$emit
  )

  expect_equal(result$x_col, "Dato")
  expect_equal(result$y_col, "Tæller")
  expect_equal(recorder$calls(), "auto_detection_completed")
  expect_true(app_state$autodetect$frozen_until_next_trigger)
})

test_that("detect_columns_name_based bruger robuste navnemønstre", {
  columns <- c("Måned", "Værdi", "Total", "Skift", "Frys", "Kommentar")
  result <- detect_columns_name_based(columns)

  expect_equal(result$x_col, "Måned")
  expect_equal(result$y_col, "Værdi")
  expect_equal(result$n_col, "Total")
  expect_equal(result$skift_col, "Skift")
  expect_equal(result$frys_col, "Frys")
  expect_equal(result$kommentar_col, "Kommentar")
})
