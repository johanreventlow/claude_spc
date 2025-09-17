# test-name-only-detection-fixed.R
# Tests for detect_columns_name_only() function with correct return structure

# Source required functions
source("../../R/fct_data_processing.R")
source("../../R/utils_session_helpers.R")

context("Name-only column detection")

test_that("detect_columns_name_only grundlæggende funktionalitet", {
  # TEST: Name-only detection på standard SPC kolonner

  # Mock standard kolonnenavne
  standard_cols <- c("Dato", "Tæller", "Nævner", "Skift", "Frys", "Kommentar")

  # Mock session og values objects
  mock_session <- list(token = "test_session")
  mock_values <- list()

  # Call detect_columns_name_only
  result <- detect_columns_name_only(standard_cols, NULL, mock_session, mock_values, NULL)

  # TEST: Function returns correct detection results
  expect_true(is.list(result))
  expect_equal(result$x_col, "Dato")
  expect_equal(result$taeller_col, "Tæller")
  expect_equal(result$naevner_col, "Nævner")
  expect_equal(result$skift_col, "Skift")
  expect_equal(result$frys_col, "Frys")
  expect_equal(result$kommentar_col, "Kommentar")

  # TEST: UI sync data blev sat i values
  expect_true(!is.null(mock_values$ui_sync_needed))
  ui_sync <- mock_values$ui_sync_needed
  expect_equal(ui_sync$x_col, "Dato")
  expect_equal(ui_sync$taeller_col, "Tæller")
  expect_true("" %in% ui_sync$col_choices)
  expect_true(all(standard_cols %in% ui_sync$col_choices))
})

test_that("detect_columns_name_only med partielle matches", {
  # TEST: Name-only detection når kun nogle kolonner findes

  partial_cols <- c("Dato", "Tæller", "Andre_kolonner", "Yderligere_data")
  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(partial_cols, NULL, mock_session, mock_values, NULL)

  # TEST: Kun matchende kolonner detekteres
  expect_equal(result$x_col, "Dato")
  expect_equal(result$taeller_col, "Tæller")
  expect_null(result$naevner_col)
  expect_null(result$skift_col)
  expect_null(result$frys_col)
  expect_null(result$kommentar_col)

  # TEST: UI sync data indeholder alle kolonner (ikke kun matchede)
  ui_sync <- mock_values$ui_sync_needed
  expect_true(all(partial_cols %in% ui_sync$col_choices))
})

test_that("detect_columns_name_only med komplekse kolonnenavne", {
  # TEST: Name-only detection med substring matches

  complex_cols <- c("Start_Dato_Slut", "Tæller_Værdi", "Total_Nævner", "Kommentar_Text")
  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(complex_cols, NULL, mock_session, mock_values, NULL)

  # TEST: Substring matches virker
  expect_equal(result$x_col, "Start_Dato_Slut")
  expect_equal(result$taeller_col, "Tæller_Værdi")
  expect_equal(result$naevner_col, "Total_Nævner")
  expect_equal(result$kommentar_col, "Kommentar_Text")
  expect_null(result$skift_col)  # Ingen "Skift" substring
  expect_null(result$frys_col)   # Ingen "Frys" substring
})

test_that("detect_columns_name_only med ingen matches", {
  # TEST: Name-only detection når ingen standard kolonner findes

  no_match_cols <- c("Andet", "Data", "Information", "Værdi")
  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(no_match_cols, NULL, mock_session, mock_values, NULL)

  # TEST: X_col fallback til første kolonne, resten NULL
  expect_equal(result$x_col, "Andet")  # Fallback til første kolonne
  expect_null(result$taeller_col)
  expect_null(result$naevner_col)
  expect_null(result$skift_col)
  expect_null(result$frys_col)
  expect_null(result$kommentar_col)

  # TEST: UI sync data indeholder alle tilgængelige kolonner
  ui_sync <- mock_values$ui_sync_needed
  expect_true(all(no_match_cols %in% ui_sync$col_choices))
  expect_true("" %in% ui_sync$col_choices)
})

test_that("detect_columns_name_only med centralized state", {
  # TEST: Name-only detection med centralized state

  standard_cols <- c("Dato", "Tæller", "Nævner")
  mock_session <- list(token = "test_session")
  mock_values <- list()

  # Mock centralized state
  mock_app_state <- list(
    auto_detected_columns = list(),
    ui_sync_needed = list()
  )

  # Set global variables that function checks for
  assign("use_centralized_state", TRUE, envir = globalenv())
  assign("app_state", mock_app_state, envir = globalenv())

  result <- detect_columns_name_only(standard_cols, NULL, mock_session, mock_values, mock_app_state)

  # TEST: Return value er korrekt
  expect_equal(result$x_col, "Dato")
  expect_equal(result$taeller_col, "Tæller")
  expect_equal(result$naevner_col, "Nævner")

  # TEST: Centralized state bliver opdateret
  expect_equal(mock_app_state$auto_detected_columns$x_col, "Dato")
  expect_equal(mock_app_state$auto_detected_columns$taeller_col, "Tæller")
  expect_equal(mock_app_state$auto_detected_columns$naevner_col, "Nævner")

  # TEST: UI sync data i centralized state
  expect_equal(mock_app_state$ui_sync_needed$x_col, "Dato")
  expect_equal(mock_app_state$ui_sync_needed$taeller_col, "Tæller")

  # Cleanup global variables
  if (exists("use_centralized_state", envir = globalenv())) {
    rm(use_centralized_state, envir = globalenv())
  }
  if (exists("app_state", envir = globalenv())) {
    rm(app_state, envir = globalenv())
  }
})