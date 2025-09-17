# test-name-only-detection-working.R
# Tests for detect_columns_name_only() function that work with actual implementation

# Source required functions
source("../../R/fct_data_processing.R")
source("../../R/utils_session_helpers.R")

context("Name-only column detection - Working Tests")

test_that("detect_columns_name_only returnerer korrekte kolonner", {
  # TEST: Basis funktionalitet med standard kolonner

  standard_cols <- c("Dato", "Tæller", "Nævner", "Skift", "Frys", "Kommentar")
  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(standard_cols, NULL, mock_session, mock_values, NULL)

  # TEST: Return value struktur
  expect_true(is.list(result))
  expect_equal(result$x_col, "Dato")
  expect_equal(result$taeller_col, "Tæller")
  expect_equal(result$naevner_col, "Nævner")
  expect_equal(result$skift_col, "Skift")
  expect_equal(result$frys_col, "Frys")
  expect_equal(result$kommentar_col, "Kommentar")
})

test_that("detect_columns_name_only med partielle kolonner", {
  # TEST: Når kun nogle standard kolonner findes

  partial_cols <- c("Dato", "Tæller", "Other_Col")
  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(partial_cols, NULL, mock_session, mock_values, NULL)

  # TEST: Fundne kolonner detekteres, manglende er NULL
  expect_equal(result$x_col, "Dato")
  expect_equal(result$taeller_col, "Tæller")
  expect_null(result$naevner_col)
  expect_null(result$skift_col)
  expect_null(result$frys_col)
  expect_null(result$kommentar_col)
})

test_that("detect_columns_name_only fallback behavior", {
  # TEST: Fallback til første kolonne når ingen dato findes

  no_date_cols <- c("Count", "Total", "Other")
  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(no_date_cols, NULL, mock_session, mock_values, NULL)

  # TEST: X_col fallback til første kolonne
  expect_equal(result$x_col, "Count")  # Fallback
  expect_null(result$taeller_col)       # Ingen match for "tæller"
  expect_null(result$naevner_col)
  expect_null(result$skift_col)
  expect_null(result$frys_col)
  expect_null(result$kommentar_col)
})

test_that("detect_columns_name_only case insensitive matching", {
  # TEST: Case insensitive pattern matching

  mixed_case_cols <- c("DATO", "tæller", "Nævner")
  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(mixed_case_cols, NULL, mock_session, mock_values, NULL)

  # TEST: Case insensitive matching virker
  expect_equal(result$x_col, "DATO")
  expect_equal(result$taeller_col, "tæller")
  expect_equal(result$naevner_col, "Nævner")
})

test_that("detect_columns_name_only substring matching", {
  # TEST: Substring pattern matching

  substring_cols <- c("Start_Dato_End", "My_Tæller_Value", "Total_Nævner_Count", "User_Kommentar_Field")
  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(substring_cols, NULL, mock_session, mock_values, NULL)

  # TEST: Substring matches virker
  expect_equal(result$x_col, "Start_Dato_End")
  expect_equal(result$taeller_col, "My_Tæller_Value")
  expect_equal(result$naevner_col, "Total_Nævner_Count")
  expect_equal(result$kommentar_col, "User_Kommentar_Field")
})

test_that("detect_columns_name_only sætter auto_detected_columns i values", {
  # TEST: Funktionen opdaterer values$auto_detected_columns

  standard_cols <- c("Dato", "Tæller")
  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(standard_cols, NULL, mock_session, mock_values, NULL)

  # TEST: auto_detected_columns blev sat i values
  expect_true(!is.null(mock_values$auto_detected_columns))
  expect_equal(mock_values$auto_detected_columns$x_col, "Dato")
  expect_equal(mock_values$auto_detected_columns$taeller_col, "Tæller")
})

test_that("detect_columns_name_only sætter ui_sync_needed i values", {
  # TEST: Funktionen opretter UI sync data

  test_cols <- c("Dato", "Tæller", "Extra")
  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(test_cols, NULL, mock_session, mock_values, NULL)

  # TEST: ui_sync_needed blev sat (når centralized state ikke bruges)
  expect_true(!is.null(mock_values$ui_sync_needed))

  ui_sync <- mock_values$ui_sync_needed
  expect_equal(ui_sync$x_col, "Dato")
  expect_equal(ui_sync$taeller_col, "Tæller")

  # TEST: col_choices indeholder alle kolonner plus tom option
  expect_true("" %in% ui_sync$col_choices)
  expect_true(all(test_cols %in% ui_sync$col_choices))
  expect_equal(length(ui_sync$col_choices), length(test_cols) + 1)
})