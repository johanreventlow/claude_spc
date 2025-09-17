# test-name-only-detection-final.R
# Final working tests for detect_columns_name_only() function

# Source required functions
source("../../R/fct_data_processing.R")
source("../../R/utils_session_helpers.R")

context("Name-only column detection - Final Tests")

test_that("detect_columns_name_only grundlæggende funktionalitet", {
  # TEST: Basic name-only detection på standard SPC kolonner

  standard_cols <- c("Dato", "Tæller", "Nævner", "Skift", "Frys", "Kommentar")
  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(standard_cols, NULL, mock_session, mock_values, NULL)

  # TEST: Return value struktur og korrekte detections
  expect_true(is.list(result))
  expect_equal(result$x_col, "Dato")
  expect_equal(result$taeller_col, "Tæller")
  expect_equal(result$naevner_col, "Nævner")
  expect_equal(result$skift_col, "Skift")
  expect_equal(result$frys_col, "Frys")
  expect_equal(result$kommentar_col, "Kommentar")
})

test_that("detect_columns_name_only med partielle matches", {
  # TEST: Når kun nogle standard kolonner findes

  partial_cols <- c("Dato", "Tæller", "Other_Column")
  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(partial_cols, NULL, mock_session, mock_values, NULL)

  # TEST: Fundne kolonner detekteres korrekt, manglende er NULL
  expect_equal(result$x_col, "Dato")
  expect_equal(result$taeller_col, "Tæller")
  expect_null(result$naevner_col)
  expect_null(result$skift_col)
  expect_null(result$frys_col)
  expect_null(result$kommentar_col)
})

test_that("detect_columns_name_only case insensitive matching", {
  # TEST: Case insensitive pattern matching

  mixed_case_cols <- c("DATO", "tæller", "Nævner", "KOMMENTAR")
  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(mixed_case_cols, NULL, mock_session, mock_values, NULL)

  # TEST: Case insensitive matching virker
  expect_equal(result$x_col, "DATO")
  expect_equal(result$taeller_col, "tæller")
  expect_equal(result$naevner_col, "Nævner")
  expect_equal(result$kommentar_col, "KOMMENTAR")
})

test_that("detect_columns_name_only substring matching", {
  # TEST: Substring pattern matching i komplekse kolonnenavne

  complex_cols <- c("Data_Dato_Field", "Count_Tæller", "Total_Nævner_Sum", "User_Kommentar_Notes")
  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(complex_cols, NULL, mock_session, mock_values, NULL)

  # TEST: Substring matches virker korrekt
  expect_equal(result$x_col, "Data_Dato_Field")
  expect_equal(result$taeller_col, "Count_Tæller")
  expect_equal(result$naevner_col, "Total_Nævner_Sum")
  expect_equal(result$kommentar_col, "User_Kommentar_Notes")
  expect_null(result$skift_col)   # Ingen "Skift" substring
  expect_null(result$frys_col)    # Ingen "Frys" substring
})

test_that("detect_columns_name_only fallback til første kolonne", {
  # TEST: X-kolonne fallback når ingen dato patterns findes

  no_date_cols <- c("Value", "Other", "Data")
  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(no_date_cols, NULL, mock_session, mock_values, NULL)

  # TEST: X_col fallback til første kolonne når ingen dato match
  expect_equal(result$x_col, "Value")  # Fallback til første kolonne
  expect_null(result$taeller_col)      # Ingen "tæller" match
  expect_null(result$naevner_col)
  expect_null(result$skift_col)
  expect_null(result$frys_col)
  expect_null(result$kommentar_col)
})

test_that("detect_columns_name_only med overlappende patterns", {
  # TEST: Håndtering af kolonner der matcher flere patterns

  overlapping_cols <- c("Count_Data", "Total_Count")  # "Count" matcher både tæller og naevner
  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(overlapping_cols, NULL, mock_session, mock_values, NULL)

  # TEST: Første match vinder (tæller før naevner i algoritmen)
  expect_equal(result$x_col, "Count_Data")     # Fallback til første
  expect_equal(result$taeller_col, "Count_Data")  # Første match for "count"
  expect_equal(result$naevner_col, "Total_Count")  # "total" match
})

test_that("detect_columns_name_only med tom kolonneliste", {
  # TEST: Håndtering af edge case med ingen kolonner

  empty_cols <- character(0)
  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(empty_cols, NULL, mock_session, mock_values, NULL)

  # TEST: Alle detections er NULL når ingen kolonner
  expect_null(result$x_col)
  expect_null(result$taeller_col)
  expect_null(result$naevner_col)
  expect_null(result$skift_col)
  expect_null(result$frys_col)
  expect_null(result$kommentar_col)
})

test_that("detect_columns_name_only med eksakt skift/frys matches", {
  # TEST: Skift og Frys kræver eksakte matches

  exact_match_cols <- c("Skift", "Frys", "Skift_Extra", "Extra_Frys", "Other")
  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(exact_match_cols, NULL, mock_session, mock_values, NULL)

  # TEST: Kun eksakte "skift" og "frys" matches detekteres
  expect_equal(result$skift_col, "Skift")    # Eksakt match
  expect_equal(result$frys_col, "Frys")      # Eksakt match
  expect_equal(result$x_col, "Skift")        # Fallback til første
})

test_that("detect_columns_name_only pattern priority test", {
  # TEST: Pattern priority og matching order

  priority_cols <- c("Date_Time", "Count_Value", "Denominator", "Shift_Type", "Freeze_Level", "Comment_Field")
  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(priority_cols, NULL, mock_session, mock_values, NULL)

  # TEST: Pattern matches baseret på regex patterns
  expect_equal(result$x_col, "Date_Time")          # "date|time" match
  expect_equal(result$taeller_col, "Count_Value")  # "count" match
  expect_equal(result$naevner_col, "Denominator")  # "denom" match
  expect_null(result$skift_col)                    # "Shift_Type" er ikke eksakt "^skift$"
  expect_null(result$frys_col)                     # "Freeze_Level" er ikke eksakt "^frys$"
  expect_equal(result$kommentar_col, "Comment_Field")  # "comment" match
})