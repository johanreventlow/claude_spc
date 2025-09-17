# test-name-only-detection.R
# Tests for detect_columns_name_only() function

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

  # TEST: Function returns structure with detected columns
  expect_true(is.list(result))

  # TEST: Standard kolonner bliver korrekt detekteret
  detected <- result
  expect_equal(detected$x_col, "Dato")
  expect_equal(detected$taeller_col, "Tæller")
  expect_equal(detected$naevner_col, "Nævner")
  expect_equal(detected$skift_col, "Skift")
  expect_equal(detected$frys_col, "Frys")
  expect_equal(detected$kommentar_col, "Kommentar")

  # TEST: UI sync data blev sat i values
  expect_true(!is.null(mock_values$ui_sync_needed))
  ui_sync <- mock_values$ui_sync_needed
  expect_equal(ui_sync$x_col, "Dato")
  expect_equal(ui_sync$taeller_col, "Tæller")
  expect_true("" %in% ui_sync$col_choices)
  expect_true(all(standard_cols %in% ui_sync$col_choices))
})

test_that("detect_columns_name_only med variationer i navne", {
  # TEST: Name-only detection med case-insensitive matching

  # Mock kolonner med forskellige cases
  mixed_case_cols <- c("dato", "TÆLLER", "Nævner", "skift", "FRYS", "kommentar")

  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(mixed_case_cols, NULL, mock_session, mock_values, NULL)

  # TEST: Case-insensitive matching virker
  detected <- result
  expect_equal(detected$x_col, "dato")
  expect_equal(detected$taeller_col, "TÆLLER")
  expect_equal(detected$naevner_col, "Nævner")
  expect_equal(detected$skift_col, "skift")
  expect_equal(detected$frys_col, "FRYS")
  expect_equal(detected$kommentar_col, "kommentar")
})

test_that("detect_columns_name_only med partielle matches", {
  # TEST: Name-only detection når kun nogle kolonner findes

  # Mock kolonner hvor kun nogle standard kolonner findes
  partial_cols <- c("Dato", "Tæller", "Andre_kolonner", "Yderligere_data")

  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(partial_cols, NULL, mock_session, mock_values, NULL)

  # TEST: Kun matchende kolonner detekteres
  detected <- result$detected
  expect_equal(detected$x_col, "Dato")
  expect_equal(detected$taeller_col, "Tæller")
  expect_null(detected$naevner_col)  # Ikke fundet
  expect_null(detected$skift_col)    # Ikke fundet
  expect_null(detected$frys_col)     # Ikke fundet
  expect_null(detected$kommentar_col) # Ikke fundet
})

test_that("detect_columns_name_only UI sync data struktur", {
  # TEST: UI sync data struktur er korrekt

  standard_cols <- c("Dato", "Tæller", "Nævner", "Skift", "Frys", "Kommentar")
  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(standard_cols, NULL, mock_session, mock_values, NULL)

  # TEST: UI sync data indeholder alle nødvendige felter
  ui_sync <- result$ui_sync_data
  expect_true("x_col" %in% names(ui_sync))
  expect_true("taeller_col" %in% names(ui_sync))
  expect_true("naevner_col" %in% names(ui_sync))
  expect_true("skift_col" %in% names(ui_sync))
  expect_true("frys_col" %in% names(ui_sync))
  expect_true("kommentar_col" %in% names(ui_sync))
  expect_true("col_choices" %in% names(ui_sync))
  expect_true("timestamp" %in% names(ui_sync))

  # TEST: col_choices inkluderer tom option plus alle kolonner
  expect_true("" %in% ui_sync$col_choices)
  expect_true(all(standard_cols %in% ui_sync$col_choices))
  expect_equal(length(ui_sync$col_choices), length(standard_cols) + 1)  # "" + alle kolonner
})

test_that("detect_columns_name_only med komplekse kolonnenavne", {
  # TEST: Name-only detection med navne der indeholder target strings

  # Mock kolonner hvor target strings er substring af større navne
  complex_cols <- c("Start_Dato_Slut", "Tæller_Værdi", "Total_Nævner", "Skift_Type", "Frys_Niveau", "Kommentar_Text")

  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(complex_cols, NULL, mock_session, mock_values, NULL)

  # TEST: Substring matches virker korrekt
  detected <- result$detected
  expect_equal(detected$x_col, "Start_Dato_Slut")
  expect_equal(detected$taeller_col, "Tæller_Værdi")
  expect_equal(detected$naevner_col, "Total_Nævner")
  expect_equal(detected$skift_col, "Skift_Type")
  expect_equal(detected$frys_col, "Frys_Niveau")
  expect_equal(detected$kommentar_col, "Kommentar_Text")
})

test_that("detect_columns_name_only med ingen matches", {
  # TEST: Name-only detection når ingen standard kolonner findes

  # Mock kolonner uden standard navne
  no_match_cols <- c("Andet", "Data", "Information", "Værdi")

  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(no_match_cols, NULL, mock_session, mock_values, NULL)

  # TEST: Alle kolonner er NULL når ingen matches
  detected <- result$detected
  expect_null(detected$x_col)
  expect_null(detected$taeller_col)
  expect_null(detected$naevner_col)
  expect_null(detected$skift_col)
  expect_null(detected$frys_col)
  expect_null(detected$kommentar_col)

  # TEST: col_choices indeholder stadig alle tilgængelige kolonner
  ui_sync <- result$ui_sync_data
  expect_true(all(no_match_cols %in% ui_sync$col_choices))
  expect_true("" %in% ui_sync$col_choices)
})

test_that("detect_columns_name_only centralized state sync", {
  # TEST: Name-only detection synkroniserer til centralized state når tilgængelig

  standard_cols <- c("Dato", "Tæller", "Nævner")
  mock_session <- list(token = "test_session")
  mock_values <- list()

  # Mock centralized state
  mock_app_state <- list(
    columns = list(
      detected = list(),
      ui_sync = list()
    )
  )

  # Set global variables for centralized state
  assign("use_centralized_state", TRUE, envir = globalenv())

  result <- detect_columns_name_only(standard_cols, NULL, mock_session, mock_values, mock_app_state)

  # TEST: Centralized state bliver opdateret
  expect_equal(mock_app_state$columns$detected$x_col, "Dato")
  expect_equal(mock_app_state$columns$detected$taeller_col, "Tæller")
  expect_equal(mock_app_state$columns$detected$naevner_col, "Nævner")

  # Cleanup
  if (exists("use_centralized_state", envir = globalenv())) {
    rm(use_centralized_state, envir = globalenv())
  }
})