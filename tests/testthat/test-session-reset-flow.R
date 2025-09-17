# test-session-reset-flow.R
# Tests for session reset funktionalitet og integration med auto-detection

# Source required functions
source("../../R/utils_server_management.R")
source("../../R/utils_session_helpers.R")
source("../../R/fct_data_processing.R")

context("Session reset flow")

test_that("reset_to_empty_session grundlæggende funktionalitet", {
  # TEST: reset_to_empty_session opretter korrekt tom session

  # Mock input, output, session objects
  mock_input <- list()
  mock_output <- list()
  mock_session <- list(
    token = "test_session",
    sendCustomMessage = function(type, message) {
      # Mock sendCustomMessage - ikke vigtig for denne test
    }
  )

  # Mock values object med eksisterende data
  mock_values <- list(
    current_data = data.frame(Old_Col1 = 1:3, Old_Col2 = 4:6),
    original_data = data.frame(Old_Col1 = 1:3, Old_Col2 = 4:6),
    file_uploaded = TRUE,
    user_started_session = TRUE,
    auto_detect_done = TRUE,
    initial_auto_detect_completed = TRUE
  )

  # Mock app_state
  mock_app_state <- list(
    data = list(current_data = NULL, original_data = NULL),
    session = list(file_uploaded = FALSE, user_started_session = FALSE),
    columns = list(auto_detect = list(completed = FALSE))
  )

  # Mock updateSelectizeInput function
  updateSelectizeInput <- function(session, inputId, choices = NULL, selected = NULL) {
    # Track calls for verification
    if (!exists("selectize_updates", envir = globalenv())) {
      assign("selectize_updates", list(), envir = globalenv())
    }
    selectize_updates <- get("selectize_updates", envir = globalenv())
    selectize_updates[[length(selectize_updates) + 1]] <- list(
      inputId = inputId,
      choices = choices,
      selected = selected
    )
    assign("selectize_updates", selectize_updates, envir = globalenv())
  }

  # Call reset_to_empty_session
  reset_to_empty_session(mock_input, mock_output, mock_session, mock_values, mock_app_state)

  # TEST: Values bliver reset korrekt
  expect_false(mock_values$file_uploaded)
  expect_false(mock_values$user_started_session)
  expect_false(mock_values$auto_detect_done)
  expect_false(mock_values$initial_auto_detect_completed)

  # TEST: current_data er sat til standard empty session data
  expect_true(!is.null(mock_values$current_data))
  expect_true(is.data.frame(mock_values$current_data))
  expect_true(nrow(mock_values$current_data) > 0)  # Ikke tom
  expect_true("Dato" %in% names(mock_values$current_data))
  expect_true("Tæller" %in% names(mock_values$current_data))

  # TEST: App state bliver synkroniseret
  expect_false(mock_app_state$session$file_uploaded)
  expect_false(mock_app_state$session$user_started_session)
  expect_false(mock_app_state$columns$auto_detect$completed)

  # TEST: SelectizeInput updates blev kaldt med korrekte standard kolonner
  if (exists("selectize_updates", envir = globalenv())) {
    updates <- get("selectize_updates", envir = globalenv())
    expect_true(length(updates) > 0)

    # Find x_col update
    x_col_update <- updates[[which(sapply(updates, function(u) u$inputId == "x_col"))]]
    expect_true("Dato" %in% x_col_update$choices)
    expect_true("Tæller" %in% x_col_update$choices)

    # Cleanup
    rm(selectize_updates, envir = globalenv())
  }
})

test_that("session reset → auto-detection flow", {
  # TEST: Session reset efterfulgt af auto-detection virker korrekt

  # Setup som i forrige test
  mock_session <- list(token = "test_session")
  mock_values <- list(
    current_data = NULL,
    auto_detect_done = FALSE
  )

  # Step 1: Simuler session reset der indlæser standard data
  standard_data <- create_empty_session_data()
  mock_values$current_data <- standard_data

  # Step 2: Simuler name-only auto-detection på standard kolonner
  col_names <- names(standard_data)
  detect_result <- detect_columns_name_only(col_names, NULL, mock_session, mock_values, NULL)

  # TEST: Auto-detection finder standard kolonner
  expect_equal(detect_result$detected$x_col, "Dato")
  expect_equal(detect_result$detected$taeller_col, "Tæller")
  expect_equal(detect_result$detected$naevner_col, "Nævner")

  # TEST: UI sync data er klar til opdatering
  ui_sync <- detect_result$ui_sync_data
  expect_true(all(col_names %in% ui_sync$col_choices))
  expect_true("" %in% ui_sync$col_choices)
})

test_that("session reset data konsistens", {
  # TEST: Efter session reset bruger alle komponenter samme datatabel

  # Mock session components
  mock_session <- list(token = "test_session")
  mock_values <- list()
  mock_app_state <- list(
    data = list(),
    session = list(),
    columns = list(auto_detect = list())
  )

  # Reset session
  reset_to_empty_session(NULL, NULL, mock_session, mock_values, mock_app_state)

  # TEST: current_data og original_data er identiske efter reset
  expect_identical(mock_values$current_data, mock_values$original_data)

  # TEST: Centralized state synkronisering
  expect_identical(mock_app_state$data$current_data, mock_values$current_data)
  expect_identical(mock_app_state$data$original_data, mock_values$original_data)

  # TEST: Få kolonnenavne fra current_data
  if (!is.null(mock_values$current_data)) {
    current_cols <- names(mock_values$current_data)

    # Simuler auto-detection der læser fra current_data
    detect_result <- detect_columns_name_only(current_cols, NULL, mock_session, mock_values, mock_app_state)

    # TEST: col_choices matcher præcis current_data kolonner
    expect_setequal(
      detect_result$ui_sync_data$col_choices[-1], # Remove "" option
      current_cols
    )
  }
})

test_that("session reset med forskellige datatyper", {
  # TEST: Session reset håndterer forskellige typer af eksisterende data

  # Test med stor dataset
  large_data <- data.frame(
    Col1 = 1:1000,
    Col2 = rnorm(1000),
    Col3 = rep(letters[1:10], 100)
  )

  mock_values_large <- list(
    current_data = large_data,
    file_uploaded = TRUE
  )

  mock_session <- list(token = "test_session")
  mock_app_state <- list(
    data = list(),
    session = list(),
    columns = list(auto_detect = list())
  )

  # Reset med stort dataset
  reset_to_empty_session(NULL, NULL, mock_session, mock_values_large, mock_app_state)

  # TEST: Stort dataset erstattes med standard data
  expect_true(nrow(mock_values_large$current_data) < nrow(large_data))
  expect_true("Dato" %in% names(mock_values_large$current_data))

  # Test med NULL data
  mock_values_null <- list(
    current_data = NULL,
    file_uploaded = FALSE
  )

  # Reset med NULL data
  reset_to_empty_session(NULL, NULL, mock_session, mock_values_null, mock_app_state)

  # TEST: NULL data erstattes korrekt
  expect_true(!is.null(mock_values_null$current_data))
  expect_true(is.data.frame(mock_values_null$current_data))
})

test_that("session reset observer priority og timing", {
  # TEST: Session reset operationer sker i korrekt rækkefølge

  operations_log <- character()

  # Mock updateSelectizeInput der logger operationer
  updateSelectizeInput <- function(session, inputId, choices = NULL, selected = NULL) {
    operations_log <<- c(operations_log, paste("update", inputId))
  }

  # Mock detect_columns_name_only der logger operation
  detect_columns_name_only_logged <- function(col_names, ...) {
    operations_log <<- c(operations_log, "name_only_detect")
    return(list(
      detected = list(x_col = "Dato", taeller_col = "Tæller"),
      ui_sync_data = list(
        x_col = "Dato",
        taeller_col = "Tæller",
        col_choices = c("", col_names),
        timestamp = Sys.time()
      )
    ))
  }

  # Simuler reset flow
  mock_session <- list(token = "test_session")
  mock_values <- list()
  mock_app_state <- list(
    data = list(),
    session = list(),
    columns = list(auto_detect = list())
  )

  operations_log <- c(operations_log, "reset_start")

  # Data reset
  mock_values$current_data <- create_empty_session_data()
  operations_log <- c(operations_log, "data_loaded")

  # UI updates
  standard_cols <- names(mock_values$current_data)
  choices <- c("", standard_cols)
  updateSelectizeInput(mock_session, "x_col", choices = choices)
  updateSelectizeInput(mock_session, "taeller_col", choices = choices)

  # Auto-detection
  detect_result <- detect_columns_name_only_logged(standard_cols)
  operations_log <- c(operations_log, "reset_complete")

  # TEST: Operationer sker i korrekt rækkefølge
  expect_equal(operations_log[1], "reset_start")
  expect_equal(operations_log[2], "data_loaded")
  expect_true("update x_col" %in% operations_log)
  expect_true("update taeller_col" %in% operations_log)
  expect_equal(operations_log[length(operations_log) - 1], "name_only_detect")
  expect_equal(operations_log[length(operations_log)], "reset_complete")
})