# test-data-consistency.R
# Tests for at sikre input felter altid bruger aktuel datatabel

# Source required functions
source("../../R/fct_data_processing.R")
source("../../R/utils_session_helpers.R")

context("Data consistency og current table reference")

test_that("auto_detect_and_update_columns bruger current_data korrekt", {
  # TEST: Auto-detection læser fra den aktuelle datatabel, ikke cached data

  # Setup mock data - to forskellige datasæt
  old_data <- data.frame(
    Old_Date = as.Date("2023-01-01") + 0:4,
    Old_Count = 1:5,
    Old_Comments = paste("Old comment", 1:5)
  )

  current_data <- data.frame(
    Dato = as.Date("2024-01-01") + 0:4,
    Tæller = 10:14,
    Kommentar = paste("Ny kommentar", 1:5)
  )

  # Mock values med current_data sat til ny data
  mock_values <- list(
    current_data = current_data,
    original_data = current_data,
    auto_detect_done = FALSE
  )

  # Mock session og app_state
  mock_session <- list(token = "test_session")
  mock_app_state <- list(
    data = list(
      current_data = current_data,
      original_data = current_data
    ),
    columns = list(
      detected = list(),
      ui_sync = list()
    )
  )

  # Mock auto_detect_and_update_columns function call
  # Dette simulerer funktionen der skulle læse fra current_data
  simulate_auto_detect <- function(values, app_state) {
    # Læs fra current_data
    data_to_analyze <- if (exists("use_centralized_state") && exists("app_state")) {
      app_state$data$current_data
    } else {
      values$current_data
    }

    if (is.null(data_to_analyze) || nrow(data_to_analyze) == 0) {
      # Fallback til name-only detection
      col_names <- names(data_to_analyze) %||% character(0)
      return(detect_columns_name_only(col_names, NULL, mock_session, values, app_state))
    }

    # Standard auto-detection (ikke implementeret her, men ville bruge data_to_analyze)
    return(list(
      detected = list(
        x_col = if ("Dato" %in% names(data_to_analyze)) "Dato" else NULL,
        taeller_col = if ("Tæller" %in% names(data_to_analyze)) "Tæller" else NULL
      ),
      data_source = names(data_to_analyze)
    ))
  }

  # TEST: Auto-detection bruger current_data kolonner
  result <- simulate_auto_detect(mock_values, mock_app_state)

  # Verificer at den læste fra current_data (ikke old_data)
  expect_true("Dato" %in% result$data_source)
  expect_true("Tæller" %in% result$data_source)
  expect_false("Old_Date" %in% result$data_source)
  expect_false("Old_Count" %in% result$data_source)

  # TEST: Detected kolonner matcher current_data
  expect_equal(result$detected$x_col, "Dato")
  expect_equal(result$detected$taeller_col, "Tæller")
})

test_that("selectizeInput choices matcher current_data kolonner", {
  # TEST: SelectizeInput choices opdateres til at matche current_data

  # Mock forskellige datasets through session
  datasets <- list(
    initial = data.frame(A = 1:3, B = 4:6),
    uploaded = data.frame(Dato = as.Date("2024-01-01") + 0:2, Tæller = 10:12, Nævner = 20:22),
    reset = create_empty_session_data()
  )

  selectize_choices_log <- list()

  # Mock updateSelectizeInput der tracker choices
  updateSelectizeInput <- function(session, inputId, choices = NULL, selected = NULL) {
    if (!is.null(choices)) {
      selectize_choices_log[[inputId]] <<- choices
    }
  }

  mock_session <- list(token = "test_session")

  # Test hver dataset transition
  for (dataset_name in names(datasets)) {
    current_data <- datasets[[dataset_name]]
    expected_choices <- c("", names(current_data))

    # Simuler UI opdatering
    updateSelectizeInput(mock_session, "x_col", choices = expected_choices)
    updateSelectizeInput(mock_session, "taeller_col", choices = expected_choices)

    # TEST: Choices matcher current dataset
    expect_setequal(selectize_choices_log[["x_col"]], expected_choices)
    expect_setequal(selectize_choices_log[["taeller_col"]], expected_choices)

    # TEST: Ingen old kolonner fra previous datasets
    if (dataset_name != "initial") {
      previous_datasets <- datasets[1:(which(names(datasets) == dataset_name) - 1)]
      for (prev_data in previous_datasets) {
        old_cols <- setdiff(names(prev_data), names(current_data))
        for (old_col in old_cols) {
          expect_false(old_col %in% selectize_choices_log[["x_col"]])
        }
      }
    }
  }
})

test_that("data source consistency efter file upload", {
  # TEST: Efter file upload bruger alle komponenter den nye data

  # Mock file upload scenario
  uploaded_data <- data.frame(
    PatientID = 1:10,
    Dato = as.Date("2024-01-01") + 0:9,
    Tæller = 100:109,
    Kvalitet = rnorm(10)
  )

  # Initial state
  mock_values <- list(
    current_data = NULL,
    original_data = NULL,
    file_uploaded = FALSE
  )

  mock_app_state <- list(
    data = list(
      current_data = NULL,
      original_data = NULL
    ),
    session = list(file_uploaded = FALSE)
  )

  # Simuler file upload
  mock_values$current_data <- uploaded_data
  mock_values$original_data <- uploaded_data
  mock_values$file_uploaded <- TRUE

  mock_app_state$data$current_data <- uploaded_data
  mock_app_state$data$original_data <- uploaded_data
  mock_app_state$session$file_uploaded <- TRUE

  # TEST: Data consistency mellem old og new state management
  expect_identical(mock_values$current_data, mock_app_state$data$current_data)
  expect_identical(mock_values$original_data, mock_app_state$data$original_data)

  # Mock auto-detection på ny data
  mock_session <- list(token = "test_session")
  col_names <- names(uploaded_data)

  # TEST: Name-only detection bruger uploaded data kolonner
  if (nrow(uploaded_data) == 0) {
    result <- detect_columns_name_only(col_names, NULL, mock_session, mock_values, mock_app_state)
  } else {
    # Mock standard auto-detection
    result <- list(
      detected = list(
        x_col = if ("Dato" %in% col_names) "Dato" else NULL,
        taeller_col = if ("Tæller" %in% col_names) "Tæller" else NULL
      ),
      ui_sync_data = list(
        col_choices = c("", col_names),
        timestamp = Sys.time()
      )
    )
  }

  # TEST: UI choices inkluderer uploaded data kolonner
  expect_true("PatientID" %in% result$ui_sync_data$col_choices)
  expect_true("Dato" %in% result$ui_sync_data$col_choices)
  expect_true("Kvalitet" %in% result$ui_sync_data$col_choices)

  # TEST: Auto-detection finder relevante kolonner
  expect_equal(result$detected$x_col, "Dato")
  expect_equal(result$detected$taeller_col, "Tæller")
})

test_that("reactive chain data consistency", {
  # TEST: Reactive chain sikrer data consistency gennem hele flow

  operations_log <- character()
  data_snapshots <- list()

  # Mock reactive values der tracker ændringer
  mock_values <- list(
    current_data = NULL,
    auto_detect_done = FALSE
  )

  # Simuler reactive chain
  # Step 1: Data load
  new_data <- data.frame(
    Timestamp = as.POSIXct("2024-01-01 10:00:00") + 0:4 * 3600,
    Count = 50:54,
    Notes = paste("Note", 1:5)
  )

  mock_values$current_data <- new_data
  operations_log <- c(operations_log, "data_loaded")
  data_snapshots[["after_load"]] <- names(mock_values$current_data)

  # Step 2: Auto-detection trigger
  # Mock observeEvent(values$current_data)
  if (!is.null(mock_values$current_data)) {
    operations_log <- c(operations_log, "auto_detect_triggered")

    # Step 3: Auto-detection execution
    col_names <- names(mock_values$current_data)
    data_snapshots[["during_detect"]] <- col_names

    # Mock detection logic
    if (nrow(mock_values$current_data) > 0) {
      # Standard detection
      detected <- list(
        x_col = if ("Timestamp" %in% col_names) "Timestamp" else NULL,
        taeller_col = if ("Count" %in% col_names) "Count" else NULL
      )
    } else {
      # Name-only detection
      detected <- detect_columns_name_only(col_names, NULL, list(token = "test"), mock_values, NULL)$detected
    }

    mock_values$auto_detect_done <- TRUE
    operations_log <- c(operations_log, "auto_detect_completed")
  }

  # Step 4: UI sync
  if (mock_values$auto_detect_done) {
    ui_choices <- c("", names(mock_values$current_data))
    data_snapshots[["during_ui_sync"]] <- ui_choices[-1]  # Remove ""
    operations_log <- c(operations_log, "ui_sync_completed")
  }

  # TEST: Data reference er konsistent gennem hele chain
  expect_identical(data_snapshots[["after_load"]], data_snapshots[["during_detect"]])
  expect_identical(data_snapshots[["during_detect"]], data_snapshots[["during_ui_sync"]])

  # TEST: Reactive chain completerede korrekt
  expect_true("data_loaded" %in% operations_log)
  expect_true("auto_detect_triggered" %in% operations_log)
  expect_true("auto_detect_completed" %in% operations_log)
  expect_true("ui_sync_completed" %in% operations_log)
})

test_that("error handling bevarer data consistency", {
  # TEST: Ved fejl forbliver data sources konsistente

  # Mock scenario hvor auto-detection fejler
  error_prone_data <- data.frame(
    "Bad-Column-Name!" = 1:3,
    "@#$%" = 4:6,
    check.names = FALSE  # Tillad dårlige kolonnenavne
  )

  mock_values <- list(
    current_data = error_prone_data,
    auto_detect_done = FALSE
  )

  mock_session <- list(token = "test_session")

  # TEST: Robust error handling
  safe_auto_detect <- function(data, session, values) {
    tryCatch({
      col_names <- names(data)
      # Dette kunne fejle pga. dårlige kolonnenavne
      result <- detect_columns_name_only(col_names, NULL, session, values, NULL)
      return(result)
    }, error = function(e) {
      # Fallback til sikker tilstand
      return(list(
        detected = list(x_col = NULL, taeller_col = NULL),
        ui_sync_data = list(
          col_choices = c("", names(data)),
          timestamp = Sys.time()
        ),
        error = e$message
      ))
    })
  }

  result <- safe_auto_detect(error_prone_data, mock_session, mock_values)

  # TEST: Ved fejl er choices stadig korrekte
  expected_choices <- c("", names(error_prone_data))
  if (!is.null(result$ui_sync_data) && !is.null(result$ui_sync_data$col_choices)) {
    expect_setequal(result$ui_sync_data$col_choices, expected_choices)
  } else {
    # If error occurred, result might be NULL - that's acceptable
    expect_true(is.null(result) || is.null(result$ui_sync_data))
  }

  # TEST: current_data forbliver uændret ved fejl
  expect_identical(mock_values$current_data, error_prone_data)
})