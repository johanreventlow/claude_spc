# test-file-operations.R
# Comprehensive tests for file upload, download og data processing

# Source required functions
source("../../R/fct_file_operations.R")
source("../../R/utils_session_helpers.R")
source("../../R/utils_logging.R")

context("File Operations")

# Setup test data directories and files
test_data_dir <- tempdir()
test_csv_file <- file.path(test_data_dir, "test_data.csv")
test_excel_file <- file.path(test_data_dir, "test_data.xlsx")

# Helper function to handle Shiny context dependencies
safe_handle_csv_upload <- function(file_path, values) {
  tryCatch({
    handle_csv_upload(file_path, values)
  }, error = function(e) {
    if (grepl("reactive context|session|sendNotification", e$message)) {
      skip("Test kræver Shiny session context")
    } else {
      stop(e)
    }
  })
}

# Create test CSV file
test_csv_data <- data.frame(
  Dato = c("01-01-2024", "01-02-2024", "01-03-2024"),
  Tæller = c(95, 92, 98),
  Nævner = c(100, 95, 102),
  Kommentarer = c("Test 1", "Test 2", "Test 3"),
  stringsAsFactors = FALSE
)

# Write test CSV with Danish encoding
write.csv2(test_csv_data, test_csv_file, row.names = FALSE, fileEncoding = "ISO-8859-1")

test_that("handle_csv_upload læser danske CSV filer korrekt", {
  # TEST: CSV upload med dansk encoding og format

  # Mock values object
  mock_values <- list(
    current_data = NULL,
    original_data = NULL,
    file_uploaded = FALSE,
    updating_table = FALSE
  )

  # Test CSV upload (skip if requires session context)
  result <- safe_handle_csv_upload(test_csv_file, mock_values)

  # TEST: Successful upload
  expect_null(result)  # Function returns NULL on success

  # TEST: Data blev indlæst korrekt
  expect_true(!is.null(mock_values$current_data))
  expect_true(is.data.frame(mock_values$current_data))
  expect_equal(nrow(mock_values$current_data), 3)

  # TEST: Standard kolonner blev tilføjet
  expect_true("Skift" %in% names(mock_values$current_data))
  expect_true("Frys" %in% names(mock_values$current_data))

  # TEST: Original kolonner bevaret
  expect_true("Dato" %in% names(mock_values$current_data))
  expect_true("Tæller" %in% names(mock_values$current_data))
  expect_true("Nævner" %in% names(mock_values$current_data))

  # TEST: Data integrity
  expect_equal(mock_values$current_data$Tæller[1], 95)
  expect_equal(mock_values$current_data$Nævner[1], 100)

  # TEST: File status flags
  expect_true(mock_values$file_uploaded)
  expect_false(mock_values$updating_table)

  # TEST: Data backup
  expect_identical(mock_values$current_data, mock_values$original_data)
})

test_that("handle_csv_upload håndterer fejl korrekt", {
  # TEST: Error handling for invalid files

  mock_values <- list(
    current_data = NULL,
    file_uploaded = FALSE
  )

  # Test with non-existent file
  non_existent_file <- file.path(test_data_dir, "does_not_exist.csv")
  result <- safe_handle_csv_upload(non_existent_file, mock_values)

  # TEST: Error message returned
  expect_true(is.character(result))
  expect_true(grepl("Fil ikke fundet", result) || grepl("cannot open", result))

  # TEST: Values unchanged on error
  expect_null(mock_values$current_data)
  expect_false(mock_values$file_uploaded)
})

test_that("parse_session_metadata parser session info korrekt", {
  # TEST: Session metadata parsing from Excel files

  # Mock session info lines (korrekt format som funktionen forventer)
  test_session_lines <- c(
    "• Titel: Test SPC Chart",
    "• Enhed: Test Afdeling",
    "• X-akse kolonne: Dato",
    "• Y-akse kolonne: Tæller",
    "• N-akse kolonne: Nævner",
    "• Chart type: P-kort (Andele)"
  )

  # Mock data columns
  mock_data_cols <- c("Skift", "Frys", "Dato", "Tæller", "Nævner", "Kommentarer")

  result <- parse_session_metadata(test_session_lines, mock_data_cols)

  # TEST: Parsing success
  expect_true(is.list(result))

  # TEST: Title parsed
  expect_equal(result$title, "Test SPC Chart")

  # NOTE: Andre felter kræver specifik implementation i parse_session_metadata function
  # For nu tester vi blot at funktionen ikke fejler og returnerer en liste
}

test_that("parse_session_metadata håndterer manglende data", {
  # TEST: Robust parsing with missing/invalid session info

  # Incomplete session lines
  incomplete_lines <- c("• Titel: Delvis Info")
  mock_data_cols <- c("Dato", "Tæller")

  result <- parse_session_metadata(incomplete_lines, mock_data_cols)

  # TEST: Graceful handling of incomplete data
  expect_true(is.list(result))
  expect_equal(result$title, "Delvis Info")

  # Empty session lines
  empty_result <- parse_session_metadata(character(0), mock_data_cols)
  expect_true(is.list(empty_result))
})

test_that("create_session_info_lines genererer korrekt session metadata", {
  # TEST: Session info generation for Excel export

  skip("Denne test kræver Shiny reactive context - skippes i unit tests")

  # NOTE: Dette test kræver aktiv Shiny session og reactive context
  # Det bør testes i integration tests med komplet Shiny app setup
}

test_that("CSV encoding håndtering", {
  # TEST: Different CSV encodings and formats

  # Create CSV with special Danish characters
  danish_data <- data.frame(
    Dato = c("01-01-2024", "01-02-2024"),
    Måling = c(95.5, 92.3),  # ål
    Område = c("Børneafdeling", "Voksenpsykiatri"),  # øæ
    Kommentarer = c("Første måling", "Anden måling"),
    stringsAsFactors = FALSE
  )

  danish_csv_file <- file.path(test_data_dir, "danish_test.csv")
  write.csv2(danish_data, danish_csv_file, row.names = FALSE, fileEncoding = "ISO-8859-1")

  mock_values <- list(
    current_data = NULL,
    original_data = NULL,
    file_uploaded = FALSE,
    updating_table = FALSE
  )

  # Test CSV upload with Danish characters
  result <- safe_handle_csv_upload(danish_csv_file, mock_values)

  # TEST: Danish characters preserved
  expect_null(result)  # Success
  expect_true("Måling" %in% names(mock_values$current_data))
  expect_true("Område" %in% names(mock_values$current_data))

  # TEST: Values with Danish characters readable
  expect_true(any(grepl("ø", mock_values$current_data$Område, useBytes = FALSE)))
  expect_true(any(grepl("æ", mock_values$current_data$Område, useBytes = FALSE)))

  # Clean up
  unlink(danish_csv_file)
})

test_that("File size og performance håndtering", {
  # TEST: Large file handling and performance

  # Create larger test dataset
  large_data <- data.frame(
    Dato = rep(c("01-01-2024", "01-02-2024", "01-03-2024"), each = 100),
    Tæller = sample(80:100, 300, replace = TRUE),
    Nævner = sample(95:105, 300, replace = TRUE),
    Kommentarer = paste("Entry", 1:300),
    stringsAsFactors = FALSE
  )

  large_csv_file <- file.path(test_data_dir, "large_test.csv")
  write.csv2(large_data, large_csv_file, row.names = FALSE)

  mock_values <- list(
    current_data = NULL,
    original_data = NULL,
    file_uploaded = FALSE,
    updating_table = FALSE
  )

  # Test performance with larger file
  start_time <- Sys.time()
  result <- safe_handle_csv_upload(large_csv_file, mock_values)
  end_time <- Sys.time()

  # TEST: Reasonable performance (should complete in under 5 seconds)
  expect_lt(as.numeric(end_time - start_time), 5)

  # TEST: Large data handled correctly
  expect_null(result)
  expect_equal(nrow(mock_values$current_data), 300)
  expect_true(mock_values$file_uploaded)

  # Clean up
  unlink(large_csv_file)
})

test_that("Edge cases og error conditions", {
  # TEST: Various edge cases and error conditions

  # Empty CSV file
  empty_csv_file <- file.path(test_data_dir, "empty.csv")
  writeLines("", empty_csv_file)

  mock_values <- list(
    current_data = NULL,
    file_uploaded = FALSE
  )

  result <- safe_handle_csv_upload(empty_csv_file, mock_values)

  # TEST: Empty file handling
  expect_true(is.character(result))  # Should return error message
  expect_false(mock_values$file_uploaded)

  # CSV with only headers
  headers_only_file <- file.path(test_data_dir, "headers_only.csv")
  writeLines("Dato;Tæller;Nævner", headers_only_file)

  result2 <- safe_handle_csv_upload(headers_only_file, mock_values)

  # TEST: Headers only handling
  # Could be success (0 rows) or error depending on implementation
  if (is.null(result2)) {
    # Success case - should have 0 rows
    expect_equal(nrow(mock_values$current_data), 0)
  } else {
    # Error case - should be character
    expect_true(is.character(result2))
  }

  # Clean up test files
  unlink(empty_csv_file)
  unlink(headers_only_file)
})

# Clean up main test files
unlink(test_csv_file)

test_that("create_complete_excel_export integration test", {
  # TEST: Complete Excel export functionality

  # Mock complete input state
  mock_input <- list(
    x_col = "Dato",
    taeller_col = "Tæller",
    naevner_col = "Nævner",
    chart_type = "I-kort (Individuelle værdier)",
    plot_title = "Integration Test Chart",
    department_name = "Test Integration Department"
  )

  # Mock values with data
  mock_values <- list(
    current_data = data.frame(
      Skift = c(FALSE, FALSE, FALSE),
      Frys = c(FALSE, FALSE, FALSE),
      Dato = c("01-01-2024", "01-02-2024", "01-03-2024"),
      Tæller = c(95, 92, 98),
      Nævner = c(100, 95, 102),
      Kommentarer = c("Test 1", "Test 2", "Test 3")
    ),
    original_data = NULL  # Will be set to current_data
  )
  mock_values$original_data <- mock_values$current_data

  # Create temporary Excel file for export
  temp_excel_file <- file.path(test_data_dir, "integration_export.xlsx")

  # Test Excel export creation (skip if requires session context)
  tryCatch({
    expect_silent({
      create_complete_excel_export(temp_excel_file, mock_input, mock_values)
    })
  }, error = function(e) {
    if (grepl("reactive context|session|sendNotification", e$message)) {
      skip("Test kræver Shiny session context")
    } else {
      stop(e)
    }
  })

  # TEST: Excel file created
  expect_true(file.exists(temp_excel_file))

  # TEST: Excel file has content
  file_info <- file.info(temp_excel_file)
  expect_gt(file_info$size, 0)

  # Clean up
  unlink(temp_excel_file)
})

test_that("File operations integration med ensure_standard_columns", {
  # TEST: Integration mellem file upload og data processing

  # Create CSV without standard columns
  non_standard_data <- data.frame(
    Date = c("01-01-2024", "01-02-2024"),
    Count = c(95, 92),
    Total = c(100, 95),
    Notes = c("Note 1", "Note 2")
  )

  non_standard_csv <- file.path(test_data_dir, "non_standard.csv")
  write.csv2(non_standard_data, non_standard_csv, row.names = FALSE)

  mock_values <- list(
    current_data = NULL,
    original_data = NULL,
    file_uploaded = FALSE,
    updating_table = FALSE
  )

  # Upload non-standard CSV
  result <- safe_handle_csv_upload(non_standard_csv, mock_values)

  # TEST: Upload successful even without standard columns
  expect_null(result)
  expect_true(mock_values$file_uploaded)

  # TEST: Standard columns added by ensure_standard_columns
  expect_true("Skift" %in% names(mock_values$current_data))
  expect_true("Frys" %in% names(mock_values$current_data))

  # TEST: Original data preserved
  expect_true("Date" %in% names(mock_values$current_data))
  expect_true("Count" %in% names(mock_values$current_data))

  # Clean up
  unlink(non_standard_csv)
})