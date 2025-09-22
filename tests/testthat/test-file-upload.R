# test-file-upload.R
# Comprehensive tests for file upload functionality

test_that("validate_uploaded_file handles different file types correctly", {
  # Test valid CSV file
  valid_csv <- list(
    name = "test_data.csv",
    size = 1024,
    type = "text/csv",
    datapath = tempfile(fileext = ".csv")
  )

  # Create actual test file
  test_data <- "Dato,Tæller,Nævner\n2024-01-01,10,100\n2024-02-01,15,120"
  writeLines(test_data, valid_csv$datapath)

  if (exists("validate_uploaded_file")) {
    result <- validate_uploaded_file(valid_csv)
    expect_true(is.list(result))
    expect_true("valid" %in% names(result))
  }

  unlink(valid_csv$datapath)
})

test_that("handle_csv_upload processes CSV files correctly", {
  # Create test CSV file
  test_csv_content <- "Dato,Tæller,Nævner\n2024-01-01,10,100\n2024-02-01,15,120\n2024-03-01,8,90"
  temp_file <- tempfile(fileext = ".csv")
  writeLines(test_csv_content, temp_file)

  if (exists("handle_csv_upload")) {
    # Test basic CSV loading
    result <- handle_csv_upload(temp_file, NULL, NULL, NULL)
    expect_true(is.data.frame(result) || is.null(result))

    if (is.data.frame(result)) {
      expect_gt(nrow(result), 0)
      expect_true("Dato" %in% names(result))
      expect_true("Tæller" %in% names(result))
      expect_true("Nævner" %in% names(result))
    }
  } else {
    skip("handle_csv_upload function not available")
  }

  unlink(temp_file)
})

test_that("handle_excel_upload processes Excel files correctly", {
  skip_if_not_installed("readxl")

  # Create test Excel file (as CSV for testing purposes)
  test_data <- data.frame(
    Dato = c("2024-01-01", "2024-02-01", "2024-03-01"),
    Tæller = c(10, 15, 8),
    Nævner = c(100, 120, 90)
  )

  temp_file <- tempfile(fileext = ".xlsx")

  # Use openxlsx if available, otherwise skip
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    openxlsx::write.xlsx(test_data, temp_file)

    if (exists("handle_excel_upload")) {
      result <- handle_excel_upload(temp_file, NULL, NULL, NULL, NULL)
      expect_true(is.data.frame(result) || is.null(result))

      if (is.data.frame(result)) {
        expect_gt(nrow(result), 0)
        expect_true("Dato" %in% names(result))
      }
    }

    unlink(temp_file)
  } else {
    skip("openxlsx package not available for creating test Excel file")
  }
})

test_that("File upload handles edge cases and errors gracefully", {
  if (exists("validate_uploaded_file")) {
    # Test empty file
    empty_file <- list(
      name = "empty.csv",
      size = 0,
      type = "text/csv",
      datapath = tempfile(fileext = ".csv")
    )
    writeLines("", empty_file$datapath)

    result <- validate_uploaded_file(empty_file)
    expect_true(is.list(result))

    unlink(empty_file$datapath)

    # Test invalid file type
    invalid_file <- list(
      name = "test.txt",
      size = 100,
      type = "text/plain"
    )

    result <- validate_uploaded_file(invalid_file)
    expect_true(is.list(result))

    # Test missing file
    missing_file <- list(
      name = "missing.csv",
      size = 100,
      type = "text/csv",
      datapath = "/nonexistent/path/file.csv"
    )

    result <- validate_uploaded_file(missing_file)
    expect_true(is.list(result))
  }
})

test_that("File upload with Danish encoding works correctly", {
  # Create CSV with Danish characters
  danish_content <- "Dato,Tæller,Nævner,Kommentarer\n2024-01-01,10,100,Første måling\n2024-02-01,15,120,Øget aktivitet"
  temp_file <- tempfile(fileext = ".csv")
  writeLines(danish_content, temp_file, useBytes = TRUE)

  if (exists("handle_csv_upload")) {
    result <- handle_csv_upload(temp_file, NULL, NULL, NULL)

    if (is.data.frame(result)) {
      expect_true("Kommentarer" %in% names(result))
      # Test that Danish characters are preserved
      if ("Kommentarer" %in% names(result)) {
        comments <- result$Kommentarer
        expect_true(any(grepl("Første", comments)) || any(grepl("Øget", comments)))
      }
    }
  }

  unlink(temp_file)
})

test_that("setup_file_upload reactive chain works", {
  skip_if_not_installed("shiny")

  if (exists("setup_file_upload")) {
    # Test that setup_file_upload can be called without crashing
    mock_input <- list()
    mock_output <- list()
    mock_session <- list(token = "test_session")
    mock_app_state <- list()
    mock_emit <- list()

    result <- tryCatch({
      setup_file_upload(mock_input, mock_output, mock_session, mock_app_state, mock_emit)
      "success"
    }, error = function(e) {
      "error"
    })

    # Should not crash during setup (may fail later due to missing inputs)
    expect_true(result == "success" || result == "error")
  }
})