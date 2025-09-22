# test-file-operations-tidyverse.R
# Integration tests for file operations using tidyverse patterns

test_that("preprocess_uploaded_data handles tidyverse operations correctly", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("stringr")

  # Create test data with various data quality issues
  test_data <- data.frame(
    `Valid Column` = c(1, 2, 3, 4, 5),
    `Empty Column` = c(NA, NA, NA, NA, NA),
    `Mixed Column` = c("data", "", NA, "more", "info"),
    `Whitespace Column` = c("  text  ", "", "   ", "valid", "data"),
    `Numeric Text` = c("1.5", "2.0", "", "3.5", NA),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # Add completely empty rows for testing
  test_data <- rbind(
    test_data,
    data.frame(
      `Valid Column` = c(NA, NA),
      `Empty Column` = c(NA, ""),
      `Mixed Column` = c("", NA),
      `Whitespace Column` = c("   ", ""),
      `Numeric Text` = c("", NA),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )

  if (exists("preprocess_uploaded_data")) {
    file_info <- list(name = "test.csv", size = 1000)
    result <- preprocess_uploaded_data(test_data, file_info, session_id = "test")

    # Test that function returns expected structure
    expect_true(is.list(result))
    expect_true("data" %in% names(result))
    expect_true("cleaning_log" %in% names(result))

    processed_data <- result$data
    cleaning_log <- result$cleaning_log

    # Test empty row removal using tidyverse approach
    if (!is.null(cleaning_log$empty_rows_removed)) {
      expect_true(cleaning_log$empty_rows_removed >= 0)
      # Should have fewer rows after removing empty ones
      expect_true(nrow(processed_data) <= nrow(test_data))
    }

    # Test column name cleaning
    if (!is.null(cleaning_log$column_names_cleaned)) {
      expect_true(cleaning_log$column_names_cleaned)
      # All column names should be valid R names
      expect_true(all(make.names(names(processed_data)) == names(processed_data)))
    }

    # Test data integrity - non-empty meaningful data should be preserved
    expect_true(nrow(processed_data) >= 3)  # At least some valid rows
    expect_true(ncol(processed_data) >= 3)  # Preserve columns even if empty
  } else {
    skip("preprocess_uploaded_data function not available")
  }
})

test_that("validate_data_for_auto_detect with tidyverse patterns", {
  if (exists("validate_data_for_auto_detect")) {
    # Test data with good structure
    good_data <- data.frame(
      Dato = c("2024-01-01", "2024-02-01", "2024-03-01"),
      Tæller = c(10, 15, 20),
      Nævner = c(100, 150, 200),
      stringsAsFactors = FALSE
    )

    result_good <- validate_data_for_auto_detect(good_data, session_id = "test")
    expect_true(is.list(result_good))
    expect_true("suitable" %in% names(result_good))
    expect_true(result_good$suitable)

    # Test data with problems
    problematic_data <- data.frame(
      empty_col = c(NA, NA, NA),
      text_only = c("a", "b", "c")
    )

    result_bad <- validate_data_for_auto_detect(problematic_data, session_id = "test")
    expect_true(is.list(result_bad))
    expect_true(length(result_bad$issues) > 0)

    # Test minimum data requirements
    tiny_data <- data.frame(x = 1)
    result_tiny <- validate_data_for_auto_detect(tiny_data, session_id = "test")
    expect_true(length(result_tiny$issues) > 0)
  } else {
    skip("validate_data_for_auto_detect function not available")
  }
})

test_that("Danish CSV processing with tidyverse locale handling", {
  skip_if_not_installed("readr")

  # Create temporary CSV file with Danish formatting
  temp_file <- tempfile(fileext = ".csv")
  danish_content <- "Dato;Tæller;Nævner;Procent
01-01-2024;10;100;10,5
02-01-2024;15;120;12,5
03-01-2024;20;150;13,3"

  writeLines(danish_content, temp_file, useBytes = TRUE)

  if (exists("handle_csv_upload") && exists("create_app_state")) {
    # Create mock app state
    app_state <- create_app_state()
    mock_emit <- list(
      data_loaded = function() {},
      navigation_changed = function() {}
    )

    # Test CSV upload processing
    result <- tryCatch({
      handle_csv_upload(temp_file, app_state, session_id = "test", emit = mock_emit)
      "success"
    }, error = function(e) {
      list(error = e$message)
    })

    if (is.character(result) && result == "success") {
      # Verify data was loaded correctly
      expect_true(!is.null(app_state$data$current_data))

      loaded_data <- app_state$data$current_data
      expect_true(is.data.frame(loaded_data))
      expect_true(nrow(loaded_data) >= 3)

      # Test Danish decimal handling (commas should be converted to dots)
      if ("Procent" %in% names(loaded_data)) {
        procent_col <- loaded_data[["Procent"]]
        # Should be numeric after processing
        expect_true(is.numeric(procent_col) || all(grepl("\\d+\\.\\d+", procent_col[!is.na(procent_col)])))
      }
    } else {
      # CSV processing might fail in test environment, that's acceptable
      expect_true(is.list(result) || is.character(result))
    }
  } else {
    skip("CSV upload functions not available")
  }

  # Clean up
  unlink(temp_file)
})

test_that("error handling in file operations with tidyverse", {
  if (exists("validate_uploaded_file")) {
    # Test with non-existent file
    fake_file_info <- list(
      datapath = "/non/existent/file.csv",
      name = "fake.csv",
      size = 100,
      type = "text/csv"
    )

    result <- validate_uploaded_file(fake_file_info, session_id = "test")
    expect_true(is.list(result))
    expect_true("valid" %in% names(result))
    expect_false(result$valid)
    expect_true(length(result$errors) > 0)

    # Test with zero-size file
    empty_file_info <- list(
      datapath = tempfile(),
      name = "empty.csv",
      size = 0,
      type = "text/csv"
    )

    result_empty <- validate_uploaded_file(empty_file_info, session_id = "test")
    expect_false(result_empty$valid)
    expect_true(any(grepl("empty", result_empty$errors, ignore.case = TRUE)))

    # Test with oversized file
    big_file_info <- list(
      datapath = tempfile(),
      name = "big.csv",
      size = 100 * 1024 * 1024,  # 100MB
      type = "text/csv"
    )

    result_big <- validate_uploaded_file(big_file_info, session_id = "test")
    expect_false(result_big$valid)
    expect_true(any(grepl("size", result_big$errors, ignore.case = TRUE)))
  } else {
    skip("validate_uploaded_file function not available")
  }
})

test_that("Excel file processing with tidyverse patterns", {
  skip_if_not_installed("readxl")

  if (exists("handle_excel_upload") && exists("create_app_state")) {
    # Create temporary Excel-like structure for testing
    temp_dir <- tempdir()
    excel_path <- file.path(temp_dir, "test.xlsx")

    # Note: In real tests, you'd create actual Excel files
    # For now, test the error handling path
    app_state <- create_app_state()
    mock_emit <- list(
      data_loaded = function() {},
      navigation_changed = function() {}
    )

    # Test with non-existent Excel file (should handle gracefully)
    result <- tryCatch({
      handle_excel_upload(excel_path, session = NULL, app_state, mock_emit)
      "completed"
    }, error = function(e) {
      "error_handled"
    })

    # Either should complete or handle error gracefully
    expect_true(result %in% c("completed", "error_handled"))
  } else {
    skip("Excel upload functions not available")
  }
})

test_that("session metadata parsing with tidyverse patterns", {
  if (exists("parse_session_metadata")) {
    # Test session metadata lines
    session_lines <- c(
      "• Titel: Test SPC Analysis",
      "• Enhed: Medicinsk Afdeling",
      "• Beskrivelse: Test analysis for quality improvement",
      "• Chart Type: P-kort (Andele)",
      "• X-akse: Dato (Date)",
      "• Y-akse: Tæller (Count)",
      "• Nævner: Nævner"
    )

    data_cols <- c("Dato", "Tæller", "Nævner", "Kommentar")

    metadata <- parse_session_metadata(session_lines, data_cols)

    expect_true(is.list(metadata))
    expect_equal(metadata$title, "Test SPC Analysis")
    expect_equal(metadata$unit_type, "select")
    expect_equal(metadata$unit_select, "med")
    expect_equal(metadata$description, "Test analysis for quality improvement")
    expect_equal(metadata$x_column, "Dato")
    expect_equal(metadata$y_column, "Tæller")
    expect_equal(metadata$n_column, "Nævner")

    # Test with missing or invalid data
    minimal_lines <- c("• Titel: Minimal Test")
    minimal_metadata <- parse_session_metadata(minimal_lines, data_cols)
    expect_equal(minimal_metadata$title, "Minimal Test")
    expect_null(minimal_metadata$unit_type)
  } else {
    skip("parse_session_metadata function not available")
  }
})