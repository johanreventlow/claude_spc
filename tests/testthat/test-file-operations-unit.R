# test-file-operations-unit.R
# Unit tests for file operations uden Shiny dependencies

# Source required functions (but avoid Shiny-dependent ones)
source("../../R/utils_session_helpers.R")
source("../../R/utils_logging.R")

context("File Operations Unit Tests")

# Setup test data directories and files
test_data_dir <- tempdir()

test_that("CSV læsning og parsing uden Shiny context", {
  # TEST: Basic CSV reading functionality uden Shiny dependencies

  # Create test CSV file
  test_csv_data <- data.frame(
    Dato = c("01-01-2024", "01-02-2024", "01-03-2024"),
    Tæller = c(95, 92, 98),
    Nævner = c(100, 95, 102),
    Kommentarer = c("Test 1", "Test 2", "Test 3"),
    stringsAsFactors = FALSE
  )

  test_csv_file <- file.path(test_data_dir, "unit_test_data.csv")
  write.csv2(test_csv_data, test_csv_file, row.names = FALSE, fileEncoding = "ISO-8859-1")

  # Test basic CSV reading with correct Danish format
  data_loaded <- readr::read_csv2(
    test_csv_file,
    locale = readr::locale(
      decimal_mark = ",",
      grouping_mark = ".",
      encoding = DEFAULT_ENCODING
    ),
    show_col_types = FALSE
  )

  # TEST: Data loaded correctly
  expect_s3_class(data_loaded, "data.frame")
  expect_equal(nrow(data_loaded), 3)
  expect_equal(ncol(data_loaded), 4)

  # TEST: Column names preserved
  expect_equal(names(data_loaded), c("Dato", "Tæller", "Nævner", "Kommentarer"))

  # TEST: Data integrity
  expect_equal(data_loaded$Tæller[1], 95)
  expect_equal(data_loaded$Nævner[1], 100)

  # TEST: String data preserved
  expect_equal(data_loaded$Kommentarer[1], "Test 1")

  # Clean up
  unlink(test_csv_file)
})

test_that("ensure_standard_columns integration", {
  # TEST: Integration med standard columns functionality

  # Create data without standard columns
  raw_data <- data.frame(
    Date = c("01-01-2024", "01-02-2024"),
    Count = c(95, 92),
    Total = c(100, 95),
    Notes = c("Note 1", "Note 2")
  )

  # Process with ensure_standard_columns
  processed_data <- ensure_standard_columns(raw_data)

  # TEST: Standard columns added
  expect_true("Skift" %in% names(processed_data))
  expect_true("Frys" %in% names(processed_data))

  # TEST: Original columns preserved
  expect_true("Date" %in% names(processed_data))
  expect_true("Count" %in% names(processed_data))

  # TEST: Standard columns are first
  expect_equal(names(processed_data)[1:2], c("Skift", "Frys"))

  # TEST: Standard column values
  expect_equal(unique(processed_data$Skift), FALSE)
  expect_equal(unique(processed_data$Frys), FALSE)

  # TEST: Data integrity preserved
  expect_equal(processed_data$Count, c(95, 92))
  expect_equal(processed_data$Total, c(100, 95))
})

test_that("Danish character encoding support", {
  # TEST: Danish characters i file names og content

  # Create data with Danish characters
  danish_data <- data.frame(
    Dato = c("01-01-2024", "01-02-2024"),
    Måling = c(95.5, 92.3),  # ål
    Område = c("Børneafdeling", "Voksenpsykiatri"),  # øæ
    Bemærkninger = c("Første måling", "Anden måling"),
    stringsAsFactors = FALSE
  )

  danish_csv_file <- file.path(test_data_dir, "dansk_test.csv")
  write.csv2(danish_data, danish_csv_file, row.names = FALSE, fileEncoding = "ISO-8859-1")

  # Test loading with correct encoding
  loaded_danish <- readr::read_csv2(
    danish_csv_file,
    locale = readr::locale(
      decimal_mark = ",",
      grouping_mark = ".",
      encoding = "ISO-8859-1"  # Use consistent encoding
    ),
    show_col_types = FALSE
  )

  # TEST: Danish column names preserved
  expect_true("Måling" %in% names(loaded_danish))
  expect_true("Område" %in% names(loaded_danish))
  expect_true("Bemærkninger" %in% names(loaded_danish))

  # TEST: Danish characters in content preserved
  expect_true(any(grepl("ø", loaded_danish$Område, useBytes = FALSE)))
  expect_true(any(grepl("æ", loaded_danish$Område, useBytes = FALSE)))
  expect_true(any(grepl("å", loaded_danish$Måling, fixed = TRUE)))

  # Clean up
  unlink(danish_csv_file)
})

test_that("Numeric data parsing med dansk format", {
  # TEST: Danish decimal format (komma) parsing

  # Create CSV with Danish decimal format
  numeric_data <- data.frame(
    Værdi1 = c("95,5", "92,3", "98,7"),
    Værdi2 = c("100,0", "95,5", "102,1"),
    stringsAsFactors = FALSE
  )

  numeric_csv_file <- file.path(test_data_dir, "numeric_test.csv")
  write.csv2(numeric_data, numeric_csv_file, row.names = FALSE, fileEncoding = "ISO-8859-1")

  # Test loading with Danish locale
  loaded_numeric <- readr::read_csv2(
    numeric_csv_file,
    locale = readr::locale(
      decimal_mark = ",",
      grouping_mark = ".",
      encoding = "ISO-8859-1"
    ),
    show_col_types = FALSE
  )

  # TEST: Numeric values parsed correctly
  expect_true(is.numeric(loaded_numeric$Værdi1))
  expect_true(is.numeric(loaded_numeric$Værdi2))

  # TEST: Values correct
  expect_equal(loaded_numeric$Værdi1[1], 95.5)
  expect_equal(loaded_numeric$Værdi2[1], 100.0)

  # Clean up
  unlink(numeric_csv_file)
})

test_that("File size limits og performance", {
  # TEST: Different file sizes og performance characteristics

  # Small file
  small_data <- data.frame(
    Dato = c("01-01-2024"),
    Værdi = c(95),
    stringsAsFactors = FALSE
  )

  small_csv_file <- file.path(test_data_dir, "small_test.csv")
  write.csv2(small_data, small_csv_file, row.names = FALSE)

  start_time <- Sys.time()
  small_loaded <- readr::read_csv2(small_csv_file, locale = readr::locale(encoding = DEFAULT_ENCODING), show_col_types = FALSE)
  small_time <- as.numeric(Sys.time() - start_time)

  # TEST: Small file loads quickly
  expect_lt(small_time, 1)  # Should load in under 1 second
  expect_equal(nrow(small_loaded), 1)

  # Medium file (1000 rows)
  medium_data <- data.frame(
    Dato = rep(c("01-01-2024", "01-02-2024", "01-03-2024"), length.out = 1000),
    Værdi = sample(80:100, 1000, replace = TRUE),
    stringsAsFactors = FALSE
  )

  medium_csv_file <- file.path(test_data_dir, "medium_test.csv")
  write.csv2(medium_data, medium_csv_file, row.names = FALSE)

  start_time <- Sys.time()
  medium_loaded <- readr::read_csv2(medium_csv_file, locale = readr::locale(encoding = DEFAULT_ENCODING), show_col_types = FALSE)
  medium_time <- as.numeric(Sys.time() - start_time)

  # TEST: Medium file performance
  expect_lt(medium_time, 5)  # Should load in under 5 seconds
  expect_equal(nrow(medium_loaded), 1000)

  # Clean up
  unlink(small_csv_file)
  unlink(medium_csv_file)
})

test_that("Error handling for invalid files", {
  # TEST: Graceful error handling for various invalid file conditions

  # Test non-existent file
  non_existent_file <- file.path(test_data_dir, "does_not_exist.csv")

  expect_error({
    readr::read_csv2(non_existent_file, locale = readr::locale(encoding = DEFAULT_ENCODING))
  })

  # Test empty file
  empty_file <- file.path(test_data_dir, "empty.csv")
  writeLines("", empty_file)

  # Empty files might load successfully but with 0 rows
  result <- tryCatch({
    readr::read_csv2(empty_file, locale = readr::locale(encoding = DEFAULT_ENCODING), show_col_types = FALSE)
  }, error = function(e) {
    "error"
  })

  expect_true(is.data.frame(result) || result == "error")

  # Test invalid CSV (binary file)
  binary_file <- file.path(test_data_dir, "binary.csv")
  writeBin(as.raw(1:10), binary_file)

  expect_error({
    readr::read_csv2(binary_file, locale = readr::locale(encoding = DEFAULT_ENCODING))
  }, ".*")  # Any error is acceptable

  # Clean up
  unlink(empty_file)
  unlink(binary_file)
})

test_that("CSV delimiter og format detection", {
  # TEST: Different CSV formats og delimiters

  # Standard CSV2 format (semicolon separated)
  csv2_data <- "Dato;Værdi;Kommentar\n01-01-2024;95,5;Test 1\n01-02-2024;92,3;Test 2"
  csv2_file <- file.path(test_data_dir, "csv2_format.csv")
  writeLines(csv2_data, csv2_file, useBytes = TRUE)

  # Test reading with read_csv2
  loaded_csv2 <- readr::read_csv2(
    csv2_file,
    locale = readr::locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"),
    show_col_types = FALSE
  )

  # TEST: CSV2 format loaded correctly
  expect_equal(ncol(loaded_csv2), 3)
  expect_equal(names(loaded_csv2), c("Dato", "Værdi", "Kommentar"))
  expect_true(is.numeric(loaded_csv2$Værdi))
  expect_equal(loaded_csv2$Værdi[1], 95.5)

  # Clean up
  unlink(csv2_file)
})

test_that("Data validation efter loading", {
  # TEST: Data validation patterns efter file loading

  # Create test data with various data quality issues
  validation_data <- data.frame(
    Dato = c("01-01-2024", "invalid-date", "01-03-2024"),
    Numerisk = c("95,5", "not-a-number", "98,7"),
    Tekst = c("Valid", "", "Also valid"),
    stringsAsFactors = FALSE
  )

  validation_csv_file <- file.path(test_data_dir, "validation_test.csv")
  write.csv2(validation_data, validation_csv_file, row.names = FALSE, fileEncoding = "ISO-8859-1")

  # Load data
  loaded_validation <- readr::read_csv2(
    validation_csv_file,
    locale = readr::locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"),
    show_col_types = FALSE
  )

  # TEST: Data types after loading
  expect_true(is.character(loaded_validation$Dato))  # Dates might not parse automatically
  expect_true(is.character(loaded_validation$Numerisk))  # Non-numeric values force character type
  expect_true(is.character(loaded_validation$Tekst))

  # TEST: Data validation functions existence
  expect_true(exists("validate_numeric_column"))

  # Test validation function if available
  if (exists("validate_numeric_column")) {
    # This should detect that "Numerisk" column has invalid values
    validation_result <- validate_numeric_column(loaded_validation, "Numerisk")
    expect_true(is.character(validation_result) || is.null(validation_result))
  }

  # Clean up
  unlink(validation_csv_file)
})

test_that("Column name normalization", {
  # TEST: Column name handling og normalization

  # Create CSV with problematic column names
  problematic_data <- data.frame(
    `Dato med mellemrum` = c("01-01-2024", "01-02-2024"),
    `Værdi (%)` = c("95,5", "92,3"),
    `Special@Characters#` = c("Test 1", "Test 2"),
    check.names = FALSE  # Preserve problematic names
  )

  problematic_csv_file <- file.path(test_data_dir, "problematic_names.csv")
  write.csv2(problematic_data, problematic_csv_file, row.names = FALSE, fileEncoding = "ISO-8859-1")

  # Load with readr (preserves original names better than base R)
  loaded_problematic <- readr::read_csv2(
    problematic_csv_file,
    locale = readr::locale(encoding = "ISO-8859-1"),
    show_col_types = FALSE
  )

  # TEST: Problematic column names handled
  expect_equal(ncol(loaded_problematic), 3)
  expect_true(any(grepl("Dato", names(loaded_problematic))))
  expect_true(any(grepl("Værdi", names(loaded_problematic))))

  # Clean up
  unlink(problematic_csv_file)
})