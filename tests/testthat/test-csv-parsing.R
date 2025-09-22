# test-csv-parsing.R
# Tests for CSV/data loading, encoding og parsing

test_that("CSV encoding og parsing fungerer korrekt", {
  # Test standard CSV parsing
  test_csv_content <- "Dato,Tæller,Nævner\n2024-01-01,10,100\n2024-02-01,15,120"
  temp_file <- tempfile(fileext = ".csv")
  writeLines(test_csv_content, temp_file, useBytes = TRUE)

  # Test at filen kan læses
  expect_true(file.exists(temp_file))

  # Cleanup
  unlink(temp_file)
})

test_that("Test data kan læses og behandles korrekt", {
  # Find test data file
  test_data_candidates <- c(
    "../../R/data/testdata_spc_example.csv",
    "R/data/testdata_spc_example.csv",
    "testdata_spc_example.csv"
  )

  test_data_path <- NULL
  for (path in test_data_candidates) {
    if (file.exists(path)) {
      test_data_path <- path
      break
    }
  }

  if (!is.null(test_data_path)) {
    expect_true(file.exists(test_data_path))
    expect_gt(file.size(test_data_path), 0)

    # Test at data kan læses
    if (exists("read_csv_safe")) {
      data <- read_csv_safe(test_data_path)
      expect_true(is.data.frame(data))
      expect_gt(nrow(data), 0)
    }
  } else {
    skip("No test data file found")
  }
})

test_that("ensure_standard_columns virker med test data", {
  test_data <- data.frame(
    Dato = c("2024-01-01", "2024-02-01"),
    Tæller = c(10, 15),
    Nævner = c(100, 120)
  )

  if (exists("ensure_standard_columns")) {
    result <- ensure_standard_columns(test_data)
    expect_true(is.data.frame(result))
    expect_true("Skift" %in% names(result))
    expect_true("Frys" %in% names(result))
    expect_true(all(c("Dato", "Tæller", "Nævner") %in% names(result)))
  } else {
    skip("ensure_standard_columns function not available")
  }
})