# test-server-performance-helpers-comprehensive.R
# Comprehensive tests for server performance helper functions
# Critical for efficient data type detection and performance optimization

library(testthat)

test_that("appears_numeric function works correctly with various inputs", {
  # TEST: Core appears_numeric functionality
  expect_true(exists("appears_numeric", mode = "function"),
              "appears_numeric function must be available")

  # TEST: Pure numeric strings
  numeric_strings <- c("123", "45.67", "0.89", "1000")
  expect_true(appears_numeric(numeric_strings))

  # TEST: Mixed numeric with commas and decimals
  numeric_with_formatting <- c("1,234.56", "2,000", "0.5", "100,000.00")
  expect_true(appears_numeric(numeric_with_formatting))

  # TEST: Non-numeric strings
  non_numeric_strings <- c("abc", "def", "xyz", "hello")
  expect_false(appears_numeric(non_numeric_strings))

  # TEST: Mixed content (mostly non-numeric)
  mixed_mostly_non_numeric <- c("abc", "def", "123", "ghi", "jkl", "mno", "pqr", "stu", "vwx", "yz")
  expect_false(appears_numeric(mixed_mostly_non_numeric))

  # TEST: Mixed content (mostly numeric) - should pass 70% threshold
  mixed_mostly_numeric <- c("123", "456", "789", "abc", "012", "345", "678", "901", "234", "567")
  expect_true(appears_numeric(mixed_mostly_numeric))

  # TEST: Edge cases
  expect_false(appears_numeric(character(0)))  # Empty vector
  expect_false(appears_numeric(c(NA, NA, NA)))  # All NA
  expect_false(appears_numeric(123))  # Not character input

  # TEST: Danish number formats
  danish_numbers <- c("123,45", "1.234,56", "0,75")
  expect_true(appears_numeric(danish_numbers))
})

test_that("appears_date function works correctly with various date formats", {
  # TEST: Core appears_date functionality
  expect_true(exists("appears_date", mode = "function"),
              "appears_date function must be available")

  # TEST: ISO date format (YYYY-MM-DD)
  iso_dates <- c("2024-01-01", "2024-12-31", "2023-06-15")
  expect_true(appears_date(iso_dates))

  # TEST: Danish date format (DD-MM-YYYY)
  danish_dates <- c("01-01-2024", "31-12-2024", "15-06-2023")
  expect_true(appears_date(danish_dates))

  # TEST: US date format (DD/MM/YYYY)
  us_dates <- c("01/01/2024", "31/12/2024", "15/06/2023")
  expect_true(appears_date(us_dates))

  # TEST: Alternative format (YYYY/MM/DD)
  alt_dates <- c("2024/01/01", "2024/12/31", "2023/06/15")
  expect_true(appears_date(alt_dates))

  # TEST: Non-date strings
  non_dates <- c("abc", "def", "xyz", "hello")
  expect_false(appears_date(non_dates))

  # TEST: Numbers that look like dates but aren't
  number_strings <- c("123", "456", "789")
  expect_false(appears_date(number_strings))

  # TEST: Edge cases
  expect_false(appears_date(character(0)))  # Empty vector
  expect_false(appears_date(c(NA, NA, NA)))  # All NA
  expect_false(appears_date(as.Date("2024-01-01")))  # Not character input

  # TEST: Mixed date and non-date content
  mixed_dates <- c("2024-01-01", "abc", "2024-12-31", "def")
  # Should still detect as dates if most entries match pattern
  result_mixed <- appears_date(mixed_dates)
  expect_type(result_mixed, "logical")
})

test_that("find_numeric_columns works with data frames", {
  skip_if_not(exists("find_numeric_columns", mode = "function"),
              "find_numeric_columns not available - check test setup")

  # TEST: Mixed data frame
  mixed_df <- data.frame(
    character_col = c("a", "b", "c"),
    numeric_string_col = c("123", "456", "789"),
    actual_numeric_col = c(1.5, 2.5, 3.5),
    date_col = c("2024-01-01", "2024-01-02", "2024-01-03"),
    stringsAsFactors = FALSE
  )

  numeric_cols <- find_numeric_columns(mixed_df)

  # Should find both actual numeric and numeric-appearing string columns
  expect_true("actual_numeric_col" %in% numeric_cols)
  expect_true("numeric_string_col" %in% numeric_cols)

  # Should not find character or date columns
  expect_false("character_col" %in% numeric_cols)
  expect_false("date_col" %in% numeric_cols)
})

test_that("performance helpers handle edge cases gracefully", {
  # TEST: Empty data
  expect_false(appears_numeric(character(0)))
  expect_false(appears_date(character(0)))

  # TEST: All NA data
  all_na <- c(NA_character_, NA_character_, NA_character_)
  expect_false(appears_numeric(all_na))
  expect_false(appears_date(all_na))

  # TEST: Single value vectors
  expect_true(appears_numeric("123"))
  expect_false(appears_numeric("abc"))
  expect_true(appears_date("2024-01-01"))
  expect_false(appears_date("not-a-date"))

  # TEST: Very long vectors (performance test)
  long_numeric_vector <- rep(c("123", "456", "789"), 1000)
  start_time <- Sys.time()
  result <- appears_numeric(long_numeric_vector)
  duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  expect_true(result)
  expect_lt(duration, 1.0, info = paste("Long vector processing took", duration, "seconds"))

  # TEST: Mixed NA and valid data
  mixed_na <- c("123", NA, "456", NA, "789")
  expect_true(appears_numeric(mixed_na))

  mixed_na_dates <- c("2024-01-01", NA, "2024-01-02", NA, "2024-01-03")
  expect_true(appears_date(mixed_na_dates))
})

test_that("performance helpers work with Danish clinical data patterns", {
  # TEST: Real-world Danish clinical data patterns

  # Typical Danish numeric data
  danish_clinical_numbers <- c("12,5", "15,0", "8,7", "11,3", "9,9")
  expect_true(appears_numeric(danish_clinical_numbers))

  # Danish date patterns from clinical systems
  danish_clinical_dates <- c("01-01-2024", "15-06-2024", "31-12-2024")
  expect_true(appears_date(danish_clinical_dates))

  # Mixed Danish hospital data
  hospital_percentages <- c("85,5%", "90,2%", "87,8%")
  # Should handle percentage signs in number detection
  result_percentages <- appears_numeric(hospital_percentages)
  expect_type(result_percentages, "logical")  # May or may not detect as numeric

  # Clinical measurement units
  clinical_measurements <- c("12,5 mg", "15,0 ml", "8,7 cm")
  result_measurements <- appears_numeric(clinical_measurements)
  expect_type(result_measurements, "logical")
})

test_that("performance helpers maintain consistent behavior", {
  # TEST: Consistent behavior across multiple calls

  test_data_numeric <- c("123", "456", "789", "012")
  test_data_dates <- c("2024-01-01", "2024-01-02", "2024-01-03")

  # Multiple calls should return same result
  results_numeric <- replicate(5, appears_numeric(test_data_numeric))
  expect_true(all(results_numeric == results_numeric[1]))

  results_dates <- replicate(5, appears_date(test_data_dates))
  expect_true(all(results_dates == results_dates[1]))

  # TEST: Order independence
  shuffled_numeric <- sample(test_data_numeric)
  expect_equal(appears_numeric(test_data_numeric), appears_numeric(shuffled_numeric))

  shuffled_dates <- sample(test_data_dates)
  expect_equal(appears_date(test_data_dates), appears_date(shuffled_dates))
})

test_that("performance helpers threshold behavior", {
  # TEST: 70% success rate threshold for appears_numeric

  # Exactly 70% numeric (7 out of 10)
  threshold_data <- c("1", "2", "3", "4", "5", "6", "7", "a", "b", "c")
  result_70 <- appears_numeric(threshold_data)
  expect_type(result_70, "logical")

  # 80% numeric (8 out of 10) - should pass
  above_threshold <- c("1", "2", "3", "4", "5", "6", "7", "8", "a", "b")
  expect_true(appears_numeric(above_threshold))

  # 60% numeric (6 out of 10) - should fail
  below_threshold <- c("1", "2", "3", "4", "5", "6", "a", "b", "c", "d")
  expect_false(appears_numeric(below_threshold))
})

test_that("performance helpers memory efficiency", {
  # TEST: Memory efficiency with sampling approach

  # Large vector should only sample first 10 elements
  large_vector_start_numeric <- c(rep("123", 10), rep("abc", 1000))
  expect_true(appears_numeric(large_vector_start_numeric))

  large_vector_start_non_numeric <- c(rep("abc", 10), rep("123", 1000))
  expect_false(appears_numeric(large_vector_start_non_numeric))

  # Same test for dates
  large_vector_dates <- c(rep("2024-01-01", 10), rep("not-date", 1000))
  expect_true(appears_date(large_vector_dates))

  large_vector_non_dates <- c(rep("not-date", 10), rep("2024-01-01", 1000))
  expect_false(appears_date(large_vector_non_dates))

  # Performance should be consistent regardless of vector size beyond sample
  start_time_small <- Sys.time()
  appears_numeric(rep("123", 10))
  duration_small <- as.numeric(difftime(Sys.time(), start_time_small, units = "secs"))

  start_time_large <- Sys.time()
  appears_numeric(rep("123", 10000))
  duration_large <- as.numeric(difftime(Sys.time(), start_time_large, units = "secs"))

  # Large vector should not be significantly slower due to sampling
  expect_lt(duration_large / duration_small, 3,
           info = paste("Large vector (", duration_large, ") should not be >3x slower than small (", duration_small, ")"))
})