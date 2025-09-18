# test-robust-date-detection.R
# Tests for Fase 2: Robust Date Detection with Danish formats
# Focus on comprehensive date format handling and lubridate integration

test_that("dansk datoformater detekteres korrekt med høj prioritet", {
  # SETUP: Test data med forskellige danske datoformater
  test_data <- data.frame(
    ID = 1:5,
    DanskDato1 = c("01-01-2024", "02-01-2024", "03-01-2024", "04-01-2024", "05-01-2024"),  # dd-mm-yyyy
    DanskDato2 = c("1/1/2024", "2/1/2024", "3/1/2024", "4/1/2024", "5/1/2024"),         # d/m/yyyy
    DanskDato3 = c("01.01.2024", "02.01.2024", "03.01.2024", "04.01.2024", "05.01.2024"), # dd.mm.yyyy
    DanskDato4 = c("1. jan 2024", "2. feb 2024", "3. mar 2024", "4. apr 2024", "5. maj 2024"), # Dansk månedsnavn
    IntlDato = c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-04", "2024-01-05"),   # yyyy-mm-dd
    Tæller = c(90, 85, 92, 88, 94),
    stringsAsFactors = FALSE
  )

  # TEST: detect_date_columns_robust should identify all date columns
  date_results <- detect_date_columns_robust(test_data)

  # Should identify all date columns
  expect_true("DanskDato1" %in% names(date_results))
  expect_true("DanskDato2" %in% names(date_results))
  expect_true("DanskDato3" %in% names(date_results))
  expect_true("IntlDato" %in% names(date_results))

  # Should NOT identify numeric columns as dates
  expect_false("Tæller" %in% names(date_results))
  expect_false("ID" %in% names(date_results))

  # Danish formats should have high success rates
  expect_true(date_results$DanskDato1$score >= 0.8)
  expect_true(date_results$DanskDato2$score >= 0.8)
  expect_true(date_results$DanskDato3$score >= 0.8)

  # Should suggest appropriate formats
  expect_true(!is.null(date_results$DanskDato1$suggested_format))
  expect_true(grepl("danish|dmy", date_results$DanskDato1$reason, ignore.case = TRUE))
})

test_that("danske månedsnavn og specialformater håndteres", {
  # SETUP: Test data med danske månedsnavn og komplekse formater
  test_data <- data.frame(
    DanskMåned1 = c("1. januar 2024", "15. februar 2024", "31. december 2023"),
    DanskMåned2 = c("jan 2024", "feb 2024", "mar 2024"),
    DanskMåned3 = c("Januar 2024", "Februar 2024", "Marts 2024"),
    Mellemrum = c("1 1 2024", "2 2 2024", "3 3 2024"),  # Space-separated
    MixedFormat = c("01-jan-2024", "02-feb-2024", "03-mar-2024"),
    stringsAsFactors = FALSE
  )

  # TEST: Should handle Danish month names and complex formats
  date_results <- detect_date_columns_robust(test_data)

  # Should identify some of these challenging formats
  expect_true(length(date_results) > 0)

  # At least one format should be recognized with reasonable confidence
  if (length(date_results) > 0) {
    max_score <- max(sapply(date_results, function(x) x$score))
    expect_true(max_score >= 0.6)  # Lower threshold for complex formats
  }
})

test_that("internationale formater fungerer som fallback", {
  # SETUP: Test data med internationale formater
  test_data <- data.frame(
    ISO8601 = c("2024-01-01", "2024-02-01", "2024-03-01"),     # yyyy-mm-dd
    American = c("01/01/2024", "02/01/2024", "03/01/2024"),    # mm/dd/yyyy (ambiguous)
    European = c("01-01-2024", "02-01-2024", "03-01-2024"),   # dd-mm-yyyy
    Compact = c("20240101", "20240201", "20240301"),          # yyyymmdd
    stringsAsFactors = FALSE
  )

  # TEST: International formats should be detected
  date_results <- detect_date_columns_robust(test_data)

  # Should identify clear international formats
  expect_true("ISO8601" %in% names(date_results))
  expect_true(date_results$ISO8601$score >= 0.9)  # Very high confidence for ISO format

  # Should suggest appropriate international formats
  expect_true(grepl("ymd|international", date_results$ISO8601$reason, ignore.case = TRUE))
})

test_that("edge cases og error conditions håndteres gracefully", {
  # SETUP: Various edge cases

  # Empty data
  empty_data <- data.frame()
  expect_equal(length(detect_date_columns_robust(empty_data)), 0)

  # NULL data
  expect_equal(length(detect_date_columns_robust(NULL)), 0)

  # Data with no date columns
  no_dates <- data.frame(
    Number = 1:3,
    Text = c("Hello", "World", "Test"),
    Logic = c(TRUE, FALSE, TRUE)
  )
  expect_equal(length(detect_date_columns_robust(no_dates)), 0)

  # Data with mostly invalid dates (should fail threshold)
  bad_dates <- data.frame(
    BadDate = c("invalid", "not-a-date", "32/13/2024", "abc", "123")
  )
  bad_results <- detect_date_columns_robust(bad_dates)
  expect_equal(length(bad_results), 0)  # Should not pass threshold

  # Mixed valid/invalid dates (should pass if enough valid)
  mixed_dates <- data.frame(
    MixedDate = c("01-01-2024", "02-01-2024", "invalid", "03-01-2024", "04-01-2024")
  )
  mixed_results <- detect_date_columns_robust(mixed_dates)

  # Should pass threshold if >80% valid (4/5 = 80%)
  if (length(mixed_results) > 0) {
    expect_true(mixed_results$MixedDate$score >= 0.8)
  }
})

test_that("lubridate format testing fungerer korrekt", {
  # SETUP: Specific format tests
  sample_dmy <- c("01-01-2024", "15-03-2024", "31-12-2023")
  sample_ymd <- c("2024-01-01", "2024-03-15", "2023-12-31")
  sample_mdy <- c("01/01/2024", "03/15/2024", "12/31/2023")

  # TEST: Format-specific parsing
  dmy_score <- test_date_parsing_format(sample_dmy, "dmy")
  ymd_score <- test_date_parsing_format(sample_ymd, "ymd")
  mdy_score <- test_date_parsing_format(sample_mdy, "mdy")

  # Should have high success rates for matching formats
  expect_true(dmy_score >= 0.9)
  expect_true(ymd_score >= 0.9)
  expect_true(mdy_score >= 0.9)

  # Cross-format testing (should have lower scores)
  dmy_as_ymd <- test_date_parsing_format(sample_dmy, "ymd")
  expect_true(dmy_as_ymd < 0.5)  # Should fail or have low success
})

test_that("performance med store datasæt er acceptabel", {
  # SETUP: Large dataset simulation
  large_n <- 1000
  large_data <- data.frame(
    ID = 1:large_n,
    Date1 = rep(c("01-01-2024", "02-01-2024", "03-01-2024"), length.out = large_n),
    Date2 = rep(c("2024-01-01", "2024-02-01", "2024-03-01"), length.out = large_n),
    Number = rnorm(large_n),
    Text = rep(c("A", "B", "C"), length.out = large_n),
    stringsAsFactors = FALSE
  )

  # TEST: Performance timing
  start_time <- Sys.time()
  results <- detect_date_columns_robust(large_data)
  end_time <- Sys.time()

  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Should complete within reasonable time (adjust threshold as needed)
  expect_true(elapsed < 5.0)  # Should complete within 5 seconds

  # Should still identify date columns correctly
  expect_true("Date1" %in% names(results))
  expect_true("Date2" %in% names(results))
  expect_false("Number" %in% names(results))
})

test_that("find_best_format function works correctly", {
  # SETUP: Test data with clear format patterns
  danish_data <- c("01-01-2024", "15-03-2024", "31-12-2023")
  iso_data <- c("2024-01-01", "2024-03-15", "2023-12-31")

  # TEST: Should identify best formats
  danish_formats <- c("dmy", "dmY", "d/m/Y", "d-m-Y", "d.m.Y")
  intl_formats <- c("ymd", "mdy", "Ymd", "mdY")

  # For Danish data, should prefer Danish format
  danish_result <- find_best_format(danish_data, danish_formats, intl_formats)
  expect_true(danish_result %in% danish_formats)

  # For ISO data, should prefer international format
  iso_result <- find_best_format(iso_data, danish_formats, intl_formats)
  expect_true(iso_result %in% intl_formats)
})

test_that("integration med unified autodetect engine fungerer", {
  # SETUP: Create app_state and test data
  app_state <- new.env()
  app_state$autodetect <- list(frozen_until_next_trigger = FALSE)
  app_state$columns <- list()

  emit <- list(
    autodetection_completed = function() { "completed" }
  )

  # Test data with Danish date formats
  test_data <- data.frame(
    Dato = c("01-01-2024", "02-01-2024", "03-01-2024"),
    Tæller = c(90, 85, 92),
    Nævner = c(100, 95, 100),
    stringsAsFactors = FALSE
  )

  # TEST: Full autodetect engine with robust date detection
  results <- autodetect_engine(
    data = test_data,
    trigger_type = "file_upload",
    app_state = app_state,
    emit = emit
  )

  # Should have detected date column correctly
  expect_equal(results$x_col, "Dato")
  expect_equal(results$y_col, "Tæller")
  expect_equal(results$n_col, "Nævner")

  # Should have updated app_state
  expect_true(app_state$autodetect$frozen_until_next_trigger)
  expect_equal(app_state$autodetect$last_run$trigger, "file_upload")
})