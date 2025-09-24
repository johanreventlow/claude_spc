# test-autodetect-core.R
# Focused tests for auto-detection core functionality

test_that("Auto-detect identificerer kolonnetyper korrekt", {
  test_data <- data.frame(
    Skift = c(FALSE, FALSE, TRUE, FALSE),
    Frys = c(FALSE, TRUE, FALSE, FALSE),
    Dato = c("01-01-2024", "02-01-2024", "03-01-2024", "04-01-2024"),
    Tæller = c(10, 15, 12, 18),
    Nævner = c(100, 120, 110, 130),
    Kommentarer = c("Start", "Problem", "Fix", "End")
  )

  # Test appears_date function
  # Strong assertion that fails the test if function is missing
  expect_true(exists("appears_date", mode = "function"),
              "appears_date must be available for this test")

  expect_true(appears_date(test_data$Dato))
  expect_false(appears_date(test_data$Tæller))

  # Test appears_numeric function
  # Strong assertion that fails the test if function is missing
  expect_true(exists("appears_numeric", mode = "function"),
              "appears_numeric must be available for this test")

  expect_true(appears_numeric(test_data$Tæller))
  expect_true(appears_numeric(test_data$Nævner))
  expect_false(appears_numeric(test_data$Kommentarer))

  # Test find_numeric_columns
  # Strong assertion that fails the test if function is missing
  expect_true(exists("find_numeric_columns", mode = "function"),
              "find_numeric_columns must be available for this test")

  numeric_cols <- find_numeric_columns(test_data)
  expect_true(is.character(numeric_cols))
  expect_true("Tæller" %in% numeric_cols)
  expect_true("Nævner" %in% numeric_cols)
})

test_that("Auto-detect håndterer dansk dato format", {
  danish_dates <- c("01-01-2024", "15-02-2024", "31-12-2023")

  # Strong assertion that fails the test if function is missing
  expect_true(exists("appears_date", mode = "function"),
              "appears_date must be available for this test")

  expect_true(appears_date(danish_dates))

  skip_if_not(exists("safe_date_parse", mode = "function"),
              "safe_date_parse not available - check test setup")

  parsed_dates <- safe_date_parse(danish_dates[1])
  expect_true(!is.na(parsed_dates) || is.null(parsed_dates))
})

test_that("Auto-detect håndterer edge cases", {
  edge_case_data <- data.frame(
    empty_col = rep("", 5),
    na_col = rep(NA, 5),
    mixed_col = c("1", "2", "text", "4", "5"),
    single_value = rep("constant", 5)
  )

  # Test at edge cases ikke crasher auto-detect
  # Strong assertion that fails the test if function is missing
  expect_true(exists("appears_numeric", mode = "function"),
              "appears_numeric must be available for this test")

  expect_false(appears_numeric(edge_case_data$empty_col))
  expect_false(appears_numeric(edge_case_data$na_col))
  expect_false(appears_numeric(edge_case_data$mixed_col))

  # Strong assertion that fails the test if function is missing
  expect_true(exists("appears_date", mode = "function"),
              "appears_date must be available for this test")

  expect_false(appears_date(edge_case_data$empty_col))
  expect_false(appears_date(edge_case_data$single_value))
})

test_that("Column mapping logic fungerer korrekt", {
  test_data <- data.frame(
    Skift = c(FALSE, FALSE, TRUE),
    Frys = c(FALSE, TRUE, FALSE),
    ObservationDate = c("01-01-2024", "02-01-2024", "03-01-2024"),
    Count = c(10, 15, 12),
    Total = c(100, 120, 110)
  )

  # Strong assertion that fails the test if function is missing
  expect_true(exists("detect_columns_with_cache", mode = "function"),
              "detect_columns_with_cache must be available for this test")

  # Test 1: Med app_state parameter (eksisterende funktionalitet)
  app_state <- create_test_app_state()
  detected_with_state <- detect_columns_with_cache(test_data, app_state)
  expect_true(is.list(detected_with_state))

  # Test 2: Uden app_state parameter (ny funktionalitet)
  detected_without_state <- detect_columns_with_cache(test_data)
  expect_true(is.list(detected_without_state))

  # Test at begge resultater er sammenlignelige
  expect_true(identical(names(detected_with_state), names(detected_without_state)))

  # Test at vi får relevante mapping forslag
  if ("x_col" %in% names(detected_without_state)) {
    expect_true(is.character(detected_without_state$x_col))
  }
})