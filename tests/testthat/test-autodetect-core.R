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
  if (exists("appears_date")) {
    expect_true(appears_date(test_data$Dato))
    expect_false(appears_date(test_data$Tæller))
  }

  # Test appears_numeric function
  if (exists("appears_numeric")) {
    expect_true(appears_numeric(test_data$Tæller))
    expect_true(appears_numeric(test_data$Nævner))
    expect_false(appears_numeric(test_data$Kommentarer))
  }

  # Test find_numeric_columns
  if (exists("find_numeric_columns")) {
    numeric_cols <- find_numeric_columns(test_data)
    expect_true(is.character(numeric_cols))
    expect_true("Tæller" %in% numeric_cols)
    expect_true("Nævner" %in% numeric_cols)
  }
})

test_that("Auto-detect håndterer dansk dato format", {
  danish_dates <- c("01-01-2024", "15-02-2024", "31-12-2023")

  if (exists("appears_date")) {
    expect_true(appears_date(danish_dates))
  }

  if (exists("safe_date_parse")) {
    parsed_dates <- safe_date_parse(danish_dates[1])
    expect_true(!is.na(parsed_dates) || is.null(parsed_dates))
  }
})

test_that("Auto-detect håndterer edge cases", {
  edge_case_data <- data.frame(
    empty_col = rep("", 5),
    na_col = rep(NA, 5),
    mixed_col = c("1", "2", "text", "4", "5"),
    single_value = rep("constant", 5)
  )

  # Test at edge cases ikke crasher auto-detect
  if (exists("appears_numeric")) {
    expect_false(appears_numeric(edge_case_data$empty_col))
    expect_false(appears_numeric(edge_case_data$na_col))
    expect_false(appears_numeric(edge_case_data$mixed_col))
  }

  if (exists("appears_date")) {
    expect_false(appears_date(edge_case_data$empty_col))
    expect_false(appears_date(edge_case_data$single_value))
  }
})

test_that("Column mapping logic fungerer korrekt", {
  test_data <- data.frame(
    Skift = c(FALSE, FALSE, TRUE),
    Frys = c(FALSE, TRUE, FALSE),
    ObservationDate = c("01-01-2024", "02-01-2024", "03-01-2024"),
    Count = c(10, 15, 12),
    Total = c(100, 120, 110)
  )

  if (exists("detect_columns_with_cache")) {
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
  }
})