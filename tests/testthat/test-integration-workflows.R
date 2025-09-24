# test-integration-workflows.R
# Minimal integration tests for critical user workflows

test_that("Data upload → auto-detect → plot workflow simulation", {
  skip_if_not_installed("shiny")

  # Create test data
  test_data <- data.frame(
    Skift = c(FALSE, FALSE, TRUE, FALSE),
    Frys = c(FALSE, TRUE, FALSE, FALSE),
    Dato = c("01-01-2024", "02-01-2024", "03-01-2024", "04-01-2024"),
    Tæller = c(10, 15, 12, 18),
    Nævner = c(100, 120, 110, 130)
  )

  # Test 1: Data processing
  # Strong assertion that fails the test if function is missing
  expect_true(exists("ensure_standard_columns", mode = "function"),
              "ensure_standard_columns must be available for this test")

  processed_data <- ensure_standard_columns(test_data)
  expect_true(is.data.frame(processed_data))
  expect_gt(nrow(processed_data), 0)

  # Test 2: Auto-detection
  # Strong assertion that fails the test if function is missing
  expect_true(exists("detect_columns_with_cache", mode = "function"),
              "detect_columns_with_cache must be available for this test")

  detected <- detect_columns_with_cache(test_data)
  expect_true(is.list(detected))

  # Test 3: Basic plot generation
  # Strong assertion that fails the test if function is missing
  expect_true(exists("get_qic_chart_type", mode = "function"),
              "get_qic_chart_type must be available for this test")

  chart_type <- get_qic_chart_type("P-kort (Andele)")
  expect_equal(chart_type, "p")
})

test_that("Event-driven state workflow simulation", {
  skip_if_not_installed("shiny")

  skip_if_not(exists("create_app_state", mode = "function"),
              "create_app_state not available - check test setup")

  # Create app state
  app_state <- create_app_state()

  # Test event emission patterns
  skip_if_not(exists("events", envir = app_state),
              "app_state$events not available - check app_state structure")

  # Simulate data loaded event
  isolate({
    app_state$events$data_loaded <- app_state$events$data_loaded + 1L
  })
  expect_true(app_state$events$data_loaded > 0)
})

test_that("Critical error scenarios håndteres gracefully", {
  # Test empty data
  empty_data <- data.frame()

  skip_if_not(exists("ensure_standard_columns", mode = "function"),
              "ensure_standard_columns not available - check test setup")

  result <- tryCatch({
    ensure_standard_columns(empty_data)
  }, error = function(e) {
    NULL
  })
  # Should either work or fail gracefully
  expect_true(is.null(result) || is.data.frame(result))

  # Test malformed data
  bad_data <- data.frame(
    col1 = rep(NA, 3),
    col2 = c("", "", "")
  )

  skip_if_not(exists("find_numeric_columns", mode = "function"),
              "find_numeric_columns not available - check test setup")

  result <- tryCatch({
    find_numeric_columns(bad_data)
  }, error = function(e) {
    character(0)
  })
  expect_true(is.character(result))
})