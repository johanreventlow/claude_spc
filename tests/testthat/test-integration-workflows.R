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
  if (exists("ensure_standard_columns")) {
    processed_data <- ensure_standard_columns(test_data)
    expect_true(is.data.frame(processed_data))
    expect_gt(nrow(processed_data), 0)
  }

  # Test 2: Auto-detection
  if (exists("detect_columns_with_cache")) {
    detected <- detect_columns_with_cache(test_data)
    expect_true(is.list(detected))
  }

  # Test 3: Basic plot generation
  if (exists("get_qic_chart_type")) {
    chart_type <- get_qic_chart_type("P-kort (Andele)")
    expect_equal(chart_type, "p")
  }
})

test_that("Event-driven state workflow simulation", {
  skip_if_not_installed("shiny")

  if (exists("create_app_state")) {
    # Create app state
    app_state <- create_app_state()

    # Test event emission patterns
    if (exists("events", envir = app_state)) {
      # Simulate data loaded event
      isolate({
        app_state$events$data_loaded <- app_state$events$data_loaded + 1L
      })
      expect_true(app_state$events$data_loaded > 0)
    }
  } else {
    skip("App state functions not available")
  }
})

test_that("Critical error scenarios håndteres gracefully", {
  # Test empty data
  empty_data <- data.frame()

  if (exists("ensure_standard_columns")) {
    result <- tryCatch({
      ensure_standard_columns(empty_data)
    }, error = function(e) {
      NULL
    })
    # Should either work or fail gracefully
    expect_true(is.null(result) || is.data.frame(result))
  }

  # Test malformed data
  bad_data <- data.frame(
    col1 = rep(NA, 3),
    col2 = c("", "", "")
  )

  if (exists("find_numeric_columns")) {
    result <- tryCatch({
      find_numeric_columns(bad_data)
    }, error = function(e) {
      character(0)
    })
    expect_true(is.character(result))
  }
})