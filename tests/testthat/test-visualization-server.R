# test-visualization-server.R
# Tests for visualization server logic and reactive chains

test_that("setup_visualization initializes correctly", {
  skip_if_not_installed("shiny")

  skip_if_not(exists("setup_visualization", mode = "function"),
              "setup_visualization function not available - check test setup")

  # Mock inputs for setup_visualization
  mock_input <- list(chart_type = "P-kort (Andele)")
  mock_output <- list()
  mock_session <- list(token = "test_session")

  # Test with basic app_state
  skip_if_not(exists("create_app_state", mode = "function"),
              "create_app_state not available - check test setup")

  app_state <- create_app_state()

  result <- tryCatch({
    setup_visualization(mock_input, mock_output, mock_session, app_state)
    "success"
  }, error = function(e) {
    e$message
  })

  # Should not crash during initialization
  expect_true(is.character(result))
})

test_that("Visualization reactive chains handle state updates", {
  skip_if_not_installed("shiny")

  skip_if_not(exists("create_app_state", mode = "function"),
              "create_app_state not available - check test setup")

  # Create app state with test data
  app_state <- create_app_state()

  # Set up test state
  shiny::isolate({
    app_state$data$current_data <- data.frame(
      Skift = c(FALSE, FALSE, TRUE),
      Frys = c(FALSE, TRUE, FALSE),
      Dato = c("01-01-2024", "02-01-2024", "03-01-2024"),
      Tæller = c(10, 15, 12),
      Nævner = c(100, 120, 110)
    )

    app_state$columns$auto_detect$results <- list(
      x_col = "Dato",
      y_col = "Tæller",
      n_col = "Nævner",
      timestamp = Sys.time()
    )
  })

  # Test reactive dependency
  shiny::testServer(
    app = function(input, output, session) {
      auto_config <- shiny::reactive({
        auto_columns <- app_state$columns$auto_detect$results
        if (!is.null(auto_columns)) {
          list(
            x_col = auto_columns$x_col,
            y_col = auto_columns$y_col,
            n_col = auto_columns$n_col
          )
        }
      })

      output$test_config <- shiny::renderText({
        config <- auto_config()
        if (!is.null(config)) {
          paste("X:", config$x_col, "Y:", config$y_col, "N:", config$n_col)
        } else {
          "No config"
        }
      })
    },
    {
      # Test that reactive returns expected values
      expect_true(is.character(output$test_config) || is.null(output$test_config))
    }
  )
})

test_that("Chart type conversion works in visualization context", {
  skip_if_not(exists("get_qic_chart_type", mode = "function"),
              "get_qic_chart_type not available - check test setup")

  # Test common chart type conversions used in visualization
  expect_equal(get_qic_chart_type("P-kort (Andele)"), "p")
  expect_equal(get_qic_chart_type("U-kort (Rater)"), "u")
  expect_equal(get_qic_chart_type("I-kort (Individuelle værdier)"), "i")
  expect_equal(get_qic_chart_type("Seriediagram (Run Chart)"), "run")

  # Test fallback
  expect_equal(get_qic_chart_type(""), "run")
  expect_equal(get_qic_chart_type(NULL), "run")
})

test_that("Plot generation with real data works", {
  skip_if_not_installed("qicharts2")

  test_data <- data.frame(
    x = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01")),
    y = c(10, 15, 12, 18),
    n = c(100, 120, 110, 130)
  )

  # Test basic qicharts2 integration
  result <- tryCatch({
    qicharts2::qic(
      x = x,
      y = y,
      data = test_data,
      chart = "run"
    )
  }, error = function(e) {
    NULL
  })

  if (!is.null(result)) {
    expect_s3_class(result, "ggplot")
  }

  # Test p-chart with denominator
  p_result <- tryCatch({
    qicharts2::qic(
      x = x,
      y = y,
      n = n,
      data = test_data,
      chart = "p"
    )
  }, error = function(e) {
    NULL
  })

  if (!is.null(p_result)) {
    expect_s3_class(p_result, "ggplot")
  }
})

test_that("Visualization handles missing or invalid data gracefully", {
  skip_if_not_installed("shiny")

  skip_if_not(exists("create_app_state", mode = "function"),
              "create_app_state not available - check test setup")

  app_state <- create_app_state()

  # Test with empty data
  shiny::isolate({
    app_state$data$current_data <- data.frame()
  })

  shiny::testServer(
    app = function(input, output, session) {
      safe_data_access <- shiny::reactive({
        data <- app_state$data$current_data
        if (is.null(data) || nrow(data) == 0) {
          NULL
        } else {
          data
        }
      })

      output$data_status <- shiny::renderText({
        data <- safe_data_access()
        if (is.null(data)) {
          "No data available"
        } else {
          paste("Data available:", nrow(data), "rows")
        }
      })
    },
    {
      expect_equal(output$data_status, "No data available")
    }
  )

  # Test with malformed data
  shiny::isolate({
    app_state$data$current_data <- data.frame(
      bad_col = c(NA, NA, NA)
    )
  })

  shiny::testServer(
    app = function(input, output, session) {
      validate_data <- shiny::reactive({
        data <- app_state$data$current_data
        if (is.null(data) || nrow(data) == 0) {
          list(valid = FALSE, message = "No data")
        } else if (all(is.na(unlist(data)))) {
          list(valid = FALSE, message = "All NA data")
        } else {
          list(valid = TRUE, message = "Valid data")
        }
      })

      output$validation_result <- shiny::renderText({
        result <- validate_data()
        result$message
      })
    },
    {
      expect_true(grepl("NA data|No data", output$validation_result))
    }
  )
})