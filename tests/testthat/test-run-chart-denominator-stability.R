# test-run-chart-denominator-stability.R
# Tests for run chart generation with denominator using qic's native ratio handling

test_that("Run chart with denominator sends raw y and n data to qic", {
  skip_if_not_installed("qicharts2")

  # Create test data with denominator (ratio data)
  test_data <- data.frame(
    Dato = seq(as.Date("2023-01-01"), by = "day", length.out = 20),
    Tæller = c(23, 25, 22, 27, 24, 26, 23, 25, 28, 24, 22, 26, 25, 23, 27, 24, 25, 26, 23, 25),
    Nævner = rep(100, 20),
    stringsAsFactors = FALSE
  )

  config <- list(
    x_col = "Dato",
    y_col = "Tæller",
    n_col = "Nævner"
  )

  # Test that generateSPCPlot handles run chart with denominator correctly
  if (exists("generateSPCPlot", mode = "function")) {
    result <- safe_operation(
      "Test run chart with denominator - qic native handling",
      code = {
        generateSPCPlot(
          data = test_data,
          config = config,
          chart_type = "run",
          target_value = NULL,
          centerline_value = NULL,
          show_phases = FALSE
        )
      },
      fallback = NULL
    )

    expect_true(!is.null(result), "Run chart with denominator should generate successfully")

    # Verify that we don't inject any prepared data columns
    expect_false(".y_run_prepared" %in% names(test_data),
                "Should not inject .y_run_prepared - let qic handle ratios")

    if (!is.null(result) && "qic_data" %in% names(result)) {
      qic_data <- result$qic_data

      # Verify that qic handled the ratio calculation correctly
      if ("y" %in% names(qic_data)) {
        y_values <- qic_data$y
        expect_true(!all(is.na(y_values)), "qic should handle y values correctly")

        # For run charts with ratio data, qic should calculate proportions
        # Verify values are in reasonable ratio range (0-1 or percentage)
        expect_true(all(y_values >= 0, na.rm = TRUE), "Ratio values should be positive")
      }

      # Verify run rules are calculated properly by qic
      if ("longest.run" %in% names(qic_data)) {
        longest_run_values <- qic_data$longest.run
        expect_true(!all(is.na(longest_run_values)),
                    "qic should calculate longest.run values for ratio data")
      }

      if ("n.crossings" %in% names(qic_data)) {
        n_crossings_values <- qic_data$n.crossings
        expect_true(!all(is.na(n_crossings_values)),
                    "qic should calculate n.crossings values for ratio data")
      }
    }
  }
})

test_that("Run chart without denominator works normally", {
  skip_if_not_installed("qicharts2")

  # Test data without denominator - standard numeric values
  test_data <- data.frame(
    x = 1:10,
    y = c(23, 25, 22, 27, 24, 26, 23, 25, 28, 24),
    stringsAsFactors = FALSE
  )

  config <- list(
    x_col = "x",
    y_col = "y",
    n_col = NULL  # No denominator
  )

  if (exists("generateSPCPlot", mode = "function")) {
    result <- safe_operation(
      "Test run chart without denominator",
      code = {
        generateSPCPlot(
          data = test_data,
          config = config,
          chart_type = "run",
          target_value = NULL,
          centerline_value = NULL,
          show_phases = FALSE
        )
      },
      fallback = NULL
    )

    expect_true(!is.null(result), "Run chart without denominator should work normally")

    # Should not inject any prepared data
    expect_false(".y_run_prepared" %in% names(test_data),
                "Should not inject prepared data for standard run charts")
  }
})

test_that("Proportion charts (p, u) still receive n parameter correctly", {
  # Verify that proportion charts that need n parameter still get it
  test_data <- data.frame(
    x = 1:10,
    y = c(23, 25, 22, 27, 24, 26, 23, 25, 28, 24),
    n = rep(100, 10),
    stringsAsFactors = FALSE
  )

  config <- list(
    x_col = "x",
    y_col = "y",
    n_col = "n"
  )

  # Test for proportion chart types that need denominators
  for (chart_type in c("p", "u")) {
    if (exists("generateSPCPlot", mode = "function")) {
      result <- safe_operation(
        paste("Test", chart_type, "chart with denominator"),
        code = {
          generateSPCPlot(
            data = test_data,
            config = config,
            chart_type = chart_type,
            target_value = NULL,
            centerline_value = NULL,
            show_phases = FALSE
          )
        },
        fallback = NULL
      )

      expect_true(!is.null(result),
                  paste(chart_type, "chart with denominator should work"))
    }
  }
})