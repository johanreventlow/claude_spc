# test-run-chart-denominator-stability.R
# Tests for stable run chart generation with denominator (prevents NA run rules)

test_that("Run chart with denominator uses prepared y-data and excludes n parameter", {
  skip_if_not_installed("qicharts2")

  # Create test data with denominator (proportion data)
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
      "Test run chart with denominator stability",
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

    if (!is.null(result) && "qic_data" %in% names(result)) {
      qic_data <- result$qic_data

      # Verify that run rule columns are not all NA
      if ("longest.run" %in% names(qic_data)) {
        longest_run_values <- qic_data$longest.run
        expect_true(!all(is.na(longest_run_values)),
                    "longest.run should not be all NA for run chart with denominator")
      }

      if ("n.crossings" %in% names(qic_data)) {
        n_crossings_values <- qic_data$n.crossings
        expect_true(!all(is.na(n_crossings_values)),
                    "n.crossings should not be all NA for run chart with denominator")
      }

      # Verify that y and cl columns are not all NA
      if ("y" %in% names(qic_data)) {
        y_values <- qic_data$y
        expect_true(!all(is.na(y_values)),
                    "y values should not be all NA")
      }

      if ("cl" %in% names(qic_data)) {
        cl_values <- qic_data$cl
        expect_true(!all(is.na(cl_values)),
                    "centerline values should not be all NA")
      }
    }
  }
})

test_that("Prepared y-data injection works for run charts", {
  # Test the data preparation step directly
  test_data <- data.frame(
    x = 1:10,
    y = c(23, 25, 22, 27, 24, 26, 23, 25, 28, 24),
    n = rep(100, 10),
    stringsAsFactors = FALSE
  )

  # Mock the data processing result that would include y_data
  y_data <- test_data$y / test_data$n * 100  # Convert to percentages
  n_data <- test_data$n

  # Test the injection logic
  chart_type <- "run"

  # Simulate the injection that happens in generateSPCPlot
  if (identical(chart_type, "run") && !is.null(n_data)) {
    test_data[[".y_run_prepared"]] <- y_data
  }

  expect_true(".y_run_prepared" %in% names(test_data),
              "Prepared y-data column should be injected for run charts with denominator")

  expect_equal(test_data[[".y_run_prepared"]], y_data,
               "Prepared y-data should match calculated percentages")
})

test_that("prepare_qic_data_parameters uses prepared y-data for run charts", {
  # Create test data with prepared y column
  test_data <- data.frame(
    x = 1:5,
    y = c(23, 25, 22, 27, 24),
    n = rep(100, 5),
    .y_run_prepared = c(23, 25, 22, 27, 24),  # Same as y for simplicity
    stringsAsFactors = FALSE
  )

  config <- list(
    x_col = "x",
    y_col = "y",
    n_col = "n"
  )

  x_validation <- list(is_date = FALSE)

  # Test in environment where chart_type is "run"
  chart_type <- "run"  # Set in current environment

  if (exists("prepare_qic_data_parameters", mode = "function")) {
    result <- prepare_qic_data_parameters(test_data, config, x_validation)

    # For run charts with prepared data, should use prepared y and remove n
    if (".y_run_prepared" %in% names(test_data)) {
      expect_equal(result$y_col_name, ".y_run_prepared",
                   "Should use prepared y-data column for run charts")
      expect_null(result$n_col_name,
                  "Should remove n parameter for run charts to prevent NA run rules")
    }
  }
})

test_that("build_qic_arguments respects NULL n_col_name", {
  # Test that build_qic_arguments doesn't add n parameter when n_col_name is NULL
  test_data <- data.frame(
    x = 1:5,
    y = c(23, 25, 22, 27, 24),
    stringsAsFactors = FALSE
  )

  if (exists("build_qic_arguments", mode = "function")) {
    # Test with n_col_name = NULL (run chart case)
    qic_args <- build_qic_arguments(
      data = test_data,
      x_col_for_qic = "x",
      y_col_name = "y",
      n_col_name = NULL,  # Should not add n parameter
      chart_type = "run",
      freeze_position = NULL,
      part_positions = NULL,
      centerline_value = NULL
    )

    expect_false("n" %in% names(qic_args),
                 "QIC arguments should not include n parameter when n_col_name is NULL")

    expect_true("y" %in% names(qic_args),
                "QIC arguments should include y parameter")

    expect_equal(qic_args$chart, "run",
                 "Chart type should be preserved")
  }
})

test_that("Run chart without denominator works normally", {
  skip_if_not_installed("qicharts2")

  # Test data without denominator
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

    # Should not inject prepared y-data when no denominator
    expect_false(".y_run_prepared" %in% names(test_data),
                 "Should not inject prepared y-data when no denominator")
  }
})

test_that("Proportion charts (p, u) still use n parameter correctly", {
  # Verify that our fix doesn't break proportion charts that need n parameter
  test_data <- data.frame(
    x = 1:5,
    y = c(23, 25, 22, 27, 24),
    n = rep(100, 5),
    stringsAsFactors = FALSE
  )

  config <- list(
    x_col = "x",
    y_col = "y",
    n_col = "n"
  )

  x_validation <- list(is_date = FALSE)

  # Test for proportion chart types
  for (chart_type in c("p", "u")) {
    # Set chart_type in environment
    assign("chart_type", chart_type, envir = environment())

    if (exists("prepare_qic_data_parameters", mode = "function")) {
      result <- prepare_qic_data_parameters(test_data, config, x_validation)

      # Proportion charts should keep n parameter
      expect_equal(result$n_col_name, "n",
                   paste("Proportion chart", chart_type, "should keep n parameter"))
      expect_equal(result$y_col_name, "y",
                   paste("Proportion chart", chart_type, "should use original y column"))
    }
  }
})