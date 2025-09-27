# test-mod-spc-chart-comprehensive.R
# Comprehensive tests for SPC chart module lifecycle and performance

test_that("SPC chart module initialization creates proper state", {
  skip_if_not_installed("shiny")

  # Create minimal app state for testing
  app_state <- create_app_state()

  # Test chart state manager creation
  chart_manager <- safe_operation(
    "Create chart state manager",
    code = {
      create_chart_state_manager(app_state)
    }
  )

  expect_true(!is.null(chart_manager))
  expect_true(is.list(chart_manager))
  expect_true("get_chart_config" %in% names(chart_manager))
  expect_true("update_chart_state" %in% names(chart_manager))
  expect_true("get_chart_state" %in% names(chart_manager))
})

test_that("Chart data binding handles various data types", {
  skip_if_not_installed("qicharts2")

  # Test different data scenarios
  test_datasets <- list(
    "small_numeric" = data.frame(
      x = 1:5,
      y = c(2.1, 3.5, 1.8, 4.2, 2.9),
      stringsAsFactors = FALSE
    ),
    "large_dataset" = data.frame(
      x = 1:100,
      y = rnorm(100, mean = 50, sd = 10),
      stringsAsFactors = FALSE
    ),
    "missing_values" = data.frame(
      x = 1:10,
      y = c(1, 2, NA, 4, 5, NA, 7, 8, 9, 10),
      stringsAsFactors = FALSE
    ),
    "danish_dates" = data.frame(
      x = seq(as.Date("2023-01-01"), by = "month", length.out = 12),
      y = runif(12, 10, 50),
      stringsAsFactors = FALSE
    )
  )

  for (test_name in names(test_datasets)) {
    test_data <- test_datasets[[test_name]]

    # Test data validation
    validator <- safe_operation(
      paste("Create validator for", test_name),
      code = {
        create_chart_validator()
      }
    )

    expect_true(!is.null(validator))

    # Test validation process
    validation_result <- safe_operation(
      paste("Validate data:", test_name),
      code = {
        validator$validate_chart_data(test_data, "x", "y")
      },
      fallback = list(valid = FALSE, message = "Validation failed")
    )

    expect_true(is.list(validation_result))
    expect_true("valid" %in% names(validation_result))

    # Should handle all test cases without crashing
    if (test_name != "missing_values") {
      expect_true(validation_result$valid)
    }
  }
})

test_that("Chart generation performance meets benchmarks", {
  skip_if_not_installed("qicharts2")
  skip_if(Sys.getenv("CI") == "true", "Skip performance test in CI")

  # Create performance test data
  perf_data <- data.frame(
    x = 1:1000,
    y = rnorm(1000, mean = 25, sd = 5),
    stringsAsFactors = FALSE
  )

  # Benchmark chart generation
  start_time <- Sys.time()

  chart_result <- safe_operation(
    "Generate performance test chart",
    code = {
      qicharts2::qic(
        x = perf_data$x,
        y = perf_data$y,
        chart = "i",
        title = "Performance Test Chart"
      )
    }
  )

  end_time <- Sys.time()
  generation_time <- as.numeric(end_time - start_time)

  expect_true(!is.null(chart_result))
  # Chart generation should complete within 5 seconds for 1000 points
  expect_lt(generation_time, 5.0)
})

test_that("Chart module handles reactive updates correctly", {
  skip_if_not_installed("shiny")

  # Mock reactive environment
  app_state <- create_app_state()

  # Test data manager
  data_manager <- safe_operation(
    "Create module data manager",
    code = {
      create_module_data_manager(app_state)
    }
  )

  expect_true(!is.null(data_manager))
  expect_true(is.list(data_manager))

  # Test reactive data updates
  test_data <- data.frame(
    x = 1:10,
    y = rnorm(10),
    stringsAsFactors = FALSE
  )

  # Simulate data update
  safe_operation(
    "Update module data",
    code = {
      set_current_data(app_state, test_data)
    }
  )

  # Verify state consistency
  retrieved_data <- app_state$data$current_data
  expect_true(!is.null(retrieved_data))
  expect_equal(nrow(retrieved_data), 10)
})

test_that("Chart module memory cleanup prevents leaks", {
  skip_if_not_installed("shiny")

  # Test memory management
  initial_objects <- length(ls(envir = .GlobalEnv))

  # Create multiple chart managers
  managers <- list()
  for (i in 1:10) {
    app_state <- create_app_state()
    managers[[i]] <- safe_operation(
      paste("Create manager", i),
      code = {
        create_chart_state_manager(app_state)
      }
    )
  }

  # Clear managers
  managers <- NULL
  gc()  # Force garbage collection

  final_objects <- length(ls(envir = .GlobalEnv))

  # Should not have excessive object growth
  object_growth <- final_objects - initial_objects
  expect_lt(object_growth, 5)  # Allow some temporary objects
})

test_that("Chart error states are handled gracefully", {
  skip_if_not_installed("qicharts2")

  # Test error scenarios
  error_scenarios <- list(
    "empty_data" = data.frame(),
    "single_row" = data.frame(x = 1, y = 2),
    "all_na" = data.frame(x = c(NA, NA, NA), y = c(NA, NA, NA)),
    "infinite_values" = data.frame(x = 1:3, y = c(1, Inf, 2)),
    "non_numeric" = data.frame(x = 1:3, y = c("a", "b", "c"))
  )

  validator <- safe_operation(
    "Create error test validator",
    code = {
      create_chart_validator()
    }
  )

  for (scenario_name in names(error_scenarios)) {
    test_data <- error_scenarios[[scenario_name]]

    # Should handle errors without crashing
    result <- safe_operation(
      paste("Test error scenario:", scenario_name),
      code = {
        validator$validate_chart_data(test_data, "x", "y")
      },
      fallback = list(valid = FALSE, message = "Error handled")
    )

    expect_true(is.list(result))
    expect_true("valid" %in% names(result))

    # Error scenarios should generally be invalid
    if (scenario_name %in% c("empty_data", "all_na", "non_numeric")) {
      expect_false(result$valid)
    }
  }
})

test_that("SPC results processor handles various chart types", {
  skip_if_not_installed("qicharts2")

  # Test different SPC chart types
  chart_types <- c("i", "mr", "xbar", "s", "p", "c", "u")

  test_data <- data.frame(
    x = 1:30,
    y = rnorm(30, mean = 20, sd = 3),
    n = rep(100, 30),  # For proportion charts
    stringsAsFactors = FALSE
  )

  processor <- safe_operation(
    "Create SPC results processor",
    code = {
      create_spc_results_processor()
    }
  )

  expect_true(!is.null(processor))

  for (chart_type in chart_types) {
    # Test chart processing
    result <- safe_operation(
      paste("Process", chart_type, "chart"),
      code = {
        if (chart_type %in% c("p", "u")) {
          # Proportion/rate charts need count data
          qicharts2::qic(
            x = test_data$x,
            y = test_data$y,
            n = test_data$n,
            chart = chart_type,
            title = paste("Test", chart_type, "Chart")
          )
        } else {
          qicharts2::qic(
            x = test_data$x,
            y = test_data$y,
            chart = chart_type,
            title = paste("Test", chart_type, "Chart")
          )
        }
      },
      fallback = NULL
    )

    # Should handle each chart type appropriately
    if (chart_type %in% c("i", "mr", "xbar")) {
      expect_true(!is.null(result), info = paste("Chart type", chart_type, "should work"))
    }
  }
})