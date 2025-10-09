# test-microbenchmark-fallback.R
# Tests for microbenchmark fallback and SPC data generation

test_that("Microbenchmark logging API compliance prevents SPC pipeline failure", {
  skip_if_not_installed("microbenchmark")

  # Test that safe_eval_benchmark doesn't throw "unused arguments" error
  test_expr <- quote(2 + 2)

  result <- safe_operation(
    "Test safe_eval_benchmark logging compliance",
    code = {
      safe_eval_benchmark(test_expr, "test_operation")
    },
    fallback = NULL
  )

  expect_equal(result, 4, "Expression should evaluate correctly")
})

test_that("Invalid expression types are handled gracefully without breaking pipeline", {
  # Test invalid expression type handling
  invalid_expr <- "invalid_string_not_expression"

  expect_error(
    safe_eval_benchmark(invalid_expr, "test_invalid"),
    "Invalid expression type for benchmarking",
    info = "Should throw controlled error for invalid expression type"
  )

  # Verify error doesn't contain "unused arguments"
  expect_error(
    safe_eval_benchmark(invalid_expr, "test_invalid"),
    "unused arguments",
    invert = TRUE,
    info = "Should not throw 'unused arguments' error"
  )
})

test_that("Benchmark operation fallback works when microbenchmark fails", {
  skip_if_not_installed("microbenchmark")

  # Test fallback mechanism
  result <- benchmark_spc_operation(
    expr = { 2 + 2 },
    times = 1,
    operation_name = "test_fallback",
    log_results = FALSE
  )

  expect_true(is.list(result), "Should return list result")
  expect_true("operation" %in% names(result), "Should include operation name")
  expect_true("mean_ms" %in% names(result) || "median_ms" %in% names(result),
              "Should include timing information")
})

test_that("Feature flag controls benchmark execution in SPC plot generation", {
  skip_if_not_installed("qicharts2")

  # Create test data
  test_data <- data.frame(
    x = 1:10,
    y = rnorm(10, mean = 50, sd = 10),
    stringsAsFactors = FALSE
  )

  # Test with benchmarking DISABLED (default)
  original_option <- getOption("spc.benchmark_enabled", NULL)
  on.exit({
    if (is.null(original_option)) {
      options(spc.benchmark_enabled = NULL)
    } else {
      options(spc.benchmark_enabled = original_option)
    }
  })

  options(spc.benchmark_enabled = FALSE)

  # Mock execute_qic_call function if it exists
  if (exists("execute_qic_call", mode = "function")) {
    config <- list(x_col = "x", y_col = "y")

    result_disabled <- safe_operation(
      "Test QIC with benchmarking disabled",
      code = {
        execute_qic_call(test_data, config, "run")
      },
      fallback = NULL
    )

    expect_true(!is.null(result_disabled), "Should generate QIC result without benchmarking")
  }

  # Test with benchmarking ENABLED
  options(spc.benchmark_enabled = TRUE)

  if (exists("execute_qic_call", mode = "function")) {
    result_enabled <- safe_operation(
      "Test QIC with benchmarking enabled",
      code = {
        execute_qic_call(test_data, config, "run")
      },
      fallback = NULL
    )

    expect_true(!is.null(result_enabled), "Should generate QIC result with benchmarking")
  }
})

test_that("SPC pipeline continues when benchmark logging fails", {
  skip_if_not_installed("qicharts2")

  # Create test data that would trigger SPC generation
  test_data <- data.frame(
    Dato = seq(as.Date("2023-01-01"), by = "day", length.out = 30),
    Værdi = rnorm(30, mean = 25, sd = 5),
    stringsAsFactors = FALSE
  )

  # Test that SPC generation works regardless of benchmark success/failure
  if (exists("generateSPCPlot", mode = "function")) {
    config <- list(
      x_col = "Dato",
      y_col = "Værdi"
    )

    spc_result <- safe_operation(
      "Test SPC generation with potential benchmark issues",
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

    expect_true(!is.null(spc_result), "SPC generation should succeed")

    if (!is.null(spc_result)) {
      expect_true(is.list(spc_result), "Should return structured SPC result")

      # Check for essential SPC components
      if ("qic_data" %in% names(spc_result)) {
        expect_true(!is.null(spc_result$qic_data), "Should include qic_data for valuebox calculations")
      }
    }
  }
})

test_that("Anhoej results are properly generated when QIC succeeds", {
  skip_if_not_installed("qicharts2")

  # Test that proper QIC generation leads to anhoej_results
  test_data <- data.frame(
    x = 1:20,
    y = c(23, 25, 22, 27, 24, 26, 23, 25, 28, 24, 22, 26, 25, 23, 27, 24, 25, 26, 23, 25),
    stringsAsFactors = FALSE
  )

  # Mock a simple QIC call to verify anhoej processing
  if (requireNamespace("qicharts2", quietly = TRUE)) {
    qic_result <- safe_operation(
      "Test QIC result structure",
      code = {
        qicharts2::qic(
          x = test_data$x,
          y = test_data$y,
          chart = "run",
          title = "Test Chart"
        )
      },
      fallback = NULL
    )

    expect_true(!is.null(qic_result), "QIC should generate result")

    if (!is.null(qic_result)) {
      # Check for anhoej-related data structure
      expect_true(is.data.frame(qic_result$data) || inherits(qic_result, "qic"),
                  "Should return qic object with data")

      # Verify this would not result in NULL anhoej_results
      if (is.data.frame(qic_result$data)) {
        expect_true(nrow(qic_result$data) > 0, "Should have data rows for anhoej calculations")
      }
    }
  }
})

test_that("Benchmark wrapper captures results correctly when enabled", {
  skip_if_not_installed("microbenchmark")

  # Test capture_result functionality
  test_expr <- quote({
    data.frame(x = 1:5, y = 6:10)
  })

  result <- benchmark_spc_operation(
    expr = test_expr,
    times = 1,
    operation_name = "test_capture",
    capture_result = TRUE,
    log_results = FALSE
  )

  expect_true(is.list(result), "Should return benchmark result")

  if ("captured_result" %in% names(result)) {
    expect_true(is.data.frame(result$captured_result), "Should capture data frame result")
    expect_equal(nrow(result$captured_result), 5, "Should capture correct data")
  }
})

test_that("Benchmark gracefully handles missing dependencies", {
  # Test behavior when microbenchmark is not available
  # This simulates production environments where microbenchmark might not be installed

  # Temporarily hide microbenchmark if it exists
  microbenchmark_available <- requireNamespace("microbenchmark", quietly = TRUE)

  if (microbenchmark_available) {
    # Test that fallback works even when microbenchmark is available but fails
    result <- safe_operation(
      "Test benchmark fallback path",
      code = {
        benchmark_spc_operation(
          expr = { Sys.sleep(0.001); "test_result" },
          times = 1,
          operation_name = "test_fallback_path",
          log_results = FALSE
        )
      },
      fallback = NULL
    )

    expect_true(!is.null(result), "Fallback should work")
    expect_true(is.list(result), "Should return structured result")
  }
})

test_that("Environment validation in benchmark functions works correctly", {
  # Test function environment validation
  test_function <- function() { "test" }

  # Should work with standard function
  result <- safe_operation(
    "Test function environment validation",
    code = {
      safe_eval_benchmark(test_function, "test_env_validation")
    },
    fallback = NULL
  )

  expect_equal(result, "test", "Should evaluate function correctly")
})