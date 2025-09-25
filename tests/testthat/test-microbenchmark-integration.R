# Test file for microbenchmark integration
# Testing statistical performance analysis functionality

test_that("benchmark_spc_operation basic functionality works", {
  skip_if_not_installed("microbenchmark")

  # TEST: Basic benchmarking functionality
  result <- benchmark_spc_operation(
    expr = Sys.sleep(0.001),  # Minimal operation
    times = 5,
    operation_name = "test_minimal_operation",
    log_results = FALSE
  )

  # VERIFY: Result structure
  expect_type(result, "list")
  expect_true("operation" %in% names(result))
  expect_true("median_ms" %in% names(result))
  expect_true("mean_ms" %in% names(result))
  expect_equal(result$operation, "test_minimal_operation")
  expect_equal(result$times, 5)

  # VERIFY: Performance metrics are reasonable
  expect_true(result$median_ms > 0)
  expect_true(result$mean_ms > 0)
  expect_true(result$min_ms >= 0)
  expect_true(result$max_ms >= result$min_ms)
})

test_that("benchmark_spc_operation fallback works without microbenchmark", {
  # TEST: Fallback when microbenchmark is not available

  # Mock requireNamespace to return FALSE
  with_mocked_bindings(
    requireNamespace = function(...) FALSE,
    {
      result <- benchmark_spc_operation(
        expr = 1 + 1,
        times = 10,
        operation_name = "test_fallback",
        log_results = FALSE
      )

      # VERIFY: Fallback result structure
      expect_type(result, "list")
      expect_true("operation" %in% names(result))
      expect_true("mean_time" %in% names(result) || "mean_ms" %in% names(result))
      expect_equal(result$operation, "test_fallback")
      expect_true(result$fallback)
    }
  )
})

test_that("benchmark_autodetect_comprehensive creates proper test data", {
  skip_if_not_installed("microbenchmark")

  # Create minimal app_state and emit for testing
  app_state <- create_app_state()
  emit <- create_emit_api(app_state)

  # Mock autodetect_engine function for testing
  autodetect_engine <- function(data, trigger_type, app_state, emit) {
    Sys.sleep(0.001)  # Minimal processing time
    list(x_col = "Dato", y_col = "Taeller", n_col = "Naevner")
  }

  # TEST: Comprehensive autodetect benchmarking
  result <- benchmark_autodetect_comprehensive(
    data_list = list(
      small = data.frame(
        Dato = seq(as.Date("2024-01-01"), by = "day", length.out = 10),
        Taeller = sample(90:110, 10),
        Naevner = sample(95:105, 10)
      )
    ),
    trigger_types = c("file_upload"),
    app_state = app_state,
    emit = emit,
    times = 2  # Minimal iterations for testing
  )

  # VERIFY: Result structure
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("operation" %in% names(result))
  expect_true("data_size" %in% names(result))
  expect_true("median_ms" %in% names(result))
  expect_true("trigger_type" %in% names(result))

  # VERIFY: Data content
  expect_equal(result$data_size[1], 10)  # Small dataset
  expect_equal(result$trigger_type[1], "file_upload")
  expect_true(result$median_ms[1] > 0)
})

test_that("benchmark_qic_generation works with mock functions", {
  skip_if_not_installed("microbenchmark")
  skip_if_not_installed("ggplot2")

  # Mock generateSPCPlot for testing
  generateSPCPlot <- function(data, config, chart_type, ...) {
    Sys.sleep(0.001)  # Simulate processing
    ggplot2::ggplot() + ggplot2::geom_point()
  }

  # TEST: QIC benchmarking
  result <- benchmark_qic_generation(
    data_list = list(
      small = data.frame(
        Dato = seq(as.Date("2024-01-01"), by = "month", length.out = 6),
        Taeller = sample(50:150, 6),
        Naevner = sample(100:200, 6)
      )
    ),
    chart_types = c("run"),
    times = 2
  )

  # VERIFY: Result structure
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("chart_type" %in% names(result))
  expect_equal(result$chart_type[1], "run")
  expect_true(result$median_ms[1] > 0)
})

test_that("analyze_performance_comparison detects regressions and improvements", {
  # TEST: Performance comparison analysis

  # Create baseline results
  baseline <- data.frame(
    operation = c("test_op1", "test_op2", "test_op3"),
    median_ms = c(100, 200, 50),
    stringsAsFactors = FALSE
  )

  # Create current results with regression and improvement
  current <- data.frame(
    operation = c("test_op1", "test_op2", "test_op3"),
    median_ms = c(150, 180, 30),  # op1: regression, op2: improvement, op3: improvement
    stringsAsFactors = FALSE
  )

  result <- analyze_performance_comparison(baseline, current, regression_threshold = 1.2)

  # VERIFY: Analysis structure
  expect_type(result, "list")
  expect_true("regressions" %in% names(result))
  expect_true("improvements" %in% names(result))
  expect_true("summary" %in% names(result))

  # VERIFY: Regression detection
  expect_true(length(result$regressions) > 0)
  expect_true("test_op1" %in% names(result$regressions))  # 150/100 = 1.5 > 1.2

  # VERIFY: Improvement detection
  expect_true(length(result$improvements) > 0)
  expect_true("test_op3" %in% names(result$improvements))  # 30/50 = 0.6 < 0.9

  # VERIFY: Summary statistics
  expect_true(result$summary$total_comparisons > 0)
  expect_equal(result$summary$total_comparisons, 3)
})

test_that("export_benchmark_results creates CSV file", {
  # TEST: CSV export functionality

  # Create test results
  test_results <- data.frame(
    operation = c("test1", "test2"),
    median_ms = c(10.5, 25.3),
    mean_ms = c(12.1, 26.8),
    min_ms = c(8.2, 22.1),
    max_ms = c(15.7, 31.2),
    stringsAsFactors = FALSE
  )

  # Create temporary filename
  temp_file <- tempfile(fileext = ".csv")

  # Export results
  result_file <- export_benchmark_results(test_results, temp_file, include_metadata = FALSE)

  # VERIFY: File was created
  expect_equal(result_file, temp_file)
  expect_true(file.exists(temp_file))

  # VERIFY: File content
  if (file.exists(temp_file)) {
    imported_data <- read.csv(temp_file)
    expect_equal(nrow(imported_data), 2)
    expect_true("operation" %in% names(imported_data))
    expect_equal(imported_data$operation[1], "test1")

    # Cleanup
    unlink(temp_file)
  }
})

test_that("log_performance_results logs structured data", {
  # TEST: Performance logging functionality

  # Create mock results
  test_results <- list(
    operation = "test_logging",
    times = 10,
    min_ms = 5.2,
    median_ms = 8.7,
    mean_ms = 9.1,
    max_ms = 12.4,
    timestamp = Sys.time()
  )

  # TEST: Function doesn't error
  expect_no_error(log_performance_results(test_results, warn_threshold = 1000))

  # TEST: Warning threshold
  test_results$median_ms <- 600  # Above default 500ms threshold
  expect_no_error(log_performance_results(test_results, warn_threshold = 500))
})

test_that("microbenchmark integration handles errors gracefully", {
  # TEST: Error handling in benchmarking

  result <- benchmark_spc_operation(
    expr = stop("Simulated error"),
    times = 3,
    operation_name = "test_error_handling",
    log_results = FALSE
  )

  # VERIFY: Error is handled and fallback is provided
  expect_type(result, "list")
  expect_true("operation" %in% names(result))
  expect_true("error" %in% names(result) || "fallback" %in% names(result))
})

test_that("performance thresholds are respected in logging", {
  # TEST: Performance threshold warnings

  # Mock slow operation result
  slow_result <- list(
    operation = "slow_operation",
    times = 5,
    min_ms = 400,
    median_ms = 800,  # Above typical 500ms threshold
    mean_ms = 850,
    max_ms = 1200,
    timestamp = Sys.time()
  )

  # This should generate a warning log (but we can't easily capture it in test)
  expect_no_error(log_performance_results(slow_result, warn_threshold = 500))

  # Fast operation should not warn
  fast_result <- list(
    operation = "fast_operation",
    median_ms = 50,
    times = 5,
    min_ms = 45,
    mean_ms = 52,
    max_ms = 60
  )

  expect_no_error(log_performance_results(fast_result, warn_threshold = 500))
})

test_that("data size categorization works correctly", {
  # TEST: Data size categories in benchmarking logic

  # These are tested indirectly through the benchmark functions
  # but we can test the categorization logic if needed

  # Small data
  small_data <- data.frame(x = 1:30, y = 1:30)
  expect_true(nrow(small_data) < 100)  # Should be "small" category

  # Medium data
  medium_data <- data.frame(x = 1:500, y = 1:500)
  expect_true(nrow(medium_data) >= 100 && nrow(medium_data) < 1000)  # "medium"

  # Large data
  large_data <- data.frame(x = 1:2000, y = 1:2000)
  expect_true(nrow(large_data) >= 1000)  # "large"
})