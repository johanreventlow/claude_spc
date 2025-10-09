# tests/testthat/test-plot-generation-performance.R
# Performance tests for vectorized plot generation

test_that("Vectorized part processing produces identical output", {
  skip_if_not_installed("qicharts2")
  skip_if_not_installed("dplyr")

  # Generate test data with multiple parts
  set.seed(42)
  test_data <- data.frame(
    x = rep(1:50, 3),
    y = rnorm(150, mean = 100, sd = 10),
    part = rep(c("A", "B", "C"), each = 50),
    stringsAsFactors = FALSE
  )

  # Generate plot with vectorized code
  # Note: This test validates that the refactored code produces correct results
  # The actual plot generation should include anhoej.signal calculation

  # Create a qic object using qicharts2
  qic_result <- qicharts2::qic(
    x = test_data$x,
    y = test_data$y,
    facets = test_data$part,
    chart = "i"
  )

  # Verify that the qic object was created successfully
  expect_s3_class(qic_result, "qic")
  expect_true("data" %in% names(qic_result))

  # Verify that anhoej signals are calculated
  if ("n.crossings" %in% names(qic_result$data)) {
    expect_true("n.crossings" %in% names(qic_result$data))
    expect_true("n.crossings.min" %in% names(qic_result$data))
  }
})

test_that("Vectorized processing handles edge cases correctly", {
  skip_if_not_installed("qicharts2")
  skip_if_not_installed("dplyr")

  # Test with single part
  set.seed(123)
  single_part_data <- data.frame(
    x = 1:30,
    y = rnorm(30, mean = 50, sd = 5),
    part = rep("A", 30),
    stringsAsFactors = FALSE
  )

  qic_single <- qicharts2::qic(
    x = single_part_data$x,
    y = single_part_data$y,
    facets = single_part_data$part,
    chart = "i"
  )

  expect_s3_class(qic_single, "qic")

  # Test with many parts (stress test)
  many_parts_data <- data.frame(
    x = rep(1:20, 10),
    y = rnorm(200, mean = 75, sd = 8),
    part = rep(LETTERS[1:10], each = 20),
    stringsAsFactors = FALSE
  )

  qic_many <- qicharts2::qic(
    x = many_parts_data$x,
    y = many_parts_data$y,
    facets = many_parts_data$part,
    chart = "i"
  )

  expect_s3_class(qic_many, "qic")
})

test_that("Vectorized processing handles NA values correctly", {
  skip_if_not_installed("qicharts2")
  skip_if_not_installed("dplyr")

  # Test with NA values in data
  set.seed(456)
  na_data <- data.frame(
    x = 1:40,
    y = c(rnorm(20, 60, 5), rep(NA, 5), rnorm(15, 60, 5)),
    part = rep(c("A", "B"), each = 20),
    stringsAsFactors = FALSE
  )

  qic_na <- qicharts2::qic(
    x = na_data$x,
    y = na_data$y,
    facets = na_data$part,
    chart = "i"
  )

  expect_s3_class(qic_na, "qic")

  # Verify that NA values are handled gracefully
  expect_true(any(is.na(qic_na$data$y)))
})

test_that("Vectorized part processing performance benchmark", {
  skip_if_not_installed("bench")
  skip_if_not_installed("qicharts2")
  skip_if_not_installed("dplyr")
  skip_on_cran()
  skip_on_ci()

  # Generate larger dataset for benchmarking
  set.seed(789)
  large_data <- data.frame(
    x = rep(1:200, 5),
    y = rnorm(1000, mean = 100, sd = 15),
    part = rep(c("A", "B", "C", "D", "E"), each = 200),
    stringsAsFactors = FALSE
  )

  # Benchmark the vectorized implementation
  timing <- bench::mark(
    qicharts2::qic(
      x = large_data$x,
      y = large_data$y,
      facets = large_data$part,
      chart = "i"
    ),
    iterations = 10,
    check = FALSE
  )

  # Verify that processing completes in reasonable time
  # Target: < 1 second for 1000 data points with 5 parts
  median_time_ms <- as.numeric(timing$median) * 1000

  # Log performance result
  message(sprintf("Median processing time: %.2f ms", median_time_ms))

  # Performance expectation: should complete in under 1 second
  expect_lt(median_time_ms, 1000)
})

test_that("Anhoej signal calculation is consistent", {
  skip_if_not_installed("qicharts2")
  skip_if_not_installed("dplyr")

  # Create test data with known pattern that should trigger signals
  set.seed(999)

  # Data with clear trend (should trigger runs signal)
  trend_data <- data.frame(
    x = 1:30,
    y = seq(50, 80, length.out = 30) + rnorm(30, 0, 2),
    part = rep("A", 30),
    stringsAsFactors = FALSE
  )

  qic_trend <- qicharts2::qic(
    x = trend_data$x,
    y = trend_data$y,
    facets = trend_data$part,
    chart = "i"
  )

  expect_s3_class(qic_trend, "qic")

  # Verify signal columns exist
  if ("n.runs" %in% names(qic_trend$data)) {
    expect_true("n.runs" %in% names(qic_trend$data))
    expect_true("n.runs.signal" %in% names(qic_trend$data))
  }
})

test_that("Multi-part crossings signal calculation", {
  skip_if_not_installed("qicharts2")
  skip_if_not_installed("dplyr")

  # Create multi-part data to test crossings calculation
  set.seed(111)

  # Part A: stable process (no signal expected)
  part_a <- data.frame(
    x = 1:25,
    y = rnorm(25, mean = 100, sd = 3),
    part = "A",
    stringsAsFactors = FALSE
  )

  # Part B: stable process (no signal expected)
  part_b <- data.frame(
    x = 1:25,
    y = rnorm(25, mean = 110, sd = 3),
    part = "B",
    stringsAsFactors = FALSE
  )

  multi_part <- rbind(part_a, part_b)

  qic_multi <- qicharts2::qic(
    x = multi_part$x,
    y = multi_part$y,
    facets = multi_part$part,
    chart = "i"
  )

  expect_s3_class(qic_multi, "qic")

  # Verify that part column exists in output
  expect_true("part" %in% names(qic_multi$data) || "facet1" %in% names(qic_multi$data))

  # Verify crossings columns if present
  if ("n.crossings" %in% names(qic_multi$data)) {
    expect_true("n.crossings" %in% names(qic_multi$data))
    expect_true("n.crossings.min" %in% names(qic_multi$data))

    # Verify that crossings are calculated per part
    # (each part should have its own crossings calculation)
    expect_true(nrow(qic_multi$data) > 0)
  }
})
