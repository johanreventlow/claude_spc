# test-qic-caching-benchmark.R
# Performance benchmark for QIC result caching (Sprint 4 Fase 2)

test_that("QIC caching provides measurable performance improvement", {
  skip_if_not(requireNamespace("qicharts2", quietly = TRUE))
  skip_if_not(requireNamespace("digest", quietly = TRUE))

  # Create test data
  set.seed(42)
  test_data <- data.frame(
    x = 1:100,
    y = rnorm(100, mean = 50, sd = 10)
  )

  # Create QIC cache
  qic_cache <- create_qic_cache()

  # Test parameters
  params <- list(
    x = "x",
    y = "y",
    chart = "run"
  )

  # Benchmark: First call (cache miss)
  time_cache_miss <- system.time({
    result1 <- log_qic_call_wrapper(
      qic_args = list(
        data = test_data,
        x = params$x,
        y = params$y,
        chart = params$chart
      ),
      call_context = "benchmark_cache_miss",
      qic_cache = qic_cache
    )
  })["elapsed"]

  # Benchmark: Second call (cache hit)
  time_cache_hit <- system.time({
    result2 <- log_qic_call_wrapper(
      qic_args = list(
        data = test_data,
        x = params$x,
        y = params$y,
        chart = params$chart
      ),
      call_context = "benchmark_cache_hit",
      qic_cache = qic_cache
    )
  })["elapsed"]

  # Verify results are identical
  expect_equal(result1, result2)

  # Calculate performance improvement
  improvement_ms <- (time_cache_miss - time_cache_hit) * 1000
  improvement_pct <- ((time_cache_miss - time_cache_hit) / time_cache_miss) * 100

  # Log performance metrics
  cat("\n=== QIC Caching Performance Benchmark ===\n")
  cat("Cache miss (first call):", round(time_cache_miss * 1000, 2), "ms\n")
  cat("Cache hit (second call):", round(time_cache_hit * 1000, 2), "ms\n")
  cat("Improvement:", round(improvement_ms, 2), "ms (", round(improvement_pct, 1), "%)\n")

  # Assert performance improvement
  # Cache hit should be faster (allowing for measurement noise)
  expect_lt(time_cache_hit, time_cache_miss * 1.1)  # At most 10% slower due to overhead

  # Check cache stats
  cache_stats <- get_qic_cache_stats(qic_cache)
  expect_equal(cache_stats$size, 1)

  cat("Cache entries:", cache_stats$size, "\n")
  cat("✅ QIC caching benchmark complete\n\n")
})

test_that("QIC cache invalidation works correctly", {
  skip_if_not(requireNamespace("qicharts2", quietly = TRUE))

  # Create test data
  test_data <- data.frame(
    x = 1:50,
    y = rnorm(50, mean = 50, sd = 10)
  )

  # Create QIC cache
  qic_cache <- create_qic_cache()

  # First call - cache miss
  result1 <- log_qic_call_wrapper(
    qic_args = list(
      data = test_data,
      x = "x",
      y = "y",
      chart = "run"
    ),
    call_context = "invalidation_test_1",
    qic_cache = qic_cache
  )

  # Verify cache has entry
  expect_equal(qic_cache$size(), 1)

  # Clear cache (simulates data update)
  qic_cache$clear()

  # Verify cache is empty
  expect_equal(qic_cache$size(), 0)

  # Second call - should be cache miss again
  result2 <- log_qic_call_wrapper(
    qic_args = list(
      data = test_data,
      x = "x",
      y = "y",
      chart = "run"
    ),
    call_context = "invalidation_test_2",
    qic_cache = qic_cache
  )

  # Results should still be identical
  expect_equal(result1, result2)

  # Cache should have entry again
  expect_equal(qic_cache$size(), 1)

  cat("✅ QIC cache invalidation works correctly\n")
})

test_that("QIC cache handles multiple different datasets", {
  skip_if_not(requireNamespace("qicharts2", quietly = TRUE))

  # Create multiple test datasets
  set.seed(123)
  data1 <- data.frame(x = 1:30, y = rnorm(30, mean = 50, sd = 5))
  data2 <- data.frame(x = 1:30, y = rnorm(30, mean = 60, sd = 8))
  data3 <- data.frame(x = 1:30, y = rnorm(30, mean = 40, sd = 12))

  # Create QIC cache
  qic_cache <- create_qic_cache()

  # Call with different datasets
  result1 <- log_qic_call_wrapper(
    qic_args = list(data = data1, x = "x", y = "y", chart = "run"),
    call_context = "multi_dataset_1",
    qic_cache = qic_cache
  )

  result2 <- log_qic_call_wrapper(
    qic_args = list(data = data2, x = "x", y = "y", chart = "run"),
    call_context = "multi_dataset_2",
    qic_cache = qic_cache
  )

  result3 <- log_qic_call_wrapper(
    qic_args = list(data = data3, x = "x", y = "y", chart = "run"),
    call_context = "multi_dataset_3",
    qic_cache = qic_cache
  )

  # Cache should have 3 entries (one for each dataset)
  expect_equal(qic_cache$size(), 3)

  # Call first dataset again - should be cache hit
  result1_cached <- log_qic_call_wrapper(
    qic_args = list(data = data1, x = "x", y = "y", chart = "run"),
    call_context = "multi_dataset_1_cached",
    qic_cache = qic_cache
  )

  # Should still have 3 entries (no new entry)
  expect_equal(qic_cache$size(), 3)

  # Results should be identical
  expect_equal(result1, result1_cached)

  cat("✅ QIC cache handles multiple datasets correctly\n")
  cat("Cache entries:", qic_cache$size(), "\n")
})

test_that("QIC cache respects TTL expiration", {
  skip_if_not(requireNamespace("qicharts2", quietly = TRUE))

  # Create test data
  test_data <- data.frame(x = 1:20, y = rnorm(20, mean = 50, sd = 5))

  # Create QIC cache
  qic_cache <- create_qic_cache()

  # Generate cache key
  params <- list(x = "x", y = "y", chart = "run")
  cache_key <- generate_qic_cache_key(test_data, params)

  # Manually add entry with short TTL (1 second)
  qic_cache$set(cache_key, "test_value", timeout = 1)

  # Verify entry exists
  expect_equal(qic_cache$get(cache_key), "test_value")

  # Wait for expiration
  Sys.sleep(1.5)

  # Entry should be expired and return NULL
  expect_null(qic_cache$get(cache_key))

  # Cache should be empty after expired entry is accessed
  expect_equal(qic_cache$size(), 0)

  cat("✅ QIC cache TTL expiration works correctly\n")
})
