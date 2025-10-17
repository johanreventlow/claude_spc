# test-spc-cache-integration.R
# Unit tests for backend-agnostic SPC cache integration
# Task 033 Stream 1: Cache abstraction for BFHchart service

library(testthat)

# Test data generator
create_test_data <- function(n_rows = 50, chart_type = "run") {
  data <- data.frame(
    date = seq.Date(Sys.Date() - n_rows + 1, Sys.Date(), by = "day"),
    value = rnorm(n_rows, mean = 100, sd = 15),
    stringsAsFactors = FALSE
  )

  # Add denominator for rate-based charts
  if (chart_type %in% c("p", "pp", "u", "up", "c")) {
    data$n <- sample(50:200, n_rows, replace = TRUE)
  }

  data
}

# ====================================================================
# Test Suite 1: Cache Key Generation
# ====================================================================

test_that("generate_spc_cache_key creates deterministic keys", {
  skip_if_not_installed("digest")

  data <- create_test_data(50, "run")
  config <- list(
    chart_type = "run",
    x_column = "date",
    y_column = "value"
  )

  # Generate key twice - should be identical
  key1 <- generate_spc_cache_key(data, config)
  key2 <- generate_spc_cache_key(data, config)

  expect_equal(key1, key2)
  expect_type(key1, "character")
  expect_true(nchar(key1) > 0)
})


test_that("generate_spc_cache_key format is correct", {
  skip_if_not_installed("digest")

  data <- create_test_data(50, "run")
  config <- list(
    chart_type = "run",
    x_column = "date",
    y_column = "value"
  )

  key <- generate_spc_cache_key(data, config)

  # Key should start with "spc_"
  expect_true(grepl("^spc_", key))

  # Key should contain chart type
  expect_true(grepl("_run_", key))

  # Key should have at least 4 parts (spc, chart_type, data_sig, config_sig)
  parts <- strsplit(key, "_")[[1]]
  expect_gte(length(parts), 4)
})


test_that("generate_spc_cache_key changes with data changes", {
  skip_if_not_installed("digest")

  data1 <- create_test_data(50, "run")
  data2 <- create_test_data(50, "run")
  data2$value <- data2$value + 10  # Change data values

  config <- list(
    chart_type = "run",
    x_column = "date",
    y_column = "value"
  )

  key1 <- generate_spc_cache_key(data1, config)
  key2 <- generate_spc_cache_key(data2, config)

  # Different data should produce different keys
  expect_false(key1 == key2)
})


test_that("generate_spc_cache_key changes with config changes", {
  skip_if_not_installed("digest")

  data <- create_test_data(50, "run")

  config1 <- list(
    chart_type = "run",
    x_column = "date",
    y_column = "value"
  )

  config2 <- list(
    chart_type = "i",  # Changed chart type
    x_column = "date",
    y_column = "value"
  )

  key1 <- generate_spc_cache_key(data, config1)
  key2 <- generate_spc_cache_key(data, config2)

  # Different config should produce different keys
  expect_false(key1 == key2)
})


test_that("generate_spc_cache_key handles optional parameters", {
  skip_if_not_installed("digest")

  data <- create_test_data(50, "p")

  config_minimal <- list(
    chart_type = "p",
    x_column = "date",
    y_column = "value",
    n_column = "n"
  )

  config_full <- list(
    chart_type = "p",
    x_column = "date",
    y_column = "value",
    n_column = "n",
    target_value = 75,
    multiply_by = 100,
    freeze_position = 12
  )

  key1 <- generate_spc_cache_key(data, config_minimal)
  key2 <- generate_spc_cache_key(data, config_full)

  # Different parameters should produce different keys
  expect_false(key1 == key2)

  # Both keys should be valid
  expect_true(grepl("^spc_p_", key1))
  expect_true(grepl("^spc_p_", key2))
})


test_that("generate_spc_cache_key validates inputs", {
  skip_if_not_installed("digest")

  data <- create_test_data(50, "run")

  # NULL data should return NULL (via safe_operation)
  result <- generate_spc_cache_key(NULL, list(chart_type = "run", x_column = "date", y_column = "value"))
  expect_null(result)

  # NULL config should return NULL (via safe_operation)
  result <- generate_spc_cache_key(data, NULL)
  expect_null(result)

  # Missing required keys should return NULL (via safe_operation)
  result <- generate_spc_cache_key(data, list(chart_type = "run"))
  expect_null(result)
})


# ====================================================================
# Test Suite 2: Cache Integration
# ====================================================================

test_that("cache_spc_result stores and retrieves results", {
  skip_if_not_installed("digest")

  # Create mock cache
  qic_cache <- create_qic_cache(max_size = 10)

  # Create test result
  test_result <- list(
    plot = "mock_plot",
    qic_data = data.frame(x = 1:10, y = 1:10),
    metadata = list(chart_type = "run", n_points = 10)
  )

  cache_key <- "spc_run_test_key_12345"

  # Store in cache
  success <- cache_spc_result(cache_key, test_result, qic_cache, ttl = 60)
  expect_true(success)

  # Retrieve from cache
  cached <- get_cached_spc_result(cache_key, qic_cache)
  expect_equal(cached$plot, "mock_plot")
  expect_equal(nrow(cached$qic_data), 10)
  expect_equal(cached$metadata$chart_type, "run")
})


test_that("get_cached_spc_result returns NULL on cache miss", {
  skip_if_not_installed("digest")

  qic_cache <- create_qic_cache(max_size = 10)

  # Try to retrieve non-existent key
  cached <- get_cached_spc_result("spc_run_nonexistent_12345", qic_cache)
  expect_null(cached)
})


test_that("cache expires after TTL", {
  skip_if_not_installed("digest")

  qic_cache <- create_qic_cache(max_size = 10)

  test_result <- list(
    plot = "mock_plot",
    qic_data = data.frame(x = 1:10, y = 1:10),
    metadata = list(chart_type = "run", n_points = 10)
  )

  cache_key <- "spc_run_test_key_ttl"

  # Store with 1 second TTL
  cache_spc_result(cache_key, test_result, qic_cache, ttl = 1)

  # Should be available immediately
  cached1 <- get_cached_spc_result(cache_key, qic_cache)
  expect_false(is.null(cached1))

  # Wait for expiration
  Sys.sleep(1.5)

  # Should be expired now
  cached2 <- get_cached_spc_result(cache_key, qic_cache)
  expect_null(cached2)
})


test_that("clear_spc_cache removes all entries", {
  skip_if_not_installed("digest")

  qic_cache <- create_qic_cache(max_size = 10)

  # Add multiple entries
  for (i in 1:5) {
    cache_key <- paste0("spc_run_test_", i)
    test_result <- list(plot = paste0("plot_", i))
    cache_spc_result(cache_key, test_result, qic_cache, ttl = 60)
  }

  # Verify entries exist
  stats_before <- get_spc_cache_stats(qic_cache)
  expect_equal(stats_before$size, 5)

  # Clear cache
  success <- clear_spc_cache(qic_cache)
  expect_true(success)

  # Verify cache is empty
  stats_after <- get_spc_cache_stats(qic_cache)
  expect_equal(stats_after$size, 0)
})


# ====================================================================
# Test Suite 3: Cache Statistics
# ====================================================================

test_that("get_spc_cache_stats returns correct metrics", {
  skip_if_not_installed("digest")

  qic_cache <- create_qic_cache(max_size = 10)

  # Initial stats
  stats <- get_spc_cache_stats(qic_cache)
  expect_equal(stats$size, 0)
  expect_equal(stats$hits, 0)
  expect_equal(stats$misses, 0)
  expect_equal(stats$hit_rate_percent, 0)

  # Add entry and access it
  cache_key <- "spc_run_test_stats"
  test_result <- list(plot = "mock_plot")
  cache_spc_result(cache_key, test_result, qic_cache, ttl = 60)

  # Cache hit
  get_cached_spc_result(cache_key, qic_cache)

  # Cache miss
  get_cached_spc_result("nonexistent_key", qic_cache)

  stats <- get_spc_cache_stats(qic_cache)
  expect_equal(stats$size, 1)
  expect_equal(stats$hits, 1)
  expect_equal(stats$misses, 1)
  expect_equal(stats$hit_rate_percent, 50)  # 1 hit out of 2 requests
})


# ====================================================================
# Test Suite 4: Cache Hit Rate Simulation
# ====================================================================

test_that("cache achieves >80% hit rate in typical workflow", {
  skip_if_not_installed("digest")

  qic_cache <- create_qic_cache(max_size = 50)

  # Simulate typical workflow with repeated requests
  data <- create_test_data(100, "run")

  configs <- list(
    run = list(chart_type = "run", x_column = "date", y_column = "value"),
    i = list(chart_type = "i", x_column = "date", y_column = "value")
  )

  # Simulate 100 requests with 80% repeats (typical pattern)
  for (i in 1:100) {
    # 80% of requests use same config (cache hits expected)
    config <- if (runif(1) < 0.8) configs$run else configs$i

    cache_key <- generate_spc_cache_key(data, config)

    # Check cache first
    cached <- get_cached_spc_result(cache_key, qic_cache)

    if (is.null(cached)) {
      # Cache miss - compute and store
      test_result <- list(
        plot = "mock_plot",
        qic_data = data.frame(x = 1:10, y = 1:10),
        metadata = list(chart_type = config$chart_type)
      )
      cache_spc_result(cache_key, test_result, qic_cache, ttl = 300)
    }
  }

  # Check hit rate
  stats <- get_spc_cache_stats(qic_cache)

  # Should achieve >80% hit rate
  expect_gte(stats$hit_rate_percent, 80)

  # Log stats for visibility
  cat("\n=== Cache Hit Rate Test ===\n")
  cat("Total requests:", stats$total_requests, "\n")
  cat("Cache hits:", stats$hits, "\n")
  cat("Cache misses:", stats$misses, "\n")
  cat("Hit rate:", stats$hit_rate_percent, "%\n")
  cat("Cache size:", stats$size, "/", stats$max_size, "\n")
})


# ====================================================================
# Test Suite 5: Collision Resistance
# ====================================================================

test_that("cache keys avoid collisions for similar configs", {
  skip_if_not_installed("digest")

  data <- create_test_data(50, "run")

  # Create similar but distinct configurations
  configs <- list(
    run_no_freeze = list(
      chart_type = "run",
      x_column = "date",
      y_column = "value"
    ),
    run_freeze_12 = list(
      chart_type = "run",
      x_column = "date",
      y_column = "value",
      freeze_position = 12
    ),
    run_freeze_13 = list(
      chart_type = "run",
      x_column = "date",
      y_column = "value",
      freeze_position = 13
    ),
    i_chart = list(
      chart_type = "i",
      x_column = "date",
      y_column = "value"
    )
  )

  # Generate keys
  keys <- lapply(configs, function(cfg) generate_spc_cache_key(data, cfg))

  # All keys should be unique
  expect_equal(length(unique(keys)), length(keys))

  # Log keys for manual inspection
  cat("\n=== Cache Key Collision Test ===\n")
  for (name in names(keys)) {
    cat(sprintf("%s: %s\n", name, substr(keys[[name]], 1, 60)))
  }
})


# ====================================================================
# Test Suite 6: Backend Independence
# ====================================================================

test_that("cache key is backend-agnostic", {
  skip_if_not_installed("digest")

  data <- create_test_data(50, "run")

  # Config without backend-specific parameters
  config <- list(
    chart_type = "run",
    x_column = "date",
    y_column = "value",
    multiply_by = 100
  )

  # Generate key
  key <- generate_spc_cache_key(data, config)

  # Key should not contain backend-specific identifiers
  expect_false(grepl("bfh", key, ignore.case = TRUE))
  expect_false(grepl("qic", key, ignore.case = TRUE))

  # Key should be deterministic regardless of backend
  # (same data + config = same key for both BFHchart and qicharts2)
  expect_true(grepl("^spc_run_", key))
})


# ====================================================================
# Test Suite 7: Error Handling
# ====================================================================

test_that("cache functions handle invalid inputs gracefully", {
  skip_if_not_installed("digest")

  qic_cache <- create_qic_cache(max_size = 10)

  # cache_spc_result with NULL key
  result <- cache_spc_result(NULL, list(plot = "test"), qic_cache)
  expect_false(result)

  # cache_spc_result with NULL result
  result <- cache_spc_result("test_key", NULL, qic_cache)
  expect_false(result)

  # get_cached_spc_result with NULL key
  cached <- get_cached_spc_result(NULL, qic_cache)
  expect_null(cached)

  # get_cached_spc_result with invalid cache
  cached <- get_cached_spc_result("test_key", NULL)
  expect_null(cached)
})
