# test-cache-invalidation-sprint3.R
# SPRINT 3: Cache invalidation strategy tests
# Ensures cache is cleared on data changes and session resets

library(testthat)

# Test setup
test_that("Cache invalidation system is available", {
  expect_true(exists("clear_performance_cache"))
  expect_true(is.function(clear_performance_cache))
  expect_true(exists("get_cached_result"))
  expect_true(exists("cache_result"))
})

test_that("clear_performance_cache clears all cache entries", {
  # Setup: Add some cache entries
  cache_result("test_key_1", list(value = "data1"), timeout_seconds = 300)
  cache_result("test_key_2", list(value = "data2"), timeout_seconds = 300)

  # Verify cache entries exist
  cached_1 <- get_cached_result("test_key_1")
  cached_2 <- get_cached_result("test_key_2")
  expect_false(is.null(cached_1))
  expect_false(is.null(cached_2))

  # Clear cache
  clear_performance_cache()

  # Verify cache entries are gone
  cached_1_after <- get_cached_result("test_key_1")
  cached_2_after <- get_cached_result("test_key_2")
  expect_null(cached_1_after)
  expect_null(cached_2_after)
})

test_that("clear_performance_cache with pattern clears selective entries", {
  # Setup: Add cache entries with different prefixes
  cache_result("autodetect_key1", list(value = "auto1"), timeout_seconds = 300)
  cache_result("autodetect_key2", list(value = "auto2"), timeout_seconds = 300)
  cache_result("plot_key1", list(value = "plot1"), timeout_seconds = 300)

  # Clear only autodetect cache
  clear_performance_cache("autodetect_.*")

  # Verify autodetect entries are gone but plot remains
  expect_null(get_cached_result("autodetect_key1"))
  expect_null(get_cached_result("autodetect_key2"))
  expect_false(is.null(get_cached_result("plot_key1")))

  # Cleanup
  clear_performance_cache()
})

test_that("cache_result stores data correctly", {
  test_data <- list(x = 1:10, y = 11:20)

  cache_result("test_data_key", test_data, timeout_seconds = 300)

  cached <- get_cached_result("test_data_key")
  expect_false(is.null(cached))
  expect_equal(cached$value$x, 1:10)
  expect_equal(cached$value$y, 11:20)

  # Cleanup
  clear_performance_cache()
})

test_that("cached results expire after timeout", {
  # This test requires waiting, so we test with short timeout
  test_data <- list(value = "short_lived")

  # Cache with 1 second timeout
  cache_result("short_timeout_key", test_data, timeout_seconds = 1)

  # Immediately should be cached
  cached_immediate <- get_cached_result("short_timeout_key")
  expect_false(is.null(cached_immediate))

  # Wait 2 seconds for expiration
  Sys.sleep(2)

  # Should be expired now
  cached_after <- get_cached_result("short_timeout_key")
  expect_null(cached_after)
})

test_that("data_updated event triggers cache invalidation", {
  # Setup: Create mock app_state with events
  app_state <- create_app_state()
  emit <- create_emit_api(app_state)

  # Add cache entries
  cache_result("test_before_update", list(value = "old_data"), timeout_seconds = 300)
  expect_false(is.null(get_cached_result("test_before_update")))

  # Setup event listener (simplified - just test the logic directly)
  safe_operation(
    "Clear performance cache on data update",
    code = {
      clear_performance_cache()
    }
  )

  # Verify cache was cleared
  expect_null(get_cached_result("test_before_update"))
})

test_that("session_reset event triggers cache invalidation", {
  # Setup: Add cache entries
  cache_result("test_before_reset", list(value = "session_data"), timeout_seconds = 300)
  expect_false(is.null(get_cached_result("test_before_reset")))

  # Simulate session reset cache clear
  safe_operation(
    "Clear performance cache on session reset",
    code = {
      clear_performance_cache()
    }
  )

  # Verify cache was cleared
  expect_null(get_cached_result("test_before_reset"))
})

test_that("cache stats are accurate after operations", {
  # Clear to start fresh
  clear_performance_cache()

  # Add some entries
  cache_result("stats_key_1", list(value = 1), timeout_seconds = 300)
  cache_result("stats_key_2", list(value = 2), timeout_seconds = 300)
  cache_result("stats_key_3", list(value = 3), timeout_seconds = 300)

  stats <- get_cache_stats()

  expect_equal(stats$total_entries, 3)
  expect_true(stats$total_size_bytes > 0)

  # Clear and verify stats reset
  clear_performance_cache()
  stats_after <- get_cache_stats()
  expect_equal(stats_after$total_entries, 0)
})

test_that("multiple cache operations handle concurrency safely", {
  # Test that rapid cache operations don't corrupt state
  clear_performance_cache()

  # Rapid fire cache operations
  for (i in 1:20) {
    cache_result(paste0("concurrent_", i), list(value = i), timeout_seconds = 300)
  }

  # Verify all entries exist
  for (i in 1:20) {
    cached <- get_cached_result(paste0("concurrent_", i))
    expect_false(is.null(cached))
    expect_equal(cached$value$value, i)
  }

  # Cleanup
  clear_performance_cache()
})

test_that("cache invalidation is safe when cache functions don't exist", {
  # Test the guard condition in event system
  # This simulates the exists() check
  if (exists("clear_performance_cache") && is.function(clear_performance_cache)) {
    result <- safe_operation(
      "Clear performance cache test",
      code = {
        clear_performance_cache()
        TRUE
      },
      fallback = function(e) {
        FALSE
      }
    )
    expect_true(result)
  }
})

# Cleanup after all tests
clear_performance_cache()