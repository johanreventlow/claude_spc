# test-qic-cache-metrics.R
# Sprint 5 Fase 2: QIC Cache Metrics and LRU Testing

library(testthat)

context("QIC Cache Metrics and LRU")

test_that("Cache tracks hits and misses correctly", {
  qic_cache <- create_qic_cache(max_size = 10)

  # Initial stats
  stats <- qic_cache$stats()
  expect_equal(stats$hits, 0)
  expect_equal(stats$misses, 0)
  expect_equal(stats$hit_rate_percent, 0)

  # First access - miss
  result1 <- qic_cache$get("key1")
  expect_true(is.null(result1))

  stats <- qic_cache$stats()
  expect_equal(stats$misses, 1)
  expect_equal(stats$hits, 0)

  # Set value
  qic_cache$set("key1", list(data = "value1"))

  # Second access - hit
  result2 <- qic_cache$get("key1")
  expect_false(is.null(result2))
  expect_equal(result2$data, "value1")

  stats <- qic_cache$stats()
  expect_equal(stats$hits, 1)
  expect_equal(stats$misses, 1)
  expect_equal(stats$hit_rate_percent, 50)
})

test_that("Cache hit rate calculation is correct", {
  qic_cache <- create_qic_cache(max_size = 10)

  # Set 3 values
  qic_cache$set("k1", "v1")
  qic_cache$set("k2", "v2")
  qic_cache$set("k3", "v3")

  # 6 hits, 2 misses
  qic_cache$get("k1")  # hit
  qic_cache$get("k1")  # hit
  qic_cache$get("k2")  # hit
  qic_cache$get("k2")  # hit
  qic_cache$get("k3")  # hit
  qic_cache$get("k3")  # hit
  qic_cache$get("k4")  # miss
  qic_cache$get("k5")  # miss

  stats <- qic_cache$stats()
  expect_equal(stats$hits, 6)
  expect_equal(stats$misses, 2)
  expect_equal(stats$total_requests, 8)
  expect_equal(stats$hit_rate_percent, 75)
})

test_that("LRU eviction works correctly", {
  qic_cache <- create_qic_cache(max_size = 3)

  # Fill cache to capacity
  qic_cache$set("key1", "value1")
  qic_cache$set("key2", "value2")
  qic_cache$set("key3", "value3")

  expect_equal(qic_cache$size(), 3)

  # Access key1 and key2 (making key3 least recently used)
  qic_cache$get("key1")
  qic_cache$get("key2")

  # Add key4 - should evict key3 (LRU)
  qic_cache$set("key4", "value4")

  expect_equal(qic_cache$size(), 3)

  # key3 should be evicted
  result_key3 <- qic_cache$get("key3")
  expect_true(is.null(result_key3))

  # key1, key2, key4 should still be present
  expect_false(is.null(qic_cache$get("key1")))
  expect_false(is.null(qic_cache$get("key2")))
  expect_false(is.null(qic_cache$get("key4")))

  # Check eviction stats
  stats <- qic_cache$stats()
  expect_equal(stats$evictions, 1)
})

test_that("Access count increments correctly", {
  qic_cache <- create_qic_cache(max_size = 10)

  qic_cache$set("key1", "value1")

  # Access multiple times
  qic_cache$get("key1")
  qic_cache$get("key1")
  qic_cache$get("key1")

  stats <- qic_cache$stats()
  expect_equal(stats$hits, 3)
})

test_that("Cache clear resets all metrics", {
  qic_cache <- create_qic_cache(max_size = 10)

  # Populate cache
  qic_cache$set("key1", "value1")
  qic_cache$set("key2", "value2")
  qic_cache$get("key1")
  qic_cache$get("key2")
  qic_cache$get("key3")  # miss

  stats_before <- qic_cache$stats()
  expect_gt(stats_before$hits, 0)
  expect_gt(stats_before$misses, 0)
  expect_gt(stats_before$size, 0)

  # Clear cache
  qic_cache$clear()

  stats_after <- qic_cache$stats()
  expect_equal(stats_after$size, 0)
  expect_equal(stats_after$hits, 0)
  expect_equal(stats_after$misses, 0)
  expect_equal(stats_after$evictions, 0)
  expect_equal(stats_after$hit_rate_percent, 0)
})

test_that("Max size is enforced", {
  max_size <- 5
  qic_cache <- create_qic_cache(max_size = max_size)

  # Add more than max_size entries
  for (i in 1:10) {
    qic_cache$set(paste0("key", i), paste0("value", i))
  }

  # Size should not exceed max_size
  expect_lte(qic_cache$size(), max_size)

  stats <- qic_cache$stats()
  expect_equal(stats$max_size, max_size)
  expect_lte(stats$size, max_size)

  # Should have evictions
  expect_gt(stats$evictions, 0)
})

test_that("Expired entries are removed on access", {
  qic_cache <- create_qic_cache(max_size = 10)

  # Set entry with very short timeout
  qic_cache$set("key1", "value1", timeout = 0.001)

  # Wait for expiration
  Sys.sleep(0.1)

  # Access should return NULL and remove expired entry
  result <- qic_cache$get("key1")
  expect_true(is.null(result))

  # Should count as miss, not hit
  stats <- qic_cache$stats()
  expect_equal(stats$misses, 1)
  expect_equal(stats$hits, 0)
})

test_that("get_qic_cache_stats returns comprehensive stats", {
  qic_cache <- create_qic_cache(max_size = 20)

  qic_cache$set("key1", "value1")
  qic_cache$set("key2", "value2")
  qic_cache$get("key1")
  qic_cache$get("key3")  # miss

  stats <- get_qic_cache_stats(qic_cache)

  expect_true(!is.null(stats$size))
  expect_true(!is.null(stats$max_size))
  expect_true(!is.null(stats$hits))
  expect_true(!is.null(stats$misses))
  expect_true(!is.null(stats$evictions))
  expect_true(!is.null(stats$hit_rate_percent))
  expect_true(!is.null(stats$total_requests))

  expect_equal(stats$max_size, 20)
  expect_equal(stats$size, 2)
  expect_equal(stats$hits, 1)
  expect_equal(stats$misses, 1)
})

test_that("LRU eviction selects least recently used entry", {
  qic_cache <- create_qic_cache(max_size = 3)

  # Add 3 entries
  qic_cache$set("key1", "value1")
  qic_cache$set("key2", "value2")
  qic_cache$set("key3", "value3")

  # Access key1 and key3 (key2 is LRU)
  qic_cache$get("key1")
  qic_cache$get("key3")

  # Add key4 - should evict key2
  qic_cache$set("key4", "value4")

  # key2 should be gone
  expect_true(is.null(qic_cache$get("key2")))

  # Others should remain
  expect_false(is.null(qic_cache$get("key1")))
  expect_false(is.null(qic_cache$get("key3")))
  expect_false(is.null(qic_cache$get("key4")))
})

test_that("Cache handles rapid successive operations", {
  qic_cache <- create_qic_cache(max_size = 100)

  # Rapid set operations
  for (i in 1:50) {
    qic_cache$set(paste0("key", i), paste0("value", i))
  }

  expect_equal(qic_cache$size(), 50)

  # Rapid get operations
  for (i in 1:50) {
    result <- qic_cache$get(paste0("key", i))
    expect_false(is.null(result))
  }

  stats <- qic_cache$stats()
  expect_equal(stats$hits, 50)
  expect_equal(stats$hit_rate_percent, 100)
})

test_that("Updating existing key does not trigger eviction", {
  qic_cache <- create_qic_cache(max_size = 3)

  # Fill cache
  qic_cache$set("key1", "value1")
  qic_cache$set("key2", "value2")
  qic_cache$set("key3", "value3")

  stats_before <- qic_cache$stats()
  evictions_before <- stats_before$evictions

  # Update existing key
  qic_cache$set("key1", "value1_updated")

  stats_after <- qic_cache$stats()

  # No eviction should occur
  expect_equal(stats_after$evictions, evictions_before)
  expect_equal(qic_cache$size(), 3)
})
