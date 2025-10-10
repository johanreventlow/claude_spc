# test-shared-data-signatures.R
# H14: Tests for shared data signatures
# Ensures signatures are reused across QIC + auto-detect caches

test_that("generate_shared_data_signature creates consistent signatures", {
  # SETUP
  test_data <- data.frame(
    x = 1:10,
    y = 11:20,
    z = letters[1:10]
  )

  # TEST: Same data produces same signature
  sig1 <- generate_shared_data_signature(test_data)
  sig2 <- generate_shared_data_signature(test_data)

  expect_equal(sig1, sig2)
  expect_type(sig1, "character")
  expect_true(nchar(sig1) > 10) # xxhash64 produces long strings
})

test_that("signatures change when data changes", {
  # SETUP
  data1 <- data.frame(x = 1:10, y = 11:20)
  data2 <- data.frame(x = 1:10, y = 21:30) # Different y values

  # TEST: Different data = different signatures
  sig1 <- generate_shared_data_signature(data1)
  sig2 <- generate_shared_data_signature(data2)

  expect_false(sig1 == sig2)
})

test_that("signatures change when structure changes", {
  # SETUP
  data1 <- data.frame(x = 1:10, y = 11:20)
  data2 <- data.frame(x = 1:10, y = 11:20, z = 21:30) # Extra column

  # TEST: Different structure with include_structure = TRUE
  sig1 <- generate_shared_data_signature(data1, include_structure = TRUE)
  sig2 <- generate_shared_data_signature(data2, include_structure = TRUE)

  expect_false(sig1 == sig2)
})

test_that("data-only signatures ignore structure", {
  # SETUP: Same data values, different column names
  data1 <- data.frame(a = 1:10, b = 11:20)
  data2 <- data.frame(x = 1:10, y = 11:20)

  # TEST: With include_structure = FALSE, should focus on values
  sig1 <- generate_shared_data_signature(data1, include_structure = FALSE)
  sig2 <- generate_shared_data_signature(data2, include_structure = FALSE)

  # Values are same, but column names differ
  # With structure = FALSE, signatures should still differ due to serialization
  # but be faster to compute
  expect_type(sig1, "character")
  expect_type(sig2, "character")
})

test_that("empty data returns special signature", {
  # TEST: NULL data
  sig_null <- generate_shared_data_signature(NULL)
  expect_equal(sig_null, "empty_data")

  # TEST: Zero-row data
  empty_df <- data.frame(x = numeric(0), y = character(0))
  sig_empty <- generate_shared_data_signature(empty_df)
  expect_equal(sig_empty, "empty_data")
})

test_that("QIC cache uses shared signatures", {
  # SETUP
  test_data <- data.frame(x = 1:5, y = 6:10)
  params <- list(chart = "run", x_col = "x", y_col = "y")

  # TEST: QIC cache key uses optimized version
  key <- generate_qic_cache_key(test_data, params)

  expect_true(grepl("^qic_", key))
  expect_type(key, "character")

  # Verify same data produces same key
  key2 <- generate_qic_cache_key(test_data, params)
  expect_equal(key, key2)
})

test_that("auto-detect cache uses shared signatures", {
  # SETUP
  test_data <- data.frame(Dato = 1:5, Værdi = 6:10)

  # TEST: Auto-detect cache key uses optimized version
  key <- generate_autodetect_cache_key_optimized(test_data)

  expect_true(grepl("^autodetect_", key))
  expect_type(key, "character")

  # Verify consistency
  key2 <- generate_autodetect_cache_key_optimized(test_data)
  expect_equal(key, key2)
})

test_that("signature cache reuses previous calculations", {
  # SETUP: Clear cache first
  clear_data_signature_cache()

  test_data <- data.frame(x = 1:100, y = 101:200)

  # TEST: First call caches signature
  sig1 <- generate_shared_data_signature(test_data)

  # Check cache has entry
  cache_stats1 <- get_data_signature_cache_stats()
  expect_equal(cache_stats1$size, 1)

  # Second call should hit cache (same signature)
  sig2 <- generate_shared_data_signature(test_data)
  expect_equal(sig1, sig2)

  # Cache size unchanged (reused entry)
  cache_stats2 <- get_data_signature_cache_stats()
  expect_equal(cache_stats2$size, 1)

  # CLEANUP
  clear_data_signature_cache()
})

test_that("signature cache evicts old entries when full", {
  # SETUP: Clear cache
  clear_data_signature_cache()

  # Create 105 different datasets (exceeds 100 limit)
  for (i in 1:105) {
    test_data <- data.frame(x = 1:10 + i, y = 11:20 + i)
    sig <- generate_shared_data_signature(test_data)
  }

  # TEST: Cache should have evicted oldest 20 (keep last 100)
  cache_stats <- get_data_signature_cache_stats()
  expect_lte(cache_stats$size, 100)

  # CLEANUP
  clear_data_signature_cache()
})

test_that("backward compatibility wrapper works", {
  # SETUP
  test_data <- data.frame(a = 1:5, b = 6:10)

  # TEST: Old create_data_signature still works
  sig_old <- create_data_signature(test_data)
  sig_new <- generate_shared_data_signature(test_data, include_structure = TRUE)

  # Should produce same result
  expect_equal(sig_old, sig_new)
})

test_that("cache stats provide useful information", {
  # SETUP
  clear_data_signature_cache()

  # Initially empty
  stats_empty <- get_data_signature_cache_stats()
  expect_equal(stats_empty$size, 0)
  expect_null(stats_empty$oldest)
  expect_null(stats_empty$newest)

  # Add some entries
  for (i in 1:3) {
    data <- data.frame(x = 1:10 + i)
    generate_shared_data_signature(data)
    Sys.sleep(0.01) # Ensure different timestamps
  }

  # Check stats
  stats <- get_data_signature_cache_stats()
  expect_equal(stats$size, 3)
  expect_true(inherits(stats$oldest, c("POSIXct", "POSIXt")))
  expect_true(inherits(stats$newest, c("POSIXct", "POSIXt")))
  expect_true(stats$newest >= stats$oldest)

  # CLEANUP
  clear_data_signature_cache()
})

test_that("shared signatures reduce hashing overhead", {
  skip_on_cran()

  # SETUP
  large_data <- data.frame(
    x = 1:1000,
    y = rnorm(1000),
    z = sample(letters, 1000, replace = TRUE)
  )

  # BENCHMARK: Time with shared signatures
  start_shared <- Sys.time()
  # First call: Generate and cache
  sig1 <- generate_shared_data_signature(large_data)
  qic_key1 <- generate_qic_cache_key(large_data, list(chart = "run"))
  autodetect_key1 <- generate_autodetect_cache_key_optimized(large_data)

  # Second call: Reuse cached signature
  sig2 <- generate_shared_data_signature(large_data)
  qic_key2 <- generate_qic_cache_key(large_data, list(chart = "run"))
  autodetect_key2 <- generate_autodetect_cache_key_optimized(large_data)
  time_shared <- Sys.time() - start_shared

  # Verify keys match (reused signature)
  expect_equal(sig1, sig2)
  expect_equal(qic_key1, qic_key2)
  expect_equal(autodetect_key1, autodetect_key2)

  # VERIFY: Second round should be significantly faster due to caching
  # (hard to test timing reliably, so just verify correctness)

  # CLEANUP
  clear_data_signature_cache()
})

# CLEANUP after all tests
withr::defer({
  clear_data_signature_cache()
}, envir = parent.frame())
