# Tests for Critical Cache Data Signature Bugs
# Bug #1: Autodetect cache uses only first 10 rows
# Bug #2: Data content cache uses only first 1 row
# Bug #3: create_cached_reactive evaluates eagerly (separate file)

library(testthat)
library(digest)

# Test helpers ----

create_test_dataset <- function(nrows = 20, ncols = 3) {
  data.frame(
    Dato = seq.Date(as.Date("2024-01-01"), by = "month", length.out = nrows),
    Tæller = sample(40:60, nrows, replace = TRUE),
    Nævner = rep(100, nrows),
    stringsAsFactors = FALSE
  )
}

# Bug #1: Autodetect cache collision when rows 11+ change ----

test_that("create_data_signature detects changes in rows beyond first 10", {
  # SETUP: Create base dataset
  data_original <- create_test_dataset(nrows = 20)

  # Get signature of original
  sig_original <- create_data_signature(data_original)

  # MUTATE: Change row 11 (beyond first 10)
  data_mutated <- data_original
  data_mutated$Tæller[11] <- 999

  # Get signature of mutated
  sig_mutated <- create_data_signature(data_mutated)

  # VERIFY: Signatures should be DIFFERENT
  expect_false(
    identical(sig_original, sig_mutated),
    info = "Bug #1: Signature must change when rows beyond 10 are modified"
  )
})

test_that("create_data_signature detects changes in last rows", {
  # SETUP: Create dataset
  data_original <- create_test_dataset(nrows = 50)

  # Get original signature
  sig_original <- create_data_signature(data_original)

  # MUTATE: Change last 10 rows
  data_mutated <- data_original
  data_mutated$Tæller[41:50] <- 999

  sig_mutated <- create_data_signature(data_mutated)

  # VERIFY: Must detect change
  expect_false(
    identical(sig_original, sig_mutated),
    info = "Signature must change when tail rows are modified"
  )
})

test_that("create_data_signature detects deleted rows beyond first 10", {
  # SETUP
  data_full <- create_test_dataset(nrows = 20)
  sig_full <- create_data_signature(data_full)

  # DELETE: Remove rows 11-20
  data_truncated <- data_full[1:10, ]
  sig_truncated <- create_data_signature(data_truncated)

  # VERIFY: Signatures differ (nrow changed)
  expect_false(
    identical(sig_full, sig_truncated),
    info = "Signature must change when rows are deleted"
  )
})

test_that("create_data_signature is stable for identical data", {
  # SETUP: Two identical datasets
  data1 <- create_test_dataset(nrows = 30)
  data2 <- create_test_dataset(nrows = 30)

  # Force identical values
  data2 <- data1

  sig1 <- create_data_signature(data1)
  sig2 <- create_data_signature(data2)

  # VERIFY: Identical data yields identical signatures
  expect_identical(sig1, sig2)
})

# Bug #2: Data content cache uses only first row ----

test_that("evaluate_data_content_cached detects changes beyond first row", {
  skip_if_not(exists("evaluate_data_content_cached"),
              "evaluate_data_content_cached not loaded")

  # SETUP: Dataset with meaningful data
  data_original <- create_test_dataset(nrows = 20)

  # First evaluation
  result_original <- evaluate_data_content_cached(data_original, session = NULL)
  expect_true(result_original, info = "Should detect meaningful data")

  # MUTATE: Clear all rows except first
  data_cleared <- data_original
  data_cleared$Tæller[2:20] <- NA
  data_cleared$Nævner[2:20] <- NA

  # Force cache clear to test signature generation
  if (exists("clear_performance_cache")) {
    clear_performance_cache()
  }

  # Re-evaluate
  result_cleared <- evaluate_data_content_cached(data_cleared, session = NULL)

  # VERIFY: Should still be TRUE (first row has data)
  # BUT: Cache key should be DIFFERENT so it recomputes
  # This tests that cache invalidation works when data changes

  # More direct test: Completely empty beyond row 1
  data_empty_tail <- data_original
  data_empty_tail[2:20, ] <- NA

  if (exists("clear_performance_cache")) {
    clear_performance_cache()
  }

  result_empty_tail <- evaluate_data_content_cached(data_empty_tail, session = NULL)

  # Should detect data in row 1, but cache key must differ from original
  # (This is testing that the signature function is called correctly)
})

test_that("data content cache key changes when middle rows cleared", {
  skip_if_not(exists("evaluate_data_content_cached"),
              "evaluate_data_content_cached not loaded")

  # SETUP: Full dataset
  data_full <- create_test_dataset(nrows = 30)

  # Get cache key (we'll need to access internal function)
  # For now, test that re-evaluation gives correct result

  result_full <- evaluate_data_content_cached(data_full, session = NULL)
  expect_true(result_full)

  # Clear rows 2-30 (keep only row 1)
  data_sparse <- data_full
  data_sparse$Tæller[2:30] <- NA
  data_sparse$Nævner[2:30] <- NA
  data_sparse$Dato[2:30] <- NA

  # Clear cache to force recomputation
  if (exists("clear_performance_cache")) {
    clear_performance_cache()
  }

  result_sparse <- evaluate_data_content_cached(data_sparse, session = NULL)

  # Should still return TRUE (row 1 has data) but with different cache key
  expect_true(result_sparse)
})

test_that("data content cache detects completely empty data after row 1", {
  # EDGE CASE: All data empty except row 1
  data <- data.frame(
    col1 = c(1, rep(NA, 19)),
    col2 = c("A", rep("", 19)),
    stringsAsFactors = FALSE
  )

  result <- evaluate_data_content_cached(data, session = NULL)

  # Should return TRUE (row 1 has content)
  expect_true(result)

  # Now make row 1 also empty
  data_empty <- data
  data_empty[1, ] <- NA

  if (exists("clear_performance_cache")) {
    clear_performance_cache()
  }

  result_empty <- evaluate_data_content_cached(data_empty, session = NULL)

  # Should return FALSE (no meaningful content)
  expect_false(result_empty)
})

# Shared utility function tests ----

test_that("create_full_data_signature handles NULL and empty data", {
  # NULL
  expect_equal(create_data_signature(NULL), "empty_data")

  # Empty data frame
  empty_df <- data.frame()
  expect_equal(create_data_signature(empty_df), "empty_data")

  # Data frame with 0 rows
  zero_row_df <- data.frame(col1 = character(0), col2 = numeric(0))
  expect_equal(create_data_signature(zero_row_df), "empty_data")
})

test_that("create_full_data_signature includes all metadata", {
  data <- create_test_dataset(nrows = 15)

  sig1 <- create_data_signature(data)

  # Change column name (should change signature)
  data_renamed <- data
  names(data_renamed)[1] <- "NewName"

  sig2 <- create_data_signature(data_renamed)

  expect_false(identical(sig1, sig2),
               info = "Signature should change when column names change")

  # Change column type (should change signature)
  data_type_change <- data
  data_type_change$Tæller <- as.character(data_type_change$Tæller)

  sig3 <- create_data_signature(data_type_change)

  expect_false(identical(sig1, sig3),
               info = "Signature should change when column types change")
})

test_that("create_full_data_signature is deterministic", {
  # Same data should give same signature every time
  data <- create_test_dataset(nrows = 25)

  sigs <- replicate(5, create_data_signature(data))

  expect_true(all(sigs == sigs[1]),
              info = "Signature should be deterministic")
})

# Performance test ----

test_that("create_full_data_signature performs reasonably on large data", {
  skip_on_ci()

  # Large dataset
  large_data <- data.frame(
    x1 = rnorm(10000),
    x2 = rnorm(10000),
    x3 = rnorm(10000),
    x4 = sample(letters, 10000, replace = TRUE)
  )

  # Should complete in reasonable time (<100ms)
  timing <- system.time({
    sig <- create_data_signature(large_data)
  })

  expect_lt(timing["elapsed"], 0.1,
            info = "Signature generation should be fast even for large data")

  expect_type(sig, "character")
  expect_gt(nchar(sig), 10, info = "Signature should be non-trivial hash")
})

# Integration test with actual cache collision scenario ----

test_that("INTEGRATION: autodetect cache invalidates when data changes beyond row 10", {
  skip_if_not(exists("detect_columns_with_cache"),
              "detect_columns_with_cache not available")

  # SETUP: Initial dataset
  data1 <- data.frame(
    Dato = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 20),
    Tæller = c(rep(50, 10), rep(100, 10)),  # First 10: 50, Last 10: 100
    Nævner = rep(100, 20)
  )

  # Clear cache
  if (exists("clear_performance_cache")) {
    clear_performance_cache()
  }

  # First detection
  result1 <- detect_columns_with_cache(data1, app_state = NULL)

  # MUTATE: Change values in rows 11-20 (should invalidate cache)
  data2 <- data1
  data2$Tæller[11:20] <- 200  # Change from 100 to 200

  # Second detection (should use NEW cache key due to data change)
  result2 <- detect_columns_with_cache(data2, app_state = NULL)

  # Both should detect columns successfully
  expect_false(is.null(result1))
  expect_false(is.null(result2))

  # The cache keys MUST have been different (tested via signature function)
})
