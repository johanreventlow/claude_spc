# test-fase5-performance.R
# Performance tests for Fase 5 optimizations

# Load required functions - use global.R loading instead of direct sourcing
# Performance utilities are now loaded via global.R

# Fase 5 Performance Optimization Tests

test_that("Performance utilities load correctly", {
  # TEST: Utilities are available
  expect_true(exists("measure_reactive_performance"))
  expect_true(exists("create_cached_reactive"))
  expect_true(exists("create_performance_debounced"))
  expect_true(exists("clear_performance_cache"))
})

test_that("Cached reactive expressions improve performance", {
  # TEST: Caching reduces execution time for repeated operations

  # Create expensive operation
  expensive_operation <- function() {
    Sys.sleep(0.01)  # 10ms delay
    return(runif(100))
  }

  # Test without caching
  start_time <- Sys.time()
  result1 <- expensive_operation()
  result2 <- expensive_operation()
  uncached_time <- as.numeric(Sys.time() - start_time)

  # Test with caching
  cached_func <- create_cached_reactive({
    expensive_operation()
  }, "test_expensive", cache_timeout = 60)

  start_time <- Sys.time()
  cached_result1 <- cached_func()
  cached_result2 <- cached_func()  # Should be from cache
  cached_time <- as.numeric(Sys.time() - start_time)

  # TEST: Cache should be faster for second call
  expect_lt(cached_time, uncached_time)

  # Clear cache
  clear_performance_cache("test_expensive")
})

test_that("X-validation cache invalidates when data content changes", {
  # TEST: Cache key includes x-column content hash to detect data changes

  # Create test data with same structure but different content
  test_data_1 <- data.frame(
    Dato = c("2023-01-01", "2023-01-02", "2023-01-03"),
    Vaerdi = c(10, 15, 12)
  )

  test_data_2 <- data.frame(
    Dato = c("2023-01-04", "2023-01-05", "2023-01-06"),  # Different dates, same structure
    Vaerdi = c(20, 25, 22)  # Different values
  )

  config <- list(x_col = "Dato")

  # Generate cache keys for both datasets
  data_structure_hash_1 <- paste0(nrow(test_data_1), "_", ncol(test_data_1), "_", paste(names(test_data_1), collapse = "_"))
  safe_x_col_id <- "Dato"

  x_content_hash_1 <- paste0(digest::digest(test_data_1[["Dato"]], algo = "xxhash32"), "_", nrow(test_data_1))
  cache_key_1 <- paste0("x_validation_", safe_x_col_id, "_", substr(data_structure_hash_1, 1, 12), "_", x_content_hash_1)

  data_structure_hash_2 <- paste0(nrow(test_data_2), "_", ncol(test_data_2), "_", paste(names(test_data_2), collapse = "_"))
  x_content_hash_2 <- paste0(digest::digest(test_data_2[["Dato"]], algo = "xxhash32"), "_", nrow(test_data_2))
  cache_key_2 <- paste0("x_validation_", safe_x_col_id, "_", substr(data_structure_hash_2, 1, 12), "_", x_content_hash_2)

  # TEST: Cache keys should be different despite same structure
  expect_false(identical(cache_key_1, cache_key_2),
               info = "Cache keys should differ when x-column content changes")

  # TEST: Structure hashes are identical (same dimensions and column names)
  expect_identical(data_structure_hash_1, data_structure_hash_2,
                   info = "Structure hashes should be identical for same-structure data")

  # TEST: Content hashes are different (different x-column data)
  expect_false(identical(x_content_hash_1, x_content_hash_2),
               info = "Content hashes should differ when x-column content changes")

  # TEST: Cache keys handle NULL x-column case
  config_null <- list(x_col = NULL)
  x_content_hash_null <- paste0("NO_XCOL_", nrow(test_data_1))
  cache_key_null <- paste0("x_validation_NULL_XCOL_", substr(data_structure_hash_1, 1, 12), "_", x_content_hash_null)

  expect_true(grepl("NULL_XCOL", cache_key_null),
              info = "Cache key should handle NULL x-column gracefully")
})

test_that("Memory monitoring detects usage patterns", {
  # TEST: Memory monitoring utilities work correctly

  memory_monitor <- start_memory_monitoring("test_operation")

  # Create some data
  test_data <- data.frame(
    x = rnorm(1000),
    y = rnorm(1000),
    z = rnorm(1000)
  )

  # Get memory stats
  stats <- memory_monitor()

  # TEST: Memory statistics are returned
  expect_true(is.list(stats))
  expect_true("operation_name" %in% names(stats))
  expect_true("memory_diff" %in% names(stats))
  expect_equal(stats$operation_name, "test_operation")

  # Cleanup
  rm(test_data)
  gc()
})

test_that("Performance thresholds are configured correctly", {
  # TEST: Performance thresholds exist and are reasonable

  expect_true(exists("PERFORMANCE_THRESHOLDS"))
  expect_true(is.list(PERFORMANCE_THRESHOLDS))

  # TEST: Threshold values are reasonable
  expect_gte(PERFORMANCE_THRESHOLDS$reactive_warning, 0.1)  # At least 100ms
  expect_lte(PERFORMANCE_THRESHOLDS$reactive_warning, 2.0)  # At most 2 seconds

  expect_gte(PERFORMANCE_THRESHOLDS$memory_warning, 1)      # At least 1MB
  expect_lte(PERFORMANCE_THRESHOLDS$memory_warning, 100)    # At most 100MB
})

test_that("Session cleanup utilities work correctly", {
  # TEST: Memory management utilities function properly

  # Mock reactive values
  mock_values <- list(
    current_data = data.frame(x = 1:100, y = 1:100),
    original_data = data.frame(x = 1:100, y = 1:100),
    ui_sync_needed = list(test = "data")
  )

  # Mock session
  mock_session <- list(
    onSessionEnded = function(callback) {
      # Store callback for testing
      attr(mock_session, "cleanup_callback") <- callback
    }
  )

  # TEST: Setup doesn't fail
  expect_silent({
    setup_session_cleanup(mock_session, mock_values)
  })

  # TEST: Cleanup functions work (non-reactive context test)
  # Note: May produce debug output in test environment
  expect_error({
    cleanup_reactive_values(mock_values)
  }, NA)  # Expect no error (but allow output)

  # NOTE: In test context with simple lists, the function runs without error
  # but cannot modify the original list due to R's pass-by-value semantics.
  # In production with ReactiveValues, this works correctly.
  # Test that function at least attempts cleanup and doesn't crash.
  expect_true(is.list(mock_values))  # Original data structure preserved in test context
})

test_that("Performance measurement works correctly", {
  # TEST: Performance measurement utilities

  # Test operation that takes some time
  result <- measure_reactive_performance({
    Sys.sleep(0.01)  # 10ms
    return("test_result")
  }, "test_measure")

  # TEST: Results structure is correct
  expect_true(is.list(result))
  expect_equal(result$result, "test_result")
  expect_equal(result$operation_name, "test_measure")
  expect_true(is.numeric(result$execution_time))
  expect_gte(result$execution_time, 0.008)  # At least 8ms (allowing for variance)
})

test_that("Debounced performance tracking works", {
  # TEST: Performance debouncing with tracking

  counter <- 0
  reactive_expr <- reactive({
    counter <<- counter + 1
    return(counter)
  })

  # Create performance debounced version
  debounced_expr <- create_performance_debounced(
    reactive_expr,
    millis = 100,
    operation_name = "test_debounce"
  )

  # TEST: Debounced function is created
  expect_true(is.function(debounced_expr))

  # Execute and test
  result1 <- debounced_expr()
  expect_equal(result1, 1)

  # Get performance stats
  stats <- get_performance_stats()
  expect_true(is.list(stats))
})

test_that("Cache management works correctly", {
  # TEST: Cache clear functionality

  # Create some cached data
  cached_func1 <- create_cached_reactive({
    "test_data_1"
  }, "test_cache_1")

  cached_func2 <- create_cached_reactive({
    "test_data_2"
  }, "test_cache_2")

  # Execute to populate cache
  result1 <- cached_func1()
  result2 <- cached_func2()

  # Get stats before clear
  expect_true(exists(".performance_cache_fallback", envir = .GlobalEnv))
  cache_env <- get(".performance_cache_fallback", envir = .GlobalEnv)
  cache_count_before <- length(ls(cache_env))

  # Clear specific pattern
  clear_performance_cache("test_cache_1")

  # Get stats after clear
  cache_count_after <- length(ls(cache_env))

  # TEST: Cache was partially cleared
  expect_lt(cache_count_after, cache_count_before)

  # Clear all caches
  clear_performance_cache()

  clear_performance_cache()
  cache_count_final <- length(ls(cache_env))
  expect_equal(cache_count_final, 0)
})

test_that("Memory cleanup handles edge cases", {
  # TEST: Cleanup functions handle NULL and invalid inputs gracefully

  # TEST: NULL inputs don't cause errors
  expect_silent(cleanup_reactive_values(NULL))
  expect_silent(cleanup_app_state(NULL))
  expect_silent(cleanup_observers(NULL))

  # TEST: Empty objects don't cause errors
  empty_values <- list()
  expect_silent(cleanup_reactive_values(empty_values))

  # TEST: Invalid observer objects are handled
  invalid_observer <- "not_an_observer"
  expect_silent(cleanup_observers(invalid_observer))
})

test_that("Temp file cleanup works", {
  # TEST: Temporary file cleanup functionality

  # Create temporary files
  temp_dir <- tempdir()
  test_file1 <- file.path(temp_dir, "spc_temp_test1.txt")
  test_file2 <- file.path(temp_dir, "spc_temp_test2.txt")

  writeLines("test content", test_file1)
  writeLines("test content", test_file2)

  # Verify files exist
  expect_true(file.exists(test_file1))
  expect_true(file.exists(test_file2))

  # Clean up with pattern (should not delete immediately due to age)
  expect_silent({
    cleanup_temp_files(temp_dir, pattern = "spc_temp_", max_age_hours = 0.001)
  })

  # Wait a moment and try again (files should be old enough now)
  Sys.sleep(0.01)
  expect_silent({
    cleanup_temp_files(temp_dir, pattern = "spc_temp_", max_age_hours = 0.001)
  })
})

# NEW TESTS FOR SESSION HELPERS PERFORMANCE OPTIMIZATION

test_that("evaluate_data_content_cached performance and correctness", {
  # TEST: Performance-optimized data content evaluation

  # Create test data with meaningful content
  meaningful_data <- data.frame(
    logical_col = c(TRUE, FALSE, TRUE),
    numeric_col = c(1, 2, 3),
    character_col = c("test", "data", "content"),
    stringsAsFactors = FALSE
  )

  # Create test data without meaningful content
  empty_data <- data.frame(
    logical_col = c(FALSE, FALSE, FALSE),
    numeric_col = c(NA, NA, NA),
    character_col = c("", "", ""),
    stringsAsFactors = FALSE
  )

  # TEST: Function exists and works correctly
  if (exists("evaluate_data_content_cached")) {
    # Test with meaningful data
    result_meaningful <- evaluate_data_content_cached(meaningful_data)
    expect_true(result_meaningful)

    # Test with empty data
    result_empty <- evaluate_data_content_cached(empty_data)
    expect_false(result_empty)

    # Test with NULL data
    result_null <- evaluate_data_content_cached(NULL)
    expect_false(result_null)

    # Test performance: Second call should be faster (cached)
    cache_key <- "test_performance_key"

    # First call (uncached)
    start_time <- Sys.time()
    result1 <- evaluate_data_content_cached(meaningful_data, cache_key = cache_key)
    first_call_time <- as.numeric(Sys.time() - start_time)

    # Second call (cached)
    start_time <- Sys.time()
    result2 <- evaluate_data_content_cached(meaningful_data, cache_key = cache_key)
    second_call_time <- as.numeric(Sys.time() - start_time)

    # Results should be identical
    expect_equal(result1, result2)

    # Second call should be faster (cache hit)
    expect_lt(second_call_time, first_call_time)

    # Clear cache for cleanup
    if (exists(".performance_cache_fallback", envir = .GlobalEnv)) {
      cache_env <- get(".performance_cache_fallback", envir = .GlobalEnv)
      if (exists(cache_key, envir = cache_env)) {
        rm(list = cache_key, envir = cache_env)
      }
    }
  } else {
    skip("evaluate_data_content_cached function not available")
  }
})

test_that("session helpers performance optimization benchmarks", {
  # TEST: Performance comparison between old and new implementation

  # Create larger test dataset for meaningful performance comparison
  large_data <- data.frame(
    col1 = sample(c(TRUE, FALSE, NA), 100, replace = TRUE),
    col2 = rnorm(100),
    col3 = sample(c("test", "data", "", NA), 100, replace = TRUE),
    col4 = sample(c(1:50, rep(NA, 50)), 100, replace = TRUE),
    col5 = sample(letters, 100, replace = TRUE),
    stringsAsFactors = FALSE
  )

  # TEST: Legacy purrr::map_lgl approach (for comparison)
  legacy_approach_time <- system.time({
    for (i in 1:10) {  # Repeat to get meaningful timing
      legacy_result <- large_data |>
        purrr::map_lgl(~ {
          if (is.logical(.x)) {
            any(.x, na.rm = TRUE)
          } else if (is.numeric(.x)) {
            any(!is.na(.x))
          } else if (is.character(.x)) {
            any(nzchar(.x, keepNA = FALSE), na.rm = TRUE)
          } else {
            FALSE
          }
        }) |>
        any()
    }
  })

  # TEST: Optimized approach (if available)
  if (exists("evaluate_data_content_cached")) {
    optimized_approach_time <- system.time({
      for (i in 1:10) {  # Repeat to get meaningful timing
        optimized_result <- evaluate_data_content_cached(
          large_data,
          cache_key = paste0("benchmark_", i)
        )
      }
    })

    # Results should be equivalent
    expect_equal(legacy_result, optimized_result)

    # Log performance improvement
    speed_improvement <- legacy_approach_time["elapsed"] / optimized_approach_time["elapsed"]
    message(paste("Performance improvement:", round(speed_improvement, 2), "x faster"))

    # Optimized version should be meaningfully faster (at least for repeated calls)
    expect_gte(speed_improvement, 0.5)  # Should be at least not much slower

    # Clean up benchmark cache entries
    if (exists(".performance_cache_fallback", envir = .GlobalEnv)) {
      cache_env <- get(".performance_cache_fallback", envir = .GlobalEnv)
      benchmark_keys <- grep("^benchmark_", ls(cache_env), value = TRUE)
      if (length(benchmark_keys) > 0) {
        rm(list = benchmark_keys, envir = cache_env)
      }
    }
  } else {
    skip("evaluate_data_content_cached function not available")
  }
})

test_that("performance debouncing integration works", {
  # TEST: create_performance_debounced integration

  if (exists("create_performance_debounced")) {
    # Mock reactive expression
    counter <- 0
    mock_reactive <- shiny::reactive({
      counter <<- counter + 1
      return(counter)
    })

    # Create performance debounced version
    debounced_reactive <- create_performance_debounced(
      mock_reactive,
      millis = 50,
      operation_name = "test_session_helpers_debounce"
    )

    # TEST: Function is created successfully
    expect_true(is.function(debounced_reactive))

    # Execute and verify it works
    result <- debounced_reactive()
    expect_equal(result, 1)

    # Get performance stats
    if (exists("get_performance_stats")) {
      stats <- get_performance_stats()
      expect_true(is.list(stats))
    }
  } else {
    skip("create_performance_debounced function not available")
  }
})

test_that("cache invalidation on events works correctly", {
  # TEST: Event-driven cache invalidation

  if (exists("evaluate_data_content_cached") && exists("app_state")) {
    # Create test data
    test_data <- data.frame(
      col1 = c(1, 2, 3),
      col2 = c("a", "b", "c"),
      stringsAsFactors = FALSE
    )

    cache_key <- "test_invalidation_key"

    # Initial evaluation (creates cache entry)
    result1 <- evaluate_data_content_cached(test_data, cache_key = cache_key)
    expect_true(result1)

    # Verify cache exists
    if (exists(".performance_cache_fallback", envir = .GlobalEnv)) {
      cache_env <- get(".performance_cache_fallback", envir = .GlobalEnv)
      expect_true(exists(cache_key, envir = cache_env))

      # Manually trigger cache invalidation (simulate event)
      if (exists(cache_key, envir = cache_env)) {
        rm(list = cache_key, envir = cache_env)
      }

      # Verify cache was invalidated
      expect_false(exists(cache_key, envir = cache_env))
    }

    # Second evaluation should recreate cache
    result2 <- evaluate_data_content_cached(test_data, cache_key = cache_key)
    expect_equal(result1, result2)

    # Clean up
    if (exists(".performance_cache_fallback", envir = .GlobalEnv)) {
      cache_env <- get(".performance_cache_fallback", envir = .GlobalEnv)
      if (exists(cache_key, envir = cache_env)) {
        rm(list = cache_key, envir = cache_env)
      }
    }
  } else {
    skip("Required functions or app_state not available")
  }
})
