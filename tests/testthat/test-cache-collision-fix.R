# test-cache-collision-fix.R
# Tests for process-specific cache namespace fix

test_that("create_cached_reactive uses process-specific cache namespace", {
  # TEST: Cache isolation between processes/contexts

  # Create cached function
  cached_func <- create_cached_reactive({
    Sys.time()
  }, "process_test_cache")

  # First call - should create process-specific cache
  result1 <- cached_func()

  # Verify process-specific cache was created
  process_cache_pattern <- paste0(".performance_cache_fallback_", Sys.getpid(), "_")
  existing_caches <- ls(pattern = process_cache_pattern, envir = .GlobalEnv, all.names = TRUE)

  expect_true(length(existing_caches) > 0,
              info = "Process-specific cache should be created")

  # Verify cache name contains process ID
  cache_name <- existing_caches[1]
  expect_true(grepl(paste0("_", Sys.getpid(), "_"), cache_name),
              info = "Cache name should contain process ID")

  # Second call - should hit cache
  result2 <- cached_func()
  expect_identical(result1, result2,
                   info = "Second call should return cached result")

  # Clean up
  if (length(existing_caches) > 0) {
    rm(list = existing_caches, envir = .GlobalEnv)
  }
})

test_that("get_session_cache creates process-specific fallback", {
  # TEST: Session cache helper uses same process isolation

  # Get cache without session (should create process-specific)
  cache_env1 <- get_session_cache(session = NULL)
  cache_env2 <- get_session_cache(session = NULL)

  # Both calls should return same environment for same process
  expect_identical(cache_env1, cache_env2,
                   info = "Same process should reuse cache environment")

  # Verify it's process-specific
  process_cache_pattern <- paste0(".performance_cache_fallback_", Sys.getpid(), "_")
  existing_caches <- ls(pattern = process_cache_pattern, envir = .GlobalEnv, all.names = TRUE)

  expect_true(length(existing_caches) > 0,
              info = "Process-specific cache should exist")

  # Clean up
  if (length(existing_caches) > 0) {
    rm(list = existing_caches, envir = .GlobalEnv)
  }
})

test_that("clear_performance_cache handles process-specific caches", {
  # TEST: Cache clearing works with process-specific namespaces

  # Create some cached functions
  cached_func1 <- create_cached_reactive({
    runif(1)
  }, "clear_test_1")

  cached_func2 <- create_cached_reactive({
    runif(1)
  }, "clear_test_2")

  # Execute to populate cache
  result1 <- cached_func1()
  result2 <- cached_func2()

  # Get process-specific cache
  process_cache_pattern <- paste0(".performance_cache_fallback_", Sys.getpid(), "_")
  existing_caches <- ls(pattern = process_cache_pattern, envir = .GlobalEnv, all.names = TRUE)

  if (length(existing_caches) > 0) {
    cache_env <- get(existing_caches[1], envir = .GlobalEnv)
    cache_count_before <- length(ls(cache_env))

    expect_true(cache_count_before >= 2,
                info = "Cache should contain our test entries")

    # Clear specific pattern
    clear_performance_cache("clear_test_1")

    # Verify selective clearing worked
    cache_count_after <- length(ls(cache_env))
    expect_lt(cache_count_after, cache_count_before)
    # Cache count should decrease after pattern clear

    # Clean up remaining
    clear_performance_cache()

    # Clean up cache environments
    rm(list = existing_caches, envir = .GlobalEnv)
  } else {
    skip("No process-specific cache created")
  }
})

test_that("concurrent cache access doesn't collide", {
  # TEST: Multiple cache keys don't interfere in same process

  # Create multiple cached functions with different keys
  cached_func_a <- create_cached_reactive({
    paste0("result_a_", sample(1:1000, 1))
  }, "concurrent_test_a")

  cached_func_b <- create_cached_reactive({
    paste0("result_b_", sample(1:1000, 1))
  }, "concurrent_test_b")

  # Execute both
  result_a1 <- cached_func_a()
  result_b1 <- cached_func_b()

  # Second calls should return cached values
  result_a2 <- cached_func_a()
  result_b2 <- cached_func_b()

  # Verify caching worked independently
  expect_identical(result_a1, result_a2,
                   info = "Function A should return cached result")
  expect_identical(result_b1, result_b2,
                   info = "Function B should return cached result")

  # Results should be different between functions
  expect_false(identical(result_a1, result_b1),
               info = "Different functions should have different results")

  # Clean up
  clear_performance_cache()
  process_cache_pattern <- paste0(".performance_cache_fallback_", Sys.getpid(), "_")
  existing_caches <- ls(pattern = process_cache_pattern, envir = .GlobalEnv, all.names = TRUE)
  if (length(existing_caches) > 0) {
    rm(list = existing_caches, envir = .GlobalEnv)
  }
})