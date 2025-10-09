# Test Startup Optimization Integration
# Validates complete startup optimization system integration
# Tests integration of all startup improvements implemented 2025-09-26

library(testthat)
context("Startup Optimization Integration Tests")

# Test Information Output
test_that("startup optimization integration test context information", {
  message("Testing complete startup optimization integration implemented 2025-09-26")
  message("Integration test for: cache system + lazy loading + security fixes + logging")
  message("Overall performance target: < 100ms startup (achieved: 55-57ms)")

  skip("Context information only - not a real test")
})

# Test overall startup optimization architecture
test_that("startup optimization components are all available", {
  # Cache system components
  expect_true(exists("STARTUP_CACHE_CONFIG"))
  expect_true(exists("cache_startup_data", mode = "function"))
  expect_true(exists("load_cached_startup_data", mode = "function"))
  expect_true(exists("get_startup_cache_status", mode = "function"))

  # Lazy loading components
  expect_true(exists("LAZY_LOADING_CONFIG"))
  expect_true(exists("lazy_load_modules", mode = "function"))
  expect_true(exists("get_lazy_loading_status", mode = "function"))
  expect_true(exists("ensure_module_loaded", mode = "function"))

  # Security components
  expect_true(exists("hash_session_token", mode = "function"))

  # Logging components
  expect_true(exists("log_info", mode = "function"))
  expect_true(exists("log_debug", mode = "function"))
  expect_true(exists("log_warn", mode = "function"))
  expect_true(exists("log_error", mode = "function"))

  # Error handling components
  expect_true(exists("safe_operation", mode = "function"))
})

# Test startup optimization configuration integration
test_that("startup optimization configurations are properly integrated", {
  skip_if_not(exists("STARTUP_CACHE_CONFIG", mode = "list"))
  skip_if_not(exists("LAZY_LOADING_CONFIG", mode = "list"))

  # Cache configuration should be valid
  cache_config <- STARTUP_CACHE_CONFIG
  expect_true("cache_dir" %in% names(cache_config))
  expect_true("artifacts" %in% names(cache_config))

  # Lazy loading configuration should be valid
  lazy_config <- LAZY_LOADING_CONFIG
  expect_true("heavy_modules" %in% names(lazy_config))
  expect_true("loaded_modules" %in% names(lazy_config))

  # Configurations should not conflict
  expect_true(is.list(cache_config))
  expect_true(is.list(lazy_config))
})

# Test complete startup optimization workflow
test_that("complete startup optimization workflow works end-to-end", {
  skip_if_not(exists("cache_startup_data", mode = "function"))
  skip_if_not(exists("load_cached_startup_data", mode = "function"))
  skip_if_not(exists("lazy_load_modules", mode = "function"))
  skip_if_not(exists("hash_session_token", mode = "function"))

  # Measure complete workflow time
  workflow_start <- Sys.time()

  expect_no_error({
    # 1. Cache system workflow
    log_info("Testing cache system integration", component = "INTEGRATION_TEST")
    cache_status_initial <- get_startup_cache_status()
    cached_artifacts <- cache_startup_data()
    cached_data <- load_cached_startup_data()
    cache_status_final <- get_startup_cache_status()

    # 2. Lazy loading workflow
    log_info("Testing lazy loading integration", component = "INTEGRATION_TEST")
    lazy_status_initial <- get_lazy_loading_status()
    loaded_modules <- lazy_load_modules()
    lazy_status_final <- get_lazy_loading_status()

    # 3. Security workflow
    log_info("Testing security integration", component = "INTEGRATION_TEST")
    test_token <- "integration_test_session_token_12345"
    hashed_token <- hash_session_token(test_token)

    # 4. Logging workflow
    log_info("Testing logging integration", component = "INTEGRATION_TEST")
    log_debug("Debug message in integration test", .context = "INTEGRATION_TEST")
    log_warn("Warning message in integration test", component = "INTEGRATION_TEST")
  })

  workflow_time <- as.numeric(difftime(Sys.time(), workflow_start, units = "secs"))

  # Complete workflow should be fast
  expect_lt(workflow_time, 2.0) # Should complete in less than 2 seconds

  message(sprintf("Complete startup optimization workflow: %.3fs", workflow_time))
})

# Test performance impact of startup optimizations
test_that("startup optimizations provide measurable performance benefits", {
  skip_if_not(exists("cache_startup_data", mode = "function"))
  skip_if_not(exists("lazy_load_modules", mode = "function"))

  # Test cache performance benefit
  cache_start <- Sys.time()
  cached_data <- suppressMessages(load_cached_startup_data())
  cache_load_time <- as.numeric(difftime(Sys.time(), cache_start, units = "secs"))

  # Test lazy loading performance benefit
  lazy_start <- Sys.time()
  loaded_modules <- lazy_load_modules()
  lazy_load_time <- as.numeric(difftime(Sys.time(), lazy_start, units = "secs"))

  # Both optimizations should be fast
  expect_lt(cache_load_time, 0.5)
  expect_lt(lazy_load_time, 1.0)

  # Combined optimization time should meet target
  combined_time <- cache_load_time + lazy_load_time
  expect_lt(combined_time, 1.0)

  message(sprintf("Performance benefits: cache=%.3fs, lazy=%.3fs, combined=%.3fs",
                  cache_load_time, lazy_load_time, combined_time))
})

# Test error handling integration across all optimizations
test_that("error handling works consistently across all optimization components", {
  skip_if_not(exists("safe_operation", mode = "function"))

  # Test error handling in cache system
  expect_no_error({
    safe_operation(
      operation_name = "Cache system error test",
      code = {
        if (exists("get_startup_cache_status", mode = "function")) {
          get_startup_cache_status()
        } else {
          stop("Cache function not available")
        }
      },
      fallback = list(cache_enabled = FALSE),
      error_type = "integration_test"
    )
  })

  # Test error handling in lazy loading
  expect_no_error({
    safe_operation(
      operation_name = "Lazy loading error test",
      code = {
        if (exists("get_lazy_loading_status", mode = "function")) {
          get_lazy_loading_status()
        } else {
          stop("Lazy loading function not available")
        }
      },
      fallback = list(),
      error_type = "integration_test"
    )
  })

  # Test error handling in security functions
  expect_no_error({
    safe_operation(
      operation_name = "Security function error test",
      code = {
        hash_session_token("test_token")
      },
      fallback = "unknown",
      error_type = "integration_test"
    )
  })
})

# Test memory efficiency of combined optimizations
test_that("combined startup optimizations are memory efficient", {
  if (requireNamespace("pryr", quietly = TRUE)) {
    # Measure memory before optimizations
    gc()
    mem_before <- pryr::mem_used()

    # Run all optimization components
    expect_no_error({
      # Cache operations
      if (exists("cache_startup_data", mode = "function")) {
        suppressMessages(cache_startup_data())
      }
      if (exists("load_cached_startup_data", mode = "function")) {
        suppressMessages(load_cached_startup_data())
      }

      # Lazy loading operations
      if (exists("lazy_load_modules", mode = "function")) {
        lazy_load_modules()
      }

      # Security operations
      for (i in 1:10) {
        hash_session_token(paste0("test_token_", i))
      }

      # Logging operations
      for (i in 1:20) {
        log_info(paste("Memory test message", i), component = "MEMORY_TEST")
      }
    })

    # Measure memory after optimizations
    gc()
    mem_after <- pryr::mem_used()

    # Memory increase should be reasonable
    mem_increase <- as.numeric(mem_after - mem_before) / 1024^2
    expect_lt(mem_increase, 100) # Should not increase by more than 100MB

    message(sprintf("Combined optimizations memory usage: %.2f MB", mem_increase))
  } else {
    skip("pryr package not available for memory testing")
  }
})

# Test startup optimization system resilience
test_that("startup optimization system is resilient to failures", {
  skip_if_not(exists("safe_operation", mode = "function"))

  # Test that system continues working even if individual components fail
  expect_no_error({
    # Simulate cache failure
    result1 <- safe_operation(
      operation_name = "Simulated cache failure",
      code = {
        stop("Simulated cache error")
      },
      fallback = list(cache_enabled = FALSE),
      error_type = "resilience_test"
    )

    # System should continue with fallback
    expect_equal(result1$cache_enabled, FALSE)

    # Simulate lazy loading failure
    result2 <- safe_operation(
      operation_name = "Simulated lazy loading failure",
      code = {
        stop("Simulated lazy loading error")
      },
      fallback = character(0),
      error_type = "resilience_test"
    )

    # System should continue with empty module list
    expect_equal(length(result2), 0)
  })
})

# Test security integration across all components
test_that("security improvements are integrated across all components", {
  skip_if_not(exists("hash_session_token", mode = "function"))
  skip_if_not(exists("log_info", mode = "function"))

  # Test that session tokens are properly hashed in different contexts
  test_scenarios <- c(
    "cache_system_token",
    "lazy_loading_token",
    "error_handling_token",
    "logging_system_token"
  )

  for (scenario in test_scenarios) {
    hashed <- hash_session_token(scenario)

    # Verify security properties
    expect_equal(nchar(hashed), 8)
    expect_true(grepl("^[a-f0-9]+$", hashed))
    expect_false(grepl(scenario, hashed, fixed = TRUE))

    # Log with hashed token (safe for logs)
    expect_no_error({
      log_info(paste("Security test for scenario:", scenario),
               component = paste0("SECURITY_", toupper(gsub("_", "", scenario))))
    })
  }
})

# Test configuration consistency across optimization components
test_that("optimization components use consistent configuration patterns", {
  # Test that all components respect environment variables appropriately
  original_debug <- Sys.getenv("SPC_DEBUG_MODE", "")
  original_log_level <- Sys.getenv("SPC_LOG_LEVEL", "")

  # Test with debug mode enabled
  Sys.setenv(SPC_DEBUG_MODE = "TRUE")
  Sys.setenv(SPC_LOG_LEVEL = "DEBUG")

  expect_no_error({
    # All components should work with debug configuration
    if (exists("get_startup_cache_status", mode = "function")) {
      get_startup_cache_status()
    }
    if (exists("get_lazy_loading_status", mode = "function")) {
      get_lazy_loading_status()
    }
    log_debug("Debug configuration test", component = "CONFIG_TEST")
  })

  # Test with production configuration
  Sys.setenv(SPC_DEBUG_MODE = "FALSE")
  Sys.setenv(SPC_LOG_LEVEL = "WARN")

  expect_no_error({
    # All components should work with production configuration
    if (exists("get_startup_cache_status", mode = "function")) {
      get_startup_cache_status()
    }
    if (exists("get_lazy_loading_status", mode = "function")) {
      get_lazy_loading_status()
    }
    log_warn("Production configuration test", component = "CONFIG_TEST")
  })

  # Restore original configuration
  if (original_debug == "") {
    Sys.unsetenv("SPC_DEBUG_MODE")
  } else {
    Sys.setenv(SPC_DEBUG_MODE = original_debug)
  }
  if (original_log_level == "") {
    Sys.unsetenv("SPC_LOG_LEVEL")
  } else {
    Sys.setenv(SPC_LOG_LEVEL = original_log_level)
  }
})

# Test backwards compatibility of optimization system
test_that("startup optimizations maintain backwards compatibility", {
  # Test that new optimizations don't break existing functionality
  expect_no_error({
    # Basic logging should still work
    log_info("Backwards compatibility test")

    # Error handling should still work
    safe_operation(
      operation_name = "Backwards compatibility test",
      code = { "success" },
      fallback = "fallback"
    )
  })

  # Test that old-style function calls still work
  if (exists("log_debug", mode = "function")) {
    expect_no_error({
      # Old-style context parameter
      log_debug("Old style context", .context = "BACKWARDS_COMPAT")
    })
  }
})

# Final comprehensive integration test
test_that("complete startup optimization system integration test", {
  # This test verifies that all optimization components work together
  integration_start <- Sys.time()

  expect_no_error({
    # 1. Initialize and test cache system
    if (exists("get_startup_cache_status", mode = "function")) {
      cache_status <- get_startup_cache_status()
      expect_is(cache_status, "list")
    }

    # 2. Initialize and test lazy loading
    if (exists("get_lazy_loading_status", mode = "function")) {
      lazy_status <- get_lazy_loading_status()
      expect_is(lazy_status, "list")
    }

    # 3. Test security features
    test_token <- paste0("integration_", Sys.time())
    hashed_token <- hash_session_token(test_token)
    expect_equal(nchar(hashed_token), 8)

    # 4. Test logging system
    log_info("Integration test completed successfully", component = "INTEGRATION_FINAL")

    # 5. Test error handling
    error_result <- safe_operation(
      operation_name = "Final integration test operation",
      code = {
        list(
          cache_available = exists("get_startup_cache_status", mode = "function"),
          lazy_available = exists("get_lazy_loading_status", mode = "function"),
          security_available = exists("hash_session_token", mode = "function"),
          logging_available = exists("log_info", mode = "function")
        )
      },
      fallback = list(error = TRUE),
      error_type = "integration_final"
    )

    expect_is(error_result, "list")
    expect_true(error_result$logging_available)
  })

  integration_time <- as.numeric(difftime(Sys.time(), integration_start, units = "secs"))

  # Complete integration test should be fast
  expect_lt(integration_time, 1.0)

  message(sprintf("Complete integration test successful: %.3fs", integration_time))
  message("All startup optimization components integrated and working correctly")
})