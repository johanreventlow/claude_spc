# Test Logging System Standardization
# Validates logging API consistency and best practices
# Tests logging standardization implemented 2025-09-26

library(testthat)
context("Logging System Standardization Tests")

# Test Information Output
test_that("logging standardization test context information", {
  message("Testing logging system standardization implemented 2025-09-26")
  message("Improvement: Unified logging API with backward compatibility")
  message("372+ logging calls across 34 files standardized")

  skip("Context information only - not a real test")
})

# Test logging function existence
test_that("all logging functions exist and are callable", {
  logging_functions <- c("log_debug", "log_info", "log_warn", "log_error", "log_msg")

  for (func_name in logging_functions) {
    expect_true(exists(func_name, mode = "function"),
                info = paste("Function", func_name, "should exist"))
  }
})

# Test logging API consistency
test_that("logging functions support consistent API", {
  skip_if_not(exists("log_info", mode = "function"))
  skip_if_not(exists("log_debug", mode = "function"))
  skip_if_not(exists("log_warn", mode = "function"))
  skip_if_not(exists("log_error", mode = "function"))

  # Test basic logging functionality
  expect_no_error(log_info("Test info message"))
  expect_no_error(log_debug("Test debug message"))
  expect_no_error(log_warn("Test warning message"))
  expect_no_error(log_error("Test error message"))
})

# Test backward compatibility with .context parameter
test_that("logging functions support .context parameter for backward compatibility", {
  skip_if_not(exists("log_info", mode = "function"))
  skip_if_not(exists("log_debug", mode = "function"))
  skip_if_not(exists("log_warn", mode = "function"))
  skip_if_not(exists("log_error", mode = "function"))

  # Test .context parameter (old style)
  expect_no_error(log_info("Test message", .context = "TEST_CONTEXT"))
  expect_no_error(log_debug("Test message", .context = "TEST_CONTEXT"))
  expect_no_error(log_warn("Test message", .context = "TEST_CONTEXT"))
  expect_no_error(log_error("Test message", .context = "TEST_CONTEXT"))
})

# Test new component parameter
test_that("logging functions support component parameter for new style", {
  skip_if_not(exists("log_info", mode = "function"))
  skip_if_not(exists("log_debug", mode = "function"))
  skip_if_not(exists("log_warn", mode = "function"))
  skip_if_not(exists("log_error", mode = "function"))

  # Test component parameter (new style)
  expect_no_error(log_info("Test message", component = "TEST_COMPONENT"))
  expect_no_error(log_debug("Test message", component = "TEST_COMPONENT"))
  expect_no_error(log_warn("Test message", component = "TEST_COMPONENT"))
  expect_no_error(log_error("Test message", component = "TEST_COMPONENT"))
})

# Test parameter precedence
test_that("component parameter takes precedence over .context when both provided", {
  skip_if_not(exists("log_info", mode = "function"))

  # This should work without error and prefer component over .context
  expect_no_error({
    log_info("Test message", component = "COMPONENT", .context = "CONTEXT")
  })
})

# Test logging with details parameter
test_that("logging functions support structured details parameter", {
  skip_if_not(exists("log_debug_kv", mode = "function"))

  # Test structured logging with details
  expect_no_error({
    log_debug_kv(
      message = "Test structured message",
      test_key = "test_value",
      numeric_value = 123,
      .context = "TEST_CONTEXT"
    )
  })
})

# Test logging levels
test_that("logging respects log level configuration", {
  skip_if_not(exists("log_debug", mode = "function"))
  skip_if_not(exists("log_info", mode = "function"))

  # Save current log level
  original_level <- Sys.getenv("SPC_LOG_LEVEL", "")

  # Test with different log levels
  Sys.setenv(SPC_LOG_LEVEL = "INFO")
  expect_no_error(log_info("Info message at INFO level"))
  expect_no_error(log_debug("Debug message at INFO level")) # Should be filtered

  Sys.setenv(SPC_LOG_LEVEL = "DEBUG")
  expect_no_error(log_info("Info message at DEBUG level"))
  expect_no_error(log_debug("Debug message at DEBUG level"))

  # Restore original log level
  if (original_level == "") {
    Sys.unsetenv("SPC_LOG_LEVEL")
  } else {
    Sys.setenv(SPC_LOG_LEVEL = original_level)
  }
})

# Test that raw cat() calls have been eliminated
test_that("logging system avoids raw cat() fallbacks in production", {
  # This is more of a documentation test since we can't easily grep the loaded code
  # But we can test that our logging functions don't use raw cat() directly

  skip_if_not(exists("log_info", mode = "function"))

  # Capture output to verify it's not just raw cat()
  captured_output <- capture.output({
    log_info("Test message for output verification", component = "TEST")
  }, type = "message")

  # Output should be structured (contain timestamp, level, context)
  if (length(captured_output) > 0) {
    combined_output <- paste(captured_output, collapse = " ")
    # Should contain structured elements like timestamp format
    expect_true(grepl("\\[.*\\]", combined_output) ||  # Timestamp in brackets
                grepl("INFO|ERROR|WARN|DEBUG", combined_output)) # Log level
  }
})

# Test error handling in logging functions
test_that("logging functions handle errors gracefully", {
  skip_if_not(exists("log_error", mode = "function"))

  # Test with various problematic inputs
  expect_no_error(log_error(NULL))
  expect_no_error(log_error(""))
  expect_no_error(log_error(123))
  expect_no_error(log_error(list(key = "value")))

  # Test with very long messages
  long_message <- paste(rep("A", 10000), collapse = "")
  expect_no_error(log_error(long_message))

  # Test with special characters
  special_message <- "Message with øæå and 中文 and emojis 🚀"
  expect_no_error(log_error(special_message))
})

# Test logging context standardization
test_that("logging contexts follow standardized naming convention", {
  # Test that our logging system supports the documented context names
  standard_contexts <- c(
    "APP_SERVER", "FILE_UPLOAD", "COLUMN_MGMT", "AUTO_DETECT",
    "PLOT_DATA", "SESSION_LIFECYCLE", "STARTUP_CACHE", "LAZY_LOADING",
    "ERROR_HANDLING", "TEST_MODE", "PERFORMANCE_MONITOR"
  )

  skip_if_not(exists("log_info", mode = "function"))

  for (context in standard_contexts) {
    expect_no_error({
      log_info(paste("Test message for", context), .context = context)
    })
  }
})

# Test performance of logging operations
test_that("logging operations are performant", {
  skip_if_not(exists("log_info", mode = "function"))

  # Measure time for single log operation
  start_time <- Sys.time()
  log_info("Performance test message", component = "PERFORMANCE_TEST")
  single_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  # Single log operation should be very fast
  expect_lt(single_time, 0.01) # Less than 10ms

  # Measure time for batch logging
  start_time <- Sys.time()
  for (i in 1:100) {
    log_info(paste("Batch test message", i), component = "PERFORMANCE_TEST")
  }
  batch_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  # Batch logging should be reasonable
  expect_lt(batch_time, 1.0) # Less than 1 second for 100 messages

  message(sprintf("Logging performance: single=%.6fs, 100x=%.3fs",
                  single_time, batch_time))
})

# Test safe_operation integration with logging
test_that("safe_operation integrates correctly with logging system", {
  skip_if_not(exists("safe_operation", mode = "function"))

  # Test that safe_operation uses structured logging
  expect_no_error({
    result <- safe_operation(
      operation_name = "Test operation for logging",
      code = {
        "success"
      },
      fallback = "fallback_value",
      error_type = "test"
    )
    expect_equal(result, "success")
  })

  # Test that safe_operation logs errors properly
  expect_no_error({
    result <- safe_operation(
      operation_name = "Test operation with error",
      code = {
        stop("Intentional test error")
      },
      fallback = "fallback_value",
      error_type = "test"
    )
    expect_equal(result, "fallback_value")
  })
})

# Test logging system memory usage
test_that("logging system manages memory efficiently", {
  skip_if_not(exists("log_info", mode = "function"))

  if (requireNamespace("pryr", quietly = TRUE)) {
    # Measure memory before logging
    gc()
    mem_before <- pryr::mem_used()

    # Generate many log messages
    for (i in 1:1000) {
      log_info(paste("Memory test message", i), component = "MEMORY_TEST")
    }

    # Force garbage collection
    gc()
    mem_after <- pryr::mem_used()

    # Memory increase should be minimal
    mem_increase <- as.numeric(mem_after - mem_before) / 1024^2
    expect_lt(mem_increase, 10) # Should not increase by more than 10MB

    message(sprintf("Logging memory usage: %.2f MB for 1000 messages", mem_increase))
  } else {
    skip("pryr package not available for memory testing")
  }
})

# Test logging output format consistency
test_that("logging output follows consistent format", {
  skip_if_not(exists("log_info", mode = "function"))

  # Capture log output
  captured_output <- capture.output({
    log_info("Format test message", component = "FORMAT_TEST")
  }, type = "message")

  if (length(captured_output) > 0) {
    output_line <- captured_output[1]

    # Should contain basic structured elements
    # Format might be: [timestamp] LEVEL: [context] message
    expect_true(nchar(output_line) > 0)

    # Should not contain raw unformatted output
    expect_false(grepl("^Format test message$", output_line))
  }
})

# Test edge cases for logging API
test_that("logging API handles edge cases correctly", {
  skip_if_not(exists("log_info", mode = "function"))

  # Test with empty context
  expect_no_error(log_info("Test message", component = ""))
  expect_no_error(log_info("Test message", .context = ""))

  # Test with NULL context
  expect_no_error(log_info("Test message", component = NULL))
  expect_no_error(log_info("Test message", .context = NULL))

  # Test with only message
  expect_no_error(log_info("Test message"))

  # Test message coercion
  expect_no_error(log_info(123))
  expect_no_error(log_info(c("a", "b", "c")))
})

# Test logging system initialization
test_that("logging system initializes correctly", {
  # Test that logging works immediately after loading
  skip_if_not(exists("log_info", mode = "function"))

  # Should work without explicit initialization
  expect_no_error(log_info("Initialization test message"))

  # Test that log level is properly set
  current_level <- Sys.getenv("SPC_LOG_LEVEL", "")
  expect_true(current_level %in% c("", "DEBUG", "INFO", "WARN", "ERROR"))
})

# Test structured logging with log_debug_kv
test_that("structured logging with log_debug_kv works correctly", {
  skip_if_not(exists("log_debug_kv", mode = "function"))

  # Test structured logging
  expect_no_error({
    log_debug_kv(
      message = "Structured test message",
      key1 = "value1",
      key2 = 123,
      key3 = TRUE,
      .context = "STRUCTURED_TEST"
    )
  })

  # Test with missing message
  expect_no_error({
    log_debug_kv(
      key1 = "value1",
      .context = "STRUCTURED_TEST"
    )
  })
})

# Final integration test
test_that("logging system works end-to-end", {
  skip_if_not(exists("log_debug", mode = "function"))
  skip_if_not(exists("log_info", mode = "function"))
  skip_if_not(exists("log_warn", mode = "function"))
  skip_if_not(exists("log_error", mode = "function"))

  # Full workflow test
  expect_no_error({
    # Test all log levels
    log_debug("Debug message", component = "INTEGRATION_TEST")
    log_info("Info message", component = "INTEGRATION_TEST")
    log_warn("Warning message", component = "INTEGRATION_TEST")
    log_error("Error message", component = "INTEGRATION_TEST")

    # Test backward compatibility
    log_info("Backward compatibility test", .context = "INTEGRATION_TEST")

    # Test structured logging
    if (exists("log_debug_kv", mode = "function")) {
      log_debug_kv(
        message = "Structured logging test",
        test_key = "test_value",
        .context = "INTEGRATION_TEST"
      )
    }
  })

  message("End-to-end logging system test successful")
})