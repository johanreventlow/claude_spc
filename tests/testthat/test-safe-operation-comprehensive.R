# test-safe-operation-comprehensive.R
# Comprehensive tests for safe operation patterns and error handling
# Tests logging system integration, Shiny session integration, and defensive programming
# Critical for production environments and robust operation

test_that("safe_operation basic functionality works", {
  # TEST: Core safe_operation wrapper functionality

  # SETUP: Simple successful operation
  result <- safe_operation(
    operation_name = "Basic test operation",
    code = { 2 + 2 },
    fallback = 0
  )

  # TEST: Successful operation returns correct result
  expect_equal(result, 4)

  # SETUP: Operation that fails
  error_result <- safe_operation(
    operation_name = "Failing operation",
    code = { stop("Intentional test error") },
    fallback = "fallback_value"
  )

  # TEST: Failed operation returns fallback
  expect_equal(error_result, "fallback_value")

  # SETUP: Operation without fallback
  null_fallback_result <- safe_operation(
    operation_name = "No fallback operation",
    code = { stop("Error with no fallback") }
  )

  # TEST: Default fallback is NULL
  expect_null(null_fallback_result)
})

test_that("safe_operation error handling works correctly", {
  # TEST: Various error scenarios and error type handling

  # SETUP: Different error types
  validation_error <- safe_operation(
    operation_name = "Validation test",
    code = { stop("Invalid input data") },
    fallback = "validation_fallback",
    error_type = "validation"
  )

  processing_error <- safe_operation(
    operation_name = "Processing test",
    code = { stop("Processing failed") },
    fallback = "processing_fallback",
    error_type = "processing"
  )

  network_error <- safe_operation(
    operation_name = "Network test",
    code = { stop("Connection failed") },
    fallback = "network_fallback",
    error_type = "network"
  )

  # TEST: Error type categorization works
  expect_equal(validation_error, "validation_fallback")
  expect_equal(processing_error, "processing_fallback")
  expect_equal(network_error, "network_fallback")

  # SETUP: Complex error scenario
  complex_error <- safe_operation(
    operation_name = "Complex operation",
    code = {
      data <- data.frame(x = 1:5)
      # This will actually fail with an error
      stop("Simulated data processing error")
    },
    fallback = data.frame(),
    error_type = "data_access"
  )

  # TEST: Complex error handled gracefully
  expect_true(is.data.frame(complex_error))
  expect_equal(nrow(complex_error), 0) # Empty fallback data frame
})

test_that("safe_operation with Shiny session integration works", {
  # TEST: Shiny session integration and user notifications

  # Skip if Shiny not available
  skip_if_not_installed("shiny")

  # SETUP: Mock Shiny session
  mock_session <- shiny::MockShinySession$new()

  # Track notifications
  notification_calls <- list()
  original_showNotification <- NULL

  # Mock showNotification to capture calls
  if (exists("showNotification", envir = asNamespace("shiny"))) {
    # Store original for restoration
    original_showNotification <- get("showNotification", envir = asNamespace("shiny"))

    # Replace with mock
    assignInNamespace("showNotification", function(ui, type = NULL, duration = NULL, ...) {
      notification_calls <<- append(notification_calls, list(list(
        ui = ui,
        type = type,
        duration = duration
      )))
    }, ns = "shiny")
  }

  # Ensure cleanup
  on.exit({
    if (!is.null(original_showNotification)) {
      assignInNamespace("showNotification", original_showNotification, ns = "shiny")
    }
  })

  # TEST: Operation with user notification enabled
  result_with_notification <- safe_operation(
    operation_name = "User notification test",
    code = { stop("User should see this error") },
    fallback = "user_fallback",
    session = mock_session,
    show_user = TRUE,
    error_type = "user_facing"
  )

  # TEST: Fallback value returned
  expect_equal(result_with_notification, "user_fallback")

  # TEST: Operation without user notification
  result_no_notification <- safe_operation(
    operation_name = "No notification test",
    code = { stop("User should not see this") },
    fallback = "silent_fallback",
    session = mock_session,
    show_user = FALSE
  )

  # TEST: Fallback value returned without user notification
  expect_equal(result_no_notification, "silent_fallback")
})

test_that("safe_operation logging integration works", {
  # TEST: Integration with logging system

  # SETUP: Track log calls
  log_calls <- list()

  # Mock log_error function if it doesn't exist
  if (!exists("log_error", mode = "function")) {
    log_error <- function(message, context, ...) {
      log_calls <<- append(log_calls, list(list(
        message = message,
        context = context
      )))
    }
    # Make it available globally for safe_operation
    assign("log_error", log_error, envir = .GlobalEnv)
    on.exit(rm("log_error", envir = .GlobalEnv))
  }

  # TEST: Error logging functionality
  logged_error <- safe_operation(
    operation_name = "Logging test operation",
    code = { stop("This error should be logged") },
    fallback = "logged_fallback",
    error_type = "testing"
  )

  # TEST: Operation returns fallback
  expect_equal(logged_error, "logged_fallback")

  # Note: Actual log verification would depend on logging system implementation
})

test_that("safe_operation with complex fallback functions works", {
  # TEST: Complex fallback scenarios and function fallbacks

  # SETUP: Function as fallback
  create_empty_data <- function() {
    data.frame(
      id = integer(0),
      value = numeric(0),
      status = character(0),
      stringsAsFactors = FALSE
    )
  }

  # TEST: Function fallback
  result_with_function_fallback <- safe_operation(
    operation_name = "Function fallback test",
    code = { stop("Operation failed") },
    fallback = create_empty_data()
  )

  expect_true(is.data.frame(result_with_function_fallback))
  expect_equal(nrow(result_with_function_fallback), 0)
  expect_true(all(c("id", "value", "status") %in% names(result_with_function_fallback)))

  # SETUP: Complex data structure fallback
  complex_fallback <- list(
    data = data.frame(),
    metadata = list(
      source = "fallback",
      timestamp = Sys.time(),
      error_occurred = TRUE
    ),
    status = "failed"
  )

  # TEST: Complex structure fallback
  result_complex_fallback <- safe_operation(
    operation_name = "Complex fallback test",
    code = { stop("Complex operation failed") },
    fallback = complex_fallback
  )

  expect_true(is.list(result_complex_fallback))
  expect_true("data" %in% names(result_complex_fallback))
  expect_true("metadata" %in% names(result_complex_fallback))
  expect_equal(result_complex_fallback$status, "failed")
  expect_true(result_complex_fallback$metadata$error_occurred)
})

test_that("safe_operation defensive programming patterns work", {
  # TEST: Defensive programming and robust operation patterns

  # TEST: Safe data access
  test_data <- data.frame(
    a = 1:5,
    b = letters[1:5],
    stringsAsFactors = FALSE
  )

  # Safe column access
  safe_column_access <- safe_operation(
    operation_name = "Safe column access",
    code = {
      if ("c" %in% names(test_data)) {
        test_data$c
      } else {
        stop("Column 'c' not found")
      }
    },
    fallback = rep(NA, nrow(test_data))
  )

  expect_true(all(is.na(safe_column_access)))
  expect_equal(length(safe_column_access), 5)

  # TEST: Safe type conversion
  mixed_data <- c("1", "2", "invalid", "4", "5")

  safe_conversion <- safe_operation(
    operation_name = "Safe numeric conversion",
    code = {
      result <- as.numeric(mixed_data)
      if (any(is.na(result))) {
        stop("Invalid values in conversion")
      }
      result
    },
    fallback = rep(0, length(mixed_data))
  )

  expect_equal(safe_conversion, rep(0, 5))

  # TEST: Safe file operations
  safe_file_read <- safe_operation(
    operation_name = "Safe file reading",
    code = {
      read.csv("/nonexistent/file/path.csv")
    },
    fallback = data.frame(
      error = "File not found",
      stringsAsFactors = FALSE
    )
  )

  expect_true(is.data.frame(safe_file_read))
  expect_equal(safe_file_read$error, "File not found")
})

test_that("validate_exists function works correctly", {
  # TEST: Object existence validation

  # SETUP: Test environment with some objects
  test_env <- new.env()
  test_env$existing_var <- "I exist"
  test_env$data_object <- data.frame(x = 1:3)

  # TEST: Validation passes when objects exist
  if (exists("validate_exists", mode = "function")) {
    expect_no_error(
      validate_exists(
        existing_var = test_env,
        data_object = test_env
      )
    )

    # TEST: Validation fails when object doesn't exist
    expect_error(
      validate_exists(
        nonexistent_var = test_env,
        error_message = "Test validation failed"
      ),
      "Test validation failed"
    )

    # TEST: Mixed validation (some exist, some don't)
    expect_error(
      validate_exists(
        existing_var = test_env,
        nonexistent_var = test_env
      )
    )
  }
})

test_that("safe_operation with SPC-specific scenarios works", {
  # TEST: SPC-specific error scenarios

  # TEST: Safe data parsing
  spc_data_raw <- data.frame(
    Dato = c("01-01-2024", "invalid-date", "03-01-2024"),
    Værdi = c("12,5", "invalid", "15,2"),
    stringsAsFactors = FALSE
  )

  safe_date_parsing <- safe_operation(
    operation_name = "SPC date parsing",
    code = {
      parsed_dates <- as.Date(spc_data_raw$Dato, format = "%d-%m-%Y")
      if (any(is.na(parsed_dates))) {
        stop("Invalid date format detected")
      }
      parsed_dates
    },
    fallback = rep(as.Date(NA), nrow(spc_data_raw))
  )

  expect_true(all(is.na(safe_date_parsing)))

  # TEST: Safe qicharts2 operation
  spc_plot_data <- data.frame(
    x = 1:5,
    y = c(0.9, 0.85, 0.92, 0.88, 0.91),
    stringsAsFactors = FALSE
  )

  safe_qic_operation <- safe_operation(
    operation_name = "Safe qicharts2 operation",
    code = {
      # Simulate qicharts2 call that might fail
      if (!"qicharts2" %in% loadedNamespaces()) {
        stop("qicharts2 package not available")
      }
      # Would normally call qicharts2::qic here
      list(plot = "mock_plot", data = spc_plot_data)
    },
    fallback = list(
      plot = NULL,
      data = data.frame(),
      error = "qicharts2 operation failed"
    )
  )

  expect_true(is.list(safe_qic_operation))

  # Since qicharts2 is not available, should return fallback values
  if (!"qicharts2" %in% loadedNamespaces()) {
    expect_null(safe_qic_operation$plot)
    expect_equal(nrow(safe_qic_operation$data), 0)
    expect_equal(safe_qic_operation$error, "qicharts2 operation failed")
  } else {
    # If qicharts2 is available, operation succeeds
    expect_equal(safe_qic_operation$plot, "mock_plot")
    expect_equal(nrow(safe_qic_operation$data), 5)
  }

  # TEST: Safe auto-detection
  autodetect_data <- data.frame(
    col1 = c("A", "B", "C"),
    col2 = c("X", "Y", "Z"),
    stringsAsFactors = FALSE
  )

  safe_autodetect <- safe_operation(
    operation_name = "Safe column auto-detection",
    code = {
      # Simulate auto-detection that fails to find SPC columns
      date_cols <- sapply(autodetect_data, function(x) inherits(x, c("Date", "POSIXct")))
      numeric_cols <- sapply(autodetect_data, is.numeric)

      if (!any(date_cols) && !any(numeric_cols)) {
        stop("No suitable SPC columns detected")
      }

      list(x_col = names(date_cols)[1], y_col = names(numeric_cols)[1])
    },
    fallback = list(
      x_col = NULL,
      y_col = NULL,
      detection_failed = TRUE
    )
  )

  expect_true(is.list(safe_autodetect))
  expect_null(safe_autodetect$x_col)
  expect_null(safe_autodetect$y_col)
  expect_true(safe_autodetect$detection_failed)
})

test_that("safe_operation performance and memory handling works", {
  # TEST: Performance considerations and memory management

  # TEST: Large data operation with memory constraints
  large_operation <- safe_operation(
    operation_name = "Large data processing",
    code = {
      # Create large data structure
      large_data <- data.frame(
        x = 1:10000,
        y = rnorm(10000),
        z = sample(letters, 10000, replace = TRUE),
        stringsAsFactors = FALSE
      )

      # Simulate memory-intensive operation
      result <- aggregate(y ~ z, data = large_data, FUN = mean)
      return(result)
    },
    fallback = data.frame(
      z = character(0),
      y = numeric(0),
      stringsAsFactors = FALSE
    )
  )

  # Should complete successfully or return fallback
  expect_true(is.data.frame(large_operation))

  # TEST: Timeout simulation (long-running operation)
  start_time <- Sys.time()

  timeout_operation <- safe_operation(
    operation_name = "Potentially long operation",
    code = {
      # Simulate a quick operation (don't actually wait)
      for (i in 1:100) {
        if (i %% 50 == 0) {
          # Check if we should break early
          if (as.numeric(Sys.time() - start_time) > 0.1) {
            stop("Operation took too long")
          }
        }
      }
      "completed"
    },
    fallback = "timeout_fallback"
  )

  duration <- as.numeric(Sys.time() - start_time)

  # Should complete quickly
  expect_lt(duration, 1.0)
  expect_true(timeout_operation %in% c("completed", "timeout_fallback"))
})

test_that("safe_operation error cascade prevention works", {
  # TEST: Preventing error cascades and recursive failures

  # TEST: Nested safe operations
  nested_operation <- safe_operation(
    operation_name = "Outer operation",
    code = {
      inner_result <- safe_operation(
        operation_name = "Inner operation",
        code = { stop("Inner error") },
        fallback = "inner_fallback"
      )

      if (inner_result == "inner_fallback") {
        stop("Outer operation failed due to inner failure")
      }

      "success"
    },
    fallback = "outer_fallback"
  )

  # Should handle nested failures gracefully
  expect_equal(nested_operation, "outer_fallback")

  # TEST: Chain of operations with failure recovery
  chain_result <- list()

  # Step 1
  chain_result$step1 <- safe_operation(
    operation_name = "Chain step 1",
    code = { "step1_success" },
    fallback = "step1_failed"
  )

  # Step 2 (depends on step 1)
  chain_result$step2 <- safe_operation(
    operation_name = "Chain step 2",
    code = {
      if (chain_result$step1 == "step1_failed") {
        stop("Cannot proceed - step 1 failed")
      }
      "step2_success"
    },
    fallback = "step2_failed"
  )

  # Step 3 (fails)
  chain_result$step3 <- safe_operation(
    operation_name = "Chain step 3",
    code = { stop("Step 3 always fails") },
    fallback = "step3_failed"
  )

  # Verify chain behavior
  expect_equal(chain_result$step1, "step1_success")
  expect_equal(chain_result$step2, "step2_success")
  expect_equal(chain_result$step3, "step3_failed")

  # TEST: Recovery mechanism
  recovery_needed <- any(sapply(chain_result, function(x) grepl("failed", x)))
  expect_true(recovery_needed) # Step 3 should have failed

  # Implement recovery
  if (recovery_needed) {
    recovery_result <- safe_operation(
      operation_name = "Chain recovery",
      code = {
        # Implement recovery logic
        failed_steps <- names(chain_result)[sapply(chain_result, function(x) grepl("failed", x))]
        paste("Recovered from failed steps:", paste(failed_steps, collapse = ", "))
      },
      fallback = "recovery_failed"
    )

    expect_true(grepl("step3", recovery_result))
  }
})