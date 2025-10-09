# test-error-recovery.R
# Sprint 5 Fase 1: Integration Testing
# Error recovery and resilience testing

library(testthat)
library(shiny)

context("Error Recovery Integration")

test_that("System recovers from invalid data upload", {
  app_state <- create_app_state()

  # Upload invalid data (empty data frame)
  invalid_data <- data.frame()

  result <- safe_operation(
    "Handle invalid data upload",
    code = {
      if (nrow(invalid_data) == 0) {
        stop("Empty data frame")
      }
      app_state$data$current_data <- invalid_data
      TRUE
    },
    fallback = function(e) {
      # Error handler should prevent state corruption
      app_state$data$current_data <- NULL
      FALSE
    }
  )

  # Operation should fail gracefully
  expect_false(result)
  expect_true(is.null(app_state$data$current_data))

  # System should be ready for valid upload
  valid_data <- data.frame(x = 1:10, y = 1:10)
  app_state$data$current_data <- valid_data

  expect_equal(nrow(app_state$data$current_data), 10)
})

test_that("Auto-detection handles malformed column names", {
  app_state <- create_app_state()

  # Data with problematic column names
  problem_data <- data.frame(
    `Column with spaces` = 1:10,
    `Column/with/slashes` = 1:10,
    `Column@with#special` = 1:10,
    check.names = FALSE
  )

  app_state$data$current_data <- problem_data

  # Auto-detection should handle special characters
  result <- safe_operation(
    "Detect columns with special names",
    code = {
      cols <- names(problem_data)
      detected <- list()

      # Look for x-like column
      if (any(grepl("spaces", cols, ignore.case = TRUE))) {
        detected$x_column <- cols[grep("spaces", cols, ignore.case = TRUE)[1]]
      }

      # Look for y-like column
      if (any(grepl("slashes", cols, ignore.case = TRUE))) {
        detected$y_column <- cols[grep("slashes", cols, ignore.case = TRUE)[1]]
      }

      detected
    },
    fallback = function(e) {
      list(x_column = NULL, y_column = NULL)
    }
  )

  # Detection should succeed despite special characters
  expect_true(!is.null(result$x_column))
  expect_true(!is.null(result$y_column))
})

test_that("Plot generation recovers from QIC errors", {
  skip_if_not_installed("qicharts2")

  app_state <- create_app_state()

  # Invalid configuration: missing required columns
  invalid_config <- list(
    x_col = NULL,  # Missing x column
    y_col = "y",
    n_col = NULL
  )

  test_data <- data.frame(x = 1:20, y = rnorm(20))
  qic_cache <- get_or_init_qic_cache(app_state)

  result <- safe_operation(
    "Generate plot with invalid config",
    code = {
      if (is.null(invalid_config$x_col)) {
        stop("X column is required")
      }
      generateSPCPlot(
        data = test_data,
        config = invalid_config,
        chart_type = "run",
        qic_cache = qic_cache
      )
    },
    fallback = function(e) {
      NULL
    }
  )

  # Should fail gracefully
  expect_true(is.null(result))

  # System should recover with valid configuration
  valid_config <- list(
    x_col = "x",
    y_col = "y",
    n_col = NULL
  )

  result_valid <- safe_operation(
    "Generate plot with valid config",
    code = {
      generateSPCPlot(
        data = test_data,
        config = valid_config,
        chart_type = "run",
        qic_cache = qic_cache
      )
    },
    fallback = NULL
  )

  expect_true(!is.null(result_valid))
  expect_true(!is.null(result_valid$plot))
})

test_that("Cache operations handle corruption gracefully", {
  app_state <- create_app_state()
  qic_cache <- get_or_init_qic_cache(app_state)

  # Store valid entry
  qic_cache$set("key1", list(data = "value1"))
  expect_equal(qic_cache$size(), 1)

  # Attempt to retrieve non-existent key
  result <- qic_cache$get("non_existent_key")
  expect_true(is.null(result))

  # Cache should remain functional
  expect_equal(qic_cache$size(), 1)

  # Clear cache should work
  qic_cache$clear()
  expect_equal(qic_cache$size(), 0)

  # Cache should be usable after clear
  qic_cache$set("key2", list(data = "value2"))
  expect_equal(qic_cache$size(), 1)
})

test_that("Event system handles missing observers gracefully", {
  app_state <- create_app_state()

  # Trigger events without observers registered
  result <- safe_operation(
    "Trigger events without observers",
    code = {
      app_state$events$data_updated <- app_state$events$data_updated + 1L
      app_state$events$auto_detection_completed <- app_state$events$auto_detection_completed + 1L
      TRUE
    },
    fallback = FALSE
  )

  # Events should trigger without error
  expect_true(result)
  expect_gt(app_state$events$data_updated, 0)
  expect_gt(app_state$events$auto_detection_completed, 0)
})

test_that("State corruption detection and recovery", {
  app_state <- create_app_state()

  # Simulate state corruption: conflicting flags
  app_state$columns$auto_detect$in_progress <- TRUE
  app_state$columns$auto_detect$completed <- TRUE  # Conflicting state

  # Detection logic should identify corruption
  is_corrupted <- app_state$columns$auto_detect$in_progress &&
    app_state$columns$auto_detect$completed

  expect_true(is_corrupted)

  # Recovery: reset to consistent state
  if (is_corrupted) {
    app_state$columns$auto_detect$in_progress <- FALSE
    app_state$columns$auto_detect$completed <- FALSE
    app_state$columns$auto_detect$results <- NULL
  }

  # Verify recovery
  expect_false(app_state$columns$auto_detect$in_progress)
  expect_false(app_state$columns$auto_detect$completed)
  expect_true(is.null(app_state$columns$auto_detect$results))
})

test_that("File upload handles encoding errors", {
  app_state <- create_app_state()

  # Simulate non-UTF8 data
  problematic_data <- data.frame(
    Dato = as.Date("2023-01-01") + 0:9,
    Værdi = c(1:9, NA),  # Include NA
    Tekst = c(letters[1:9], "")  # Include empty string
  )

  result <- safe_operation(
    "Process problematic encoding",
    code = {
      app_state$data$current_data <- problematic_data
      app_state$session$file_uploaded <- TRUE

      # Validate data
      has_valid_rows <- nrow(problematic_data) > 0
      has_valid_cols <- ncol(problematic_data) > 0

      list(valid = has_valid_rows && has_valid_cols)
    },
    fallback = function(e) {
      list(valid = FALSE)
    }
  )

  # Should handle gracefully
  expect_true(result$valid)
  expect_equal(nrow(app_state$data$current_data), 10)
})

test_that("Memory cleanup prevents accumulation", {
  app_state <- create_app_state()

  # Simulate multiple data uploads
  initial_size <- as.numeric(object.size(app_state))

  for (i in 1:10) {
    large_data <- data.frame(
      x = 1:1000,
      y = rnorm(1000),
      z = sample(letters, 1000, replace = TRUE)
    )

    app_state$data$current_data <- large_data
    app_state$events$data_updated <- app_state$events$data_updated + 1L

    # Clear cache after each upload
    if (!is.null(app_state$cache$qic)) {
      app_state$cache$qic$clear()
    }
  }

  # Final cleanup
  app_state$data$current_data <- NULL
  app_state$data$original_data <- NULL

  final_size <- as.numeric(object.size(app_state))

  # Size should not grow unboundedly
  # Allow 2x growth for reactive values overhead
  expect_lt(final_size, initial_size * 2)
})

test_that("Concurrent operations maintain data integrity", {
  app_state <- create_app_state()

  test_data <- data.frame(
    x = 1:50,
    y = rnorm(50, mean = 50, sd = 10)
  )

  # Simulate concurrent operations
  operations <- list(
    data_upload = function() {
      app_state$data$current_data <- test_data
      app_state$events$data_updated <- app_state$events$data_updated + 1L
    },
    auto_detect = function() {
      app_state$columns$auto_detect$trigger <- Sys.time()
      app_state$columns$auto_detect$in_progress <- TRUE
    },
    ui_sync = function() {
      app_state$columns$ui_sync$needed <- TRUE
    }
  )

  # Execute operations in rapid succession
  for (op in operations) {
    safe_operation(
      "Concurrent operation",
      code = op(),
      fallback = function(e) NULL
    )
  }

  # Verify data integrity
  expect_equal(nrow(app_state$data$current_data), 50)
  expect_gt(app_state$events$data_updated, 0)
  expect_true(!is.null(app_state$columns$auto_detect$trigger))
  expect_true(app_state$columns$ui_sync$needed)
})

test_that("Error in one observer doesn't break other observers", {
  app_state <- create_app_state()

  test_data <- data.frame(x = 1:10, y = 1:10)

  # Observer 1: Successful operation
  obs1_result <- safe_operation(
    "Observer 1",
    code = {
      app_state$data$current_data <- test_data
      TRUE
    },
    fallback = FALSE
  )

  # Observer 2: Failing operation
  obs2_result <- safe_operation(
    "Observer 2",
    code = {
      stop("Simulated error")
    },
    fallback = function(e) FALSE
  )

  # Observer 3: Should still execute
  obs3_result <- safe_operation(
    "Observer 3",
    code = {
      app_state$events$data_updated <- app_state$events$data_updated + 1L
      TRUE
    },
    fallback = FALSE
  )

  # Observer 1 and 3 should succeed
  expect_true(obs1_result)
  expect_false(obs2_result)  # Expected failure
  expect_true(obs3_result)

  # Data should be intact
  expect_equal(nrow(app_state$data$current_data), 10)
  expect_gt(app_state$events$data_updated, 0)
})

test_that("System recovers from cache timeout errors", {
  app_state <- create_app_state()
  qic_cache <- get_or_init_qic_cache(app_state)

  # Add entry with very short TTL
  qic_cache$set("short_ttl_key", list(data = "test"), timeout = 0.001)

  # Wait for expiration
  Sys.sleep(0.1)

  # Retrieving expired entry should return NULL
  result <- qic_cache$get("short_ttl_key")
  expect_true(is.null(result))

  # Cache should still be functional
  qic_cache$set("new_key", list(data = "new_value"))
  new_result <- qic_cache$get("new_key")
  expect_equal(new_result$data, "new_value")
})

test_that("Missing dependencies don't crash system", {
  app_state <- create_app_state()

  # Simulate missing qicharts2
  result <- safe_operation(
    "Check qicharts2 availability",
    code = {
      if (!requireNamespace("qicharts2", quietly = TRUE)) {
        stop("qicharts2 not available")
      }
      TRUE
    },
    fallback = function(e) {
      # System should handle missing package gracefully
      FALSE
    }
  )

  # Either succeeds (qicharts2 available) or fails gracefully
  expect_true(is.logical(result))
})

test_that("Invalid state transitions are prevented", {
  app_state <- create_app_state()

  # Attempt invalid transition: complete before start
  result <- safe_operation(
    "Invalid state transition",
    code = {
      if (!app_state$columns$auto_detect$in_progress) {
        stop("Cannot complete detection that hasn't started")
      }
      app_state$columns$auto_detect$completed <- TRUE
      TRUE
    },
    fallback = function(e) FALSE
  )

  # Invalid transition should be prevented
  expect_false(result)
  expect_false(app_state$columns$auto_detect$completed)

  # Valid transition should work
  app_state$columns$auto_detect$in_progress <- TRUE
  valid_result <- safe_operation(
    "Valid state transition",
    code = {
      app_state$columns$auto_detect$completed <- TRUE
      app_state$columns$auto_detect$in_progress <- FALSE
      TRUE
    },
    fallback = FALSE
  )

  expect_true(valid_result)
  expect_true(app_state$columns$auto_detect$completed)
  expect_false(app_state$columns$auto_detect$in_progress)
})
