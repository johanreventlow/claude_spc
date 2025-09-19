# FASE 4: STRESS TESTING for Token+Queue System
# Test system robustness under high load conditions

#' Stress Test: High Volume UI Updates
#'
#' Tests system behavior with rapid, high-volume UI updates
#' to ensure queue and token systems handle load gracefully.
test_that("High volume UI updates stress test", {
  cat("\n\n=== STRESS TEST: High Volume UI Updates ===\n")

  # SETUP: Create app_state for stress testing
  source("../../global.R", local = TRUE)
  app_state <- create_app_state()

  # Mock session for testing
  update_counter <- 0
  mock_session <- list(
    sendCustomMessage = function(type, message) {
      update_counter <<- update_counter + 1
      cat(paste("STRESS_TEST: UI update", update_counter, "processed\n"))
    }
  )

  # STRESS TEST: Simulate 100 rapid UI updates
  cat("STRESS_TEST: Executing 100 rapid UI updates...\n")
  start_time <- Sys.time()

  results <- list()
  for (i in 1:100) {
    result <- safe_programmatic_ui_update(
      session = mock_session,
      app_state = app_state,
      update_function = function() {
        updateSelectizeInput(mock_session, "test_column",
                           choices = c("Option1", "Option2"),
                           selected = paste0("Option", (i %% 2) + 1))
      },
      delay_ms = 50
    )
    results[[i]] <- result
  }

  end_time <- Sys.time()
  total_duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # VERIFY: Check system performance under stress
  cat(paste("STRESS_TEST: Completed 100 updates in", round(total_duration, 2), "seconds\n"))

  # Get performance report
  report <- get_performance_report(app_state)
  cat("STRESS_TEST: Final performance report:\n")
  cat(report$formatted_text)

  # ASSERTIONS: System should handle stress gracefully
  expect_true(total_duration < 30)  # Should complete within 30 seconds
  expect_true(report$total_updates >= 1)  # At least some updates should be processed
  expect_true(report$health_status %in% c("HEALTHY", "CAUTION", "WARNING"))  # Valid health status

  cat("STRESS_TEST: ✅ High volume stress test completed successfully\n")
})

#' Stress Test: Memory Pressure Testing
#'
#' Tests system behavior under memory pressure with many pending tokens
test_that("Memory pressure stress test", {
  cat("\n\n=== STRESS TEST: Memory Pressure ===\n")

  # SETUP: Create app_state for memory testing
  source("../../global.R", local = TRUE)
  app_state <- create_app_state()

  # STRESS TEST: Create many pending tokens to test memory limits
  cat("STRESS_TEST: Creating many pending tokens to test memory management...\n")

  # Simulate 150 pending tokens (exceeds max_pending_tokens limit of 100)
  for (i in 1:150) {
    token_id <- paste0("stress_token_", i)
    isolate({
      app_state$ui$pending_programmatic_inputs[[paste0("test_input_", i)]] <- list(
        token = token_id,
        value = paste0("value_", i),
        timestamp = Sys.time() - sample(1:600, 1),  # Random timestamps up to 10 minutes ago
        session_token = paste0("session_", i)
      )
    })
  }

  initial_token_count <- length(isolate(app_state$ui$pending_programmatic_inputs))
  cat(paste("STRESS_TEST: Created", initial_token_count, "pending tokens\n"))

  # TEST: Run comprehensive cleanup to test memory management
  comprehensive_system_cleanup(app_state)

  # VERIFY: Memory management should have kicked in
  final_token_count <- length(isolate(app_state$ui$pending_programmatic_inputs))
  max_tokens <- isolate(app_state$ui$memory_limits$max_pending_tokens)

  cat(paste("STRESS_TEST: Token count after cleanup:", final_token_count, "/", max_tokens, "\n"))

  # ASSERTIONS: Memory limits should be enforced
  expect_true(final_token_count <= max_tokens)  # Should not exceed limits
  expect_true(initial_token_count > final_token_count)  # Should have cleaned up some tokens

  cat("STRESS_TEST: ✅ Memory pressure test completed successfully\n")
})

#' Stress Test: Queue Overflow Testing
#'
#' Tests queue behavior when exceeding capacity limits
test_that("Queue overflow stress test", {
  cat("\n\n=== STRESS TEST: Queue Overflow ===\n")

  # SETUP: Create app_state for queue testing
  source("../../global.R", local = TRUE)
  app_state <- create_app_state()

  # SETUP: Force queue to be in "updating" state to trigger queueing
  app_state$ui$updating_programmatically <- TRUE

  # STRESS TEST: Add more queue entries than max_queue_size
  max_queue_size <- isolate(app_state$ui$memory_limits$max_queue_size)
  overflow_count <- max_queue_size + 20

  cat(paste("STRESS_TEST: Adding", overflow_count, "queue entries (max:", max_queue_size, ")\n"))

  mock_session <- list(sendCustomMessage = function(...) {})

  for (i in 1:overflow_count) {
    result <- safe_programmatic_ui_update(
      session = mock_session,
      app_state = app_state,
      update_function = function() {
        updateSelectizeInput(mock_session, "overflow_test",
                           choices = c("A", "B"), selected = "A")
      },
      delay_ms = 10
    )
  }

  # VERIFY: Queue should be limited to max size
  final_queue_size <- length(isolate(app_state$ui$queued_updates))
  cat(paste("STRESS_TEST: Final queue size:", final_queue_size, "/", max_queue_size, "\n"))

  # ASSERTIONS: Queue overflow should be handled gracefully
  expect_true(final_queue_size <= max_queue_size)  # Should not exceed limits

  # Get performance metrics
  queue_metrics <- isolate(app_state$ui$performance_metrics$queued_updates)
  cat(paste("STRESS_TEST: Total queued updates metric:", queue_metrics, "\n"))

  expect_true(queue_metrics > 0)  # Should have recorded queued updates

  cat("STRESS_TEST: ✅ Queue overflow test completed successfully\n")
})

#' Stress Test: Concurrent Token and Queue Operations
#'
#' Tests system behavior with mixed token consumption and queueing
test_that("Concurrent operations stress test", {
  cat("\n\n=== STRESS TEST: Concurrent Token and Queue Operations ===\n")

  # SETUP: Create app_state for concurrent testing
  source("../../global.R", local = TRUE)
  app_state <- create_app_state()

  # SETUP: Create some pending tokens first
  for (i in 1:10) {
    isolate({
      app_state$ui$pending_programmatic_inputs[[paste0("concurrent_input_", i)]] <- list(
        token = paste0("concurrent_token_", i),
        value = paste0("concurrent_value_", i),
        timestamp = Sys.time(),
        session_token = paste0("concurrent_session_", i)
      )
    })
  }

  # STRESS TEST: Simulate token consumption while queue operations happen
  cat("STRESS_TEST: Simulating concurrent token consumption and queue operations...\n")

  # Simulate token consumption
  consumed_tokens <- 0
  for (i in 1:5) {
    input_id <- paste0("concurrent_input_", i)
    pending_token <- isolate(app_state$ui$pending_programmatic_inputs[[input_id]])

    if (!is.null(pending_token)) {
      # Simulate token consumption (like in utils_event_system.R)
      app_state$ui$pending_programmatic_inputs[[input_id]] <- NULL
      isolate({
        app_state$ui$performance_metrics$tokens_consumed <-
          app_state$ui$performance_metrics$tokens_consumed + 1L
      })
      consumed_tokens <- consumed_tokens + 1
    }
  }

  # Simulate queue operations simultaneously
  app_state$ui$updating_programmatically <- TRUE
  queue_operations <- 0
  for (i in 1:15) {
    mock_session <- list(sendCustomMessage = function(...) {})
    result <- safe_programmatic_ui_update(
      session = mock_session,
      app_state = app_state,
      update_function = function() {
        updateSelectizeInput(mock_session, "concurrent_test",
                           choices = c("X", "Y"), selected = "X")
      },
      delay_ms = 5
    )
    if (isTRUE(result$queued)) {
      queue_operations <- queue_operations + 1
    }
  }

  # VERIFY: Both operations should work correctly
  final_report <- get_performance_report(app_state)
  cat("STRESS_TEST: Final concurrent operations report:\n")
  cat(final_report$formatted_text)

  # ASSERTIONS: Both token consumption and queueing should work
  expect_true(consumed_tokens > 0)  # Should have consumed some tokens
  expect_true(queue_operations > 0)  # Should have queued some operations
  expect_true(final_report$tokens_consumed >= consumed_tokens)  # Metrics should reflect consumption

  cat(paste("STRESS_TEST: Consumed", consumed_tokens, "tokens and queued", queue_operations, "operations\n"))
  cat("STRESS_TEST: ✅ Concurrent operations test completed successfully\n")
})

#' Performance Benchmark Test
#'
#' Benchmarks system performance and establishes baseline metrics
test_that("Performance benchmark test", {
  cat("\n\n=== BENCHMARK: System Performance ===\n")

  # SETUP: Create app_state for benchmarking
  source("../../global.R", local = TRUE)
  app_state <- create_app_state()

  mock_session <- list(sendCustomMessage = function(...) {})

  # BENCHMARK: Measure single update performance
  single_update_times <- numeric(10)
  for (i in 1:10) {
    start <- Sys.time()
    result <- safe_programmatic_ui_update(
      session = mock_session,
      app_state = app_state,
      update_function = function() {
        updateSelectizeInput(mock_session, "benchmark_test",
                           choices = c("Benchmark1", "Benchmark2"),
                           selected = "Benchmark1")
      },
      delay_ms = 10
    )
    end <- Sys.time()
    single_update_times[i] <- as.numeric(difftime(end, start, units = "secs")) * 1000
  }

  avg_single_update <- mean(single_update_times)
  cat(paste("BENCHMARK: Average single update time:", round(avg_single_update, 2), "ms\n"))

  # BENCHMARK: Measure cleanup performance
  # First create some data to clean
  for (i in 1:50) {
    isolate({
      app_state$ui$pending_programmatic_inputs[[paste0("benchmark_token_", i)]] <- list(
        token = paste0("bench_token_", i),
        value = paste0("bench_value_", i),
        timestamp = Sys.time() - sample(1:1000, 1),
        session_token = paste0("bench_session_", i)
      )
    })
  }

  cleanup_start <- Sys.time()
  comprehensive_system_cleanup(app_state)
  cleanup_end <- Sys.time()
  cleanup_time <- as.numeric(difftime(cleanup_end, cleanup_start, units = "secs")) * 1000

  cat(paste("BENCHMARK: Comprehensive cleanup time:", round(cleanup_time, 2), "ms\n"))

  # BENCHMARK: Get final performance report
  final_report <- get_performance_report(app_state)
  cat("BENCHMARK: Final system performance:\n")
  cat(final_report$formatted_text)

  # ASSERTIONS: Performance should be within acceptable ranges
  expect_true(avg_single_update < 1000)  # Single updates should be under 1 second
  expect_true(cleanup_time < 5000)       # Cleanup should be under 5 seconds
  expect_equal(final_report$health_status, "HEALTHY")  # System should be healthy

  cat("BENCHMARK: ✅ Performance benchmark completed successfully\n")
})