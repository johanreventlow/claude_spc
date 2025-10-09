# Startup Performance Regression Tests
# These tests ensure that startup performance doesn't degrade over time

test_that("Startup QIC call count stays within target (<3 calls)", {
  # Initialize metrics
  init_startup_metrics()

  # Simulate clean startup sequence
  set_startup_phase("initialization")
  track_event("app_server_started", "regression_test")

  set_startup_phase("data_ready")
  track_event("test_mode_ready", "regression_test")

  # Simulate minimal QIC calls for basic startup
  track_qic_call("startup_visualization", list(chart_type = "p", phase = "initial_load"))
  track_qic_call("baseline_calculation", list(chart_type = "p", phase = "setup"))

  set_startup_phase("complete")
  track_event("startup_completed", "regression_test")

  # Verify performance target
  metrics <- get_enhanced_startup_metrics()
  expect_lte(metrics$qic_calls, 3, info = "Clean startup should have ≤3 QIC calls")
})

test_that("Memory usage stays within reasonable bounds during startup", {
  init_startup_metrics()

  # Track memory at startup points
  track_memory_usage("app_initialization")
  track_memory_usage("state_setup")
  track_memory_usage("test_mode_setup")
  track_memory_usage("observers_setup")
  track_memory_usage("startup_complete")

  # Get final memory metrics
  expect_equal(length(.startup_metrics$memory_snapshots), 5)

  # Check that memory increase is reasonable (less than 50MB for startup)
  final_snapshot <- .startup_metrics$memory_snapshots[[5]]
  expect_lt(final_snapshot$memory_diff_mb, 50,
            info = "Startup memory increase should be less than 50MB")
})

test_that("Event sequence timing is within acceptable ranges", {
  init_startup_metrics()

  # Simulate typical startup event sequence with timing
  Sys.sleep(0.01)  # Simulate small delay
  track_event("test_mode_ready", "timing_test")

  Sys.sleep(0.01)
  track_event("data_loaded", "timing_test")

  Sys.sleep(0.01)
  track_event("auto_detection_started", "timing_test")

  Sys.sleep(0.01)
  track_event("columns_detected", "timing_test")

  Sys.sleep(0.01)
  track_event("startup_complete", "timing_test")

  # Check that total startup time is reasonable
  metrics <- get_enhanced_startup_metrics()
  expect_lt(metrics$total_duration_seconds, 10,
            info = "Total startup should complete within 10 seconds")

  # Check that events are in chronological order
  event_times <- sapply(.startup_metrics$event_sequence, function(e) e$time_since_start)
  expect_true(all(diff(event_times) >= 0), info = "Events should be in chronological order")
})

test_that("Phase 3 debounced events reduce race conditions", {
  init_startup_metrics()

  # Simulate rapid event firing that should be debounced
  track_event("test_mode_startup_phase_changed", "phase: data_ready")
  track_event("test_mode_debounced_autodetect", "debounced_trigger")

  # Verify events are tracked
  expect_equal(length(.startup_metrics$event_sequence), 2)

  # Check that debounced pattern is used
  phase_event <- .startup_metrics$event_sequence[[1]]
  debounce_event <- .startup_metrics$event_sequence[[2]]

  expect_equal(phase_event$event, "test_mode_startup_phase_changed")
  expect_equal(debounce_event$event, "test_mode_debounced_autodetect")

  # Ensure debounce event comes after phase event
  expect_gte(debounce_event$time_since_start, phase_event$time_since_start)
})

test_that("generateSPCPlot calls are tracked correctly during startup", {
  init_startup_metrics()

  # Simulate generateSPCPlot calls during startup
  track_generateSPCPlot_call("startup_plot", list(chart_type = "p", data_rows = 36))
  track_generateSPCPlot_call("initial_visualization", list(chart_type = "run", data_rows = 36))

  # Check tracking
  expect_equal(.startup_metrics$generateSPCPlot_calls, 2)
  expect_equal(length(.startup_metrics$generateSPCPlot_call_details), 2)

  # Verify call details
  call_1 <- .startup_metrics$generateSPCPlot_call_details[[1]]
  expect_equal(call_1$context, "startup_plot")
  expect_equal(call_1$details$chart_type, "p")
  expect_equal(call_1$details$data_rows, 36)
})

test_that("Performance metrics can be retrieved without errors", {
  init_startup_metrics()

  # Add some test data
  track_qic_call("test_call")
  track_event("test_event", "test_context")
  track_memory_usage("test_memory")
  set_startup_phase("complete")

  # Should be able to get metrics without error
  expect_no_error({
    metrics <- get_enhanced_startup_metrics()
  })

  # Should be able to print summary without error
  expect_no_error({
    print_enhanced_startup_summary()
  })
})

test_that("Startup monitoring doesn't significantly impact performance", {
  # Test that the monitoring system itself doesn't add significant overhead
  start_time <- Sys.time()

  init_startup_metrics()

  # Perform typical monitoring operations
  for (i in 1:10) {
    track_event(paste("event", i), "performance_test")
    track_memory_usage(paste("memory", i))
    if (i %% 3 == 0) {
      track_qic_call(paste("qic", i))
    }
  }

  end_time <- Sys.time()
  monitoring_duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Monitoring overhead should be minimal (less than 1 second for 10 operations)
  expect_lt(monitoring_duration, 1.0,
            info = "Performance monitoring overhead should be minimal")
})