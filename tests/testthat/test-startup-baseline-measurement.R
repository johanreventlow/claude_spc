# tests/testthat/test-startup-baseline-measurement.R
# Test script for enhanced startup performance measurement

test_that("Enhanced performance monitoring functions work", {
  # Initialize monitoring
  init_startup_metrics()

  # Simulate some activity
  track_qic_call("test_context", list(chart_type = "p"))
  track_generateSPCPlot_call("test_context", list(chart_type = "run"))
  track_event("data_loaded", "test")
  track_event("columns_detected", "test")

  set_startup_phase("data_ready")

  # Wait briefly to ensure timing differences
  Sys.sleep(0.1)

  set_startup_phase("complete")

  # Get metrics
  metrics <- get_enhanced_startup_metrics()

  # Verify basic functionality
  expect_false(is.null(metrics))
  expect_false(!is.null(metrics$error))
  expect_equal(metrics$qic_calls, 1)
  expect_equal(metrics$generateSPCPlot_calls, 1)
  expect_equal(metrics$events_fired$data_loaded, 1)
  expect_equal(metrics$events_fired$columns_detected, 1)
  expect_equal(metrics$current_phase, "complete")
  expect_true(metrics$total_duration_seconds > 0)

  # Verify event sequence
  expect_equal(length(metrics$event_sequence), 2)
  expect_equal(metrics$event_sequence[[1]]$event, "data_loaded")
  expect_equal(metrics$event_sequence[[2]]$event, "columns_detected")

  # Verify phase timing
  expect_true(!is.null(metrics$phase_times$data_ready))
  expect_true(!is.null(metrics$phase_times$complete))
})

test_that("Performance assessment works correctly", {
  # Test with low call count (should be excellent)
  init_startup_metrics()
  track_qic_call("test1")
  track_qic_call("test2")

  metrics <- get_enhanced_startup_metrics()
  expect_equal(metrics$qic_calls, 2)

  # Capture output for assessment
  output <- capture.output(print_enhanced_startup_summary())
  expect_true(any(grepl("EXCELLENT", output)))

  # Test with high call count (should need optimization)
  init_startup_metrics()
  for(i in 1:8) {
    track_qic_call(paste("test", i))
  }

  metrics <- get_enhanced_startup_metrics()
  expect_equal(metrics$qic_calls, 8)

  output <- capture.output(print_enhanced_startup_summary())
  expect_true(any(grepl("NEEDS OPTIMIZATION", output)))
})

test_that("Error handling works when metrics not initialized", {
  # Clear any existing metrics by creating a new environment
  if (exists(".startup_metrics", envir = .GlobalEnv)) {
    rm(".startup_metrics", envir = .GlobalEnv)
  }

  metrics <- get_enhanced_startup_metrics()
  expect_true(!is.null(metrics$error))
  expect_true(grepl("not initialized", metrics$error))
})

test_that("Legacy compatibility with existing counters", {
  # Test that new system works alongside existing counter system
  reset_qic_counters()
  init_startup_metrics()

  # Legacy counters should start at 0
  legacy <- get_qic_call_counts()
  expect_equal(legacy$generateSPCPlot_calls, 0)
  expect_equal(legacy$actual_qic_calls, 0)

  # New system should also start at 0
  enhanced <- get_enhanced_startup_metrics()
  expect_equal(enhanced$qic_calls, 0)
  expect_equal(enhanced$generateSPCPlot_calls, 0)

  # Simulate some activity
  track_qic_call("test")
  track_generateSPCPlot_call("test")

  # New system should show activity
  enhanced <- get_enhanced_startup_metrics()
  expect_equal(enhanced$qic_calls, 1)
  expect_equal(enhanced$generateSPCPlot_calls, 1)
})