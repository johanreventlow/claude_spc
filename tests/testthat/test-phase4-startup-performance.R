# Phase 4: Startup Performance Monitoring Tests

test_that("Enhanced startup metrics initialization works", {
  # Initialize startup metrics
  init_startup_metrics()

  # Check that all expected fields are initialized
  expect_true(exists("start_time", envir = .startup_metrics))
  expect_true(exists("qic_calls", envir = .startup_metrics))
  expect_true(exists("generateSPCPlot_calls", envir = .startup_metrics))
  expect_true(exists("events_fired", envir = .startup_metrics))
  expect_true(exists("event_sequence", envir = .startup_metrics))

  # Phase 4 specific fields
  expect_true(exists("memory_snapshots", envir = .startup_metrics))
  expect_true(exists("initial_memory_mb", envir = .startup_metrics))

  # Check initial values
  expect_equal(.startup_metrics$qic_calls, 0)
  expect_equal(.startup_metrics$generateSPCPlot_calls, 0)
  expect_equal(length(.startup_metrics$events_fired), 0)
  expect_equal(length(.startup_metrics$event_sequence), 0)
})

test_that("QIC call tracking works correctly", {
  init_startup_metrics()

  # Track some QIC calls
  track_qic_call("test_context_1", list(chart_type = "p"))
  track_qic_call("test_context_2", list(chart_type = "run"))

  # Check counters
  expect_equal(.startup_metrics$qic_calls, 2)
  expect_equal(length(.startup_metrics$qic_call_details), 2)

  # Check details
  call_1 <- .startup_metrics$qic_call_details[[1]]
  expect_equal(call_1$context, "test_context_1")
  expect_equal(call_1$call_number, 1)
  expect_true(call_1$time_since_start >= 0)
})

test_that("Event tracking captures startup sequence", {
  init_startup_metrics()

  # Simulate startup event sequence
  track_event("test_mode_ready", "startup_sequence")
  track_event("data_loaded", "startup_sequence")
  track_event("auto_detection_started", "startup_sequence")
  track_event("columns_detected", "startup_sequence")

  # Check event sequence
  expect_equal(length(.startup_metrics$event_sequence), 4)
  expect_equal(.startup_metrics$event_sequence[[1]]$event, "test_mode_ready")
  expect_equal(.startup_metrics$event_sequence[[2]]$event, "data_loaded")
  expect_equal(.startup_metrics$event_sequence[[3]]$event, "auto_detection_started")
  expect_equal(.startup_metrics$event_sequence[[4]]$event, "columns_detected")

  # Check timing information
  for (i in 1:4) {
    expect_true(.startup_metrics$event_sequence[[i]]$time_since_start >= 0)
    expect_equal(.startup_metrics$event_sequence[[i]]$context, "startup_sequence")
  }
})

test_that("Memory tracking captures usage snapshots", {
  init_startup_metrics()

  # Track memory at different points
  track_memory_usage("test_point_1")
  track_memory_usage("test_point_2")

  # Check memory snapshots
  expect_equal(length(.startup_metrics$memory_snapshots), 2)
  expect_true(.startup_metrics$initial_memory_mb > 0)

  # Check snapshot structure
  snapshot_1 <- .startup_metrics$memory_snapshots[[1]]
  expect_equal(snapshot_1$context, "test_point_1")
  expect_true(snapshot_1$memory_mb > 0)
  expect_true(is.numeric(snapshot_1$memory_diff_mb))
  expect_true(snapshot_1$time_since_start >= 0)
})

test_that("Performance assessment works correctly", {
  init_startup_metrics()

  # Test excellent performance (≤3 QIC calls)
  track_qic_call("test1")
  track_qic_call("test2")
  track_qic_call("test3")

  metrics <- get_enhanced_startup_metrics()
  expect_equal(metrics$qic_calls, 3)

  # Test acceptable performance (4-5 calls)
  track_qic_call("test4")
  track_qic_call("test5")

  metrics <- get_enhanced_startup_metrics()
  expect_equal(metrics$qic_calls, 5)

  # Test needs optimization (>5 calls)
  track_qic_call("test6")

  metrics <- get_enhanced_startup_metrics()
  expect_equal(metrics$qic_calls, 6)
})

test_that("Startup phase tracking works", {
  init_startup_metrics()

  # Test phase progression
  set_startup_phase("data_ready")
  expect_equal(.startup_metrics$startup_phase, "data_ready")

  set_startup_phase("ui_ready")
  expect_equal(.startup_metrics$startup_phase, "ui_ready")

  set_startup_phase("complete")
  expect_equal(.startup_metrics$startup_phase, "complete")
})

test_that("generateSPCPlot call tracking works", {
  init_startup_metrics()

  # Track some generateSPCPlot calls
  track_generateSPCPlot_call("plot_generation", list(chart_type = "p"))
  track_generateSPCPlot_call("plot_generation", list(chart_type = "run"))

  # Check counters
  expect_equal(.startup_metrics$generateSPCPlot_calls, 2)
  expect_equal(length(.startup_metrics$generateSPCPlot_call_details), 2)
})