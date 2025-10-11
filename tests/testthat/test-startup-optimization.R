# test-startup-optimization.R
# H18: Startup optimization test harnesses
# Tests for startup performance monitoring and optimization infrastructure

# Skip all tests if performance monitoring not available
skip_if_not(exists("init_startup_metrics", mode = "function"),
  "Performance monitoring functions not available")

test_that("init_startup_metrics initializes correctly", {
  # SETUP
  if (exists(".startup_metrics", where = ".GlobalEnv")) {
    rm(".startup_metrics", envir = .GlobalEnv)
  }

  # TEST: Initialize startup metrics
  init_startup_metrics()

  # Verify structure
  expect_true(exists("start_time", envir = .startup_metrics))
  expect_true(exists("phase_times", envir = .startup_metrics))
  expect_true(exists("qic_calls", envir = .startup_metrics))
  expect_true(exists("generateSPCPlot_calls", envir = .startup_metrics))
  expect_true(exists("events_fired", envir = .startup_metrics))
  expect_true(exists("startup_phase", envir = .startup_metrics))

  # Verify initial values
  expect_equal(.startup_metrics$qic_calls, 0)
  expect_equal(.startup_metrics$generateSPCPlot_calls, 0)
  expect_equal(.startup_metrics$startup_phase, "initializing")
  expect_type(.startup_metrics$phase_times, "list")
  expect_type(.startup_metrics$events_fired, "list")
})

test_that("track_qic_call increments counter and records context", {
  # SETUP
  init_startup_metrics()

  # TEST: Track first call
  track_qic_call("test_context", list(chart_type = "run"))

  expect_equal(.startup_metrics$qic_calls, 1)
  expect_true(exists("qic_call_details", envir = .startup_metrics))
  expect_equal(length(.startup_metrics$qic_call_details), 1)

  # Verify call details
  call_info <- .startup_metrics$qic_call_details[[1]]
  expect_equal(call_info$call_number, 1)
  expect_equal(call_info$context, "test_context")
  expect_equal(call_info$details$chart_type, "run")
  expect_true(inherits(call_info$timestamp, c("POSIXct", "POSIXt")))

  # TEST: Track second call
  track_qic_call("another_context")
  expect_equal(.startup_metrics$qic_calls, 2)
  expect_equal(length(.startup_metrics$qic_call_details), 2)
})

test_that("track_generateSPCPlot_call increments counter", {
  # SETUP
  init_startup_metrics()

  # TEST: Track calls
  track_generateSPCPlot_call("plot_gen_1", list(data_rows = 50))
  expect_equal(.startup_metrics$generateSPCPlot_calls, 1)

  track_generateSPCPlot_call("plot_gen_2")
  expect_equal(.startup_metrics$generateSPCPlot_calls, 2)

  # Verify details
  expect_true(exists("generateSPCPlot_call_details", envir = .startup_metrics))
  expect_equal(length(.startup_metrics$generateSPCPlot_call_details), 2)
})

test_that("track_event records events and counts", {
  # SETUP
  init_startup_metrics()

  # TEST: Track different events
  track_event("data_loaded", "file_upload")
  track_event("auto_detection_started", "triggered_by_data")
  track_event("data_loaded", "second_file")

  # Verify event counts
  expect_equal(.startup_metrics$events_fired$data_loaded, 2)
  expect_equal(.startup_metrics$events_fired$auto_detection_started, 1)

  # Verify event sequence
  expect_equal(length(.startup_metrics$event_sequence), 3)
  expect_equal(.startup_metrics$event_sequence[[1]]$event, "data_loaded")
  expect_equal(.startup_metrics$event_sequence[[2]]$event, "auto_detection_started")
  expect_equal(.startup_metrics$event_sequence[[3]]$event, "data_loaded")
})

test_that("set_startup_phase records phase transitions", {
  # SETUP
  init_startup_metrics()
  start_time <- .startup_metrics$start_time

  # TEST: Set phases
  # NOTE: No Sys.sleep - rely on system time precision
  set_startup_phase("data_ready")

  expect_equal(.startup_metrics$startup_phase, "data_ready")
  expect_true("data_ready" %in% names(.startup_metrics$phase_times))

  phase_info <- .startup_metrics$phase_times$data_ready
  expect_true(inherits(phase_info$start_time, c("POSIXct", "POSIXt")))
  expect_true(phase_info$time_since_start > 0)

  # TEST: Transition to next phase
  set_startup_phase("ui_ready")
  expect_equal(.startup_metrics$startup_phase, "ui_ready")
  expect_true("ui_ready" %in% names(.startup_metrics$phase_times))
})

test_that("get_enhanced_startup_metrics returns complete data", {
  # SETUP
  init_startup_metrics()

  track_qic_call("test", list())
  track_generateSPCPlot_call("plot_test", list())
  track_event("test_event", "test_context")
  set_startup_phase("data_ready")

  # TEST: Get metrics
  metrics <- get_enhanced_startup_metrics()

  # Verify structure
  expect_type(metrics, "list")
  expect_named(metrics, c(
    "qic_calls", "generateSPCPlot_calls",
    "start_time", "current_time", "total_duration_seconds",
    "current_phase",
    "events_fired", "event_sequence",
    "phase_times",
    "qic_call_details", "generateSPCPlot_call_details"
  ))

  # Verify values
  expect_equal(metrics$qic_calls, 1)
  expect_equal(metrics$generateSPCPlot_calls, 1)
  expect_equal(metrics$current_phase, "data_ready")
  expect_equal(metrics$events_fired$test_event, 1)
  expect_equal(length(metrics$event_sequence), 1)
})

test_that("startup metrics track memory usage", {
  # SETUP
  init_startup_metrics()

  # TEST: Track memory at different points
  track_startup_memory("initial")
  # Force slight time progression without sleep
  dummy_var <- replicate(10, sum(1:100))
  track_startup_memory("after_load")

  # Verify memory snapshots
  expect_true(exists("memory_snapshots", envir = .startup_metrics))
  expect_equal(length(.startup_metrics$memory_snapshots), 2)

  # Verify first snapshot
  snapshot1 <- .startup_metrics$memory_snapshots[[1]]
  expect_equal(snapshot1$context, "initial")
  expect_true(is.numeric(snapshot1$memory_mb))
  expect_true(snapshot1$memory_mb > 0)

  # Verify second snapshot
  snapshot2 <- .startup_metrics$memory_snapshots[[2]]
  expect_equal(snapshot2$context, "after_load")
  expect_true(snapshot2$time_since_start > snapshot1$time_since_start)
})

test_that("startup metrics handle concurrent operations", {
  # SETUP
  init_startup_metrics()

  # TEST: Concurrent QIC and generateSPCPlot calls
  track_qic_call("concurrent_1")
  track_generateSPCPlot_call("concurrent_plot_1")
  track_event("concurrent_event_1")

  track_qic_call("concurrent_2")
  track_generateSPCPlot_call("concurrent_plot_2")
  track_event("concurrent_event_2")

  # Verify all tracked
  expect_equal(.startup_metrics$qic_calls, 2)
  expect_equal(.startup_metrics$generateSPCPlot_calls, 2)
  expect_equal(length(.startup_metrics$event_sequence), 2)
})

test_that("reset_qic_counters clears counters", {
  # SETUP: Create counters in global env
  assign("qic_call_counter", 10, envir = .GlobalEnv)
  assign("actual_qic_call_counter", 5, envir = .GlobalEnv)

  # TEST: Reset
  reset_qic_counters()

  expect_false(exists("qic_call_counter", envir = .GlobalEnv))
  expect_false(exists("actual_qic_call_counter", envir = .GlobalEnv))
})

test_that("get_qic_call_counts returns current counts", {
  # SETUP: Set counters
  assign("qic_call_counter", 15, envir = .GlobalEnv)
  assign("actual_qic_call_counter", 8, envir = .GlobalEnv)

  # TEST: Get counts
  counts <- get_qic_call_counts()

  expect_type(counts, "list")
  expect_equal(counts$generateSPCPlot_calls, 15)
  expect_equal(counts$actual_qic_calls, 8)

  # CLEANUP
  reset_qic_counters()
})

test_that("get_qic_call_counts handles missing counters", {
  # SETUP: Ensure no counters exist
  reset_qic_counters()

  # TEST: Get counts with missing counters
  counts <- get_qic_call_counts()

  expect_equal(counts$generateSPCPlot_calls, 0)
  expect_equal(counts$actual_qic_calls, 0)
})

# ============================================================================
# PERFORMANCE BENCHMARKING HARNESS
# ============================================================================

test_that("startup metrics can measure realistic workflow", {
  skip_on_cran()
  skip_if_not(interactive(), "Performance test only for interactive sessions")

  # SETUP: Initialize metrics
  init_startup_metrics()

  # SIMULATE: Realistic startup sequence
  set_startup_phase("initializing")

  # Simulate data load
  track_event("data_loaded", "test_workflow")
  set_startup_phase("data_ready")

  # Simulate auto-detection
  track_event("auto_detection_started", "test_workflow")
  track_qic_call("autodetect", list(rows = 100))
  track_event("auto_detection_completed", "test_workflow")

  # Simulate plot generation
  track_generateSPCPlot_call("initial_plot", list(chart_type = "run"))
  set_startup_phase("ui_ready")

  # Simulate UI sync
  track_event("ui_sync_completed", "test_workflow")
  set_startup_phase("complete")

  # VERIFY: Workflow metrics
  metrics <- get_enhanced_startup_metrics()

  expect_equal(metrics$current_phase, "complete")
  expect_gte(metrics$qic_calls, 1)
  expect_gte(metrics$generateSPCPlot_calls, 1)
  expect_gte(length(metrics$event_sequence), 4)
  expect_true(metrics$total_duration_seconds > 0)
})

test_that("startup metrics detect performance bottlenecks", {
  skip_on_cran()

  # SETUP
  init_startup_metrics()

  # SIMULATE: Multiple QIC calls (potential bottleneck)
  for (i in 1:7) {
    track_qic_call(paste0("call_", i), list(iteration = i))
  }

  # TEST: Detect excessive QIC calls
  metrics <- get_enhanced_startup_metrics()

  # Bottleneck detection
  expect_gte(metrics$qic_calls, 5) # More than acceptable threshold

  # Verify all calls are tracked
  expect_equal(length(metrics$qic_call_details), 7)

  # Verify timing information
  for (i in 1:7) {
    expect_true(metrics$qic_call_details[[i]]$time_since_start >= 0)
  }
})

test_that("startup metrics support incremental analysis", {
  # SETUP
  init_startup_metrics()

  # TEST: Capture metrics at different points
  track_event("phase1", "start")
  metrics_phase1 <- get_enhanced_startup_metrics()

  # Force time progression
  dummy1 <- replicate(10, sum(1:100))

  track_event("phase2", "middle")
  metrics_phase2 <- get_enhanced_startup_metrics()

  # Force time progression
  dummy2 <- replicate(10, sum(1:100))

  track_event("phase3", "end")
  metrics_phase3 <- get_enhanced_startup_metrics()

  # VERIFY: Incremental progression
  expect_lt(metrics_phase1$total_duration_seconds, metrics_phase2$total_duration_seconds)
  expect_lt(metrics_phase2$total_duration_seconds, metrics_phase3$total_duration_seconds)

  expect_equal(length(metrics_phase1$event_sequence), 1)
  expect_equal(length(metrics_phase2$event_sequence), 2)
  expect_equal(length(metrics_phase3$event_sequence), 3)
})

# ============================================================================
# INTEGRATION WITH APP STARTUP
# ============================================================================

test_that("startup metrics integrate with app_state", {
  skip_on_cran()

  # SETUP: Create minimal app state
  app_state <- create_app_state()
  init_startup_metrics()

  # SIMULATE: Startup sequence with app_state
  set_startup_phase("initializing")

  # Data upload simulation
  test_data <- data.frame(x = 1:10, y = 11:20)
  app_state$data$current_data <- test_data
  track_event("data_loaded", "app_startup_test")

  # Auto-detection simulation
  app_state$columns$auto_detect$in_progress <- TRUE
  track_qic_call("app_autodetect", list(rows = nrow(test_data)))
  app_state$columns$auto_detect$completed <- TRUE

  # Plot generation simulation
  track_generateSPCPlot_call("app_plot", list(data_rows = nrow(test_data)))

  set_startup_phase("complete")

  # VERIFY: Metrics captured startup flow
  metrics <- get_enhanced_startup_metrics()

  expect_equal(metrics$current_phase, "complete")
  expect_gte(metrics$qic_calls, 1)
  expect_gte(metrics$generateSPCPlot_calls, 1)
  expect_true("data_loaded" %in% names(metrics$events_fired))
})

# ============================================================================
# CLEANUP
# ============================================================================

# Clean up after all tests
withr::defer({
  if (exists(".startup_metrics")) {
    rm(".startup_metrics", envir = .GlobalEnv)
  }
  reset_qic_counters()
}, envir = parent.frame())
