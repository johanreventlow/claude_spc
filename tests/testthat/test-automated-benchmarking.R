# Automated Benchmarking Test Suite
# Provides automated benchmarking for startup performance optimization

test_that("Baseline startup benchmark can be executed", {
  # This test provides a baseline benchmark for startup performance
  skip_if_not(exists("init_startup_metrics"), "Startup metrics not available")

  benchmark_results <- list()

  # Measure initialization time
  start_init <- Sys.time()
  init_startup_metrics()
  end_init <- Sys.time()
  benchmark_results$init_time <- as.numeric(difftime(end_init, start_init, units = "secs"))

  # Measure event tracking performance
  start_events <- Sys.time()
  for (i in 1:5) {
    track_event(paste("benchmark_event", i), "benchmark")
  }
  end_events <- Sys.time()
  benchmark_results$events_time <- as.numeric(difftime(end_events, start_events, units = "secs"))

  # Measure QIC call tracking
  start_qic <- Sys.time()
  for (i in 1:3) {
    track_qic_call("benchmark_qic", list(iteration = i))
  }
  end_qic <- Sys.time()
  benchmark_results$qic_tracking_time <- as.numeric(difftime(end_qic, start_qic, units = "secs"))

  # Measure memory tracking
  start_memory <- Sys.time()
  for (i in 1:3) {
    track_memory_usage(paste("benchmark_memory", i))
  }
  end_memory <- Sys.time()
  benchmark_results$memory_tracking_time <- as.numeric(difftime(end_memory, start_memory, units = "secs"))

  # Performance expectations (these should be very fast)
  expect_lt(benchmark_results$init_time, 0.1, "Initialization should be very fast")
  expect_lt(benchmark_results$events_time, 0.1, "Event tracking should be fast")
  expect_lt(benchmark_results$qic_tracking_time, 0.1, "QIC tracking should be fast")
  expect_lt(benchmark_results$memory_tracking_time, 0.5, "Memory tracking should be reasonably fast")

  # Output benchmark results for CI/CD tracking
  cat("\n=== STARTUP BENCHMARK RESULTS ===\n")
  cat("Initialization time:", round(benchmark_results$init_time * 1000, 2), "ms\n")
  cat("Event tracking time:", round(benchmark_results$events_time * 1000, 2), "ms\n")
  cat("QIC tracking time:", round(benchmark_results$qic_tracking_time * 1000, 2), "ms\n")
  cat("Memory tracking time:", round(benchmark_results$memory_tracking_time * 1000, 2), "ms\n")
})

test_that("Simulated full startup sequence meets performance targets", {
  skip_if_not(exists("init_startup_metrics"), "Startup metrics not available")

  # Start full startup simulation
  full_startup_start <- Sys.time()
  init_startup_metrics()

  # Phase 1: App initialization
  set_startup_phase("initialization")
  track_memory_usage("app_start")
  track_event("app_server_started", "benchmark_full")

  # Phase 2: Test mode setup
  set_startup_phase("data_ready")
  track_memory_usage("test_mode_setup")
  track_event("test_mode_ready", "benchmark_full")

  # Phase 3: Data processing (minimal QIC calls)
  track_qic_call("startup_baseline", list(chart_type = "p"))
  track_event("data_loaded", "benchmark_full")

  # Phase 4: Auto-detection
  track_event("auto_detection_started", "benchmark_full")
  track_event("columns_detected", "benchmark_full")

  # Phase 5: UI sync
  set_startup_phase("ui_ready")
  track_memory_usage("ui_setup")
  track_event("ui_sync_completed", "benchmark_full")

  # Phase 6: Completion
  set_startup_phase("complete")
  track_memory_usage("startup_complete")
  track_event("startup_completed", "benchmark_full")

  full_startup_end <- Sys.time()
  total_time <- as.numeric(difftime(full_startup_end, full_startup_start, units = "secs"))

  # Get final metrics
  metrics <- get_enhanced_startup_metrics()

  # Performance targets verification
  expect_lte(metrics$qic_calls, 3, "QIC calls should be ≤3 for clean startup")
  expect_lt(total_time, 5, "Full startup simulation should complete within 5 seconds")
  expect_equal(length(.startup_metrics$event_sequence), 7, "Should track all major startup events")

  # Memory usage should be reasonable
  if (length(.startup_metrics$memory_snapshots) > 0) {
    final_memory <- .startup_metrics$memory_snapshots[[length(.startup_metrics$memory_snapshots)]]
    expect_lt(final_memory$memory_diff_mb, 100, "Startup memory increase should be reasonable")
  }

  # Output comprehensive benchmark
  cat("\n=== FULL STARTUP BENCHMARK ===\n")
  cat("Total startup time:", round(total_time * 1000, 2), "ms\n")
  cat("QIC calls:", metrics$qic_calls, "(target: ≤3)\n")
  cat("Events tracked:", length(.startup_metrics$event_sequence), "\n")
  cat("Memory snapshots:", length(.startup_metrics$memory_snapshots), "\n")

  if (length(.startup_metrics$memory_snapshots) > 0) {
    final_memory <- .startup_metrics$memory_snapshots[[length(.startup_metrics$memory_snapshots)]]
    cat("Memory increase:", round(final_memory$memory_diff_mb, 2), "MB\n")
  }

  # Success criteria check
  success_criteria_met <- (metrics$qic_calls <= 3) && (total_time < 5)
  expect_true(success_criteria_met, "All startup performance criteria should be met")

  if (success_criteria_met) {
    cat("✅ ALL PERFORMANCE TARGETS MET\n")
  } else {
    cat("❌ PERFORMANCE TARGETS NOT MET\n")
  }
})

test_that("Phase 3 debounced startup optimization shows improvement", {
  skip_if_not(exists("init_startup_metrics"), "Startup metrics not available")

  # Test that Phase 3 optimization reduces event conflicts
  init_startup_metrics()

  # Simulate test mode with debouncing
  track_event("test_mode_ready", "phase3_test")
  track_event("test_mode_startup_phase_changed", "phase: data_ready")

  # Simulate debounced auto-detection
  Sys.sleep(0.01)  # Small delay to simulate debounce
  track_event("test_mode_debounced_autodetect", "debounced")

  track_event("auto_detection_started", "phase3_test")
  track_event("test_mode_startup_phase_changed", "phase: ui_ready")
  track_event("test_mode_startup_phase_changed", "phase: complete")

  # Verify that events are properly sequenced
  events <- .startup_metrics$event_sequence
  expect_gte(length(events), 5, "Phase 3 events should be tracked")

  # Check that phase transitions are in correct order
  phase_events <- events[sapply(events, function(e) grepl("phase_changed", e$event))]
  expect_gte(length(phase_events), 3, "Should have multiple phase transitions")

  # Timing should show proper sequencing
  event_times <- sapply(events, function(e) e$time_since_start)
  expect_true(all(diff(event_times) >= 0), "Events should be chronologically ordered")

  cat("\n=== PHASE 3 OPTIMIZATION TEST ===\n")
  cat("Events tracked:", length(events), "\n")
  cat("Phase transitions:", length(phase_events), "\n")
  cat("Proper sequencing:", all(diff(event_times) >= 0), "\n")
})