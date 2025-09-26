# baseline_measurement.R
# Script to establish baseline performance metrics for startup optimization

cat("=== BASELINE PERFORMANCE MEASUREMENT ===\n")
cat("Starting baseline measurement...\n")

# Load the app infrastructure
source('global.R')

# Initialize enhanced performance monitoring
init_startup_metrics()

cat("Enhanced performance monitoring initialized\n")

# Simulate typical startup sequence
set_startup_phase("initialization")

# Simulate data loading
track_event("data_loaded", "baseline_test")

# Simulate auto-detection
set_startup_phase("auto_detection")
track_event("auto_detection_started", "baseline_test")
track_event("columns_detected", "baseline_test")

# Simulate some QIC calls during startup
track_qic_call("startup_visualization", list(chart_type = "p", phase = "initial_load"))
track_generateSPCPlot_call("startup_plot", list(chart_type = "p", context = "baseline"))

# Simulate UI synchronization (CONSOLIDATED)
set_startup_phase("ui_sync")
track_event("ui_sync_requested", "baseline_test")  # Consolidated event
track_event("ui_sync_completed", "baseline_test")

# Final phase
set_startup_phase("ready")
track_event("test_mode_ready", "baseline_test")

# Wait a moment to simulate realistic timing
Sys.sleep(0.5)

cat("\n=== BASELINE RESULTS ===\n")
print_enhanced_startup_summary()

# Get the raw metrics for analysis
metrics <- get_enhanced_startup_metrics()

cat("\n=== ANALYSIS ===\n")
cat("Total Events:", length(metrics$event_sequence), "\n")
cat("Event Types:", length(metrics$events_fired), "\n")
cat("QIC Calls:", metrics$qic_calls, "\n")
cat("generateSPCPlot Calls:", metrics$generateSPCPlot_calls, "\n")

# Check for potential issues
if (length(metrics$event_sequence) > 10) {
  cat("⚠️  HIGH EVENT COUNT: Too many events during startup\n")
}

if (metrics$total_duration_seconds > 3) {
  cat("⚠️  SLOW STARTUP: Duration exceeds 3 seconds\n")
}

cat("\nBaseline measurement complete.\n")