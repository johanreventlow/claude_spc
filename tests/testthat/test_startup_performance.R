# Startup Performance Test Script
# Run this to measure how many times QIC calculations occur during app startup

# Load required functions
source("../../global.R")

# Function to test startup performance
test_startup_performance <- function() {
  cat("=== SPC App Startup Performance Test ===\n")
  cat("Dette script måler hvor mange gange qic-beregninger køres ved app startup\n\n")

  # Reset counters
  reset_qic_counters()

  cat("Starter app på port 4040 med DEBUG logging...\n")
  cat("Vent venligst 30 sekunder for startup-måling\n\n")

  # Start app in background with debug logging
  app_process <- callr::r_bg(function() {
    source("../../global.R")
    # Enable debug logging
    Sys.setenv(LOG_LEVEL = "DEBUG")
    run_app(log_level = "DEBUG", port = 4040, launch.browser = FALSE)
  })

  # Monitor for 30 seconds
  start_time <- Sys.time()
  initial_counts <- get_qic_call_counts()

  cat("Initial counts:\n")
  cat(paste("  generateSPCPlot calls:", initial_counts$generateSPCPlot_calls, "\n"))
  cat(paste("  Actual qic() calls:", initial_counts$actual_qic_calls, "\n\n"))

  # Wait for startup to complete
  Sys.sleep(30)

  final_counts <- get_qic_call_counts()
  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

  cat("Final counts after", round(duration, 1), "seconds:\n")
  cat(paste("  generateSPCPlot calls:", final_counts$generateSPCPlot_calls, "\n"))
  cat(paste("  Actual qic() calls:", final_counts$actual_qic_calls, "\n\n"))

  total_generateSPCPlot <- final_counts$generateSPCPlot_calls - initial_counts$generateSPCPlot_calls
  total_qic <- final_counts$actual_qic_calls - initial_counts$actual_qic_calls

  cat("=== RESULTAT ===\n")
  cat(paste("Total generateSPCPlot kald under startup:", total_generateSPCPlot, "\n"))
  cat(paste("Total qic() kald under startup:", total_qic, "\n"))
  cat(paste("Genberegninger pr. sekund:", round(total_generateSPCPlot / duration, 2), "\n"))

  if (total_generateSPCPlot > 3) {
    cat("\n⚠️  PROBLEM: Mere end 3 qic-beregninger under startup tyder på unødvendige genberegninger\n")
  } else if (total_generateSPCPlot <= 1) {
    cat("\n✅ GODT: Minimal antal qic-beregninger under startup\n")
  } else {
    cat("\n⚡ ACCEPTABELT: Moderat antal qic-beregninger under startup\n")
  }

  # Cleanup
  if (app_process$is_alive()) {
    app_process$kill()
    cat("\nApp proces stoppet\n")
  }

  cat("\nFor at se detaljeret log, kør: run_app(log_level = 'DEBUG', port = 4040)\n")
  cat("Og søg efter: 'generateSPCPlot CALL #' og 'ACTUAL qic() CALL #'\n")
}

# Manual test function for console use
test_startup_manual <- function() {
  cat("=== Manual Startup Performance Test ===\n")
  cat("1. Kør: reset_qic_counters()\n")
  cat("2. Kør: run_app(log_level = 'DEBUG', port = 4040)\n")
  cat("3. Vent på app startup (15-30 sekunder)\n")
  cat("4. Kør: get_qic_call_counts()\n")
  cat("5. Analyser resultaterne\n\n")

  cat("Forventet resultat for optimal performance:\n")
  cat("  - generateSPCPlot_calls: 0-2 (ingen data loadet endnu)\n")
  cat("  - actual_qic_calls: 0-1 (minimal beregning)\n\n")

  cat("Problem-indikatorer:\n")
  cat("  - generateSPCPlot_calls > 5: Unødvendige genberegninger\n")
  cat("  - actual_qic_calls > 3: Race conditions eller reactive loops\n")
}

# Run the test if script is executed directly
if (interactive()) {
  cat("Startup performance test loaded.\n")
  cat("Kør test_startup_performance() for automatisk test\n")
  cat("Eller test_startup_manual() for manual test instruktioner\n")
} else {
  test_startup_performance()
}