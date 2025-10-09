# Data Load Performance Test Script
# Måler hvor mange gange QIC beregning køres når data indlæses

# Manual test instruktioner for at måle qic-beregning ved data indlæsning
test_data_load_manual <- function() {
  cat("=== Manual Data Load Performance Test ===\n\n")

  cat("INSTRUKTIONER:\n")
  cat("1. Åbn ny R session og kør:\n")
  cat("   source('global.R')\n")
  cat("   reset_qic_counters()\n")
  cat("   run_app(log_level = 'DEBUG', port = 4040)\n\n")

  cat("2. Vent på app startup (app åbner i browser)\n\n")

  cat("3. I app'en:\n")
  cat("   - Upload en CSV fil ELLER\n")
  cat("   - Klik på 'Test Data' for at indlæse eksempeldata\n\n")

  cat("4. I R console, kør straks efter data indlæsning:\n")
  cat("   get_qic_call_counts()\n\n")

  cat("5. Analyser resultaterne:\n")
  cat("   - generateSPCPlot_calls: Antal gange plot-funktionen kaldes\n")
  cat("   - actual_qic_calls: Antal gange qicharts2::qic() køres\n\n")

  cat("FORVENTEDE RESULTATER:\n")
  cat("✅ OPTIMALT:\n")
  cat("   - generateSPCPlot_calls: 1-2 (initial + efter kolonne-detection)\n")
  cat("   - actual_qic_calls: 1-2 (samme som generateSPCPlot)\n\n")

  cat("⚠️  PROBLEM-INDIKATORER:\n")
  cat("   - generateSPCPlot_calls > 5: Unødvendige genberegninger\n")
  cat("   - actual_qic_calls >> generateSPCPlot_calls: Caching problemer\n")
  cat("   - Gentagne kald efter data indlæst: Reactive loops\n\n")

  cat("6. Test også kolonneskift:\n")
  cat("   - Skift Y-kolonne i dropdown\n")
  cat("   - Kør get_qic_call_counts() igen\n")
  cat("   - Burde kun give 1 ekstra qic-beregning\n\n")

  cat("DEBUGGING:\n")
  cat("Se i R console efter:\n")
  cat("   [SPC_CALC_DEBUG] generateSPCPlot CALL # [nummer]\n")
  cat("   [QIC_CALL_DEBUG] ACTUAL qic() CALL # [nummer]\n\n")

  cat("Hvis der er for mange kald, søg efter pattern i reactive chains\n")
}

# Automated test version (simplified)
test_data_load_automated <- function() {
  cat("=== Automated Data Load Performance Test ===\n\n")

  # This requires shinytest2 or similar to interact with the app
  # For now, just provide the framework

  cat("Automated test kræver shinytest2 integration\n")
  cat("Brug test_data_load_manual() for manuel test\n")
}

# Performance benchmark with test data
benchmark_with_test_data <- function() {
  cat("=== Benchmark QIC Performance with Test Data ===\n\n")

  # Load via pkgload or fall back to global.R
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(here::here(), quiet = TRUE)
  } else {
    source("global.R")
  }

  # Reset counters
  reset_qic_counters()

  # Load test data
  if (file.exists("R/data/spc_testdata_p_chart.csv")) {
    test_data <- read.csv("R/data/spc_testdata_p_chart.csv", stringsAsFactors = FALSE)
    cat("Loaded test data:", nrow(test_data), "rows\n")

    # Simulate plot generation
    config <- list(
      x_col = "Dato",
      y_col = "Tæller",
      n_col = "Nævner"
    )

    cat("Generating SPC plot...\n")
    start_time <- Sys.time()

    tryCatch({
      result <- generateSPCPlot(
        data = test_data,
        config = config,
        chart_type = "p"
      )

      end_time <- Sys.time()
      duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

      counts <- get_qic_call_counts()

      cat("\n=== BENCHMARK RESULTATER ===\n")
      cat("Duration:", round(duration, 3), "seconds\n")
      cat("generateSPCPlot calls:", counts$generateSPCPlot_calls, "\n")
      cat("Actual qic() calls:", counts$actual_qic_calls, "\n")
      cat("Data points processed:", nrow(test_data), "\n")
      cat("Performance:", round(nrow(test_data) / duration, 1), "rows/sec\n")

    }, error = function(e) {
      cat("Error in benchmark:", e$message, "\n")
    })

  } else {
    cat("Test data file not found\n")
    cat("Expected: R/data/spc_testdata_p_chart.csv\n")
  }
}

if (interactive()) {
  cat("Data load performance tests loaded.\n\n")
  cat("Available functions:\n")
  cat("  test_data_load_manual() - Manual test instructions\n")
  cat("  benchmark_with_test_data() - Performance benchmark\n")
}