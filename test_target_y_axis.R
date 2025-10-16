# Test Target Value and Y-Axis Formatting (Bug #1 + #4)
# Verifies that target_value, y_axis_unit, and chart_title work with BFHcharts low-level API

# Set environment
Sys.setenv(GOLEM_CONFIG_ACTIVE = "development")
Sys.setenv(SPC_LOG_LEVEL = "DEBUG")

# Load package via source (matching app.R approach)
options(spc.debug.source_loading = TRUE)
source("global.R")

# Load ALL R files to ensure all dependencies are available
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (file in r_files) {
  source(file, local = FALSE)
}
cat(sprintf("✓ Loaded %d R files\n", length(r_files)))

# Create test data for P-chart (percentage-based)
test_data <- data.frame(
  Dato = seq.Date(from = as.Date("2024-01-01"), by = "week", length.out = 20),
  Komplikationer = c(12, 15, 18, 14, 16, 19, 17, 15, 18, 20,
                     22, 21, 19, 23, 25, 24, 22, 26, 28, 27),
  Procedurer = rep(100, 20),
  stringsAsFactors = FALSE
)

cat("=== Target Value and Y-Axis Formatting Test ===\n\n")
cat("Test data structure:\n")
cat(sprintf("  Rows: %d\n", nrow(test_data)))
cat(sprintf("  Dato: %s to %s\n", test_data$Dato[1], test_data$Dato[nrow(test_data)]))
cat(sprintf("  Komplikationer: %d to %d\n", min(test_data$Komplikationer), max(test_data$Komplikationer)))
cat(sprintf("  Procedurer: %d\n\n", test_data$Procedurer[1]))

cat("--- Calling compute_spc_results_bfh() with target, y_axis_unit, and chart_title ---\n")
cat("Parameters:\n")
cat("  chart_type: p\n")
cat("  multiply: 100 (display as percentages)\n")
cat("  target_value: 18 (should appear on plot)\n")
cat("  y_axis_unit: percent (should format y-axis)\n")
cat("  chart_title: Test P-Chart with Target\n\n")

result <- tryCatch(
  {
    compute_spc_results_bfh(
      data = test_data,
      x_var = "Dato",
      y_var = "Komplikationer",
      n_var = "Procedurer",
      chart_type = "p",
      multiply = 100,
      target_value = 18,
      y_axis_unit = "percent",
      chart_title = "Test P-Chart with Target"
    )
  },
  error = function(e) {
    cat(sprintf("\n✗ ERROR: %s\n", e$message))
    cat("   Stack trace:\n")
    print(e)
    return(NULL)
  }
)

cat("\n=== TEST RESULTS ===\n")
if (!is.null(result)) {
  cat("✓ SUCCESS: compute_spc_results_bfh() completed\n")
  cat(sprintf("  Result is ggplot: %s\n", inherits(result$plot, "ggplot")))
  cat(sprintf("  QIC data rows: %d\n", nrow(result$qic_data)))
  cat(sprintf("  Signals detected: %d\n", result$metadata$signals_detected))
  cat(sprintf("  Chart type: %s\n", result$metadata$chart_type))

  # Check for target line in plot layers
  plot_layers <- sapply(result$plot$layers, function(l) class(l$geom)[1])
  cat(sprintf("  Plot layers: %s\n", paste(plot_layers, collapse = ", ")))

  # Check plot title
  plot_title <- result$plot$labels$title
  cat(sprintf("  Plot title: %s\n", if (!is.null(plot_title)) plot_title else "NULL"))

  # Check y-axis label
  y_label <- result$plot$labels$y
  cat(sprintf("  Y-axis label: %s\n", if (!is.null(y_label)) y_label else "NULL"))

  cat("\n✓ If you see:\n")
  cat("  - Plot title matches 'Test P-Chart with Target'\n")
  cat("  - Y-axis label indicates percentage formatting\n")
  cat("  - Target line visible in plot (GeomHline or similar)\n")
  cat("  → Then Bug #1 and #4 are FIXED!\n\n")

  cat("✗ If you see:\n")
  cat("  - Plot title is NULL or missing\n")
  cat("  - Y-axis label is generic 'y'\n")
  cat("  - No target line in plot layers\n")
  cat("  → Then parameters are not being passed correctly\n")

  # Save plot for manual inspection
  cat("\nSaving plot to test_target_plot.png for manual inspection...\n")
  tryCatch({
    ggplot2::ggsave(
      filename = "test_target_plot.png",
      plot = result$plot,
      width = 10,
      height = 6,
      dpi = 150
    )
    cat("✓ Plot saved successfully\n")
  }, error = function(e) {
    cat(sprintf("✗ Failed to save plot: %s\n", e$message))
  })
} else {
  cat("✗ FAILED: compute_spc_results_bfh() returned NULL\n")
  cat("\nReview debug logs above to identify where two-stage workflow failed\n")
}

cat("\n=== END TEST ===\n")
