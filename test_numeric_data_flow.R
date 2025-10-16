# Test Numeric Data Flow Through BFH Service
# Verifies that numeric data is preserved through column sanitization

# Set environment
Sys.setenv(GOLEM_CONFIG_ACTIVE = "development")

# Load package (with all debug logging)
library(SPCify)

# Create test data - EXACTLY matching user's structure
test_data <- data.frame(
  Dato = seq.Date(from = as.Date("2024-01-01"), by = "week", length.out = 20),
  Tæller = c(15, 18, 20, 17, 19, 22, 21, 18, 20, 23,
             25, 24, 22, 26, 28, 27, 25, 29, 30, 28),
  Nævner = rep(100, 20),
  stringsAsFactors = FALSE
)

cat("=== Numeric Data Flow Test ===\n\n")
cat("Initial data types:\n")
cat(sprintf("  Dato: %s\n", class(test_data$Dato)[1]))
cat(sprintf("  Tæller: %s\n", class(test_data$Tæller)[1]))
cat(sprintf("  Nævner: %s\n", class(test_data$Nævner)[1]))
cat(sprintf("  First 3 Tæller values: %s\n\n", paste(head(test_data$Tæller, 3), collapse = ", ")))

# Test compute_spc_results_bfh with p-chart (uses both y and n)
cat("--- Calling compute_spc_results_bfh() ---\n")
cat("Watch for debug logs showing data types at each stage...\n\n")

result <- tryCatch(
  {
    compute_spc_results_bfh(
      data = test_data,
      x_var = "Dato",
      y_var = "Tæller",
      n_var = "Nævner",
      chart_type = "p",
      multiply = 100,
      chart_title_reactive = "Test P-Chart",
      y_axis_unit = "percent"
    )
  },
  error = function(e) {
    cat(sprintf("\n✗ ERROR: %s\n", e$message))
    cat(sprintf("   Call stack: %s\n", paste(deparse(sys.calls()), collapse = "\n")))
    return(NULL)
  }
)

if (!is.null(result)) {
  cat("\n✓ SUCCESS: compute_spc_results_bfh() completed\n")
  cat(sprintf("  Result is ggplot: %s\n", inherits(result$plot, "ggplot")))
  cat(sprintf("  QIC data rows: %d\n", nrow(result$qic_data)))
  cat(sprintf("  Signals detected: %d\n", result$metadata$signals_detected))
} else {
  cat("\n✗ FAILED: compute_spc_results_bfh() returned NULL\n")
  cat("\nReview debug logs above to identify where numeric → character conversion occurs\n")
}

cat("\n=== END TEST ===\n")
