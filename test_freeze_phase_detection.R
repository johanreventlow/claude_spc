# Test Freeze and Phase Detection in BFH Service
# Verifies that freeze (frys) and phase (skift) columns are detected and applied

# Set environment
Sys.setenv(GOLEM_CONFIG_ACTIVE = "development")
Sys.setenv(SPC_LOG_LEVEL = "DEBUG")  # Enable debug logging

# Load package via source (matching app.R approach)
options(spc.debug.source_loading = TRUE)
source("global.R")

# Load ALL R files to ensure all dependencies are available
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (file in r_files) {
  source(file, local = FALSE)
}
cat(sprintf("✓ Loaded %d R files\n", length(r_files)))

# Create test data with freeze and phase columns
test_data <- data.frame(
  Dato = seq.Date(from = as.Date("2024-01-01"), by = "week", length.out = 30),
  Tæller = c(15, 18, 20, 17, 19, 22, 21, 18, 20, 23,
             25, 24, 22, 26, 28, 27, 25, 29, 30, 28,
             32, 31, 29, 33, 35, 34, 32, 36, 38, 37),
  Nævner = rep(100, 30),
  Frys = c(rep(FALSE, 9), TRUE, rep(FALSE, 20)),  # Freeze at row 10
  Skift = c(rep(FALSE, 14), TRUE, rep(FALSE, 15)), # Phase shift at row 15
  stringsAsFactors = FALSE
)

cat("=== Freeze and Phase Detection Test ===\n\n")
cat("Test data structure:\n")
cat(sprintf("  Rows: %d\n", nrow(test_data)))
cat(sprintf("  Dato: %s (%s to %s)\n", class(test_data$Dato)[1],
    test_data$Dato[1], test_data$Dato[nrow(test_data)]))
cat(sprintf("  Tæller: %s (range: %d to %d)\n", class(test_data$Tæller)[1],
    min(test_data$Tæller), max(test_data$Tæller)))
cat(sprintf("  Nævner: %s\n", class(test_data$Nævner)[1]))
cat(sprintf("  Frys: %s (TRUE at row %d)\n", class(test_data$Frys)[1],
    which(test_data$Frys)[1]))
cat(sprintf("  Skift: %s (TRUE at row %d)\n\n", class(test_data$Skift)[1],
    which(test_data$Skift)[1]))

cat("--- Calling compute_spc_results_bfh() with freeze_var and part_var ---\n")
cat("Watch for debug logs showing:\n")
cat("  1. Column sanitization (Frys → Frys, Skift → Skift)\n")
cat("  2. Freeze position detection\n")
cat("  3. Part boundary detection\n")
cat("  4. 'has_freeze = TRUE' and 'has_part = TRUE' in parameter mapping log\n\n")

result <- tryCatch(
  {
    compute_spc_results_bfh(
      data = test_data,
      x_var = "Dato",
      y_var = "Tæller",
      n_var = "Nævner",
      chart_type = "p",
      multiply = 100,
      chart_title_reactive = "Test P-Chart with Freeze and Phase",
      y_axis_unit = "percent",
      freeze_var = "Frys",
      part_var = "Skift"
    )
  },
  error = function(e) {
    cat(sprintf("\n✗ ERROR: %s\n", e$message))
    cat("   Review debug logs above to identify detection issue\n")
    return(NULL)
  }
)

cat("\n=== TEST RESULTS ===\n")
if (!is.null(result)) {
  cat("✓ SUCCESS: compute_spc_results_bfh() completed\n")
  cat(sprintf("  Result is ggplot: %s\n", inherits(result$plot, "ggplot")))
  cat(sprintf("  QIC data rows: %d\n", nrow(result$qic_data)))
  cat(sprintf("  Signals detected: %d\n", result$metadata$signals_detected))

  # Check if freeze was applied
  cat(sprintf("  Freeze applied: %s\n", result$metadata$freeze_applied))

  # Debug: Show qic_data columns
  cat(sprintf("  QIC data columns: %s\n", paste(names(result$qic_data), collapse = ", ")))

  # Check for phase column in qic_data
  has_parts <- "part" %in% names(result$qic_data)
  cat(sprintf("  Part column present: %s\n", has_parts))
  if (has_parts) {
    n_phases <- length(unique(result$qic_data$part))
    cat(sprintf("  Number of phases detected: %d\n", n_phases))
  }

  cat("\n✓ If you see:\n")
  cat("  - has_freeze = TRUE in logs\n")
  cat("  - has_part = TRUE in logs\n")
  cat("  - Freeze applied: TRUE\n")
  cat("  - Number of phases > 1\n")
  cat("  → Then detection is working! Bug is elsewhere (visual rendering)\n\n")
  cat("✗ If you see:\n")
  cat("  - has_freeze = FALSE or has_part = FALSE in logs\n")
  cat("  → Then detection is failing - need to debug sanitization or value parsing\n")
} else {
  cat("✗ FAILED: compute_spc_results_bfh() returned NULL\n")
  cat("\nReview debug logs above to identify where freeze/phase detection fails\n")
}

cat("\n=== END TEST ===\n")
