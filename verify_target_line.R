# Verification Script: Target Line Rendering
# Purpose: Verify that target_value is passed to BFHcharts and renders correctly
# Branch: feat/target-line-rendering

# Load app using global.R (development approach)
source("global.R")

# Create test data with known characteristics
set.seed(20251016)
test_data <- data.frame(
  month = seq.Date(from = as.Date("2024-01-01"), by = "month", length.out = 20),
  infections = round(rnorm(20, mean = 50, sd = 10))
)

cat("\n=== Target Line Rendering Verification ===\n\n")

# Test 1: Call BFH service with target_value
cat("Test 1: Calling compute_spc_results_bfh() with target_value = 45\n")

result <- compute_spc_results_bfh(
  data = test_data,
  x_var = "month",
  y_var = "infections",
  chart_type = "i",
  multiply = 1,
  target = 45  # Target value
)

cat("\n✓ Function executed without errors\n")

# Test 2: Verify plot structure
cat("\nTest 2: Inspecting plot structure for target line elements...\n")

if (!is.null(result$plot)) {
  plot_layers <- sapply(result$plot$layers, function(layer) class(layer$geom)[1])
  cat("Plot layers found:\n")
  print(plot_layers)

  # Check for target line (GeomHline or similar)
  has_hline <- any(grepl("Hline", plot_layers, ignore.case = TRUE))
  cat(ifelse(has_hline,
             "\n✓ Target line layer detected (GeomHline)\n",
             "\n⚠ No GeomHline layer found - target line may not be rendered\n"))

  # Check plot data for target column
  if (!is.null(result$plot$data) && "target" %in% names(result$plot$data)) {
    cat("✓ Target column exists in plot data\n")
    cat("  Target value:", unique(result$plot$data$target)[1], "\n")
  } else {
    cat("⚠ No target column in plot data\n")
  }
} else {
  cat("✗ Plot is NULL\n")
}

# Test 3: Check metadata
cat("\nTest 3: Checking metadata...\n")
if (!is.null(result$metadata)) {
  cat("Backend:", result$metadata$backend, "\n")
  if ("target_value" %in% names(result$metadata)) {
    cat("✓ target_value in metadata:", result$metadata$target_value, "\n")
  } else {
    cat("⚠ target_value not in metadata\n")
  }
}

# Test 4: Visual inspection (save plot)
cat("\nTest 4: Saving plot for visual inspection...\n")
if (!is.null(result$plot)) {
  png_file <- "verify_target_line_output.png"
  png(png_file, width = 800, height = 600)
  print(result$plot)
  dev.off()
  cat("✓ Plot saved to:", png_file, "\n")
  cat("  Please open the file to visually verify target line rendering\n")
}

cat("\n=== Verification Complete ===\n")
cat("\nExpected behavior:\n")
cat("- BFHcharts should render a horizontal target line at y = 45\n")
cat("- Target line should have distinct styling (color, linetype)\n")
cat("- Plot should show data points with target line overlay\n\n")

cat("Next steps:\n")
cat("1. Open", png_file, "and verify target line is visible\n")
cat("2. Check console logs for 'Target parameter included' debug message\n")
cat("3. If target line is missing, check BFHcharts version/compatibility\n")
