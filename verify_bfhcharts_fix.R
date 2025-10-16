# Verification Script: BFHcharts centerline_value Support
# Purpose: Test if BFHcharts now supports centerline_value parameter
# Date: 2025-10-16

cat("\n=== BFHcharts centerline_value Support Verification ===\n\n")

# Step 1: Load updated BFHcharts from source
cat("Step 1: Loading updated BFHcharts from source...\n")
bfhcharts_path <- "/Users/johanreventlow/Documents/R/BFHcharts"

if (!dir.exists(bfhcharts_path)) {
  stop("BFHcharts source not found at: ", bfhcharts_path)
}

# Unload any existing BFHcharts first
if ("package:BFHcharts" %in% search()) {
  detach("package:BFHcharts", unload = TRUE)
  cat("  Unloaded existing BFHcharts package\n")
}

# Load from source using pkgload
if (!requireNamespace("pkgload", quietly = TRUE)) {
  install.packages("pkgload")
}

tryCatch({
  pkgload::load_all(bfhcharts_path, quiet = TRUE)
  cat("✓ BFHcharts loaded from source:", bfhcharts_path, "\n")
}, error = function(e) {
  stop("Failed to load BFHcharts: ", e$message)
})

# Step 2: Load SPCify
cat("\nStep 2: Loading SPCify...\n")
source("global.R")
cat("✓ SPCify loaded\n")

# Step 3: Create test data
cat("\nStep 3: Creating test data...\n")
set.seed(20251016)
test_data <- data.frame(
  month = seq.Date(from = as.Date("2024-01-01"), by = "month", length.out = 20),
  infections = round(rnorm(20, mean = 50, sd = 10))
)
cat("✓ Test data created (20 observations)\n")

# Step 4: Test centerline_value parameter
cat("\nStep 4: Testing centerline_value parameter...\n")
cat("  Calling compute_spc_results_bfh() with centerline_value = 45\n")

result <- tryCatch({
  compute_spc_results_bfh(
    data = test_data,
    x_var = "month",
    y_var = "infections",
    chart_type = "i",
    multiply = 1,
    centerline_value = 45  # Custom centerline
  )
}, error = function(e) {
  list(error = e$message, plot = NULL)
})

if (!is.null(result$error)) {
  cat("✗ ERROR:", result$error, "\n")
  cat("\n=== Test FAILED ===\n")
  cat("BFHcharts still rejects centerline_value parameter\n")
  quit(status = 1)
}

cat("✓ Function executed without errors\n")

# Step 5: Inspect plot structure
cat("\nStep 5: Inspecting plot structure for centerline elements...\n")

if (!is.null(result$plot)) {
  plot_layers <- sapply(result$plot$layers, function(layer) class(layer$geom)[1])
  cat("Plot layers found:\n")
  print(plot_layers)

  # Check for centerline/baseline layer
  has_centerline <- any(grepl("Hline|Line", plot_layers, ignore.case = TRUE))
  cat(ifelse(has_centerline,
             "\n✓ Line layers detected - centerline may be rendered\n",
             "\n⚠ No obvious centerline layer found\n"))

  # Check plot data for centerline column
  if (!is.null(result$plot$data)) {
    centerline_cols <- grep("centerline|baseline|center",
                           names(result$plot$data),
                           ignore.case = TRUE,
                           value = TRUE)
    if (length(centerline_cols) > 0) {
      cat("✓ Centerline-related columns in plot data:\n")
      print(centerline_cols)
      for (col in centerline_cols) {
        unique_vals <- unique(result$plot$data[[col]])
        cat("  ", col, "values:", paste(unique_vals, collapse = ", "), "\n")
      }
    } else {
      cat("⚠ No centerline-related columns in plot data\n")
    }
  }
} else {
  cat("✗ Plot is NULL\n")
}

# Step 6: Check metadata
cat("\nStep 6: Checking metadata...\n")
if (!is.null(result$metadata)) {
  cat("Backend:", result$metadata$backend, "\n")
  if ("centerline_value" %in% names(result$metadata)) {
    cat("✓ centerline_value in metadata:", result$metadata$centerline_value, "\n")
  } else {
    cat("⚠ centerline_value not in metadata\n")
  }
}

# Step 7: Save plot for visual inspection
cat("\nStep 7: Saving plot for visual inspection...\n")
if (!is.null(result$plot)) {
  png_file <- "verify_bfhcharts_centerline.png"
  png(png_file, width = 800, height = 600)
  print(result$plot)
  dev.off()
  cat("✓ Plot saved to:", png_file, "\n")
  cat("  Please open the file to visually verify centerline rendering\n")
}

# Step 8: Summary
cat("\n=== Verification Complete ===\n\n")

if (is.null(result$error)) {
  cat("✓ SUCCESS: BFHcharts accepted centerline_value parameter!\n")
  cat("\nExpected visual result:\n")
  cat("- Horizontal centerline at y = 45\n")
  cat("- Distinct styling (color, linetype) different from control limits\n")
  cat("- Data points with centerline overlay\n\n")

  cat("Next steps:\n")
  cat("1. Open", png_file, "and verify centerline is visible\n")
  cat("2. If centerline renders correctly, update documentation\n")
  cat("3. Run automated tests to ensure integration works\n")
  cat("4. Consider testing with percentage charts (scale normalization)\n\n")
} else {
  cat("✗ FAILED: BFHcharts still rejects centerline_value\n")
  cat("Check BFHcharts implementation and parameter name\n\n")
}
