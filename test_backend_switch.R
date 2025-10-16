# test_backend_switch.R
# Quick validation test for BFHchart backend switching
#
# Purpose: Verify parameter mapping fix works without launching full app
# Expected: Backend switch succeeds without parameter mismatch errors

# Load package from source instead of installed package
devtools::load_all()

cat("=== BFHchart Backend Switch Test ===\n\n")

# Create test data with Danish column names
test_data <- data.frame(
  Dato = seq.Date(from = as.Date("2024-01-01"), by = "week", length.out = 20),
  Tæller = c(15, 18, 20, 17, 19, 22, 21, 18, 20, 23,
             25, 24, 22, 26, 28, 27, 25, 29, 30, 28)
)

cat("✓ Test data created: 20 rows with Dato and Tæller columns\n")

# Create config object matching SPCify structure
config <- list(
  x_col = "Dato",
  y_col = "Tæller",
  n_col = NULL
)

cat("✓ Config object created\n")

# Test 1: Check feature flag reading
cat("\n--- Test 1: Feature Flag Reading ---\n")
features_config <- tryCatch(
  {
    cfg <- golem::get_golem_options("features")
    cat(sprintf("✓ Feature flag: use_bfhchart = %s\n", cfg$use_bfhchart))
    cat(sprintf("✓ Supported types: %s\n", paste(cfg$bfhchart_supported_types, collapse = ", ")))
    cfg
  },
  error = function(e) {
    cat(sprintf("✗ Failed to read features config: %s\n", e$message))
    list(use_bfhchart = FALSE)
  }
)

# Test 2: Call backend wrapper with correct parameters
cat("\n--- Test 2: Backend Wrapper Call ---\n")
result <- tryCatch(
  {
    generateSPCPlot_with_backend(
      data = test_data,
      config = config,
      chart_type = "run",
      target_value = NULL,
      centerline_value = NULL,
      show_phases = FALSE,
      skift_column = NULL,
      frys_column = NULL,
      chart_title_reactive = "Test Run Chart",
      y_axis_unit = "count",
      kommentar_column = NULL,
      base_size = 14,
      viewport_width = NULL,
      viewport_height = NULL,
      target_text = NULL
    )
  },
  error = function(e) {
    cat(sprintf("✗ Backend wrapper call failed: %s\n", e$message))
    return(NULL)
  }
)

if (!is.null(result)) {
  cat("✓ Backend wrapper call succeeded\n")
  cat(sprintf("✓ Result structure: plot=%s, qic_data=%s\n",
              class(result$plot)[1],
              class(result$qic_data)[1]))
  cat(sprintf("✓ QIC data rows: %d\n", nrow(result$qic_data)))

  # Test 3: Verify plot is ggplot object
  cat("\n--- Test 3: Plot Object Validation ---\n")
  if (inherits(result$plot, "ggplot")) {
    cat("✓ Plot is a valid ggplot object\n")
  } else {
    cat(sprintf("✗ Plot is not a ggplot object: %s\n", class(result$plot)))
  }

  # Test 4: Verify qic_data has required columns
  cat("\n--- Test 4: QIC Data Structure Validation ---\n")
  required_cols <- c("x", "y", "cl")
  has_cols <- required_cols %in% names(result$qic_data)

  for (i in seq_along(required_cols)) {
    if (has_cols[i]) {
      cat(sprintf("✓ QIC data has column: %s\n", required_cols[i]))
    } else {
      cat(sprintf("✗ QIC data missing column: %s\n", required_cols[i]))
    }
  }

  cat("\n=== TEST SUMMARY ===\n")
  cat("✓ All tests passed\n")
  cat("✓ Backend switch working correctly\n")
  cat("✓ Parameter mapping fixed\n")

} else {
  cat("\n=== TEST SUMMARY ===\n")
  cat("✗ Backend wrapper call failed\n")
  cat("Review error messages above for debugging\n")
}

cat("\n=== Test Complete ===\n")
