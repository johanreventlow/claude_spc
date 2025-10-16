# Simple BFH test without full package loading
# Tests just the BFH service layer directly

# Set GOLEM_CONFIG_ACTIVE before loading anything
Sys.setenv(GOLEM_CONFIG_ACTIVE = "development")

# Source only required files
source("/Users/johanreventlow/Documents/R/epic-bfhcharts-spc-migration/R/utils_safe_operation.R")
source("/Users/johanreventlow/Documents/R/epic-bfhcharts-spc-migration/R/config_log_contexts.R")
source("/Users/johanreventlow/Documents/R/epic-bfhcharts-spc-migration/R/utils_logging.R")
source("/Users/johanreventlow/Documents/R/epic-bfhcharts-spc-migration/R/fct_spc_bfh_service.R")
source("/Users/johanreventlow/Documents/R/epic-bfhcharts-spc-migration/R/config_chart_types.R")

# Create test data
test_data <- data.frame(
  Dato = seq.Date(from = as.Date("2024-01-01"), by = "week", length.out = 20),
  Tæller = c(15, 18, 20, 17, 19, 22, 21, 18, 20, 23,
             25, 24, 22, 26, 28, 27, 25, 29, 30, 28)
)

cat("=== BFHcharts Service Layer Test ===\n\n")
cat("✓ Test data created: 20 rows with Danish column names (Dato, Tæller)\n\n")

# Test compute_spc_results_bfh directly
cat("--- Testing compute_spc_results_bfh() ---\n")
result <- tryCatch(
  {
    compute_spc_results_bfh(
      data = test_data,
      x_var = "Dato",
      y_var = "Tæller",
      chart_type = "run",
      n_var = NULL,
      cl_var = NULL,
      freeze_var = NULL,
      part_var = NULL,
      notes_column = NULL,
      multiply = 1,
      chart_title_reactive = "Test Run Chart",
      y_axis_unit = "count",
      target_value = NULL,
      target_text = NULL,
      centerline_value = NULL
    )
  },
  error = function(e) {
    cat(sprintf("✗ ERROR: %s\n", e$message))
    cat(sprintf("   Traceback: %s\n", paste(deparse(sys.calls()), collapse = "\n")))
    return(NULL)
  }
)

if (!is.null(result)) {
  cat("\n✓ compute_spc_results_bfh() succeeded\n")
  cat(sprintf("✓ Result is ggplot: %s\n", inherits(result, "ggplot")))
  cat(sprintf("✓ Result class: %s\n", paste(class(result), collapse = ", ")))

  cat("\n=== TEST PASSED ===\n")
  cat("BFHcharts service layer working correctly\n")
} else {
  cat("\n=== TEST FAILED ===\n")
  cat("Review error messages above\n")
}
