# Test basic service layer functionality
library(testthat)
library(tibble)
library(BFHcharts)
library(ggplot2)
library(ggrepel)
library(dplyr)

# Source the service layer
source("R/utils_error_handling.R")
source("R/utils_logging.R")
source("R/utils_danish_locale.R")
source("R/utils_spc_data_processing.R")
source("R/fct_spc_bfh_service.R")

# Set log level for testing
Sys.setenv(SPC_LOG_LEVEL = "ERROR")

# Run a simple test
set.seed(20251015)
test_data <- tibble(
  month = seq.Date(from = as.Date("2024-01-01"), by = "month", length.out = 20),
  value = round(rnorm(20, mean = 50, sd = 10), 1)
)

cat("Testing basic run chart...\n")
result <- compute_spc_results_bfh(
  data = test_data,
  x_var = "month",
  y_var = "value",
  chart_type = "run"
)

if (!is.null(result)) {
  cat("SUCCESS: Result returned\n")
  cat("- Result structure:", paste(names(result), collapse = ", "), "\n")
  cat("- Plot class:", class(result$plot)[1], "\n")
  cat("- Data rows:", nrow(result$qic_data), "\n")
  cat("- Data columns:", paste(names(result$qic_data)[1:min(6, ncol(result$qic_data))], collapse = ", "), "\n")
  cat("- Metadata chart_type:", result$metadata$chart_type, "\n")
  cat("\nAll required columns present:\n")
  cat("- x:", "x" %in% names(result$qic_data), "\n")
  cat("- y:", "y" %in% names(result$qic_data), "\n")
  cat("- cl:", "cl" %in% names(result$qic_data), "\n")
  cat("- ucl:", "ucl" %in% names(result$qic_data), "\n")
  cat("- lcl:", "lcl" %in% names(result$qic_data), "\n")
  cat("- signal:", "signal" %in% names(result$qic_data), "\n")
} else {
  cat("FAIL: Result is NULL\n")
  q(status = 1)
}

cat("\n\nTesting I chart...\n")
result_i <- compute_spc_results_bfh(
  data = test_data,
  x_var = "month",
  y_var = "value",
  chart_type = "i"
)

if (!is.null(result_i)) {
  cat("SUCCESS: I chart result returned\n")
  cat("- Chart type:", result_i$metadata$chart_type, "\n")
} else {
  cat("FAIL: I chart result is NULL\n")
  q(status = 1)
}

cat("\n\nAll basic tests passed!\n")
