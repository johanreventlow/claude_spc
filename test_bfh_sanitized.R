# Test BFHcharts with Sanitized Column Names
# Verifies that ASCII-safe column names work

Sys.setenv(GOLEM_CONFIG_ACTIVE = "development")
library(BFHcharts)

# Create test data with DANISH column names (original)
test_data_danish <- data.frame(
  Dato = seq.Date(from = as.Date("2024-01-01"), by = "week", length.out = 20),
  Tæller = c(15, 18, 20, 17, 19, 22, 21, 18, 20, 23,
             25, 24, 22, 26, 28, 27, 25, 29, 30, 28),
  stringsAsFactors = FALSE
)

# Create test data with SANITIZED column names
test_data_sanitized <- test_data_danish
names(test_data_sanitized) <- c("Dato", "Taeller")  # æ → ae

cat("=== BFHcharts Column Name Test ===\n\n")

# Test with Danish characters
cat("--- Test 1: Danish characters (Tæller) ---\n")
result_danish <- tryCatch(
  {
    BFHcharts::create_spc_chart(
      data = test_data_danish,
      x = Dato,
      y = Tæller,
      chart_type = "run"
    )
  },
  error = function(e) {
    cat(sprintf("✗ FAILED: %s\n", e$message))
    return(NULL)
  }
)

cat(sprintf("Result: %s\n\n", if (!is.null(result_danish)) "✓ PASS" else "✗ FAIL"))

# Test with sanitized names
cat("--- Test 2: Sanitized names (Taeller) ---\n")
result_sanitized <- tryCatch(
  {
    BFHcharts::create_spc_chart(
      data = test_data_sanitized,
      x = Dato,
      y = Taeller,
      chart_type = "run"
    )
  },
  error = function(e) {
    cat(sprintf("✗ FAILED: %s\n", e$message))
    return(NULL)
  }
)

cat(sprintf("Result: %s\n\n", if (!is.null(result_sanitized)) "✓ PASS" else "✗ FAIL"))

# Summary
cat("=== SUMMARY ===\n")
if (is.null(result_danish) && !is.null(result_sanitized)) {
  cat("✓ CONFIRMED: BFHcharts requires ASCII-safe column names\n")
  cat("✓ Solution: Temporarily rename columns before BFHcharts call\n")
  cat("  → Keep original names for UI display\n")
  cat("  → Map to sanitized names internally\n")
} else if (!is.null(result_danish)) {
  cat("? Unexpected: Danish characters work?\n")
} else {
  cat("✗ Both failed - different issue\n")
}
