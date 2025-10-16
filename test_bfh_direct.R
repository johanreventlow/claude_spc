# Direct BFHcharts Test - Minimal Example
# Tests BFHcharts outside Shiny context to isolate issues

# Set environment
Sys.setenv(GOLEM_CONFIG_ACTIVE = "development")

# Load required packages
library(BFHcharts)

# Create minimal test data (same structure as user's data)
test_data <- data.frame(
  Dato = seq.Date(from = as.Date("2024-01-01"), by = "week", length.out = 20),
  Tæller = c(15, 18, 20, 17, 19, 22, 21, 18, 20, 23,
             25, 24, 22, 26, 28, 27, 25, 29, 30, 28),
  stringsAsFactors = FALSE
)

cat("=== Direct BFHcharts Test ===\n\n")
cat("Test data: 20 rows with Danish column names (Dato, Tæller)\n\n")

# Test 1: Direct BFHcharts::create_spc_chart() call
cat("--- Test 1: BFHcharts::create_spc_chart() ---\n")
result1 <- tryCatch(
  {
    BFHcharts::create_spc_chart(
      data = test_data,
      x = Dato,
      y = Tæller,
      chart_type = "run"
    )
  },
  error = function(e) {
    cat(sprintf("✗ ERROR: %s\n", e$message))
    return(NULL)
  }
)

if (!is.null(result1)) {
  cat("✓ BFHcharts::create_spc_chart() succeeded\n")
  cat(sprintf("✓ Result is ggplot: %s\n", inherits(result1, "ggplot")))
  cat(sprintf("✓ Result class: %s\n", paste(class(result1), collapse = ", ")))

  # Try to extract data
  if (!is.null(result1$data)) {
    cat(sprintf("✓ Plot data available: %d rows\n", nrow(result1$data)))
  } else {
    cat("✗ No plot data attached\n")
  }

  cat("\n=== Test 1 PASSED ===\n\n")
} else {
  cat("\n=== Test 1 FAILED ===\n\n")
}

# Test 2: With NSE using rlang::sym()
cat("--- Test 2: BFHcharts with rlang::sym() ---\n")
result2 <- tryCatch(
  {
    BFHcharts::create_spc_chart(
      data = test_data,
      x = !!rlang::sym("Dato"),
      y = !!rlang::sym("Tæller"),
      chart_type = "run"
    )
  },
  error = function(e) {
    cat(sprintf("✗ ERROR: %s\n", e$message))
    return(NULL)
  }
)

if (!is.null(result2)) {
  cat("✓ BFHcharts with rlang::sym() succeeded\n")
  cat(sprintf("✓ Result is ggplot: %s\n", inherits(result2, "ggplot")))
  cat("\n=== Test 2 PASSED ===\n\n")
} else {
  cat("\n=== Test 2 FAILED ===\n\n")
}

# Test 3: Using do.call() like SPCify does
cat("--- Test 3: BFHcharts with do.call() ---\n")
result3 <- tryCatch(
  {
    params <- list(
      data = test_data,
      x = rlang::sym("Dato"),
      y = rlang::sym("Tæller"),
      chart_type = "run"
    )
    do.call(BFHcharts::create_spc_chart, params)
  },
  error = function(e) {
    cat(sprintf("✗ ERROR: %s\n", e$message))
    return(NULL)
  }
)

if (!is.null(result3)) {
  cat("✓ BFHcharts with do.call() succeeded\n")
  cat(sprintf("✓ Result is ggplot: %s\n", inherits(result3, "ggplot")))
  cat("\n=== Test 3 PASSED ===\n\n")
} else {
  cat("\n=== Test 3 FAILED ===\n\n")
}

cat("\n=== SUMMARY ===\n")
cat(sprintf("Test 1 (direct call): %s\n", if(!is.null(result1)) "PASS" else "FAIL"))
cat(sprintf("Test 2 (rlang::sym): %s\n", if(!is.null(result2)) "PASS" else "FAIL"))
cat(sprintf("Test 3 (do.call): %s\n", if(!is.null(result3)) "PASS" else "FAIL"))

if (!is.null(result1)) {
  cat("\n✓ BFHcharts works correctly outside SPCify\n")
  cat("  → Problem is likely in SPCify integration layer\n")
} else {
  cat("\n✗ BFHcharts has fundamental issues\n")
  cat("  → Check BFHcharts installation and dependencies\n")
}
