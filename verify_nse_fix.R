# verify_nse_fix.R
# Test that call_qicharts2_for_anhoej_metadata() doesn't hit NSE bug

library(SPCify)

# Test data
test_data <- data.frame(
  Måned = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
  Værdi = c(12, 15, 18),
  Nævner = rep(100, 3)
)

cat("Testing call_qicharts2_for_anhoej_metadata() with NSE fix...\n\n")

# Test 1: P-chart with denominators
cat("Test 1: P-chart med denominators\n")
result1 <- tryCatch({
  SPCify:::call_qicharts2_for_anhoej_metadata(
    data = test_data,
    x_var = "Måned",
    y_var = "Værdi",
    chart_type = "p",
    n_var = "Nævner",
    target_value = 0.1,
    centerline_value = 0.08
  )
}, error = function(e) {
  cat("FEJL:", e$message, "\n")
  NULL
})

if (!is.null(result1) && is.data.frame(result1)) {
  cat("✓ Test 1 PASSED:", nrow(result1), "rows returned\n")
  cat("  Kolonner:", paste(names(result1), collapse = ", "), "\n")
} else {
  cat("✗ Test 1 FAILED\n")
}

cat("\n")

# Test 2: Run chart with part (multi-phase)
test_data2 <- data.frame(
  Måned = 1:10,
  Værdi = c(8, 9, 10, 11, 12, 13, 14, 15, 16, 17),
  Skift = c(rep(0, 5), rep(1, 5))  # Phase break at row 6
)

cat("Test 2: Run chart med part (multi-phase)\n")
result2 <- tryCatch({
  SPCify:::call_qicharts2_for_anhoej_metadata(
    data = test_data2,
    x_var = "Måned",
    y_var = "Værdi",
    chart_type = "run",
    part_var = "Skift"
  )
}, error = function(e) {
  cat("FEJL:", e$message, "\n")
  NULL
})

if (!is.null(result2) && is.data.frame(result2)) {
  cat("✓ Test 2 PASSED:", nrow(result2), "rows returned\n")
  cat("  Has part column:", "part" %in% names(result2), "\n")
} else {
  cat("✗ Test 2 FAILED\n")
}

cat("\n")

# Test 3: I-chart without denominators
test_data3 <- data.frame(
  x = 1:5,
  y = c(10, 12, 15, 13, 11)
)

cat("Test 3: I-chart uden denominators\n")
result3 <- tryCatch({
  SPCify:::call_qicharts2_for_anhoej_metadata(
    data = test_data3,
    x_var = "x",
    y_var = "y",
    chart_type = "i"
  )
}, error = function(e) {
  cat("FEJL:", e$message, "\n")
  NULL
})

if (!is.null(result3) && is.data.frame(result3)) {
  cat("✓ Test 3 PASSED:", nrow(result3), "rows returned\n")
} else {
  cat("✗ Test 3 FAILED\n")
}

cat("\n=== Alle tests færdige ===\n")
