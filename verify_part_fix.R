# verify_part_fix.R
# Test that part parameter works correctly after NSE fix

source('global.R')

# Test data with part boundary at row 6
test_data <- data.frame(
  Måned = 1:10,
  Værdi = c(8, 9, 10, 11, 12, 13, 14, 15, 16, 17),
  Skift = c(rep(0, 5), rep(1, 1), rep(0, 4))  # Part boundary at row 6
)

cat("Testing part parameter with NSE fix...\n\n")

# Test: Part boundary should be at position 6
cat("Test 1: Part parameter med boundary ved observation 6\n")
result <- tryCatch({
  SPCify:::call_qicharts2_for_anhoej_metadata(
    data = test_data,
    x_var = "Måned",
    y_var = "Værdi",
    chart_type = "run",
    part_var = "Skift"
  )
}, error = function(e) {
  cat("FEJL:", e$message, "\n")
  NULL
})

if (!is.null(result) && is.data.frame(result)) {
  cat("✓ Test 1 PASSED:", nrow(result), "rows returned\n")
  cat("  Has part column:", "part" %in% names(result), "\n")
  if ("part" %in% names(result)) {
    part_changes <- which(diff(as.numeric(result$part)) != 0)
    cat("  Part changes at positions:", paste(part_changes, collapse = ", "), "\n")
    cat("  Expected part change at position 5 (before row 6)\n")
    if (length(part_changes) > 0 && part_changes[1] == 5) {
      cat("  ✓ Part boundary is CORRECT at position 5-6\n")
    } else {
      cat("  ✗ Part boundary is WRONG - should be at position 5-6\n")
    }
  }
} else {
  cat("✗ Test 1 FAILED\n")
}

cat("\n=== Test færdig ===\n")
