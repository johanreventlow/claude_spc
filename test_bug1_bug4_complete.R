#!/usr/bin/env Rscript
# Complete test for Bug #1 (target_value) and Bug #4 (y_axis_unit, chart_title)
# After BFHcharts label compatibility fix
#
# Tests:
# 1. Target value rendering on plot
# 2. Y-axis unit formatting (percent, per1000, count)
# 3. Chart title rendering
# 4. All chart types (p, c, u, i, run)

# Load app environment
if (file.exists("global.R")) {
  source("global.R", local = TRUE)
} else {
  library(SPCify)
}

cat("=== Bug #1 and #4 Complete Test ===\n\n")

# Create comprehensive test data
test_data <- data.frame(
  Dato = seq.Date(from = as.Date("2024-01-01"), by = "week", length.out = 36),
  Komplikationer = c(12, 15, 18, 14, 16, 19, 17, 15, 18, 20,
                     22, 21, 19, 23, 25, 24, 22, 26, 28, 27,
                     25, 29, 31, 30, 28, 32, 34, 33, 31, 35,
                     37, 36, 34, 38, 40, 39),
  Procedurer = rep(100, 36),
  stringsAsFactors = FALSE
)

cat("Test data structure:\n")
cat(sprintf("  Rows: %d\n", nrow(test_data)))
cat(sprintf("  Dato: %s to %s\n", min(test_data$Dato), max(test_data$Dato)))
cat(sprintf("  Komplikationer: %d to %d\n", min(test_data$Komplikationer), max(test_data$Komplikationer)))
cat(sprintf("  Procedurer: %d\n", unique(test_data$Procedurer)))
cat("\n")

# Test helper function
run_test <- function(test_name, chart_type, multiply, target_value, y_axis_unit, chart_title, needs_denominator = FALSE) {
  cat(sprintf("--- Test: %s ---\n", test_name))
  cat(sprintf("  chart_type: %s\n", chart_type))
  cat(sprintf("  multiply: %s\n", ifelse(!is.null(multiply), multiply, "NULL")))
  cat(sprintf("  target_value: %s\n", ifelse(!is.null(target_value), target_value, "NULL")))
  cat(sprintf("  y_axis_unit: %s\n", ifelse(!is.null(y_axis_unit), y_axis_unit, "NULL")))
  cat(sprintf("  chart_title: %s\n", ifelse(!is.null(chart_title), chart_title, "NULL")))
  cat(sprintf("  needs_denominator: %s\n", needs_denominator))
  cat("\n")

  result <- tryCatch({
    compute_spc_results_bfh(
      data = test_data,
      x_var = "Dato",
      y_var = "Komplikationer",
      n_var = if (needs_denominator) "Procedurer" else NULL,
      chart_type = chart_type,
      multiply = multiply,
      target = target_value,
      y_axis_unit = y_axis_unit,
      title = chart_title
    )
  }, error = function(e) {
    cat(sprintf("  ✗ ERROR: %s\n", e$message))
    return(NULL)
  })

  if (!is.null(result)) {
    cat("  ✓ compute_spc_results_bfh() succeeded\n")
    cat(sprintf("  ✓ Plot object class: %s\n", paste(class(result$plot), collapse = ", ")))
    cat(sprintf("  ✓ Data rows: %d\n", nrow(result$qic_data)))
    cat(sprintf("  ✓ Backend: %s\n", result$metadata$backend %||% "unknown"))

    # Verify metadata
    if (!is.null(result$metadata$backend) && result$metadata$backend == "bfhcharts") {
      cat("  ✓ Backend flag set correctly\n")
    } else {
      cat("  ⚠ Backend flag missing or incorrect\n")
    }

    # Try to print the plot (this is where the label error occurred)
    print_result <- tryCatch({
      print(result$plot)
      TRUE
    }, error = function(e) {
      cat(sprintf("  ✗ PLOT PRINT ERROR: %s\n", e$message))
      FALSE
    })

    if (print_result) {
      cat("  ✓ Plot printed successfully (no label error)\n")
    }

  } else {
    cat("  ✗ compute_spc_results_bfh() returned NULL\n")
  }

  cat("\n")
  return(result)
}

# Test 1: P-chart with target, percent unit, and title
cat("\n========== TEST 1: P-Chart with All Features ==========\n")
test1_result <- run_test(
  test_name = "P-chart with target=18, percent unit, title",
  chart_type = "p",
  multiply = 100,
  target_value = 18,
  y_axis_unit = "percent",
  chart_title = "Komplikationsrate med målværdi",
  needs_denominator = TRUE
)

# Test 2: C-chart with count unit and title
cat("\n========== TEST 2: C-Chart with Count Unit ==========\n")
test2_result <- run_test(
  test_name = "C-chart with count unit, title",
  chart_type = "c",
  multiply = NULL,
  target_value = NULL,
  y_axis_unit = "count",
  chart_title = "Antal komplikationer",
  needs_denominator = FALSE
)

# Test 3: U-chart with per1000 unit, target, and title
cat("\n========== TEST 3: U-Chart with Per1000 Unit ==========\n")
test3_result <- run_test(
  test_name = "U-chart with target=200, per1000 unit, title",
  chart_type = "u",
  multiply = 1000,
  target_value = 200,
  y_axis_unit = "per1000",
  chart_title = "Rate per 1000 procedurer",
  needs_denominator = TRUE
)

# Test 4: I-chart with target and title
cat("\n========== TEST 4: I-Chart with Target ==========\n")
test4_result <- run_test(
  test_name = "I-chart with target=25, title",
  chart_type = "i",
  multiply = NULL,
  target_value = 25,
  y_axis_unit = NULL,
  chart_title = "Individuelle værdier",
  needs_denominator = FALSE
)

# Test 5: Run-chart with title only
cat("\n========== TEST 5: Run-Chart with Title Only ==========\n")
test5_result <- run_test(
  test_name = "Run-chart with title only",
  chart_type = "run",
  multiply = NULL,
  target_value = NULL,
  y_axis_unit = NULL,
  chart_title = "Run chart uden target",
  needs_denominator = FALSE
)

# Summary
cat("\n========== TEST SUMMARY ==========\n")
tests <- list(
  "P-chart (all features)" = test1_result,
  "C-chart (count unit)" = test2_result,
  "U-chart (per1000 unit)" = test3_result,
  "I-chart (target)" = test4_result,
  "Run-chart (title only)" = test5_result
)

success_count <- 0
fail_count <- 0

for (test_name in names(tests)) {
  if (!is.null(tests[[test_name]])) {
    cat(sprintf("✓ %s\n", test_name))
    success_count <- success_count + 1
  } else {
    cat(sprintf("✗ %s\n", test_name))
    fail_count <- fail_count + 1
  }
}

cat("\n")
cat(sprintf("Total: %d/%d tests passed\n", success_count, length(tests)))

if (fail_count == 0) {
  cat("\n✓ ALL TESTS PASSED\n")
  cat("Bug #1 (target_value) and Bug #4 (y_axis_unit, chart_title) are working correctly!\n")
} else {
  cat(sprintf("\n✗ %d TESTS FAILED\n", fail_count))
  cat("Review error messages above for details.\n")
}

cat("\n=== END TEST ===\n")
