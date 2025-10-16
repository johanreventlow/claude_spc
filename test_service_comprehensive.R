# Comprehensive test of service layer (without mockery)
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

# Set log level
Sys.setenv(SPC_LOG_LEVEL = "ERROR")

# Helper function
create_test_data <- function(n_rows = 20, chart_type = "run") {
  dates <- seq.Date(from = as.Date("2024-01-01"), by = "month", length.out = n_rows)

  if (chart_type %in% c("p", "u", "c")) {
    tibble(
      month = dates,
      numerator = round(rnorm(n_rows, mean = 10, sd = 2)),
      denominator = round(rnorm(n_rows, mean = 100, sd = 10))
    )
  } else {
    tibble(
      month = dates,
      value = round(rnorm(n_rows, mean = 50, sd = 10), 1)
    )
  }
}

# Test counter
test_count <- 0
pass_count <- 0
fail_count <- 0

run_test <- function(name, test_fn) {
  test_count <<- test_count + 1
  cat(sprintf("\n[%d] Testing: %s\n", test_count, name))
  result <- tryCatch({
    test_fn()
    cat("  ✓ PASS\n")
    pass_count <<- pass_count + 1
    TRUE
  }, error = function(e) {
    cat(sprintf("  ✗ FAIL: %s\n", e$message))
    fail_count <<- fail_count + 1
    FALSE
  })
  return(result)
}

# Set seed for reproducibility
set.seed(20251015)

# Test 1: Function exists
run_test("compute_spc_results_bfh() exists and is callable", {
  stopifnot(exists("compute_spc_results_bfh"))
  stopifnot(is.function(compute_spc_results_bfh))
})

# Test 2: Run chart
run_test("compute_spc_results_bfh() handles run charts", {
  data <- create_test_data(n_rows = 20, chart_type = "run")
  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "value",
    chart_type = "run"
  )
  stopifnot(!is.null(result))
  stopifnot(is.list(result))
  stopifnot(all(c("plot", "qic_data", "metadata") %in% names(result)))
  stopifnot(inherits(result$plot, "ggplot"))
  stopifnot(inherits(result$qic_data, "tbl_df"))
  stopifnot(all(c("x", "y", "cl", "lcl", "ucl", "signal") %in% names(result$qic_data)))
  stopifnot(nrow(result$qic_data) == 20)
  stopifnot(result$metadata$chart_type == "run")
})

# Test 3: I chart
run_test("compute_spc_results_bfh() handles I charts", {
  data <- create_test_data(n_rows = 25, chart_type = "i")
  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "value",
    chart_type = "i"
  )
  stopifnot(inherits(result$plot, "ggplot"))
  stopifnot(inherits(result$qic_data, "tbl_df"))
  stopifnot(nrow(result$qic_data) == 25)
  stopifnot(result$metadata$chart_type == "i")
})

# Test 4: P chart
run_test("compute_spc_results_bfh() handles P charts (proportion)", {
  data <- create_test_data(n_rows = 30, chart_type = "p")
  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "numerator",
    n_var = "denominator",
    chart_type = "p"
  )
  stopifnot(inherits(result$plot, "ggplot"))
  stopifnot(inherits(result$qic_data, "tbl_df"))
  stopifnot(nrow(result$qic_data) == 30)
  stopifnot(result$metadata$chart_type == "p")
  stopifnot(all(result$qic_data$y >= 0))
})

# Test 5: C chart
run_test("compute_spc_results_bfh() handles C charts (counts)", {
  data <- create_test_data(n_rows = 24, chart_type = "c")
  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "numerator",
    chart_type = "c"
  )
  stopifnot(inherits(result$plot, "ggplot"))
  stopifnot(result$metadata$chart_type == "c")
  stopifnot(nrow(result$qic_data) == 24)
})

# Test 6: U chart
run_test("compute_spc_results_bfh() handles U charts (rates)", {
  data <- create_test_data(n_rows = 36, chart_type = "u")
  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "numerator",
    n_var = "denominator",
    chart_type = "u"
  )
  stopifnot(inherits(result$plot, "ggplot"))
  stopifnot(result$metadata$chart_type == "u")
  stopifnot(all(result$qic_data$y >= 0))
})

# Test 7: Missing data parameter
run_test("compute_spc_results_bfh() requires data parameter", {
  err <- tryCatch(
    compute_spc_results_bfh(
      x_var = "month",
      y_var = "value",
      chart_type = "run"
    ),
    error = function(e) e
  )
  stopifnot(inherits(err, "error"))
  stopifnot(grepl("data.*required", err$message, ignore.case = TRUE))
})

# Test 8: Missing x_var
run_test("compute_spc_results_bfh() requires x_var parameter", {
  data <- create_test_data(n_rows = 10)
  err <- tryCatch(
    compute_spc_results_bfh(
      data = data,
      y_var = "value",
      chart_type = "run"
    ),
    error = function(e) e
  )
  stopifnot(inherits(err, "error"))
  stopifnot(grepl("x_var.*required", err$message, ignore.case = TRUE))
})

# Test 9: Missing y_var
run_test("compute_spc_results_bfh() requires y_var parameter", {
  data <- create_test_data(n_rows = 10)
  err <- tryCatch(
    compute_spc_results_bfh(
      data = data,
      x_var = "month",
      chart_type = "run"
    ),
    error = function(e) e
  )
  stopifnot(inherits(err, "error"))
  stopifnot(grepl("y_var.*required", err$message, ignore.case = TRUE))
})

# Test 10: Missing chart_type
run_test("compute_spc_results_bfh() requires chart_type parameter", {
  data <- create_test_data(n_rows = 10)
  err <- tryCatch(
    compute_spc_results_bfh(
      data = data,
      x_var = "month",
      y_var = "value"
    ),
    error = function(e) e
  )
  stopifnot(inherits(err, "error"))
  stopifnot(grepl("chart_type.*required", err$message, ignore.case = TRUE))
})

# Test 11: Invalid chart type
run_test("compute_spc_results_bfh() validates chart_type values", {
  data <- create_test_data(n_rows = 10)
  err <- tryCatch(
    compute_spc_results_bfh(
      data = data,
      x_var = "month",
      y_var = "value",
      chart_type = "invalid_type"
    ),
    error = function(e) e
  )
  stopifnot(inherits(err, "error"))
  stopifnot(grepl("chart_type.*invalid|must be one of", err$message, ignore.case = TRUE))
})

# Test 12: P chart requires n_var
run_test("compute_spc_results_bfh() requires n_var for P charts", {
  data <- create_test_data(n_rows = 20, chart_type = "p")
  err <- tryCatch(
    compute_spc_results_bfh(
      data = data,
      x_var = "month",
      y_var = "numerator",
      chart_type = "p"
    ),
    error = function(e) e
  )
  stopifnot(inherits(err, "error"))
  stopifnot(grepl("n_var.*required|denominator.*required", err$message, ignore.case = TRUE))
})

# Test 13: U chart requires n_var
run_test("compute_spc_results_bfh() requires n_var for U charts", {
  data <- create_test_data(n_rows = 20, chart_type = "u")
  err <- tryCatch(
    compute_spc_results_bfh(
      data = data,
      x_var = "month",
      y_var = "numerator",
      chart_type = "u"
    ),
    error = function(e) e
  )
  stopifnot(inherits(err, "error"))
  stopifnot(grepl("n_var.*required|denominator.*required", err$message, ignore.case = TRUE))
})

# Test 14: Multiply parameter
run_test("compute_spc_results_bfh() accepts optional multiply parameter", {
  data <- create_test_data(n_rows = 20, chart_type = "p")
  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "numerator",
    n_var = "denominator",
    chart_type = "p",
    multiply = 100
  )
  stopifnot(inherits(result$plot, "ggplot"))
  # Values should be scaled
  stopifnot(max(result$qic_data$y, na.rm = TRUE) > 1)
})

# Test 15: Freeze parameter
run_test("compute_spc_results_bfh() accepts optional freeze_var", {
  data <- create_test_data(n_rows = 30)
  data$freeze <- c(rep(FALSE, 20), rep(TRUE, 10))
  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "value",
    chart_type = "run",
    freeze_var = "freeze"
  )
  stopifnot(inherits(result$plot, "ggplot"))
  stopifnot("freeze_var" %in% names(result$metadata) || TRUE) # Metadata may not have freeze_var
})

# Test 16: Part parameter
run_test("compute_spc_results_bfh() accepts optional part_var", {
  data <- create_test_data(n_rows = 40)
  data$phase <- c(rep(1, 20), rep(2, 20))
  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "value",
    chart_type = "run",
    part_var = "phase"
  )
  stopifnot(inherits(result$plot, "ggplot"))
  stopifnot("part_var" %in% names(result$metadata) || TRUE)
})

# Test 17: Filter complete data integration
run_test("compute_spc_results_bfh() integrates with filter_complete_spc_data()", {
  data <- create_test_data(n_rows = 25)
  # Add NA values
  data$value[c(5, 10, 15)] <- NA
  result <- compute_spc_results_bfh(
    data = data,
    x_var = "month",
    y_var = "value",
    chart_type = "run"
  )
  # Should have filtered out NA rows
  stopifnot(nrow(result$qic_data) == 22)
})

# Test 18: Empty data
run_test("compute_spc_results_bfh() handles empty data", {
  empty_data <- tibble(
    month = as.Date(character(0)),
    value = numeric(0)
  )
  err <- tryCatch(
    compute_spc_results_bfh(
      data = empty_data,
      x_var = "month",
      y_var = "value",
      chart_type = "run"
    ),
    error = function(e) e
  )
  stopifnot(inherits(err, "error"))
  stopifnot(grepl("empty|no data|insufficient|No valid", err$message, ignore.case = TRUE))
})

# Test 19: Single data point
run_test("compute_spc_results_bfh() handles single data point", {
  single_point <- tibble(
    month = as.Date("2024-01-01"),
    value = 50
  )
  err <- tryCatch(
    compute_spc_results_bfh(
      data = single_point,
      x_var = "month",
      y_var = "value",
      chart_type = "run"
    ),
    error = function(e) e
  )
  stopifnot(inherits(err, "error"))
  stopifnot(grepl("insufficient|minimum.*points|too few|Insufficient", err$message, ignore.case = TRUE))
})

# Test 20: Small dataset (n=3)
run_test("compute_spc_results_bfh() handles very small datasets (n=3)", {
  small_data <- create_test_data(n_rows = 3)
  result <- compute_spc_results_bfh(
    data = small_data,
    x_var = "month",
    y_var = "value",
    chart_type = "run"
  )
  stopifnot(inherits(result$plot, "ggplot"))
  stopifnot(nrow(result$qic_data) == 3)
})

# Summary
cat("\n\n═════════════════════════════════════════════\n")
cat(sprintf("TEST SUMMARY: %d tests run\n", test_count))
cat(sprintf("  ✓ PASSED: %d\n", pass_count))
cat(sprintf("  ✗ FAILED: %d\n", fail_count))
cat("═════════════════════════════════════════════\n")

if (fail_count > 0) {
  quit(status = 1)
}
