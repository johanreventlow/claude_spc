#!/usr/bin/env Rscript
# capture_baselines.R
# Captures qicharts2 regression baselines for BFHchart migration (Issue #29)
#
# Generates 21 baseline files: 7 chart types × 3 scenarios
# All baselines are deterministic (fixed seed, fixed dates)

library(qicharts2)
library(dplyr)
library(tibble)

# Set seed for reproducibility
set.seed(20251015)

# Output directory
output_dir <- here::here("tests/testthat/fixtures/qic-baseline")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Create deterministic test data
#'
#' Generates fixed test data for baseline capture with controlled patterns
#' for Anhøj violations and freeze periods.
#'
#' @param n_points Number of data points
#' @param scenario One of: "basic", "anhoej", "freeze"
#' @param chart_type Chart type for appropriate data generation
#' @return tibble with test data
create_test_data <- function(n_points = 36, scenario = "basic", chart_type = "run") {
  # Fixed dates (monthly from Jan 2022)
  dates <- seq.Date(as.Date("2022-01-01"), by = "month", length.out = n_points)

  # Base data structure
  data <- tibble(
    Dato = dates,
    Skift = rep(FALSE, n_points),
    Frys = rep(FALSE, n_points)
  )

  # Generate appropriate data based on chart type and scenario
  if (chart_type %in% c("run", "p", "u")) {
    # Charts requiring numerator and denominator
    if (scenario == "basic") {
      # Basic stable process
      data$Taeller <- rpois(n_points, lambda = 90) + 80
      data$Naevner <- rpois(n_points, lambda = 100) + 95
    } else if (scenario == "anhoej") {
      # Create pattern for Anhøj violations (runs ≥8 on one side)
      # First 18 points above median, next 18 below
      data$Taeller <- c(
        rpois(18, lambda = 95) + 90,  # Higher values (above median)
        rpois(18, lambda = 85) + 75   # Lower values (below median)
      )
      data$Naevner <- rpois(n_points, lambda = 100) + 95
    } else if (scenario == "freeze") {
      # Stable baseline, then shift after freeze
      data$Taeller <- c(
        rpois(24, lambda = 90) + 80,  # Baseline (24 months)
        rpois(12, lambda = 95) + 90   # Improved (12 months)
      )
      data$Naevner <- rpois(n_points, lambda = 100) + 95
      data$Frys[24] <- TRUE  # Freeze at month 24
    }
  } else {
    # Charts with single value (i, c, g, mr, s, xbar)
    if (scenario == "basic") {
      # Basic stable process
      data$Vaerdi <- rnorm(n_points, mean = 50, sd = 5)
    } else if (scenario == "anhoej") {
      # Create pattern for Anhøj violations
      data$Vaerdi <- c(
        rnorm(18, mean = 55, sd = 3),  # Above median
        rnorm(18, mean = 45, sd = 3)   # Below median
      )
    } else if (scenario == "freeze") {
      # Stable baseline, then shift
      data$Vaerdi <- c(
        rnorm(24, mean = 50, sd = 5),  # Baseline
        rnorm(12, mean = 45, sd = 4)   # Improved
      )
      data$Frys[24] <- TRUE
    }
  }

  return(data)
}

#' Capture qicharts2 baseline
#'
#' Runs qicharts2::qic() with specified parameters and captures all outputs
#'
#' @param data Input data
#' @param chart_type Chart type code (e.g., "run", "i", "p")
#' @param scenario Scenario name
#' @return list with baseline data
capture_qic_baseline <- function(data, chart_type, scenario) {
  message(sprintf("Capturing baseline: %s - %s", chart_type, scenario))

  # Build qic call based on chart type
  if (chart_type %in% c("run", "p", "u")) {
    # Charts with numerator and denominator
    qic_result <- qicharts2::qic(
      x = Dato,
      y = Taeller,
      n = Naevner,
      data = data,
      chart = chart_type,
      freeze = if (any(data$Frys)) which(data$Frys)[1] else NULL,
      return.data = TRUE
    )

    plot <- qicharts2::qic(
      x = Dato,
      y = Taeller,
      n = Naevner,
      data = data,
      chart = chart_type,
      freeze = if (any(data$Frys)) which(data$Frys)[1] else NULL,
      title = paste(chart_type, scenario, sep = " - ")
    )
  } else {
    # Charts with single value
    qic_result <- qicharts2::qic(
      x = Dato,
      y = Vaerdi,
      data = data,
      chart = chart_type,
      freeze = if (any(data$Frys)) which(data$Frys)[1] else NULL,
      return.data = TRUE
    )

    plot <- qicharts2::qic(
      x = Dato,
      y = Vaerdi,
      data = data,
      chart = chart_type,
      freeze = if (any(data$Frys)) which(data$Frys)[1] else NULL,
      title = paste(chart_type, scenario, sep = " - ")
    )
  }

  # Extract key components from qic result
  baseline <- list(
    chart_type = chart_type,
    scenario = scenario,
    input_data = data,
    qic_output = list(
      plot_data = qic_result,
      control_limits = if (!is.null(qic_result$ucl) && !is.null(qic_result$lcl)) {
        list(
          ucl = unique(qic_result$ucl[!is.na(qic_result$ucl)]),
          lcl = unique(qic_result$lcl[!is.na(qic_result$lcl)])
        )
      } else NULL,
      center_line = if (!is.null(qic_result$cl)) {
        unique(qic_result$cl[!is.na(qic_result$cl)])
      } else NULL,
      anhoej_rules = list(
        runs_signal = if ("runs.signal" %in% names(qic_result)) {
          any(qic_result$runs.signal, na.rm = TRUE)
        } else FALSE,
        n_crossings = if ("n.crossings" %in% names(qic_result)) {
          unique(qic_result$n.crossings[!is.na(qic_result$n.crossings)])[1]
        } else NA,
        n_crossings_min = if ("n.crossings.min" %in% names(qic_result)) {
          unique(qic_result$n.crossings.min[!is.na(qic_result$n.crossings.min)])[1]
        } else NA
      ),
      ggplot_object = plot
    ),
    metadata = list(
      captured_date = Sys.Date(),
      qicharts2_version = as.character(packageVersion("qicharts2")),
      r_version = R.version.string,
      seed = 20251015
    )
  )

  return(baseline)
}

# ============================================================================
# MAIN CAPTURE LOGIC
# ============================================================================

# Define chart types and scenarios
chart_types <- c("run", "i", "p", "c", "u", "xbar", "s")
scenarios <- c("basic", "anhoej", "freeze")

# Track captured baselines
captured <- list()
failed <- list()

# Capture all combinations
for (chart_type in chart_types) {
  for (scenario in scenarios) {
    baseline_name <- paste(chart_type, scenario, sep = "-")

    tryCatch({
      # Generate appropriate test data
      data <- create_test_data(
        n_points = 36,
        scenario = scenario,
        chart_type = chart_type
      )

      # Capture baseline
      baseline <- capture_qic_baseline(data, chart_type, scenario)

      # Save to RDS
      output_file <- file.path(output_dir, paste0(baseline_name, ".rds"))
      saveRDS(baseline, output_file)

      # Track success
      captured[[baseline_name]] <- output_file
      message(sprintf("✓ Saved: %s", basename(output_file)))

    }, error = function(e) {
      failed[[baseline_name]] <- e$message
      message(sprintf("✗ Failed: %s - %s", baseline_name, e$message))
    })
  }
}

# ============================================================================
# SUMMARY REPORT
# ============================================================================

cat("\n")
cat("========================================\n")
cat("BASELINE CAPTURE SUMMARY\n")
cat("========================================\n")
cat(sprintf("Total expected: %d\n", length(chart_types) * length(scenarios)))
cat(sprintf("Successfully captured: %d\n", length(captured)))
cat(sprintf("Failed: %d\n", length(failed)))
cat("\n")

if (length(failed) > 0) {
  cat("Failed baselines:\n")
  for (name in names(failed)) {
    cat(sprintf("  - %s: %s\n", name, failed[[name]]))
  }
  cat("\n")
}

cat("Captured baselines:\n")
for (name in names(captured)) {
  cat(sprintf("  ✓ %s\n", name))
}
cat("\n")

# Verify all files are loadable
cat("Verifying baseline files are loadable...\n")
verification_failed <- FALSE
for (name in names(captured)) {
  file_path <- captured[[name]]
  tryCatch({
    baseline <- readRDS(file_path)
    # Basic structure checks
    if (!is.list(baseline)) stop("Not a list")
    if (!"qic_output" %in% names(baseline)) stop("Missing qic_output")
    if (!"input_data" %in% names(baseline)) stop("Missing input_data")
    cat(sprintf("  ✓ %s verified\n", name))
  }, error = function(e) {
    cat(sprintf("  ✗ %s verification failed: %s\n", name, e$message))
    verification_failed <- TRUE
  })
}

if (!verification_failed && length(failed) == 0) {
  cat("\n✓ All baselines captured and verified successfully!\n")
} else {
  cat("\n✗ Some baselines failed. See details above.\n")
  quit(status = 1)
}
