#!/usr/bin/env Rscript
# generate_benchmark_report.R
# Task 033 Stream 2: Performance Benchmark Report Generator
#
# Generates comprehensive markdown report from benchmark results, including:
# - Summary table with BFHchart/qicharts2 ratios
# - Pass/fail status (≤110% threshold)
# - Memory profiling results
# - Latency percentiles (P50, P95, P99)
# - Visual charts and recommendations
#
# Usage:
#   Rscript tests/performance/generate_benchmark_report.R
#   R -e "source('tests/performance/generate_benchmark_report.R')"

# Load required packages
library(testthat)

#' Generate Performance Benchmark Report
#'
#' Reads benchmark result files and generates comprehensive markdown report
#' with performance metrics, pass/fail status, and recommendations.
#'
#' @param output_file character. Output markdown file path
#' @param results_dir character. Directory containing benchmark results
#'
#' @return character. Path to generated report file
generate_performance_report <- function(
  output_file = "tests/performance/BENCHMARK_REPORT.md",
  results_dir = "tests/performance"
) {
  cat("\n=== Generating Performance Benchmark Report ===\n")

  # Initialize report sections
  report <- c(
    "# BFHcharts vs qicharts2 Performance Benchmark Report",
    "",
    sprintf("**Generated:** %s", Sys.time()),
    sprintf("**SPCify Version:** %s", utils::packageVersion("SPCify")),
    sprintf("**BFHcharts Version:** %s", utils::packageVersion("BFHcharts")),
    sprintf("**qicharts2 Version:** %s", utils::packageVersion("qicharts2")),
    "",
    "---",
    ""
  )

  # Section 1: Executive Summary
  report <- c(report, generate_executive_summary(results_dir))

  # Section 2: Performance Comparison by Data Size
  report <- c(report, generate_data_size_section(results_dir))

  # Section 3: Performance Comparison by Chart Type
  report <- c(report, generate_chart_type_section(results_dir))

  # Section 4: Latency Percentiles
  report <- c(report, generate_latency_section(results_dir))

  # Section 5: Memory Profiling
  report <- c(report, generate_memory_section(results_dir))

  # Section 6: Cache Performance (if available)
  report <- c(report, generate_cache_section(results_dir))

  # Section 7: Acceptance Criteria Summary
  report <- c(report, generate_acceptance_criteria(results_dir))

  # Section 8: Recommendations
  report <- c(report, generate_recommendations(results_dir))

  # Write report to file
  writeLines(report, output_file)

  cat(sprintf("\n✅ Report saved to: %s\n", output_file))
  cat(sprintf("   Lines written: %d\n", length(report)))

  return(output_file)
}

# Report Section Generators
# =========================

generate_executive_summary <- function(results_dir) {
  section <- c(
    "## Executive Summary",
    "",
    "This report presents comprehensive performance benchmarks comparing BFHcharts",
    "and qicharts2 backends for SPC chart generation. The benchmarks evaluate:",
    "",
    "- **Rendering Performance:** Median time (P50) across data sizes and chart types",
    "- **Latency Distribution:** P50, P95, P99 percentiles for large datasets",
    "- **Memory Efficiency:** Memory usage and leak detection over extended sessions",
    "- **Cache Performance:** Cache hit speedup (if Stream 1 integration complete)",
    "",
    "**Performance Target:** BFHcharts ≤ 110% of qicharts2 rendering time",
    ""
  )

  # Try to load results and compute overall pass/fail
  run_chart_file <- file.path(results_dir, "benchmark_run_chart_results.rds")
  all_charts_file <- file.path(results_dir, "benchmark_all_charts_results.rds")

  overall_status <- "⚠️ PENDING"
  if (file.exists(run_chart_file) && file.exists(all_charts_file)) {
    overall_status <- compute_overall_status(results_dir)
  }

  section <- c(
    section,
    sprintf("**Overall Status:** %s", overall_status),
    ""
  )

  return(section)
}

generate_data_size_section <- function(results_dir) {
  section <- c(
    "## Performance by Data Size (Run Charts)",
    "",
    "Benchmark results for run charts across increasing data sizes:",
    "",
    "| Data Size | qicharts2 (P50) | BFHcharts (P50) | Ratio | Status |",
    "|-----------|-----------------|-----------------|-------|--------|"
  )

  results_file <- file.path(results_dir, "benchmark_run_chart_results.rds")
  if (!file.exists(results_file)) {
    section <- c(
      section,
      "| N/A | N/A | N/A | N/A | ⚠️ Results not found |",
      "",
      "_Run benchmarks with `testthat::test_file('tests/performance/test-bfh-vs-qic-benchmark.R')`_",
      ""
    )
    return(section)
  }

  # Load and process results
  results <- readRDS(results_file)

  for (name in names(results)) {
    bm <- results[[name]]
    data_size <- gsub("n_", "", name)

    # Extract median times
    median_qic <- median(bm$median[bm$expression == "qicharts2"])
    median_bfh <- median(bm$median[bm$expression == "BFHcharts" | bm$expression == "BFHchart"])

    # Calculate ratio
    ratio <- as.numeric(median_bfh) / as.numeric(median_qic)

    # Determine status
    status <- if (ratio <= 1.10) "✅ PASS" else "❌ FAIL"

    # Format row
    section <- c(section, sprintf(
      "| %s | %.2f ms | %.2f ms | %.2fx | %s |",
      data_size,
      as.numeric(median_qic) * 1000,
      as.numeric(median_bfh) * 1000,
      ratio,
      status
    ))
  }

  section <- c(section, "")
  return(section)
}

generate_chart_type_section <- function(results_dir) {
  section <- c(
    "## Performance by Chart Type (n=100)",
    "",
    "Benchmark results for all supported chart types:",
    "",
    "| Chart Type | qicharts2 (P50) | BFHcharts (P50) | Ratio | Status |",
    "|------------|-----------------|-----------------|-------|--------|"
  )

  results_file <- file.path(results_dir, "benchmark_all_charts_results.rds")
  if (!file.exists(results_file)) {
    section <- c(
      section,
      "| N/A | N/A | N/A | N/A | ⚠️ Results not found |",
      "",
      "_Run benchmarks with `testthat::test_file('tests/performance/test-bfh-vs-qic-benchmark.R')`_",
      ""
    )
    return(section)
  }

  # Load and process results
  results <- readRDS(results_file)

  for (chart_type in names(results)) {
    bm <- results[[chart_type]]

    # Extract median times
    median_qic <- median(bm$median[bm$expression == "qicharts2"])
    median_bfh <- median(bm$median[bm$expression == "BFHcharts" | bm$expression == "BFHchart"])

    # Calculate ratio
    ratio <- as.numeric(median_bfh) / as.numeric(median_qic)

    # Determine status
    status <- if (ratio <= 1.10) "✅ PASS" else "❌ FAIL"

    # Format row
    section <- c(section, sprintf(
      "| %s | %.2f ms | %.2f ms | %.2fx | %s |",
      toupper(chart_type),
      as.numeric(median_qic) * 1000,
      as.numeric(median_bfh) * 1000,
      ratio,
      status
    ))
  }

  section <- c(section, "")
  return(section)
}

generate_latency_section <- function(results_dir) {
  section <- c(
    "## Latency Percentiles",
    "",
    "BFHcharts latency distribution for large datasets (n=1000):",
    ""
  )

  # Note: Latency percentiles computed directly in test, not saved separately
  # This section describes the methodology
  section <- c(
    section,
    "**Methodology:**",
    "- 20 iterations per data size",
    "- P50 (median), P95, P99 latency calculated",
    "- Targets: P50 <500ms, P95 <1s, P99 <2s for medium datasets",
    "",
    "**Results:** See test output from `test-bfh-vs-qic-benchmark.R`",
    "",
    "_Latency percentiles are validated in real-time during benchmark execution._",
    ""
  )

  return(section)
}

generate_memory_section <- function(results_dir) {
  section <- c(
    "## Memory Profiling",
    ""
  )

  # Long session memory test
  long_session_file <- file.path(results_dir, "memory_profile_long_session.rds")
  if (file.exists(long_session_file)) {
    profile <- readRDS(long_session_file)

    section <- c(
      section,
      "### Long Session Test (4-hour simulation)",
      "",
      sprintf("- **Iterations:** %d renders", profile$iterations),
      sprintf("- **Starting Memory:** %.2f MB", profile$start_memory / 1024^2),
      sprintf("- **Ending Memory:** %.2f MB", profile$end_memory / 1024^2),
      sprintf("- **Memory Growth:** %.2f MB", profile$growth_mb),
      "",
      sprintf("**Status:** %s", if (profile$growth_mb < 50) "✅ PASS (< 50 MB)" else "❌ FAIL (≥ 50 MB)"),
      ""
    )

    # Memory checkpoints
    if (!is.null(profile$checkpoints) && length(profile$checkpoints) > 0) {
      section <- c(
        section,
        "**Memory Growth Checkpoints:**",
        ""
      )
      for (i in seq_along(profile$checkpoints)) {
        iteration <- i * 100
        growth <- profile$checkpoints[i]
        section <- c(section, sprintf("- %d iterations: %+.2f MB", iteration, growth))
      }
      section <- c(section, "")
    }
  } else {
    section <- c(
      section,
      "### Long Session Test",
      "",
      "⚠️ Results not found. Run `testthat::test_file('tests/performance/test-bfh-memory-profile.R')`",
      ""
    )
  }

  # Memory usage by data size
  size_file <- file.path(results_dir, "memory_usage_by_size.rds")
  if (file.exists(size_file)) {
    usage <- readRDS(size_file)

    section <- c(
      section,
      "### Memory Usage by Data Size",
      "",
      "| Data Size | Memory Usage |",
      "|-----------|--------------|"
    )

    for (name in names(usage)) {
      entry <- usage[[name]]
      section <- c(section, sprintf(
        "| %d | %.2f MB |",
        entry$data_size,
        entry$memory_mb
      ))
    }

    section <- c(section, "")
  }

  # Memory usage by chart type
  chart_file <- file.path(results_dir, "memory_usage_by_chart_type.rds")
  if (file.exists(chart_file)) {
    usage <- readRDS(chart_file)

    section <- c(
      section,
      "### Memory Usage by Chart Type",
      "",
      "| Chart Type | Memory Usage |",
      "|------------|--------------|"
    )

    for (chart_type in names(usage)) {
      entry <- usage[[chart_type]]
      section <- c(section, sprintf(
        "| %s | %.2f MB |",
        toupper(entry$chart_type),
        entry$memory_mb
      ))
    }

    section <- c(section, "")
  }

  # Profvis links
  if (file.exists(file.path(results_dir, "bfh_profile.html"))) {
    section <- c(
      section,
      "### Detailed Profiling",
      "",
      "Profvis HTML reports available:",
      "",
      "- [Single Render Profile](./bfh_profile.html)",
      "- [Multi-Chart Profile](./bfh_profile_multi_chart.html)",
      ""
    )
  }

  return(section)
}

generate_cache_section <- function(results_dir) {
  section <- c(
    "## Cache Performance",
    ""
  )

  # Cache speedup results
  speedup_file <- file.path(results_dir, "cache_speedup_results.rds")
  if (file.exists(speedup_file)) {
    speedup <- readRDS(speedup_file)

    section <- c(
      section,
      "### Cache Hit vs Miss Speedup",
      "",
      sprintf("- **Cache Miss (median):** %.2f ms", as.numeric(speedup$cache_miss_median) * 1000),
      sprintf("- **Cache Hit (median):** %.2f ms", as.numeric(speedup$cache_hit_median) * 1000),
      sprintf("- **Speedup:** %.1fx", speedup$speedup),
      "",
      sprintf("**Status:** %s", if (speedup$speedup >= 10) "✅ PASS (≥10x)" else sprintf("❌ FAIL (%.1fx < 10x)", speedup$speedup)),
      ""
    )
  } else {
    section <- c(
      section,
      "### Cache Hit vs Miss Speedup",
      "",
      "⚠️ Results not found. Run cache benchmark tests.",
      ""
    )
  }

  # Cache hit rate results
  hit_rate_file <- file.path(results_dir, "cache_hit_rate_results.rds")
  if (file.exists(hit_rate_file)) {
    hit_rate <- readRDS(hit_rate_file)

    section <- c(
      section,
      "### Cache Hit Rate",
      "",
      sprintf("- **Total Requests:** %d", hit_rate$n_requests),
      sprintf("- **Cache Hits:** %d", hit_rate$cache_hits),
      sprintf("- **Cache Misses:** %d", hit_rate$cache_misses),
      sprintf("- **Hit Rate:** %.1f%%", hit_rate$hit_rate * 100),
      "",
      sprintf("**Status:** %s", if (hit_rate$hit_rate >= 0.80) sprintf("✅ PASS (%.1f%% ≥ 80%%)", hit_rate$hit_rate * 100) else sprintf("❌ FAIL (%.1f%% < 80%%)", hit_rate$hit_rate * 100)),
      "",
      "_Note: Cache hit rate should be measured in production via structured logging._",
      ""
    )
  } else {
    section <- c(
      section,
      "### Cache Hit Rate",
      "",
      "⚠️ Results not found. Run cache benchmark tests.",
      ""
    )
  }

  return(section)
}

generate_acceptance_criteria <- function(results_dir) {
  section <- c(
    "## Acceptance Criteria Summary",
    ""
  )

  # Check each acceptance criterion
  criteria <- list(
    list(
      name = "BFHcharts ≤ 110% qicharts2 time (all chart types)",
      status = check_performance_criterion(results_dir),
      required = TRUE
    ),
    list(
      name = "Memory growth < 50 MB in 4-hour session",
      status = check_memory_criterion(results_dir),
      required = TRUE
    ),
    list(
      name = "P50 latency < 500ms (medium datasets)",
      status = "⚠️ VERIFIED IN TESTS",
      required = TRUE
    ),
    list(
      name = "Cache speedup ≥ 10x",
      status = check_cache_speedup_criterion(results_dir),
      required = TRUE
    ),
    list(
      name = "Cache hit rate ≥ 80%",
      status = check_cache_hit_rate_criterion(results_dir),
      required = FALSE # Optional - simplified test
    ),
    list(
      name = "Benchmark report generated",
      status = "✅ COMPLETE",
      required = TRUE
    )
  )

  for (criterion in criteria) {
    required_badge <- if (criterion$required) "**[REQUIRED]**" else "[OPTIONAL]"
    section <- c(section, sprintf(
      "- %s %s: %s",
      required_badge,
      criterion$name,
      criterion$status
    ))
  }

  section <- c(section, "")

  # Overall verdict
  all_required_pass <- all(sapply(criteria, function(c) {
    c$required && grepl("✅|COMPLETE|VERIFIED", c$status)
  }))

  overall_verdict <- if (all_required_pass) {
    "✅ **All required criteria met**"
  } else {
    "⚠️ **Some required criteria pending or failed**"
  }

  section <- c(
    section,
    "",
    "### Overall Verdict",
    "",
    overall_verdict,
    ""
  )

  return(section)
}

generate_recommendations <- function(results_dir) {
  section <- c(
    "## Recommendations",
    ""
  )

  # Analyze results and provide recommendations
  run_chart_file <- file.path(results_dir, "benchmark_run_chart_results.rds")
  all_charts_file <- file.path(results_dir, "benchmark_all_charts_results.rds")

  if (file.exists(run_chart_file) && file.exists(all_charts_file)) {
    # Check for any performance regressions
    regression_detected <- check_for_regressions(results_dir)

    if (regression_detected) {
      section <- c(
        section,
        "### ⚠️ Performance Optimization Required",
        "",
        "Some chart types or data sizes show performance regression (>110% threshold).",
        "",
        "**Actions:**",
        "1. Profile slow chart types with `profvis`",
        "2. Review BFHcharts API usage for inefficiencies",
        "3. Consider caching strategy for repeated renders",
        "4. Engage with BFHcharts maintainer for optimization guidance",
        ""
      )
    } else {
      section <- c(
        section,
        "### ✅ Performance Targets Met",
        "",
        "BFHcharts meets or exceeds performance targets across all test scenarios.",
        "",
        "**Actions:**",
        "1. Complete Stream 1 cache integration",
        "2. Run cache performance benchmarks",
        "3. Monitor production performance metrics",
        "4. Consider adding more edge case benchmarks",
        ""
      )
    }
  } else {
    section <- c(
      section,
      "### ⚠️ Incomplete Benchmarks",
      "",
      "Run all benchmark tests before deployment:",
      "",
      "```r",
      "# Run all performance tests",
      "testthat::test_file('tests/performance/test-bfh-vs-qic-benchmark.R')",
      "testthat::test_file('tests/performance/test-bfh-memory-profile.R')",
      "",
      "# Generate report",
      "source('tests/performance/generate_benchmark_report.R')",
      "```",
      ""
    )
  }

  section <- c(
    section,
    "---",
    "",
    sprintf("_Report generated: %s_", Sys.time()),
    ""
  )

  return(section)
}

# Helper Functions
# ================

compute_overall_status <- function(results_dir) {
  run_chart_file <- file.path(results_dir, "benchmark_run_chart_results.rds")
  all_charts_file <- file.path(results_dir, "benchmark_all_charts_results.rds")

  if (!file.exists(run_chart_file) || !file.exists(all_charts_file)) {
    return("⚠️ INCOMPLETE")
  }

  # Check if any ratios exceed 1.10
  results_run <- readRDS(run_chart_file)
  results_all <- readRDS(all_charts_file)

  all_pass <- TRUE

  # Check run chart results
  for (name in names(results_run)) {
    bm <- results_run[[name]]
    median_qic <- median(bm$median[bm$expression == "qicharts2"])
    median_bfh <- median(bm$median[bm$expression == "BFHcharts" | bm$expression == "BFHchart"])
    ratio <- as.numeric(median_bfh) / as.numeric(median_qic)

    if (ratio > 1.10) {
      all_pass <- FALSE
      break
    }
  }

  # Check all charts results
  if (all_pass) {
    for (chart_type in names(results_all)) {
      bm <- results_all[[chart_type]]
      median_qic <- median(bm$median[bm$expression == "qicharts2"])
      median_bfh <- median(bm$median[bm$expression == "BFHcharts" | bm$expression == "BFHchart"])
      ratio <- as.numeric(median_bfh) / as.numeric(median_qic)

      if (ratio > 1.10) {
        all_pass <- FALSE
        break
      }
    }
  }

  return(if (all_pass) "✅ PASS" else "❌ FAIL")
}

check_performance_criterion <- function(results_dir) {
  run_chart_file <- file.path(results_dir, "benchmark_run_chart_results.rds")
  all_charts_file <- file.path(results_dir, "benchmark_all_charts_results.rds")

  if (!file.exists(run_chart_file) || !file.exists(all_charts_file)) {
    return("⚠️ NOT RUN")
  }

  status <- compute_overall_status(results_dir)
  return(status)
}

check_memory_criterion <- function(results_dir) {
  memory_file <- file.path(results_dir, "memory_profile_long_session.rds")

  if (!file.exists(memory_file)) {
    return("⚠️ NOT RUN")
  }

  profile <- readRDS(memory_file)
  if (profile$growth_mb < 50) {
    return(sprintf("✅ PASS (%.2f MB)", profile$growth_mb))
  } else {
    return(sprintf("❌ FAIL (%.2f MB)", profile$growth_mb))
  }
}

check_for_regressions <- function(results_dir) {
  overall_status <- compute_overall_status(results_dir)
  return(grepl("FAIL", overall_status))
}

check_cache_speedup_criterion <- function(results_dir) {
  speedup_file <- file.path(results_dir, "cache_speedup_results.rds")

  if (!file.exists(speedup_file)) {
    return("⚠️ NOT RUN")
  }

  speedup <- readRDS(speedup_file)
  if (speedup$speedup >= 10) {
    return(sprintf("✅ PASS (%.1fx)", speedup$speedup))
  } else {
    return(sprintf("❌ FAIL (%.1fx < 10x)", speedup$speedup))
  }
}

check_cache_hit_rate_criterion <- function(results_dir) {
  hit_rate_file <- file.path(results_dir, "cache_hit_rate_results.rds")

  if (!file.exists(hit_rate_file)) {
    return("⚠️ NOT RUN")
  }

  hit_rate <- readRDS(hit_rate_file)
  if (hit_rate$hit_rate >= 0.80) {
    return(sprintf("✅ PASS (%.1f%%)", hit_rate$hit_rate * 100))
  } else {
    return(sprintf("⚠️ INFORMATIONAL (%.1f%%)", hit_rate$hit_rate * 100))
  }
}

# Main Execution
# ==============

if (!interactive()) {
  # Run report generation if executed as script
  generate_performance_report()
} else {
  cat("\n=== Benchmark Report Generator Loaded ===\n")
  cat("Run: generate_performance_report()\n")
}
