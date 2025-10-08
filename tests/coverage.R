#!/usr/bin/env Rscript
# coverage.R
# Test coverage reporting script for SPC App
#
# Usage:
#   R -e "source('tests/coverage.R')"
#   Rscript tests/coverage.R
#
# Output:
#   - Console summary of coverage metrics
#   - HTML report in coverage/index.html (optional)

# Ensure covr is available
if (!requireNamespace("covr", quietly = TRUE)) {
  stop(
    "Package 'covr' is required for coverage reporting.\n",
    "Install with: install.packages('covr')"
  )
}

# Configuration
COVERAGE_CONFIG <- list(
  # Coverage thresholds (percentage)
  target_coverage = 90,
  critical_paths_target = 100,

  # Output options
  generate_html = TRUE,
  html_output_dir = "coverage",

  # Coverage scope
  exclude_patterns = c(
    "R/zzz\\.R$",              # Package hooks
    "R/app_dependencies\\.R$", # Dependency management (tested via integration)
    "R/golem_utils\\.R$"       # Golem utilities (external)
  ),

  # Critical paths requiring 100% coverage
  critical_paths = c(
    "R/state_management\\.R$",
    "R/utils_error_handling\\.R$",
    "R/fct_file_operations\\.R$"
  )
)

# Helper function: Format coverage percentage
format_coverage <- function(coverage_value) {
  sprintf("%.1f%%", coverage_value)
}

# Helper function: Color-coded coverage status
coverage_status <- function(coverage_value, threshold = 90) {
  if (coverage_value >= threshold) {
    "✅ PASS"
  } else if (coverage_value >= threshold - 10) {
    "⚠️  WARN"
  } else {
    "❌ FAIL"
  }
}

# Main coverage function
run_coverage_report <- function(config = COVERAGE_CONFIG) {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  SPC App Test Coverage Report\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")

  # Calculate coverage
  cat("📊 Calculating test coverage...\n")
  coverage <- covr::package_coverage(
    path = ".",
    type = "all",
    quiet = FALSE,
    clean = TRUE
  )

  # Overall coverage metrics
  overall_coverage <- covr::percent_coverage(coverage)
  cat("\n")
  cat("─────────────────────────────────────────────────────────────\n")
  cat("  Overall Coverage Metrics\n")
  cat("─────────────────────────────────────────────────────────────\n")
  cat(sprintf("  Total Coverage: %s %s\n",
              format_coverage(overall_coverage),
              coverage_status(overall_coverage, config$target_coverage)))
  cat(sprintf("  Target: %s\n", format_coverage(config$target_coverage)))
  cat("\n")

  # File-level coverage
  cat("─────────────────────────────────────────────────────────────\n")
  cat("  File-Level Coverage\n")
  cat("─────────────────────────────────────────────────────────────\n")

  file_coverage <- covr::file_coverage(coverage)
  file_df <- data.frame(
    File = names(file_coverage),
    Coverage = sapply(file_coverage, function(x) as.numeric(x)),
    stringsAsFactors = FALSE
  )
  file_df <- file_df[order(file_df$Coverage), ]

  # Show low coverage files first (most important to improve)
  low_coverage <- file_df[file_df$Coverage < config$target_coverage, ]
  if (nrow(low_coverage) > 0) {
    cat("\n  ⚠️  Files Below Target Coverage:\n\n")
    for (i in 1:nrow(low_coverage)) {
      cat(sprintf("    %s: %s\n",
                  format_coverage(low_coverage$Coverage[i]),
                  basename(low_coverage$File[i])))
    }
  }

  # Show high coverage files
  high_coverage <- file_df[file_df$Coverage >= config$target_coverage, ]
  if (nrow(high_coverage) > 0) {
    cat("\n  ✅ Files Meeting Target Coverage:\n\n")
    displayed <- min(5, nrow(high_coverage))
    for (i in 1:displayed) {
      cat(sprintf("    %s: %s\n",
                  format_coverage(high_coverage$Coverage[i]),
                  basename(high_coverage$File[i])))
    }
    if (nrow(high_coverage) > displayed) {
      cat(sprintf("    ... and %d more files\n",
                  nrow(high_coverage) - displayed))
    }
  }
  cat("\n")

  # Critical paths coverage
  cat("─────────────────────────────────────────────────────────────\n")
  cat("  Critical Paths Coverage (100% Required)\n")
  cat("─────────────────────────────────────────────────────────────\n")

  critical_files <- file_df[grepl(paste(config$critical_paths, collapse = "|"),
                                  file_df$File), ]
  if (nrow(critical_files) > 0) {
    for (i in 1:nrow(critical_files)) {
      status <- coverage_status(critical_files$Coverage[i], 100)
      cat(sprintf("  %s %s: %s\n",
                  status,
                  format_coverage(critical_files$Coverage[i]),
                  basename(critical_files$File[i])))
    }
  } else {
    cat("  ⚠️  No critical paths found\n")
  }
  cat("\n")

  # Zero coverage files (critical issue)
  zero_coverage <- file_df[file_df$Coverage == 0, ]
  if (nrow(zero_coverage) > 0) {
    cat("─────────────────────────────────────────────────────────────\n")
    cat("  ❌ Files with Zero Coverage (CRITICAL)\n")
    cat("─────────────────────────────────────────────────────────────\n")
    for (i in 1:nrow(zero_coverage)) {
      cat(sprintf("    %s\n", basename(zero_coverage$File[i])))
    }
    cat("\n")
  }

  # Generate HTML report if requested
  if (config$generate_html) {
    cat("─────────────────────────────────────────────────────────────\n")
    cat("  HTML Report Generation\n")
    cat("─────────────────────────────────────────────────────────────\n")

    if (!dir.exists(config$html_output_dir)) {
      dir.create(config$html_output_dir, recursive = TRUE)
    }

    tryCatch({
      covr::report(
        coverage,
        file = file.path(config$html_output_dir, "index.html"),
        browse = FALSE
      )
      cat(sprintf("  ✅ HTML report generated: %s/index.html\n",
                  config$html_output_dir))
    }, error = function(e) {
      cat(sprintf("  ⚠️  HTML report generation failed: %s\n", e$message))
    })
    cat("\n")
  }

  # Summary and recommendations
  cat("─────────────────────────────────────────────────────────────\n")
  cat("  Summary & Recommendations\n")
  cat("─────────────────────────────────────────────────────────────\n")

  if (overall_coverage >= config$target_coverage) {
    cat(sprintf("  ✅ Overall coverage (%s) meets target (%s)\n",
                format_coverage(overall_coverage),
                format_coverage(config$target_coverage)))
  } else {
    gap <- config$target_coverage - overall_coverage
    cat(sprintf("  ⚠️  Coverage gap: %s (need %s to reach target)\n",
                format_coverage(gap),
                format_coverage(config$target_coverage)))
    cat("\n  Focus areas for improvement:\n")
    if (nrow(low_coverage) > 0) {
      top_priority <- head(low_coverage, 3)
      for (i in 1:nrow(top_priority)) {
        cat(sprintf("    • %s (%s)\n",
                    basename(top_priority$File[i]),
                    format_coverage(top_priority$Coverage[i])))
      }
    }
  }
  cat("\n")

  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  Coverage report complete\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")

  # Return coverage object for programmatic use
  invisible(list(
    coverage = coverage,
    overall_coverage = overall_coverage,
    file_coverage = file_df,
    meets_target = overall_coverage >= config$target_coverage
  ))
}

# Run coverage report if executed as script
if (!interactive()) {
  result <- run_coverage_report()

  # Exit with error code if coverage below target
  if (!result$meets_target) {
    quit(status = 1)
  }
}
