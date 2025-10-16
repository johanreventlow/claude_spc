#!/usr/bin/env Rscript
# Simple Backend Switching Test for Task #32 Stream A - Step 3
# Tests wrapper function structure without requiring full package load

cat("========================================\n")
cat("Task #32 - Backend Switching Test\n")
cat("========================================\n\n")

# Source the wrapper function file directly
cat("Loading wrapper function...\n")
source_result <- tryCatch({
  source("R/fct_spc_plot_generation.R", local = TRUE)
  cat("✓ fct_spc_plot_generation.R loaded successfully\n\n")
  TRUE
}, error = function(e) {
  cat("✗ ERROR loading file:", e$message, "\n")
  FALSE
})

if (!source_result) {
  quit(save = "no", status = 1)
}

# ==============================================================================
# TEST 1: Function Structure
# ==============================================================================

cat("TEST 1: Function Structure\n")
cat("--------------------------\n")

test1_results <- list()

# Check if generateSPCPlot_with_backend exists
if (exists("generateSPCPlot_with_backend")) {
  cat("✓ generateSPCPlot_with_backend() exists\n")
  test1_results$wrapper_exists <- TRUE
} else {
  cat("✗ generateSPCPlot_with_backend() NOT found\n")
  test1_results$wrapper_exists <- FALSE
}

# Check if generateSPCPlot_qicharts2 exists
if (exists("generateSPCPlot_qicharts2")) {
  cat("✓ generateSPCPlot_qicharts2() exists\n")
  test1_results$qicharts2_exists <- TRUE
} else {
  cat("✗ generateSPCPlot_qicharts2() NOT found\n")
  test1_results$qicharts2_exists <- FALSE
}

# Check if legacy alias exists
if (exists("generateSPCPlot")) {
  cat("✓ generateSPCPlot() legacy alias exists\n")

  # Check if it's aliased correctly
  if (is.function(generateSPCPlot)) {
    # Compare function bodies (they should be identical for alias)
    cat("✓ generateSPCPlot is a function\n")
    test1_results$alias_exists <- TRUE
  } else {
    cat("✗ generateSPCPlot is not a function\n")
    test1_results$alias_exists <- FALSE
  }
} else {
  cat("✗ generateSPCPlot() legacy alias NOT found\n")
  test1_results$alias_exists <- FALSE
}

cat("\n")

# ==============================================================================
# TEST 2: Feature Flag Configuration
# ==============================================================================

cat("TEST 2: Feature Flag Configuration\n")
cat("-----------------------------------\n")

test2_results <- list()

# Check if golem-config.yml has features section
config_check <- tryCatch({
  library(yaml)
  config <- yaml::read_yaml("inst/golem-config.yml")

  if ("default" %in% names(config)) {
    default_config <- config$default

    if ("features" %in% names(default_config)) {
      features <- default_config$features

      cat("✓ Features section found in inst/golem-config.yml\n")

      # Check use_bfhchart flag
      if ("use_bfhchart" %in% names(features)) {
        cat("✓ use_bfhchart flag exists:", features$use_bfhchart, "\n")
        test2_results$flag_exists <- TRUE
        test2_results$flag_value <- features$use_bfhchart
      } else {
        cat("✗ use_bfhchart flag NOT found\n")
        test2_results$flag_exists <- FALSE
      }

      # Check supported types
      if ("bfhchart_supported_types" %in% names(features)) {
        types <- features$bfhchart_supported_types
        cat("✓ Supported types defined:", paste(types, collapse = ", "), "\n")
        test2_results$supported_types <- types
      } else {
        cat("✗ bfhchart_supported_types NOT found\n")
      }

      # Check version requirement
      if ("bfhchart_version_required" %in% names(features)) {
        cat("✓ Version requirement:", features$bfhchart_version_required, "\n")
      }

    } else {
      cat("✗ Features section NOT found in default config\n")
      test2_results$flag_exists <- FALSE
    }
  } else {
    cat("✗ Default config section NOT found\n")
    test2_results$flag_exists <- FALSE
  }

  TRUE
}, error = function(e) {
  cat("✗ ERROR reading config:", e$message, "\n")
  FALSE
})

cat("\n")

# ==============================================================================
# TEST 3: Wrapper Function Logic (Static Analysis)
# ==============================================================================

cat("TEST 3: Wrapper Function Logic\n")
cat("-------------------------------\n")

test3_results <- list()

# Read the function source to check for key patterns
wrapper_source <- readLines("R/fct_spc_plot_generation.R")

# Check for feature flag reading
if (any(grepl("golem::get_golem_options.*features", wrapper_source))) {
  cat("✓ Feature flag reading code detected\n")
  test3_results$reads_flag <- TRUE
} else {
  cat("✗ Feature flag reading code NOT detected\n")
  test3_results$reads_flag <- FALSE
}

# Check for backend selection logic
if (any(grepl("if.*use_bfhchart", wrapper_source))) {
  cat("✓ Backend selection logic detected\n")
  test3_results$has_selection <- TRUE
} else {
  cat("✗ Backend selection logic NOT detected\n")
  test3_results$has_selection <- FALSE
}

# Check for error handling (tryCatch)
if (any(grepl("tryCatch", wrapper_source))) {
  cat("✓ Error handling (tryCatch) detected\n")
  test3_results$has_error_handling <- TRUE
} else {
  cat("✗ Error handling NOT detected\n")
  test3_results$has_error_handling <- FALSE
}

# Check for fallback to qicharts2
if (any(grepl("generateSPCPlot_qicharts2", wrapper_source))) {
  cat("✓ Fallback to qicharts2 detected\n")
  test3_results$has_fallback <- TRUE
} else {
  cat("✗ Fallback to qicharts2 NOT detected\n")
  test3_results$has_fallback <- FALSE
}

# Check for logging
if (any(grepl("log_info|log_debug|log_warn|log_error", wrapper_source))) {
  cat("✓ Structured logging detected\n")
  test3_results$has_logging <- TRUE
} else {
  cat("✗ Structured logging NOT detected\n")
  test3_results$has_logging <- FALSE
}

cat("\n")

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("========================================\n")
cat("TEST SUMMARY\n")
cat("========================================\n\n")

# Count passed tests
test1_passed <- test1_results$wrapper_exists && test1_results$qicharts2_exists && test1_results$alias_exists
test2_passed <- test2_results$flag_exists && !is.null(test2_results$flag_value)
test3_passed <- test3_results$reads_flag && test3_results$has_selection &&
                test3_results$has_error_handling && test3_results$has_fallback &&
                test3_results$has_logging

if (test1_passed) {
  cat("✓ TEST 1: Function structure - PASSED\n")
} else {
  cat("✗ TEST 1: Function structure - FAILED\n")
}

if (test2_passed) {
  cat("✓ TEST 2: Feature flag configuration - PASSED\n")
} else {
  cat("✗ TEST 2: Feature flag configuration - FAILED\n")
}

if (test3_passed) {
  cat("✓ TEST 3: Wrapper function logic - PASSED\n")
} else {
  cat("✗ TEST 3: Wrapper function logic - FAILED\n")
}

cat("\n")

total_tests <- 3
passed_tests <- sum(test1_passed, test2_passed, test3_passed)

cat("RESULT:", passed_tests, "of", total_tests, "tests passed\n")

if (passed_tests == total_tests) {
  cat("\n🎉 All tests PASSED - Backend wrapper correctly implemented!\n\n")
  cat("Next Steps:\n")
  cat("  1. ✓ Feature flag configuration - COMPLETE\n")
  cat("  2. ✓ Backend wrapper creation - COMPLETE\n")
  cat("  3. ✓ Static validation - COMPLETE\n")
  cat("  4. → Ready for runtime testing with actual app\n\n")
  quit(save = "no", status = 0)
} else {
  cat("\n⚠ Some tests FAILED - Review errors above\n")
  quit(save = "no", status = 1)
}
