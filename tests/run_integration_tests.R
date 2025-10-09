#!/usr/bin/env Rscript
# Run Integration Tests
#
# Integration tests for full workflow validation.
# These tests verify end-to-end functionality.

cat("=== Running Integration Tests ===\n\n")

# Source global.R to load the application
source("global.R")

# Run integration tests
if (dir.exists("tests/integration")) {
  testthat::test_dir("tests/integration")
} else {
  cat("No integration tests found - skipping\n")
}

cat("\n=== Integration Tests Complete ===\n")
