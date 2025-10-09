#!/usr/bin/env Rscript
# Run Performance Tests
#
# Slow performance and benchmarking tests.
# These are typically run on release branches only.

cat("=== Running Performance Tests ===\n\n")

# Source global.R to load the application
source("global.R")

# Run performance tests
testthat::test_dir("tests/performance")

cat("\n=== Performance Tests Complete ===\n")
