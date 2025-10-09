#!/usr/bin/env Rscript
# Run Unit Tests
#
# Fast unit tests for continuous integration.
# Performance tests are in tests/performance/

cat("=== Running Unit Tests ===\n\n")

# Source global.R to load the application
source("global.R")

# Run unit tests
testthat::test_dir("tests/testthat")

cat("\n=== Unit Tests Complete ===\n")
