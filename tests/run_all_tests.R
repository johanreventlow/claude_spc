#!/usr/bin/env Rscript
# Run All Tests
#
# Comprehensive test suite including unit, performance, and integration tests.
# Used for full validation before releases.

cat("=== Running All Tests ===\n\n")

# Track start time
start_time <- Sys.time()

# Run unit tests
cat("\n--- Unit Tests ---\n")
source("tests/run_unit_tests.R")

# Run performance tests
cat("\n--- Performance Tests ---\n")
source("tests/run_performance_tests.R")

# Run integration tests
cat("\n--- Integration Tests ---\n")
source("tests/run_integration_tests.R")

# Summary
end_time <- Sys.time()
duration <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 2)

cat("\n=== All Tests Complete ===\n")
cat("Total time:", duration, "seconds\n")
