# Integration Tests

Sprint 5 Fase 1: Comprehensive integration testing suite for SPC App.

## Overview

Integration tests verify that multiple components work correctly together, covering:
- Complete data workflows from upload to visualization
- Session lifecycle and state management
- Error recovery and system resilience
- UI interactions and user workflows

## Test Structure

```
tests/integration/
├── README.md                      # This file
├── setup-shinytest2.R            # shinytest2 configuration and helpers
├── test-full-data-workflow.R     # End-to-end data flow testing
├── test-session-lifecycle.R      # Session management testing
├── test-error-recovery.R         # Error handling validation
└── test-ui-workflow.R            # UI integration with shinytest2
```

## Running Integration Tests

### All Integration Tests

```r
# From project root
source('tests/run_integration_tests.R')
```

### Individual Test Files

```r
# Full data workflow
testthat::test_file('tests/integration/test-full-data-workflow.R')

# Session lifecycle
testthat::test_file('tests/integration/test-session-lifecycle.R')

# Error recovery
testthat::test_file('tests/integration/test-error-recovery.R')

# UI workflow (requires shinytest2)
testthat::test_file('tests/integration/test-ui-workflow.R')
```

## Test Categories

### 1. Full Data Workflow (`test-full-data-workflow.R`)

Tests complete data processing pipeline:

- ✅ Data upload → auto-detection → plot generation
- ✅ QIC cache hit/miss behavior
- ✅ Cache invalidation on data updates
- ✅ Handling missing columns gracefully
- ✅ Chart type validation
- ✅ Event-driven workflow cascade
- ✅ Performance metrics (cache speedup)

**Example:**
```r
test_that("Complete data workflow: upload → auto-detect → plot generation", {
  app_state <- create_test_app_state()
  test_data <- create_test_data(50)

  # Upload data
  app_state$data$current_data <- test_data

  # Run auto-detection
  detected <- detect_columns(test_data)

  # Generate plot with cache
  plot <- generateSPCPlot(test_data, config, qic_cache)

  expect_true(!is.null(plot))
})
```

### 2. Session Lifecycle (`test-session-lifecycle.R`)

Tests session initialization, state transitions, and cleanup:

- ✅ App state initialization
- ✅ Startup sequence ordering
- ✅ Data upload lifecycle
- ✅ Auto-detection state transitions
- ✅ Session cleanup and reset
- ✅ Cache lifecycle management
- ✅ Concurrent state updates
- ✅ User preference persistence

**Example:**
```r
test_that("Session cleanup resets state correctly", {
  app_state <- create_test_app_state()

  # Populate session with data
  app_state$data$current_data <- test_data

  # Trigger session reset
  app_state$events$session_reset <- app_state$events$session_reset + 1L

  # Perform cleanup
  reset_session_state(app_state)

  # Verify clean state
  expect_true(is.null(app_state$data$current_data))
})
```

### 3. Error Recovery (`test-error-recovery.R`)

Tests system resilience and error handling:

- ✅ Invalid data upload recovery
- ✅ Malformed column names handling
- ✅ QIC error recovery
- ✅ Cache corruption handling
- ✅ Missing observer tolerance
- ✅ State corruption detection
- ✅ Encoding error handling
- ✅ Memory cleanup
- ✅ Concurrent operation safety
- ✅ Observer isolation

**Example:**
```r
test_that("System recovers from invalid data upload", {
  app_state <- create_test_app_state()

  # Upload invalid data
  result <- safe_operation("Handle invalid data", {
    if (nrow(data) == 0) stop("Empty data")
  }, fallback = function(e) FALSE)

  # System should recover gracefully
  expect_false(result)
  expect_true(is.null(app_state$data$current_data))
})
```

### 4. UI Workflow (`test-ui-workflow.R`)

Tests UI interactions using shinytest2:

- ✅ UI initialization
- ✅ File upload workflow
- ✅ Auto-detection UI updates
- ✅ Chart type selection
- ✅ Error message display
- ✅ Session reset

**Example:**
```r
test_that("File upload workflow completes successfully", {
  app <- create_test_app_driver()

  # Upload file
  simulate_file_upload(app, "file_upload", test_file)
  app$wait_for_idle()

  # Verify UI updates
  has_plot <- wait_for_element(app, ".plotly")
  expect_true(has_plot)

  app$stop()
})
```

## Dependencies

### Required Packages

```r
# Core testing
library(testthat)
library(shiny)

# UI testing (optional)
library(shinytest2)  # install.packages("shinytest2")
```

### Install shinytest2

```r
install.packages("shinytest2")

# Or with chromote for headless testing
install.packages(c("shinytest2", "chromote"))
```

## Test Execution Time

- **Full Data Workflow**: ~2-5 seconds
- **Session Lifecycle**: ~1-2 seconds
- **Error Recovery**: ~2-3 seconds
- **UI Workflow**: ~15-30 seconds (requires browser automation)

**Total**: ~20-40 seconds for complete integration test suite

## CI/CD Integration

### GitHub Actions Example

```yaml
name: Integration Tests

on: [push, pull_request]

jobs:
  integration-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: r-lib/actions/setup-r@v2
      - name: Install dependencies
        run: |
          install.packages(c("testthat", "shiny", "shinytest2"))
      - name: Run integration tests
        run: |
          source('tests/run_integration_tests.R')
```

## Writing New Integration Tests

### Template

```r
# test-my-integration.R
library(testthat)

context("My Integration Test Suite")

test_that("Integration scenario works correctly", {
  # 1. Setup
  app_state <- create_test_app_state()
  test_data <- create_test_data()

  # 2. Execute workflow
  app_state$data$current_data <- test_data
  app_state$events$data_updated <- app_state$events$data_updated + 1L

  # 3. Verify expectations
  expect_true(!is.null(app_state$data$current_data))
  expect_gt(app_state$events$data_updated, 0)

  # 4. Cleanup (if needed)
  app_state$data$current_data <- NULL
})
```

### Best Practices

1. **Use `safe_operation()`** for error handling tests
2. **Create isolated test data** with helper functions
3. **Verify event cascades** across multiple components
4. **Test both happy path and error scenarios**
5. **Clean up resources** (temp files, app drivers)
6. **Skip expensive tests on CRAN** with `skip_on_cran()`
7. **Document test intent** with clear descriptions

## Test Helpers

### Available Helpers

```r
# From setup-shinytest2.R
create_test_app_driver()      # Create shinytest2 AppDriver
wait_for_element()            # Wait for UI element
simulate_file_upload()        # Upload test file
check_for_errors()            # Check for error messages
get_app_state()               # Get reactive state value
create_test_data_file()       # Create temp CSV file
cleanup_test_files()          # Remove temp files

# From test files
create_test_app_state()       # Create app_state for testing
create_test_data()            # Generate test data frame
```

## Debugging Integration Tests

### Verbose Mode

```r
# Enable debug logging
Sys.setenv(SPC_LOG_LEVEL = "DEBUG")

# Run test with verbose output
testthat::test_file(
  'tests/integration/test-full-data-workflow.R',
  reporter = testthat::DebugReporter$new()
)
```

### Interactive Testing

```r
# Source test helpers
source('tests/integration/setup-shinytest2.R')

# Create app driver for manual testing
app <- create_test_app_driver()

# Interact with app
app$set_inputs(file_upload = "test.csv")
app$wait_for_idle()
app$get_html(".plotly")

# Remember to stop when done
app$stop()
```

## Known Issues

### shinytest2 Chromium Dependency

If UI tests fail with Chromium errors:

```r
# Install chromote and update Chromium
install.packages("chromote")
chromote::set_chrome_args(c(
  "--disable-gpu",
  "--no-sandbox",
  "--disable-dev-shm-usage"
))
```

### Timeout Issues

If tests timeout:

```r
# Increase timeout in create_test_app_driver()
app <- create_test_app_driver(timeout = 20, load_timeout = 10)
```

## Coverage Goals

- ✅ **Data workflow**: Complete end-to-end coverage
- ✅ **Session management**: All lifecycle transitions
- ✅ **Error recovery**: Major error scenarios
- ✅ **UI interactions**: Core user workflows

**Current Coverage**: ~85% of critical integration paths

## Future Enhancements

- [ ] Performance benchmarking integration
- [ ] Multi-user concurrent access testing
- [ ] Mobile/responsive UI testing
- [ ] Accessibility testing
- [ ] Cross-browser testing matrix

## References

- [testthat Documentation](https://testthat.r-lib.org/)
- [shinytest2 Guide](https://rstudio.github.io/shinytest2/)
- [Sprint 5 Plan](../../docs/SPRINT5_PLAN.md)
- [Test Organization](../README.md)
