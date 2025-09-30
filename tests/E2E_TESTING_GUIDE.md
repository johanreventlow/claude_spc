# E2E Testing Guide - shinytest2

## Overview

This guide documents End-to-End (E2E) testing patterns for the SPC App using shinytest2.

**Purpose**: Verify complete user workflows with actual UI interactions and visual regression testing.

**Test Location**: `tests/testthat/test-e2e-user-workflows.R`

## Quick Start

### Running E2E Tests

```r
# Run all E2E tests
testthat::test_file("tests/testthat/test-e2e-user-workflows.R")

# Run specific test
testthat::test_that("E2E: App launches successfully", { ... })

# Run in RStudio
# Use "Run Tests" button with test file open
```

### Prerequisites

1. **shinytest2 installed**: Already available (v0.4.1)
2. **Chromium/Chrome**: Required for headless browser testing
3. **Test data files**: Created automatically in tests

## Test Structure

### Basic E2E Test Pattern

```r
test_that("E2E: Test description", {
  skip_if_not_installed("shinytest2")
  skip_on_ci()  # Skip in CI unless configured

  # 1. SETUP: Create test data
  test_data <- data.frame(...)
  temp_file <- tempfile(fileext = ".csv")
  write.csv(test_data, temp_file, row.names = FALSE)

  # 2. LAUNCH: Start app
  app <- AppDriver$new(
    app_dir = "../../",        # Root of project
    name = "test_name",        # Unique test name
    height = 800,
    width = 1200
  )

  # 3. WAIT: Let app initialize
  app$wait_for_idle()

  # 4. INTERACT: Perform user actions
  app$upload_file(data_file = temp_file)
  app$wait_for_idle(duration = 2000)

  # 5. VERIFY: Check expectations
  app$expect_screenshot()    # Visual regression
  values <- app$get_values()  # State verification
  expect_true(!is.null(values))

  # 6. CLEANUP: Stop app and remove temp files
  unlink(temp_file)
  app$stop()
})
```

## Core E2E Test Patterns

### 1. File Upload Pattern

```r
# Create test data
test_data <- data.frame(
  Dato = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
  Værdi = c(10, 20, 30)
)

temp_file <- tempfile(fileext = ".csv")
write.csv(test_data, temp_file, row.names = FALSE)

# Upload to app
app$upload_file(data_file = temp_file)
app$wait_for_idle(duration = 2000)

# Verify upload succeeded
app$expect_screenshot("after_upload")
```

### 2. Input Selection Pattern

```r
# Set input values
tryCatch({
  app$set_inputs(
    chart_type = "P-kort (Andele)",
    x_column = "Dato",
    y_column = "Værdi"
  )
  app$wait_for_idle(duration = 1000)
}, error = function(e) {
  message("Could not set inputs: ", e$message)
})

# Verify selection
app$expect_screenshot("after_selection")
```

### 3. Multi-Step Workflow Pattern

```r
# PHASE 1: Upload
app$upload_file(data_file = temp_file)
app$wait_for_idle(duration = 2000)
app$expect_screenshot("01_uploaded")

# PHASE 2: Auto-detect
app$wait_for_idle(duration = 1500)
app$expect_screenshot("02_autodetected")

# PHASE 3: Generate chart
app$set_inputs(chart_type = "Run chart")
app$wait_for_idle(duration = 1500)
app$expect_screenshot("03_chart_generated")
```

### 4. Error Handling Pattern

```r
# Upload invalid data
bad_data <- data.frame(
  ColA = c("text", "only", "data"),
  ColB = c("no", "numbers", "here")
)

temp_file <- tempfile(fileext = ".csv")
write.csv(bad_data, temp_file, row.names = FALSE)

app$upload_file(data_file = temp_file)
app$wait_for_idle(duration = 2000)

# App should still run (not crash)
expect_true(app$is_running())
app$expect_screenshot("error_state")
```

## AppDriver Methods

### Essential Methods

**Initialization:**
```r
app <- AppDriver$new(
  app_dir = "path/to/app",
  name = "test_name",
  height = 800,
  width = 1200,
  load_timeout = 10000  # 10 seconds
)
```

**Waiting:**
```r
app$wait_for_idle()                    # Wait for all async operations
app$wait_for_idle(duration = 2000)    # Wait 2 seconds minimum
app$wait_for_value(input = "x_column") # Wait for specific input
```

**Interactions:**
```r
app$upload_file(data_file = "path/to/file.csv")
app$set_inputs(input_id = "value")
app$click("button_id")
```

**Verification:**
```r
app$expect_screenshot()                 # Visual regression
app$expect_screenshot("custom_name")    # Named screenshot
values <- app$get_values()              # Get all reactive values
app$get_value(input = "x_column")      # Get specific input
```

**State:**
```r
app$is_running()                        # Check if app is alive
app$get_logs()                          # Get console logs
app$stop()                              # Cleanup
```

## Screenshot Management

### Visual Regression Testing

shinytest2 automatically manages screenshot baselines:

1. **First run**: Creates baseline screenshots in `tests/testthat/_snaps/`
2. **Subsequent runs**: Compares against baseline
3. **Differences**: Fails test and shows visual diff

**Screenshot Best Practices:**
- Use descriptive names: `app$expect_screenshot("after_upload")`
- Take screenshots at key workflow steps
- Verify UI state, not just functionality
- Screenshots are OS/resolution specific

### Managing Snapshots

```bash
# Accept new snapshots (after reviewing diffs)
testthat::snapshot_accept("test_name")

# Review snapshot differences
# Diffs are shown in test output with paths to images

# Delete outdated snapshots
rm -rf tests/testthat/_snaps/test_name/
```

## Test Data Patterns

### Simple Test Data

```r
test_data <- data.frame(
  Dato = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
  Værdi = c(10, 20, 30)
)
```

### SPC-Specific Test Data

```r
# P-chart data (proportions with denominators)
p_chart_data <- data.frame(
  Dato = seq.Date(as.Date("2023-01-01"), by = "week", length.out = 20),
  Komplikationer = rpois(20, lambda = 5),
  Operationer = rep(100, 20),
  Kommentar = c(rep("", 19), "Outlier")
)

# Run chart data (continuous values)
run_chart_data <- data.frame(
  Dato = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 30),
  Værdi = rnorm(30, mean = 50, sd = 10)
)

# Danish format data
danish_data <- data.frame(
  Dato = c("01-01-2023", "02-01-2023", "03-01-2023"),
  Værdi = c("5,5", "10,2", "15,8")  # Comma as decimal separator
)
```

### Helper Function

```r
create_e2e_test_data <- function(n_rows = 20, include_denominator = TRUE) {
  data <- data.frame(
    Dato = seq.Date(as.Date("2023-01-01"), by = "day", length.out = n_rows),
    Værdi = rnorm(n_rows, mean = 50, sd = 10)
  )

  if (include_denominator) {
    data$Nævner <- rep(100, n_rows)
  }

  data
}
```

## CI/CD Integration

### Skipping E2E Tests on CI

```r
test_that("E2E: Test name", {
  skip_on_ci()  # Skip unless CI is configured for headless testing
  # ... test code ...
})
```

### Configuring CI for E2E

For GitHub Actions or similar:

```yaml
# .github/workflows/test.yml
- name: Install Chrome
  run: |
    sudo apt-get install -y chromium-browser

- name: Run E2E Tests
  run: |
    Rscript -e "testthat::test_file('tests/testthat/test-e2e-user-workflows.R')"
  env:
    CHROMOTE_CHROME: /usr/bin/chromium-browser
```

## Debugging E2E Tests

### Common Issues

**1. App doesn't load**
```r
# Increase timeout
app <- AppDriver$new(
  app_dir = "../../",
  load_timeout = 20000  # 20 seconds
)
```

**2. Timing issues**
```r
# Add explicit waits
app$wait_for_idle(duration = 3000)  # Wait longer

# Or wait for specific condition
app$wait_for_value(input = "x_column", timeout = 5000)
```

**3. Input not settable**
```r
# Wrap in tryCatch
tryCatch({
  app$set_inputs(chart_type = "Run chart")
}, error = function(e) {
  message("Could not set input: ", e$message)
  # Use alternative approach or skip
})
```

**4. Screenshot differences**
```r
# Review diffs carefully
# Accept if intentional UI change
testthat::snapshot_accept("test_name")
```

### Debug Mode

```r
# Enable verbose logging
app <- AppDriver$new(
  app_dir = "../../",
  name = "debug_test",
  shiny_args = list(
    options = list(
      shiny.trace = TRUE  # Enable Shiny tracing
    )
  )
)

# Get console logs
logs <- app$get_logs()
print(logs)
```

## Best Practices

### 1. Test Independence

- Each test should be completely independent
- Create fresh test data for each test
- Don't rely on order of test execution

### 2. Wait Strategically

```r
# Good: Explicit waits after actions
app$upload_file(data_file = temp_file)
app$wait_for_idle(duration = 2000)

# Better: Wait for specific condition
app$upload_file(data_file = temp_file)
app$wait_for_value(output = "data_table")
```

### 3. Cleanup

```r
# Always cleanup in test
on.exit({
  unlink(temp_file)
  app$stop()
})
```

### 4. Meaningful Screenshots

```r
# Bad: Generic names
app$expect_screenshot()

# Good: Descriptive names
app$expect_screenshot("after_upload")
app$expect_screenshot("chart_with_comments")
app$expect_screenshot("error_invalid_data")
```

### 5. Error Context

```r
# Provide context for failures
tryCatch({
  app$set_inputs(chart_type = "P-kort")
  app$wait_for_idle()
}, error = function(e) {
  message("Failed to set chart type: ", e$message)
  message("Available values: ", app$get_value(input = "chart_type"))
  stop(e)
})
```

## Test Coverage Goals

### Current E2E Tests (8 tests)

1. ✅ App launch verification
2. ✅ File upload workflow
3. ✅ Auto-detection workflow
4. ✅ Chart generation workflow
5. ✅ Column selection workflow
6. ✅ Table edit workflow
7. ✅ Error handling
8. ✅ Complete user journey

### Future E2E Tests (Recommended)

- Multi-file upload scenarios
- Chart type switching
- Download functionality
- Session persistence
- Mobile responsive testing
- Accessibility testing

## Resources

- [shinytest2 Documentation](https://rstudio.github.io/shinytest2/)
- [AppDriver Reference](https://rstudio.github.io/shinytest2/reference/AppDriver.html)
- [Visual Testing Best Practices](https://rstudio.github.io/shinytest2/articles/use-package.html)

## Troubleshooting

### Issue: "Chrome not found"

**Solution**: Install Chromium
```bash
# macOS
brew install chromium

# Ubuntu/Debian
sudo apt-get install chromium-browser

# Set environment variable
export CHROMOTE_CHROME=/path/to/chromium
```

### Issue: "Timeout waiting for app"

**Solution**: Increase load timeout or check app startup
```r
app <- AppDriver$new(
  app_dir = "../../",
  load_timeout = 30000  # 30 seconds
)

# Check if app starts manually
shiny::runApp("../../")
```

### Issue: "Screenshot mismatch"

**Solution**: Review and accept if intentional
```r
# Review diff images in tests/testthat/_snaps/
# If intentional UI change, accept:
testthat::snapshot_accept("test_name")
```

## Summary

E2E tests with shinytest2 provide:
- ✅ Full user workflow verification
- ✅ Visual regression testing
- ✅ UI interaction validation
- ✅ Real browser testing
- ✅ Screenshot-based verification

Use E2E tests for critical user journeys and supplement with unit/integration tests for detailed logic verification.