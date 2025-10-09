# Test Coverage Analysis Report

**Dato**: 2025-10-09
**Agent**: test-coverage-analyzer
**Score**: 40/100 (Under target)

## Executive Summary

Based på comprehensive analysis af SPC application test suite, current coverage er **35-40% overall** (target: ≥90%). Der findes **99 test files**, men **58 af 78 source files (73%) har INGEN dedikerede tests**. Critical paths har circa 50% coverage (target: 100%).

**Kritisk Gap:** De vigtigste komponenter - plot generation, visualization module, og event orchestration - har minimal eller ingen test coverage.

---

## Current Test Coverage Status

### Overall Metrics

| Metric | Current | Target | Status |
|--------|---------|--------|--------|
| **Overall Coverage** | 35-40% | ≥90% | ❌ **CRITICAL** |
| **Critical Paths** | ~50% | 100% | ❌ **HIGH RISK** |
| **Edge Cases** | ~20% | 80% | ❌ **HIGH RISK** |
| **Test Files** | 99 files | - | ✅ Good infrastructure |
| **Untested Files** | 58/78 (73%) | 0% | ❌ **CRITICAL** |

---

## Critical Gaps (Zero Test Coverage)

### 1. **Plot Generation System (ZERO COVERAGE)**

**File:** `/Users/johanreventlow/Documents/R/claude_spc/R/fct_spc_plot_generation.R` (1,438 lines)
**Status:** ❌ **NO TESTS**
**Risk:** **KRITISK** - Core functionality untested

**Missing Test Coverage:**

```r
# tests/testthat/test-spc-plot-generation.R (SHOULD EXIST)

test_that("generateSPCPlot handles basic run chart", {
  # Setup
  data <- data.frame(
    x = 1:20,
    y = rnorm(20, mean = 100, sd = 10)
  )

  config <- list(
    x_col = "x",
    y_col = "y"
  )

  # Execute
  result <- generateSPCPlot(
    data = data,
    config = config,
    chart_type = "run",
    y_axis_unit = "count"
  )

  # Assert
  expect_type(result, "list")
  expect_true("plot" %in% names(result))
  expect_true("qic_data" %in% names(result))
  expect_s3_class(result$plot, "ggplot")
})

test_that("generateSPCPlot handles empty data gracefully", {
  data <- data.frame(x = numeric(0), y = numeric(0))

  expect_error(
    generateSPCPlot(data, config, "run"),
    "Data kan ikke være tom"
  )
})

test_that("generateSPCPlot applies correct y-axis unit formatting", {
  # Test percent formatting
  result_pct <- generateSPCPlot(data, config, "p", y_axis_unit = "percent")
  expect_match(result_pct$plot$labels$y, "%")

  # Test count formatting with K/M notation
  large_data <- data.frame(x = 1:10, y = seq(1000, 10000, by = 1000))
  result_count <- generateSPCPlot(large_data, config, "run", y_axis_unit = "count")
  # Should use "K" notation for thousands
})

test_that("generateSPCPlot handles multi-part data", {
  data_multipart <- data.frame(
    x = rep(1:10, 3),
    y = rnorm(30, 100, 10),
    part = rep(c("A", "B", "C"), each = 10)
  )

  result <- generateSPCPlot(data_multipart, config, "i")

  # Should create separate control limits for each part
  expect_true(length(unique(result$qic_data$part)) == 3)
})
```

**Prioritet**: **KRITISK**
**Effort**: 8-12 timer

---

### 2. **Visualization Module (ZERO COVERAGE)**

**File:** `/Users/johanreventlow/Documents/R/claude_spc/R/mod_spc_chart_server.R` (1,238 lines)
**Status:** ❌ **NO TESTS**
**Risk:** **KRITISK** - Complex reactive chains untested

**Missing Test Coverage:**

```r
# tests/testthat/test-mod-spc-chart-integration.R (SHOULD EXIST)

test_that("Visualization module updates plot when data changes", {
  testServer(visualizationModuleServer, args = list(
    data_reactive = reactive(test_data),
    column_config_reactive = reactive(test_config),
    chart_type_reactive = reactive("run"),
    app_state = test_app_state
  ), {
    # Trigger data update
    session$setInputs(trigger_update = 1)

    # Assert plot updated
    expect_true(!is.null(output$plot))
    expect_s3_class(output$plot$src, "ggplot")
  })
})

test_that("Module handles cache updates atomically", {
  # Test atomic cache update flag
  # Simulate concurrent updates
  # Verify no race conditions
})

test_that("Module degrades gracefully on plot generation failure", {
  # Inject error in plot generation
  # Verify error handling
  # Verify user sees appropriate message
})
```

**Prioritet**: **KRITISK**
**Effort**: 10-12 timer

---

### 3. **Event Orchestration (ZERO COVERAGE)**

**File:** `/Users/johanreventlow/Documents/R/claude_spc/R/utils_server_event_listeners.R` (1,265 lines)
**Status:** ❌ **NO TESTS**
**Risk:** **KRITISK** - Event chains untested

**Missing Test Coverage:**

```r
# tests/testthat/test-event-listener-integration.R (SHOULD EXIST)

test_that("data_updated event triggers autodetect cascade", {
  # Setup mock app_state
  app_state <- create_test_app_state()
  emit <- create_emit_api(app_state)

  # Track event sequence
  events_fired <- character(0)

  # Mock observers
  observeEvent(app_state$events$data_updated, {
    events_fired <- c(events_fired, "data_updated")
  })

  observeEvent(app_state$events$auto_detection_started, {
    events_fired <- c(events_fired, "auto_detection_started")
  })

  # Trigger
  emit$data_updated("upload")

  # Assert sequence
  expect_equal(events_fired, c("data_updated", "auto_detection_started"))
})

test_that("Observer priorities execute in correct order", {
  # Verify STATE_MANAGEMENT runs before AUTO_DETECT
  # Verify AUTO_DETECT runs before UI_SYNC
})

test_that("Event listeners handle cleanup on session end", {
  # Simulate session end
  # Verify all observers destroyed
  # Verify no memory leaks
})
```

**Prioritet**: **KRITISK**
**Effort**: 8-10 timer

---

### 4. **Auto-Detection Engine (ZERO COVERAGE)**

**Files:**
- `/Users/johanreventlow/Documents/R/claude_spc/R/fct_autodetect_unified.R` (394 lines) - ❌ NO TESTS
- `/Users/johanreventlow/Documents/R/claude_spc/R/fct_autodetect_helpers.R` (795 lines) - ❌ NO TESTS

**Risk:** **HØJ** - Core heuristics untested

**Missing Test Coverage:**

```r
# tests/testthat/test-autodetect-engine.R (SHOULD EXIST)

test_that("Auto-detection identifies date columns correctly", {
  data <- data.frame(
    dato = c("2023-01-01", "2023-01-02", "2023-01-03"),
    værdi = c(100, 105, 110)
  )

  result <- autodetect_engine(data, app_state, "manual")

  expect_equal(result$x_col, "dato")
  expect_equal(result$y_col, "værdi")
})

test_that("Auto-detection handles Danish month names", {
  data <- data.frame(
    måned = c("jan 2023", "feb 2023", "mar 2023"),
    antal = c(50, 55, 60)
  )

  result <- autodetect_engine(data, app_state, "manual")

  # Should detect Danish months and convert
  expect_true(!is.null(result$x_col))
})

test_that("Auto-detection respects frozen state", {
  # Set frozen flag
  app_state$columns$auto_detect$frozen_until_next_trigger <- TRUE

  result <- autodetect_engine(data, app_state, "data_loaded")

  # Should skip detection
  expect_null(result)
})

test_that("Auto-detection handles ambiguous data gracefully", {
  # Multiple numeric columns
  # No clear date column
  # Should return NULL or prompt user
})
```

**Prioritet**: **HØJ**
**Effort**: 6-8 timer

---

## Edge Case Coverage Gaps

### Missing Edge Case Tests

1. **Empty Data**
   - ❌ Empty data.frame handling
   - ❌ Single row data
   - ❌ Single column data

2. **Danish Characters**
   - ❌ Danish letters (æ, ø, å) i column names
   - ❌ Danish date formats
   - ❌ Danish decimal separators (komma vs punkt)

3. **Extreme Values**
   - ❌ Very large numbers (>1e9)
   - ❌ Very small numbers (<1e-9)
   - ❌ Negative values i control charts
   - ❌ NA/NULL handling throughout

4. **Large Files**
   - ❌ >50,000 rows (limit)
   - ❌ >100 columns
   - ❌ Performance degradation

5. **Error Conditions**
   - ❌ Invalid chart type
   - ❌ Missing required columns
   - ❌ Corrupted CSV files
   - ❌ Network failures (if applicable)

---

## Recommended Test Cases to Add

### Critical Path Tests (Week 1-2)

```r
# tests/testthat/test-critical-path-data-load.R
test_that("Data upload → Plot generation critical path", {
  # Full integration test
  # Upload CSV → Autodetect → UI sync → Plot generation
  # Verify each step completes successfully
  # Target: <2 seconds for 1000 rows
})

# tests/testthat/test-critical-path-column-change.R
test_that("Manual column selection → Plot update", {
  # Change column dropdown → UI sync → Plot regeneration
  # Verify reactivity chain
})

# tests/testthat/test-critical-path-chart-type-change.R
test_that("Chart type change → QIC recalculation → Plot", {
  # run → i chart switch
  # Verify cache behavior
  # Verify control limits recalculated
})
```

---

### Integration Tests (Week 3-4)

```r
# tests/testthat/test-event-bus-integration.R
test_that("Event bus handles full reactive chain", {
  # Trigger top-level event
  # Trace through entire chain
  # Verify all observers fire in correct order
  # Verify final state correct
})

# tests/testthat/test-state-management-integration.R
test_that("State updates propagate correctly across modules", {
  # Update app_state in one module
  # Verify other modules react appropriately
  # Test hierarchical state access
})
```

---

### Performance Tests (Week 5)

```r
# tests/testthat/test-performance-benchmarks.R
test_that("Plot generation completes within 500ms for 1000 rows", {
  large_data <- generate_test_data(n = 1000)

  timing <- system.time({
    result <- generateSPCPlot(large_data, config, "run")
  })

  expect_lt(timing["elapsed"], 0.5) # <500ms
})

test_that("QIC cache provides >80% hit rate", {
  # Warm cache
  # Repeat plot generation
  # Measure cache hits
  cache_stats <- get_cache_statistics(app_state$cache$qic)
  expect_gt(cache_stats$hit_rate, 0.8)
})
```

---

## 6-Week Test Coverage Roadmap

### Week 1-2: Critical Paths (20-25 timer)
**Target: 60% coverage på critical paths**

1. Plot generation tests (8-12h)
   - Basic chart types (run, i, p, c, u)
   - Y-axis formatting (percent, count, rate)
   - Multi-part handling
   - Edge cases

2. Visualization module tests (10-12h)
   - Reactive chain testing
   - Cache update atomicity
   - Error handling
   - UI rendering

3. Event orchestration tests (8-10h)
   - Event sequence verification
   - Observer priority testing
   - Cleanup verification

---

### Week 3-4: Integration Tests (15-20 timer)
**Target: 70% overall coverage**

4. Auto-detection engine tests (6-8h)
   - Column detection heuristics
   - Danish locale handling
   - Frozen state management
   - Ambiguous data handling

5. Event bus integration (4-6h)
   - Full reactive chains
   - State propagation
   - Error propagation

6. State management integration (3-4h)
   - Hierarchical access
   - Module isolation
   - Concurrent updates

7. File operations tests (4-6h)
   - CSV parsing (various encodings)
   - Excel support
   - File validation
   - Error handling

---

### Week 5-6: Edge Cases + Refactoring (10-15 timer)
**Target: 90% overall coverage**

8. Edge case coverage (6-8h)
   - Empty/single row/column data
   - Danish characters comprehensive
   - Extreme values
   - Large files

9. Performance benchmarks (2-3h)
   - Plot generation timing
   - Cache hit rates
   - Memory usage

10. Existing test refactoring (4-6h)
    - Consolidate duplicates
    - Improve test structure
    - Add missing assertions
    - Mock external dependencies

---

## Test Quality Issues

### Current Test Suite Problems

1. **Insufficient Mocking**
   - Direct dependencies on external packages
   - No mock for qicharts2 calls
   - File I/O not mocked

2. **Lack of Fixtures**
   - Test data generated ad-hoc
   - No standardized test datasets
   - Difficult to reproduce failures

3. **Missing Assertions**
   - Some tests verify existence only
   - No validation of correctness
   - No performance assertions

4. **Test Organization**
   - Inconsistent naming (test-*, test_*)
   - No clear categorization
   - Duplicate test logic

---

## Recommendations

### Immediate Actions (Week 1)

1. ✅ Create tests for `fct_spc_plot_generation.R` (highest priority)
2. ✅ Create tests for `mod_spc_chart_server.R` (complex reactivity)
3. ✅ Create tests for `utils_server_event_listeners.R` (event chains)

### Short-term Actions (Week 2-4)

4. ✅ Add auto-detection engine tests
5. ✅ Add integration tests for reactive chains
6. ✅ Add file operations tests
7. ✅ Create standardized test fixtures

### Long-term Actions (Week 5-6)

8. ✅ Comprehensive edge case coverage
9. ✅ Performance benchmarking suite
10. ✅ Refactor existing tests for quality
11. ✅ CI/CD integration with coverage reporting

---

## Testing Infrastructure Improvements

### Recommended Tools

```r
# Install testing packages
install.packages(c(
  "covr",         # Coverage reporting
  "mockery",      # Mocking framework
  "withr",        # Temporary state management
  "shinytest2"    # Shiny app testing
))

# Coverage reporting
covr::package_coverage()

# Generate HTML report
covr::report()
```

### Test Fixtures

```r
# tests/testthat/fixtures/test-data.R
create_standard_test_data <- function(n = 100) {
  data.frame(
    dato = seq.Date(Sys.Date() - n + 1, Sys.Date(), by = "day"),
    værdi = rnorm(n, mean = 100, sd = 10),
    gruppe = sample(c("A", "B", "C"), n, replace = TRUE)
  )
}
```

---

## Conclusion

Current test coverage på **35-40%** er **signifikant under target** (≥90%). De mest kritiske komponenter - plot generation, visualization module, og event orchestration - har **zero eller minimal coverage**.

**Anbefalet approach:**
1. Focus først på critical paths (Week 1-2)
2. Build out integration tests (Week 3-4)
3. Fill edge case gaps (Week 5-6)
4. Kontinuerlig refactoring af existing tests

**Forventet outcome:** 90% coverage efter 6 uger (~45-60 timer total effort)

**Risk mitigation:** Start med highest risk areas først - plot generation og event orchestration.
