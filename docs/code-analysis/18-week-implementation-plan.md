# 18-Week Implementation Plan

**Projekt**: SPC App Code Modernization & Quality Improvement
**Dato**: 2025-10-09
**Version**: 0.0.3-dev
**Total Effort**: 128-167 timer (18 uger)

---

## Plan Oversigt

Denne plan guideer systematisk forbedring af SPC-applikationen gennem 5 faser over 18 uger. Hver uge inkluderer konkrete tasks, success criteria, og test requirements.

### Faser

1. **Quick Wins** (Uge 1-2): Kritiske fixes - 15-20 timer
2. **Performance** (Uge 3-4): Optimizering - 20-25 timer
3. **Test Coverage** (Uge 5-10): Kvalitetssikring - 45-60 timer
4. **Refactoring** (Uge 11-15): Maintainability - 36-44 timer
5. **Cleanup & Polish** (Uge 16-18): Code quality - 12-18 timer

---

## FASE 1: QUICK WINS (UGE 1-2)

**Fokus**: Eliminér højrisiko issues
**Total Effort**: 15-20 timer
**Mål**: Production-ready deployment

### UGE 1 (8-10 timer)

#### Task 1.1: Fix Observer Cleanup Memory Leak (KRITISK)
**File**: `/R/utils_server_event_listeners.R:1201-1240`
**Effort**: 2 timer
**Prioritet**: KRITISK

**Steps**:
1. Read current cleanup implementation (15min)
2. Implement explicit nullification + verification (45min)
3. Add failed_observers tracking (30min)
4. Write unit test for cleanup success (30min)

**Success Criteria**:
- [ ] All observers explicitly nullified after destroy
- [ ] Failed observers logged med details
- [ ] Test verifies 100% cleanup success
- [ ] No memory leaks ved repeated session starts

**Test**:
```r
# tests/testthat/test-observer-cleanup.R
test_that("Observer cleanup completes successfully", {
  # Setup mock observers
  # Trigger session end
  # Verify all destroyed + nullified
  # Check for failed_observers
})
```

**Dependencies**: None
**Branch**: `fix/observer-cleanup-memory-leak`

---

#### Task 1.2: Smart QIC Cache Invalidation (KRITISK)
**File**: `/R/utils_server_event_listeners.R:89-115`
**Effort**: 4 timer
**Prioritet**: KRITISK

**Steps**:
1. Analyze current cache clear strategy (30min)
2. Implement `invalidate_qic_cache_smart()` (1.5h)
3. Update `data_updated` observer (30min)
4. Add context-aware invalidation logic (1h)
5. Test cache hit rate improvement (30min)

**Implementation**:
```r
# R/utils_qic_cache_invalidation.R (NEW FILE)
invalidate_qic_cache_smart <- function(app_state, update_context) {
  context <- update_context$context

  # Clear entire cache only on structural changes
  if (context %in% c("upload", "column_added", "column_removed")) {
    app_state$cache$qic$clear()
    log_info("Full QIC cache clear", .context = "QIC_CACHE")
  } else if (context %in% c("table_cells_edited", "value_change")) {
    # Selective invalidation
    chart_type <- app_state$columns$mappings$chart_type
    app_state$cache$qic$invalidate_pattern(paste0("chart_", chart_type))
    log_info("Selective QIC cache invalidation", .context = "QIC_CACHE")
  }
  # Cosmetic changes (comments) don't clear cache
}
```

**Success Criteria**:
- [ ] Cache only cleared når nødvendigt
- [ ] Cosmetic changes preserverer cache
- [ ] Cache hit rate >80% efter implementation
- [ ] Performance improvement målbar (60-80% fewer recalcs)

**Test**:
```r
# tests/testthat/test-qic-cache-smart-invalidation.R
test_that("Cache preserved on cosmetic changes", {
  # Warm cache
  # Trigger comment update
  # Verify cache not cleared
})

test_that("Cache cleared on structural changes", {
  # Warm cache
  # Trigger column added
  # Verify full cache clear
})
```

**Dependencies**: None
**Branch**: `feat/smart-qic-cache-invalidation`

---

#### Task 1.3: CSV Formula Injection Protection (SIKKERHED)
**File**: Download handlers (find med grep)
**Effort**: 1 time
**Prioritet**: KRITISK

**Steps**:
1. Find all `downloadHandler` instances (10min)
2. Apply `sanitize_csv_output()` til hver (30min)
3. Test med malicious formulas (20min)

**Implementation**:
```r
# Find download handlers
grep -r "downloadHandler" R/

# For each handler, wrap data:
output$download_data <- downloadHandler(
  filename = function() paste0("spc_data_", Sys.Date(), ".csv"),
  content = function(file) {
    safe_data <- sanitize_csv_output(app_state$data$current_data)
    readr::write_csv2(safe_data, file, na = "")
  }
)
```

**Success Criteria**:
- [ ] All download handlers use `sanitize_csv_output()`
- [ ] Test data med `=SUM()` bliver escaped til `'=SUM()`
- [ ] Manual verification i Excel (opens safely)

**Test**:
```r
# tests/testthat/test-csv-sanitization.R
test_that("CSV download escapes formulas", {
  malicious_data <- data.frame(
    x = c("=SUM(A1:A10)", "@WEBSERVICE()", "-2+3"),
    y = c(1, 2, 3)
  )

  sanitized <- sanitize_csv_output(malicious_data)

  expect_true(all(grepl("^'=", sanitized$x[grepl("^=", malicious_data$x)])))
})
```

**Dependencies**: None
**Branch**: `fix/csv-formula-injection-protection`

---

### UGE 2 (7-10 timer)

#### Task 1.4: Session Token Logging Audit (SIKKERHED)
**Files**: Entire codebase
**Effort**: 2 timer
**Prioritet**: HØJ

**Steps**:
1. Grep for all `session$token` references (10min)
2. Verify each uses `sanitize_session_token()` (1h)
3. Fix inconsistencies (30min)
4. Update to SHA256 (20min)

**Command**:
```bash
# Find all session token references
grep -r "session\\\$token" R/ --exclude-dir=tests | grep -v "sanitize_session_token"
```

**Success Criteria**:
- [ ] Zero unsanitized `session$token` i logs
- [ ] All hashing uses SHA256 (upgrade fra SHA1)
- [ ] Consistent 8-character hash prefix

**Dependencies**: None
**Branch**: `fix/session-token-logging-consistency`

---

#### Task 1.5: Plot Generation Vectorization (PERFORMANCE)
**File**: `/R/fct_spc_plot_generation.R:813-831`
**Effort**: 4 timer
**Prioritet**: HØJ

**Steps**:
1. Analyze current for-loop part processing (30min)
2. Implement dplyr group_by + mutate solution (2h)
3. Benchmark performance improvement (30min)
4. Update tests for behavioral changes (1h)

**Implementation**:
```r
# BEFORE (O(n*p)):
for (p in unique(qic_data$part)) {
  part_rows <- which(qic_data$part == p)
  part_data <- qic_data[part_rows, ]
  # Process...
}

# AFTER (O(n)):
qic_data_with_signals <- qic_data |>
  dplyr::group_by(part) |>
  dplyr::mutate(
    part_n_cross = max(n.crossings, na.rm = TRUE),
    part_n_cross_min = max(n.crossings.min, na.rm = TRUE),
    crossings_signal = !is.na(part_n_cross) & !is.na(part_n_cross_min) &
                       part_n_cross < part_n_cross_min
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(anhoej.signal = runs.signal | crossings_signal)
```

**Success Criteria**:
- [ ] 40-60% speedup for multi-part charts (benchmark)
- [ ] Identical output (snapshot test)
- [ ] All existing tests pass

**Test**:
```r
# tests/testthat/test-plot-generation-performance.R
test_that("Vectorized part processing faster", {
  large_multipart <- generate_test_data(n = 1000, parts = 5)

  timing <- bench::mark(
    generateSPCPlot(large_multipart, config, "i"),
    iterations = 10
  )

  expect_lt(median(timing$time), as_bench_time("500ms"))
})
```

**Dependencies**: None
**Branch**: `feat/vectorize-plot-part-processing`

---

#### Task 1.6: Cache Key Optimization (PERFORMANCE)
**File**: `/R/fct_spc_plot_generation.R:693-735`
**Effort**: 2 timer
**Prioritet**: MEDIUM-HØJ

**Steps**:
1. Profile current digest performance (30min)
2. Implement structural-only hashing (1h)
3. Benchmark improvement (30min)

**Implementation**:
```r
# BEFORE: Full digest
x_content_hash <- digest::digest(x_column_data, algo = "xxhash32")

# AFTER: Structural attributes only
x_content_hash <- paste0(
  nrow(data), "_",
  ncol(data), "_",
  digest::digest(names(data), algo = "xxhash32"),
  "_",
  digest::digest(head(data[[config$x_col]], 100), algo = "xxhash32")
)
```

**Success Criteria**:
- [ ] 30-50% faster cache key generation
- [ ] 50-100ms saved per generateSPCPlot() call
- [ ] Cache hit rate maintained (>80%)

**Dependencies**: None
**Branch**: `feat/optimize-cache-key-generation`

---

### UGE 1-2 MILESTONES

**Week 1 Deliverables**:
- ✅ Memory leak fixed
- ✅ Smart cache invalidation implemented
- ✅ CSV injection protection applied

**Week 2 Deliverables**:
- ✅ Session token logging consistent
- ✅ Plot generation vectorized
- ✅ Cache keys optimized

**Testing**:
- [ ] All Quick Wins tests passing
- [ ] Manual smoke test af app
- [ ] Performance benchmark showing improvements

**Merge Strategy**:
- Create individual PRs for each task
- Review + merge før næste task
- Update `master` branch incrementally

---

## FASE 2: PERFORMANCE OPTIMIZATIONS (UGE 3-4)

**Fokus**: 30-50% samlet performance boost
**Total Effort**: 20-25 timer
**Mål**: Industry-leading performance

### UGE 3 (10-12 timer)

#### Task 2.1: Consolidate Column Observers (PERFORMANCE)
**File**: `/R/utils_server_event_listeners.R:730-771`
**Effort**: 4 timer
**Prioritet**: HØJ

**Steps**:
1. Analyze current 7 separate observers (30min)
2. Create `handle_column_input()` helper (1.5h)
3. Refactor til single parameterized observer (1.5h)
4. Test observer functionality (30min)

**Implementation**:
```r
# R/utils_server_column_input.R (NEW FILE)
handle_column_input <- function(col_name, new_value, app_state, emit) {
  # Shared logic for all column inputs
  # Token consumption
  # Normalization
  # State update
  # Cache invalidation
  # Emit column_choices_changed
}

# In event listeners:
create_column_observer <- function(col_name) {
  shiny::observeEvent(input[[col_name]], {
    handle_column_input(col_name, input[[col_name]], app_state, emit)
  }, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$MEDIUM)
}

observers <- purrr::map(columns_to_observe, create_column_observer)
```

**Success Criteria**:
- [ ] 7 observers → 1 parameterized observer
- [ ] 30-40% reduction i observer setup time
- [ ] Identical behavior (integration test)
- [ ] Easier to add new columns

**Test**:
```r
# tests/testthat/test-column-observer-consolidation.R
test_that("Consolidated observer handles all columns", {
  # Test each column type
  # Verify state updates
  # Verify events emitted
})
```

**Dependencies**: None
**Branch**: `refactor/consolidate-column-observers`

---

#### Task 2.2: Debounce Auto-Detection (PERFORMANCE)
**File**: `/R/utils_server_event_listeners.R` (auto-detect observer)
**Effort**: 2 timer
**Prioritet**: HØJ

**Steps**:
1. Identify auto-detection triggers (30min)
2. Wrap i shiny::debounce() (30min)
3. Test prevents redundant calls (1h)

**Implementation**:
```r
# In utils_server_event_listeners.R
debounced_autodetect <- shiny::debounce(
  reactive({ app_state$events$auto_detection_started }),
  millis = 300
)

observeEvent(debounced_autodetect(), {
  # Auto-detection logic...
}, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$AUTO_DETECT)
```

**Success Criteria**:
- [ ] Prevent 3-5 redundant autodetect calls during rapid updates
- [ ] User experience unchanged (300ms delay imperceptible)
- [ ] Integration test verifies debouncing

**Test**:
```r
# tests/testthat/test-autodetect-debouncing.R
test_that("Auto-detection debounced during rapid updates", {
  # Trigger 5 rapid data updates
  # Verify only 1-2 autodetect calls
})
```

**Dependencies**: None
**Branch**: `feat/debounce-auto-detection`

---

#### Task 2.3: Throttle UI Sync (PERFORMANCE)
**File**: `/R/utils_server_event_listeners.R` (UI sync observer)
**Effort**: 2 timer
**Prioritet**: MEDIUM

**Steps**:
1. Identify UI sync patterns (30min)
2. Implement shiny::throttle() (30min)
3. Test UI responsiveness (1h)

**Implementation**:
```r
throttled_ui_sync <- shiny::throttle(
  reactive({ app_state$events$ui_sync_requested }),
  millis = 250
)

observeEvent(throttled_ui_sync(), {
  # UI sync logic...
}, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$UI_SYNC)
```

**Success Criteria**:
- [ ] 20-30% reduction i UI update overhead
- [ ] No visual lag (<250ms imperceptible)
- [ ] All UI updates still occur

**Dependencies**: None
**Branch**: `feat/throttle-ui-sync`

---

#### Task 2.4: Pre-compute ggplot Layers (PERFORMANCE)
**File**: `/R/fct_spc_plot_generation.R:844-1248`
**Effort**: 4 timer
**Prioritet**: MEDIUM

**Steps**:
1. Analyze sequential layer additions (1h)
2. Build layer list før adding (2h)
3. Benchmark improvement (30min)
4. Visual regression test (30min)

**Implementation**:
```r
# BEFORE:
plot <- plot + layer1 + layer2 + layer3 + ...

# AFTER:
layers <- list()
if (has_control_limits) {
  layers <- c(layers, list(geom_ribbon(...)))
}
if (has_target) {
  layers <- c(layers, list(geom_hline(...)))
}
# ... build all layers ...

plot <- plot + layers  # Single addition
```

**Success Criteria**:
- [ ] 15-25% reduction i plot build time
- [ ] Identical visual output (snapshot test)
- [ ] Code more maintainable

**Dependencies**: None
**Branch**: `feat/precompute-ggplot-layers`

---

### UGE 4 (10-13 timer)

#### Task 2.5: Implement Reactive Batching (PERFORMANCE)
**File**: `/R/utils_server_event_listeners.R` (multiple locations)
**Effort**: 3 timer
**Prioritet**: MEDIUM

**Steps**:
1. Identify rapid-fire event patterns (1h)
2. Implement `schedule_batched_update()` (1.5h)
3. Test reactive storm prevention (30min)

**Implementation**:
```r
# R/utils_reactive_batching.R (NEW FILE)
schedule_batched_update <- function(update_fn, delay_ms = 50) {
  if (is.null(app_state$ui$pending_batch)) {
    app_state$ui$pending_batch <- TRUE
    later::later(function() {
      update_fn()
      app_state$ui$pending_batch <- NULL
    }, delay_ms / 1000)
  }
}

# Usage:
schedule_batched_update(function() {
  emit$column_choices_changed()
}, delay_ms = 50)
```

**Success Criteria**:
- [ ] 25-35% reduction i reactive storm overhead
- [ ] No UX degradation (<50ms delay)
- [ ] Integration test verifies batching

**Dependencies**: None
**Branch**: `feat/reactive-batching`

---

#### Task 2.6: Tidyverse High-Priority Items (MODERNIZATION)
**Files**: Multiple (se tidyverse rapport)
**Effort**: 4-6 timer
**Prioritet**: MEDIUM

**Scope** (choose 2-3 highest impact):
1. Month replacement loops → purrr (2h)
2. Path detection loop → purrr::detect (30min)
3. sapply label formatting → purrr::map + case_when (2h)

**Success Criteria**:
- [ ] 2-3 high-impact tidyverse conversions complete
- [ ] Code more readable
- [ ] All tests pass
- [ ] No performance degradation

**Dependencies**: None
**Branch**: `feat/tidyverse-high-priority`

---

### UGE 3-4 MILESTONES

**Week 3 Deliverables**:
- ✅ Column observers consolidated
- ✅ Auto-detection debounced
- ✅ UI sync throttled
- ✅ ggplot layers pre-computed

**Week 4 Deliverables**:
- ✅ Reactive batching implemented
- ✅ 2-3 tidyverse conversions complete

**Testing**:
- [ ] Performance benchmarks showing 30-50% improvement
- [ ] All existing tests pass
- [ ] Integration tests for new patterns

**Metrics**:
- [ ] Startup time still <100ms
- [ ] Plot generation <500ms for 1000 rows
- [ ] QIC cache hit rate >80%

---

## FASE 3: TEST COVERAGE SPRINT (UGE 5-10)

**Fokus**: 35-40% → 90% coverage
**Total Effort**: 45-60 timer (6 uger)
**Mål**: Industry-standard test coverage

### UGE 5-6: Critical Path Tests (20-25 timer)

#### Task 3.1: Plot Generation Test Suite
**File**: `tests/testthat/test-spc-plot-generation.R` (NEW)
**Effort**: 8-12 timer
**Prioritet**: KRITISK

**Coverage Areas**:
1. Basic chart types (run, i, p, c, u) - 3h
2. Y-axis formatting (percent, count, rate) - 2h
3. Multi-part handling - 2h
4. X-axis datetime formatting - 2h
5. Edge cases (empty, single row, extreme values) - 2h
6. Performance benchmarks - 1h

**Test Structure**:
```r
# tests/testthat/test-spc-plot-generation.R

describe("generateSPCPlot", {
  describe("Basic Chart Types", {
    it("generates run chart correctly", { ... })
    it("generates i chart with control limits", { ... })
    it("generates p chart with percentage y-axis", { ... })
  })

  describe("Y-Axis Formatting", {
    it("formats percent values correctly", { ... })
    it("applies K/M notation for large counts", { ... })
    it("handles rate calculations", { ... })
  })

  describe("Multi-Part Data", {
    it("creates separate control limits per part", { ... })
    it("handles part transitions correctly", { ... })
  })

  describe("Edge Cases", {
    it("handles empty data gracefully", { ... })
    it("handles single row data", { ... })
    it("handles extreme values", { ... })
  })

  describe("Performance", {
    it("completes in <500ms for 1000 rows", { ... })
  })
})
```

**Success Criteria**:
- [ ] >80% coverage af fct_spc_plot_generation.R
- [ ] All chart types tested
- [ ] Edge cases covered
- [ ] Performance baseline established

**Dependencies**: None
**Branch**: `test/plot-generation-coverage`

---

#### Task 3.2: Visualization Module Test Suite
**File**: `tests/testthat/test-mod-spc-chart-integration.R` (NEW)
**Effort**: 10-12 timer
**Prioritet**: KRITISK

**Coverage Areas**:
1. Reactive chain testing - 4h
2. Cache update atomicity - 3h
3. Error handling - 2h
4. UI rendering - 2h
5. Module state management - 2h

**Test Structure**:
```r
# tests/testthat/test-mod-spc-chart-integration.R

test_that("Module updates plot when data changes", {
  testServer(visualizationModuleServer, args = list(...), {
    session$setInputs(data_reactive = new_data)
    expect_true(!is.null(output$plot))
  })
})

test_that("Cache updates are atomic", {
  # Simulate concurrent updates
  # Verify no race conditions
  # Check cache_updating flag behavior
})

test_that("Error handling degrades gracefully", {
  # Inject plot generation error
  # Verify user sees appropriate message
  # Verify app doesn't crash
})
```

**Success Criteria**:
- [ ] >70% coverage af mod_spc_chart_server.R
- [ ] Reactive chains tested
- [ ] Race conditions prevented
- [ ] Error paths covered

**Dependencies**: None
**Branch**: `test/visualization-module-coverage`

---

#### Task 3.3: Event Orchestration Test Suite
**File**: `tests/testthat/test-event-listener-integration.R` (NEW)
**Effort**: 8-10 timer
**Prioritet**: KRITISK

**Coverage Areas**:
1. Event sequence verification - 3h
2. Observer priority testing - 2h
3. State propagation - 2h
4. Cleanup verification - 2h

**Test Structure**:
```r
test_that("data_updated event triggers cascade", {
  events_fired <- character(0)

  observeEvent(app_state$events$data_updated, {
    events_fired <- c(events_fired, "data_updated")
  })

  observeEvent(app_state$events$auto_detection_started, {
    events_fired <- c(events_fired, "auto_detection_started")
  })

  emit$data_updated("upload")

  expect_equal(events_fired, c("data_updated", "auto_detection_started"))
})
```

**Success Criteria**:
- [ ] >60% coverage af utils_server_event_listeners.R
- [ ] Event chains verified
- [ ] Observer priorities tested
- [ ] Cleanup verified

**Dependencies**: None
**Branch**: `test/event-orchestration-coverage`

---

### UGE 7-8: Integration Tests (15-20 timer)

#### Task 3.4: Auto-Detection Engine Tests
**File**: `tests/testthat/test-autodetect-engine.R` (NEW)
**Effort**: 6-8 timer
**Prioritet**: HØJ

**Coverage Areas**:
1. Column detection heuristics - 3h
2. Danish locale handling - 2h
3. Frozen state management - 1h
4. Ambiguous data handling - 1h

**Test Structure**:
```r
test_that("Detects date columns correctly", {
  data <- data.frame(
    dato = c("2023-01-01", "2023-01-02"),
    værdi = c(100, 105)
  )

  result <- autodetect_engine(data, app_state, "manual")

  expect_equal(result$x_col, "dato")
  expect_equal(result$y_col, "værdi")
})

test_that("Handles Danish month names", {
  data <- data.frame(
    måned = c("jan 2023", "feb 2023"),
    antal = c(50, 55)
  )

  result <- autodetect_engine(data, app_state, "manual")
  expect_true(!is.null(result$x_col))
})
```

**Success Criteria**:
- [ ] >70% coverage af autodetect files
- [ ] Danish locale tested
- [ ] Frozen state behavior verified
- [ ] Ambiguous data handled

**Dependencies**: None
**Branch**: `test/autodetect-engine-coverage`

---

#### Task 3.5: Event Bus Integration Tests
**File**: `tests/testthat/test-event-bus-full-chain.R` (NEW)
**Effort**: 4-6 timer
**Prioritet**: MEDIUM

**Coverage Areas**:
1. Full reactive chains - 2h
2. State propagation across modules - 2h
3. Error propagation - 1h

**Success Criteria**:
- [ ] End-to-end event chains tested
- [ ] Cross-module state updates verified
- [ ] Error handling comprehensive

**Dependencies**: Tasks 3.1-3.3 complete
**Branch**: `test/event-bus-integration`

---

#### Task 3.6: File Operations Tests
**File**: `tests/testthat/test-file-operations.R` (NEW)
**Effort**: 4-6 timer
**Prioritet**: MEDIUM

**Coverage Areas**:
1. CSV parsing (various encodings) - 2h
2. Excel support - 1h
3. File validation - 1h
4. Error handling - 1h

**Success Criteria**:
- [ ] >80% coverage af fct_file_operations.R
- [ ] All encodings tested
- [ ] Edge cases covered

**Dependencies**: None
**Branch**: `test/file-operations-coverage`

---

### UGE 9-10: Edge Cases + Refactoring (10-15 timer)

#### Task 3.7: Comprehensive Edge Case Coverage
**Files**: Multiple test files
**Effort**: 6-8 timer
**Prioritet**: MEDIUM

**Coverage Areas**:
1. Empty/single row/column data - 2h
2. Danish characters comprehensive - 2h
3. Extreme values (very large/small) - 2h
4. Large files (50K rows) - 1h

**Success Criteria**:
- [ ] All identified edge cases tested
- [ ] No crashes on edge cases
- [ ] Graceful degradation verified

**Dependencies**: Tasks 3.1-3.6 complete
**Branch**: `test/edge-case-coverage`

---

#### Task 3.8: Performance Benchmarking Suite
**File**: `tests/testthat/test-performance-benchmarks.R` (NEW)
**Effort**: 2-3 timer
**Prioritet**: MEDIUM

**Benchmarks**:
1. Plot generation timing - 1h
2. Cache hit rates - 30min
3. Memory usage - 30min

**Success Criteria**:
- [ ] Baseline performance metrics established
- [ ] Regression detection automated
- [ ] CI/CD integration ready

**Dependencies**: Tasks 3.1-3.7 complete
**Branch**: `test/performance-benchmarks`

---

#### Task 3.9: Existing Test Refactoring
**Files**: All existing test files
**Effort**: 4-6 timer
**Prioritet**: MEDIUM

**Refactoring Goals**:
1. Consolidate duplicates - 2h
2. Improve test structure (describe/it) - 2h
3. Add missing assertions - 1h
4. Mock external dependencies - 1h

**Success Criteria**:
- [ ] No duplicate test logic
- [ ] Consistent naming (test-*.R)
- [ ] All assertions meaningful
- [ ] External dependencies mocked

**Dependencies**: None
**Branch**: `test/refactor-existing-tests`

---

### UGE 5-10 MILESTONES

**Week 5-6 Deliverables**:
- ✅ Plot generation: >80% coverage
- ✅ Visualization module: >70% coverage
- ✅ Event orchestration: >60% coverage

**Week 7-8 Deliverables**:
- ✅ Auto-detection: >70% coverage
- ✅ Event bus integration tested
- ✅ File operations: >80% coverage

**Week 9-10 Deliverables**:
- ✅ Edge cases comprehensive
- ✅ Performance benchmarks established
- ✅ Existing tests refactored

**Overall Coverage Goal**: **≥90%** (from 35-40%)

**Testing Infrastructure**:
- [ ] covr package integration
- [ ] CI/CD coverage reporting
- [ ] Automated regression detection
- [ ] Test fixtures standardized

---

## FASE 4: REFACTORING (UGE 11-15)

**Fokus**: Code maintainability
**Total Effort**: 36-44 timer
**Mål**: Clean, modular, maintainable code

### UGE 11-12: Core Refactorings (14-17 timer)

#### Task 4.1: Extract generateSPCPlot Functions
**File**: `/R/fct_spc_plot_generation.R`
**Effort**: 6-8 timer
**Prioritet**: KRITISK

**Extraction Plan**:
1. Create `R/fct_spc_plot_axis_formatting.R` (2h)
   - `format_x_axis_datetime()`
   - `format_y_axis_by_unit()`
   - `calculate_date_interval_breaks()`

2. Create `R/fct_spc_plot_data_processing.R` (2h)
   - `process_chart_data()`
   - `validate_and_sanitize_inputs()`
   - `generate_qic_data()`

3. Create `R/fct_spc_plot_building.R` (1.5h)
   - `build_base_plot()`
   - `add_control_limits()`
   - `add_target_line()`

4. Update `generateSPCPlot()` til orchestration (30min)

**Success Criteria**:
- [ ] Main function <100 lines
- [ ] Each extracted function <50 lines
- [ ] All tests pass
- [ ] No behavioral changes

**Dependencies**: Task 3.1 complete (plot tests)
**Branch**: `refactor/extract-plot-generation-functions`

---

#### Task 4.2: Consolidate Y-Axis Formatting
**Files**: Multiple
**Effort**: 3-4 timer
**Prioritet**: KRITISK

**Steps**:
1. Create `R/utils_y_axis_formatting.R` (2h)
2. Migrate all formatting logic (1.5h)
3. Update call sites (30min)

**Implementation**:
```r
# R/utils_y_axis_formatting.R (NEW)
format_y_axis_value <- function(value, unit, y_range = NULL)
format_count_with_scaling <- function(value)
format_percent_value <- function(value)
format_rate_value <- function(value)
format_time_value <- function(value, y_range)
```

**Success Criteria**:
- [ ] Single source of truth for Y-axis formatting
- [ ] ~200 lines of duplicate code removed
- [ ] All tests pass
- [ ] Visual output identical

**Dependencies**: Task 3.1 complete
**Branch**: `refactor/consolidate-y-axis-formatting`

---

#### Task 4.3: Magic Numbers to Config
**Files**: Multiple
**Effort**: 2-3 timer
**Prioritet**: KRITISK

**Steps**:
1. Create `R/config_plot_dimensions.R` (1.5h)
2. Move all magic numbers til PLOT_DIMENSIONS (1h)
3. Update references (30min)

**Implementation**:
```r
# R/config_plot_dimensions.R (NEW)
PLOT_DIMENSIONS <- list(
  ucl_linewidth_base = 2.5,
  target_linewidth_base = 1.0,
  device_height_baseline_inches = 7.8,
  x_axis_extension_factor = 0.20,
  label_inset_margin_factor = 0.01
  # ... etc
)
```

**Success Criteria**:
- [ ] All magic numbers centralized
- [ ] Self-documenting constants
- [ ] Easier tuning
- [ ] Tests verify consistency

**Dependencies**: None
**Branch**: `refactor/magic-numbers-to-config`

---

### UGE 13-14: Advanced Refactorings (12-14 timer)

#### Task 4.4: Event Handler Refactoring
**File**: `/R/utils_server_event_listeners.R`
**Effort**: 4-5 timer
**Prioritet**: IMPORTANT

**Steps**:
1. Create `R/utils_event_context_handlers.R` (2h)
2. Implement strategy pattern (2h)
3. Update event listeners (1h)

**Implementation**:
```r
# R/utils_event_context_handlers.R (NEW)
handle_data_update_by_context <- function(update_context, ...)
classify_update_context <- function(update_context)
handle_load_context <- function(...)
handle_table_edit_context <- function(...)
handle_data_change_context <- function(...)
```

**Success Criteria**:
- [ ] Cyclomatic complexity reduced (12 → ~3 per function)
- [ ] Easier to add new context types
- [ ] Better testability
- [ ] All tests pass

**Dependencies**: Task 3.3 complete (event tests)
**Branch**: `refactor/event-handler-strategy-pattern`

---

#### Task 4.5: State Accessors
**Files**: Create new + update existing
**Effort**: 5-6 timer
**Prioritet**: IMPORTANT

**Steps**:
1. Create `R/utils_state_accessors.R` (3h)
2. Implement getter/setter functions (2h)
3. Update call sites (1h)

**Implementation**:
```r
# R/utils_state_accessors.R (NEW)
get_autodetect_status <- function(app_state)
set_autodetect_in_progress <- function(app_state, value)
get_column_mappings <- function(app_state)
update_column_mapping <- function(app_state, column, value)
# ... etc
```

**Success Criteria**:
- [ ] Consistent isolate() usage
- [ ] Encapsulation of state structure
- [ ] Type safety through validation
- [ ] Easier to refactor state schema

**Dependencies**: None
**Branch**: `refactor/state-accessor-functions`

---

### UGE 15: Parameter Objects (4-5 timer)

#### Task 4.6: Parameter Objects Implementation
**Files**: Multiple function signatures
**Effort**: 4-5 timer
**Prioritet**: IMPORTANT

**Steps**:
1. Create `R/plot_config_objects.R` (2h)
2. Refactor `generateSPCPlot()` signature (1.5h)
3. Update call sites (1h)

**Implementation**:
```r
# R/plot_config_objects.R (NEW)
spc_plot_config <- function(...)
viewport_dims <- function(...)
phase_config <- function(...)

# Refactored signature:
generateSPCPlot <- function(data, config, plot_config, viewport, phases, ...)
```

**Success Criteria**:
- [ ] Function signatures <5 parameters
- [ ] Logical parameter grouping
- [ ] Easier to extend
- [ ] Backward compatible wrappers

**Dependencies**: Task 4.1 complete
**Branch**: `refactor/parameter-objects`

---

### UGE 11-15 MILESTONES

**Week 11-12 Deliverables**:
- ✅ generateSPCPlot extracted til multiple files
- ✅ Y-axis formatting consolidated
- ✅ Magic numbers centralized

**Week 13-14 Deliverables**:
- ✅ Event handlers refactored
- ✅ State accessors implemented

**Week 15 Deliverables**:
- ✅ Parameter objects implemented

**Code Quality Metrics**:
- [ ] Average function length <50 lines
- [ ] Cyclomatic complexity <10
- [ ] No code duplication >5 lines
- [ ] All CRITICAL refactorings complete

---

## FASE 5: CLEANUP & POLISH (UGE 16-18)

**Fokus**: Final quality improvements
**Total Effort**: 12-18 timer
**Mål**: Production-grade polish

### UGE 16-17: Legacy Code Cleanup (8-16 timer)

#### Task 5.1: Remove Commented Code
**Files**: Multiple
**Effort**: 1 time
**Prioritet**: HØJ

**Steps**:
1. Delete commented debug code (utils_label_placement.R:89-90)
2. Resolve commented theme code (fct_spc_plot_generation.R:1381,1389)
3. Remove legacy comments (app_server_main.R:159)

**Success Criteria**:
- [ ] Zero commented-out code i production
- [ ] Git history preserved for reference

**Dependencies**: None
**Branch**: `cleanup/remove-commented-code`

---

#### Task 5.2: Archive Migration Documentation
**Files**: `/dev/` directory
**Effort**: 2-4 timer
**Prioritet**: MEDIUM

**Steps**:
1. Create `/dev/archive/` (10min)
2. Move completed migration docs (30min)
3. Update README files (1h)
4. Clean up DEVELOPER_GUIDE.md (1-2h)

**Success Criteria**:
- [ ] Only current docs i `/dev/`
- [ ] Historical docs i `/dev/archive/`
- [ ] Clear navigation for new developers

**Dependencies**: None
**Branch**: `docs/archive-migration-docs`

---

#### Task 5.3: Audit Runtime Config Dual Format
**File**: `/R/app_runtime_config.R`
**Effort**: 4-8 timer
**Prioritet**: MEDIUM

**Steps**:
1. Find all call sites af `convert_profile_to_legacy_config()` (1h)
2. Create migration plan (2h)
3. Implement new format throughout (2-4h)
4. Remove conversion function (1h)

**Success Criteria**:
- [ ] Single configuration format
- [ ] Legacy conversion removed
- [ ] All tests pass

**Dependencies**: None
**Branch**: `refactor/remove-legacy-config-conversion`

---

### UGE 18: Security & Final Polish (4-6 timer)

#### Task 5.4: Security Enhancements
**Files**: Multiple
**Effort**: 3-4 timer
**Prioritet**: MEDIUM

**Enhancements**:
1. MIME type validation enhancement (1.5h)
2. Path traversal hardening (30min)
3. Input validation på UI (1h)
4. Context-aware escaping (30min)

**Success Criteria**:
- [ ] All MEDIUM priority security items addressed
- [ ] Security tests pass

**Dependencies**: None
**Branch**: `security/medium-priority-enhancements`

---

#### Task 5.5: Documentation Updates
**Files**: CLAUDE.md, README, etc.
**Effort**: 2-3 timer
**Prioritet**: MEDIUM

**Updates**:
1. Update CLAUDE.md med lessons learned (1h)
2. Update README med new features (30min)
3. Create CHANGELOG entry (30min)
4. Update configuration docs (1h)

**Success Criteria**:
- [ ] Documentation reflects current state
- [ ] No outdated information
- [ ] New developers can onboard easily

**Dependencies**: All other tasks complete
**Branch**: `docs/update-project-documentation`

---

### UGE 16-18 MILESTONES

**Week 16-17 Deliverables**:
- ✅ Commented code removed
- ✅ Migration docs archived
- ✅ Legacy config conversion removed

**Week 18 Deliverables**:
- ✅ Security enhancements complete
- ✅ Documentation updated

**Final Checklist**:
- [ ] All tests passing (≥90% coverage)
- [ ] No commented code
- [ ] No legacy patterns
- [ ] Documentation current
- [ ] Security hardened
- [ ] Performance optimized
- [ ] Ready for v1.0.0 release

---

## TRACKING & REPORTING

### Weekly Progress Reports

**Template**:
```markdown
## Week [X] Progress Report

**Completed Tasks**:
- [Task ID]: [Description] ✅
- [Task ID]: [Description] ✅

**In Progress**:
- [Task ID]: [Description] (60% complete)

**Blockers**:
- [Description of blocker]

**Metrics**:
- Test coverage: [X]%
- Performance: [metric]
- Code quality: [metric]

**Next Week Plan**:
- [Task ID]: [Description]
- [Task ID]: [Description]
```

---

### Branch Strategy

**Pattern**: `[type]/[short-description]`

**Types**:
- `fix/` - Bug fixes
- `feat/` - New features
- `refactor/` - Code improvements
- `test/` - Test additions
- `docs/` - Documentation
- `security/` - Security improvements
- `cleanup/` - Code cleanup

**Example**: `fix/observer-cleanup-memory-leak`

---

### Merge Strategy

1. **Feature Branch** → Create fra `master`
2. **Development** → Commit to feature branch
3. **Testing** → All tests pass locally
4. **Pull Request** → Review required
5. **CI/CD** → Automated checks
6. **Merge** → Squash + merge to `master`
7. **Deployment** → Production after each phase

---

### Success Metrics

**Phase 1 (Quick Wins)**:
- [ ] Zero KRITISK issues
- [ ] Production deployment successful

**Phase 2 (Performance)**:
- [ ] 30-50% performance improvement
- [ ] Startup time <100ms maintained
- [ ] Plot generation <500ms for 1K rows

**Phase 3 (Test Coverage)**:
- [ ] ≥90% overall coverage
- [ ] 100% på critical paths
- [ ] Zero flaky tests

**Phase 4 (Refactoring)**:
- [ ] Average function <50 lines
- [ ] Cyclomatic complexity <10
- [ ] Zero code duplication >5 lines

**Phase 5 (Cleanup)**:
- [ ] Zero commented code
- [ ] Zero legacy patterns
- [ ] Documentation 100% current

---

## KONKLUSION

Denne 18-week plan giver en systematisk approach til at bringe SPC-applikationen fra **B+ (82/100)** til **industry-leading quality (95+/100)**.

**Key Success Factors**:
1. ✅ Incremental implementation (one task at a time)
2. ✅ Test-driven approach (tests først)
3. ✅ Continuous integration (merge ofte)
4. ✅ Regular reviews (weekly progress reports)
5. ✅ Measurable metrics (coverage, performance, quality)

**Final Goal**: Production-grade SPC applikation med:
- ✅ 90%+ test coverage
- ✅ Industry-leading performance
- ✅ Secure by design
- ✅ Maintainable codebase
- ✅ Comprehensive documentation

**Estimated Completion**: Week 18 (cirka 4-5 måneder fra start)

---

**Next Step**: Review plan med team → Begin Week 1 tasks → Track progress weekly
