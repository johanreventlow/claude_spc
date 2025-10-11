# Major Visualization Refactor - Implementation Plan

**Dato**: 2025-10-11
**Status**: PLANNING - Afventer test coverage ≥90%
**Estimeret Effort**: 15-20 timer
**Afhængigheder**: Test coverage forbedring, M4-M6 tidyverse migration

**Reference**: REMEDIATION_MASTER_PLAN.md opgaver M12-M15

---

## Executive Summary

Denne plan beskriver den største refaktorering af visualiseringsmodulet siden projektets start. Målet er at opdele `visualizationModuleServer()` (1237 linjer) i testbare, modulære komponenter der følger Single Responsibility Principle og TDD-principper.

**Hvorfor nu?**
- Modulet er kritisk sti (100% af plot generation)
- Høj complexity gør debugging besværligt
- Test coverage gaps identificeret i visualizationsmodulet
- Fundament er stabilt efter Fase 1-4 remediation

**Hvorfor ikke før?**
- Kræver høj test coverage først (≥90% target)
- Tidyverse migration (M4-M6) bør være completed
- Performance baseline skal være dokumenteret

**Risiko**: HØJ - Enhver regression i plot generation er blocker for produktion

---

## 📋 Prerequisites (KRITISK)

### Før Major Refactor kan starte:

#### 1. Test Coverage ≥90% (Current: ~60-70%)

**Manglende coverage identified i test-coverage.md:**

```
✅ Skal være completed:
- [ ] tests/testthat/test-mod-spc-chart-integration.R: Real assertions (not stubs)
- [ ] tests/testthat/test-visualization-module.R: testServer() with output checks
- [ ] tests/testthat/test-anhoej-results.R: Edge cases (cache hit vs. fresh)
- [ ] tests/performance/test-qic-caching-benchmark.R: Performance baseline
```

**Action**: Se sektion "Test Coverage Improvements" nederst i dette dokument.

#### 2. Performance Baseline Dokumenteret

```bash
# SKAL køres før refactor starter:
R -e "source('tests/performance/test-qic-caching-benchmark.R')"
```

**Baseline metrics at dokumentere:**
- `generateSPCPlot()` median execution time
- Cache hit rate (%)
- Memory usage (MB) for typical workflow
- Reactive chain execution count

**Acceptance**: <5% regression i alle metrics efter refactor

#### 3. Tidyverse Migration (M4-M6) Completed (OPTIONAL)

Kan udskyde, men gør refactor nemmere hvis completed først.

**Rationale**: Hvis vi refactorer først og så migrerer til tidyverse, risikerer vi dobbelt arbejde.

---

## 🎯 Scope & Goals

### In-Scope

| # | Opgave | Beskrivelse | Effort |
|---|--------|-------------|--------|
| **M12** | Refactor visualization module (split 750 linjer) | Opdel i ≤5 helpers | 6-8h |
| **M13** | Split navigation/test-mode event registration | Separate concerns | 3h |
| **M14** | Consolidate Anhøj result treatment | Fælles helper | 2h |
| **M15** | Modularize `setup_visualization()` | Helper extraction | 3h |

**Total**: 14-16 timer (eksklusiv testing)

### Out-of-Scope

- ❌ UI ændringer (kun server-side refactor)
- ❌ Nye features (ren refactor)
- ❌ Cache strategy changes (bevar eksisterende)
- ❌ Event-bus ændringer (allerede optimeret i Fase 1-3)

### Success Criteria

✅ **Functionality**:
- Zero breaking changes (alle eksisterende tests bestået)
- Manual smoke test passed (upload → plot → download)
- Visual regression test passed (shinytest2 snapshots)

✅ **Code Quality**:
- `visualizationModuleServer()` ≤150 linjer (tynd orkestrator)
- Hver helper ≤100 linjer
- Cyclomatic complexity ≤10 per function
- 100% test coverage på nye helpers

✅ **Performance**:
- <5% regression i plot generation time
- <10% regression i memory usage
- Cache hit rate uændret

---

## 📐 Architectural Design

### Current State (mod_spc_chart_server.R)

```
visualizationModuleServer() [1237 linjer]
├── Module initialization (ns, viewport setup)
├── Helper functions (safe_max inline)
├── State management (app_state integration)
├── Data reactive pipeline (84 linjer)
│   ├── get_module_data()
│   ├── Event observers (visualization_update_needed)
│   └── module_data_reactive()
├── Chart configuration (46 linjer)
│   ├── chart_config_raw reactive
│   └── Debounced chart_config
├── Plot generation pipeline (524 linjer)
│   ├── data_ready()
│   ├── spc_inputs_raw() [96 linjer]
│   ├── spc_results() [232 linjer] ← BIGGEST COMPLEXITY
│   ├── spc_plot() [22 linjer]
│   └── Cache observers [46 linjer]
├── UI output rendering (67 linjer)
│   ├── output$spc_plot_actual
│   ├── output$plot_info
│   └── output$plot_ready
├── Value boxes (494 linjer!)
│   ├── output$plot_status_boxes
│   ├── output$data_summary_box
│   └── output$anhoej_rules_boxes [294 linjer] ← SECOND BIGGEST
└── Return values
```

**Problem hotspots:**
1. `spc_results()` reactive: 232 linjer med mixed concerns
2. `output$anhoej_rules_boxes`: 294 linjer UI logic
3. Inline helper functions (ikke testable isoleret)
4. Mixed data/UI concerns (violation of SRP)

### Target State (Efter Refactor)

```
visualizationModuleServer() [≤150 linjer - tynd orkestrator]
├── Module initialization
├── Data pipeline (via helpers)
│   ├── initialize_visualization_cache(app_state, emit)
│   └── build_module_data_reactive(app_state)
├── Chart configuration (via helpers)
│   └── build_chart_config_pipeline(app_state, input)
├── Plot generation (via helpers)
│   ├── build_spc_inputs_reactive(data, config, ..., viewport_provider)
│   ├── compute_spc_results(app_state, inputs, qic_cache)
│   └── update_anhoej_results_unified(current, new, centerline_changed, qic_data, show_phases)
├── UI rendering (via helpers)
│   ├── render_visualization_outputs(results, session, ns)
│   └── render_anhoej_value_boxes(anhoej, chart_type, data, config)
└── Return values

NEW FILES:
R/utils_visualization_pipeline.R [~300 linjer]
├── initialize_visualization_cache()
├── build_module_data_reactive()
├── build_chart_config_pipeline()
└── build_spc_inputs_reactive()

R/utils_visualization_computation.R [~200 linjer]
├── compute_spc_results()
├── validate_and_prep_data()
└── derive_qic_results()

R/utils_visualization_ui.R [~350 linjer]
├── render_visualization_outputs()
├── render_anhoej_value_boxes()
├── build_anhoej_status_info()
└── format_anhoej_box_value()

R/utils_anhoej_results.R [existing - opdateres]
├── update_anhoej_results() [eksisterende]
└── update_anhoej_results_unified() [ny - konsolideret version]
```

**Benefits:**
- ✅ Testable units (hver helper kan unit-testes)
- ✅ Clear separation of concerns (data/computation/UI)
- ✅ Reusable components (fx render_anhoej_value_boxes kan genbruges)
- ✅ Easier debugging (mindre funktioner)

---

## 🔄 Implementation Phases

### **FASE 1: Test Infrastructure** (3-4 timer)

**Mål**: Etabler solid test-base INDEN refactor

#### 1.1 Visualization Module Integration Tests

**Fil**: `tests/testthat/test-mod-spc-chart-integration.R`

**Opgave**: Replace placeholder testServer() blocks med real assertions

```r
test_that("visualization module renders plot with valid data", {
  testServer(visualizationModuleServer, args = list(
    data_reactive = reactive(test_data),
    column_config_reactive = reactive(test_config),
    chart_type_reactive = reactive("run"),
    # ... andre args
    app_state = create_app_state()
  ), {
    # Assert: module_data_reactive() returnerer data
    expect_equal(nrow(session$returned$module_data_reactive()), 30)

    # Assert: plot_ready bliver TRUE
    session$flushReact()
    expect_true(output$plot_ready)

    # Assert: anhoej_results indeholder gyldige metrics
    anhoej <- session$returned$anhoej_results()
    expect_false(is.na(anhoej$longest_run))
  })
})
```

**Coverage target**: 80%+ af visualizationModuleServer logic

#### 1.2 Anhøj Results Consolidation Tests

**Fil**: `tests/testthat/test-anhoej-results.R`

```r
test_that("update_anhoej_results preserves values on cache hit", {
  # Setup: Previous valid results
  previous <- list(longest_run = 5, n_crossings = 8, has_valid_data = TRUE)

  # New results with NA (cache hit scenario)
  new <- list(longest_run = NA_real_, n_crossings = NA_real_)

  # Execute
  result <- update_anhoej_results(previous, new, centerline_changed = FALSE,
                                   qic_data = mock_qic_data, show_phases = FALSE)

  # Assert: Old values preserved
  expect_equal(result$longest_run, 5)
  expect_equal(result$n_crossings, 8)
})
```

#### 1.3 Performance Baseline

```bash
# Kør INDEN refactor
R -e "source('tests/performance/test-qic-caching-benchmark.R')" > baseline_pre_refactor.txt

# Gem baseline
git add baseline_pre_refactor.txt
git commit -m "test(performance): baseline before M12-M15 refactor"
```

**Success**: Alle nye tests bestået, performance baseline dokumenteret

---

### **FASE 2: Extract Helpers (Data Pipeline)** (2-3 timer)

**Branch**: `refactor/visualization-data-pipeline`

#### 2.1 Opret `R/utils_visualization_pipeline.R`

**Ekstrahér funktioner:**

```r
#' Initialize Visualization Cache
#'
#' Sets up event-driven cache updates for visualization module.
#'
#' @param app_state Centralized app state
#' @param emit Emit API for event triggers
#'
#' @return NULL (sideeffect: registers observer)
#' @keywords internal
initialize_visualization_cache <- function(app_state, emit) {
  # Move lines 89-168 fra mod_spc_chart_server.R
  # Observer setup for visualization_update_needed event
}

#' Build Module Data Reactive
#'
#' Creates reactive for module-scoped data with filtering and caching.
#'
#' @param app_state Centralized app state
#'
#' @return Reactive expression returning filtered data
#' @keywords internal
build_module_data_reactive <- function(app_state) {
  # Move get_module_data() + module_data_reactive() logic
}

#' Build Chart Config Pipeline
#'
#' Creates debounced reactive for chart configuration with validation.
#'
#' @param app_state Centralized app state
#' @param input Shiny input object
#' @param session Shiny session object
#'
#' @return List with chart_config_raw and chart_config (debounced)
#' @keywords internal
build_chart_config_pipeline <- function(app_state, input, session) {
  # Move lines 208-253 fra mod_spc_chart_server.R
}
```

#### 2.2 Opdater `visualizationModuleServer()`

**Replace inline logic med helper calls:**

```r
# BEFORE (inline):
current_cached_data <- isolate(app_state$visualization$module_cached_data)
if (is.null(current_cached_data)) {
  app_state$visualization$module_cached_data <- NULL
}
# ... 150+ linjer

# AFTER (helper call):
initialize_visualization_cache(app_state, create_emit_api(app_state))
```

#### 2.3 Tests

**Fil**: `tests/testthat/test-utils-visualization-pipeline.R` (ny)

```r
test_that("initialize_visualization_cache registers observer", {
  app_state <- create_app_state()
  emit <- create_emit_api(app_state)

  # Execute
  initialize_visualization_cache(app_state, emit)

  # Trigger event
  emit$visualization_update_needed()

  # Assert: cache updated
  expect_false(is.null(app_state$visualization$module_data_cache))
})
```

**Acceptance**: Alle tests bestået, visualizationModuleServer reduceret med ~150 linjer

---

### **FASE 3: Extract Helpers (Computation)** (3-4 timer)

**Branch**: `refactor/visualization-computation`

#### 3.1 Opret `R/utils_visualization_computation.R`

**Ekstrahér `spc_results()` reactive logic:**

```r
#' Compute SPC Results
#'
#' Core computation logic for SPC plot generation with caching.
#' Handles validation, QIC computation, Anhøj results, and error recovery.
#'
#' @param app_state Centralized app state
#' @param inputs List from build_spc_inputs_reactive()
#' @param qic_cache QIC cache object (from app_state)
#'
#' @return List with plot, qic_data, cache_key
#' @keywords internal
compute_spc_results <- function(app_state, inputs, qic_cache = NULL) {
  # Move lines 366-598 fra mod_spc_chart_server.R
  # Inkluderer:
  # - Validation (validateDataForChart)
  # - generateSPCPlot() call
  # - applyHospitalTheme()
  # - derive_qic_results() call
  # - update_anhoej_results() call
  # - Error handling via safe_operation()
}

#' Derive QIC Results from QIC Data
#'
#' Extracts Anhøj metrics from qicharts2 output.
#' Shared logic for both fresh computation and cache hits.
#'
#' @param qic_data QIC data frame from qicharts2::qic()
#' @param chart_type Chart type ("run", "p", "u", etc.)
#'
#' @return List with Anhøj metrics
#' @keywords internal
derive_qic_results <- function(qic_data, chart_type) {
  # Konsolideret logik fra lines 494-509 og 643-653
  # Eliminerer duplication mellem spc_results() og cache observer
}
```

#### 3.2 Opdater `R/utils_anhoej_results.R`

**Tilføj konsolideret version:**

```r
#' Update Anhøj Results (Unified)
#'
#' Unified logic for updating Anhøj results.
#' Consolidates treatment between fresh computation and cache hits.
#'
#' M14: Replaces duplicate logic in mod_spc_chart_server.R
#'
#' @param current Current anhoej_results from app_state
#' @param qic_results New results from derive_qic_results()
#' @param centerline_changed Boolean - did centerline change?
#' @param qic_data Raw QIC data (for validation)
#' @param show_phases Boolean - are phases active?
#'
#' @return Updated anhoej_results list
#' @export
update_anhoej_results_unified <- function(current, qic_results, centerline_changed,
                                         qic_data = NULL, show_phases = FALSE) {
  # Konsolideret logik der håndterer BÅDE:
  # 1. Fresh computation (lines 514-530)
  # 2. Cache hit observer (lines 662-677)
}
```

#### 3.3 Tests

**Fil**: `tests/testthat/test-utils-visualization-computation.R` (ny)

```r
test_that("compute_spc_results handles validation errors gracefully", {
  app_state <- create_app_state()
  inputs <- list(
    data = data.frame(x = 1:5, y = rep(NA, 5)), # Invalid data
    config = list(x_col = "x", y_col = "y"),
    chart_type = "run"
  )

  result <- compute_spc_results(app_state, inputs)

  expect_null(result$plot)
  expect_false(app_state$visualization$plot_ready)
  expect_length(app_state$visualization$plot_warnings, 1)
})

test_that("derive_qic_results extracts metrics consistently", {
  qic_data <- mock_qic_data(longest_run = 5, n_crossings = 8)

  result <- derive_qic_results(qic_data, "run")

  expect_equal(result$longest_run, 5)
  expect_equal(result$n_crossings, 8)
  expect_true(result$runs_signal)
})
```

**Acceptance**: Computation logic isolated, duplicate Anhøj logic eliminated

---

### **FASE 4: Extract Helpers (UI Rendering)** (2-3 timer)

**Branch**: `refactor/visualization-ui`

#### 4.1 Opret `R/utils_visualization_ui.R`

**Ekstrahér value box rendering:**

```r
#' Render Anhøj Value Boxes
#'
#' Generates the three Anhøj rule value boxes (serielængde, kryds, kontrolgrænser).
#' Massive 294-line UI logic extracted for testability.
#'
#' M12: Extracted from mod_spc_chart_server.R:895-1188
#'
#' @param anhoej Anhøj results from app_state
#' @param chart_type Current chart type
#' @param data Module data
#' @param config Chart config
#' @param is_computing Boolean - is plot computing?
#' @param plot_ready Boolean - is plot ready?
#'
#' @return shiny::tagList with three value boxes
#' @keywords internal
render_anhoej_value_boxes <- function(anhoej, chart_type, data, config,
                                      is_computing = FALSE, plot_ready = FALSE) {
  # Move lines 895-1188 fra mod_spc_chart_server.R
  # Includes:
  # - Status determination logic
  # - Serielængde box
  # - Antal kryds box
  # - Kontrolgrænser box
}

#' Build Anhøj Status Info
#'
#' Determines current status for Anhøj value boxes.
#'
#' @param data Module data
#' @param config Chart config
#' @param is_computing Boolean
#' @param plot_ready Boolean
#'
#' @return List with status, message, theme
#' @keywords internal
build_anhoej_status_info <- function(data, config, is_computing, plot_ready) {
  # Extract status determination logic (lines 933-977)
}
```

#### 4.2 Tests

**Fil**: `tests/testthat/test-utils-visualization-ui.R` (ny)

```r
test_that("render_anhoej_value_boxes shows correct status for no data", {
  result <- render_anhoej_value_boxes(
    anhoej = NULL,
    chart_type = "run",
    data = NULL,
    config = NULL,
    is_computing = FALSE,
    plot_ready = FALSE
  )

  expect_s3_class(result, "shiny.tag.list")
  expect_length(result, 3) # Three value boxes

  # Check that boxes show "Ingen data" state
  html <- as.character(result)
  expect_match(html, "Ingen data")
})

test_that("build_anhoej_status_info detects insufficient data", {
  data <- data.frame(x = 1:5, y = 1:5) # Only 5 points
  config <- list(x_col = "x", y_col = "y")

  status <- build_anhoej_status_info(data, config, FALSE, FALSE)

  expect_equal(status$status, "insufficient_data")
  expect_match(status$message, "mindst 10")
})
```

**Acceptance**: UI logic isolated, value boxes testable outside module context

---

### **FASE 5: Event Registration Refactor (M13)** (2-3 timer)

**Branch**: `refactor/event-registration-split`

#### 5.1 Opdel `R/utils_server_event_listeners.R`

**Current state:**
- `register_navigation_events()`: 182 linjer mixed concerns

**Target state:**

```r
# Keep existing function as orchestrator:
register_navigation_events <- function(...) {
  c(
    register_session_lifecycle_events(...),
    register_test_mode_events(...),
    register_error_events(...)
  )
}

# NEW: Session lifecycle (session start/end, cache cleanup)
register_session_lifecycle_events <- function(app_state, input, output, session, emit) {
  list(
    session_reset = observeEvent(...),
    session_ended = observeEvent(...)
  )
}

# NEW: Test mode events
register_test_mode_events <- function(app_state, input, session) {
  list(
    test_mode_ready = observeEvent(...),
    test_mode_trigger = observeEvent(...)
  )
}

# NEW: Error events
register_error_events <- function(session) {
  list(
    shiny_error = observeEvent(...)
  )
}
```

#### 5.2 Remove `exists()` Guards

**Before:**
```r
if (exists("track_event", mode = "function")) {
  track_event("session_reset")
}
```

**After (dependency injection):**
```r
register_error_events <- function(session, track_event = NULL) {
  if (!is.null(track_event) && is.function(track_event)) {
    track_event("session_reset")
  }
}
```

#### 5.3 Tests

**Fil**: `tests/testthat/test-event-registration.R` (ny)

```r
test_that("register_session_lifecycle_events clears cache on reset", {
  app_state <- create_app_state()
  app_state$cache$qic_cache <- list(dummy = "data")

  observers <- register_session_lifecycle_events(
    app_state, input, output, session, create_emit_api(app_state)
  )

  # Trigger session reset
  session$sendCustomMessage("session_reset", list())
  session$flushReact()

  # Assert: cache cleared
  expect_null(app_state$cache$qic_cache)
})
```

**Acceptance**: Event registration modularized, cohesion improved

---

### **FASE 6: Integration & Polish** (2-3 timer)

#### 6.1 Update All Tests

```bash
# Run full suite
R -e "library(SPCify); testthat::test_dir('tests/testthat')"

# Verify no regressions
R -e "devtools::check()"
```

#### 6.2 Performance Validation

```bash
# Compare with baseline
R -e "source('tests/performance/test-qic-caching-benchmark.R')" > baseline_post_refactor.txt

# Diff analysis
diff baseline_pre_refactor.txt baseline_post_refactor.txt
```

**Acceptance**:
- Plot generation time: <5% increase
- Memory usage: <10% increase
- Cache hit rate: ±2% (should be stable)

#### 6.3 Visual Regression Test

```r
# shinytest2 snapshot test
test_that("Visualization output unchanged after refactor", {
  app <- AppDriver$new()
  app$upload_file(file = "test_data.csv")
  app$set_inputs(y_column = "value")
  app$wait_for_idle()

  # Snapshot current plot
  app$expect_values(output = "spc_plot_actual")
})
```

#### 6.4 Manual Smoke Test

**Checklist:**
- [ ] Upload CSV file
- [ ] Auto-detection triggers
- [ ] Plot renders correctly
- [ ] Value boxes show correct metrics
- [ ] Anhøj rules update on centerline change
- [ ] Download plot works
- [ ] Session reset clears state

#### 6.5 Documentation

**Update ADR:**

```markdown
# ADR-015: Visualization Module Refactor

## Status
Accepted

## Context
visualizationModuleServer() var 1237 linjer med blandet ansvar,
vanskelig at teste og debugge.

## Decision
Split in 3 helper files:
- utils_visualization_pipeline.R (data flow)
- utils_visualization_computation.R (SPC logic)
- utils_visualization_ui.R (rendering)

## Consequences
+ Testable units
+ Clear separation of concerns
+ Easier debugging
- More files (but clearer organization)
- Learning curve for new contributors

## Performance Impact
<5% regression acceptable, measured as acceptable.
```

---

## 🧪 Test Coverage Improvements (Prerequisite Work)

**Disse tests SKAL laves INDEN Fase 2-6:**

### 1. Visualization Module Integration Tests

**Fil**: `tests/testthat/test-mod-spc-chart-integration.R`

**Current state**: Placeholder stubs med skip()

**Target**: Real testServer() assertions

```r
test_that("visualization module handles data updates", {
  testServer(visualizationModuleServer, args = list(...), {
    # Test 1: Initial state
    expect_null(session$returned$module_data_reactive())

    # Test 2: Data load triggers update
    app_state$data$current_data <- test_data
    emit$visualization_update_needed()
    session$flushReact()

    expect_equal(nrow(session$returned$module_data_reactive()), 30)

    # Test 3: Plot ready after computation
    expect_true(session$returned$plot_ready())
  })
})

test_that("visualization module handles invalid data gracefully", {
  # Test error paths
})

test_that("visualization module caches plot correctly", {
  # Test cache behavior
})
```

### 2. Anhøj Results Consolidation Tests

**Fil**: `tests/testthat/test-anhoej-results.R`

**Expand with edge cases:**

```r
test_that("update_anhoej_results preserves on NA (cache hit)", {
  # ...
})

test_that("update_anhoej_results resets on centerline change", {
  # ...
})

test_that("update_anhoej_results handles show_phases toggle", {
  # ...
})
```

### 3. Performance Benchmark Improvements

**Fil**: `tests/performance/test-qic-caching-benchmark.R`

**Add:**
- Memory profiling (`pryr::mem_change()`)
- Cache hit rate tracking
- Reactive execution count

---

## ⚠️ Risk Mitigation

### High-Risk Areas

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| **Regression in plot generation** | Medium | Critical | Extensive integration tests, visual regression tests |
| **Performance degradation** | Low | High | Baseline benchmarks, profvis analysis |
| **Cache invalidation bugs** | Medium | High | Cache-specific tests, edge case coverage |
| **Anhøj rules calculation error** | Low | High | Unit tests for derive_qic_results(), comparison tests |
| **Event system disruption** | Low | Medium | Event bus integration tests |

### Rollback Plan

**Per Fase:**
```bash
# If regression detected
git revert <commit>
git checkout master
```

**Full Rollback:**
```bash
git tag pre-major-refactor-$(date +%Y%m%d)
# If needed:
git reset --hard pre-major-refactor-YYYYMMDD
```

### Feature Flag (Optional)

```r
# In golem-config.yml
default:
  visualization:
    use_refactored_module: false

# In mod_spc_chart.R
if (golem::get_golem_options("visualization")$use_refactored_module) {
  visualizationModuleServerRefactored(...)
} else {
  visualizationModuleServer(...)
}
```

**Fordel**: A/B testing i production, gradual rollout

---

## 📊 Success Metrics

### Code Metrics

**Before Refactor:**
- visualizationModuleServer: 1237 linjer
- Cyclomatic complexity: ~45
- Test coverage: ~60%

**After Refactor (Target):**
- visualizationModuleServer: ≤150 linjer
- Longest helper: ≤100 linjer
- Cyclomatic complexity: ≤10 per function
- Test coverage: ≥90%

### Performance Metrics

**Acceptable Regression Thresholds:**
- Plot generation time: <5% increase
- Memory usage: <10% increase
- Cache hit rate: ±2% variance
- Reactive execution count: ±10% variance

### Quality Metrics

**Zero Tolerance:**
- ❌ Breaking changes
- ❌ Test failures
- ❌ Manual smoke test failures
- ❌ Visual regressions

---

## 🚀 Execution Timeline

**Estimeret total tid: 15-20 timer over 2-3 uger**

### Uge 1: Preparation & Testing (4-5 timer)
- Dag 1-2: Test coverage improvements (Fase 1)
- Dag 3: Performance baseline + ADR draft

### Uge 2: Core Refactoring (8-10 timer)
- Dag 1-2: Data pipeline extraction (Fase 2)
- Dag 3-4: Computation extraction (Fase 3)
- Dag 5: UI extraction (Fase 4)

### Uge 3: Integration & Polish (3-5 timer)
- Dag 1: Event registration split (Fase 5)
- Dag 2: Integration testing (Fase 6)
- Dag 3: Documentation + Review

---

## 📚 References

### Internal Docs
- `docs/REMEDIATION_MASTER_PLAN.md` - M12-M15 opgaver
- `todo/visualization_event_refactor_plan.md` - Original plan
- `todo/test-coverage.md` - Coverage gaps
- `CLAUDE.md` - Development principles

### Code Locations
- **Current module**: `R/mod_spc_chart_server.R` (1237 linjer)
- **Helper utilities**: `R/utils_anhoej_results.R`
- **Event system**: `R/utils_server_event_listeners.R`
- **Tests**: `tests/testthat/test-mod-spc-chart-integration.R`

### ADRs (To Create)
- `docs/adr/ADR-015-visualization-refactor.md` - Refactor decision
- `docs/adr/ADR-016-event-registration-split.md` - Event modularization

---

## ✅ Completion Checklist

### Prerequisites
- [ ] Test coverage ≥90% på visualization module
- [ ] Performance baseline documented
- [ ] M4-M6 tidyverse migration completed (optional)
- [ ] ADR-015 draft reviewed

### Implementation
- [ ] **Fase 1**: Test infrastructure completed
- [ ] **Fase 2**: Data pipeline extraction completed
- [ ] **Fase 3**: Computation extraction completed
- [ ] **Fase 4**: UI extraction completed
- [ ] **Fase 5**: Event registration split completed
- [ ] **Fase 6**: Integration & polish completed

### Validation
- [ ] All tests passing (`testthat::test_dir()`)
- [ ] Performance within thresholds (<5% regression)
- [ ] Visual regression tests passing (shinytest2)
- [ ] Manual smoke test passed
- [ ] `devtools::check()` clean
- [ ] Documentation updated (ADR-015, CHANGELOG.md)

### Deployment
- [ ] Code review completed
- [ ] Feature branch merged to master
- [ ] Git tag created (`git tag v1.x.x-refactor`)
- [ ] Retrospective holdt

---

**Status**: Dette dokument er den autoritære kilde for Major Visualization Refactor. Opdater løbende under implementering.
