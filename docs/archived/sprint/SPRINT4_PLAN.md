# Sprint 4 Plan - Event System & Performance

## Dato: 2025-01-10

## Sprint 4 Mål

Event system maintainability og final performance optimizations.

---

## Opgave 1: Split Event Listeners for Maintainability ⏳

### Current State

**Problem**: setup_event_listeners() indeholder alle 27 observers i én stor funktion (1171 linjer).

**Analysis**:
```r
# R/utils_server_event_listeners.R (current structure)
setup_event_listeners <- function(app_state, emit, input, output, session, ui_service) {
  # 27 observers alle sammen i én funktion
  # - Data lifecycle events (6 observers)
  # - Auto-detection events (8 observers)
  # - UI sync events (7 observers)
  # - Chart type events (3 observers)
  # - Navigation events (3 observers)
}
```

**Issues**:
- Svært at navigere og vedligeholde
- Manglende separation of concerns
- Vanskelig at teste individuelt
- Observer registry er god, men kunne være mere struktureret

### Implementation Plan

**Proposed Structure**:
```r
# R/utils_server_event_listeners.R

#' Register Data Lifecycle Events
register_data_lifecycle_events <- function(app_state, emit, session) {
  observers <- list()

  # data_updated event handling
  observers$data_updated <- register_observer(
    "data_updated",
    observeEvent(app_state$events$data_updated, ...)
  )

  # data_loaded compatibility
  observers$data_loaded <- register_observer(
    "data_loaded",
    observeEvent(app_state$events$data_loaded, ...)
  )

  # ... other data events

  return(observers)
}

#' Register Auto-Detection Events
register_autodetect_events <- function(app_state, emit, session) {
  observers <- list()

  # auto_detection_started
  observers$started <- register_observer(
    "auto_detection_started",
    observeEvent(app_state$events$auto_detection_started, ...)
  )

  # ... other autodetect events

  return(observers)
}

#' Register UI Sync Events
register_ui_sync_events <- function(app_state, emit, input, output, session, ui_service) {
  observers <- list()

  # ui_sync_needed
  observers$sync_needed <- register_observer(
    "ui_sync_needed",
    observeEvent(app_state$events$ui_sync_needed, ...)
  )

  # ... other UI events

  return(observers)
}

#' Register Chart Type Events
register_chart_type_events <- function(app_state, input, session) {
  observers <- list()

  # chart_type changes
  observers$chart_type <- register_observer(
    "chart_type_change",
    observeEvent(input$chart_type, ...)
  )

  return(observers)
}

#' Register Navigation Events
register_navigation_events <- function(app_state, emit, input, session) {
  observers <- list()

  # session_started
  observers$session_started <- register_observer(
    "session_started",
    observeEvent(app_state$events$session_started, ...)
  )

  return(observers)
}

#' Main Event Listener Setup
setup_event_listeners <- function(app_state, emit, input, output, session, ui_service) {
  # Initialize registry
  observer_registry <<- list()

  # Register all event categories
  observers <- list(
    data = register_data_lifecycle_events(app_state, emit, session),
    autodetect = register_autodetect_events(app_state, emit, session),
    ui = register_ui_sync_events(app_state, emit, input, output, session, ui_service),
    chart = register_chart_type_events(app_state, input, session),
    navigation = register_navigation_events(app_state, emit, input, session)
  )

  # Setup cleanup
  session$onSessionEnded(function() {
    safe_operation(
      "Observer cleanup on session end",
      code = {
        observer_count <- length(observer_registry)
        for (observer_name in names(observer_registry)) {
          tryCatch({
            if (!is.null(observer_registry[[observer_name]])) {
              observer_registry[[observer_name]]$destroy()
            }
          }, error = function(e) {
            log_warn(paste("Failed to destroy observer:", observer_name))
          })
        }
        log_info(paste("Observer cleanup complete:", observer_count, "observers destroyed"))
      }
    )
  })

  return(observers)
}
```

**Benefits**:
- Separation of concerns - hver kategori i egen funktion
- Lettere at navigere og vedligeholde
- Kan testes individuelt
- Bedre organiseret observer registry
- Bevar centraliseret cleanup

**Implementation Steps**:
1. Create helper functions for each event category
2. Extract observers to appropriate helper
3. Update setup_event_listeners() to orchestrate
4. Test that all 27 observers still registered correctly
5. Verify cleanup still works

**Estimated effort**: 4 timer

---

## Opgave 2: QIC Result Caching ⏳

### Current State

**Problem**: QIC beregninger udføres hver gang uden caching.

**Analysis**:
```r
# Identified in performance analysis:
# qic() function from qicharts2 package called repeatedly
# Same data + same parameters = same result
# No caching = wasted computation
```

**Performance Impact**:
- QIC beregninger kan tage 50-200ms
- Kaldes ved hver chart render
- Data ændrer sig sjældent
- Perfect candidate for memoization

### Implementation Plan

**Proposed Solution**:
```r
# R/utils_qic_caching.R

#' Create QIC Result Cache
#'
#' Memoization for expensive QIC calculations.
#' Cache key based on data content + parameters.
#'
#' @export
create_qic_cache <- function() {
  cache <- new.env(parent = emptyenv())

  list(
    get = function(key) {
      if (exists(key, envir = cache)) {
        entry <- get(key, envir = cache)
        if (Sys.time() < entry$expires_at) {
          return(entry$value)
        } else {
          rm(list = key, envir = cache)
        }
      }
      return(NULL)
    },

    set = function(key, value, timeout = 300) {
      entry <- list(
        value = value,
        created_at = Sys.time(),
        expires_at = Sys.time() + timeout
      )
      assign(key, entry, envir = cache)
    },

    clear = function() {
      rm(list = ls(envir = cache), envir = cache)
    },

    size = function() {
      length(ls(envir = cache))
    }
  )
}

#' Generate QIC Cache Key
#'
#' Creates unique key based on data + parameters.
#'
#' @param data Data used for QIC
#' @param params QIC parameters (chart type, x, y, etc.)
#'
#' @export
generate_qic_cache_key <- function(data, params) {
  # Combine data digest + parameter digest
  data_digest <- digest::digest(data, algo = "md5")
  param_digest <- digest::digest(params, algo = "md5")

  paste0("qic_", data_digest, "_", param_digest)
}

#' Cached QIC Wrapper
#'
#' Wraps qic() calls with caching.
#'
#' @param data Data for QIC
#' @param x X variable
#' @param y Y variable
#' @param chart Chart type
#' @param ... Additional qic() parameters
#' @param .cache QIC cache object (created by create_qic_cache())
#' @param .cache_timeout Cache timeout in seconds (default: 300)
#'
#' @export
cached_qic <- function(data, x, y, chart, ...,
                       .cache = NULL,
                       .cache_timeout = CACHE_CONFIG$default_timeout_seconds) {

  # If no cache provided, call qic directly
  if (is.null(.cache)) {
    return(qicharts2::qic(
      data = data,
      x = x,
      y = y,
      chart = chart,
      ...
    ))
  }

  # Generate cache key
  params <- list(x = x, y = y, chart = chart, extra = list(...))
  cache_key <- generate_qic_cache_key(data, params)

  # Check cache
  cached_result <- .cache$get(cache_key)
  if (!is.null(cached_result)) {
    log_debug_kv(
      message = "QIC cache hit",
      cache_key = cache_key,
      .context = "QIC_CACHE"
    )
    return(cached_result)
  }

  # Cache miss - compute
  log_debug_kv(
    message = "QIC cache miss - computing",
    cache_key = cache_key,
    .context = "QIC_CACHE"
  )

  start_time <- Sys.time()
  result <- qicharts2::qic(
    data = data,
    x = x,
    y = y,
    chart = chart,
    ...
  )
  computation_time <- as.numeric(Sys.time() - start_time) * 1000

  # Cache result
  .cache$set(cache_key, result, timeout = .cache_timeout)

  log_debug_kv(
    message = "QIC result cached",
    cache_key = cache_key,
    computation_time_ms = round(computation_time, 2),
    .context = "QIC_CACHE"
  )

  return(result)
}
```

**Integration**:
```r
# In visualization code, replace:
# qic(data, x, y, chart, ...)

# With:
# cached_qic(data, x, y, chart, ..., .cache = qic_cache)
```

**Setup in app_state**:
```r
# R/state_management.R
app_state$cache <- reactiveValues(
  qic = create_qic_cache()
)

# Clear cache when data changes
observeEvent(app_state$events$data_updated, {
  app_state$cache$qic$clear()
})
```

**Expected Impact**:
- First render: Same speed (cache miss)
- Subsequent renders with same data: 50-200ms faster
- Especially beneficial when switching between views

**Implementation Steps**:
1. Create R/utils_qic_caching.R
2. Add qic cache to app_state
3. Replace qic() calls with cached_qic()
4. Clear cache on data_updated event
5. Test cache hit/miss logging
6. Verify performance improvement

**Estimated effort**: 2 timer

---

## Opgave 3: Performance Test Isolation ⏳

### Current State

**Problem**: Performance tests blandet med unit tests.

**Analysis**:
```bash
# Current structure
tests/testthat/
├── test-safe-operation-comprehensive.R
├── test-error-handling.R
├── test-logging-standardization.R
└── ... (mixed unit and performance tests)

# Issues:
# - Unit tests should be fast
# - Performance tests are slow (benchmarking)
# - CI/CD runs everything on each commit
```

### Implementation Plan

**Proposed Structure**:
```bash
tests/
├── testthat/           # Fast unit tests only
│   ├── test-safe-operation.R
│   ├── test-error-handling.R
│   └── test-logging.R
│
├── performance/        # Slow performance tests
│   ├── test-startup-performance.R
│   ├── test-qic-caching.R
│   ├── test-vectorization.R
│   └── helper-benchmark.R
│
└── integration/        # Integration tests
    ├── test-full-workflow.R
    └── test-session-lifecycle.R
```

**Test Runner Scripts**:
```r
# tests/run_unit_tests.R
testthat::test_dir("tests/testthat")

# tests/run_performance_tests.R
testthat::test_dir("tests/performance")

# tests/run_all_tests.R
source("tests/run_unit_tests.R")
source("tests/run_performance_tests.R")
source("tests/run_integration_tests.R")
```

**CI/CD Integration**:
```yaml
# .github/workflows/test.yml
jobs:
  unit-tests:
    runs-on: ubuntu-latest
    steps:
      - name: Run unit tests
        run: Rscript tests/run_unit_tests.R

  performance-tests:
    runs-on: ubuntu-latest
    # Only on release branches
    if: github.ref == 'refs/heads/master'
    steps:
      - name: Run performance tests
        run: Rscript tests/run_performance_tests.R
```

**Implementation Steps**:
1. Create tests/performance/ directory
2. Move performance tests from tests/testthat/
3. Create test runner scripts
4. Update CI/CD configuration
5. Document test structure in README

**Expected Benefits**:
- Faster feedback loop (unit tests ~seconds)
- Performance tests only on important branches
- Better test organization
- CI resources optimized

**Estimated effort**: 1 time

---

## Implementation Prioritet

### Sprint 4 Execution Plan

**Fase 1: Event Listeners Split** (Dag 1-2, 4 timer)
1. Create helper functions for each category
2. Extract observers systematically
3. Update main setup_event_listeners()
4. Test observer registration and cleanup

**Fase 2: QIC Caching** (Dag 2-3, 2 timer)
1. Create utils_qic_caching.R
2. Integrate with app_state
3. Replace qic() calls
4. Test and verify performance improvement

**Fase 3: Test Isolation** (Dag 3, 1 time)
1. Restructure test directories
2. Create test runners
3. Update CI/CD
4. Document changes

**Total effort**: ~7 timer over 3 dage

---

## Success Criteria

### Sprint 4 Definition of Done

**Event Listeners**: ✅ COMPLETE
- [x] 5 helper functions created (register_*_events)
- [x] All 27 observers moved to appropriate helpers
- [x] setup_event_listeners() orchestrates correctly
- [x] Cleanup still works (all observers destroyed)
- [x] Code more maintainable and navigable

**QIC Caching**: ✅ COMPLETE
- [x] utils_qic_caching.R implemented
- [x] Cache integrated in app_state (lazy initialization)
- [x] Cache integration via log_qic_call_wrapper()
- [x] Cache cleared on data updates and session reset
- [x] Performance improvement expected (50-200ms on cache hit, pending benchmarks)

**Test Isolation**: ✅ COMPLETE
- [x] tests/performance/ directory created
- [x] 16 performance tests moved
- [x] Test runners created (4 scripts)
- [x] CI/CD ready (scripts prepared, not deployed yet)
- [x] Documentation updated (tests/README.md)

---

## Timeline

**Start**: 2025-01-10
**Estimated completion**: 2025-01-10 (3 dage compressed)

**Breakdown**:
- Event listeners: 4 timer
- QIC caching: 2 timer
- Test isolation: 1 time

**Total effort**: 7 timer

---

## Expected Impact

**Maintainability**:
- Event system easier to navigate and modify
- Clear separation of concerns
- Individual components testable

**Performance**:
- QIC caching: 50-200ms speedup on repeated renders
- Faster CI/CD feedback loop (unit tests only)
- Better resource utilization

**Code Quality**:
- Organized test structure
- Performance tests isolated
- Better documentation

---

**Created**: 2025-01-10
**Completed**: 2025-01-10
**Status**: ✅ IMPLEMENTATION COMPLETE

---

## Implementation Summary

### Commits

1. **Fase 1**: Event Listener Split
   - Commit: Split event listeners into modular helper functions
   - Files: R/utils_server_event_listeners.R refactored
   - Result: 5 helper functions, 27 observers organized

2. **Fase 2**: QIC Result Caching
   - Commit: `2b14667` - feat(performance): Sprint 4 Fase 2 - QIC result caching
   - Files: R/utils_qic_caching.R (new), R/state_management.R, R/utils_qic_debug_logging.R, R/fct_spc_plot_generation.R, R/mod_spc_chart_server.R, R/utils_chart_module_helpers.R, R/utils_server_event_listeners.R
   - Result: Complete caching infrastructure with lazy initialization

3. **Fase 3**: Test Isolation
   - Commit: `b60d564` - refactor(tests): Sprint 4 Fase 3 - Test isolation
   - Files: 16 tests moved, 4 runners created, tests/README.md
   - Result: 96 unit tests (fast), 16 performance tests (slow)

### Actual Timeline

- Start: 2025-01-10
- End: 2025-01-10
- **Total time**: ~3 hours (compressed from estimated 7 hours)
- **Efficiency**: 57% faster than estimated

### Key Achievements

1. **Event System Maintainability**: ✅
   - 5 modular helper functions
   - Clear separation of concerns
   - Easier navigation and testing

2. **Performance Optimization**: ✅
   - QIC result caching implemented
   - Expected 50-200ms improvement on cache hits
   - Lazy initialization prevents dependency issues

3. **Test Organization**: ✅
   - Performance tests isolated
   - Faster CI/CD feedback loop
   - Clear test categories and runners
