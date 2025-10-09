# Sprint 5 Plan - Integration Testing & Code Cleanup

## Dato: 2025-01-10

## Sprint 5 Mål

Integration testing, code cleanup, og minor enhancements baseret på Sprint 4 findings.

---

## Opgave 1: Integration Test Suite 🎯

### Current State

**Problem**: Ingen integration tests for end-to-end workflows.

**Analysis**:
```bash
tests/
├── testthat/       # 96 unit tests ✅
├── performance/    # 16 performance tests ✅
└── integration/    # 0 tests ❌
```

**Missing Coverage**:
- Complete data upload → auto-detection → visualization workflow
- Session lifecycle management
- Error recovery scenarios
- Multi-user session isolation
- State synchronization across modules

### Implementation Plan

**Proposed Tests**:
```r
# tests/integration/test-full-data-workflow.R

test_that("complete data workflow executes successfully", {
  # 1. Upload data
  # 2. Auto-detection runs
  # 3. Columns detected
  # 4. UI synchronized
  # 5. Plot generated
  # 6. QIC cache utilized
})

test_that("session lifecycle management works correctly", {
  # 1. Session start
  # 2. Data upload
  # 3. State management
  # 4. Session reset
  # 5. Cleanup verification
})

test_that("error recovery scenarios handle gracefully", {
  # 1. Invalid data upload
  # 2. Missing columns
  # 3. QIC calculation failure
  # 4. UI sync failure
  # 5. State recovery
})
```

**Test Approach**:
- Use `shinytest2::AppDriver` for UI interaction
- Test actual workflows, not mocked components
- Verify state transitions
- Check error handling
- Validate cleanup

**Benefits**:
- Catch integration bugs before production
- Verify complete workflows
- Test error scenarios
- Validate state management
- Ensure cleanup works

**Estimated effort**: 3 timer

---

## Opgave 2: Code Cleanup & Optimization 🧹

### Current State

**Problem**: Minor code quality issues identified in Sprint 4 review.

**Findings from Code Review**:
1. Unused `cached_qic()` function in `R/utils_qic_caching.R`
2. No cache size limit (can grow unbounded)
3. No cache hit/miss metrics tracking

### Implementation Plan

#### 2.1 Remove Unused Code

**File**: `R/utils_qic_caching.R`

**Remove**:
- `cached_qic()` function (lines 175-208) - deprecated/unused
- Associated documentation

**Rationale**: Direct integration via `log_qic_call_wrapper()` is preferred approach.

#### 2.2 Add Cache Size Limit

**Enhancement**:
```r
# R/utils_qic_caching.R

create_qic_cache <- function(max_size = 50) {
  cache <- new.env(parent = emptyenv())

  # Add size limit enforcement
  enforce_size_limit = function() {
    if (length(ls(envir = cache)) > max_size) {
      # Remove oldest entries (FIFO)
      entries <- ls(envir = cache)
      oldest <- entries[1:10]  # Remove 10 oldest
      rm(list = oldest, envir = cache)
    }
  }

  list(
    set = function(key, value, timeout) {
      # ... existing code ...
      enforce_size_limit()  # Check after adding
    }
    # ... rest of functions ...
  )
}
```

**Configuration**:
```r
# R/config_system_config.R
CACHE_CONFIG <- list(
  default_timeout_seconds = 300,
  max_qic_cache_size = 50,  # NEW: Limit cache entries
  # ... existing config ...
)
```

#### 2.3 Add Cache Metrics Tracking

**Enhancement**:
```r
# R/utils_qic_caching.R

create_qic_cache <- function() {
  cache <- new.env(parent = emptyenv())
  metrics <- list(hits = 0, misses = 0, evictions = 0)

  list(
    get = function(key) {
      if (exists(key, envir = cache)) {
        metrics$hits <<- metrics$hits + 1
        # ... return cached value ...
      } else {
        metrics$misses <<- metrics$misses + 1
        return(NULL)
      }
    },

    get_metrics = function() {
      list(
        hits = metrics$hits,
        misses = metrics$misses,
        evictions = metrics$evictions,
        hit_rate = if (metrics$hits + metrics$misses > 0) {
          metrics$hits / (metrics$hits + metrics$misses)
        } else {
          0
        }
      )
    }
  )
}
```

**Expected Impact**:
- Memory bounded (max 50 entries)
- Performance monitoring (hit rate tracking)
- Better observability

**Estimated effort**: 2 timer

---

## Opgave 3: CI/CD Pipeline Enhancement 🚀

### Current State

**Problem**: Test runners created but not integrated with CI/CD.

**Analysis**:
```bash
# Test runners exist:
tests/run_unit_tests.R         ✅
tests/run_performance_tests.R  ✅
tests/run_integration_tests.R  ✅

# GitHub Actions workflows:
.github/workflows/             ❌ Not configured
```

### Implementation Plan

**Create GitHub Actions Workflow**:
```yaml
# .github/workflows/test.yml

name: R Package Tests

on:
  push:
    branches: [ master, develop, sprint* ]
  pull_request:
    branches: [ master, develop ]

jobs:
  unit-tests:
    runs-on: ubuntu-latest
    name: Unit Tests

    steps:
      - uses: actions/checkout@v3

      - name: Setup R
        uses: r-lib/actions/setup-r@v2
        with:
          r-version: '4.5.1'

      - name: Install dependencies
        run: |
          install.packages(c("remotes", "testthat"))
          remotes::install_deps(dependencies = TRUE)
        shell: Rscript {0}

      - name: Run unit tests
        run: Rscript tests/run_unit_tests.R

  performance-tests:
    runs-on: ubuntu-latest
    name: Performance Tests
    # Only on master branch
    if: github.ref == 'refs/heads/master'

    steps:
      - uses: actions/checkout@v3

      - name: Setup R
        uses: r-lib/actions/setup-r@v2
        with:
          r-version: '4.5.1'

      - name: Install dependencies
        run: |
          install.packages(c("remotes", "testthat", "microbenchmark"))
          remotes::install_deps(dependencies = TRUE)
        shell: Rscript {0}

      - name: Run performance tests
        run: Rscript tests/run_performance_tests.R

      - name: Upload performance results
        uses: actions/upload-artifact@v3
        with:
          name: performance-results
          path: performance_*.rds

  integration-tests:
    runs-on: ubuntu-latest
    name: Integration Tests
    # Only on master and develop
    if: github.ref == 'refs/heads/master' || github.ref == 'refs/heads/develop'

    steps:
      - uses: actions/checkout@v3

      - name: Setup R
        uses: r-lib/actions/setup-r@v2
        with:
          r-version: '4.5.1'

      - name: Install dependencies
        run: |
          install.packages(c("remotes", "testthat", "shinytest2"))
          remotes::install_deps(dependencies = TRUE)
        shell: Rscript {0}

      - name: Run integration tests
        run: Rscript tests/run_integration_tests.R
```

**Benefits**:
- Automated testing on every push
- Fast feedback (unit tests only on branches)
- Comprehensive validation (all tests on master)
- Performance tracking
- Integration verification

**Estimated effort**: 1 time

---

## Opgave 4: Documentation Enhancement 📚

### Current State

**Problem**: Mangler end-user dokumentation og deployment guide.

**Current Documentation**:
- ✅ CLAUDE.md - Developer instructions
- ✅ SPRINT*_PLAN.md - Sprint planning
- ✅ tests/README.md - Test structure
- ❌ USER_GUIDE.md - End-user guide
- ❌ DEPLOYMENT.md - Deployment instructions

### Implementation Plan

#### 4.1 User Guide

**File**: `docs/USER_GUIDE.md`

**Content**:
```markdown
# SPC App User Guide

## Getting Started
1. Upload your data (CSV format)
2. Columns auto-detected
3. Select chart type
4. View SPC chart

## Features
- Auto column detection
- Multiple chart types (run, i, p, u, c, g, t)
- Phase analysis
- Freeze periods
- Comments overlay

## Troubleshooting
- Missing columns → Check CSV format
- No plot generated → Verify data has ≥3 rows
- Performance issues → Clear cache
```

#### 4.2 Deployment Guide

**File**: `docs/DEPLOYMENT.md`

**Content**:
```markdown
# Deployment Guide

## Production Setup
1. Install R 4.5.1+
2. Install dependencies: `renv::restore()`
3. Configure environment: `Sys.setenv(GOLEM_CONFIG_ACTIVE = "production")`
4. Run app: `library(SPCify); run_app()`

## Docker Deployment
[Docker configuration example]

## Shiny Server Deployment
[Shiny Server configuration]

## Monitoring
- Check logs: `logs/`
- Performance metrics: Cache hit rates
- Error tracking: shinylogs integration
```

**Estimated effort**: 1 time

---

## Implementation Prioritet

### Sprint 5 Execution Plan

**Fase 1: Integration Tests** (Dag 1-2, 3 timer)
1. Create test-full-data-workflow.R
2. Create test-session-lifecycle.R
3. Create test-error-recovery.R
4. Verify all integration tests pass

**Fase 2: Code Cleanup** (Dag 2, 2 timer)
1. Remove unused cached_qic() function
2. Add cache size limit enforcement
3. Implement cache metrics tracking
4. Update documentation

**Fase 3: CI/CD Pipeline** (Dag 3, 1 time)
1. Create .github/workflows/test.yml
2. Configure job triggers
3. Test workflow execution
4. Document CI/CD setup

**Fase 4: Documentation** (Dag 3, 1 time)
1. Create USER_GUIDE.md
2. Create DEPLOYMENT.md
3. Update README.md
4. Review all documentation

**Total effort**: ~7 timer over 3 dage

---

## Success Criteria

### Sprint 5 Definition of Done

**Integration Tests**:
- [ ] 3+ integration test files created
- [ ] Full workflow tests passing
- [ ] Session lifecycle tests passing
- [ ] Error recovery tests passing
- [ ] shinytest2 integration working

**Code Cleanup**:
- [ ] unused cached_qic() removed
- [ ] Cache size limit implemented (max 50 entries)
- [ ] Cache metrics tracking added (hits/misses/evictions)
- [ ] Documentation updated

**CI/CD Pipeline**:
- [ ] GitHub Actions workflow created
- [ ] Unit tests run on all branches
- [ ] Performance tests run on master only
- [ ] Integration tests run on master/develop
- [ ] Artifacts uploaded correctly

**Documentation**:
- [ ] USER_GUIDE.md created
- [ ] DEPLOYMENT.md created
- [ ] README.md updated
- [ ] All guides reviewed and accurate

---

## Timeline

**Start**: 2025-01-10 (after Sprint 4)
**Estimated completion**: 2025-01-13

**Breakdown**:
- Integration tests: 3 timer
- Code cleanup: 2 timer
- CI/CD pipeline: 1 time
- Documentation: 1 time

**Total effort**: 7 timer

---

## Expected Impact

**Quality**:
- Integration bugs caught early
- Comprehensive test coverage
- Better code maintainability

**Performance**:
- Cache memory bounded
- Metrics for monitoring
- Performance tracking in CI

**Developer Experience**:
- Automated testing
- Clear deployment instructions
- Better documentation

**User Experience**:
- User guide for end-users
- Fewer production issues
- Better support materials

---

**Created**: 2025-01-10
**Status**: Planning Complete - Awaiting Sprint 4 Merge

---

## Notes from Sprint 4

### Lessons Learned
1. **Modularization Works**: Event listener split improved maintainability significantly
2. **Caching Pays Off**: 165ms (99.4%) improvement exceeded expectations
3. **Test Organization**: Performance test separation improved feedback loop

### Technical Debt Identified
1. Cache size unbounded → Add limit in Sprint 5
2. No cache metrics → Add tracking in Sprint 5
3. unused cached_qic() → Remove in Sprint 5
4. No integration tests → Add in Sprint 5
5. CI/CD manual → Automate in Sprint 5

### Recommendations
- Keep sprint scope focused (3-4 opgaver maximum)
- Performance benchmarks are valuable (continue in Sprint 5)
- Code reviews catch issues early (continue practice)
- Documentation is critical (enhance in Sprint 5)
