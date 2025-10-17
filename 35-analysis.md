# Task 035 Analysis: Cleanup & Documentation for BFHcharts SPC Migration

**Task ID:** 035
**Epic:** BFHcharts SPC Migration
**Type:** Cleanup, Documentation, Testing
**Dependencies:** Tasks 029-034 (Migration Work)
**Status:** Planning
**Created:** 2025-10-17

---

## Executive Summary

Task 035 is the final cleanup and documentation phase of the BFHcharts SPC migration epic. Based on analysis of the migration branch (`epic/bfhcharts-spc-migration`), significant migration work has been completed including:

- **BFHcharts integration** with HYBRID backend architecture (Tasks 031-033)
- **Shinytest2 snapshot tests** for BFHcharts module (Task 032)
- **Cache abstraction** and BFHcharts integration (Task 033)
- **Cross-repository coordination infrastructure** (Task 036)

This task focuses on removing legacy qicharts2 code, cleaning up temporary test files, updating documentation, and ensuring the codebase is production-ready.

---

## 1. Current State Assessment

### 1.1 Migration Work Completed

**Recent Commits:**
```
12407d3 - Task 032 Stream B: Shinytest2 snapshot tests for BFHchart module integration
d2fe776 - Task 033 Stream 1: Cache abstraction and BFHchart integration
07780e7 - Task 036: docs(coordination): add cross-repository coordination infrastructure
```

**Key Files Modified:**
- `R/fct_spc_plot_generation.R` (649 lines) - Contains legacy code marked for removal
- `R/fct_spc_bfh_service.R` (1,964 lines) - New BFHcharts service layer
- `R/mod_spc_chart_server.R` - Updated for backend switching

**Backend Architecture:**
- HYBRID workflow implemented (`generateSPCPlot_with_backend()`)
- Feature flag system via `inst/golem-config.yml`
- BFHcharts backend for supported chart types (run, i, p, c, u)
- qicharts2 fallback for unsupported types

### 1.2 Legacy Code Still Present

**Functions Marked for Removal:**
1. **`add_plot_enhancements()`** - Lines 461-594 (133 lines)
   - DUPLICATE of `BFHcharts::add_plot_enhancements()`
   - Extended CL/Target lines, comment annotations
   - Status: Comments indicate "Moved to BFHcharts" but function body still exists

2. **`applyHospitalTheme()`** - Lines 1389-1438 (50 lines)
   - OBSOLETE - Replaced by `BFHcharts::apply_spc_theme()` + `BFHtheme::theme_bfh()`
   - Status: Comments indicate "Moved to BFHcharts/BFHtheme" but function body still exists

3. **`generateSPCPlot_qicharts2()`** - Still active (649 lines file)
   - Legacy backend, should be DEPRECATED (not removed)
   - Needed for backward compatibility and unsupported chart types

**Current References:**
- `BFHcharts::` or `bfhchart::` - 6 occurrences across 3 files
- `applyHospitalTheme` or `add_plot_enhancements` - 4 occurrences across 2 files
- `qicharts2` - 63 files still reference qicharts2 (including tests)

### 1.3 Temporary Test Files

**Root-level test files (19 files):**
```
test_backend_switch.R
test_backend_switching_simple.R
test_backend_switching.R
test_bfh_direct.R
test_bfh_sanitized.R
test_bfh_simple.R
test_bug1_bug4_complete.R
test_bug1_bug4_minimal.R
test_bug1_bug4_simple.R
test_freeze_phase_detection.R
test_numeric_data_flow.R
test_qicharts2_minimal.R
test_service_basic.R
test_service_comprehensive.R
test_target_y_axis.R
verify_bfhcharts_fix.R
verify_nse_fix.R
verify_part_fix.R
verify_target_line.R
```

**Status:** These are temporary development/debugging scripts created during migration. Should be reviewed and either:
- Moved to `tests/testthat/` if valuable
- Documented in `tests/MANUAL_TEST_GUIDE.md` if manual
- Deleted if obsolete

### 1.4 Documentation Status

**New Documentation (Created):**
- ✅ `docs/CROSS_REPO_COORDINATION.md` (1,896 lines) - Comprehensive cross-repo guide
- ✅ `docs/LEGACY_CODE_CLEANUP.md` (376 lines) - Cleanup strategy and plan
- ✅ `docs/GITHUB_LABELS_SETUP.md` - Issue tracking infrastructure
- ✅ `CHANGELOG.md` - Version history

**Existing Documentation (Needs Update):**
- ⚠️ `CLAUDE.md` - Section 5.3 covers external package ownership, needs BFHcharts integration notes
- ⚠️ `README.md` - Mentions qicharts2 integration, needs BFHcharts backend update
- ⚠️ `docs/CONFIGURATION.md` - May need updates for BFHcharts config
- ⚠️ Architecture docs - Need to reflect HYBRID backend pattern

**Missing Documentation:**
- ❌ BFHcharts migration guide for developers
- ❌ BFHcharts known limitations and workarounds
- ❌ Backend switching guide (when to use which)
- ❌ Cross-repo issue escalation examples

### 1.5 Test Coverage

**Integration Tests:**
- ✅ Shinytest2 snapshot tests added (Task 032)
- ✅ Cache integration tests (Task 033)
- ⚠️ May need additional BFHcharts-specific tests

**TODO/FIXME Comments:**
Only 3 files contain TODO/FIXME/HACK comments:
- `R/fct_spc_bfh_service.R`
- `tests/testthat/test-phase2-device-size-matrix.R`
- `R/utils_server_server_management.R`

---

## 2. Work Streams

### Stream A: Legacy Code Removal
**Estimated Time:** 4-6 hours
**Priority:** High
**Risk:** Medium (requires careful testing)

### Stream B: Documentation Updates
**Estimated Time:** 6-8 hours
**Priority:** High
**Risk:** Low

### Stream C: Test Cleanup & Validation
**Estimated Time:** 4-5 hours
**Priority:** Medium
**Risk:** Low

### Stream D: Final Integration Testing
**Estimated Time:** 3-4 hours
**Priority:** High
**Risk:** Medium

**Total Estimated Time:** 17-23 hours (2-3 days)

---

## 3. Detailed Work Plan

### 3.1 Stream A: Legacy Code Removal

#### A1: Remove Duplicate `add_plot_enhancements()` (1-2 hours)

**Location:** `R/fct_spc_plot_generation.R` lines 461-594

**Pre-removal Checklist:**
- [ ] Search codebase for direct calls to `add_plot_enhancements()`
- [ ] Verify all calls use `BFHcharts::add_plot_enhancements()`
- [ ] Check NAMESPACE imports `importFrom(BFHcharts, add_plot_enhancements)`
- [ ] Run full test suite to confirm no breakage

**Steps:**
1. **Search for references:**
   ```bash
   cd /path/to/epic-bfhcharts-spc-migration
   grep -rn "add_plot_enhancements" R/ tests/ --exclude-dir=.git
   ```

2. **Update any remaining calls:**
   ```r
   # BEFORE:
   plot <- add_plot_enhancements(plot, qic_data, comment_data)

   # AFTER:
   plot <- BFHcharts::add_plot_enhancements(plot, qic_data, comment_data)
   ```

3. **Delete function body:**
   - Delete lines 461-594 in `R/fct_spc_plot_generation.R`
   - Keep comment header: `# Moved to BFHcharts - see BFHcharts:::add_plot_enhancements()`

4. **Update tests:**
   - Remove unit tests for `add_plot_enhancements()` (now BFHcharts responsibility)
   - Keep integration tests that verify plot output

5. **Run verification:**
   ```r
   devtools::test()
   devtools::check()
   ```

**Expected Outcome:**
- ~133 lines removed from `fct_spc_plot_generation.R`
- Zero test failures
- Cleaner codebase with single source of truth

#### A2: Remove Obsolete `applyHospitalTheme()` (1 hour)

**Location:** `R/fct_spc_plot_generation.R` lines 1389-1438

**Pre-removal Checklist:**
- [ ] Search for `applyHospitalTheme()` calls
- [ ] Verify replacement with `BFHcharts::apply_spc_theme()`
- [ ] Check NAMESPACE imports

**Steps:**
1. **Search and replace:**
   ```r
   # BEFORE:
   plot <- applyHospitalTheme(plot, base_size = 14)

   # AFTER:
   plot <- BFHcharts::apply_spc_theme(plot, base_size = 14)
   ```

2. **Delete function (lines 1389-1438)**

3. **Update tests and verify**

**Expected Outcome:**
- ~50 lines removed
- Consistent theming via BFHcharts/BFHtheme

#### A3: Deprecate `generateSPCPlot_qicharts2()` (1-2 hours)

**Important:** Do NOT remove - needed for backward compatibility

**Steps:**
1. **Add deprecation warning:**
   ```r
   #' @title Generate SPC Plot (qicharts2 Backend - DEPRECATED)
   #'
   #' @description
   #' **DEPRECATED**: This function uses the legacy qicharts2 backend.
   #' Use [generateSPCPlot_with_backend()] with BFHcharts backend instead.
   #'
   #' This function is maintained for backward compatibility and fallback
   #' for chart types not yet supported by BFHcharts.
   #'
   #' @inheritParams generateSPCPlot_with_backend
   #' @return List with plot and qic_data
   #' @export
   #' @keywords internal
   generateSPCPlot_qicharts2 <- function(...) {
     .Deprecated(
       new = "generateSPCPlot_with_backend",
       msg = "generateSPCPlot_qicharts2() is deprecated. Use generateSPCPlot_with_backend() with BFHcharts backend instead."
     )

     # ... existing implementation ...
   }
   ```

2. **Update documentation:**
   - Mark as deprecated in roxygen2 comments
   - Add `@keywords internal` to hide from main docs
   - Document when it will be removed (e.g., "Will be removed in v2.0.0")

3. **Update NAMESPACE:**
   ```r
   # Keep exported for backward compatibility, but mark internal
   ```

**Expected Outcome:**
- Function preserved for compatibility
- Clear deprecation signals to developers
- Maintained for unsupported chart types

#### A4: Clean Up Candidates for Deletion (1 hour)

**Location:** `candidates_for_deletion/utils_server_performance_opt.R`

**Steps:**
1. **Review file content:**
   - Determine if any code should be salvaged
   - Check git history for context

2. **Decision:**
   - If truly obsolete: Delete directory
   - If contains useful code: Extract and move to proper location
   - Document decision in cleanup commit message

**Expected Outcome:**
- Cleaner directory structure
- No orphaned code directories

---

### 3.2 Stream B: Documentation Updates

#### B1: Update `CLAUDE.md` with BFHcharts Integration (2-3 hours)

**Location:** `/Users/johanreventlow/Documents/R/claude_spc/CLAUDE.md`

**Updates Needed:**

1. **Section 5.3 - External Package Ownership (ENHANCE):**
   - Already mentions BFHcharts/BFHthemes ownership ✅
   - Add subsection: "BFHcharts Backend Integration"

   ```markdown
   ### 5.3.1 BFHcharts Backend Integration

   **Architecture:** HYBRID backend system

   SPCify implements a feature flag system for backend selection:

   ```r
   # Configuration: inst/golem-config.yml
   features:
     use_bfhchart: true  # Enable BFHcharts backend
     bfhchart_supported_types: ["run", "i", "p", "c", "u"]
   ```

   **Backend Selection Flow:**
   1. `generateSPCPlot_with_backend()` reads feature flag
   2. Routes to `compute_spc_results_bfh()` (BFHcharts) if enabled + supported
   3. Falls back to `generateSPCPlot_qicharts2()` for unsupported types

   **When to Use Which Backend:**
   - **BFHcharts (Preferred):** All supported chart types in production
   - **qicharts2 (Fallback):** Unsupported types, debugging, regression testing

   **Cross-Repository Coordination:**
   See `docs/CROSS_REPO_COORDINATION.md` for:
   - Issue escalation workflows
   - Version coordination strategies
   - Test case sharing
   - Breaking change management
   ```

2. **Section 5.3.2 - BFHcharts Known Limitations:**

   ```markdown
   ### 5.3.2 BFHcharts Known Limitations

   **Current Limitations:**
   1. **Centerline parameter:** Use `cl` instead of `centerline_value`
   2. **Chart types:** Only run, i, p, c, u supported (not mr, xbar, s)
   3. **Target line rendering:** Requires BFHcharts >= 0.3.0

   **Workarounds in SPCify:**
   - Parameter mapping in `compute_spc_results_bfh()`
   - Automatic fallback to qicharts2 for unsupported types
   - See `R/fct_spc_bfh_service.R` for implementation
   ```

3. **Section 3.1 - Shiny Best Practices (ADD):**

   ```markdown
   **Backend Switching Pattern:**
   ```r
   # ✅ Correct: Use unified interface
   result <- generateSPCPlot_with_backend(
     data = data, config = config, chart_type = chart_type,
     target_value = target_value
   )

   # ❌ Incorrect: Direct backend calls
   result <- compute_spc_results_bfh(...)  # Bypasses feature flag
   result <- generateSPCPlot_qicharts2(...) # Legacy
   ```
   ```

4. **Appendix F - BFHcharts Quick Reference (NEW):**

   ```markdown
   ## 📎 Appendix F: BFHcharts Quick Reference

   ### Backend Architecture

   ```
   generateSPCPlot() (alias)
         ↓
   generateSPCPlot_with_backend()
         ↓
      [Feature Flag Check]
         ↓
   ┌──────────┴──────────┐
   │                     │
   BFHcharts         qicharts2
   (Preferred)       (Fallback)
   ```

   ### File Structure
   - `R/fct_spc_plot_generation.R` - Wrapper + qicharts2 backend (649 lines)
   - `R/fct_spc_bfh_service.R` - BFHcharts backend (1,964 lines)
   - `inst/golem-config.yml` - Feature flags

   ### Common Tasks

   **Enable BFHcharts backend:**
   ```yaml
   # inst/golem-config.yml
   features:
     use_bfhchart: true
   ```

   **Add chart type support:**
   1. Add to `bfhchart_supported_types` in config
   2. Ensure BFHcharts implements chart type
   3. Test with integration suite

   **Debug backend selection:**
   ```r
   options(spc.debug.backend = TRUE)
   # Check logs for "[BACKEND_WRAPPER]" component
   ```

   ### Testing BFHcharts Integration

   ```r
   # Unit test BFHcharts service
   testthat::test_file('tests/testthat/test-spc-bfh-service.R')

   # Shinytest2 snapshot tests
   testthat::test_file('tests/testthat/test-bfhcharts-integration.R')

   # Full regression suite
   devtools::test()
   ```
   ```

**Validation:**
- [ ] All code examples tested
- [ ] Links to referenced files correct
- [ ] Consistent with existing CLAUDE.md style
- [ ] No breaking changes to existing workflows

#### B2: Update `README.md` (1-2 hours)

**Location:** `/Users/johanreventlow/Documents/R/epic-bfhcharts-spc-migration/README.md`

**Current State:** Mentions qicharts2 integration prominently

**Updates Needed:**

1. **Line 3 - Update project description:**
   ```markdown
   # BEFORE:
   En professionel Shiny applikation til **Statistical Process Control (SPC)**
   analyser med dansk interface og integration med qicharts2.

   # AFTER:
   En professionel Shiny applikation til **Statistical Process Control (SPC)**
   analyser med dansk interface. Anvender BFHcharts visualization engine med
   qicharts2 fallback for maksimal kompatibilitet.
   ```

2. **Section "Features" - Add Backend Architecture:**
   ```markdown
   ### Backend Architecture
   - **HYBRID Backend System**: BFHcharts (primary) + qicharts2 (fallback)
   - **Feature Flag Control**: Runtime backend selection via golem config
   - **Cross-Repository Coordination**: Integrated issue escalation med BFHcharts
   - **Comprehensive Testing**: Shinytest2 snapshots + integration tests
   ```

3. **Section "Architecture" - Update State Management:**
   ```markdown
   ### Backend Selection (HYBRID)
   ```r
   # Feature flag configuration (inst/golem-config.yml)
   features:
     use_bfhchart: true
     bfhchart_supported_types: ["run", "i", "p", "c", "u"]

   # Unified interface
   generateSPCPlot() → generateSPCPlot_with_backend()
                    ↓
            [BFHcharts eller qicharts2]
   ```
   ```

4. **Section "Dependencies" - Add BFHcharts:**
   ```markdown
   ### Core Dependencies
   ```r
   # SPC Engines
   BFHcharts (>= 0.2.0)  # Primary visualization backend
   qicharts2             # Legacy fallback backend

   # Theming
   BFHtheme (>= 0.1.0)   # Hospital branding
   ```
   ```

5. **Section "Documentation" - Add BFHcharts guides:**
   ```markdown
   ### Backend Documentation
   - **[CROSS_REPO_COORDINATION.md](docs/CROSS_REPO_COORDINATION.md)**:
     BFHcharts issue escalation og version coordination
   - **[LEGACY_CODE_CLEANUP.md](docs/LEGACY_CODE_CLEANUP.md)**:
     Migration plan og cleanup strategy
   ```

**Validation:**
- [ ] All new sections formatted correctly
- [ ] Code examples syntax-valid
- [ ] Links to docs/ files work
- [ ] Consistent Danish/English usage

#### B3: Create Developer Migration Guide (2-3 hours)

**New File:** `docs/BFHCHARTS_MIGRATION_GUIDE.md`

**Content Structure:**

```markdown
# Developer Guide: BFHcharts Backend Migration

**Target Audience:** SPCify developers maintaining or extending the codebase
**Last Updated:** 2025-10-17
**Version:** 1.0.0

---

## Table of Contents

1. [Overview](#overview)
2. [Architecture Changes](#architecture-changes)
3. [Code Migration Patterns](#code-migration-patterns)
4. [Testing Strategy](#testing-strategy)
5. [Troubleshooting](#troubleshooting)
6. [FAQ](#faq)

---

## 1. Overview

### What Changed?

SPCify transitioned from qicharts2-only to a HYBRID backend system:

**Before (Legacy):**
```r
generateSPCPlot() → generateSPCPlot_qicharts2() → qicharts2::qic()
```

**After (HYBRID):**
```r
generateSPCPlot() → generateSPCPlot_with_backend()
                         ↓
                [Feature Flag Check]
                         ↓
        ┌────────────────┴────────────────┐
        ↓                                 ↓
  BFHcharts::spc_chart()        qicharts2::qic()
  (Preferred)                   (Fallback)
```

### Why?

1. **Better Separation of Concerns:** Visualization engine separated from integration logic
2. **Maintainability:** BFHcharts owned by project maintainer (not external)
3. **Flexibility:** Feature flag allows runtime backend selection
4. **Backward Compatibility:** qicharts2 fallback for unsupported chart types

---

## 2. Architecture Changes

### 2.1 File Structure

**New Files:**
- `R/fct_spc_bfh_service.R` (1,964 lines) - BFHcharts backend implementation
- `tests/testthat/test-spc-bfh-service.R` - BFHcharts service tests
- `tests/testthat/test-bfhcharts-integration.R` - Shinytest2 snapshots

**Modified Files:**
- `R/fct_spc_plot_generation.R` - Added `generateSPCPlot_with_backend()`
- `R/mod_spc_chart_server.R` - Uses unified interface
- `inst/golem-config.yml` - Feature flags

**Removed Functions:**
- `add_plot_enhancements()` → Moved to `BFHcharts::add_plot_enhancements()`
- `applyHospitalTheme()` → Moved to `BFHcharts::apply_spc_theme()`

### 2.2 Feature Flag System

**Configuration:** `inst/golem-config.yml`

```yaml
default:
  features:
    use_bfhchart: false  # Disabled by default (safe rollout)
    bfhchart_supported_types: ["run", "i", "p", "c", "u"]

development:
  features:
    use_bfhchart: true  # Enabled in dev for testing

production:
  features:
    use_bfhchart: true  # Enable after validation
```

**Reading Feature Flags:**
```r
features_config <- golem::get_golem_options("features")
use_bfhchart <- isTRUE(features_config$use_bfhchart)
```

### 2.3 Backend Interface Contract

Both backends return consistent structure:

```r
# Return structure (both backends)
list(
  plot = <ggplot2 object>,        # The rendered plot
  qic_data = <data.frame>,        # Underlying SPC data
  metadata = list(                # Optional metadata
    backend = "bfhcharts",        # Which backend was used
    chart_type = "i",
    version = "0.2.0"
  )
)
```

---

## 3. Code Migration Patterns

### 3.1 Using the Unified Interface

**Do:**
```r
# ✅ Use unified interface (respects feature flag)
result <- generateSPCPlot(
  data = data,
  config = config,
  chart_type = "i",
  target_value = 50
)

# Or explicit wrapper:
result <- generateSPCPlot_with_backend(...)
```

**Don't:**
```r
# ❌ Direct backend calls (bypasses feature flag)
result <- compute_spc_results_bfh(...)
result <- generateSPCPlot_qicharts2(...)
```

### 3.2 Adding New Features

**When adding features that affect plot generation:**

1. **Determine ownership:**
   - Visualization logic → BFHcharts (escalate issue)
   - Parameter mapping → SPCify facade layer
   - UI/UX → SPCify Shiny modules

2. **Implement in both backends (if SPCify-owned):**
   ```r
   # BFHcharts backend (R/fct_spc_bfh_service.R)
   compute_spc_results_bfh <- function(..., new_parameter = NULL) {
     # ... implementation using BFHcharts API ...
   }

   # qicharts2 backend (R/fct_spc_plot_generation.R)
   generateSPCPlot_qicharts2 <- function(..., new_parameter = NULL) {
     # ... implementation using qicharts2 API ...
   }

   # Wrapper passes through
   generateSPCPlot_with_backend <- function(..., new_parameter = NULL) {
     # Routes to appropriate backend
   }
   ```

3. **Write tests for both paths:**
   ```r
   test_that("New parameter works with BFHcharts backend", {
     with_bfhchart_enabled({
       result <- generateSPCPlot(..., new_parameter = 42)
       expect_equal(result$metadata$backend, "bfhcharts")
       # ... verify behavior ...
     })
   })

   test_that("New parameter works with qicharts2 fallback", {
     with_bfhchart_disabled({
       result <- generateSPCPlot(..., new_parameter = 42)
       # ... verify behavior ...
     })
   })
   ```

### 3.3 Escalating to BFHcharts

**When to escalate:**
- Chart rendering bugs
- Missing chart types
- Statistical calculation errors
- Styling/theming issues

**Process:**
1. Create SPCify issue first (provides user context)
2. Use decision tree: `.claude/ISSUE_ESCALATION_DECISION_TREE.md`
3. Create BFHcharts issue with template
4. Cross-link issues
5. Share test cases

**See:** `docs/CROSS_REPO_COORDINATION.md` for full process

---

## 4. Testing Strategy

### 4.1 Test Types

**Unit Tests:**
```r
# BFHcharts service layer
testthat::test_file('tests/testthat/test-spc-bfh-service.R')

# qicharts2 backend
testthat::test_file('tests/testthat/test-spc-plot-generation-comprehensive.R')
```

**Integration Tests:**
```r
# End-to-end with feature flag control
testthat::test_file('tests/testthat/test-e2e-workflows.R')
```

**Snapshot Tests:**
```r
# Visual regression (shinytest2)
testthat::test_file('tests/testthat/test-bfhcharts-integration.R')
```

### 4.2 Testing Both Backends

**Helper Functions:**
```r
# tests/testthat/helper-backend-testing.R

with_bfhchart_enabled <- function(code) {
  withr::with_options(
    list(golem.app.prod = FALSE),
    {
      withr::with_envvar(
        c(GOLEM_CONFIG_ACTIVE = "development"),
        code
      )
    }
  )
}

with_bfhchart_disabled <- function(code) {
  withr::with_options(
    list(golem.app.prod = FALSE),
    {
      withr::with_envvar(
        c(GOLEM_CONFIG_ACTIVE = "testing_qic_only"),
        code
      )
    }
  )
}
```

**Usage:**
```r
test_that("Feature works with both backends", {
  # Test BFHcharts
  with_bfhchart_enabled({
    result <- generateSPCPlot(...)
    expect_equal(result$metadata$backend, "bfhcharts")
    # ... assertions ...
  })

  # Test qicharts2 fallback
  with_bfhchart_disabled({
    result <- generateSPCPlot(...)
    # ... assertions ...
  })
})
```

### 4.3 Regression Testing

**Before merging changes:**
```bash
# Full test suite
R -e "devtools::test()"

# Performance benchmarks
R -e "source('tests/performance/test-performance.R')"

# Manual smoke test
R -e "GOLEM_CONFIG_ACTIVE=development shiny::runApp('.')"
```

---

## 5. Troubleshooting

### 5.1 Feature Flag Not Working

**Symptom:** BFHcharts backend not used despite enabled flag

**Diagnosis:**
```r
# Check config reading
features_config <- golem::get_golem_options("features")
print(features_config)  # Should show use_bfhchart = TRUE

# Check environment
Sys.getenv("GOLEM_CONFIG_ACTIVE")  # Should be "development" or "production"
```

**Solutions:**
1. Verify `inst/golem-config.yml` syntax
2. Restart R session to reload config
3. Check for typos in config keys
4. Enable debug logging: `options(spc.debug.backend = TRUE)`

### 5.2 Backend Compatibility Issues

**Symptom:** Feature works in qicharts2 but not BFHcharts

**Diagnosis:**
1. Check if chart type is supported:
   ```r
   supported <- c("run", "i", "p", "c", "u")
   chart_type %in% supported
   ```

2. Review parameter mapping in `compute_spc_results_bfh()`

3. Check BFHcharts version:
   ```r
   packageVersion("BFHcharts")  # Should be >= 0.2.0
   ```

**Solutions:**
1. Add to `bfhchart_supported_types` if implemented
2. Update parameter mapping if API changed
3. Escalate to BFHcharts if missing feature

### 5.3 Test Failures After Backend Changes

**Symptom:** Tests pass locally but fail in CI/CD

**Common Causes:**
1. **Version mismatch:** CI uses different BFHcharts version
   - Solution: Pin version in DESCRIPTION

2. **Config not loaded:** GOLEM_CONFIG_ACTIVE not set
   - Solution: Set environment variable in CI config

3. **Snapshot differences:** Visual regression tests
   - Solution: Update snapshots or use tolerance

---

## 6. FAQ

### Q: When should I use BFHcharts vs qicharts2?

**A:** Use BFHcharts for all supported chart types in production. qicharts2 is:
- Fallback for unsupported chart types
- Regression testing baseline
- Debugging reference implementation

### Q: How do I add support for a new chart type?

**A:**
1. Check if BFHcharts implements it
2. If yes: Add to `bfhchart_supported_types` in config
3. If no: Escalate to BFHcharts (see CROSS_REPO_COORDINATION.md)
4. Temporarily: Use qicharts2 fallback

### Q: Do I need to update both backends for every change?

**A:** No. Only update:
- **Both:** If change affects SPCify parameter interface
- **BFHcharts facade only:** If BFHcharts API changed
- **qicharts2 only:** If fixing qicharts2-specific bug
- **Neither:** If change is in UI/data processing layer

### Q: How do I test a change affects both backends correctly?

**A:** Use helper functions:
```r
test_that("Change works with both backends", {
  test_with_both_backends({
    result <- generateSPCPlot(...)
    expect_true(some_condition(result))
  })
})
```

See `tests/testthat/helper-backend-testing.R` for utilities.

### Q: What if BFHcharts has a bug?

**A:**
1. Create SPCify issue with user impact
2. Escalate to BFHcharts (use template)
3. Implement workaround in SPCify facade if critical
4. Mark workaround with TODO and issue reference
5. Remove workaround after BFHcharts fix released

See: `docs/CROSS_REPO_COORDINATION.md` Section "Hotfix Coordination"

---

## 7. Resources

### Documentation
- [CROSS_REPO_COORDINATION.md](CROSS_REPO_COORDINATION.md) - Full cross-repo guide
- [LEGACY_CODE_CLEANUP.md](LEGACY_CODE_CLEANUP.md) - Migration cleanup plan
- [CLAUDE.md](../CLAUDE.md) - Main development guide

### Code
- `R/fct_spc_bfh_service.R` - BFHcharts backend
- `R/fct_spc_plot_generation.R` - Wrapper + qicharts2 backend
- `tests/testthat/helper-backend-testing.R` - Test utilities

### External
- [BFHcharts Repository](https://github.com/user/BFHcharts)
- [BFHtheme Repository](https://github.com/user/BFHtheme)
- [qicharts2 Documentation](https://github.com/anhoej/qicharts2)

---

**Document History:**
- **v1.0.0** (2025-10-17): Initial version for Task 035
- **Next review:** After BFHcharts 0.3.0 release
```

**Validation:**
- [ ] All code examples tested
- [ ] Links work
- [ ] FAQ addresses common scenarios
- [ ] Consistent with CLAUDE.md guidance

#### B4: Document BFHcharts Limitations (1 hour)

**New File:** `docs/BFHCHARTS_LIMITATIONS.md`

**Content:**

```markdown
# BFHcharts Known Limitations and Workarounds

**Last Updated:** 2025-10-17
**BFHcharts Version:** 0.2.x
**SPCify Version:** epic/bfhcharts-spc-migration branch

---

## Chart Type Support

### Supported Chart Types
- ✅ Run chart (`run`)
- ✅ I chart (`i`) - Individuals
- ✅ P chart (`p`) - Proportions
- ✅ C chart (`c`) - Counts
- ✅ U chart (`u`) - Rates

### Unsupported Chart Types
- ❌ MR chart (`mr`) - Moving Range
- ❌ Xbar chart (`xbar`) - Mean
- ❌ S chart (`s`) - Standard Deviation
- ❌ R chart (`r`) - Range
- ❌ T chart (`t`) - Time Between Events

**Workaround:** SPCify automatically falls back to qicharts2 for unsupported types.

**Escalation:** Track BFHcharts chart type roadmap in BFHcharts repository.

---

## Parameter Differences

### Centerline Parameter

**Issue:** BFHcharts uses `cl` instead of `centerline_value`

**Impact:** Parameter mapping required in SPCify facade layer

**Workaround:**
```r
# SPCify accepts both names, maps internally
generateSPCPlot(..., centerline_value = 50)
# → BFHcharts receives cl = 50

# In R/fct_spc_bfh_service.R
qic_args$cl <- centerline_value  # Mapping
```

**Resolution:** Consider standardizing parameter names in BFHcharts 1.0.0

---

## Known Issues

### 1. Target Line Rendering (BFHcharts < 0.3.0)

**Issue:** Target lines may not render correctly in some chart types

**Affected Versions:** BFHcharts < 0.3.0

**Workaround:** Ensure BFHcharts >= 0.3.0 in DESCRIPTION

**Status:** Fixed in BFHcharts 0.3.0 (upcoming)

**References:**
- BFHcharts Issue #XX
- SPCify Issue #YY

### 2. Extended Centerline Support

**Issue:** Extended centerlines (20% beyond last data point) use hardcoded logic

**Impact:** Less flexible than qicharts2 implementation

**Workaround:** Implemented in `BFHcharts::add_plot_enhancements()`

**Status:** Working as intended, may add configuration in future

---

## Performance Considerations

### BFHcharts vs qicharts2 Performance

**Benchmarks (1000 data points):**
- BFHcharts: ~450ms (i-chart)
- qicharts2: ~380ms (i-chart)

**Overhead:** ~15-20% slower than qicharts2 (acceptable tradeoff for maintainability)

**Optimization Opportunities:**
- Vectorized calculations (BFHcharts side)
- Caching improvements (SPCify side)

---

## Dependency Management

### Version Constraints

**Current DESCRIPTION:**
```r
Imports:
    BFHcharts (>= 0.2.0)
```

**Recommendation:** Pin to minor version for stability
```r
Imports:
    BFHcharts (>= 0.2.0, < 0.3.0)
```

**Update Policy:**
- Test new BFHcharts versions in dev environment
- Update constraint after validation
- Coordinate major version upgrades

---

## Testing Gaps

### Limited Test Coverage Areas

1. **Edge Cases:**
   - Very large datasets (>10,000 points)
   - Missing data handling differences
   - Character encoding edge cases

2. **Visual Regression:**
   - Snapshot tests exist but limited coverage
   - Manual visual inspection recommended for major updates

**Action Items:**
- [ ] Expand shinytest2 snapshot coverage
- [ ] Add performance regression tests
- [ ] Document visual QA checklist

---

## Workarounds in SPCify

### Temporary Workarounds

**Location:** `R/fct_spc_bfh_service.R`

1. **Parameter Mapping:**
   - Lines 354-406: centerline_value → cl mapping

2. **Data Preprocessing:**
   - Additional data cleaning for BFHcharts compatibility

3. **Error Handling:**
   - Graceful fallback to qicharts2 on BFHcharts errors

**TODO:** Remove workarounds as BFHcharts matures

---

## Escalation Process

**When to escalate to BFHcharts:**
1. Core rendering bugs
2. Statistical calculation errors
3. Missing features that block users
4. Performance degradation

**Process:** See `docs/CROSS_REPO_COORDINATION.md`

---

## Future Roadmap

### Planned Improvements (BFHcharts)
- [ ] Additional chart types (mr, xbar, s, r, t)
- [ ] Performance optimizations
- [ ] Enhanced customization options
- [ ] Improved documentation

### SPCify Integration Plans
- [ ] Remove qicharts2 fallback (BFHcharts 1.0.0+)
- [ ] Full test parity between backends
- [ ] Performance benchmarking suite
- [ ] Automated compatibility testing

---

**References:**
- [BFHcharts Repository](https://github.com/user/BFHcharts)
- [Cross-Repo Coordination Guide](CROSS_REPO_COORDINATION.md)
- [SPCify CLAUDE.md](../CLAUDE.md)
```

**Validation:**
- [ ] Limitations accurate and up-to-date
- [ ] Workarounds documented with code locations
- [ ] Escalation process clear

---

### 3.3 Stream C: Test Cleanup & Validation

#### C1: Review and Migrate Temporary Test Files (2-3 hours)

**Location:** Root directory (19 files)

**Process:**

1. **Categorize each file:**
   ```bash
   # Create inventory
   cd /path/to/epic-bfhcharts-spc-migration
   for file in test_*.R verify_*.R; do
     echo "=== $file ===" >> test_inventory.txt
     head -20 "$file" >> test_inventory.txt
     echo "" >> test_inventory.txt
   done
   ```

2. **Decision Matrix:**

| File | Purpose | Action |
|------|---------|--------|
| `test_backend_switch*.R` | Backend switching tests | → Move to `tests/testthat/test-backend-switching.R` |
| `test_bfh_*.R` | BFHcharts specific tests | → Merge into `test-spc-bfh-service.R` |
| `test_bug1_bug4_*.R` | Bug reproduction | → Document in `tests/MANUAL_TEST_GUIDE.md`, delete scripts |
| `test_qicharts2_minimal.R` | qicharts2 regression | → Move to `tests/testthat/test-qicharts2-regression.R` |
| `verify_*.R` | Manual verification | → Document in `MANUAL_TEST_GUIDE.md`, delete scripts |

3. **Move valuable tests:**
   ```r
   # Extract useful test code from temp files
   # Add to appropriate testthat files
   # Add roxygen2 documentation
   ```

4. **Update `tests/MANUAL_TEST_GUIDE.md`:**
   ```markdown
   ## BFHcharts Backend Verification

   ### Manual Test: Backend Switching

   **Purpose:** Verify feature flag correctly routes to backends

   **Steps:**
   1. Start app with BFHcharts enabled
   2. Generate I-chart
   3. Check logs for "[BACKEND_WRAPPER] Using BFHcharts backend"
   4. Restart with `use_bfhchart: false`
   5. Generate same chart
   6. Verify qicharts2 used

   **Expected:** Seamless switching, identical output

   **Original script:** `test_backend_switch.R` (archived)
   ```

5. **Delete obsolete files:**
   ```bash
   git rm test_*.R verify_*.R
   git commit -m "cleanup: remove temporary development test scripts

   Migrated valuable tests to tests/testthat/
   Documented manual verification procedures in MANUAL_TEST_GUIDE.md

   Removed 19 temporary test files created during BFHcharts migration."
   ```

**Expected Outcome:**
- Zero root-level test scripts
- Valuable tests preserved in testthat suite
- Manual tests documented

#### C2: Resolve TODO/FIXME Comments (1 hour)

**Files with TODOs:** Only 3 files

1. **`R/fct_spc_bfh_service.R`:**
   - Review TODOs
   - Create GitHub issues for unresolved items
   - Update comments to reference issues
   - Remove resolved TODOs

2. **`tests/testthat/test-phase2-device-size-matrix.R`:**
   - Evaluate if TODOs still relevant
   - Fix or document

3. **`R/utils_server_server_management.R`:**
   - Review and resolve

**Process:**
```bash
# Find all TODOs
grep -rn "TODO\|FIXME\|XXX\|HACK" R/ tests/ --exclude-dir=.git

# For each TODO:
1. Assess if still needed
2. If yes: Create GitHub issue, reference in comment
3. If no: Remove comment
4. If done: Remove comment
```

**Expected Outcome:**
- Zero untracked TODOs
- All TODOs linked to GitHub issues
- Technical debt visible in issue tracker

#### C3: Validate Test Suite (1-2 hours)

**Full Test Run:**
```bash
# Run all tests
R -e "devtools::test()"

# Run with coverage
R -e "covr::package_coverage()"

# Performance tests
R -e "source('tests/performance/test-performance.R')"

# Integration tests
R -e "testthat::test_dir('tests/integration')"
```

**Validation Checklist:**
- [ ] All unit tests pass
- [ ] All integration tests pass
- [ ] Shinytest2 snapshots current
- [ ] Coverage >= 90% overall
- [ ] Coverage 100% on critical paths (plot generation, state management)
- [ ] No failing benchmarks
- [ ] Zero test warnings

**Fix Issues:**
- Update snapshots if visual changes expected
- Fix failing tests
- Add missing test coverage
- Document known test limitations

**Expected Outcome:**
- Clean test run
- Coverage targets met
- No regressions

---

### 3.4 Stream D: Final Integration Testing

#### D1: Manual Smoke Testing (1-2 hours)

**Test Scenarios:**

1. **Scenario A: BFHcharts Backend (Full Flow)**
   ```bash
   # Start app with BFHcharts enabled
   GOLEM_CONFIG_ACTIVE=development R -e "shiny::runApp('.')"
   ```

   **Steps:**
   - [ ] Upload CSV file (dansk format)
   - [ ] Auto-detection runs successfully
   - [ ] Generate I-chart (verify BFHcharts used via logs)
   - [ ] Add target line
   - [ ] Add centerline
   - [ ] Toggle freeze/shift
   - [ ] Download Excel export
   - [ ] Verify plot quality

2. **Scenario B: qicharts2 Fallback**
   ```bash
   # Disable BFHcharts
   # Edit inst/golem-config.yml: use_bfhchart: false
   GOLEM_CONFIG_ACTIVE=development R -e "shiny::runApp('.')"
   ```

   **Steps:**
   - [ ] Repeat Scenario A steps
   - [ ] Verify qicharts2 used (check logs)
   - [ ] Confirm feature parity

3. **Scenario C: Unsupported Chart Type**
   ```bash
   # BFHcharts enabled
   GOLEM_CONFIG_ACTIVE=development R -e "shiny::runApp('.')"
   ```

   **Steps:**
   - [ ] Select MR chart (unsupported by BFHcharts)
   - [ ] Verify automatic fallback to qicharts2
   - [ ] Check logs for fallback message
   - [ ] Confirm plot renders correctly

4. **Scenario D: Performance**
   - [ ] Upload large dataset (>1000 rows)
   - [ ] Measure plot generation time
   - [ ] Verify < 2 seconds for 1000 rows
   - [ ] Check memory usage < 100MB

**Document Results:**
- Create `docs/SMOKE_TEST_RESULTS_20251017.md`
- Note any issues or unexpected behavior
- Attach screenshots if visual differences

#### D2: Cross-Browser Testing (1 hour)

**Browsers to Test:**
- [ ] Chrome (latest)
- [ ] Firefox (latest)
- [ ] Safari (if Mac)
- [ ] Edge (Windows)

**Test Matrix:**
- Upload file
- Generate plot
- Export functionality
- Responsive layout

**Expected Outcome:**
- Consistent behavior across browsers
- No JavaScript errors
- Plots render identically

#### D3: Final Package Check (1 hour)

```bash
# Comprehensive R CMD check
R -e "devtools::check()"

# Expected: 0 errors, 0 warnings, 0 notes
```

**Fix Any Issues:**
- Update documentation if needed
- Fix namespace issues
- Resolve dependency warnings

**Build Package:**
```bash
R -e "devtools::build()"
```

**Install and Test:**
```bash
R -e "install.packages('SPCify_0.9.0.tar.gz', repos = NULL, type = 'source')"
R -e "library(SPCify); SPCify::run_app()"
```

**Expected Outcome:**
- Clean package check
- Successful build
- Package installs without errors

---

## 4. Success Criteria

### 4.1 Code Cleanliness
- [ ] `add_plot_enhancements()` function removed (lines 461-594)
- [ ] `applyHospitalTheme()` function removed (lines 1389-1438)
- [ ] `generateSPCPlot_qicharts2()` marked as deprecated
- [ ] `candidates_for_deletion/` directory cleaned
- [ ] Zero root-level `test_*.R` or `verify_*.R` files
- [ ] All TODO/FIXME comments tracked in GitHub issues

### 4.2 Documentation Quality
- [ ] `CLAUDE.md` updated with BFHcharts integration notes
- [ ] `README.md` reflects HYBRID backend architecture
- [ ] `docs/BFHCHARTS_MIGRATION_GUIDE.md` created and comprehensive
- [ ] `docs/BFHCHARTS_LIMITATIONS.md` documents known issues
- [ ] All code examples tested and working
- [ ] Links between docs validated

### 4.3 Test Coverage
- [ ] All tests pass (unit + integration + shinytest2)
- [ ] Test coverage >= 90% overall
- [ ] Test coverage 100% on critical paths
- [ ] Temporary test files migrated or documented
- [ ] No test warnings or deprecations

### 4.4 Integration Quality
- [ ] Manual smoke tests pass (BFHcharts + qicharts2 paths)
- [ ] Cross-browser testing complete
- [ ] `devtools::check()` passes with 0 errors/warnings/notes
- [ ] Package builds successfully
- [ ] Performance benchmarks within targets

### 4.5 Production Readiness
- [ ] Feature flag system working
- [ ] Graceful fallback to qicharts2
- [ ] Error handling robust
- [ ] Logging provides actionable debugging info
- [ ] No known critical bugs

---

## 5. Implementation Steps

### Phase 1: Preparation (30 min)
1. Create feature branch: `task/035-cleanup-documentation`
2. Review all analysis findings
3. Set up tracking spreadsheet for checklist items

### Phase 2: Legacy Code Removal (4-6 hours)
1. Execute Stream A1: Remove `add_plot_enhancements()`
2. Execute Stream A2: Remove `applyHospitalTheme()`
3. Execute Stream A3: Deprecate `generateSPCPlot_qicharts2()`
4. Execute Stream A4: Clean candidates_for_deletion
5. Run tests after each removal
6. Commit atomically with clear messages

### Phase 3: Documentation (6-8 hours)
1. Execute Stream B1: Update CLAUDE.md
2. Execute Stream B2: Update README.md
3. Execute Stream B3: Create migration guide
4. Execute Stream B4: Document limitations
5. Review all docs for consistency
6. Have someone else review docs

### Phase 4: Test Cleanup (4-5 hours)
1. Execute Stream C1: Migrate temporary tests
2. Execute Stream C2: Resolve TODOs
3. Execute Stream C3: Validate test suite
4. Fix any test failures
5. Generate coverage report

### Phase 5: Integration Testing (3-4 hours)
1. Execute Stream D1: Manual smoke testing
2. Execute Stream D2: Cross-browser testing
3. Execute Stream D3: Package check
4. Document any issues found
5. Fix critical issues

### Phase 6: Finalization (1 hour)
1. Review all success criteria
2. Create summary commit message
3. Prepare pull request description
4. Request code review

---

## 6. Risks and Mitigations

### Risk 1: Breaking Changes During Cleanup
**Probability:** Medium
**Impact:** High
**Mitigation:**
- Run tests after each code removal
- Keep commits atomic for easy rollback
- Test both backend paths manually
- Maintain qicharts2 fallback

### Risk 2: Documentation Gaps
**Probability:** Low
**Impact:** Medium
**Mitigation:**
- Have documentation reviewed by another developer
- Test all code examples
- Validate links
- Get user feedback on clarity

### Risk 3: Test Failures in CI
**Probability:** Medium
**Impact:** Medium
**Mitigation:**
- Run full test suite locally before push
- Check CI environment matches local
- Pin dependency versions
- Update snapshots if needed

### Risk 4: Missed Legacy References
**Probability:** Medium
**Impact:** Medium
**Mitigation:**
- Use comprehensive grep searches
- Check NAMESPACE imports
- Review git grep for old function names
- Test package build and installation

---

## 7. Rollback Plan

If critical issues found during cleanup:

1. **Immediate Rollback:**
   ```bash
   git revert <commit-hash>
   ```

2. **Partial Rollback:**
   - Revert specific file changes
   - Keep documentation updates
   - Re-add removed functions with deprecation warnings

3. **Communication:**
   - Document issue in GitHub
   - Update team on rollback status
   - Create plan for re-attempt

---

## 8. Post-Completion Tasks

### 8.1 Merge Strategy
1. Squash commits for clean history
2. Write comprehensive PR description
3. Request review from maintainer
4. Address review feedback
5. Merge to `epic/bfhcharts-spc-migration` branch

### 8.2 Validation After Merge
- [ ] CI/CD passes
- [ ] No new warnings introduced
- [ ] Documentation renders correctly
- [ ] Package builds on clean system

### 8.3 Communication
- [ ] Update epic tracking with Task 035 completion
- [ ] Notify team of documentation updates
- [ ] Share migration guide with contributors
- [ ] Update project README with epic completion status

---

## 9. Time Estimates Summary

| Stream | Description | Estimated Time |
|--------|-------------|----------------|
| **A** | Legacy Code Removal | 4-6 hours |
| **B** | Documentation Updates | 6-8 hours |
| **C** | Test Cleanup & Validation | 4-5 hours |
| **D** | Final Integration Testing | 3-4 hours |
| **Total** | All Streams | **17-23 hours** |

**Buffer:** 20% (3-5 hours) for unexpected issues

**Total with Buffer:** 20-28 hours (2.5-3.5 days)

---

## 10. Notes and Observations

### 10.1 Positive Findings
- ✅ Cross-repo coordination infrastructure already in place
- ✅ Migration work (Tasks 031-033) appears complete
- ✅ HYBRID backend architecture well-designed
- ✅ Comprehensive documentation already exists (CROSS_REPO_COORDINATION.md)
- ✅ Feature flag system functional

### 10.2 Concerns
- ⚠️ Legacy functions commented as "Moved to BFHcharts" but bodies still exist
- ⚠️ 19 temporary test files in root (need cleanup)
- ⚠️ Only 6 references to `BFHcharts::` (may indicate incomplete migration)
- ⚠️ 63 files still reference qicharts2 (expected, but validate)

### 10.3 Recommendations
1. **Prioritize Stream A (Legacy Code Removal)** - High impact, clear ROI
2. **Invest in Stream B (Documentation)** - Critical for maintainability
3. **Automate Stream C (Test Cleanup)** - Create script to categorize temp files
4. **Don't rush Stream D (Integration Testing)** - Quality gate before production

---

## Appendix A: File Inventory

### Root-Level Temporary Test Files
```
test_backend_switch.R
test_backend_switching_simple.R
test_backend_switching.R
test_bfh_direct.R
test_bfh_sanitized.R
test_bfh_simple.R
test_bug1_bug4_complete.R
test_bug1_bug4_minimal.R
test_bug1_bug4_simple.R
test_freeze_phase_detection.R
test_numeric_data_flow.R
test_qicharts2_minimal.R
test_service_basic.R
test_service_comprehensive.R
test_target_y_axis.R
verify_bfhcharts_fix.R
verify_nse_fix.R
verify_part_fix.R
verify_target_line.R
```

### Legacy Functions to Remove
```
R/fct_spc_plot_generation.R:
  - add_plot_enhancements() (lines 461-594)
  - applyHospitalTheme() (lines 1389-1438)
```

### Documentation to Create
```
docs/BFHCHARTS_MIGRATION_GUIDE.md (NEW)
docs/BFHCHARTS_LIMITATIONS.md (NEW)
```

### Documentation to Update
```
CLAUDE.md (Section 5.3 + new Appendix F)
README.md (Features, Architecture, Dependencies sections)
```

---

## Appendix B: Grep Search Commands

**Find legacy function calls:**
```bash
grep -rn "add_plot_enhancements\|applyHospitalTheme" R/ tests/ --exclude-dir=.git
```

**Find qicharts2 references:**
```bash
grep -rn "qicharts2::\|generateSPCPlot_qicharts2" R/ tests/ --exclude-dir=.git
```

**Find BFHcharts references:**
```bash
grep -rn "BFHcharts::\|bfhchart::" R/ tests/ --exclude-dir=.git
```

**Find TODO/FIXME comments:**
```bash
grep -rn "TODO\|FIXME\|XXX\|HACK" R/ tests/ --exclude-dir=.git
```

**Find temporary test files:**
```bash
find . -maxdepth 1 -name "test_*.R" -o -name "verify_*.R"
```

---

## Appendix C: Test Commands

**Full test suite:**
```bash
R -e "devtools::test()"
```

**Coverage report:**
```bash
R -e "source('tests/coverage.R'); run_coverage_report()"
```

**Package check:**
```bash
R -e "devtools::check()"
```

**Manual app start (BFHcharts enabled):**
```bash
GOLEM_CONFIG_ACTIVE=development R -e "shiny::runApp('.')"
```

**Manual app start (qicharts2 only):**
```bash
# Edit inst/golem-config.yml: use_bfhchart: false
GOLEM_CONFIG_ACTIVE=development R -e "shiny::runApp('.')"
```

---

**Document Version:** 1.0.0
**Created:** 2025-10-17
**Author:** Task 035 Analysis
**Next Review:** After implementation completion
