# Test Refactoring Plan
# Generated: 2025-10-10
# Task 3.9: Existing Test Refactoring Analysis

## Executive Summary

This document identifies test file overlaps, consolidation opportunities, and refactoring priorities for the SPCify test suite (115 test files total).

## Key Findings

### 1. Auto-Detection Tests (6 files - HIGH OVERLAP)

**Files:**
- `test-auto-detection.R` (legacy, basic tests)
- `test-autodetect-algorithms.R` (algorithm-specific)
- `test-autodetect-core.R` (core functionality)
- `test-autodetect-engine.R` (✅ NEW, Week 7-8, comprehensive)
- `test-autodetect-tidyverse-integration.R` (tidyverse patterns)
- `test-autodetect-unified-comprehensive.R` (comprehensive suite)

**Recommendation:**
- **KEEP**: `test-autodetect-engine.R` (most comprehensive, modern describe/it)
- **CONSOLIDATE**: Merge `test-auto-detection.R` → `test-autodetect-engine.R`
- **KEEP**: `test-autodetect-algorithms.R` (algorithm-specific edge cases)
- **KEEP**: `test-autodetect-tidyverse-integration.R` (tidyverse-specific)
- **DEPRECATE**: `test-autodetect-core.R` (superseded by engine tests)
- **DEPRECATE**: `test-autodetect-unified-comprehensive.R` (superseded)

**Priority**: HIGH (6 files → 3 files)

### 2. File Operation Tests (5 files - MEDIUM OVERLAP)

**Files:**
- `test-file-io-comprehensive.R`
- `test-file-operations.R` (✅ NEW, Week 7-8, comprehensive)
- `test-file-operations-tidyverse.R`
- `test-file-upload.R`
- `test-csv-parsing.R`

**Recommendation:**
- **KEEP**: `test-file-operations.R` (most comprehensive, modern)
- **KEEP**: `test-csv-parsing.R` (CSV-specific edge cases)
- **CONSOLIDATE**: Merge `test-file-upload.R` → `test-file-operations.R`
- **KEEP**: `test-file-operations-tidyverse.R` (tidyverse-specific)
- **DEPRECATE**: `test-file-io-comprehensive.R` (superseded)

**Priority**: MEDIUM (5 files → 3 files)

### 3. Plot Generation Tests (8+ files - HIGH OVERLAP)

**Files:**
- `test-plot-core.R`
- `test-plot-generation.R`
- `test-plot-generation-performance.R`
- `test-generateSPCPlot-comprehensive.R` (✅ NEW, Week 5-6)
- `test-spc-plot-generation-comprehensive.R`
- `test-plot-diff.R`
- `test-qic-calculations.R`
- `test-qicharts2-integration.R`

**Recommendation:**
- **KEEP**: `test-generateSPCPlot-comprehensive.R` (most comprehensive)
- **KEEP**: `test-plot-generation-performance.R` (performance-specific)
- **KEEP**: `test-qicharts2-integration.R` (integration-specific)
- **CONSOLIDATE**: Merge `test-plot-core.R` + `test-plot-generation.R` → comprehensive
- **DEPRECATE**: `test-spc-plot-generation-comprehensive.R` (duplicate)
- **KEEP**: `test-plot-diff.R` (diff-specific functionality)
- **KEEP**: `test-qic-calculations.R` (calculation-specific)

**Priority**: HIGH (8 files → 5 files)

### 4. Event System Tests (5 files - LOW OVERLAP)

**Files:**
- `test-event-bus-full-chain.R` (✅ NEW, Week 7-8)
- `test-event-driven-reactive.R`
- `test-event-listener-integration.R` (✅ NEW, Week 5-6)
- `test-event-system-emit.R`
- `test-event-system-observers.R`

**Recommendation:**
- **KEEP ALL** - Each has distinct focus
- **MINOR**: Add cross-references between files
- **MINOR**: Ensure no test duplication

**Priority**: LOW (5 files → 5 files, minimal changes)

### 5. Cache Tests (6 files - MEDIUM OVERLAP)

**Files:**
- `test-cache-collision-fix.R`
- `test-cache-data-signature-bugs.R`
- `test-cache-invalidation-sprint3.R`
- `test-cache-reactive-lazy-evaluation.R`
- `test-phase0-cache-isolation.R`
- `test-qic-cache-metrics.R`
- `test-qic-cache-smart-invalidation.R`

**Recommendation:**
- **CONSOLIDATE**: Create `test-cache-comprehensive.R`
- Merge: collision + signature bugs + invalidation
- **KEEP**: `test-qic-cache-metrics.R` (metrics-specific)
- **KEEP**: `test-cache-reactive-lazy-evaluation.R` (reactive-specific)

**Priority**: MEDIUM (7 files → 4 files)

### 6. Critical Fixes Tests (4 files - HIGH DUPLICATION)

**Files:**
- `test-critical-fixes.R`
- `test-critical-fixes-integration.R`
- `test-critical-fixes-regression.R`
- `test-critical-fixes-security.R`

**Recommendation:**
- **CONSOLIDATE**: Merge all into `test-critical-fixes-comprehensive.R`
- Organize by category: integration, regression, security
- Use describe/it structure

**Priority**: HIGH (4 files → 1 file)

### 7. Label Placement Tests (6 files - LOW OVERLAP)

**Files:**
- `test-first-render-label-placement.R`
- `test-label-formatting.R`
- `test-label-height-cache-invalidation.R`
- `test-label-height-estimation.R`
- `test-label-placement-bounds.R`
- `test-label-placement-core.R`
- `test-label-size-safeguard.R`

**Recommendation:**
- **CONSOLIDATE**: Create `test-label-placement-comprehensive.R`
- Organize by: core, caching, bounds, formatting
- **KEEP**: Individual files are focused, but consolidation improves maintainability

**Priority**: LOW (7 files → 1-2 files, but not urgent)

### 8. Fase/Phase Tests (8 files - SEQUENTIAL, LOW OVERLAP)

**Files:**
- `test-fase1-refactoring.R`
- `test-fase2-reactive-chains.R`
- `test-fase3-event-driven-state-machine.R`
- `test-fase4-intelligent-heuristics.R`
- `test-phase0-cache-isolation.R`
- `test-phase1-first-run-effect.R`
- `test-phase1-grob-measurement-stability.R`
- `test-phase1-qic-reproducibility.R`
- `test-phase2-device-size-matrix.R`
- `test-phase3-test-mode-optimization.R`

**Recommendation:**
- **KEEP ALL** - Historical/sequential tests document evolution
- **MINOR**: Add deprecation notices if superseded
- **OPTIONAL**: Archive to `tests/testthat/archive/` if no longer relevant

**Priority**: LOW (keep as-is or archive)

### 9. Logging Tests (3 files - LOW OVERLAP)

**Files:**
- `test-logging-debug-cat.R`
- `test-logging-standardization.R`
- `test-logging-system.R`

**Recommendation:**
- **CONSOLIDATE**: Merge into `test-logging-comprehensive.R`
- Sections: system, standardization, debug-cat deprecation

**Priority**: LOW (3 files → 1 file)

### 10. Smaller Categories

**UI Sync (2 files):**
- `test-comprehensive-ui-sync.R`
- `test-ui-synchronization.R`
→ **CONSOLIDATE** to one file

**Y-Axis (3 files):**
- `test-y-axis-mapping.R`
- `test-y-axis-model.R`
- `test-y-axis-scaling-overhaul.R`
→ **KEEP** separate (distinct aspects)

**E2E Workflows (2 files):**
- `test-e2e-user-workflows.R`
- `test-e2e-workflows.R`
→ **CONSOLIDATE** to one file

## Immediate Actions (Week 9-10)

### High Priority Consolidations

1. **Critical Fixes** (30 min)
   - Merge 4 files → 1 comprehensive file
   - Files: `test-critical-fixes*.R` → `test-critical-fixes-comprehensive.R`

2. **Auto-Detection** (45 min)
   - Deprecate 2 legacy files
   - Consolidate basic tests into engine
   - Files: `test-auto-detection.R`, `test-autodetect-core.R`, `test-autodetect-unified-comprehensive.R`

3. **Plot Generation** (45 min)
   - Deprecate duplicate comprehensive file
   - Consolidate core + generation
   - Files: `test-plot-core.R`, `test-spc-plot-generation-comprehensive.R`

4. **UI Sync** (15 min)
   - Merge 2 files
   - Files: `test-comprehensive-ui-sync.R`, `test-ui-synchronization.R`

5. **E2E Workflows** (15 min)
   - Merge 2 files
   - Files: `test-e2e-user-workflows.R`, `test-e2e-workflows.R`

### Medium Priority (Week 11-12)

6. **Cache Tests** (1h)
   - Consolidate 7 files → 4 files

7. **File Operations** (30 min)
   - Consolidate 5 files → 3 files

8. **Logging** (30 min)
   - Consolidate 3 files → 1 file

## Refactoring Guidelines

### 1. Preserve All Tests
- Never delete tests during consolidation
- Move tests to new files, don't remove

### 2. Use describe/it Structure
```r
describe("Feature Name", {
  it("does specific thing", {
    # Test code
  })
})
```

### 3. Add Skip Guards
```r
skip_if_not(exists("function_name", mode = "function"))
```

### 4. Consolidation Pattern
```r
# test-feature-comprehensive.R
# ==============================================================================
# COMPREHENSIVE TEST SUITE: Feature Name
# ==============================================================================
#
# CONSOLIDATED FROM:
#   - test-feature-old.R (deprecated YYYY-MM-DD)
#   - test-feature-legacy.R (deprecated YYYY-MM-DD)
#
# COVERAGE AREAS:
#   1. Core functionality
#   2. Edge cases
#   3. Integration
# ==============================================================================

library(testthat)

# CORE FUNCTIONALITY ===========================================================
describe("Core Functionality", {
  it("...", {})
})

# EDGE CASES ===================================================================
describe("Edge Cases", {
  it("...", {})
})
```

### 5. Deprecation Pattern
Add to top of deprecated file:
```r
# DEPRECATED: YYYY-MM-DD
# This test file has been consolidated into test-feature-comprehensive.R
# This file is kept for reference but will be removed in next major cleanup
```

## Estimated Time Investment

### Week 9-10 (Immediate)
- High priority consolidations: **2.5h**
- Documentation and validation: **1h**
- **Total: 3.5h**

### Week 11-12 (Follow-up)
- Medium priority consolidations: **2h**
- Archive old fase/phase tests: **0.5h**
- **Total: 2.5h**

### Total Refactoring: **6h**

## Success Metrics

### Before Refactoring
- **115 test files**
- Overlap: ~25-30%
- Unclear test organization
- Mix of old/new patterns

### After Refactoring (Target)
- **~85-90 test files** (25% reduction)
- Clear test organization
- Consistent describe/it patterns
- Comprehensive coverage maintained
- Improved maintainability

## Implementation Strategy

### Phase 1: Week 9-10 (Immediate - This Week)
1. Create comprehensive files for high-priority items
2. Move tests (copy-paste, don't delete originals yet)
3. Add deprecation notices to old files
4. Run full test suite to verify no breakage
5. Commit consolidated files first
6. Keep deprecated files for one release cycle

### Phase 2: Week 11-12 (Follow-up)
1. Medium priority consolidations
2. Archive fase/phase tests if agreed
3. Final validation

### Phase 3: Week 13-14 (Cleanup)
1. Remove deprecated files after validation period
2. Update test documentation
3. Final test suite optimization

## Notes

- All refactoring follows TDD principles
- No functionality changes during refactoring
- Comprehensive test coverage maintained at ≥90%
- All changes tracked in git with clear commit messages
- Deprecated files kept for 1-2 releases before removal
