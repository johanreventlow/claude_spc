# Legacy Visualization Code Cleanup Plan

**Created:** 2025-10-17
**Status:** Planning
**Related Issue:** #49

## Executive Summary

SPCify's `epic/bfhcharts-spc-migration` branch contains significant duplicate visualization code that overlaps with BFHcharts and BFHtheme packages. This document outlines the cleanup strategy and migration plan.

**Code Reduction Target:** 1,482 → ~400 lines in `R/fct_spc_plot_generation.R`
**Functions to Remove:** 2 (add_plot_enhancements, applyHospitalTheme)
**Functions to Deprecate:** 1 (generateSPCPlot_qicharts2)

---

## Part 1: Duplicate Code Analysis

### 1.1 Extended Lines & Comment Annotations

**Status:** ✅ **EXACT DUPLICATE** - Remove from SPCify

#### SPCify Implementation
- **File:** `R/fct_spc_plot_generation.R`
- **Function:** `add_plot_enhancements()` (linje 461-594)
- **Lines of Code:** 133
- **Responsibilities:**
  - Extended CL (centerline) lines 20% beyond last data point (linje 517-525)
  - Extended Target lines 20% beyond last data point (linje 533-540)
  - Comment annotations with ggrepel (linje 574-587)

#### BFHcharts Implementation
- **File:** `R/plot_enhancements.R`
- **Function:** `add_plot_enhancements()` (linje 27-148)
- **Lines of Code:** 121
- **Identical Logic:**
  - Same 20% extension calculation (linje 34-51 vs SPCify 472-491)
  - Same centerline logic (linje 56-81 vs SPCify 497-525)
  - Same target line logic (linje 83-93 vs SPCify 529-540)
  - Same ggrepel comment annotation (linje 130-145 vs SPCify 574-587)

#### Key Differences
| Aspect | SPCify | BFHcharts | Impact |
|--------|--------|-----------|--------|
| Hospital Colors | Hardcoded `hospital_colors$hospitalblue` | `BFHtheme::bfh_cols("hospital_blue")` | ⚠️ SPCify outdated |
| Grey Color | Hardcoded `"#565656"` | `BFHtheme::bfh_cols("hospital_dark_grey")` | ⚠️ SPCify outdated |
| Color Management | Custom function | Centralized BFHtheme API | ✅ BFHcharts better |

#### Migration Action
```r
# BEFORE (SPCify):
plot <- add_plot_enhancements(plot, qic_data, comment_data, y_axis_unit="count")

# AFTER (Using BFHcharts):
plot <- BFHcharts::add_plot_enhancements(plot, qic_data, comment_data)
```

**Decision:** ❌ **REMOVE** SPCify's `add_plot_enhancements()` → use `BFHcharts::add_plot_enhancements()`

---

### 1.2 Hospital Theme Styling

**Status:** ✅ **OBSOLETE** - Replace with BFHtheme + BFHcharts

#### SPCify Implementation
- **File:** `R/fct_spc_plot_generation.R`
- **Function:** `applyHospitalTheme()` (linje 1389-1438)
- **Lines of Code:** 50
- **Features:**
  - Base: `ggplot2::theme_minimal(base_size = base_size)`
  - Font: Hardcoded `family = "Roboto Medium"`
  - Panel styling: Grid blank, background white
  - Coordinates: `lemon::coord_capped_cart(bottom = "right", gap = 0)`

#### BFHtheme Implementation (Better Design)
- **File:** `R/themes.R`
- **Function:** `theme_bfh()` (linje 42-116)
- **Advantages:**
  - Auto-detects best available font via `get_bfh_font()` (not hardcoded)
  - Centralized color management via `bfh_cols()`
  - Extensive customization (5 theme variants available)
  - Proper marquee typography support

#### BFHcharts SPC-Specific Wrapper
- **File:** `R/themes.R`
- **Function:** `apply_spc_theme()` (linje 30-35)
- **Purpose:** Applies `BFHtheme::theme_bfh()` + SPC-specific adjustments
- **Includes:** Capped coordinates for SPC charts

#### Migration Decision
```r
# BEFORE (SPCify):
plot <- applyHospitalTheme(plot, base_size = 14)

# AFTER (Using BFHcharts):
plot <- BFHcharts::apply_spc_theme(plot, base_size = 14)
```

**Decision:** ❌ **REMOVE** SPCify's `applyHospitalTheme()` → use `BFHcharts::apply_spc_theme()`

---

### 1.3 Y-Axis Formatting

**Status:** ⚠️ **LIKELY REUSABLE** - Verify imports

#### SPCify Implementation
- **File:** `R/fct_spc_plot_generation.R`
- **Function:** `apply_y_axis_formatting()` (called linje 1301)
- **Actual Implementation:** `R/utils_y_axis_formatting.R` (separate file)
- **Features:**
  - Unit-based formatting (count, time, percentage)
  - Time conversion and display
  - Dynamic scaling

#### BFHcharts Implementation
- **File:** `R/utils_y_axis_formatting.R`
- **Status:** Exists in BFHcharts

#### Action
- [ ] **TODO:** Verify if SPCify reuses BFHcharts version or has own implementation
- If own implementation: Check for NAMESPACE import
- If BFHcharts has it: Consider migration

---

### 1.4 Date/Axis Intelligent Formatting

**Status:** ⚠️ **SPCify-SPECIFIC** - Keep for now

#### SPCify Implementation
- **File:** `R/fct_spc_plot_generation.R`
- **Lines:** 1098-1296 (detailed intelligent date formatting)
- **Smart Features:**
  - `detect_date_interval()` - Auto-detect weekly/monthly/daily patterns
  - Adaptive break calculation
  - Smart label formatting based on interval type
  - `get_optimal_formatting()` - Determines best format

#### Status in BFHcharts
- **File:** `R/utils_date_formatting.R` (exists)
- **Similarity:** Unknown - likely more basic

#### Decision
- ⚠️ **KEEP** SPCify's smart date formatting for now
- **TODO:** Evaluate if BFHcharts version is sufficient
- Long-term: Consider migrating to BFHcharts if robust enough

---

## Part 2: Code Removal Strategy

### Phase 1: Immediate (Update Imports & References)

**Objective:** Prepare codebase for function removal

#### 1.1 Update NAMESPACE
```r
# Add to NAMESPACE:
importFrom(BFHcharts, add_plot_enhancements)
importFrom(BFHcharts, apply_spc_theme)
```

#### 1.2 Replace All Internal Calls

**File: R/fct_spc_plot_generation.R**

Find and replace:
```r
# add_plot_enhancements calls (appears ~2-3 times in generateSPCPlot_qicharts2)
# BEFORE:
plot <- add_plot_enhancements(plot, qic_data, comment_data, ...)

# AFTER:
plot <- BFHcharts::add_plot_enhancements(plot, qic_data, comment_data)
```

**File: R/mod_spc_chart_server.R** (if used)
- Search for `add_plot_enhancements` calls
- Search for `applyHospitalTheme` calls
- Replace with BFHcharts:: versions

**File: R/fct_spc_bfh_service.R** (if used)
- Similar replacements

#### 1.3 Update Color References

Replace hardcoded colors with BFHtheme API:
```r
# In add_plot_enhancements() calls, colors are already handled by BFHcharts
# No additional changes needed if using BFHcharts::add_plot_enhancements()

# In any remaining code:
# BEFORE:
color = hospital_colors$hospitalblue

# AFTER:
color = BFHtheme::bfh_cols("hospital_blue")
```

#### 1.4 Tests to Update
- Search `tests/testthat/` for references to `add_plot_enhancements`
- Search for references to `applyHospitalTheme`
- Update test expectations if color values changed

### Phase 2: Code Cleanup (After Phase 1 Verification)

**Objective:** Remove duplicate functions

#### 2.1 Remove `add_plot_enhancements()`
```bash
# File: R/fct_spc_plot_generation.R
# Lines 461-594: DELETE ENTIRE FUNCTION
```

**Steps:**
1. Verify all calls have been updated to use BFHcharts version
2. Run full test suite to confirm no regressions
3. Delete lines 461-594
4. Re-run tests

#### 2.2 Remove `applyHospitalTheme()`
```bash
# File: R/fct_spc_plot_generation.R
# Lines 1389-1438: DELETE ENTIRE FUNCTION
```

**Steps:**
1. Verify all calls have been updated to use BFHcharts version
2. Run full test suite to confirm no regressions
3. Delete lines 1389-1438
4. Re-run tests

#### 2.3 Update Tests
- Remove any tests that specifically test `add_plot_enhancements()` (redundant)
- Remove any tests that specifically test `applyHospitalTheme()` (redundant)
- Keep integration tests that verify plot output

#### 2.4 Update Documentation
- Search `docs/` and `README.md` for references
- Remove examples using removed functions
- Add migration notes if relevant

### Phase 3: Long-term (When BFHcharts = 100%)

**Objective:** Streamline to primary backend only

#### 3.1 Deprecate `generateSPCPlot_qicharts2()`
```r
#' @deprecated Use generateSPCPlot_with_backend() with BFHcharts backend instead
generateSPCPlot_qicharts2 <- function(...) {
  .Deprecated("generateSPCPlot_with_backend", msg = "... use BFHcharts backend")
  # ... existing implementation
}
```

#### 3.2 Create Compatibility Module
- Move all qicharts2-specific code to `R/compat_qicharts2.R`
- Keep for backward compatibility
- Mark as legacy/deprecated

#### 3.3 Expected Line Count Reduction
```
Before: fct_spc_plot_generation.R = 1,482 lines
After Phase 2: ~1,250 lines (removed 200+ lines)
After Phase 3: ~400-500 lines (only BFHcharts + essential qicharts2 compat)
```

---

## Part 3: Testing & Validation

### Pre-Migration Testing
- [ ] Run full test suite: `devtools::test()`
- [ ] Check visual regression: Generate sample plots
- [ ] Verify colors match hospital branding
- [ ] Check font rendering

### Phase 1 Testing (After NAMESPACE updates)
- [ ] `devtools::check()` - No warnings/errors
- [ ] Full test suite passes
- [ ] Color values unchanged in output
- [ ] Font rendering identical

### Phase 2 Testing (After code removal)
- [ ] `devtools::check()` - Passes without deprecation warnings
- [ ] Full test suite passes
- [ ] No broken references
- [ ] Build succeeds: `devtools::build()`

### Regression Testing
- [ ] Generate test plots with same data as before/after
- [ ] Compare pixel-by-pixel (if using shinytest2)
- [ ] Verify axis labels, colors, spacing identical
- [ ] Manual visual inspection

---

## Part 4: Implementation Checklist

### Immediate (Sprint Current)
- [ ] Create GitHub Issue #49 ✅ Done
- [ ] Create this documentation ✅ Done
- [ ] Update NAMESPACE: Add BFHcharts imports
- [ ] Replace SPCify calls → BFHcharts calls
- [ ] Run tests - verify no regressions
- [ ] Code review PR

### Near-term (Sprint +1)
- [ ] Remove duplicate `add_plot_enhancements()` function
- [ ] Remove duplicate `applyHospitalTheme()` function
- [ ] Update tests - remove redundant tests
- [ ] Update documentation
- [ ] Final validation

### Long-term (When BFHcharts stable)
- [ ] Deprecate `generateSPCPlot_qicharts2()`
- [ ] Migrate remaining qicharts2 code
- [ ] Archive old implementation

---

## Part 5: Risk Assessment & Mitigation

### Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|-----------|
| Visual regression in plots | Medium | High | Pixel-perfect testing, manual review |
| Color mismatch with branding | Low | High | BFHtheme::bfh_cols() is canonical |
| Font rendering issues | Low | Medium | Test multiple systems (Mac, Linux) |
| Test failures | Low | High | Comprehensive test coverage |
| Breaking API changes in BFHcharts | Low | High | Version lock in DESCRIPTION |

### Rollback Plan
1. If visual regression detected: Revert commit
2. If tests fail: Investigate specific failure
3. If colors wrong: Verify BFHtheme version in dependencies
4. If fonts break: Check system font availability

---

## Part 6: Related Code & Context

### Files That Will Change
- `NAMESPACE` - Add BFHcharts imports
- `R/fct_spc_plot_generation.R` - Main target (remove 200+ lines)
- `tests/testthat/test-spc-plot-generation*.R` - Update/remove tests
- Documentation files - Update examples

### Files That Need Verification
- `R/mod_spc_chart_server.R` - Check for function calls
- `R/fct_spc_bfh_service.R` - Check for function calls
- Any Shiny UI/server code using these functions

### Dependencies
- Must have: `BFHcharts >= 0.1.0`
- Must have: `BFHtheme >= 0.1.0`
- Already imported: `ggplot2`, `ggrepel`, `lemon`

---

## Summary

This cleanup represents a significant architectural improvement:

1. **Single Source of Truth:** Visualization logic centralized in BFHcharts
2. **Better Maintainability:** Less duplicated code to maintain
3. **Improved Branding:** BFHtheme provides canonical color/font management
4. **Reduced Complexity:** 1,482 → 400 lines in main plot file
5. **Better Separation:** SPCify focuses on data logic, BFHcharts on visualization

**Expected Effort:** 2-3 hours (Phase 1 + 2)
**Expected Value:** Significant code quality improvement + maintainability gain
