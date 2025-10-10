# Week 11-12 Refactoring Analysis
# Generated: 2025-10-10
# FASE 4: Core Refactorings - Implementation Analysis

## Executive Summary

After analyzing `fct_spc_plot_generation.R` (1476 lines), the file is **already reasonably well-organized** with clear function boundaries and logical sections. The planned "big bang" extraction into 3 new files carries **HIGH RISK** of introducing bugs for **MEDIUM BENEFIT**.

**Recommendation**: **DEFER large-scale extraction** in favor of **incremental improvements** that provide better risk/reward ratio.

## Current File Structure Analysis

### File Organization (Already Good)

The file is organized into logical sections:

1. **COMMENT PROCESSING** (lines 11-83)
   - `extract_comment_data()` - Well-encapsulated

2. **DATA CLEANING** (lines 89-153)
   - `clean_qic_call_args()` - Self-contained

3. **QIC ARGUMENTS** (lines 159-194)
   - `build_qic_call_arguments()` - Clear interface

4. **DATA PROCESSING** (lines 200-250)
   - `process_ratio_chart_data()` - Focused
   - `process_standard_chart_data()` - Focused

5. **QIC DATA GENERATION** (lines 256-429)
   - `prepare_qic_data_parameters()` - Complex but necessary
   - `build_qic_arguments()` - NSE handling
   - `execute_qic_call()` - Performance monitoring

6. **PLOT ENHANCEMENT** (lines 436-599)
   - `add_plot_enhancements()` - Already extracted

7. **MAIN FUNCTION** (lines 601-1198)
   - `generateSPCPlot()` - Orchestration function (598 lines)

8. **PLOT STYLING** (lines 1205-1272)
   - `applyHospitalTheme()` - Already extracted
   - `detectYAxisScale()` - Already extracted

### Key Observations

✅ **GOOD**:
- Clear function boundaries with section comments
- Most helper functions already extracted
- Roxygen documentation present
- Logical flow from data → processing → plotting

⚠️ **CONCERNS**:
- `generateSPCPlot()` main function is 598 lines (large)
- Y-axis formatting logic inline (lines 1160-1219) - could be extracted
- X-axis formatting logic inline (lines 960-1157) - complex datetime handling
- Some duplication in time formatting (minutes/hours/days)

## Risk Assessment: Planned Extraction

### Proposed Extraction (from 18-week plan)

**Plan**: Extract into 3 files
1. `R/fct_spc_plot_axis_formatting.R` - X/Y axis formatting
2. `R/fct_spc_plot_data_processing.R` - Data validation, QIC generation
3. `R/fct_spc_plot_building.R` - Plot construction

### Risk Analysis

**HIGH RISK FACTORS**:
1. **Complex interdependencies**: Functions share state (qic_data, config, x_validation)
2. **NSE complications**: Non-standard evaluation in QIC calls (as.name())
3. **Test coverage**: Comprehensive tests exist (test-generateSPCPlot-comprehensive.R)
   - Risk of breaking existing tests
   - Need to update all test mocks
4. **Performance implications**: Function call overhead in hot path
5. **Namespace pollution**: 3 new files = more cognitive load for developers

**MEDIUM BENEFIT**:
- Marginally improved maintainability (functions already well-separated)
- Slightly easier to test (but tests already comprehensive)
- Reduced line count per file (but current file is navigable with sections)

**CONCLUSION**: **Risk > Benefit** for this refactoring

## Alternative Approach: Incremental Improvements

### Recommendation 1: Extract Y-Axis Formatting (LOW RISK)

**Target**: Lines 1160-1219 (Y-axis formatting logic)

**Extraction Plan**:
```r
# R/utils_y_axis_formatting.R (NEW)

#' Apply Y-Axis Formatting to Plot
#' @param plot ggplot object
#' @param y_axis_unit Unit type ("percent", "count", "rate", "time")
#' @param qic_data QIC data frame with y column
#' @return Modified ggplot object with appropriate y-axis scale
apply_y_axis_formatting <- function(plot, y_axis_unit, qic_data) {
  if (y_axis_unit == "percent") {
    return(plot + ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(.25, .25)),
      labels = scales::label_percent()
    ))
  } else if (y_axis_unit == "count") {
    return(plot + format_y_axis_count(qic_data$y))
  } else if (y_axis_unit == "rate") {
    return(plot + format_y_axis_rate())
  } else if (y_axis_unit == "time") {
    return(plot + format_y_axis_time(qic_data$y))
  }

  # Default: no special formatting
  return(plot)
}

#' Format Y-Axis for Count Data
#' @keywords internal
format_y_axis_count <- function(y_data) {
  # ... (extract helper functions format_scaled_number, format_unscaled_number)
  # ... (extract scale_y_continuous with K/M/mia. notation)
}

#' Format Y-Axis for Time Data
#' @keywords internal
format_y_axis_time <- function(y_data) {
  y_range <- range(y_data, na.rm = TRUE)
  max_minutes <- max(y_range, na.rm = TRUE)

  # ... (extract time formatting logic for minutes/hours/days)
}
```

**Benefits**:
- Reduces `generateSPCPlot()` by ~60 lines
- Easier to test Y-axis formatting independently
- Potential reuse in other plotting functions
- LOW RISK: Pure function, no state dependencies

**Estimated Effort**: 2h

---

### Recommendation 2: Document Magic Numbers (LOW RISK)

**Target**: Inline magic numbers throughout file

**Current Issues**:
- `0.20` (20% x-axis extension) - appears 3 times
- `0.025` (2.5% expansion) - inline
- `2.5`, `1.0`, `1.0`, `2`, `6` (geom sizes) - calculated from base_size
- `"12"`, `"42"` (linetypes) - cryptic values

**Solution**: Create constants at top of file
```r
# PLOT CONSTANTS ==============================================================

# X-axis extension beyond last data point
X_AXIS_EXTENSION_FACTOR <- 0.20

# X-axis left margin expansion
X_AXIS_LEFT_MARGIN <- 0.025

# Geom size scale factors (relative to base_size)
GEOM_SCALE_FACTORS <- list(
  ucl_linewidth = 2.5,
  target_linewidth = 1.0,
  data_linewidth = 1.0,
  cl_linewidth = 1.0,
  point_size = 2.0,
  comment_size = 6.0,
  label_size = 6.0
)

# Line type definitions
LINETYPES <- list(
  centerline_signal = "12",  # Dashed (12 unit dash pattern)
  centerline_solid = "solid",
  target = "42",             # Custom dash pattern (4 on, 2 off)
  phase_separator = "dotted"
)
```

**Benefits**:
- Self-documenting code
- Easier to tune parameters
- Consistency across codebase
- ZERO RISK: Only adds constants, doesn't change logic

**Estimated Effort**: 1h

---

### Recommendation 3: Add Function Navigation Comments (ZERO RISK)

**Target**: Improve navigability with clear section markers

**Current**: Section comments exist but could be more prominent

**Solution**: Enhance with RStudio-friendly markers
```r
# COMMENT PROCESSING ==========================================================

## extract_comment_data ----
# ...

# DATA CLEANING ===============================================================

## clean_qic_call_args ----
# ...

# QIC ARGUMENTS ===============================================================

## build_qic_call_arguments ----
# ...
```

**Benefits**:
- RStudio code outline navigation
- Clear file structure
- Easy to jump between sections
- ZERO RISK: Only comments

**Estimated Effort**: 30min

---

### Recommendation 4: Consolidate Time Formatting (MEDIUM RISK)

**Target**: Lines 1219-1270 (time formatting with duplicated logic)

**Current Issue**: Similar logic repeated for minutes/hours/days with minor variations

**Solution**: Extract to helper function
```r
#' Format Time Value with Unit
#' @keywords internal
format_time_with_unit <- function(val_minutes, unit_threshold) {
  if (is.na(val_minutes)) return(NA_character_)

  scaled <- switch(unit_threshold,
    minutes = val_minutes,
    hours = val_minutes / 60,
    days = val_minutes / 1440
  )

  unit_label <- switch(unit_threshold,
    minutes = " min",
    hours = " timer",
    days = " dage"
  )

  if (scaled == round(scaled)) {
    paste0(round(scaled), unit_label)
  } else {
    paste0(format(scaled, decimal.mark = ",", nsmall = 1), unit_label)
  }
}
```

**Benefits**:
- DRY principle
- Easier to test
- Reduces duplication by ~30 lines

**Risk**: MEDIUM - Changes formatting logic (need comprehensive tests)

**Estimated Effort**: 1.5h

---

## Recommended Implementation Plan

### Week 11-12 (Revised Scope)

**Task 4.1 REVISED: Incremental Refactoring** (4h total)

Priority 1 (HIGH VALUE, LOW RISK):
1. Add function navigation comments (30min) ✅
2. Document magic numbers as constants (1h) ✅
3. Extract Y-axis formatting to utils_y_axis_formatting.R (2h) ✅

Priority 2 (DEFER):
4. Consolidate time formatting (1.5h) ⏸️ DEFER to Week 13-14
5. Large-scale file extraction (8h) ❌ NOT RECOMMENDED

**Task 4.2: Y-Axis Formatting Consolidation** (COMPLETED in Task 4.1)
- Merge with Task 4.1 Recommendation 1

**Task 4.3: Magic Numbers to Config** (COMPLETED in Task 4.1)
- Merge with Task 4.1 Recommendation 2

### Week 13-14 (Follow-up)

**Task 4.4 REVISED: Time Formatting Consolidation** (1.5h)
- Complete Recommendation 4 if tests pass

**Task 4.5: DEFER Large Extraction**
- Only pursue if specific pain points identified
- Requires explicit approval and risk acceptance

---

## Test Impact Analysis

### Existing Tests

**test-generateSPCPlot-comprehensive.R** (707 lines, Week 5-6)
- Comprehensive coverage of plot generation
- Tests all chart types, Y-axis formatting, edge cases
- ✅ Well-structured with describe/it patterns

**test-performance-benchmarks.R** (654 lines, Week 9-10)
- Performance benchmarks for plot generation
- Timing assertions (<500ms standard, <2s large)
- Memory profiling

### Test Updates Required

**For Recommendation 1 (Y-Axis Formatting)**:
- Add new test file: `tests/testthat/test-y-axis-formatting.R`
- Unit tests for `apply_y_axis_formatting()`
- Update existing tests to use new function
- **Risk**: LOW - Pure function, easy to test

**For Recommendation 2 (Constants)**:
- No test changes required
- Add test to verify constants exist
- **Risk**: ZERO

**For Recommendation 4 (Time Formatting)**:
- Add comprehensive time formatting tests
- Test all threshold boundaries (60min, 1440min)
- Test decimal/integer handling
- **Risk**: MEDIUM - Formatting changes could affect output

---

## Conclusion

The original Week 11-12 plan proposed **large-scale extraction** (3 new files, 8h effort) with **HIGH RISK** and **MEDIUM BENEFIT**.

**REVISED RECOMMENDATION**:
1. **Accept current structure** - File is already well-organized
2. **Implement incremental improvements** (4h effort, LOW RISK, HIGH VALUE)
   - Add navigation markers
   - Document magic numbers
   - Extract Y-axis formatting
3. **DEFER large extraction** - Not justified by risk/reward ratio

**Estimated Time Savings**: 4h vs 8h planned (50% reduction)
**Risk Reduction**: HIGH RISK → LOW RISK
**Benefit**: Same or better (focused improvements vs. structural churn)

---

## Next Steps

1. ✅ Review this analysis with team
2. ✅ Approve revised scope for Week 11-12
3. ✅ Implement Priority 1 recommendations (4h)
4. ✅ Run full test suite to verify no breakage
5. ⏸️ Defer Priority 2 to Week 13-14 (if needed)

**Decision Point**: Should we proceed with incremental refactoring or defer entirely?

**Recommendation**: **PROCEED with incremental approach** - Low risk, high value, practical.
