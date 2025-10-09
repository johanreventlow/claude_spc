# Refactoring Opportunities Report

**Dato**: 2025-10-09
**Agent**: refactoring-advisor
**Score**: 75/100

## Executive Summary

SPC-applikationen demonstrerer solid arkitektonisk foundation med centraliseret state management, event-driven architecture, og TDD principles. Dog findes der flere refactoring opportunities der ville signifikant forbedre code maintainability, reducere complexity, og enhance testability.

**Samlet Effort**: 36-44 timer over 5 uger

---

## CRITICAL Priority Refactorings

### 1. **Monster Function: `generateSPCPlot()` (1,330 lines)**

**Location:** `/Users/johanreventlow/Documents/R/claude_spc/R/fct_spc_plot_generation.R:601-1329`

**Issue Description:**
Denne single function håndterer 10+ responsibilities:
- Parameter processing
- Data validation
- Chart type routing
- X-axis formatting (200+ lines)
- Y-axis formatting (150+ lines)
- Plot building
- Enhancement additions
- Label placement
- Phase/freeze configuration
- Cache management

**Code Smell:** God Object, Long Method, Feature Envy

**Proposed Solution:**
Extract specialized functions:

```r
# Before (simplified excerpt)
generateSPCPlot <- function(data, config, chart_type, ...) {
  # 200+ lines of x-axis formatting
  # 150+ lines of y-axis formatting
  # ... more logic
}

# After (refactored)
generateSPCPlot <- function(data, config, chart_type, ...) {
  # Core orchestration only (~50 lines)
  validated_config <- validate_and_sanitize_inputs(data, config)
  processed_data <- process_chart_data(data, validated_config, chart_type, y_axis_unit)
  qic_data <- generate_qic_data(processed_data, validated_config, chart_type)

  plot <- build_base_plot(qic_data, validated_config)
  plot <- apply_axis_formatting(plot, qic_data, x_validation, y_axis_unit)
  plot <- add_enhancements_and_labels(plot, qic_data, validated_config)
  plot <- applyHospitalTheme(plot, base_size)

  return(list(plot = plot, qic_data = qic_data))
}

# Extracted functions (new files):
# - R/fct_spc_plot_axis_formatting.R
#     - format_x_axis_datetime()
#     - format_y_axis_by_unit()
# - R/fct_spc_plot_data_processing.R
#     - process_chart_data()
#     - validate_and_sanitize_inputs()
# - R/fct_spc_plot_building.R
#     - build_base_plot()
#     - add_control_limits()
```

**Benefits:**
- Each function < 50 lines
- Single responsibility per function
- Easier unit testing
- Reduced cognitive load
- Better code reuse

**Effort:** 6-8 timer (high impact)
**Prioritet:** CRITICAL

---

### 2. **Duplicated Y-Axis Formatting Logic**

**Location:** Multiple instances across:
- `/Users/johanreventlow/Documents/R/claude_spc/R/fct_spc_plot_generation.R:1115-1247`
- `/Users/johanreventlow/Documents/R/claude_spc/R/utils_label_formatting.R`

**Issue Description:**
Y-axis formatting logic (percent, count with K/M notation, rate, time) appears duplicated. Same complex conditionals for decimal mark, thousand separators, and unit suffixes exist in multiple places.

**Code Smell:** DRY Violation, Copy-Paste Programming

**Proposed Solution:**
Create centralized formatting module:

```r
# R/utils_y_axis_formatting.R (new file)

#' Format Y-axis values with intelligent scaling
#' @export
format_y_axis_value <- function(value, unit = "count", y_range = NULL) {
  if (is.na(value)) return(NA_character_)

  switch(unit,
    "percent" = format_percent_value(value),
    "count" = format_count_with_scaling(value),
    "rate" = format_rate_value(value),
    "time" = format_time_value(value, y_range),
    format_default(value)
  )
}

format_count_with_scaling <- function(value) {
  # Single source of truth for K/M/mia. logic
  scales <- c(1e9, 1e6, 1e3)
  suffixes <- c(" mia.", "M", "K")

  for (i in seq_along(scales)) {
    if (abs(value) >= scales[i]) {
      scaled <- value / scales[i]
      return(format_scaled_value(scaled, suffixes[i]))
    }
  }

  format_base_value(value)
}
```

**Benefits:**
- Single source of truth
- Consistent behavior
- Easier to add new units
- Reduced code by ~200 lines
- Testable in isolation

**Effort:** 3-4 timer
**Prioritet:** CRITICAL

---

### 3. **Magic Numbers and Hardcoded Values**

**Location:** Throughout codebase, especially:
- `/Users/johanreventlow/Documents/R/claude_spc/R/fct_spc_plot_generation.R:620-629`
- `/Users/johanreventlow/Documents/R/claude_spc/R/fct_add_spc_labels.R:178-189`

**Issue Description:**
Magic numbers scattered throughout:

```r
# Line 620-629: Scaling factors
ucl_linewidth <- 2.5 * scale_factor
target_linewidth <- 1 * scale_factor
data_linewidth <- 1 * scale_factor
point_size <- 2 * scale_factor
comment_size <- 6 * scale_factor

# Line 178: Device height baseline
device_height_baseline <- 7.8 # inches

# Line 448: X-axis extension
extended_x <- last_x + (x_range * 0.20) # 20% extension
```

**Code Smell:** Magic Number Anti-pattern

**Proposed Solution:**
Move to centralized configuration:

```r
# R/config_plot_dimensions.R (new file)

#' Plot Visual Configuration Constants
#' @export
PLOT_DIMENSIONS <- list(
  # Line weights (base = 1.0 at scale_factor 1.0)
  ucl_linewidth_base = 2.5,
  target_linewidth_base = 1.0,
  data_linewidth_base = 1.0,

  # Point and label sizes
  point_size_base = 2.0,
  comment_size_base = 6.0,

  # Device and scaling
  device_height_baseline_inches = 7.8,
  base_font_size = 14,

  # Plot extensions and margins
  x_axis_extension_factor = 0.20,  # 20% beyond last point
  label_inset_margin_factor = 0.01 # 1% from edge
)

# Usage:
ucl_linewidth <- PLOT_DIMENSIONS$ucl_linewidth_base * scale_factor
```

**Benefits:**
- Centralized tuning
- Self-documenting code
- Easier A/B testing
- Prevents inconsistencies

**Effort:** 2-3 timer
**Prioritet:** CRITICAL

---

## IMPORTANT Priority Refactorings

### 4. **Complex Conditional Logic in Event Listeners**

**Location:** `/Users/johanreventlow/Documents/R/claude_spc/R/utils_server_event_listeners.R:121-166`

**Issue Description:**
Deep nesting and complex conditionals in `data_updated` event handler.

**Code Smell:** Arrow Anti-pattern, Complex Conditional

**Proposed Solution:**
Strategy pattern with context handlers:

```r
# R/utils_event_context_handlers.R (new file)

#' Handle data update by context type
handle_data_update_by_context <- function(update_context, app_state, ...) {
  context_type <- classify_update_context(update_context)

  handler <- switch(context_type,
    "load" = handle_load_context,
    "table_edit" = handle_table_edit_context,
    "data_change" = handle_data_change_context,
    "general" = handle_general_context,
    handle_unknown_context
  )

  handler(app_state, input, output, session, ui_service, emit)
}

classify_update_context <- function(update_context) {
  if (is.null(update_context)) return("general")

  context <- update_context$context %||% "general"

  if (identical(context, "table_cells_edited")) return("table_edit")
  if (grepl("load|upload|new", context, ignore.case = TRUE)) return("load")
  if (grepl("change|edit|modify", context, ignore.case = TRUE)) return("data_change")

  "general"
}
```

**Benefits:**
- Reduced cyclomatic complexity (from 12 to ~3 per function)
- Easier to add new context types
- Better testability
- Self-documenting

**Effort:** 4-5 timer
**Prioritet:** IMPORTANT

---

### 5. **Inconsistent State Access Patterns**

**Location:** Throughout codebase

**Issue Description:**
Mixed patterns for accessing app_state:

```r
# Pattern 1: Direct access
app_state$columns$auto_detect$in_progress

# Pattern 2: With isolate
shiny::isolate(app_state$columns$auto_detect$in_progress)

# Pattern 3: Through helper
current_data <- get_current_data(app_state)
```

**Code Smell:** Inconsistent Abstraction Level

**Proposed Solution:**
Standardize with accessor functions:

```r
# R/utils_state_accessors.R (new file)

#' Get autodetect status safely
get_autodetect_status <- function(app_state) {
  shiny::isolate({
    list(
      in_progress = app_state$columns$auto_detect$in_progress %||% FALSE,
      completed = app_state$columns$auto_detect$completed %||% FALSE,
      results = app_state$columns$auto_detect$results
    )
  })
}

#' Set autodetect in progress flag
set_autodetect_in_progress <- function(app_state, value) {
  shiny::isolate({
    app_state$columns$auto_detect$in_progress <- value
  })
}
```

**Benefits:**
- Consistent isolate() usage
- Encapsulation of state structure
- Easier to refactor state schema
- Type safety through validation

**Effort:** 5-6 timer
**Prioritet:** IMPORTANT

---

### 6. **Long Parameter Lists**

**Location:** Multiple functions:
- `/Users/johanreventlow/Documents/R/claude_spc/R/fct_spc_plot_generation.R:601` (13 parameters)
- `/Users/johanreventlow/Documents/R/claude_spc/R/fct_add_spc_labels.R:49` (11 parameters)

**Issue Description:**
Functions with excessive parameters:

```r
generateSPCPlot <- function(data, config, chart_type, target_value = NULL,
                           centerline_value = NULL, show_phases = FALSE,
                           skift_column = NULL, frys_column = NULL,
                           chart_title_reactive = NULL, y_axis_unit = "count",
                           kommentar_column = NULL, base_size = 14,
                           viewport_width = NULL, viewport_height = NULL,
                           target_text = NULL, qic_cache = NULL)
```

**Code Smell:** Long Parameter List, Data Clump

**Proposed Solution:**
Parameter objects (S3 classes):

```r
# R/plot_config_objects.R (new file)

#' Create SPC plot configuration object
#' @export
spc_plot_config <- function(chart_type, y_axis_unit = "count",
                             base_size = 14, target_value = NULL,
                             centerline_value = NULL, target_text = NULL) {
  structure(
    list(
      chart_type = chart_type,
      y_axis_unit = y_axis_unit,
      base_size = base_size,
      target_value = target_value,
      centerline_value = centerline_value,
      target_text = target_text
    ),
    class = "spc_plot_config"
  )
}

# Refactored function signature:
generateSPCPlot <- function(data, config, plot_config, viewport, phases,
                           chart_title_reactive = NULL,
                           kommentar_column = NULL, qic_cache = NULL) {
  # Access via plot_config$chart_type, viewport$width, phases$show_phases
}
```

**Benefits:**
- Reduced function signature complexity
- Logical grouping of related parameters
- Easier to add new parameters
- Better documentation
- Default value management centralized

**Effort:** 4-5 timer
**Prioritet:** IMPORTANT

---

## Summary Tables

### By Priority

| Priority | # of Issues | Total Effort | Impact |
|----------|-------------|--------------|--------|
| CRITICAL | 3 | 11-15 timer | High |
| IMPORTANT | 3 | 13-16 timer | Medium-High |
| NICE-TO-HAVE | 4 | 12-13 timer | Medium |
| **TOTAL** | **10** | **36-44 timer** | **Very High** |

### By Code Smell Type

| Code Smell | Count | Files Affected |
|------------|-------|----------------|
| Long Method | 3 | fct_spc_plot_generation.R, fct_add_spc_labels.R |
| DRY Violation | 2 | Multiple files |
| Magic Numbers | 1 | 5+ files |
| Complex Conditional | 2 | utils_server_event_listeners.R, fct_autodetect_unified.R |
| Long Parameter List | 1 | 3+ files |
| Inconsistent Naming | 1 | Entire codebase |

### Recommended Implementation Order

1. **Week 1:** Magic numbers → Config (2-3h), Y-axis formatting DRY (3-4h)
2. **Week 2:** Extract `generateSPCPlot()` functions (6-8h)
3. **Week 3:** Event handler refactoring (4-5h), State accessors (5-6h)
4. **Week 4:** Parameter objects (4-5h)
5. **Week 5:** Guard system (3h), Dead code (1h), Naming (4-5h)

---

## Testing Strategy

For hver refactoring:

1. **Pre-refactor:** Run full test suite, dokumentér passing tests
2. **Extract function:** Create new function with tests (TDD)
3. **Replace calls:** Update all call sites incrementally
4. **Post-refactor:** Re-run full test suite, ensure 100% pass rate
5. **Integration test:** Run manual UI workflow tests
6. **Performance check:** Benchmark if performance-sensitive area

---

## Risk Assessment

| Refactoring | Risk Level | Mitigation |
|-------------|------------|------------|
| generateSPCPlot extraction | Medium | Extensive integration tests, phased rollout |
| Y-axis formatting | Low | Pure functions, easy to test |
| Magic numbers → Config | Low | Search/replace with tests |
| Event handler refactoring | Medium-High | Careful state management testing |
| State accessors | Medium | Gradual migration, parallel old/new |
| Parameter objects | Low | Backward compatible wrappers initially |

---

## Conclusion

SPC-applikationen har en solid foundation men ville benefit significantly fra these refactorings. **Priority should be given to CRITICAL items** (monster function extraction, DRY violations, magic numbers) da de påvirker multiple areas og har high impact på maintainability.

Refactorings respect existing architecture (event-driven, centralized state, TDD) og align med projekt's Danish commenting conventions og quality standards outlined i CLAUDE.md.
