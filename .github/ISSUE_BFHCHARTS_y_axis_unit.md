<!-- GitHub Issue Template for BFHcharts Repository -->

# [BFHcharts] y_axis_unit Parameter Not Mapped to qicharts2's y.percent

## Issue Type
🐛 Bug / 🎯 Feature Gap

## Priority
🔴 **HIGH** - Regression from qicharts2 → BFHcharts migration

---

## Problem Description

BFHcharts accepts `y_axis_unit = "percent"` parameter but does not map it to qicharts2's `y.percent` parameter. This results in percentage-based charts displaying decimal values (0.75) instead of percentages (75%).

### Minimal Reproducible Example

```r
library(BFHcharts)

# Create sample P-chart data
data <- data.frame(
  month = 1:12,
  infections = c(15, 18, 12, 20, 14, 16, 13, 17, 15, 19, 14, 16),
  procedures = rep(100, 12)
)

# Call with y_axis_unit = "percent"
plot <- create_spc_chart(
  data = data,
  x = month,
  y = infections,
  n = procedures,
  chart_type = "p",
  y_axis_unit = "percent"  # ✅ Parameter accepted
)

print(plot)
# ❌ Y-axis shows: 0.10, 0.15, 0.20
# ✅ Expected: 10%, 15%, 20%
```

### Current Behavior
- Y-axis displays decimals: **0.15** (incorrect)

### Expected Behavior
- Y-axis displays percentages: **15%** (correct)

---

## Root Cause

In `BFHcharts::create_spc_chart()`, the function builds `qic_args` list and calls `qicharts2::qic()`, but **never maps** the `y_axis_unit` parameter to qicharts2's `y.percent` parameter.

**Current implementation (simplified):**
```r
qic_args <- list(
  data = data,
  x = substitute(x),
  y = substitute(y),
  chart = chart_type,
  # ... other parameters
)

# ❌ Missing: Map y_axis_unit to y.percent
# qic_args$y.percent <- (y_axis_unit == "percent")

qic_data <- do.call(qicharts2::qic, qic_args)
```

---

## Proposed Fix

### Code Change (3 lines)

**Location:** `R/create_spc_chart.R` (or wherever `create_spc_chart()` is defined)

**Insert AFTER** building `qic_args` and **BEFORE** `do.call(qicharts2::qic, ...)`:

```r
# Map y_axis_unit to qicharts2's y.percent parameter
if (!is.null(y_axis_unit) && y_axis_unit == "percent") {
  qic_args$y.percent <- TRUE
}
```

### Complete Implementation Context

```r
create_spc_chart <- function(
  data, x, y, n = NULL, chart_type = "run",
  y_axis_unit = "count", # ... other params
) {
  # ... validation code ...

  # Build qic_args
  qic_args <- list(
    data = data,
    x = substitute(x),
    y = substitute(y),
    chart = chart_type
  )

  if (!is.null(n)) {
    qic_args$n <- substitute(n)
  }
  if (!is.null(part)) {
    qic_args$part <- part
  }
  # ... other parameter mappings ...

  # ✅ NEW: Map y_axis_unit to y.percent
  if (!is.null(y_axis_unit) && y_axis_unit == "percent") {
    qic_args$y.percent <- TRUE
  }

  # Call qicharts2::qic
  qic_data <- do.call(qicharts2::qic, qic_args, envir = parent.frame())

  # ... rest of function ...
}
```

---

## Testing

### Suggested Unit Test

```r
test_that("y_axis_unit = 'percent' maps to y.percent parameter", {
  data <- data.frame(
    x = 1:12,
    y = c(15, 18, 12, 20, 14, 16, 13, 17, 15, 19, 14, 16),
    n = rep(100, 12)
  )

  plot <- create_spc_chart(
    data = data,
    x = x,
    y = y,
    n = n,
    chart_type = "p",
    y_axis_unit = "percent"
  )

  # Verify y-axis labels contain percentage symbols
  y_labels <- ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$y$get_labels()
  expect_true(any(grepl("%", y_labels)))
})

test_that("y_axis_unit = 'count' does not apply percentage formatting", {
  data <- data.frame(
    x = 1:12,
    y = rnorm(12, 100, 10)
  )

  plot <- create_spc_chart(
    data = data,
    x = x,
    y = y,
    chart_type = "run",
    y_axis_unit = "count"
  )

  # Verify y-axis labels do NOT contain percentage symbols
  y_labels <- ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$y$get_labels()
  expect_false(any(grepl("%", y_labels)))
})
```

---

## Impact

### Breaking Changes
**None** - This is a pure feature addition. Existing code behavior is unchanged.

### Affected Use Cases
- P-charts with percentage interpretation
- U-charts with percentage interpretation
- Any chart where users set `y_axis_unit = "percent"`

### Integration
This fix is required for **SPCify** integration. SPCify passes `y_axis_unit` to BFHcharts and expects correct y-axis formatting.

---

## Related Issues
- SPCify regression: Y-axis formatting lost after qicharts2 → BFHcharts migration
- User expectation: Percentage charts should display "75%" not "0.75"

---

## qicharts2 Reference

From `?qicharts2::qic`:
```
y.percent: Logical value. If TRUE, formats y axis labels as percentages.
```

---

## Additional Context

**Environment:**
- BFHcharts version: [insert version]
- qicharts2 version: [insert version]
- R version: [insert version]

**Downstream Dependencies:**
- SPCify (R Shiny application for clinical SPC charts)

---

## Checklist for Maintainer

- [ ] Review proposed fix
- [ ] Implement `y_axis_unit → y.percent` mapping
- [ ] Add unit tests
- [ ] Update documentation (if needed)
- [ ] Release new version
- [ ] Notify SPCify team of fix availability

---

**Reported by:** SPCify Development Team
**Date:** 2025-10-17
**Related Documentation:** `docs/BFHCHARTS_FEATURE_REQUEST_y_axis_unit.md`
