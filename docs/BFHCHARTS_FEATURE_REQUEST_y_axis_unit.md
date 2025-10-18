# BFHcharts Feature Request: Y-Axis Unit Formatting Support

## Status
**REQUEST** - Pending BFHcharts maintainer review

## Problem Statement

BFHcharts accepts `y_axis_unit` parameter but does **not** map it to qicharts2's `y.percent` parameter, resulting in incorrect y-axis formatting for percentage-based charts.

### Current Behavior
```r
# User calls BFHcharts with percentage unit
BFHcharts::create_spc_chart(
  data = data,
  x = Date,
  y = Rate,
  chart_type = "p",
  y_axis_unit = "percent"  # ✅ Accepted parameter
)
# Result: Y-axis shows decimals (0.75) instead of percentages (75%)
```

### Root Cause
In `BFHcharts::create_spc_chart()`, the function builds `qic_args` and calls `qicharts2::qic()`, but **never maps** `y_axis_unit` to `y.percent`:

```r
# Current BFHcharts implementation (simplified)
qic_args <- list(
  data = data,
  x = substitute(x),
  y = substitute(y),
  chart = chart_type,
  target = target_value,
  # ... other parameters
)
# Missing: qic_args$y.percent <- (y_axis_unit == "percent")

qic_data <- do.call(qicharts2::qic, qic_args)
```

qicharts2 expects `y.percent = TRUE/FALSE` to format the y-axis correctly.

---

## Proposed Solution

### Implementation Location
In `BFHcharts::create_spc_chart()`, **after** building `qic_args` and **before** calling `do.call(qicharts2::qic, ...)`.

### Code Change
```r
# In create_spc_chart() - after line where qic_args is built:

# Map y_axis_unit to qicharts2's y.percent parameter
if (!is.null(y_axis_unit) && y_axis_unit == "percent") {
  qic_args$y.percent <- TRUE
}
```

### Complete Context
```r
# BFHcharts::create_spc_chart() - proposed implementation

# Build qic_args (existing code)
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
# ... other parameters ...

# ✅ NEW: Map y_axis_unit to qicharts2's y.percent
if (!is.null(y_axis_unit) && y_axis_unit == "percent") {
  qic_args$y.percent <- TRUE
}

# Call qicharts2::qic with mapped parameters
qic_data <- do.call(qicharts2::qic, qic_args, envir = parent.frame())
```

---

## Expected Behavior After Fix

### Test Case 1: P-chart with Percentage Unit
```r
result <- BFHcharts::create_spc_chart(
  data = infection_data,
  x = Month,
  y = InfectionRate,
  n = Procedures,
  chart_type = "p",
  y_axis_unit = "percent"
)
# Expected: Y-axis displays "75%" instead of "0.75"
```

### Test Case 2: Run Chart with Count Unit (default)
```r
result <- BFHcharts::create_spc_chart(
  data = volume_data,
  x = Week,
  y = PatientCount,
  chart_type = "run",
  y_axis_unit = "count"
)
# Expected: Y-axis displays "100" (no percentage formatting)
```

---

## Impact Assessment

### Breaking Changes
**None** - This is a pure feature addition. Existing code without `y_axis_unit = "percent"` will behave identically.

### Affected Parameters
- `y_axis_unit = "percent"` → Enables percentage formatting
- `y_axis_unit = "count"` (default) → No change (existing behavior)
- `y_axis_unit = "rate"` → No change (existing behavior)
- `y_axis_unit = "time"` → No change (existing behavior)

### Integration with SPCify
SPCify currently **passes** `y_axis_unit` to BFHcharts but **does not receive** the expected formatting. This fix will restore SPCify's y-axis formatting functionality lost during qicharts2 → BFHcharts migration.

---

## Testing Requirements

### Unit Tests
```r
test_that("y_axis_unit percent maps to y.percent parameter", {
  result <- BFHcharts::create_spc_chart(
    data = test_data,
    x = x,
    y = y,
    n = n,
    chart_type = "p",
    y_axis_unit = "percent"
  )

  # Verify y-axis labels show percentages
  y_labels <- ggplot2::ggplot_build(result)$layout$panel_params[[1]]$y$get_labels()
  expect_true(any(grepl("%", y_labels)))
})

test_that("y_axis_unit count does not apply percentage formatting", {
  result <- BFHcharts::create_spc_chart(
    data = test_data,
    x = x,
    y = y,
    chart_type = "run",
    y_axis_unit = "count"
  )

  # Verify y-axis labels do NOT show percentages
  y_labels <- ggplot2::ggplot_build(result)$layout$panel_params[[1]]$y$get_labels()
  expect_false(any(grepl("%", y_labels)))
})
```

### Integration Tests with SPCify
1. Upload CSV with percentage data
2. Select P-chart
3. Set Y-axis unit to "Procent"
4. Verify y-axis displays "75%" not "0.75"

---

## Related Documentation

### qicharts2 Reference
From `?qicharts2::qic`:
```
y.percent: Logical value. If TRUE, formats y axis labels as percentages.
```

### BFHcharts Current Parameters
```r
create_spc_chart(
  y_axis_unit = "count" | "percent" | "rate" | "time"
)
```

### SPCify Integration
- File: `R/fct_spc_bfh_service.R`
- Function: `compute_spc_results_bfh()`
- Current status: Passes `y_axis_unit` but formatting does not apply

---

## Priority
**HIGH** - This is a regression from qicharts2 → BFHcharts migration. Users expect percentage charts to display "75%" not "0.75".

---

## Workaround (Temporary - for SPCify Only)

Until this is fixed in BFHcharts, SPCify could implement a temporary workaround:

```r
# In SPCify's map_to_bfh_params() - TEMPORARY WORKAROUND
# Mark clearly as temporary and remove when BFHcharts is fixed

# TEMPORARY: Map y_axis_unit to y.percent for qicharts2 compatibility
# TODO: Remove when BFHcharts implements y_axis_unit → y.percent mapping
if (!is.null(y_axis_unit) && y_axis_unit == "percent") {
  # BFHcharts doesn't support this yet, but we can pass it through
  # via extra_params if BFHcharts accepts ... parameters
  warning("y_axis_unit = 'percent' requires BFHcharts fix - formatting may not apply")
}
```

**Note:** This workaround is **not recommended** as it violates the External Package Ownership principle. Better to fix in BFHcharts directly.

---

## Next Steps

1. **BFHcharts maintainer** reviews this feature request
2. **Implements** the 3-line fix in `create_spc_chart()`
3. **Adds** unit tests for y.percent mapping
4. **Releases** new BFHcharts version
5. **SPCify updates** to new BFHcharts version
6. **Verification** in SPCify integration tests

---

## Contact
**Requester:** SPCify development team
**Date:** 2025-10-17
**Related Issues:** SPCify regression after BFHcharts migration
