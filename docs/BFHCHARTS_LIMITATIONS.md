# BFHcharts Limitations & Workarounds

**Version:** 2.0.0 (Pure BFHcharts - No Fallback)
**Last Updated:** 2025-10-17

## Quick Reference

| Feature | Status | Notes |
|---------|--------|-------|
| Run Charts | ✅ Full Support | All features supported |
| I Charts | ✅ Full Support | All features supported |
| P Charts | ✅ Full Support | All features supported |
| C Charts | ✅ Full Support | All features supported |
| U Charts | ✅ Full Support | All features supported |
| X̄ Charts | ❌ Not Supported | No fallback - explicit error |
| S Charts | ❌ Not Supported | No fallback - explicit error |
| MR Charts | ❌ Not Supported | No fallback - explicit error |

---

## Supported Chart Types

### ✅ Run Charts

**Status:** Full support in BFHcharts 0.1.0

**Features:**
- Sequential point plotting
- Median line
- Anhøj runs rules
- Phase markers
- Target lines
- Comment annotations

**Example:**
```r
compute_spc_results_bfh(
  data = data,
  x_var = "Dato",
  y_var = "Vaerdi",
  chart_type = "run"
)
```

**No limitations** for run charts.

---

### ✅ I Charts (Individual Measurements)

**Status:** Full support in BFHcharts 0.1.0

**Features:**
- Individual value plotting
- Control limits (±3σ)
- Centerline (mean)
- Anhøj rules (runs, crossings)
- Phase markers
- Target lines

**Example:**
```r
compute_spc_results_bfh(
  data = data,
  x_var = "Dato",
  y_var = "Vaerdi",
  chart_type = "i"
)
```

**No limitations** for I charts.

---

### ✅ P Charts (Proportions)

**Status:** Full support in BFHcharts 0.1.0

**Features:**
- Proportional data (0-1 or 0-100%)
- Control limits based on denominators
- Variable control limits
- Phase markers
- Target lines

**Example:**
```r
compute_spc_results_bfh(
  data = data,
  x_var = "Dato",
  y_var = "Numerator",
  n_var = "Denominator",
  chart_type = "p"
)
```

**Limitation:** Denominator must be provided and > 0 for each row.

**Workaround:** Pre-filter data to remove rows with missing/zero denominators.

---

### ✅ C Charts (Counts/Defects)

**Status:** Full support in BFHcharts 0.1.0

**Features:**
- Count data (integers)
- Control limits (±3√np̄)
- Fixed control limits
- Phase markers

**Example:**
```r
compute_spc_results_bfh(
  data = data,
  x_var = "Dato",
  y_var = "Count",
  chart_type = "c"
)
```

**No limitations** for C charts.

---

### ✅ U Charts (Rates with Variable Denominator)

**Status:** Full support in BFHcharts 0.1.0

**Features:**
- Rate data with variable denominators
- Control limits based on denominators
- Variable control limits per point
- Phase markers

**Example:**
```r
compute_spc_results_bfh(
  data = data,
  x_var = "Dato",
  y_var = "Defects",
  n_var = "Denominator",
  chart_type = "u"
)
```

**Limitation:** Denominator must vary per row (unlike P charts which have fixed denominator).

**Workaround:** If denominator is constant, use C chart instead.

---

## Unsupported Chart Types

### ❌ X̄ (X-bar) Charts

**Status:** NOT supported in BFHcharts 0.1.0

**Problem:** Requires subgrouped data structure that BFHcharts doesn't handle.

**Error Handling:**
```r
# Attempting to use xbar chart will result in an explicit error
tryCatch(
  {
    result <- generateSPCPlot(
      data = data,
      chart_type = "xbar",  # Unsupported chart type
      # ... other parameters
    )
  },
  error = function(e) {
    cat("Error:", e$message, "\n")
    # Error: Chart type 'xbar' is not supported in BFHcharts.
    # Supported types: run, i, p, c, u
  }
)
```

**Recommendation:** Use one of the supported chart types (run, i, p, c, u) instead. If X̄ charts are essential for your workflow, SPCify is not currently suitable.

---

### ❌ S Charts (Subgroup Standard Deviation)

**Status:** NOT supported in BFHcharts 0.1.0

**Problem:** Companion to X̄ charts, also requires subgrouped data.

**Error Handling:**
```r
# Will result in an explicit error
result <- generateSPCPlot(
  data = data,
  chart_type = "s",  # Unsupported chart type
  # ... other parameters
)
# Error: Chart type 's' is not supported in BFHcharts.
# Supported types: run, i, p, c, u
```

**Recommendation:** Use supported chart types instead.

---

### ❌ MR Charts (Moving Range)

**Status:** NOT supported in BFHcharts 0.1.0

**Problem:** BFHcharts API doesn't expose moving range calculations.

**Error Handling:**
```r
# Will result in an explicit error
result <- generateSPCPlot(
  data = data,
  chart_type = "mr",  # Unsupported chart type
  # ... other parameters
)
# Error: Chart type 'mr' is not supported in BFHcharts.
# Supported types: run, i, p, c, u
```

**Recommendation:** Use supported chart types instead.

---

## Feature Limitations

### 1. Centerline Override

**Limitation:** BFHcharts may not honor custom centerline values in all cases.

**Symptom:** Custom centerline provided but chart shows calculated mean instead.

**Workaround:**

```r
# Pre-process data to shift values so calculated mean equals desired centerline
data$Vaerdi <- data$Vaerdi - (current_mean - desired_centerline)
result <- compute_spc_results_bfh(...)
```

---

### 2. Phase Marker Positioning

**Limitation:** Phase markers may not align perfectly with data rows in all edge cases.

**Symptom:** Phase line appears slightly off from intended split point.

**Workaround:**

```r
# Ensure phase markers column is numeric and correctly indexed
data$Skift <- as.numeric(data$Skift)

# Verify with qicharts2 for comparison
result_qic <- compute_spc_results_qic(
  data = data,
  part_var = "Skift"
)
# If phase positioning differs, use qicharts2 (fallback)
```

---

### 3. Comment/Notes Rendering

**Limitation:** Very long comments may be truncated or overlap on charts.

**Symptom:** Long annotation text doesn't fully display.

**Workaround:**

```r
# Option 1: Keep comments short (< 30 characters recommended)
data$Kommentar <- substr(data$Kommentar, 1, 30)

# Option 2: Use abbreviated codes instead
data$Kommentar <- c("INT", "FIX", "MAI", ...)  # Instead of full text
```

---

### 4. X-axis Date Formatting

**Limitation:** BFHcharts may not format dates exactly as qicharts2 does.

**Symptom:** X-axis labels show different format (e.g., "2025-10-17" vs "17 Oct").

**Workaround:**

This is cosmetic only. Function behavior is unchanged.

```r
# No code workaround needed - dates display correctly,
# just potentially in different format
# If exact formatting critical, use qicharts2 fallback
```

---

### 5. Freeze Period (Baseline) Handling

**Limitation:** Freeze period line may not render in all chart type combinations.

**Symptom:** Freeze line doesn't appear on chart.

**Workaround:**

```r
# Verify freeze_var is numeric and correctly indexed
data$Fryz <- as.numeric(data$Fryz)

# Test with qicharts2 for comparison
result_qic <- compute_spc_results_qic(
  data = data,
  freeze_var = "Fryz"
)

# If BFHcharts doesn't render freeze line, verify data format and column types
```

---

## Performance Limitations

### 1. Large Datasets (>10,000 rows)

**Limitation:** BFHcharts performance degrades with very large datasets.

**Symptom:** Chart rendering takes > 10 seconds.

**Workaround:**

```r
# Option 1: Downsample data
data_downsampled <- data %>%
  dplyr::slice(seq(1, n(), by = 10))  # Every 10th row
result <- compute_spc_results_bfh(data = data_downsampled, ...)

# Option 2: Filter to recent period
data_recent <- data %>%
  dplyr::filter(Dato > Sys.Date() - 365)  # Last year only
result <- compute_spc_results_bfh(data = data_recent, ...)

# Option 3: Use cache (automatic - no code changes needed)
# Cache stores results for 1 hour, dramatically improving performance
```

---

### 2. Real-time Updates

**Limitation:** BFHcharts rendering not optimized for continuous real-time updates.

**Symptom:** UI feels sluggish when data updates every second.

**Workaround:**

```r
# Option 1: Debounce updates (Shiny automatically does this)
# Wait 500ms between updates before triggering render

# Option 2: Batch updates
# Collect 10 new data points before triggering chart update

# Option 3: Increase debounce timeout in configuration
# This reduces rendering frequency for real-time data
```

---

## Data Format Limitations

### 1. Missing Values (NA)

**Limitation:** BFHcharts doesn't handle NA values gracefully.

**Symptom:** Chart fails to render with error message.

**Workaround:**

```r
# Pre-filter to remove rows with NA in critical columns
data <- data %>%
  dplyr::filter(
    !is.na(Dato),
    !is.na(Vaerdi)
  )

result <- compute_spc_results_bfh(data = data, ...)
```

---

### 2. Non-Numeric Y Values

**Limitation:** Y-axis must be numeric.

**Symptom:** Chart fails with type error.

**Workaround:**

```r
# Ensure Y-axis column is numeric
data$Vaerdi <- as.numeric(data$Vaerdi)

# If data contains non-numeric codes, map to numeric equivalents
data$Vaerdi <- dplyr::case_when(
  data$Vaerdi == "HIGH" ~ 3,
  data$Vaerdi == "MED" ~ 2,
  data$Vaerdi == "LOW" ~ 1,
  TRUE ~ NA_real_
)

result <- compute_spc_results_bfh(data = data, ...)
```

---

### 3. Duplicate Dates

**Limitation:** Multiple rows with same X-axis value may cause rendering issues.

**Symptom:** Points overlap or X-axis compression.

**Workaround:**

```r
# Option 1: Add time component to dates
data$Dato <- data$Dato + lubridate::seconds(sample(0:3600, nrow(data)))

# Option 2: Use row number instead of date
data$Observ <- 1:nrow(data)

# Option 3: Aggregate duplicates
data <- data %>%
  dplyr::group_by(Dato) %>%
  dplyr::summarise(Vaerdi = mean(Vaerdi, na.rm = TRUE))

result <- compute_spc_results_bfh(data = data, ...)
```

---

## Color & Styling Limitations

### 1. Custom Color Palettes

**Limitation:** BFHcharts uses fixed color scheme from BFHthemes.

**Symptom:** Can't change control line colors, signal colors, etc.

**Workaround:**

```r
# No direct workaround for BFHcharts color customization
# BFHcharts uses fixed colors from BFHthemes for hospital branding (by design)
```

---

### 2. Legend Customization

**Limitation:** Legend layout/position not customizable in BFHcharts 0.1.0.

**Symptom:** Legend appears in fixed position, can't be moved.

**Workaround:**

```r
# No workaround - legend positioning is fixed
# This is acceptable as-is for most use cases
```

---

## Anhøj Rules Limitations

### 1. Runs Rules Edge Cases

**Limitation:** Some edge cases in Anhøj runs rules may differ between backends.

**Symptom:** Different run lengths reported in rare conditions.

**Workaround:**

```r
# BFHcharts uses qicharts2 for Anhøj metadata
# Results should be identical
# If discrepancies found, report as bug

# Verification
result_bfh <- compute_spc_results_bfh(...)
result_qic <- compute_spc_results_qic(...)

identical(result_bfh$qic_data$runs.signal, result_qic$runs.signal)
# Should be TRUE
```

---

## Known Issues Tracking

| Issue | Status | Workaround | Target Fix |
|-------|--------|-----------|-----------|
| X̄ charts unsupported | Known | Use supported chart types | BFHcharts 0.2.0? |
| S charts unsupported | Known | Use supported chart types | BFHcharts 0.2.0? |
| MR charts unsupported | Known | Use supported chart types | BFHcharts 0.2.0? |
| Centerline override finicky | Known | Pre-process data | BFHcharts 0.2.0? |
| Comment truncation | Minor | Keep comments short | BFHcharts 0.2.0 |
| Large dataset performance | Known | Downsample/filter/cache | BFHcharts future |

---

## Future Improvements

### BFHcharts 0.2.0 (Anticipated)

Expected to address:
- X̄ and S chart support
- Better centerline override handling
- Improved performance with large datasets
- Enhanced comment rendering

**SPCify Action:** Update wrapper layer to use new features automatically

---

### BFHcharts Roadmap Dependency

SPCify's limitations are directly tied to BFHcharts development:

- **If BFHcharts adds X̄/S support:** SPCify adds automatically (no feature flag needed)
- **If BFHcharts improves performance:** SPCify benefits automatically
- **If BFHcharts API changes:** SPCify wrapper layer updates

---

## Testing Limitations

### Snapshot Testing with BFHcharts

**Limitation:** Shinytest2 snapshots may differ between BFHcharts versions.

**Workaround:**

```r
# Use high pixel threshold tolerance (10%) for BFHcharts snapshots
app$expect_screenshot(
  selector = "#spc_plot",
  name = "bfh-run-chart",
  threshold = 0.1  # 10% tolerance for anti-aliasing, font rendering
)
```

---

## Migration Notes

For teams using SPCify with pure BFHcharts implementation:

1. **Code changes** - No feature flag branching needed (simplified)
2. **Chart types** - Limited to: run, i, p, c, u (explicit errors for others)
3. **Testing** - Use snapshot thresholds of 10% for visual tolerance
4. **Performance** - Generally equivalent to qicharts2, with caching benefit
5. **Styling** - BFHcharts styling is intentional (hospital branding maintained)

---

## Escalation Path

If you encounter limitations not listed here:

1. **Check this document** - Most limitations documented with workarounds
2. **Review MANUAL_TEST_GUIDE.md** - Specific test procedures
3. **Check BFHCHARTS_MIGRATION_GUIDE.md** - Architecture overview
4. **Review GitHub issues** - Known issues and discussions
5. **Create new issue** - If not found above

**Include in issue:**
- Specific limitation encountered
- Steps to reproduce
- Expected vs actual behavior
- Screenshot/error message
- Data sample (if applicable)

---

**Last Updated:** 2025-10-17
**BFHcharts Version:** 0.1.0
**Next Review:** When BFHcharts 0.2.0 released
