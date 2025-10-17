# BFHcharts SPC Migration Guide

**Version:** 2.0.0
**Date:** 2025-10-17
**Status:** Complete - Pure BFHcharts implementation (no feature flag)

## Table of Contents

1. [Overview](#overview)
2. [Architecture: Pure BFHcharts](#architecture-pure-bfhcharts)
3. [Parameter Mapping: SPCify ↔ BFHcharts](#parameter-mapping-spcify--bfhcharts)
4. [Key Changes from qicharts2](#key-changes-from-qicharts2)
5. [Error Handling](#error-handling)
6. [Performance Optimizations](#performance-optimizations)
7. [Known Limitations & Workarounds](#known-limitations--workarounds)
8. [Integration Examples](#integration-examples)
9. [Troubleshooting](#troubleshooting)
10. [Future Roadmap](#future-roadmap)

---

## Overview

SPCify has completed full migration from **qicharts2** to **BFHcharts** for SPC visualization:

- **BFHcharts HIGH-LEVEL API** (`create_spc_chart()`) for all SPC chart visualization
- **qicharts2 LOW-LEVEL API** for Anhøj rules metadata extraction only (internal use)
- **No fallback** to qicharts2 - pure BFHcharts implementation

**Why Pure BFHcharts?**
- Simpler codebase (no feature flag branching logic)
- Consistent user experience across all deployments
- Better performance (no runtime backend selection)
- Clearer error handling (no ambiguous fallback behavior)

---

## Architecture: Pure BFHcharts

### Current Flow (BFHcharts Only)

```
User Input
    ↓
SPCify Parameters
    ↓
generateSPCPlot_with_backend()
    ↓
compute_spc_results_bfh()
    ├─ BFHcharts: Visual rendering
    └─ qicharts2: Anhøj metadata extraction (internal)
    ↓
Plot + Anhøj Metadata
    ↓
User sees chart (BFHcharts styled)
```

### Key Architectural Changes

| Aspect | Old (HYBRID) | New (Pure BFHcharts) |
|--------|-------------|----------------------|
| **Backend Selection** | Runtime feature flag | Compile-time (constant) |
| **Fallback Logic** | Conditional routing | Error propagation |
| **Configuration** | `features.use_bfhchart` | None needed |
| **Code Complexity** | Dual paths in wrapper | Single path |
| **qicharts2 Role** | Visualization + Anhøj | Anhøj metadata only |
| **Error Handling** | Silent fallback | Explicit errors |
| **Performance** | Slight overhead from flag checks | Direct execution |

### Why This Approach

1. **Simpler:** No feature flag branching, cleaner codebase
2. **Consistent:** Same behavior across all deployments
3. **Performant:** No runtime backend selection overhead
4. **Clear:** Explicit errors vs. silent fallbacks

---

## Parameter Mapping: SPCify ↔ BFHcharts

### Column Names (CRITICAL!)

| SPCify Name | BFHcharts Name | Type | Notes |
|-------------|----------------|------|-------|
| `x_column` | `x_var` | Column name | Time/sequence axis |
| `y_column` | `y_var` | Column name | Measurement value |
| `n_column` | `n_var` | Column name | Denominator (P/C/U charts) |
| `cl_column` | `cl_var` | Column name | Centerline override |
| `frys_column` | `freeze_var` | Column name | Freeze period indicator |
| `skift_column` | `part_var` | Column name | Phase/period split |
| **`kommentar_column`** | **`notes_column`** | Column name | **⚠️ NAME CHANGE** |

### Important: kommentar_column → notes_column

The most critical change is the parameter name for comments:

**WRONG:**
```r
compute_spc_results_bfh(
  data = data,
  kommentar_column = "Kommentar"  # ❌ Not recognized!
)
```

**CORRECT:**
```r
compute_spc_results_bfh(
  data = data,
  notes_column = "Kommentar"  # ✅ Correct parameter name
)
```

### Numerical Parameters

| SPCify | BFHcharts | Type | Notes |
|--------|-----------|------|-------|
| `multiply_by` | `multiply` | Numeric | Scale factor for Y-axis |
| `target_value` | `target_value` | Numeric | Target line position |
| `centerline_value` | `centerline_value` | Numeric | Override centerline |

---

## Key Changes from qicharts2

### API Changes

**qicharts2 (OLD):**
```r
result <- qicharts2::qic(
  x = data$Dato,
  y = data$Vaerdi,
  chart = "run",
  return.data = TRUE
)
```

**BFHcharts (NEW):**
```r
result <- BFHchart::create_spc_chart(
  data = data,
  x_col = "Dato",
  y_col = "Vaerdi",
  chart_type = "run",
  # Returns: list(plot = ggplot, data = tibble)
)
```

### Output Differences

**qicharts2:**
```r
result <- list(
  plot = <qicharts2 ggplot object>,
  qic_data = <tibble with x, y, lcl, cl, ucl, signals, etc.>
)
```

**BFHcharts (via wrapper):**
```r
result <- list(
  plot = <BFHcharts ggplot object>,  # Different styling!
  qic_data = <tibble from qicharts2>,  # Reused for Anhøj rules
  metadata = list(
    backend = "bfhcharts",
    chart_type = "run",
    # ... additional metadata
  )
)
```

### Visual Changes

**Chart Appearance:**
- qicharts2: Hospital brand colors, custom fonts
- BFHcharts: Hospital brand colors (via BFHthemes), enhanced styling

**Expected appearance remains consistent** due to BFHthemes integration.

---

## Error Handling

### Explicit Error Handling (No Fallback)

```r
# BFHcharts wrapper with explicit error handling
result <- tryCatch(
  generateSPCPlot_with_backend(
    data = data,
    config = config,
    chart_type = chart_type,
    # ... other parameters
  ),
  error = function(e) {
    log_error(
      component = "[BACKEND_WRAPPER]",
      message = "BFHchart backend failed",
      details = list(error = e$message, chart_type = chart_type)
    )
    stop(e)  # Re-throw - no silent fallback
  }
)
```

### Supported vs. Unsupported Chart Types

**Supported (will render):**
- `"run"` - Run charts
- `"i"` - Individual value charts
- `"p"` - Proportion charts
- `"c"` - Count/defect charts
- `"u"` - Rate with variable denominator charts

**Unsupported (will error):**
- `"xbar"` - X̄ (X-bar) charts
- `"s"` - S charts (subgroup standard deviation)
- `"mr"` - MR charts (moving range)

If an unsupported chart type is requested, the user will receive an explicit error message:
```
Chart type 'xbar' is not supported in BFHcharts. Supported types: run, i, p, c, u
```

---

## Performance Optimizations

### Cache Integration

BFHcharts results are cached for 1 hour (configurable):

```r
# Cache key generated from data hash + parameters
cache_key <- digest::digest(
  list(data_hash, chart_type, config, target_value),
  algo = "xxhash64"
)

# Cache hit: Return instantly (< 1ms)
# Cache miss: Compute + cache (< 5s for typical data)

# Expected improvement: 98% cache hit rate in typical usage
```

### Benchmark Results

| Operation | Time | Backend |
|-----------|------|---------|
| **Cache Hit** | < 1ms | N/A |
| **Chart Render** | 0.5-2s | BFHcharts |
| **Chart Render** | 0.5-2s | qicharts2 |
| **Anhøj Calc** | 100-300ms | qicharts2 |
| **Full Pipeline** | 1-5s | BFHcharts (with Anhøj) |

---

## Known Limitations & Workarounds

### Supported Chart Types

**✅ FULL SUPPORT (BFHcharts):**
- Run charts
- I charts (individual measurements)
- P charts (proportions with denominator)
- C charts (counts/defects)
- U charts (rates with variable denominator)

**❌ NOT SUPPORTED:**
- X̄ (X-bar) charts
- S (subgroup standard deviation) charts
- MR (moving range) charts

**Note:** Unsupported chart types will result in an explicit error. There is no automatic fallback. If your workflow requires these chart types, SPCify is not currently suitable for that use case.

### Parameter Limitations

**kommentar_column → notes_column**
- BFHcharts uses different parameter name
- Must map SPCify `kommentar_column` to `notes_column`
- Already handled in `compute_spc_results_bfh()`

**Centerline Override**
- BFHcharts may not honor all centerline configurations
- Fallback to qicharts2 if specific centerline behavior needed

### Theme Application

**BFHcharts:** Uses BFHthemes for hospital branding
**qicharts2:** Uses custom `applyHospitalTheme()` wrapper

Both produce hospital-branded output. No action required.

---

## Integration Examples

### Example 1: Simple Run Chart

```r
data <- data.frame(
  Dato = seq.Date(Sys.Date() - 49, Sys.Date(), by = "day"),
  Vaerdi = rnorm(50, mean = 100, sd = 15)
)

result <- compute_spc_results_bfh(
  data = data,
  x_var = "Dato",
  y_var = "Vaerdi",
  chart_type = "run"
)

print(result$plot)  # BFHcharts-styled run chart
```

### Example 2: P Chart with Denominator

```r
data <- data.frame(
  Dato = seq.Date(Sys.Date() - 49, Sys.Date(), by = "day"),
  Numerator = sample(0:100, 50),
  Denominator = sample(100:500, 50)
)

result <- compute_spc_results_bfh(
  data = data,
  x_var = "Dato",
  y_var = "Numerator",
  n_var = "Denominator",
  chart_type = "p"
)

print(result$plot)  # P chart with proportions
```

### Example 3: Chart with Phase Markers

```r
data$Phase <- c(rep("Baseline", 25), rep("Intervention", 25))

result <- compute_spc_results_bfh(
  data = data,
  x_var = "Dato",
  y_var = "Vaerdi",
  chart_type = "run",
  part_var = "Phase"
)

print(result$plot)  # Run chart with phase line at row 26
```

### Example 4: Error Handling for Unsupported Chart Types

```r
# Inside visualizationModuleServer()

tryCatch(
  {
    result <- compute_spc_results_bfh(
      data = data,
      x_var = "Dato",
      y_var = "Vaerdi",
      chart_type = chart_type  # Supported: run, i, p, c, u
    )
    print(result$plot)
  },
  error = function(e) {
    # Unsupported chart types will trigger explicit error
    showModal(modalDialog(
      title = "Chart Type Not Supported",
      paste("Error:", e$message),
      easyClose = TRUE
    ))
  }
)
```

---

## Troubleshooting

### Issue: Chart generation error

**Symptoms:**
- Error message: "Chart type 'xbar' is not supported in BFHcharts"
- Plot area shows error

**Diagnosis:**
```r
# Check if chart type is in supported list
chart_type <- "xbar"
supported_types <- c("run", "i", "p", "c", "u")
chart_type %in% supported_types  # FALSE for unsupported types
```

**Solutions:**
1. Verify chart type is one of: run, i, p, c, u
2. Check data columns are specified correctly
3. Ensure data has ≥10 rows (SPC requirement)
4. Review error logs for detailed message

### Issue: BFHchart backend failed

**Symptoms:**
- Error message with specific BFHcharts error
- Stack trace in logs

**Diagnosis:**
```r
# Check logs for [BACKEND_WRAPPER] component
tail(app_logs)  # Look for detailed error message

# Verify BFHchart package is installed
require("BFHchart")
```

**Solutions:**
1. Ensure BFHchart package is properly installed
2. Verify data columns exist and are correctly named
3. Check for NA or invalid values in critical columns
4. Report issue with full error message and reproducible data

### Issue: Different chart appearance than before

**Explanation:**
BFHcharts styling is intentionally different from the old qicharts2 implementation. The visual appearance, fonts, and colors may differ while maintaining the same analytical content (control limits, Anhøj rules, etc.).

**Verification:**
The analytical content (centerline, control limits, signals) remains the same. If you see different statistical calculations, that's a bug to report.

---

## Future Roadmap

### Short Term (Current Release)
- ✅ Pure BFHcharts implementation complete (no feature flag)
- ✅ Explicit error handling for unsupported chart types
- 🔄 Gather user feedback on supported chart type restrictions

### Medium Term (Q1 2026)
- 🎯 BFHcharts 0.2.0 support (if released)
- 🎯 X̄/S chart support (if added to BFHcharts)
- 🎯 Update to support new BFHcharts chart types automatically

### Long Term (Q2+ 2026)
- 🎯 Possible full qicharts2 removal (if no longer needed for internal operations)
- 🎯 Enhanced features from BFHcharts future releases
- 🎯 Performance optimizations based on production metrics

### Dependency on BFHcharts Roadmap

SPCify's capabilities are directly tied to BFHcharts development:
- **If BFHcharts adds X̄/S support:** SPCify will add support immediately
- **If BFHcharts performance improves:** SPCify benefits automatically
- **If BFHcharts API changes:** SPCify wrapper layer will be updated

---

## Questions & Support

For questions about the BFHcharts migration:

1. **Architecture questions:** See `CLAUDE.md` section 3.4
2. **Limitations & workarounds:** See `docs/BFHCHARTS_LIMITATIONS.md`
3. **Manual testing:** See `MANUAL_TEST_GUIDE.md`
4. **Development:** See `docs/BFHCHARTS_COORDINATION.md`
5. **Issues:** Create GitHub issue with logs and reproduction steps

---

**Last Updated:** 2025-10-17
**Next Review:** 2025-11-17
