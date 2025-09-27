# Troubleshooting: Valueboxes stuck på "Beregner..."

## Problem Description

Valueboxes (fx "Serielængde", "Kontrolgrænser") forbliver på "Beregner..." og viser aldrig de beregnede SPC-værdier.

## Root Cause Analysis

**Typisk årsag:** SPC data generation pipeline fejler og `anhoej_results` forbliver `NULL`.

### Common Causes:

1. **Logging API fejl** - Invalid function signatures kaster "unused arguments" errors
2. **Microbenchmark fejl** - Benchmark wrapper fejler og afbryder QIC generation
3. **QIC generation fejl** - qicharts2 kald fejler pga. data issues
4. **Reactive chain interruption** - Observer fejl mellem data loading og result processing

## Debugging Steps

### Step 1: Check SPC Pipeline Logs

Søg efter disse log patterns:

```
# QIC generation start
"[SPC_PIPELINE] qic_startup_call"

# Benchmark issues (if enabled)
"Starting microbenchmark for: qic_run_"
"ERROR: Microbenchmark failed"

# Critical pipeline failure
"Generate SPC data using qicharts2 fejlede"
"Setting anhoej_results to NULL"

# Valuebox state
"longest_run_box showing 'Beregner...' - anhoej_exists= FALSE"
```

### Step 2: Check Reactive Value State

I browser console eller R debugger:

```r
# Check if anhoej_results is NULL
print(paste("anhoej_results exists:", !is.null(anhoej_results())))

# Check qic_data availability
print(paste("qic_data exists:", !is.null(app_state$data$current_data)))

# Check plot_ready state
print(paste("plot_ready:", plot_ready()))
```

### Step 3: Test QIC Generation Directly

Isoler QIC generation:

```r
# Test manual QIC call
test_data <- data.frame(
  x = 1:20,
  y = rnorm(20, 50, 10)
)

qic_result <- qicharts2::qic(
  x = test_data$x,
  y = test_data$y,
  chart = "run"
)

print("QIC succeeded:", !is.null(qic_result))
```

## Quick Fixes

### 1. Disable Microbenchmarking (Production)

```r
# In run_app() or before app start
options(spc.benchmark_enabled = FALSE)
```

### 2. Force QIC Regeneration

```r
# Reset anhoej computation
app_state$columns$auto_detect$completed <- FALSE
emit$data_updated("force_refresh")
```

### 3. Check Feature Flags

```r
# Verify test mode settings
print(Sys.getenv("TEST_MODE_AUTO_LOAD"))
print(getOption("spc.benchmark_enabled"))
```

## Prevention

### 1. Robust Error Handling

Wrap all SPC generation in `safe_operation()`:

```r
qic_result <- safe_operation(
  "Generate SPC plot",
  code = { generateSPCPlot(data, config, chart_type) },
  fallback = function(e) {
    log_error(paste("SPC generation failed:", e$message))
    return(create_fallback_plot_list())
  }
)
```

### 2. Logging API Compliance

Ensure all logging calls use correct API:

```r
# ✅ Correct
log_error("Message", .context = "COMPONENT")
log_debug_kv(key = value, .context = "COMPONENT")

# ❌ Incorrect (causes "unused arguments")
log_error(component = "COMPONENT", message = "Message", details = list())
```

### 3. Feature Flag Gating

Gate expensive/risky operations:

```r
if (isTRUE(getOption("spc.feature_enabled", TRUE))) {
  # risky operation
} else {
  # safe fallback
}
```

## Test Cases

Verificer med disse tests:

```r
# Test 1: QIC generation without benchmarking
options(spc.benchmark_enabled = FALSE)
R -e "source('global.R'); testthat::test_file('tests/testthat/test-microbenchmark-fallback.R')"

# Test 2: Complete SPC pipeline
R -e "source('global.R'); testthat::test_file('tests/testthat/test-e2e-workflows.R')"

# Test 3: Anhoej calculations
R -e "source('global.R'); testthat::test_file('tests/testthat/test-mod-spc-chart-comprehensive.R')"
```

## Historical Fixes

- **2025-09-27**: Fixed microbenchmark logging API "unused arguments" error (commit 809ae31)
- **2025-09-26**: Implemented startup optimization preventing early failures
- **2025-09-25**: Added comprehensive error handling in SPC pipeline

## Related Files

- `R/mod_spc_chart_server.R` - Valuebox rendering logic (lines 733-747)
- `R/fct_spc_plot_generation.R` - QIC generation with benchmarking (lines 364-386)
- `R/utils_microbenchmark.R` - Benchmark wrapper and logging
- `tests/testthat/test-microbenchmark-fallback.R` - Regression prevention tests