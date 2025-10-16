# qicharts2 Regression Baselines

This directory contains regression baseline data for validating the migration from qicharts2 to BFHchart (Issue #29).

## Purpose

These baselines capture the **exact** outputs from qicharts2 for all supported chart types and scenarios. During BFHchart migration, we'll compare BFHchart outputs against these baselines to ensure:

1. **Numerical accuracy**: Control limits, center lines, and Anhøj rules match exactly
2. **Visual consistency**: Plot outputs are visually equivalent
3. **No regressions**: Edge cases continue to work correctly

## Structure

Each baseline file is an RDS object containing:

```r
list(
  chart_type = "run",              # Chart type code
  scenario = "basic",              # Scenario name
  input_data = tibble(...),        # Input data used
  qic_output = list(
    plot_data = tibble(...),       # Data frame from return.data = TRUE
    control_limits = list(         # Extracted control limits
      ucl = c(...),
      lcl = c(...)
    ),
    center_line = c(...),          # Center line values
    anhoej_rules = list(           # Anhøj rules results
      runs_signal = TRUE/FALSE,
      n_crossings = numeric,
      n_crossings_min = numeric
    ),
    ggplot_object = ggplot(...)    # Complete plot object
  ),
  metadata = list(                 # Capture metadata
    captured_date = Date,
    qicharts2_version = "x.y.z",
    r_version = "R version x.y.z",
    seed = 20251015
  )
)
```

## Chart Types Covered (7)

1. **run** - Run chart (serial plot with median center line)
2. **i** - I chart (individuals chart)
3. **p** - P chart (proportions)
4. **c** - C chart (counts)
5. **u** - U chart (rates)
6. **xbar** - X̄ chart (means)
7. **s** - S chart (standard deviation)

## Scenarios per Chart (3)

### 1. Basic
- **Purpose**: Stable process, no special rules violations
- **Data**: 36 monthly observations with normal variation
- **Expected**: No Anhøj violations, stable control limits

### 2. Anhøj
- **Purpose**: Triggers Anhøj rules (runs ≥8 or insufficient crossings)
- **Data**: 18 points above median, 18 below (creates runs violation)
- **Expected**: Anhøj.signal = TRUE, dashed center line

### 3. Freeze
- **Purpose**: Baseline freeze period functionality
- **Data**: 24 baseline points, freeze at month 24, then 12 new points
- **Expected**: Control limits calculated from baseline only, second part uses frozen limits

## Total Baselines

7 chart types × 3 scenarios = **21 baseline files**

## File Naming Convention

Format: `{chart_type}-{scenario}.rds`

Examples:
- `run-basic.rds`
- `p-anhoej.rds`
- `u-freeze.rds`

## Deterministic Generation

All baselines are **fully deterministic**:
- Fixed seed: `set.seed(20251015)`
- Fixed dates: Monthly from 2022-01-01
- Fixed data patterns: Controlled random generation for reproducibility

## Usage in Tests

### Load a baseline:

```r
baseline <- readRDS(here::here("tests/testthat/fixtures/qic-baseline/run-basic.rds"))
```

### Access components:

```r
# Input data
input_data <- baseline$input_data

# qicharts2 output
qic_output <- baseline$qic_output$plot_data

# Control limits
ucl <- baseline$qic_output$control_limits$ucl
lcl <- baseline$qic_output$control_limits$lcl

# Center line
cl <- baseline$qic_output$center_line

# Anhøj rules
runs_violation <- baseline$qic_output$anhoej_rules$runs_signal
```

### Regression test pattern:

```r
test_that("BFHchart matches qicharts2 baseline for run-basic", {
  # Load baseline
  baseline <- readRDS(here::here("tests/testthat/fixtures/qic-baseline/run-basic.rds"))

  # Run BFHchart with same input
  bfh_result <- bfhchart_wrapper(
    data = baseline$input_data,
    chart_type = baseline$chart_type
  )

  # Compare outputs
  expect_equal(bfh_result$control_limits$ucl, baseline$qic_output$control_limits$ucl, tolerance = 1e-6)
  expect_equal(bfh_result$control_limits$lcl, baseline$qic_output$control_limits$lcl, tolerance = 1e-6)
  expect_equal(bfh_result$center_line, baseline$qic_output$center_line, tolerance = 1e-6)
  expect_equal(bfh_result$anhoej_signal, baseline$qic_output$anhoej_rules$runs_signal)
})
```

## Regeneration

To regenerate all baselines (e.g., if qicharts2 version changes):

```bash
cd tests/testthat/fixtures/qic-baseline
Rscript capture_baselines.R
```

**Warning**: Only regenerate baselines if:
1. qicharts2 version has changed
2. You want to update the reference implementation
3. You've validated that outputs are still correct

## Verification

All baselines include verification checks:
- RDS file is loadable
- Has correct structure (input_data, qic_output, metadata)
- Contains all expected components

Run verification:

```r
source(here::here("tests/testthat/fixtures/qic-baseline/capture_baselines.R"))
# See verification output at end of script
```

## Metadata

- **Captured date**: 2025-10-15
- **qicharts2 version**: *(captured in each baseline's metadata)*
- **R version**: *(captured in each baseline's metadata)*
- **Seed**: 20251015
- **Data points per baseline**: 36 (monthly observations)

## Related Documentation

- Issue #29: Feasibility Analysis & BFHchart Bootstrap
- Epic: bfhcharts-spc-migration
- ADR-015: BFHchart Migration Strategy

## Notes

1. **Baseline stability**: These baselines should remain **unchanged** once captured unless qicharts2 version changes
2. **Tolerance**: Use `tolerance = 1e-6` for floating-point comparisons in tests
3. **Visual regression**: The `ggplot_object` can be used for visual diff testing
4. **Chart type coverage**: Covers all SPCify-supported chart types except MR and G charts (not currently used in production)

---

*Generated by Stream B - Regression Baseline Capture (Issue #29)*
