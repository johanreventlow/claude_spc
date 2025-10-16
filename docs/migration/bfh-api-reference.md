# BFHchart API Reference for SPCify Migration

**Document Version:** 1.0
**Date:** 2025-10-15
**Purpose:** Map qicharts2 API patterns to expected BFHchart API for service layer design
**Context:** BFHchart is extracted from SPCify - API may need refinement during reintegration

---

## Overview

This document defines the **expected API** for BFHchart based on current qicharts2 usage patterns in SPCify. Since BFHchart was extracted from SPCify's visualization code, the API should closely mirror these patterns but may require adaptation.

**Usage:** This reference guides:
1. Phase 3: Service layer facade design (`compute_spc_results_bfh()`)
2. BFHchart validation: Confirm actual API matches expectations
3. Adapter development: Handle API differences between qicharts2 and BFHchart

---

## Core Function Signature (Expected)

### Primary Interface

```r
# Expected BFHchart main function
# (Actual name/signature to be validated)
bfhchart::spc_chart(
  data,                    # data.frame - Input dataset
  x,                       # column name/symbol - X-axis
  y,                       # column name/symbol - Y-axis
  n = NULL,                # column name/symbol - Denominator (optional)
  chart = "run",           # character - Chart type
  freeze = NULL,           # integer - Baseline freeze position
  phases = NULL,           # integer vector - Phase boundaries (or "part")
  target = NULL,           # numeric - Target value
  centerline = NULL,       # numeric - Custom centerline (or "cl")
  return_data = FALSE      # logical - Return data vs plot
)
```

**Naming Conventions to Validate:**
- `phases` vs `part` for phase boundaries
- `centerline` vs `cl` for centerline override
- `return_data` vs `return.data` (R naming style)

---

## Parameter Mappings

### qicharts2 → BFHchart Expected Mappings

| qicharts2 Parameter | Expected BFHchart | Type | Notes |
|---------------------|-------------------|------|-------|
| `data` | `data` | data.frame | Input dataset |
| `x` | `x` | name/symbol | X-axis column (NSE or string?) |
| `y` | `y` | name/symbol | Y-axis column |
| `n` | `n` | name/symbol | Denominator column (optional) |
| `chart` | `chart` or `type` | character | Chart type code |
| `freeze` | `freeze` or `baseline` | integer | Baseline freeze position |
| `part` | `phases` or `parts` | integer vector | Phase boundary positions |
| `target` | `target` | numeric | Target value |
| `cl` | `centerline` or `cl` | numeric | Custom centerline |
| `return.data` | `return_data` | logical | Return data vs plot |
| `title` | `title` | character | Plot title (if needed) |
| `ylab` | `ylab` | character | Y-axis label (if needed) |
| `xlab` | `xlab` | character | X-axis label (if needed) |

**Critical Validations:**
- [ ] Confirm NSE (non-standard evaluation) vs standard strings
- [ ] Validate parameter naming conventions
- [ ] Test default values match expectations

---

## Chart Type Codes

### Supported Chart Types

SPCify requires these 9 chart types:

| Danish UI Label | English Code | BFHchart Support |
|-----------------|--------------|------------------|
| Seriediagram med SPC (Run Chart) | `run` | ✅ Expected |
| I-kort (Individuelle værdier) | `i` | ✅ Expected |
| MR-kort (Moving Range) | `mr` | ✅ Expected |
| P-kort (Andele) | `p` | ✅ Expected |
| P'-kort (Andele, standardiseret) | `pp` | ⚠️ Validate |
| U-kort (Rater) | `u` | ✅ Expected |
| U'-kort (Rater, standardiseret) | `up` | ⚠️ Validate |
| C-kort (Tællinger) | `c` | ✅ Expected |
| G-kort (Tid mellem hændelser) | `g` | ✅ Expected |

**Validation Checklist:**
- [ ] Confirm all 9 chart types supported
- [ ] Test prime charts (pp, up) separately
- [ ] Document any missing chart types
- [ ] Validate chart type string matching (case-sensitive?)

---

## Return Value Structure

### Expected Output Format

**Option A: Return Data (like qicharts2 `return.data = TRUE`)**

```r
# Expected data.frame structure
bfh_data <- bfhchart::spc_chart(..., return_data = TRUE)

str(bfh_data)
# $ x : Date/numeric - X-axis values
# $ y : numeric - Y-axis values (original data)
# $ cl : numeric - Centerline per point
# $ ucl : numeric - Upper control limit
# $ lcl : numeric - Lower control limit
# $ part : integer - Phase indicator (1, 2, 3, ...)
# $ runs : logical - Runs rule violation (per point)
# $ crossings : integer - Crossings count (per phase)
# $ target : numeric - Target value (if specified)
```

**Critical Columns:**
- `x`, `y`: Original data values
- `cl`, `ucl`, `lcl`: Control limits per point (may change per phase)
- `part`: Phase grouping variable
- **Anhøj Rules:** `runs` signal and `crossings` data

**Validation:**
- [ ] Confirm column names match expectations
- [ ] Validate per-point vs per-phase values
- [ ] Test with phases (part column should increment)
- [ ] Check for additional metadata columns

---

**Option B: Return ggplot Object**

```r
# Expected ggplot object
bfh_plot <- bfhchart::spc_chart(..., return_data = FALSE)

class(bfh_plot)
# [1] "gg" "ggplot"
```

**Layer Compatibility:**
- SPCify must add layers: comments, extended lines, custom labels, theme
- **Critical:** Validate ggplot2 layers can be added without breaking

**Validation:**
- [ ] Confirm returns ggplot object
- [ ] Test adding `+ ggrepel::geom_text_repel()`
- [ ] Test adding `+ custom_theme()`
- [ ] Validate plot data accessible via `ggplot_build()`

---

## Integration Patterns

### Pattern 1: NSE (Non-Standard Evaluation) - qicharts2 Style

```r
# qicharts2 uses NSE (unquoted column names)
qic_data <- qicharts2::qic(
  data = df,
  x = Dato,         # Unquoted
  y = `Tæller`,     # Backticks for special chars
  n = `Nævner`,
  chart = "run"
)
```

**If BFHchart uses NSE:**
```r
# SPCify adapter using rlang/tidy eval
bfh_data <- bfhchart::spc_chart(
  data = df,
  x = !!rlang::sym(x_col_name),   # Convert string to symbol
  y = !!rlang::sym(y_col_name),
  n = if (!is.null(n_col_name)) !!rlang::sym(n_col_name) else NULL,
  chart = chart_type
)
```

**Validation:**
- [ ] Confirm BFHchart evaluation style (NSE vs standard)
- [ ] Test with backtick column names (`Tæller`, `Nævner`)
- [ ] Test with NULL denominator (n = NULL)

---

### Pattern 2: Standard Evaluation - String-Based

```r
# Alternative: BFHchart uses string column names
bfh_data <- bfhchart::spc_chart(
  data = df,
  x = "Dato",       # Quoted strings
  y = "Tæller",
  n = "Nævner",
  chart = "run"
)
```

**If BFHchart uses strings:**
```r
# SPCify adapter - simpler!
bfh_data <- bfhchart::spc_chart(
  data = df,
  x = x_col_name,   # Already strings
  y = y_col_name,
  n = n_col_name,
  chart = chart_type
)
```

**Validation:**
- [ ] Test string-based column specification
- [ ] Validate handles NULL denominator
- [ ] Test with Danish characters (æøå) in column names

---

### Pattern 3: Freeze & Phase Combination

```r
# qicharts2 pattern: freeze + part together
qic_data <- qicharts2::qic(
  data = df,
  x = Dato,
  y = `Tæller`,
  chart = "run",
  freeze = 12,        # Baseline up to row 12
  part = c(12, 24)    # Phases start at rows 12, 24
)
```

**Expected BFHchart behavior:**
- Control limits calculated using rows 1-12 (freeze)
- Applied to all data points (not recalculated per phase)
- OR: Phases override freeze (recalculate limits per phase)

**Validation:**
- [ ] Test freeze alone (no phases)
- [ ] Test phases alone (no freeze)
- [ ] Test freeze + phases combination
- [ ] Document interaction logic (freeze precedence?)

---

### Pattern 4: Target & Centerline Override

```r
# qicharts2 pattern: custom target and centerline
qic_data <- qicharts2::qic(
  data = df,
  x = Dato,
  y = `Tæller`,
  n = `Nævner`,
  chart = "run",
  target = 0.75,      # Target line at 75% (decimal scale)
  cl = 0.80           # Override centerline to 80%
)
```

**SPCify normalization:** Converts percentage input to decimal for qicharts2:

```r
# If chart = "run" with denominator AND target > 1
if (chart_type == "run" && !is.null(n_col_name) && target_value > 1) {
  adjusted_target <- target_value / 100  # 75 → 0.75
}
```

**Validation:**
- [ ] Confirm BFHchart target/centerline scale expectations
- [ ] Test with percentage input (75 vs 0.75)
- [ ] Validate decimal conversion needed or not
- [ ] Document scale convention per chart type

---

## Data Preparation Requirements

### Input Data Cleaning (SPCify Responsibility)

**Before calling BFHchart:**

1. **NA Removal:**
   ```r
   complete_cases <- complete.cases(x_data, y_data)
   clean_data <- data[complete_cases, ]
   ```

2. **Part/Freeze Position Adjustment:**
   ```r
   # Adjust positions for removed rows
   removed_positions <- which(!complete_cases)
   adjusted_part <- part_positions - sapply(part_positions, function(pos) {
     sum(removed_positions < pos)
   })
   ```

3. **Row ID Injection (for comment mapping):**
   ```r
   clean_data$.original_row_id <- 1:nrow(clean_data)
   ```

**Question for BFHchart:**
- Does BFHchart handle NA removal internally?
- Does BFHchart preserve `.original_row_id` column in output?
- Should SPCify pre-clean data or let BFHchart handle it?

**Validation:**
- [ ] Test BFHchart with NA values in input
- [ ] Verify `.original_row_id` preservation
- [ ] Document NA handling responsibility

---

### Danish Number Parsing (SPCify Utility)

**Conversion logic:**
```r
# Parse Danish decimal format (comma as decimal separator)
parse_danish_number <- function(x) {
  if (is.numeric(x)) return(x)

  # "1.234,56" → 1234.56
  x <- gsub("\\.", "", x)       # Remove thousand separators
  x <- gsub(",", ".", x)        # Replace comma with period
  as.numeric(x)
}
```

**Question for BFHchart:**
- Does BFHchart handle Danish number format internally?
- Should SPCify pre-parse numbers before calling BFHchart?

**Recommendation:** SPCify parses numbers (already implemented in multiple utils).

---

## Error Handling & Safe Operation

### SPCify Integration Pattern

**All BFHchart calls wrapped in `safe_operation()`:**

```r
bfh_data <- safe_operation(
  "Generate SPC data using BFHchart",
  code = {
    bfhchart::spc_chart(
      data = clean_data,
      x = x_col_name,
      y = y_col_name,
      chart = chart_type,
      freeze = freeze_position,
      phases = part_positions
    )
  },
  fallback = function(e) {
    log_error(paste("BFHchart call failed:", e$message), "BFH_SERVICE")
    stop("SPC calculation failed: ", e$message)
  },
  error_type = "processing"
)
```

**Logging Requirements:**
- Component tag: `[BFH_SERVICE]` or `[SPC_RENDERER]`
- Log inputs: chart type, data dimensions, freeze/phase positions
- Log outputs: row count, presence of control limits, Anhøj signals
- Performance: execution time per call

**Validation:**
- [ ] Confirm BFHchart error messages are informative
- [ ] Test error handling for invalid inputs
- [ ] Validate BFHchart can be wrapped without issues

---

## Performance Considerations

### Caching Strategy (SPCify Responsibility)

**Current qicharts2 caching:**
```r
# Cache key includes data structure hash
cache_key <- digest::digest(list(
  data_dims = nrow(data),
  x_col = x_col_name,
  y_col = y_col_name,
  chart_type = chart_type,
  freeze = freeze_position,
  part = part_positions
), algo = "xxhash32")

# Check cache before calling qic()
cached_result <- get_cached_result(cache_key)
if (!is.null(cached_result)) return(cached_result$value)
```

**BFHchart caching:**
- Same cache key strategy should work
- Cache timeout: 300 seconds (5 minutes) default
- Cache BFHchart output data, not ggplot objects

**Validation:**
- [ ] Benchmark BFHchart performance vs qicharts2
- [ ] Test cache hit rate with BFHchart outputs
- [ ] Validate cache key uniqueness

---

### Expected Performance Targets

**qicharts2 Baseline:**
- < 500ms for datasets < 100 rows
- < 2s for datasets < 1000 rows
- Cache hit rate: ≥ 80%

**BFHchart Target:** ≤ qicharts2 performance (no regression)

**Benchmarking:**
```r
# Microbenchmark suite (Task 33: Performance & Observability)
bench::mark(
  qicharts2 = qicharts2::qic(...),
  bfhchart = bfhchart::spc_chart(...),
  iterations = 10,
  check = FALSE
)
```

**Validation:**
- [ ] Run performance comparison suite
- [ ] Identify bottlenecks in BFHchart
- [ ] Document optimization opportunities

---

## Facade Function Design (Task 30)

### Service Layer Interface

**SPCify facade function signature:**

```r
#' Compute SPC Results Using BFHchart
#'
#' Adapter function isolating BFHchart API from SPCify internals.
#' Handles data preparation, parameter normalization, and output formatting.
#'
#' @param data data.frame - Cleaned input data
#' @param config list - Column configuration (x_col, y_col, n_col)
#' @param chart_type character - qicharts2-style chart code
#' @param freeze_position integer - Baseline freeze (optional)
#' @param part_positions integer vector - Phase boundaries (optional)
#' @param target_value numeric - Target value (optional, SPCify scale)
#' @param centerline_value numeric - Custom centerline (optional, SPCify scale)
#' @param qic_cache environment - Cache environment (optional)
#'
#' @return list(bfh_data = data.frame, plot = ggplot|NULL)
#' @export
compute_spc_results_bfh <- function(
  data,
  config,
  chart_type,
  freeze_position = NULL,
  part_positions = NULL,
  target_value = NULL,
  centerline_value = NULL,
  qic_cache = NULL
) {
  # 1. Data preparation (NA removal, row IDs)
  # 2. Parameter normalization (scale conversion)
  # 3. Call BFHchart (wrapped in safe_operation)
  # 4. Post-process output (format standardization)
  # 5. Return consistent structure
}
```

**Adapter Responsibilities:**
1. Translate SPCify config → BFHchart parameters
2. Handle scale normalization (percentage → decimal)
3. Inject `.original_row_id` for comment mapping
4. Standardize output structure (match qicharts2 format)
5. Add Anhøj combined signal (if BFHchart doesn't provide)
6. Cache results with consistent key

---

## Validation Checklist Summary

### P0 (Blocking) Validations

- [ ] **Core API:** Confirm main function name and signature
- [ ] **Chart Types:** Validate all 9 types supported
- [ ] **Return Value:** Document data.frame structure with all required columns
- [ ] **Control Limits:** Confirm cl, ucl, lcl calculated per point/phase
- [ ] **Anhøj Rules:** Validate runs/crossings data exposed
- [ ] **ggplot2 Output:** Confirm ggplot object compatible with SPCify layers
- [ ] **Freeze/Phase:** Test interaction between freeze and phase parameters
- [ ] **Performance:** Benchmark against qicharts2 baseline

### P1 (High Priority) Validations

- [ ] **Parameter Naming:** Document deviations from qicharts2 conventions
- [ ] **NSE vs Strings:** Clarify column specification style
- [ ] **Target/Centerline Scale:** Validate decimal vs percentage expectations
- [ ] **NA Handling:** Confirm who handles missing data (BFHchart vs SPCify)
- [ ] **Row ID Preservation:** Test `.original_row_id` persistence
- [ ] **Error Messages:** Validate informative error output
- [ ] **Prime Charts:** Test P'/U' standardization

### P2 (Medium Priority) Validations

- [ ] **Danish Characters:** Test æøå in column names and data
- [ ] **Cache Compatibility:** Validate BFHchart outputs cacheable
- [ ] **Memory Usage:** Profile BFHchart memory footprint
- [ ] **Documentation:** Review BFHchart function documentation

---

## Next Steps

1. **Access BFHchart Source:**
   - Locate repository or installed package
   - Review main function(s) and exports
   - Read any existing documentation

2. **Run Validation Tests:**
   - Create test script with qicharts2 baseline
   - Run same inputs through BFHchart
   - Compare outputs (numeric + visual)

3. **Document Findings:**
   - Update this document with actual API
   - Note deviations from expectations
   - Create BFHchart issues for gaps

4. **Design Facade (Task 30):**
   - Use validated API to design `compute_spc_results_bfh()`
   - Plan adapter layer for API differences
   - Define output format standardization

---

## Appendix: qicharts2 Complete Example

### Full SPCify → qicharts2 Flow

```r
# 1. Prepare data
data <- clean_spc_data(raw_data)
data$.original_row_id <- 1:nrow(data)

# 2. Build arguments with NSE
qic_args <- list(
  data = data,
  x = as.name("Dato"),
  y = as.name("Tæller"),
  n = as.name("Nævner"),
  chart = "run",
  freeze = 12,
  part = c(12, 24),
  target = 0.75,  # Pre-normalized to decimal
  return.data = TRUE
)

# 3. Call qicharts2
qic_data <- do.call(qicharts2::qic, qic_args)

# 4. Post-process
qic_data$anhoej.signal <- qic_data$runs.signal | calculate_crossings_signal(qic_data)

# 5. Extract comments
comment_data <- extract_comment_data(data, "Kommentar", qic_data)

# 6. Build ggplot
plot <- ggplot(qic_data, aes(x = x, y = y)) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), fill = "#E6F5FD") +
  geom_line(aes(y = y), color = "#AEAEAE") +
  geom_line(aes(y = cl, linetype = anhoej.signal), color = "#003d73") +
  ggrepel::geom_text_repel(data = comment_data, aes(label = comment)) +
  applyHospitalTheme()

# 7. Return
list(plot = plot, qic_data = qic_data)
```

**Goal for BFHchart:** Replicate this flow with minimal changes to SPCify integration logic.

---

**Document Owner:** Claude Code
**Status:** Expected API - Pending BFHchart Validation
**Next Update:** After BFHchart source code review (Phase 1 completion)
