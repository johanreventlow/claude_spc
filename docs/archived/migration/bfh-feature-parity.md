# BFHchart Feature Parity Matrix

**Document Version:** 1.0
**Date:** 2025-10-15
**Status:** Initial Analysis
**Context:** BFHchart is an in-house R package **extracted from SPCify** - this is a reintegration exercise

## Executive Summary

This document analyzes feature parity between qicharts2 (current implementation) and BFHchart (extracted SPCify visualization code) across 11 critical feature areas. **Important Context:** BFHchart was derived from SPCify's own codebase, meaning most functionality should exist but may require API refinement during reintegration.

**Key Findings:**
- **Assumed Feature Parity:** Since BFHchart originated from SPCify, core functionality likely exists
- **Gap Risk:** Medium - API design and integration patterns need validation
- **Blocker Risk:** Low - We control both codebases and can enhance BFHchart as needed
- **Recommendation:** Proceed with phased validation, enhance BFHchart incrementally

---

## Feature Comparison Matrix

### 1. Chart Type Support

**qicharts2 Usage in SPCify:**
- Run charts (with/without denominator)
- I charts (individuals)
- MR charts (moving range)
- P charts (proportions)
- P' charts (standardized proportions)
- U charts (rates)
- U' charts (standardized rates)
- C charts (counts)
- G charts (time between events)

**Mapping:** Danish UI labels → English qicharts2 codes via `get_qic_chart_type()`

```r
CHART_TYPES_DA <- list(
  "Seriediagram med SPC (Run Chart)" = "run",
  "I-kort (Individuelle værdier)" = "i",
  "MR-kort (Moving Range)" = "mr",
  "P-kort (Andele)" = "p",
  "P'-kort (Andele, standardiseret)" = "pp",
  "U-kort (Rater)" = "u",
  "U'-kort (Rater, standardiseret)" = "up",
  "C-kort (Tællinger)" = "c",
  "G-kort (Tid mellem hændelser)" = "g"
)
```

**BFHchart Status:** **ASSUMED_PARITY** (needs validation)
- **Rationale:** BFHchart was extracted from SPCify, which already implements all 9 chart types
- **Gap Category:** WORKAROUND_BFHchart (if API differs)
- **Action:** Validate BFHchart API supports same chart types with equivalent parameters

**Validation Checklist:**
- [ ] Confirm BFHchart supports all 9 chart types
- [ ] Validate parameter naming conventions match or can be easily mapped
- [ ] Test edge cases (small n, missing data, single phase)
- [ ] Document any chart types requiring enhancement

---

### 2. Anhøj Rules

**qicharts2 Implementation:**

SPCify combines **runs** and **crossings** signals into unified `anhoej.signal`:

```r
# Runs signal (8+ consecutive points on same side of centerline)
runs_sig_col <- qic_data$runs.signal

# Crossings signal (too few crossings of centerline)
# Calculated per-part using vectorized dplyr group_by
qic_data <- qic_data |>
  dplyr::group_by(part) |>
  dplyr::mutate(
    part_n_cross = max(n.crossings, na.rm = TRUE),
    part_n_cross_min = max(n.crossings.min, na.rm = TRUE),
    crossings_signal = !is.na(part_n_cross) & !is.na(part_n_cross_min) &
      part_n_cross < part_n_cross_min
  ) |>
  dplyr::ungroup()

# Combined signal: TRUE if EITHER runs OR crossings violation
qic_data$anhoej.signal <- runs_sig_col | qic_data$crossings_signal
```

**Visualization:** Centerline styling changes based on `anhoej.signal`:
- `FALSE` → solid line (`"solid"`)
- `TRUE` → dashed line (`"12"`)

**BFHchart Status:** **VALIDATION_REQUIRED**
- **Gap Category:** WORKAROUND_BFHchart (if missing) / ACCEPTABLE (if algorithm differs)
- **Critical Question:** Does BFHchart expose runs/crossings data or calculate internally?
- **Action:**
  1. Check if BFHchart returns Anhøj rule results per data point
  2. If not: Implement Anhøj detection in SPCify service layer
  3. If yes but different: Validate clinical acceptance of algorithm differences

**Validation Checklist:**
- [ ] Confirm BFHchart calculates Anhøj rules (runs ≥8, crossings)
- [ ] Validate rule application is per-phase (not global)
- [ ] Test crossings calculation matches qicharts2 methodology
- [ ] Document any algorithm differences requiring clinical approval

---

### 3. Control Limit Calculation Methods

**qicharts2 Implementation:**

SPCify relies on qicharts2 for automatic control limit calculation based on chart type:

```r
# Control limits returned in qic_data data.frame
qic_data$ucl  # Upper control limit
qic_data$lcl  # Lower control limit
qic_data$cl   # Centerline (mean/median per phase)
```

**Per-Phase Limits:** Control limits recalculate automatically when `part` positions specified:

```r
qic_args$part <- part_positions  # Phase boundaries
qic_data$cl   # Changes per phase (part column)
```

**Visualization:**
- Ribbon between UCL/LCL with light blue fill (`#E6F5FD`)
- Text labels on control limit lines
- Centerline rendered with anhoej.signal styling

**BFHchart Status:** **ASSUMED_PARITY**
- **Rationale:** Control limit calculations are core SPC functionality extracted from SPCify
- **Gap Category:** BLOCKER (if missing) / ACCEPTABLE (if minor differences)
- **Action:** Validate BFHchart calculates limits identically to qicharts2 for all chart types

**Validation Checklist:**
- [ ] Compare control limit calculations across all chart types
- [ ] Validate per-phase recalculation logic
- [ ] Test edge cases (n<10, all values equal, extreme outliers)
- [ ] Document tolerance for numeric differences (e.g., ±0.001)

---

### 4. Prime Chart Functionality

**qicharts2 Usage:**

SPCify supports P' (pp) and U' (up) standardized charts:

```r
chart_type_requires_denominator("pp")  # TRUE
chart_type_requires_denominator("up")  # TRUE
```

**Standardization:** qicharts2 handles standardization internally when chart type is `pp` or `up`.

**BFHchart Status:** **VALIDATION_REQUIRED**
- **Gap Category:** WORKAROUND_BFHchart (if missing)
- **Action:** Confirm BFHchart supports standardized chart types or if standardization must be done in SPCify

**Validation Checklist:**
- [ ] Test P' charts with varying denominators
- [ ] Test U' charts with rate data
- [ ] Validate standardization formula matches qicharts2
- [ ] Document if standardization should live in BFHchart or SPCify

---

### 5. Freeze Period Handling

**qicharts2 Implementation:**

SPCify supports **baseline freezing** via `freeze` parameter:

```r
qic_args$freeze <- freeze_position  # Row index for baseline end

# Freeze without subsequent phase → "BASELINE" label
has_frys_without_subsequent_skift <- !has_skift_after_frys
```

**Behavior:** Control limits calculated using data up to freeze position, extended to end of dataset.

**Interaction with Phases:** Freeze and part can be used together:

```r
qic_args$freeze <- freeze_position  # Baseline period
qic_args$part <- part_positions     # Phase boundaries AFTER freeze
```

**BFHchart Status:** **ASSUMED_PARITY**
- **Rationale:** Freeze functionality is core SPC feature likely in extracted code
- **Gap Category:** BLOCKER (if missing) / ACCEPTABLE (if behavior differs)
- **Action:** Validate BFHchart freeze parameter and baseline label placement

**Validation Checklist:**
- [ ] Test freeze position alone (no phases)
- [ ] Test freeze + phases combination
- [ ] Validate "BASELINE" vs "NUV. NIVEAU" label logic
- [ ] Document freeze position adjustment logic for NA removal

---

### 6. Part Aggregation (Phase Splits)

**qicharts2 Implementation:**

SPCify supports multiple phase boundaries via `part` parameter:

```r
qic_args$part <- part_positions  # Vector of row indices marking phase starts

# Example: c(12, 24) creates 3 phases:
#   - Rows 1-11 (phase 1)
#   - Rows 12-23 (phase 2)
#   - Rows 24-end (phase 3)
```

**Per-Phase Calculations:**
- Separate centerline per phase (`qic_data$cl` changes)
- Separate control limits per phase (if enough data points)
- Anhøj rules applied per-phase

**Adjustment Logic:** SPCify adjusts part positions when rows removed due to NA values:

```r
# From clean_qic_call_args()
adjusted_part <- call_args$part |>
  purrr::map_dbl(~ {
    pos <- .x
    removed_before <- sum(removed_positions < pos)
    pos - removed_before
  })
```

**BFHchart Status:** **VALIDATION_REQUIRED**
- **Gap Category:** BLOCKER (if missing) / WORKAROUND_SPCify (for adjustment logic)
- **Action:** Confirm BFHchart part parameter exists and handles per-phase calculations

**Validation Checklist:**
- [ ] Test single phase boundary
- [ ] Test multiple phase boundaries
- [ ] Validate per-phase centerline calculation
- [ ] Test part position adjustment for missing data
- [ ] Document phase visualization (vertical lines, shading)

---

### 7. Custom Break Points

**qicharts2 Usage:**

SPCify does NOT use qicharts2's `breaks` parameter directly. Instead, **intelligent x-axis breaks** are calculated in SPCify after qic() call:

```r
# Detect date interval (weekly, monthly, daily)
interval_info <- detect_date_interval(qic_data$x, debug = TRUE)
format_config <- get_optimal_formatting(interval_info, debug = TRUE)

# Generate breaks with adaptive spacing (max 15 breaks)
breaks_posix <- seq(from = rounded_start, to = rounded_end, by = interval_size)
```

**Custom Logic:**
- Weekly data → "Uge X, YYYY" format
- Monthly data → "MMM YYYY" format
- Daily data → "DD/MM" format
- Adaptive interval multipliers (2×, 4×, 13×) to limit break count

**BFHchart Status:** **WORKAROUND_SPCify**
- **Rationale:** This is SPCify-specific intelligent formatting, not core SPC logic
- **Gap Category:** ACCEPTABLE - Keep in SPCify integration layer
- **Action:** Apply same x-axis formatting to BFHchart plots in SPCify

**Implementation Strategy:**
- BFHchart provides raw plot
- SPCify applies `scale_x_datetime()` with intelligent breaks
- No BFHchart enhancement needed

---

### 8. Axis Formatting and Labels

**qicharts2 Usage:**

SPCify handles axis formatting **after** qic() call using ggplot2 layers:

**X-Axis:**
- `validate_x_column_format()` → Determines date vs observation number
- `scale_x_datetime()` with custom breaks and labels
- Character columns converted to factors with `forcats::fct_inorder()`

**Y-Axis:**
- Multiple unit types: count, percent, time, rate
- Custom formatters: `format_y_axis_count()`, `format_y_axis_time()`, etc.
- Applied via `apply_y_axis_formatting()`

```r
Y_AXIS_UNITS_DA <- list(
  "Antal" = "count",
  "Procent (%)" = "percent",
  "Promille (‰)" = "permille",
  "Rate pr. 1000" = "rate_1000",
  "Rate pr. 100.000" = "rate_100000",
  "Rate" = "rate",
  "Tid" = "time",
  "Dage" = "days",
  "Timer" = "hours",
  "Gram" = "grams",
  "Kilogram" = "kg",
  "Kroner" = "dkk"
)
```

**BFHchart Status:** **WORKAROUND_SPCify**
- **Rationale:** Axis formatting is presentation logic, not core SPC calculations
- **Gap Category:** ACCEPTABLE - SPCify handles via ggplot2 layers
- **Action:** Apply same formatting functions to BFHchart output

**Implementation Strategy:**
- BFHchart returns base ggplot object or data
- SPCify applies axis formatting utilities
- Reuse existing `utils_y_axis_formatting.R` and date formatting logic

---

### 9. ggplot2 Integration Approach

**qicharts2 Implementation:**

qicharts2 returns ggplot object that SPCify extensively modifies:

```r
qic_args$return.data <- TRUE  # Get data instead of plot

# SPCify builds custom ggplot from qic_data
plot <- ggplot2::ggplot(qic_data, aes(x = x, y = y))
plot <- plot + geomtextpath::geom_textline(...)  # Control limit labels
plot <- plot + ggrepel::geom_text_repel(...)     # Comments
plot <- plot + custom_theme()                     # Hospital branding
```

**Post-Processing:**
- Extended centerline/target lines (20% beyond last data point)
- Label placement via `add_spc_labels()` with collision detection
- Comment annotations with `extract_comment_data()` + ggrepel
- Hospital theme application via `applyHospitalTheme()`

**BFHchart Status:** **VALIDATION_REQUIRED**
- **Gap Category:** BLOCKER (if not ggplot2-based) / ACCEPTABLE (if compatible)
- **Critical Question:** Does BFHchart return ggplot object or raw data?
- **Action:**
  1. Confirm BFHchart output format (ggplot vs data.frame)
  2. If ggplot: Validate SPCify can add layers (comments, labels, theme)
  3. If data: Build ggplot in SPCify from BFHchart data

**Validation Checklist:**
- [ ] Confirm BFHchart returns ggplot2-compatible output
- [ ] Test adding ggplot2 layers to BFHchart output
- [ ] Validate theme application works
- [ ] Document output structure (columns, aesthetics)

---

### 10. Data Structure Requirements

**qicharts2 Input:**

SPCify prepares data for qicharts2 using NSE (non-standard evaluation):

```r
qic_args <- list(
  data = data_with_row_id,  # Data.frame with .original_row_id
  x = as.name(x_col_for_qic),
  y = as.name(y_col_name),
  n = as.name(n_col_name),  # Optional
  chart = chart_type,
  return.data = TRUE
)

qic_data <- do.call(qicharts2::qic, qic_args)
```

**Input Preparation:**
- NA removal with position tracking
- Part/freeze position adjustment for removed rows
- Row ID injection (`.original_row_id`) for comment mapping stability
- Danish number parsing (`parse_danish_number()`)

**Output Structure:**

```r
# qic_data columns:
qic_data$x            # X-axis values (dates/numbers)
qic_data$y            # Y-axis values
qic_data$cl           # Centerline per point
qic_data$ucl          # Upper control limit
qic_data$lcl          # Lower control limit
qic_data$part         # Phase indicator
qic_data$runs.signal  # Runs rule violation
qic_data$n.crossings  # Number of crossings
qic_data$n.crossings.min  # Minimum expected crossings
qic_data$.original_row_id  # Injected by SPCify
```

**BFHchart Status:** **VALIDATION_REQUIRED**
- **Gap Category:** BLOCKER (if incompatible) / WORKAROUND_SPCify (for data prep)
- **Action:**
  1. Document BFHchart input/output data structure
  2. Create adapter functions in SPCify if structure differs
  3. Validate `.original_row_id` preservation for comment mapping

**Validation Checklist:**
- [ ] Document BFHchart input parameters and types
- [ ] Document BFHchart output data.frame structure
- [ ] Test NSE vs standard evaluation requirements
- [ ] Validate row ID preservation through BFHchart processing

---

### 11. Comment/Notes Parameter Mapping ⚠️ CRITICAL

**qicharts2 Implementation (SPCify Custom):**

SPCify adds comments **after** qic() call as ggplot layer:

```r
# Extract comments with stable row mapping
comment_data <- extract_comment_data(data, kommentar_column, qic_data)

# comment_data structure:
# - x: X-axis position (from qic_data)
# - y: Y-axis position (from qic_data)
# - comment: Text (from original data via .original_row_id join)

# Add as ggrepel layer
plot <- plot + ggrepel::geom_text_repel(
  data = comment_data,
  aes(x = x, y = y, label = comment),
  box.padding = 0.5,
  point.padding = 0.5,
  arrow = arrow(length = unit(0.015, "npc")),
  max.overlaps = Inf
)
```

**Stable Row Mapping Strategy:**

SPCify injects `.original_row_id` before qic() call to prevent comment drift when qicharts2 reorders/filters rows:

```r
# Before qic()
data_with_row_id$.original_row_id <- 1:nrow(data)

# After qic()
comment_data <- merge(
  qic_data[, c("x", "y", ".original_row_id")],
  data[, c(".original_row_id", "Kommentar")],
  by = ".original_row_id"
)
```

**XSS Sanitization:** Comments sanitized before rendering:

```r
# M3: Using config constants
comment_data$comment <- sanitize_user_input(
  input_value = comment,
  max_length = SPC_COMMENT_CONFIG$max_length,  # 100
  allowed_chars = "A-Za-z0-9_æøåÆØÅ .,-:!?",
  html_escape = TRUE
)

# Truncate long comments
if (nchar(comment) > SPC_COMMENT_CONFIG$display_length) {  # 40
  comment <- paste0(substr(comment, 1, SPC_COMMENT_CONFIG$truncate_length), "...")  # 37
}
```

**BFHchart Status:** **CRITICAL_VALIDATION_REQUIRED**
- **Gap Category:** BLOCKER (if notes parameter incompatible) / WORKAROUND_SPCify (if API differs)
- **Critical Questions:**
  1. Does BFHchart have a `notes` parameter?
  2. If yes: How does it map notes to data points (by index, row ID, x/y match)?
  3. If yes: Does it handle Danish characters (æøå)?
  4. If yes: Does it support custom styling (font size, color, arrow)?
  5. If no: Can SPCify continue using ggrepel layer approach?

**Mapping Options:**

**Option A: BFHchart `notes` Parameter Exists**
```r
# Map kommentar_column → notes parameter
bfh_args$notes <- data[[kommentar_column]]

# Validate:
# - Row alignment stable (no reordering issues)
# - Danish characters render correctly
# - Styling customizable
```

**Option B: No BFHchart `notes` Parameter**
```r
# Continue SPCify approach:
# 1. Get BFHchart output data
# 2. Extract comment_data with .original_row_id join
# 3. Add ggrepel::geom_text_repel() layer
# 4. Apply sanitization as before
```

**Recommendation:** **Option B (SPCify Layer) Preferred**
- **Rationale:** Comment handling is presentation logic with complex requirements (XSS, Danish characters, collision avoidance)
- **Benefit:** SPCify retains full control over comment styling and security
- **Downside:** BFHchart `notes` parameter (if it exists) goes unused

**Validation Checklist:**
- [ ] Document BFHchart `notes` parameter API (if exists)
- [ ] Test row alignment stability with comments
- [ ] Validate Danish character (æøå) rendering
- [ ] Test XSS sanitization integration
- [ ] Test long comment truncation
- [ ] Validate ggrepel layer can be added to BFHchart output
- [ ] Performance test: comment rendering overhead

---

## Gap Category Definitions

### BLOCKER
**Definition:** Feature absolutely required for SPCify, no workaround possible without BFHchart implementation.

**Impact:** Blocks migration until BFHchart enhancement completed.

**Escalation:** Create BFHchart issue immediately, prioritize as P0.

**Examples:**
- Control limit calculation missing
- Chart type not supported
- ggplot2 output incompatible

### WORKAROUND_SPCify
**Definition:** Feature can be implemented in SPCify integration layer without BFHchart changes.

**Impact:** Adds complexity to SPCify but unblocks migration.

**Escalation:** Document in SPCify code, consider BFHchart enhancement in future.

**Examples:**
- Intelligent x-axis breaks
- Comment annotations via ggrepel
- Y-axis formatting

### WORKAROUND_BFHchart
**Definition:** Feature should be in BFHchart but can be temporarily worked around in SPCify.

**Impact:** Technical debt; BFHchart enhancement recommended.

**Escalation:** Create BFHchart issue, prioritize based on impact.

**Examples:**
- Missing chart type (can be added to BFHchart)
- Anhøj rules not exposed (can be calculated in SPCify)
- Freeze parameter missing (can be simulated)

### ACCEPTABLE
**Definition:** Minor differences acceptable; no action needed.

**Impact:** None or minimal; document for user awareness.

**Escalation:** None; note in migration documentation.

**Examples:**
- Slightly different control limit precision (±0.001)
- Alternative Anhøj algorithm (if clinically equivalent)
- Visual styling differences

---

## Triage Decisions Summary

| Feature Area | Status | Gap Category | Priority | Action |
|--------------|--------|--------------|----------|--------|
| 1. Chart Type Support | ASSUMED_PARITY | WORKAROUND_BFHchart | P1 | Validate API |
| 2. Anhøj Rules | VALIDATION_REQUIRED | WORKAROUND_BFHchart | P0 | Check algorithm |
| 3. Control Limits | ASSUMED_PARITY | BLOCKER (if missing) | P0 | Validate calculations |
| 4. Prime Charts | VALIDATION_REQUIRED | WORKAROUND_BFHchart | P1 | Test P'/U' |
| 5. Freeze Period | ASSUMED_PARITY | BLOCKER (if missing) | P0 | Validate behavior |
| 6. Part Aggregation | VALIDATION_REQUIRED | BLOCKER (if missing) | P0 | Test phases |
| 7. Custom Breaks | WORKAROUND_SPCify | ACCEPTABLE | P2 | Keep in SPCify |
| 8. Axis Formatting | WORKAROUND_SPCify | ACCEPTABLE | P2 | Keep in SPCify |
| 9. ggplot2 Integration | VALIDATION_REQUIRED | BLOCKER (if incompatible) | P0 | Confirm output |
| 10. Data Structure | VALIDATION_REQUIRED | BLOCKER (if incompatible) | P0 | Document API |
| 11. Comment/Notes | CRITICAL_VALIDATION | WORKAROUND_SPCify | P0 | Prefer SPCify layer |

**Legend:**
- **P0:** Blocker - Must resolve before Phase 3
- **P1:** High - Should resolve in Phase 3
- **P2:** Medium - Can defer to Phase 5 or later

---

## Recommendations

### Phase 1 (Current): Feasibility Analysis
1. **Immediate:** Access BFHchart source code or documentation
2. **Validate:** API for P0 features (Anhøj, control limits, freeze, phases, ggplot2 output)
3. **Document:** Any discovered BLOCKER gaps
4. **Escalate:** Create BFHchart issues for BLOCKER items

### Phase 2: Dependency Bootstrap
1. **Assume:** Feature parity sufficient to proceed (we control BFHchart)
2. **Plan:** Parallel BFHchart enhancements if BLOCKER gaps found
3. **Version Pin:** Use semantic versioning for compatibility

### Phase 3: Service Layer
1. **Adapter Pattern:** Create `compute_spc_results_bfh()` to abstract BFHchart API
2. **Fallback:** Keep qicharts2 code active behind feature flag during validation
3. **Incremental:** Implement one chart type at a time, validate against baseline

### Long-Term Strategy
1. **Keep in SPCify:**
   - Intelligent x-axis breaks
   - Y-axis formatting
   - Comment annotations
   - Hospital theme application

2. **Migrate to BFHchart (if missing):**
   - Anhøj rules calculation (if not exposed)
   - Prime chart standardization (if not supported)
   - Phase boundary visualization enhancements

3. **API Refinement:**
   - Document expected BFHchart API in Task 30
   - Propose BFHchart enhancements based on real-world usage
   - Version BFHchart releases in sync with SPCify requirements

---

## BFHchart Enhancement Tracking

**Note:** Detailed issue tracking in `docs/migration/bfh-issues.md` (created in Stream C).

**Potential BFHchart Issues (Pending Validation):**

1. **[BFHchart] Expose Anhøj rules per data point**
   - Priority: P0 (BLOCKER if missing)
   - Workaround: Calculate in SPCify service layer
   - Effort: Medium (algorithm already exists if extracted from SPCify)

2. **[BFHchart] Support freeze parameter for baseline period**
   - Priority: P0 (BLOCKER if missing)
   - Workaround: Complex - requires control limit calculation override
   - Effort: Medium (likely already exists if extracted)

3. **[BFHchart] Document output data structure**
   - Priority: P0 (Documentation)
   - Workaround: Reverse-engineer from code
   - Effort: Low (documentation task)

4. **[BFHchart] Validate ggplot2 layer compatibility**
   - Priority: P0 (BLOCKER if incompatible)
   - Workaround: Rebuild plot from data in SPCify
   - Effort: High (major architectural change)

---

## Next Steps

1. **Access BFHchart:**
   - Locate BFHchart repository/source code
   - Review API documentation (if exists)
   - Examine function signatures and return values

2. **Validate P0 Features:**
   - Write test cases for each BLOCKER feature
   - Compare BFHchart output to qicharts2 baseline
   - Document any numeric/visual differences

3. **Create Issue List:**
   - File BFHchart issues for confirmed gaps
   - Prioritize based on blocking status
   - Link SPCify use cases in issue descriptions

4. **Update Task 30:**
   - Use findings to design `compute_spc_results_bfh()` facade
   - Plan adapter layer for API differences
   - Define BFHchart enhancement schedule

---

## Appendix: qicharts2 API Reference (Current Implementation)

### Function Call Pattern
```r
qic_data <- qicharts2::qic(
  data = data.frame,           # Input data
  x = date_column,             # X-axis (NSE)
  y = value_column,            # Y-axis (NSE)
  n = denominator_column,      # Denominator (NSE, optional)
  chart = "run",               # Chart type
  freeze = 12,                 # Baseline freeze position
  part = c(12, 24),            # Phase boundaries
  target = 0.75,               # Target value (decimal scale)
  cl = 0.80,                   # Custom centerline
  return.data = TRUE           # Return data.frame instead of plot
)
```

### Return Structure
```r
# qic_data is a data.frame with:
str(qic_data)
# $ x : Date/numeric - X-axis values
# $ y : numeric - Y-axis values
# $ cl : numeric - Centerline per point
# $ ucl : numeric - Upper control limit
# $ lcl : numeric - Lower control limit
# $ part : integer - Phase indicator
# $ runs.signal : logical - Runs rule violation
# $ n.crossings : integer - Number of crossings
# $ n.crossings.min : integer - Expected minimum crossings
# $ .original_row_id : integer - Injected by SPCify
```

### Parameter Defaults
- `chart`: "run"
- `freeze`: NULL (no baseline)
- `part`: NULL (no phases)
- `target`: NULL (no target line)
- `cl`: NULL (auto-calculate)
- `return.data`: FALSE (return ggplot)

---

**Document Owner:** Claude Code
**Review Status:** Pending technical lead review
**Next Review:** After BFHchart API validation (Stream C)
