# Manual Test Guide for BFHcharts Migration

This document describes manual verification procedures for the BFHcharts SPC migration. These tests complement the automated test suite and verify runtime behavior across different scenarios.

## Test Environment Setup

Before running any manual tests:

```bash
cd /Users/johanreventlow/Documents/R/epic-bfhcharts-spc-migration

# Start R and load the app
R
> source('global.R')

# Or launch the Shiny app directly
> run_app()
```

## Backend Switching Tests

### Test 1: NSE (Non-Standard Evaluation) Fix Verification

**Purpose:** Verify that column references are handled correctly in both backends

**Files Previously Used:** `verify_nse_fix.R`

**Procedure:**

1. Launch R and source global.R
2. Create test data with named columns:
   ```r
   test_data <- data.frame(
     Dato = seq.Date(Sys.Date() - 49, Sys.Date(), by = "day"),
     Vaerdi = rnorm(50, mean = 100, sd = 15),
     Naevner = sample(50:200, 50, replace = TRUE)
   )
   ```

3. Test qicharts2 backend:
   ```r
   Sys.setenv(GOLEM_CONFIG_ACTIVE = "development")
   options(spc.use_bfhchart = FALSE)
   result_qic <- compute_spc_results_qic(
     data = test_data,
     x_var = "Dato",
     y_var = "Vaerdi",
     chart_type = "run"
   )
   ```

4. Verify: `result_qic$plot` renders without error

5. Test BFHcharts backend:
   ```r
   options(spc.use_bfhchart = TRUE)
   result_bfh <- compute_spc_results_bfh(
     data = test_data,
     x_var = "Dato",
     y_var = "Vaerdi",
     chart_type = "run"
   )
   ```

6. Verify: `result_bfh$plot` renders without error

**Expected Result:** Both backends produce valid plots

---

### Test 2: Part Parameter Position Fix Verification

**Purpose:** Verify that phase/part markers are positioned correctly

**Files Previously Used:** `verify_part_fix.R`

**Procedure:**

1. Create test data with phase markers:
   ```r
   test_data <- data.frame(
     Dato = seq.Date(Sys.Date() - 49, Sys.Date(), by = "day"),
     Vaerdi = rnorm(50, mean = 100, sd = 15),
     Skift = c(rep(0, 25), rep(1, 25))  # Phase change at row 26
   )
   ```

2. Test with `compute_spc_results_bfh()`:
   ```r
   result <- compute_spc_results_bfh(
     data = test_data,
     x_var = "Dato",
     y_var = "Vaerdi",
     chart_type = "run",
     part_var = "Skift"
   )
   ```

3. Verify: Phase line appears at correct position (between row 25-26)

**Expected Result:** Phase change line rendered at row 26, not elsewhere

---

### Test 3: Target Line Rendering

**Purpose:** Verify target line appears correctly on charts

**Files Previously Used:** `verify_target_line.R`

**Procedure:**

1. Create test data:
   ```r
   test_data <- data.frame(
     Dato = seq.Date(Sys.Date() - 49, Sys.Date(), by = "day"),
     Vaerdi = rnorm(50, mean = 100, sd = 15)
   )
   ```

2. Test with target value:
   ```r
   result <- compute_spc_results_bfh(
     data = test_data,
     x_var = "Dato",
     y_var = "Vaerdi",
     chart_type = "run",
     target_value = 95
   )
   ```

3. Verify: Horizontal target line appears at Y=95 on plot

**Expected Result:** Target line rendered correctly on both backends

---

## Backend Switching Tests

### Test 4: Feature Flag False (qicharts2)

**Purpose:** Verify app works correctly with `use_bfhchart = FALSE`

**Steps:**

1. Set environment variable:
   ```bash
   export GOLEM_CONFIG_ACTIVE=production
   ```

2. Launch app:
   ```r
   run_app()
   ```

3. Upload sample CSV with run chart data
4. Configure columns: Dato (X), Vaerdi (Y)
5. Verify: Chart renders using qicharts2

**Expected Result:** Chart displays with qicharts2 styling

---

### Test 5: Feature Flag True (BFHcharts)

**Purpose:** Verify app works correctly with `use_bfhchart = TRUE`

**Steps:**

1. Set environment variable:
   ```bash
   export GOLEM_CONFIG_ACTIVE=development
   ```

2. Launch app:
   ```r
   run_app()
   ```

3. Upload same sample CSV
4. Configure same columns
5. Verify: Chart renders using BFHcharts

**Expected Result:** Chart displays with BFHcharts styling (different appearance)

---

### Test 6: Unsupported Chart Type Fallback

**Purpose:** Verify unsupported chart types fall back to qicharts2

**Steps:**

1. Keep `use_bfhchart = TRUE` (development config)
2. Try to create X̄ (X-bar) chart
3. Verify: Chart renders using qicharts2 fallback

**Expected Result:** X̄ chart renders correctly via qicharts2

---

## Chart Type Coverage

### Test 7: All Supported Chart Types

**Purpose:** Verify all 5 supported chart types render correctly

**Test Data:**

```r
# Create data suitable for all chart types
test_data <- data.frame(
  Dato = seq.Date(Sys.Date() - 49, Sys.Date(), by = "day"),
  Vaerdi = rnorm(50, mean = 100, sd = 15),
  Naevner = sample(50:200, 50, replace = TRUE)
)
```

**Chart Types to Test:**

| Chart Type | X Column | Y Column | N Column | Expected |
|-----------|----------|----------|----------|----------|
| Run       | Dato     | Vaerdi   | —        | Run chart |
| I         | Dato     | Vaerdi   | —        | I chart   |
| P         | Dato     | Vaerdi   | Naevner  | P chart   |
| C         | Dato     | Vaerdi   | —        | C chart   |
| U         | Dato     | Vaerdi   | Naevner  | U chart   |

**Procedure:**

1. For each chart type, configure UI
2. Verify chart renders
3. Check Anhøj rules display
4. Confirm no console errors

**Expected Result:** All 5 charts render correctly

---

## Error Handling Tests

### Test 8: Both Backends Fail Gracefully

**Purpose:** Verify error messages when both backends fail

**Procedure:**

1. Create invalid test data (all NA values)
2. Try to render chart
3. Verify: Error message is user-friendly
4. Check logs for debugging information

**Expected Result:** Helpful error message, detailed logs

---

## Performance Tests

### Test 9: Response Time Comparison

**Purpose:** Verify performance characteristics

**Procedure:**

1. Create medium dataset (1000 rows)
2. Measure rendering time with qicharts2:
   ```r
   system.time(compute_spc_results_qic(...))
   ```

3. Measure rendering time with BFHcharts:
   ```r
   system.time(compute_spc_results_bfh(...))
   ```

4. Compare: BFHcharts should be ≤2x slower (acceptable due to features)

**Expected Result:** Both backends render in <5 seconds

---

## Browser Compatibility

### Test 10: Cross-Browser Testing

**Purpose:** Verify charts render correctly across browsers

**Browsers to Test:**
- Chrome/Chromium (latest)
- Firefox (latest)
- Safari (if available)
- Mobile browser (if applicable)

**Procedure:**

1. Launch app in each browser
2. Upload sample data
3. Create run chart
4. Verify: Plot renders, no console errors
5. Check: Anhøj values display correctly

**Expected Result:** Consistent appearance and functionality across browsers

---

## Regression Tests

### Test 11: Known Bugs Don't Reappear

**Purpose:** Verify previously fixed bugs stay fixed

**Previously Fixed Issues:**

1. **NSE Bug in do.call()** - Verify part parameter works
2. **Part Position Regression** - Verify phases at correct positions
3. **Line Length Linter** - Verify code style maintained
4. **Comment Sanitization** - Verify XSS protection works

**Procedure:**

For each issue:
1. Review fix in CLAUDE.md ADR section
2. Run corresponding test procedure
3. Verify bug does not reappear

**Expected Result:** No regressions

---

## Documentation Verification

### Test 12: Code Examples Work As-Is

**Purpose:** Verify documentation examples are accurate

**Procedure:**

1. Copy code examples from:
   - `CLAUDE.md` (BFHcharts section)
   - `docs/BFHCHARTS_MIGRATION_GUIDE.md`
   - `docs/BFHCHARTS_LIMITATIONS.md`

2. Paste into R and execute
3. Verify: Examples work without modification

**Expected Result:** All documentation examples run correctly

---

## Checklist for Test Completion

- [ ] NSE fix verified
- [ ] Part parameter fix verified
- [ ] Target line rendering verified
- [ ] Feature flag (FALSE) working
- [ ] Feature flag (TRUE) working
- [ ] Unsupported chart fallback verified
- [ ] All 5 chart types tested
- [ ] Error handling graceful
- [ ] Performance acceptable
- [ ] Cross-browser compatibility verified
- [ ] No regressions detected
- [ ] Documentation examples work

---

## Reporting Issues

If any test fails:

1. **Document the failure:**
   - Exact steps to reproduce
   - Expected vs actual result
   - Screenshots/error messages

2. **Check logs:**
   ```r
   # View debug logs
   options(spc.debug.logging = TRUE)
   # Run test again and review console output
   ```

3. **Create GitHub issue** with findings

---

## Contact & Support

For questions about these manual tests, see:
- `docs/BFHCHARTS_MIGRATION_GUIDE.md` - Architecture details
- `CLAUDE.md` - Development standards
- GitHub Issues - Known issues and workarounds
