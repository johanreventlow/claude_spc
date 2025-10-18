# BFHchart vs qicharts2 Visual Differences

**Date:** 2025-10-15
**Version:** BFHchart 0.1.0 vs qicharts2 0.7.0
**Status:** Clinical Validation Pending

---

## Purpose

This document catalogs visual differences between BFHchart and qicharts2 SPC chart output for clinical analyst review and approval. The goal is to ensure that all differences are either:

1. **Acceptable** - No impact on clinical interpretation
2. **Unacceptable** - Must match within specified tolerance

All unacceptable differences must be validated through automated regression testing before clinical deployment.

---

## Acceptable Visual Differences

### 1. Line Styles & Aesthetics

**Category:** Visual styling
**Clinical Impact:** None (control limits remain clearly distinguishable)

| Element | qicharts2 | BFHchart | Status |
|---------|-----------|----------|--------|
| Control limits | Dashed red lines | [To be documented after Stream B] | ⏳ Pending |
| Center line | Solid blue line | [To be documented after Stream B] | ⏳ Pending |
| Data line | Solid black line | [To be documented after Stream B] | ⏳ Pending |
| Anhøj signals | Red dots/highlights | [To be documented after Stream B] | ⏳ Pending |

**Validation Method:** Visual inspection
**Approval Required:** Clinical analyst confirmation

---

### 2. Color Palette

**Category:** Visual styling
**Clinical Impact:** None (sufficient contrast maintained for accessibility)

| Element | qicharts2 | BFHchart | Contrast Ratio |
|---------|-----------|----------|----------------|
| Data points | Black (#000000) | [To be documented] | ⏳ Pending |
| Anhøj signals | Red (#FF0000) | [To be documented] | ⏳ Pending |
| Control limits | Red (#FF0000) | [To be documented] | ⏳ Pending |
| Center line | Blue (#0000FF) | [To be documented] | ⏳ Pending |
| Freeze markers | Gray | [To be documented] | ⏳ Pending |

**Accessibility Standard:** WCAG 2.1 AA (4.5:1 for text, 3:1 for UI components)
**Validation Method:** Color contrast analysis
**Approval Status:** ⏳ Pending

---

### 3. Label Placement & Collision Avoidance

**Category:** Text rendering
**Clinical Impact:** None (readability maintained)

**qicharts2 Behavior:**
- Uses intelligent label placement with collision detection
- May overlap when density is high
- No configurable placement options

**BFHchart Behavior:**
- [To be documented after Stream B testing]
- Expected: Similar or improved collision avoidance
- SPCify integration: `config_label_placement.R` may apply additional optimizations

**Edge Cases to Test:**
1. Dense data points (>50 observations)
2. Extreme Y-axis values causing vertical crowding
3. Long Danish text labels (with æøå characters)
4. Multiple simultaneous annotations (comments + freeze markers)

**Validation Method:** Visual inspection across all chart types
**Approval Status:** ⏳ Pending

---

### 4. Axis Formatting

**Category:** Visual styling
**Clinical Impact:** None (tick marks and labels remain interpretable)

| Feature | qicharts2 | BFHchart | Notes |
|---------|-----------|----------|-------|
| X-axis tick spacing | Auto (ggplot2 default) | [To be documented] | ⏳ Pending |
| Y-axis tick spacing | Auto | [To be documented] | ⏳ Pending |
| Y-axis label format | Scientific notation for large values | [To be documented] | ⏳ Pending |
| X-axis rotation | 0° or 45° (adaptive) | [To be documented] | ⏳ Pending |
| Grid lines | Light gray background | [To be documented] | ⏳ Pending |

**Validation Method:** Visual inspection
**Approval Status:** ⏳ Pending

---

### 5. Plot Dimensions & Layout

**Category:** Visual layout
**Clinical Impact:** None (proportions maintained)

**qicharts2:**
- Default plot size: Responsive to container
- Title placement: Top center
- Legend position: Right (if applicable)

**BFHchart:**
- [To be documented after Stream B]
- Expected: Similar responsive behavior
- SPCify control: `config_ui.R` defines standard dimensions

**Validation Method:** Responsive layout testing
**Approval Status:** ⏳ Pending

---

## Unacceptable Differences (Must Match Within Tolerance)

### 1. Control Limit Values (UCL/LCL)

**Requirement:** Numerical match within **±0.001** tolerance
**Clinical Impact:** Critical - affects clinical decision-making
**Test Suite:** `tests/testthat/test-spc-regression-bfh-vs-qic.R`

| Chart Type | Metric | Tolerance | Validation |
|------------|--------|-----------|------------|
| Run chart | Median (center line) | ±0.001 | ⏳ Stream B |
| I chart | UCL/LCL (mean ± 2.66 × MR̄) | ±0.001 | ⏳ Stream B |
| P chart | UCL/LCL (binomial) | ±0.001 | ⏳ Stream B |
| C chart | UCL/LCL (Poisson) | ±0.001 | ⏳ Stream B |
| U chart | UCL/LCL (Poisson, variable n) | ±0.001 | ⏳ Stream B |
| X̄ chart | UCL/LCL (A₂ factor) | ±0.001 | ⏳ Stream B |
| S chart | UCL/LCL (c₄ factor) | ±0.001 | ⏳ Stream B |

**Failure Scenario:** If any control limit differs by >±0.001:
1. File BFHchart issue
2. Implement workaround in `R/fct_spc_bfh_service.R`
3. Document in `docs/migration/bfh-workarounds.md`
4. Re-validate

**Approval Status:** ⏳ Awaiting Stream B results

---

### 2. Anhøj Rule Detection

**Requirement:** Binary match (exact same points flagged)
**Clinical Impact:** Critical - special cause variation detection
**Test Suite:** `tests/testthat/test-anhoej-rules.R`

**Anhøj Rules:**
1. **Runs test:** ≥8 consecutive points above or below center line
2. **Crossings test:** Too few crossings of the median (statistical threshold)

| Scenario | Expected Behavior | Validation |
|----------|-------------------|------------|
| No violations | No points flagged | ⏳ Stream A/B |
| Runs violation (8+ points) | Exact same points as qicharts2 | ⏳ Stream A/B |
| Crossings violation | Signal detected (overall) | ⏳ Stream A/B |
| Both violations | Both signals triggered | ⏳ Stream A/B |

**Failure Scenario:**
- If BFHchart doesn't expose Anhøj rules → Stream A implements in SPCify
- If detection differs → File BFHchart issue, use SPCify implementation
- Document in `docs/migration/bfh-workarounds.md`

**Approval Status:** ⏳ Awaiting Stream A implementation validation

---

### 3. Data Point Positions (X/Y Coordinates)

**Requirement:** Exact positional match
**Clinical Impact:** Critical - misalignment would indicate calculation errors
**Test Suite:** Visual inspection + numerical comparison

| Chart Type | X-Axis (Time/Index) | Y-Axis (Values) | Validation |
|------------|---------------------|-----------------|------------|
| Run chart | Sequential index | Raw values | ⏳ Pending |
| I chart | Sequential index | Raw values | ⏳ Pending |
| P chart | Sequential index | Proportions (y/n) | ⏳ Pending |
| C chart | Sequential index | Raw counts | ⏳ Pending |
| U chart | Sequential index | Rates (y/n) | ⏳ Pending |
| X̄ chart | Subgroup index | Subgroup means | ⏳ Pending |
| S chart | Subgroup index | Subgroup SD | ⏳ Pending |

**Validation Method:**
1. Extract `qic_data$x` and `qic_data$y` from both systems
2. Compare with `all.equal(tolerance = 1e-10)`
3. Visual overlay comparison

**Approval Status:** ⏳ Pending

---

### 4. Freeze Period Handling

**Requirement:** Freeze points excluded from control limit calculation
**Clinical Impact:** Critical - incorrect freeze handling invalidates clinical interpretation
**Test Suite:** `test-spc-regression-bfh-vs-qic.R` (freeze scenarios)

**Expected Behavior:**
- Freeze points (freeze_var = 1) plotted but not used in calculation
- Control limits recalculated excluding freeze range
- Vertical freeze line marker rendered

| Scenario | qicharts2 | BFHchart | Validation |
|----------|-----------|----------|------------|
| No freeze points | All points included | Same | ⏳ Pending |
| Mid-series freeze (5 points) | CL recalculated excluding 5 | Same | ⏳ Pending |
| Multiple freeze ranges | Each range excluded | Same | ⏳ Pending |

**Approval Status:** ⏳ Pending

---

## Side-by-Side Visual Comparisons

### 1. Run Chart - Basic Scenario

**Test Data:** `fixtures/qic-baseline/run-basic.rds`
**Observations:** 24 monthly infection counts
**Expected Signals:** None

| qicharts2 | BFHchart |
|-----------|----------|
| ![qicharts2 run chart](screenshots/qic-run-basic.png) | ![BFHchart run chart](screenshots/bfh-run-basic.png) |

**Observations:**
- [To be documented after screenshot capture]

**Clinical Review:**
- ☐ Median line position accurate
- ☐ Data points aligned
- ☐ Visual clarity acceptable
- **Analyst Sign-Off:** ☐ Approved ☐ Rejected
  **Reviewer:** _____________ **Date:** _______

---

### 2. Run Chart - Anhøj Violations

**Test Data:** `fixtures/qic-baseline/run-anhoej.rds`
**Expected Signals:** 8+ consecutive points above median

| qicharts2 | BFHchart |
|-----------|----------|
| ![qicharts2 run Anhøj](screenshots/qic-run-anhoej.png) | ![BFHchart run Anhøj](screenshots/bfh-run-anhoej.png) |

**Observations:**
- [To be documented after screenshot capture]

**Clinical Review:**
- ☐ Anhøj run detected (points 12-19)
- ☐ Signal points highlighted correctly
- ☐ Visual distinction clear
- **Analyst Sign-Off:** ☐ Approved ☐ Rejected
  **Reviewer:** _____________ **Date:** _______

---

### 3. I Chart - Basic Scenario

**Test Data:** `fixtures/qic-baseline/i-basic.rds`
**Expected:** Mean ± 2.66 × MR̄ control limits

| qicharts2 | BFHchart |
|-----------|----------|
| ![qicharts2 I chart](screenshots/qic-i-basic.png) | ![BFHchart I chart](screenshots/bfh-i-basic.png) |

**Observations:**
- [To be documented]

**Clinical Review:**
- ☐ UCL/LCL within ±0.001 tolerance
- ☐ Moving range calculation correct
- ☐ Points above UCL flagged
- **Analyst Sign-Off:** ☐ Approved ☐ Rejected
  **Reviewer:** _____________ **Date:** _______

---

### 4. P Chart - Proportions

**Test Data:** `fixtures/qic-baseline/p-basic.rds`
**Expected:** Variable control limits (binomial distribution)

| qicharts2 | BFHchart |
|-----------|----------|
| ![qicharts2 P chart](screenshots/qic-p-basic.png) | ![BFHchart P chart](screenshots/bfh-p-basic.png) |

**Observations:**
- [To be documented]

**Clinical Review:**
- ☐ Proportion calculation correct (y/n)
- ☐ Variable limits rendered per point
- ☐ Denominator changes reflected
- **Analyst Sign-Off:** ☐ Approved ☐ Rejected
  **Reviewer:** _____________ **Date:** _______

---

### 5. C Chart - Counts

**Test Data:** `fixtures/qic-baseline/c-basic.rds`
**Expected:** Poisson-based control limits

| qicharts2 | BFHchart |
|-----------|----------|
| ![qicharts2 C chart](screenshots/qic-c-basic.png) | ![BFHchart C chart](screenshots/bfh-c-basic.png) |

**Observations:**
- [To be documented]

**Clinical Review:**
- ☐ Poisson limits appropriate for counts
- ☐ Fixed sample size handled
- **Analyst Sign-Off:** ☐ Approved ☐ Rejected
  **Reviewer:** _____________ **Date:** _______

---

### 6. U Chart - Rates

**Test Data:** `fixtures/qic-baseline/u-basic.rds`
**Expected:** Variable denominator, Poisson-adjusted limits

| qicharts2 | BFHchart |
|-----------|----------|
| ![qicharts2 U chart](screenshots/qic-u-basic.png) | ![BFHchart U chart](screenshots/bfh-u-basic.png) |

**Observations:**
- [To be documented]

**Clinical Review:**
- ☐ Rate calculation correct (y/n)
- ☐ Variable denominator handled
- ☐ Limits adjust per point
- **Analyst Sign-Off:** ☐ Approved ☐ Rejected
  **Reviewer:** _____________ **Date:** _______

---

### 7. X̄ Chart - Subgroup Means

**Test Data:** `fixtures/qic-baseline/xbar-basic.rds`
**Expected:** A₂ factor control limits

| qicharts2 | BFHchart |
|-----------|----------|
| ![qicharts2 Xbar chart](screenshots/qic-xbar-basic.png) | ![BFHchart Xbar chart](screenshots/bfh-xbar-basic.png) |

**Observations:**
- [To be documented]

**Clinical Review:**
- ☐ Subgroup means calculated correctly
- ☐ A₂ factor applied correctly
- **Analyst Sign-Off:** ☐ Approved ☐ Rejected
  **Reviewer:** _____________ **Date:** _______

---

### 8. S Chart - Standard Deviation

**Test Data:** `fixtures/qic-baseline/s-basic.rds`
**Expected:** c₄ factor control limits

| qicharts2 | BFHchart |
|-----------|----------|
| ![qicharts2 S chart](screenshots/qic-s-basic.png) | ![BFHchart S chart](screenshots/bfh-s-basic.png) |

**Observations:**
- [To be documented]

**Clinical Review:**
- ☐ Subgroup SD calculated correctly
- ☐ c₄ factor applied correctly
- **Analyst Sign-Off:** ☐ Approved ☐ Rejected
  **Reviewer:** _____________ **Date:** _______

---

## Screenshot Capture Process

**Location:** `docs/migration/screenshots/`
**Format:** PNG, 800x600 pixels minimum
**Naming Convention:** `{package}-{chart_type}-{scenario}.png`

**Capture Steps:**

1. **Run baseline generation script:**
   ```r
   source("tests/testthat/fixtures/generate-qic-baselines.R")
   ```

2. **Generate qicharts2 screenshots:**
   ```r
   library(qicharts2)
   baseline <- readRDS("tests/testthat/fixtures/qic-baseline/run-basic.rds")

   plot <- qic(
     data = baseline$input_data,
     x = names(baseline$input_data)[1],
     y = names(baseline$input_data)[2],
     chart = "run"
   )

   ggsave("docs/migration/screenshots/qic-run-basic.png",
          plot = plot, width = 8, height = 6, dpi = 150)
   ```

3. **Generate BFHchart screenshots:**
   ```r
   library(BFHchart)
   source("R/fct_spc_bfh_service.R")

   result <- compute_spc_results_bfh(
     data = baseline$input_data,
     x_var = names(baseline$input_data)[1],
     y_var = names(baseline$input_data)[2],
     chart_type = "run"
   )

   ggsave("docs/migration/screenshots/bfh-run-basic.png",
          plot = result$plot, width = 8, height = 6, dpi = 150)
   ```

4. **Repeat for all 7 chart types × 3 scenarios = 21 screenshot pairs**

**Screenshot Checklist:**
- [ ] Run chart - basic, anhoej, freeze (3 pairs)
- [ ] I chart - basic, anhoej, freeze (3 pairs)
- [ ] P chart - basic, anhoej, freeze (3 pairs)
- [ ] C chart - basic, anhoej, freeze (3 pairs)
- [ ] U chart - basic, anhoej, freeze (3 pairs)
- [ ] X̄ chart - basic, anhoej, freeze (3 pairs)
- [ ] S chart - basic, anhoej, freeze (3 pairs)

**Total:** 42 screenshots (21 qicharts2 + 21 BFHchart)

---

## Clinical Analyst Sign-Off

**Reviewer Name:** _____________________
**Qualifications:** Klinisk kvalitetsmedarbejder, SPC-certificeret
**Review Date:** _____________________

### Overall Assessment

**Visual Differences:**
- ☐ All acceptable differences approved
- ☐ No unacceptable differences found
- ☐ Concerns documented below

**Numerical Accuracy:**
- ☐ All control limits within ±0.001 tolerance (verified by Stream B tests)
- ☐ Anhøj rules match qicharts2 exactly (verified by Stream A tests)
- ☐ Data positions accurate

**Clinical Safety:**
- ☐ No misleading visual representations detected
- ☐ All chart types clinically interpretable
- ☐ Edge cases handled appropriately
- ☐ Freeze period handling correct

### Final Approval

- ☐ **APPROVED** - Ready for Task #32 (UI Integration)
- ☐ **CONDITIONAL** - Minor issues acceptable (see comments)
- ☐ **REJECTED** - Blockers identified (must resolve before deployment)

**Signature:** _____________________ **Date:** __________

**Comments/Concerns:**

```
[Clinical analyst notes here]
```

**Recommended Next Steps:**

```
[Action items if conditional/rejected]
```

---

## Appendix: Testing References

**Stream A (Anhøj Rules):**
- Implementation: `R/fct_anhoej_rules.R`
- Tests: `tests/testthat/test-anhoej-rules.R`
- Integration: `R/fct_spc_bfh_service.R`

**Stream B (Regression Testing):**
- Tests: `tests/testthat/test-spc-regression-bfh-vs-qic.R`
- Baselines: `tests/testthat/fixtures/qic-baseline/*.rds`
- Coverage: 21+ test cases (7 chart types × 3 scenarios)

**Related Documentation:**
- `docs/migration/bfh-workarounds.md` - Workarounds for BFHchart gaps
- `docs/migration/clinical-validation-checklist.md` - Detailed validation checklist
- `.claude/epics/bfhcharts-spc-migration/31-analysis.md` - Task analysis

**Version Control:**
- Epic branch: `epic/bfhcharts-spc-migration`
- Worktree: `/Users/johanreventlow/Documents/R/epic-bfhcharts-spc-migration/`
- Main repo: `/Users/johanreventlow/Documents/R/claude_spc/`
