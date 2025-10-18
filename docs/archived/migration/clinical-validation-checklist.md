# Clinical Validation Checklist - BFHchart Migration

**Validator:** _____________________
**Title:** Klinisk kvalitetsmedarbejder
**Date:** _____________________
**BFHchart Version:** 0.1.0
**SPCify Version:** 0.1.0

---

## Purpose

This checklist ensures that BFHchart-based SPC charts meet clinical safety and accuracy standards before production deployment. All items must be validated and signed off before Task #32 (UI Integration) proceeds.

**Validation Scope:**
1. **Numerical Accuracy:** Control limits, center lines, data calculations
2. **Statistical Correctness:** Anhøj rules, chart type formulas
3. **Visual Clarity:** Interpretability for Danish clinical analysts
4. **Edge Case Handling:** Empty data, extreme values, missing data
5. **Feature Completeness:** Freeze periods, phases, comments

**Acceptance Criteria:**
- All P0 (critical) items: 100% pass rate
- All P1 (important) items: ≥95% pass rate
- All P2 (nice-to-have) items: Best effort

---

## Section 1: Chart Type Validation

### 1.1 Run Chart (Non-Parametric Process Monitoring)

**Clinical Use Case:** Monitoring infection rates without assuming normal distribution

**Statistical Method:**
- Center line: Median of all data points
- No control limits (median-based process monitoring)
- Anhøj rules: Runs test + crossings test

**Validation Tests:**

#### Basic Functionality
- [ ] **P0:** Median calculated correctly
  - **Test:** `test-spc-regression-bfh-vs-qic.R` - run-basic scenario
  - **Expected:** Median matches qicharts2 within ±0.001
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Data points plotted at correct positions
  - **Test:** Visual inspection + numerical comparison
  - **Expected:** X/Y coordinates match exactly
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P1:** Chart renders without errors for 24+ months data
  - **Test:** Manual test with real clinical data
  - **Expected:** No R errors, plot displays correctly
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

#### Anhøj Rules Detection
- [ ] **P0:** Runs test detects ≥8 consecutive points above median
  - **Test:** `test-anhoej-rules.R` - runs_above scenario
  - **Expected:** Exact same points flagged as qicharts2
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Runs test detects ≥8 consecutive points below median
  - **Test:** `test-anhoej-rules.R` - runs_below scenario
  - **Expected:** Exact same points flagged as qicharts2
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Crossings test detects too few median crossings
  - **Test:** `test-anhoej-rules.R` - crossings scenario
  - **Expected:** Signal triggered when crossings < expected
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P1:** Anhøj violations visually highlighted (red points/markers)
  - **Test:** Visual inspection of screenshot
  - **Expected:** Clear distinction between normal and signal points
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

#### Edge Cases
- [ ] **P0:** Single data point handled gracefully
  - **Test:** Manual test with n=1 dataset
  - **Expected:** Median = single value, no errors
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** All identical values handled (no variation)
  - **Test:** Manual test with constant values (e.g., all 10)
  - **Expected:** Median correct, no Anhøj signals
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P2:** Missing values (NA) excluded from calculation
  - **Test:** Manual test with NA in data
  - **Expected:** Median excludes NA, chart renders
  - **Result:** ☐ Pass ☐ Fail ☐ N/A
  - **Comments:** _____________________

**Run Chart Sign-Off:**
- ☐ **APPROVED** - Ready for production
- ☐ **CONDITIONAL** - Minor issues acceptable
- ☐ **REJECTED** - Blockers found

**Reviewer:** _____________ **Date:** _______

---

### 1.2 I Chart (Individuals Chart - Moving Range)

**Clinical Use Case:** Monitoring individual patient outcomes (e.g., length of stay)

**Statistical Method:**
- Center line: Mean of all values
- Control limits: Mean ± 2.66 × MR̄ (moving range)
- Anhøj rules applied

**Validation Tests:**

#### Basic Functionality
- [ ] **P0:** Mean calculated correctly
  - **Test:** `test-spc-regression-bfh-vs-qic.R` - i-basic scenario
  - **Expected:** Mean matches qicharts2 within ±0.001
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Moving range (MR̄) calculated correctly
  - **Test:** Regression test validates control limit formula
  - **Expected:** MR̄ = mean of |consecutive differences|
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Upper control limit (UCL) = Mean + 2.66 × MR̄
  - **Test:** Regression test
  - **Expected:** UCL matches qicharts2 within ±0.001
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Lower control limit (LCL) = Mean - 2.66 × MR̄
  - **Test:** Regression test
  - **Expected:** LCL matches qicharts2 within ±0.001
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

#### Special Cause Detection
- [ ] **P0:** Points above UCL flagged correctly
  - **Test:** Visual inspection + regression test
  - **Expected:** Any point > UCL highlighted
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Points below LCL flagged correctly
  - **Test:** Visual inspection + regression test
  - **Expected:** Any point < LCL highlighted
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Anhøj rules applied (runs + crossings)
  - **Test:** Regression test - i-anhoej scenario
  - **Expected:** Same Anhøj signals as qicharts2
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

#### Edge Cases
- [ ] **P0:** Two data points handled (minimal moving range)
  - **Test:** Manual test with n=2
  - **Expected:** MR̄ calculated, control limits rendered
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P2:** Negative control limits handled (LCL < 0)
  - **Test:** Manual test with low variance data
  - **Expected:** LCL shown or set to 0 if appropriate
  - **Result:** ☐ Pass ☐ Fail ☐ N/A
  - **Comments:** _____________________

**I Chart Sign-Off:**
- ☐ **APPROVED**
- ☐ **CONDITIONAL**
- ☐ **REJECTED**

**Reviewer:** _____________ **Date:** _______

---

### 1.3 P Chart (Proportions - Binomial Distribution)

**Clinical Use Case:** Monitoring infection rates (events / patients at risk)

**Statistical Method:**
- Center line: Overall proportion (Σy / Σn)
- Control limits: Binomial-based, variable per point
- UCL/LCL = p̄ ± 3√(p̄(1-p̄)/n)

**Validation Tests:**

#### Basic Functionality
- [ ] **P0:** Proportion calculated correctly (y/n for each point)
  - **Test:** `test-spc-regression-bfh-vs-qic.R` - p-basic scenario
  - **Expected:** Proportions match qicharts2 exactly
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Overall proportion (p̄) correct
  - **Test:** Regression test
  - **Expected:** p̄ = Σy / Σn, matches qicharts2 within ±0.001
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Variable control limits per point
  - **Test:** Regression test validates UCL/LCL arrays
  - **Expected:** Each point has different limits if n varies
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Binomial formula applied correctly
  - **Test:** Regression test validates UCL/LCL within ±0.001
  - **Expected:** UCL/LCL = p̄ ± 3√(p̄(1-p̄)/n_i)
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

#### Denominator Handling
- [ ] **P0:** Variable denominator (n) handled correctly
  - **Test:** Manual test with varying patient counts
  - **Expected:** Control limits adjust per point, wider for small n
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P1:** Fixed denominator produces constant limits
  - **Test:** Manual test with constant n
  - **Expected:** UCL/LCL constant across all points
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

#### Edge Cases
- [ ] **P0:** Zero events (y=0) handled
  - **Test:** Manual test with some y=0 points
  - **Expected:** Proportion = 0, control limits calculated
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** 100% proportion (y=n) handled
  - **Test:** Manual test with y=n for some points
  - **Expected:** Proportion = 1.0, control limits calculated
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P2:** Very small denominators (n<10) generate warning
  - **Test:** Manual test with n=5
  - **Expected:** Warning logged or displayed
  - **Result:** ☐ Pass ☐ Fail ☐ N/A
  - **Comments:** _____________________

**P Chart Sign-Off:**
- ☐ **APPROVED**
- ☐ **CONDITIONAL**
- ☐ **REJECTED**

**Reviewer:** _____________ **Date:** _______

---

### 1.4 C Chart (Counts - Fixed Poisson)

**Clinical Use Case:** Monitoring number of infections per fixed time period

**Statistical Method:**
- Center line: Mean count (c̄)
- Control limits: Poisson-based, c̄ ± 3√c̄
- Fixed sample size assumption

**Validation Tests:**

#### Basic Functionality
- [ ] **P0:** Mean count (c̄) calculated correctly
  - **Test:** `test-spc-regression-bfh-vs-qic.R` - c-basic scenario
  - **Expected:** c̄ matches qicharts2 within ±0.001
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Control limits use Poisson formula
  - **Test:** Regression test validates UCL = c̄ + 3√c̄
  - **Expected:** UCL/LCL within ±0.001
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** LCL ≥ 0 (no negative counts)
  - **Test:** Visual inspection + regression test
  - **Expected:** LCL = max(0, c̄ - 3√c̄)
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

#### Edge Cases
- [ ] **P0:** Zero counts handled (c=0 for some points)
  - **Test:** Manual test with c=0 data
  - **Expected:** Chart renders, control limits appropriate
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P2:** Low mean count (c̄ < 5) considered appropriate
  - **Test:** Manual test with low count data
  - **Expected:** Poisson assumption valid or warning shown
  - **Result:** ☐ Pass ☐ Fail ☐ N/A
  - **Comments:** _____________________

**C Chart Sign-Off:**
- ☐ **APPROVED**
- ☐ **CONDITIONAL**
- ☐ **REJECTED**

**Reviewer:** _____________ **Date:** _______

---

### 1.5 U Chart (Rates - Variable Poisson)

**Clinical Use Case:** Monitoring infection rate per 1000 patient-days (variable exposure)

**Statistical Method:**
- Center line: Overall rate (ū = Σy / Σn)
- Control limits: Poisson-adjusted per point, ū ± 3√(ū/n_i)
- Variable denominator (patient-days, etc.)

**Validation Tests:**

#### Basic Functionality
- [ ] **P0:** Rate calculated correctly (y/n for each point)
  - **Test:** `test-spc-regression-bfh-vs-qic.R` - u-basic scenario
  - **Expected:** Rates match qicharts2 exactly
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Overall rate (ū) correct
  - **Test:** Regression test
  - **Expected:** ū = Σy / Σn, matches within ±0.001
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Variable control limits per point
  - **Test:** Regression test validates UCL/LCL arrays
  - **Expected:** UCL/LCL = ū ± 3√(ū/n_i), within ±0.001
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

#### Denominator Handling
- [ ] **P0:** Variable denominator (patient-days) handled
  - **Test:** Manual test with varying exposure
  - **Expected:** Control limits wider for small n, narrower for large n
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P1:** Large denominator variation reflected visually
  - **Test:** Visual inspection with 10x variation in n
  - **Expected:** Control limit "funnel" clearly visible
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

#### Edge Cases
- [ ] **P0:** Zero events (y=0) with non-zero denominator
  - **Test:** Manual test with y=0, n>0
  - **Expected:** Rate = 0, control limits calculated
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P2:** Very small denominators (n<10) generate warning
  - **Test:** Manual test with small exposure
  - **Expected:** Warning or wide control limits
  - **Result:** ☐ Pass ☐ Fail ☐ N/A
  - **Comments:** _____________________

**U Chart Sign-Off:**
- ☐ **APPROVED**
- ☐ **CONDITIONAL**
- ☐ **REJECTED**

**Reviewer:** _____________ **Date:** _______

---

### 1.6 X̄ Chart (Subgroup Means - Normal Distribution)

**Clinical Use Case:** Monitoring average lab values per day (subgroup of measurements)

**Statistical Method:**
- Center line: Grand mean (X̿)
- Control limits: X̿ ± A₂ × R̄ (using range method)
- Assumes normal distribution within subgroups

**Validation Tests:**

#### Basic Functionality
- [ ] **P0:** Grand mean (X̿) calculated correctly
  - **Test:** `test-spc-regression-bfh-vs-qic.R` - xbar-basic scenario
  - **Expected:** X̿ matches qicharts2 within ±0.001
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Average range (R̄) calculated correctly
  - **Test:** Regression test validates control limit formula
  - **Expected:** R̄ = mean of subgroup ranges
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** A₂ factor applied correctly for subgroup size
  - **Test:** Regression test validates UCL/LCL within ±0.001
  - **Expected:** Correct A₂ value for n (e.g., A₂=0.577 for n=5)
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

#### Edge Cases
- [ ] **P0:** Subgroup size = 2 handled
  - **Test:** Manual test with n=2 subgroups
  - **Expected:** A₂ = 1.880, chart renders correctly
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P2:** Variable subgroup sizes handled or rejected
  - **Test:** Manual test with varying n
  - **Expected:** Error message or adaptive A₂ factors
  - **Result:** ☐ Pass ☐ Fail ☐ N/A
  - **Comments:** _____________________

**X̄ Chart Sign-Off:**
- ☐ **APPROVED**
- ☐ **CONDITIONAL**
- ☐ **REJECTED**

**Reviewer:** _____________ **Date:** _______

---

### 1.7 S Chart (Subgroup Standard Deviation)

**Clinical Use Case:** Monitoring variability in lab measurements (process stability)

**Statistical Method:**
- Center line: Average standard deviation (S̄)
- Control limits: Use c₄ factor for unbiased estimation
- Companion chart to X̄ chart

**Validation Tests:**

#### Basic Functionality
- [ ] **P0:** Average standard deviation (S̄) calculated correctly
  - **Test:** `test-spc-regression-bfh-vs-qic.R` - s-basic scenario
  - **Expected:** S̄ matches qicharts2 within ±0.001
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** c₄ factor applied for bias correction
  - **Test:** Regression test validates control limit formula
  - **Expected:** Correct c₄ for subgroup size n
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Control limits appropriate for variability chart
  - **Test:** Regression test validates UCL/LCL within ±0.001
  - **Expected:** UCL/LCL calculated correctly
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

#### Edge Cases
- [ ] **P0:** Subgroup size ≥ 2 enforced
  - **Test:** Manual test with n=1 (should fail)
  - **Expected:** Error message (cannot calculate SD from n=1)
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P2:** Zero variance subgroup handled
  - **Test:** Manual test with identical values in subgroup
  - **Expected:** SD = 0 for that subgroup, chart renders
  - **Result:** ☐ Pass ☐ Fail ☐ N/A
  - **Comments:** _____________________

**S Chart Sign-Off:**
- ☐ **APPROVED**
- ☐ **CONDITIONAL**
- ☐ **REJECTED**

**Reviewer:** _____________ **Date:** _______

---

## Section 2: Feature Validation

### 2.1 Freeze Period Handling

**Clinical Use Case:** Exclude pandemic period from control limit calculation

**Expected Behavior:**
- Freeze points plotted but excluded from statistics
- Vertical freeze line marker rendered
- Control limits recalculated excluding freeze range

**Validation Tests:**

- [ ] **P0:** Freeze points excluded from control limit calculation
  - **Test:** `test-spc-regression-bfh-vs-qic.R` - freeze scenarios
  - **Expected:** CL/UCL/LCL exclude freeze points, match qicharts2
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Freeze points still plotted (ghosted or distinct color)
  - **Test:** Visual inspection of freeze scenario screenshots
  - **Expected:** Freeze points visible but visually distinct
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P1:** Vertical freeze line marker rendered
  - **Test:** Visual inspection
  - **Expected:** Clear visual indicator of freeze period
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Multiple freeze ranges handled correctly
  - **Test:** Manual test with 2+ freeze periods
  - **Expected:** All freeze ranges excluded, multiple markers
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Freeze column accepts 0/1 or TRUE/FALSE
  - **Test:** Manual test with both formats
  - **Expected:** Both formats work correctly
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

**Edge Cases:**
- [ ] **P2:** All points frozen (entire dataset)
  - **Test:** Manual test with all freeze = 1
  - **Expected:** Error or warning (no data for calculation)
  - **Result:** ☐ Pass ☐ Fail ☐ N/A
  - **Comments:** _____________________

- [ ] **P2:** First/last point frozen (boundary)
  - **Test:** Manual test with freeze at extremes
  - **Expected:** Correct handling, control limits calculated
  - **Result:** ☐ Pass ☐ Fail ☐ N/A
  - **Comments:** _____________________

**Freeze Feature Sign-Off:**
- ☐ **APPROVED**
- ☐ **CONDITIONAL**
- ☐ **REJECTED**

**Reviewer:** _____________ **Date:** _______

---

### 2.2 Phase/Part Markers

**Clinical Use Case:** Separate control limits before/after intervention

**Expected Behavior:**
- Phase variable splits data into segments
- Control limits recalculated per phase
- Visual phase boundaries marked

**Validation Tests:**

- [ ] **P0:** Phase variable triggers recalculation per segment
  - **Test:** Manual test with 2 phases
  - **Expected:** Different CL/UCL/LCL for each phase
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P1:** Phase boundary line rendered vertically
  - **Test:** Visual inspection
  - **Expected:** Clear separation between phases
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Anhøj rules applied within each phase
  - **Test:** Manual test with Anhøj violation in phase 2
  - **Expected:** Signals detected per phase, not across boundary
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

**Edge Cases:**
- [ ] **P2:** Single-point phase handled
  - **Test:** Manual test with phase of n=1
  - **Expected:** Graceful handling or skip that phase
  - **Result:** ☐ Pass ☐ Fail ☐ N/A
  - **Comments:** _____________________

- [ ] **P1:** Multiple phases (>2) handled
  - **Test:** Manual test with 3+ phases
  - **Expected:** All phases calculated correctly
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

**Phase Feature Sign-Off:**
- ☐ **APPROVED**
- ☐ **CONDITIONAL**
- ☐ **REJECTED**

**Reviewer:** _____________ **Date:** _______

---

### 2.3 Comment Annotations

**Clinical Use Case:** Annotate significant events (e.g., "Ny hygiejneprotokol")

**Expected Behavior:**
- Comments displayed near data points
- Danish characters (æøå) rendered correctly
- XSS sanitization prevents script injection

**Validation Tests:**

- [ ] **P1:** Comments positioned correctly near points
  - **Test:** Visual inspection with comment_var provided
  - **Expected:** Comments readable, minimal overlap
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Danish special characters (æøå) display correctly
  - **Test:** Manual test with "Ændring i procedure"
  - **Expected:** Characters render without corruption
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P1:** Long comments truncated or wrapped
  - **Test:** Manual test with 100+ character comment
  - **Expected:** Graceful handling (ellipsis or wrap)
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** XSS sanitization prevents script injection
  - **Test:** Manual test with `<script>alert('XSS')</script>`
  - **Expected:** Script NOT executed, text sanitized
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

**Edge Cases:**
- [ ] **P2:** Empty comment string handled
  - **Test:** Manual test with "" or NA in comment column
  - **Expected:** No annotation shown, no error
  - **Result:** ☐ Pass ☐ Fail ☐ N/A
  - **Comments:** _____________________

- [ ] **P1:** Multiple comments on adjacent points
  - **Test:** Visual inspection with dense annotations
  - **Expected:** Collision avoidance applied
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

**Comment Feature Sign-Off:**
- ☐ **APPROVED**
- ☐ **CONDITIONAL**
- ☐ **REJECTED**

**Reviewer:** _____________ **Date:** _______

---

## Section 3: Visual Quality & Accessibility

### 3.1 Color Contrast & Readability

**WCAG 2.1 Standards:**
- Text: 4.5:1 contrast ratio (AA level)
- UI components: 3:1 contrast ratio

**Validation Tests:**

- [ ] **P1:** Data points distinguishable from background
  - **Test:** Visual inspection + contrast checker tool
  - **Expected:** ≥3:1 contrast ratio
  - **Result:** ☐ Pass ☐ Fail
  - **Contrast Ratio:** _______:1

- [ ] **P1:** Anhøj signal highlights sufficient contrast
  - **Test:** Visual inspection + contrast checker
  - **Expected:** ≥3:1 contrast ratio for red markers
  - **Result:** ☐ Pass ☐ Fail
  - **Contrast Ratio:** _______:1

- [ ] **P1:** Control limits visible against background
  - **Test:** Visual inspection
  - **Expected:** ≥3:1 contrast ratio for dashed lines
  - **Result:** ☐ Pass ☐ Fail
  - **Contrast Ratio:** _______:1

- [ ] **P2:** Colorblind-friendly palette
  - **Test:** Simulate deuteranopia (common red-green blindness)
  - **Expected:** Signals still distinguishable
  - **Result:** ☐ Pass ☐ Fail ☐ N/A
  - **Comments:** _____________________

**Accessibility Sign-Off:**
- ☐ **APPROVED**
- ☐ **CONDITIONAL**
- ☐ **REJECTED**

**Reviewer:** _____________ **Date:** _______

---

### 3.2 Label Clarity & Font Sizing

**Standards:**
- Minimum font size: 10pt for axis labels
- Minimum font size: 8pt for data labels
- Danish characters rendered correctly

**Validation Tests:**

- [ ] **P1:** Axis labels readable at standard screen distance
  - **Test:** Visual inspection at 60cm viewing distance
  - **Expected:** Labels ≥10pt, clear text
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P1:** Data point labels don't overlap excessively
  - **Test:** Visual inspection with dense data (n>50)
  - **Expected:** Collision avoidance reduces overlap to <10%
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Danish month names displayed correctly
  - **Test:** Visual inspection (januar, februar, marts...)
  - **Expected:** Correct Danish localization
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P1:** Y-axis number formatting appropriate
  - **Test:** Visual inspection with large/small values
  - **Expected:** Scientific notation or thousands separators
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

**Label Quality Sign-Off:**
- ☐ **APPROVED**
- ☐ **CONDITIONAL**
- ☐ **REJECTED**

**Reviewer:** _____________ **Date:** _______

---

## Section 4: Edge Case & Error Handling

### 4.1 Data Quality Issues

**Validation Tests:**

- [ ] **P0:** Empty dataset (n=0) handled gracefully
  - **Test:** Manual test with empty data frame
  - **Expected:** Error message, no crash
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Missing values (NA) in Y variable
  - **Test:** Manual test with some NA values
  - **Expected:** NAs excluded, warning logged
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Non-numeric Y variable rejected
  - **Test:** Manual test with character column
  - **Expected:** Error message with clear guidance
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P1:** Extreme outliers don't break axis scaling
  - **Test:** Manual test with value 1000x larger than mean
  - **Expected:** Chart renders, outlier visible
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Negative values handled appropriately
  - **Test:** Manual test with negative Y values
  - **Expected:** Accepted for I/run charts, rejected/warned for count charts
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

**Edge Case Sign-Off:**
- ☐ **APPROVED**
- ☐ **CONDITIONAL**
- ☐ **REJECTED**

**Reviewer:** _____________ **Date:** _______

---

### 4.2 Performance & Scalability

**Performance Standards:**
- Small dataset (n<50): <200ms render time
- Medium dataset (n=50-200): <500ms render time
- Large dataset (n>200): <1000ms render time

**Validation Tests:**

- [ ] **P1:** Small dataset renders quickly (n=24)
  - **Test:** Benchmark with typical monthly data
  - **Expected:** <200ms total time
  - **Result:** ☐ Pass ☐ Fail
  - **Measured Time:** _______ ms

- [ ] **P1:** Medium dataset acceptable (n=120)
  - **Test:** Benchmark with 10 years monthly data
  - **Expected:** <500ms total time
  - **Result:** ☐ Pass ☐ Fail
  - **Measured Time:** _______ ms

- [ ] **P2:** Large dataset doesn't crash (n=500)
  - **Test:** Stress test with daily data over 2 years
  - **Expected:** <1000ms OR graceful degradation
  - **Result:** ☐ Pass ☐ Fail ☐ N/A
  - **Measured Time:** _______ ms

**Performance Sign-Off:**
- ☐ **APPROVED**
- ☐ **CONDITIONAL**
- ☐ **REJECTED**

**Reviewer:** _____________ **Date:** _______

---

## Section 5: Integration & Workflow

### 5.1 SPCify App Integration Readiness

**Validation Tests:**

- [ ] **P0:** Service layer API stable (`compute_spc_results_bfh()`)
  - **Test:** Review `R/fct_spc_bfh_service.R` implementation
  - **Expected:** Returns standardized `list(plot, qic_data, metadata)`
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** Error handling prevents app crashes
  - **Test:** Manual test with invalid inputs
  - **Expected:** Errors caught, user-friendly messages shown
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P1:** Structured logging enabled for debugging
  - **Test:** Review logs during manual testing
  - **Expected:** Key events logged with context
  - **Result:** ☐ Pass ☐ Fail
  - **Comments:** _____________________

- [ ] **P0:** All regression tests pass
  - **Test:** Run `devtools::test(filter = "regression")`
  - **Expected:** 0 failures, 0 warnings
  - **Result:** ☐ Pass ☐ Fail
  - **Test Output:** _____________________

**Integration Sign-Off:**
- ☐ **APPROVED** - Ready for Task #32 (UI Integration)
- ☐ **CONDITIONAL** - Minor issues acceptable
- ☐ **REJECTED** - Blockers must be resolved

**Reviewer:** _____________ **Date:** _______

---

## Section 6: Clinical Safety Assessment

### 6.1 Overall Clinical Safety

**Critical Questions:**

- [ ] **P0:** Could any calculation errors lead to incorrect clinical decisions?
  - **Assessment:** ☐ No risk ☐ Low risk ☐ Medium risk ☐ High risk
  - **Explanation:** _____________________

- [ ] **P0:** Could any visual differences mislead clinical analysts?
  - **Assessment:** ☐ No risk ☐ Low risk ☐ Medium risk ☐ High risk
  - **Explanation:** _____________________

- [ ] **P0:** Are edge cases handled without silent failures?
  - **Assessment:** ☐ All handled ☐ Some gaps ☐ Major gaps
  - **Explanation:** _____________________

- [ ] **P0:** Is Anhøj rule detection reliable for special cause identification?
  - **Assessment:** ☐ Reliable ☐ Mostly reliable ☐ Unreliable
  - **Explanation:** _____________________

**Risk Mitigation:**

If any medium/high risks identified:
- [ ] Risks documented in `docs/migration/bfh-workarounds.md`
- [ ] Mitigation plan created
- [ ] Clinical analyst approval obtained for accepted risks

**Clinical Safety Sign-Off:**
- ☐ **APPROVED** - No clinical safety concerns
- ☐ **CONDITIONAL** - Acceptable risks documented and mitigated
- ☐ **REJECTED** - Unacceptable clinical risks identified

**Reviewer:** _____________ **Date:** _______

---

## Section 7: Final Approval

### 7.1 Overall Validation Summary

**Chart Types Validated:**
- Run chart: ☐ Approved ☐ Conditional ☐ Rejected
- I chart: ☐ Approved ☐ Conditional ☐ Rejected
- P chart: ☐ Approved ☐ Conditional ☐ Rejected
- C chart: ☐ Approved ☐ Conditional ☐ Rejected
- U chart: ☐ Approved ☐ Conditional ☐ Rejected
- X̄ chart: ☐ Approved ☐ Conditional ☐ Rejected
- S chart: ☐ Approved ☐ Conditional ☐ Rejected

**Features Validated:**
- Freeze period: ☐ Approved ☐ Conditional ☐ Rejected
- Phase markers: ☐ Approved ☐ Conditional ☐ Rejected
- Comments: ☐ Approved ☐ Conditional ☐ Rejected

**Quality Metrics:**
- Visual quality: ☐ Approved ☐ Conditional ☐ Rejected
- Edge cases: ☐ Approved ☐ Conditional ☐ Rejected
- Performance: ☐ Approved ☐ Conditional ☐ Rejected
- Clinical safety: ☐ Approved ☐ Conditional ☐ Rejected

---

### 7.2 Identified Issues & Workarounds

**P0 Issues (Blockers):**
1. _____________________
2. _____________________

**P1 Issues (Important):**
1. _____________________
2. _____________________

**P2 Issues (Nice-to-have):**
1. _____________________
2. _____________________

**Workarounds Implemented:**
- Documented in: `docs/migration/bfh-workarounds.md`
- All P0 workarounds validated: ☐ Yes ☐ No

---

### 7.3 Final Deployment Decision

**Recommendation:**

- ☐ **APPROVED FOR PRODUCTION** - BFHchart migration ready for Task #32 (UI Integration)
  - All P0 items pass
  - All P1 items pass OR acceptable workarounds documented
  - Clinical safety confirmed
  - Ready for production deployment

- ☐ **CONDITIONAL APPROVAL** - Minor issues acceptable with documentation
  - Some P1 items conditional
  - Workarounds documented and validated
  - Clinical safety confirmed with documented limitations
  - Ready for controlled rollout with monitoring

- ☐ **REJECTED** - Blockers must be resolved before deployment
  - P0 items failing OR
  - Clinical safety concerns OR
  - Unacceptable numerical discrepancies
  - NOT ready for Task #32 - must resolve issues first

**Conditions (if conditional approval):**
1. _____________________
2. _____________________

**Required Actions (if rejected):**
1. _____________________
2. _____________________

---

### 7.4 Clinical Analyst Certification

**I certify that:**
- [ ] I have reviewed all sections of this checklist
- [ ] I have personally validated critical calculations
- [ ] I have reviewed visual comparisons for all chart types
- [ ] I understand all documented workarounds
- [ ] I confirm clinical safety for the intended use case
- [ ] I approve (or conditionally approve) deployment

**Validator Name:** _____________________
**Professional Title:** _____________________
**Institution:** _____________________
**Signature:** _____________________ **Date:** __________

**Clinical Perspective & Comments:**

```
[Clinical analyst's overall assessment, concerns, and recommendations]
```

---

## Appendix A: Test Execution Log

**Test Suite:** `devtools::test()`
**Execution Date:** _____________________

| Test File | Tests Run | Passed | Failed | Warnings | Notes |
|-----------|-----------|--------|--------|----------|-------|
| test-anhoej-rules.R | __ | __ | __ | __ | __________ |
| test-spc-regression-bfh-vs-qic.R | __ | __ | __ | __ | __________ |
| test-spc-bfh-service.R | __ | __ | __ | __ | __________ |

**Total:** _____ tests, _____ passed, _____ failed

**Failures Details:**
```
[Copy paste test failure output if any]
```

---

## Appendix B: Screenshot Reference

**Location:** `docs/migration/screenshots/`

| Chart Type | Scenario | qicharts2 File | BFHchart File | Comparison Status |
|------------|----------|----------------|---------------|-------------------|
| Run | Basic | qic-run-basic.png | bfh-run-basic.png | ☐ Approved ☐ Issues |
| Run | Anhøj | qic-run-anhoej.png | bfh-run-anhoej.png | ☐ Approved ☐ Issues |
| Run | Freeze | qic-run-freeze.png | bfh-run-freeze.png | ☐ Approved ☐ Issues |
| I | Basic | qic-i-basic.png | bfh-i-basic.png | ☐ Approved ☐ Issues |
| I | Anhøj | qic-i-anhoej.png | bfh-i-anhoej.png | ☐ Approved ☐ Issues |
| I | Freeze | qic-i-freeze.png | bfh-i-freeze.png | ☐ Approved ☐ Issues |
| P | Basic | qic-p-basic.png | bfh-p-basic.png | ☐ Approved ☐ Issues |
| P | Anhøj | qic-p-anhoej.png | bfh-p-anhoej.png | ☐ Approved ☐ Issues |
| P | Freeze | qic-p-freeze.png | bfh-p-freeze.png | ☐ Approved ☐ Issues |
| C | Basic | qic-c-basic.png | bfh-c-basic.png | ☐ Approved ☐ Issues |
| C | Anhøj | qic-c-anhoej.png | bfh-c-anhoej.png | ☐ Approved ☐ Issues |
| C | Freeze | qic-c-freeze.png | bfh-c-freeze.png | ☐ Approved ☐ Issues |
| U | Basic | qic-u-basic.png | bfh-u-basic.png | ☐ Approved ☐ Issues |
| U | Anhøj | qic-u-anhoej.png | bfh-u-anhoej.png | ☐ Approved ☐ Issues |
| U | Freeze | qic-u-freeze.png | bfh-u-freeze.png | ☐ Approved ☐ Issues |
| X̄ | Basic | qic-xbar-basic.png | bfh-xbar-basic.png | ☐ Approved ☐ Issues |
| X̄ | Anhøj | qic-xbar-anhoej.png | bfh-xbar-anhoej.png | ☐ Approved ☐ Issues |
| X̄ | Freeze | qic-xbar-freeze.png | bfh-xbar-freeze.png | ☐ Approved ☐ Issues |
| S | Basic | qic-s-basic.png | bfh-s-basic.png | ☐ Approved ☐ Issues |
| S | Anhøj | qic-s-anhoej.png | bfh-s-anhoej.png | ☐ Approved ☐ Issues |
| S | Freeze | qic-s-freeze.png | bfh-s-freeze.png | ☐ Approved ☐ Issues |

**Total Screenshots:** 42 (21 pairs)

---

## Appendix C: Related Documentation

**Task #31 Documentation:**
- Visual Differences: `docs/migration/bfh-visual-differences.md`
- Workarounds: `docs/migration/bfh-workarounds.md`
- This Checklist: `docs/migration/clinical-validation-checklist.md`

**Task Analysis:**
- `.claude/epics/bfhcharts-spc-migration/31-analysis.md`

**Test Suites:**
- Anhøj Rules: `tests/testthat/test-anhoej-rules.R` (Stream A)
- Regression Tests: `tests/testthat/test-spc-regression-bfh-vs-qic.R` (Stream B)
- Baselines: `tests/testthat/fixtures/qic-baseline/*.rds` (Task #29)

**Service Layer:**
- `R/fct_spc_bfh_service.R` - Primary BFHchart integration
- `R/fct_anhoej_rules.R` - Anhøj rules workaround (if implemented)

---

**Checklist Version:** 1.0
**Last Updated:** 2025-10-15
**Maintained By:** SPCify Development Team
**Clinical Review Cycle:** Before each major release
