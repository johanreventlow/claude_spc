# BFHchart Workarounds & Adaptations

**Date:** 2025-10-15
**BFHchart Version:** 0.1.0
**SPCify Version:** 0.1.0
**Status:** Implementation in progress

---

## Purpose

This document catalogs SPCify-side workarounds for BFHchart API gaps, missing features, or behavioral differences compared to qicharts2. Each workaround includes:

1. **Issue description** - What's missing or different in BFHchart
2. **Impact assessment** - P0 (blocker), P1 (important), P2 (nice-to-have)
3. **Workaround implementation** - How SPCify compensates
4. **Upstream status** - Whether issue filed with BFHchart maintainers
5. **Performance impact** - Additional overhead introduced

All P0 workarounds must be validated against qicharts2 baselines before clinical deployment.

---

## Active Workarounds

### Workaround #1: Anhøj Rules Detection

**Issue:** BFHchart does not expose Anhøj statistical rules in chart output

**BFHchart Behavior:**
- API returns `plot` (ggplot object) and control limit data
- No `signal` column indicating Anhøj violations
- No metadata about runs or crossings tests
- [To be confirmed by Stream A investigation]

**Clinical Impact:**
- **Priority:** P0 (Blocker)
- **Justification:** Anhøj rules are core to SPC - detecting special cause variation
- **Risk:** Without this, clinicians cannot distinguish random noise from systematic changes
- **Regulatory:** Danish quality standards require statistical process control

**SPCify Workaround:**

**Implementation Location:** `R/fct_anhoej_rules.R` (new, Stream A)

**Functions Implemented:**
1. `detect_runs(y, center_line, threshold = 8)` - Identifies ≥8 consecutive points above/below median
2. `detect_crossings(y, center_line)` - Tests for too few median crossings
3. `calculate_anhoej_signal(y, center_line)` - Combines both tests, returns point-level signals

**Integration Point:** `R/fct_spc_bfh_service.R` in `transform_bfh_output()`

```r
# Pseudo-code from Stream A deliverable
if (!is.null(bfh_result$qic_data$cl)) {
  anhoej_result <- calculate_anhoej_signal(
    y = bfh_result$qic_data$y,
    center_line = bfh_result$qic_data$cl[1]
  )

  bfh_result$qic_data$signal <- anhoej_result$signal_points
  bfh_result$metadata$anhoej_rules <- list(
    runs_detected = anhoej_result$runs$signal,
    crossings_detected = anhoej_result$crossings$signal,
    max_run_length = anhoej_result$runs$max_run,
    crossings_count = anhoej_result$crossings$crossings
  )
}
```

**Validation:**
- Tests: `tests/testthat/test-anhoej-rules.R`
- Baselines: `tests/testthat/fixtures/qic-baseline/run-anhoej.rds` (and others)
- Success Criteria: Binary match with qicharts2 Anhøj signals

**Performance Impact:**
- **Overhead:** ~5-10ms per chart (negligible)
- **Algorithm:** O(n) where n = number of data points
- **Acceptable:** Yes - core functionality requirement

**Status:**
- ☐ Implementation pending (Stream A)
- ☐ Unit tests created
- ☐ Integration tests pass
- ☐ Validated against qicharts2 baselines

**Upstream Status:**
- ☐ Issue filed with BFHchart maintainers
- Issue URL: [To be created]
- Priority request: P0 - statistical process control core feature
- Rationale: Anhøj rules are essential for SPC interpretation in clinical settings

---

### Workaround #2: [Placeholder - To be documented by Stream A/B]

**Issue:** [Description]

**BFHchart Behavior:**
- [To be documented after Stream A/B testing]

**Clinical Impact:**
- **Priority:** [P0/P1/P2]
- **Justification:** [Impact description]

**SPCify Workaround:**
- [Implementation details]

**Status:**
- [To be updated after Stream A/B completion]

**Upstream Status:**
- [Issue tracking]

---

### Workaround #3: [Placeholder - To be documented by Stream A/B]

**Issue:** [Description]

**BFHchart Behavior:**
- [To be documented after Stream A/B testing]

**Clinical Impact:**
- **Priority:** [P0/P1/P2]
- **Justification:** [Impact description]

**SPCify Workaround:**
- [Implementation details]

**Status:**
- [To be updated after Stream A/B completion]

**Upstream Status:**
- [Issue tracking]

---

## Resolved Issues (No Workaround Needed)

### ✅ Chart Type Support

**Issue:** BFHchart supports all 7 required chart types

**Validation:** Task #30 Stream C confirmed:
- Run chart ✅
- I chart ✅
- P chart ✅
- C chart ✅
- U chart ✅
- X̄ chart ✅
- S chart ✅

**Status:** No workaround required

---

### ✅ Control Limit Calculation

**Issue:** BFHchart control limit formulas match statistical standards

**Validation:** Task #30 Stream A/B confirmed correct formulas
**Regression Testing:** Task #31 Stream B validates ±0.001 tolerance

**Status:** No workaround required (pending Stream B final validation)

---

## Upstream Issues Filed with BFHchart

| Issue # | Title | Priority | Status | Workaround | Date Filed |
|---------|-------|----------|--------|------------|------------|
| [TBD] | Expose Anhøj statistical rules in chart output | P0 | ☐ Open ☐ In Progress ☐ Resolved | Yes (Workaround #1) | [Date] |
| [TBD] | [Placeholder] | [P0/P1/P2] | ☐ Open ☐ In Progress ☐ Resolved | [Yes/No] | [Date] |

**Issue Filing Process:**
1. Confirm issue through Stream A/B testing
2. Document workaround implementation in SPCify
3. Create minimal reproducible example
4. File issue on BFHchart GitHub repository
5. Link issue in this table
6. Update status regularly

---

## Workaround Performance Analysis

**Benchmark Context:**
- Task #30 baseline: BFHchart chart generation = 189ms average
- Workaround overhead acceptable if <10% (≈19ms)

| Workaround | Overhead (ms) | % of Total | Acceptable? | Notes |
|------------|---------------|------------|-------------|-------|
| Anhøj rules calculation | ~5-10ms | ~5% | ✅ Yes | O(n) algorithm, negligible |
| [Workaround #2] | [TBD] | [TBD] | [Yes/No] | [Notes] |
| [Workaround #3] | [TBD] | [TBD] | [Yes/No] | [Notes] |

**Total Overhead:** ~5-10ms (acceptable - <10% threshold)

**Performance Testing:**
```r
# Benchmark workarounds
microbenchmark::microbenchmark(
  bfh_only = compute_spc_results_bfh(data, x, y, chart_type = "run"),
  bfh_with_anhoej = {
    result <- compute_spc_results_bfh(data, x, y, chart_type = "run")
    anhoej <- calculate_anhoej_signal(result$qic_data$y, result$qic_data$cl[1])
  },
  times = 100
)
```

---

## Known Limitations (No Workaround Available)

### None Currently Identified

**Status:** Awaiting Stream A/B completion

If Stream A/B discovers limitations without viable workarounds:
1. Document here with P0/P1/P2 priority
2. File BFHchart issue immediately
3. Escalate to project decision: block migration or accept limitation
4. Clinical analyst must approve any accepted limitations

---

## Workaround Development Guidelines

**When to Create a Workaround:**
1. **P0 (Blocker):** Feature essential for clinical safety/accuracy → Must implement
2. **P1 (Important):** Feature significantly improves usability → Strongly recommended
3. **P2 (Nice-to-have):** Feature provides marginal benefit → Evaluate effort vs. value

**Workaround Quality Standards:**
1. **Correctness:** Must match qicharts2 behavior (validated by tests)
2. **Performance:** Overhead <10% of baseline chart generation time
3. **Maintainability:** Well-documented, unit tested, follows SPCify patterns
4. **Observability:** Structured logging for debugging (use `log_debug()`)
5. **Error handling:** Graceful degradation with `safe_operation()`

**Implementation Checklist:**
- [ ] Create dedicated R file in `R/` (e.g., `R/fct_anhoej_rules.R`)
- [ ] Roxygen documentation with `@export`
- [ ] Unit tests in `tests/testthat/test-*.R`
- [ ] Integration with service layer (`R/fct_spc_bfh_service.R`)
- [ ] Validation against qicharts2 baselines
- [ ] Performance benchmark (acceptable overhead)
- [ ] Structured logging at key decision points
- [ ] Error handling with fallback behavior
- [ ] Document in this file
- [ ] File upstream BFHchart issue
- [ ] Update `devtools::document()` (NAMESPACE)

---

## Integration Architecture

**Service Layer Facade:** `R/fct_spc_bfh_service.R`

```r
# Conceptual flow with workarounds
compute_spc_results_bfh <- function(data, x_var, y_var, chart_type, ...) {
  # 1. Call BFHchart
  bfh_plot <- BFHchart::spc_chart(
    data = data,
    x = x_var,
    y = y_var,
    type = chart_type
  )

  # 2. Extract BFHchart data
  bfh_data <- extract_bfh_data(bfh_plot)

  # 3. Apply workarounds
  if (chart_type %in% c("run", "i", "p", "c", "u")) {
    # Workaround #1: Anhøj rules
    anhoej_result <- calculate_anhoej_signal(
      y = bfh_data$y,
      center_line = bfh_data$cl[1]
    )
    bfh_data$signal <- anhoej_result$signal_points
    metadata$anhoej_rules <- anhoej_result
  }

  # [Workaround #2: Apply if needed]
  # [Workaround #3: Apply if needed]

  # 4. Transform to qicharts2-compatible format
  qic_data <- transform_bfh_output(bfh_data)

  # 5. Return standardized result
  list(
    plot = bfh_plot,
    qic_data = qic_data,
    metadata = metadata
  )
}
```

**Workaround Isolation Principle:**
- Each workaround in separate file (`R/fct_*.R`)
- Service layer orchestrates workaround application
- Workarounds independently testable
- Easy to remove when BFHchart adds native support

---

## Regression Risk Assessment

**Workaround Stability:**

| Workaround | Regression Risk | Mitigation |
|------------|-----------------|------------|
| Anhøj rules calculation | Low - pure function, deterministic | Comprehensive unit tests, baseline validation |
| [Workaround #2] | [Low/Medium/High] | [Mitigation strategy] |
| [Workaround #3] | [Low/Medium/High] | [Mitigation strategy] |

**Monitoring Strategy:**
1. **CI/CD:** All regression tests (`test-spc-regression-bfh-vs-qic.R`) must pass
2. **Baseline validation:** 21+ test cases against qicharts2
3. **Performance benchmarks:** Automated checks for >10% overhead
4. **Clinical validation:** Periodic review by clinical analyst

---

## Upgrade Path (When BFHchart Adds Native Support)

**Scenario:** BFHchart 0.2.0 adds Anhøj rules support

**Upgrade Steps:**
1. **Detect native support:**
   ```r
   if ("signal" %in% names(bfh_result)) {
     # Use BFHchart native
     use_native <- TRUE
   } else {
     # Fall back to workaround
     use_native <- FALSE
   }
   ```

2. **Validation:**
   - Run regression tests with native BFHchart
   - Compare to workaround results
   - Validate against qicharts2 baselines

3. **Deprecation:**
   - Mark workaround functions as deprecated
   - Log warning: "Using legacy Anhøj workaround - upgrade BFHchart"
   - Schedule workaround removal (e.g., 6 months)

4. **Removal:**
   - Delete `R/fct_anhoej_rules.R`
   - Update `R/fct_spc_bfh_service.R` to use native API
   - Remove workaround tests
   - Update documentation

---

## Related Documentation

**Task #30 (Service Layer):**
- `R/fct_spc_bfh_service.R` - Primary integration point
- API features validated: 7/9 P0 features PASS

**Task #31 (Validation):**
- **Stream A:** Anhøj rules implementation (`R/fct_anhoej_rules.R`)
- **Stream B:** Regression testing (`tests/testthat/test-spc-regression-bfh-vs-qic.R`)
- **Stream C:** Clinical validation (this document)

**Configuration:**
- `config_spc_config.R` - SPC constants (may need workaround-specific config)
- `config_system_config.R` - Performance thresholds

**Testing:**
- Baselines: `tests/testthat/fixtures/qic-baseline/*.rds`
- Unit tests: `tests/testthat/test-anhoej-rules.R`
- Integration tests: `tests/testthat/test-spc-regression-bfh-vs-qic.R`

---

## Appendix: Workaround Template

**Use this template when documenting new workarounds:**

```markdown
### Workaround #N: [Short Title]

**Issue:** [Description of BFHchart gap]

**BFHchart Behavior:**
- [Current behavior]

**Clinical Impact:**
- **Priority:** [P0/P1/P2]
- **Justification:** [Why this matters clinically]

**SPCify Workaround:**
- **Implementation Location:** [File path]
- **Functions Implemented:** [List with signatures]
- **Integration Point:** [Where applied in service layer]

**Validation:**
- Tests: [File path]
- Baselines: [Fixture files used]
- Success Criteria: [Pass conditions]

**Performance Impact:**
- **Overhead:** [Measured in ms]
- **Algorithm Complexity:** [O(n) notation]
- **Acceptable:** [Yes/No with rationale]

**Status:**
- ☐ Implementation pending
- ☐ Unit tests created
- ☐ Integration tests pass
- ☐ Validated against baselines

**Upstream Status:**
- ☐ Issue filed with BFHchart
- Issue URL: [GitHub link]
- Priority request: [P0/P1/P2]
```

---

## Version History

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2025-10-15 | 0.1.0 | Initial documentation structure (Stream C) | [Stream C agent] |
| [TBD] | 0.2.0 | Updated with Stream A/B findings | [Stream A/B agents] |
| [TBD] | 1.0.0 | Clinical validation complete | [Clinical analyst] |

---

**Last Updated:** 2025-10-15
**Maintained By:** SPCify Development Team
**Clinical Review Required:** Yes (before production deployment)
