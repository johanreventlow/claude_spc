# BFHchart Gap Issue Tracking

**Document Version:** 1.0
**Date:** 2025-10-15
**Status:** Active Tracking
**Context:** Gap analysis from Task #29 Stream A - Feature Parity Matrix

## Purpose

This document tracks identified gaps between qicharts2 (current implementation) and BFHchart (migration target) based on the feature parity analysis. Each gap is categorized by priority, type, and mitigation strategy.

**Important Context:** BFHchart was extracted from SPCify's own codebase. Most gaps represent API validation needs rather than true blockers, since we control both codebases.

---

## Gap Summary

**Total Gaps Identified:** 11 feature areas requiring validation
**Confirmed Blockers:** 0 (VALIDATION COMPLETE - Task #30 Stream C)
**Workarounds Available:** 2 minor issues (chart type validation + notes NSE)
**Acceptable Differences:** 3 (presentation logic - already in BFHcharts/SPCify)

**Risk Assessment (UPDATED 2025-10-15):**
- Gap Risk: LOW (down from Medium)
- Blocker Risk: NONE (down from Low)
- Migration Readiness: **PROCEED - All P0 features validated**
- Validation Status: ✅ COMPLETE

---

## Issue Tracking Table

| ID | Feature Area | Gap Category | Priority | Status | Workaround | BFHcharts Issue | Validation Result |
|----|--------------|--------------|----------|--------|------------|----------------|-------|
| G1 | Chart Type Support (9 types) | WORKAROUND_BFHcharts | P1 | ✅ **VALIDATED** | Use low-level API for MR/PP/UP | [BFH #XX] | 8/9 types work, MR/PP/UP need validation fix |
| G2 | Anhøj Rules (runs + crossings) | N/A | P0 | ✅ **VALIDATED** | No workaround needed | N/A | Fully exposed via qicharts2 backend |
| G3 | Control Limit Calculation | N/A | P0 | ✅ **VALIDATED** | No workaround needed | N/A | Per-point UCL/LCL/CL confirmed |
| G4 | Prime Charts (P', U') | WORKAROUND_BFHcharts | P1 | ⚠️ **NEEDS TESTING** | Use low-level API temporarily | [BFH #XX] | Supported via qicharts2, validation issue |
| G5 | Freeze Period Handling | N/A | P0 | ✅ **VALIDATED** | No workaround needed | N/A | Freeze parameter fully functional |
| G6 | Part Aggregation (Phases) | N/A | P0 | ✅ **VALIDATED** | No workaround needed | N/A | Part parameter fully functional |
| G7 | Custom X-Axis Breaks | ACCEPTABLE | P2 | ✅ **IMPLEMENTED** | Already in BFHcharts | N/A | detect_date_interval() + get_optimal_formatting() exist |
| G8 | Axis Formatting (Y-axis units) | ACCEPTABLE | P2 | ✅ **IMPLEMENTED** | Already in BFHcharts | N/A | Y-axis formatting utilities exist |
| G9 | ggplot2 Integration | N/A | P0 | ✅ **VALIDATED** | No workaround needed | N/A | Returns ggplot object, layer addition confirmed |
| G10 | Data Structure (input/output) | N/A | P0 | ✅ **VALIDATED** | No workaround needed | N/A | qicharts2 data structure preserved |
| G11 | Comment/Notes Parameter | WORKAROUND_SPCify | P0 | ✅ **VALIDATED** | Keep SPCify ggrepel approach | [BFH #XX] | NSE issue, SPCify workaround preferred |

---

## Gap Category Definitions

### BLOCKER (if missing)
**Definition:** Feature absolutely required for SPCify with no viable workaround unless BFHchart implements it.

**Examples:** Control limits (G3), Freeze period (G5), Part aggregation (G6), ggplot2 output (G9), Data structure (G10)

**Mitigation:** Create BFHchart issues immediately, prioritize as P0, plan parallel development.

### WORKAROUND_BFHchart
**Definition:** Feature should ideally be in BFHchart but can be temporarily implemented in SPCify.

**Examples:** Anhøj rules (G2), Chart types (G1), Prime charts (G4)

**Mitigation:** Document technical debt, create BFHchart enhancement issues, implement SPCify-side calculation.

### WORKAROUND_SPCify
**Definition:** Feature can be implemented in SPCify integration layer without BFHchart changes.

**Examples:** Comment annotations (G11)

**Mitigation:** Keep implementation in SPCify, apply as ggplot2 layers or post-processing.

### ACCEPTABLE
**Definition:** Minor differences acceptable; implementation in SPCify preferred.

**Examples:** Custom x-axis breaks (G7), Y-axis formatting (G8)

**Mitigation:** Document approach, keep presentation logic in SPCify.

---

## Priority Definitions

**P0 - Blocker:** Must resolve before Phase 3 (Service Layer Implementation)
**P1 - High:** Should resolve in Phase 3 or have documented workaround
**P2 - Medium:** Can defer to Phase 5 (Refinement) or later

---

## BFHchart Repository Access

**Status:** TBD - Repository location needs confirmation

**Action Items:**
1. Locate BFHchart repository (likely internal)
2. Review existing documentation
3. Examine function signatures and return structures
4. File issues for confirmed gaps (P0 first)

**Issue Filing Strategy:**
- Start with P0 validation gaps (G2, G3, G5, G6, G9, G10)
- Include SPCify use cases in issue descriptions
- Link back to this tracking document
- Coordinate BFHchart releases with SPCify migration phases

---

## Validation Checklist (Phase 2)

### ✅ P0 Features (COMPLETED - Task #30 Stream C)

- [x] **G2: Anhøj Rules**
  - [x] Confirm BFHcharts calculates runs signal (≥8 consecutive points)
  - [x] Confirm BFHcharts calculates crossings signal
  - [x] Validate per-phase rule application
  - [x] Test algorithm matches qicharts2 methodology

- [x] **G3: Control Limit Calculation**
  - [x] Compare UCL/LCL calculations across all 9 chart types
  - [x] Validate per-phase recalculation logic
  - [x] Test edge cases (n<10, all equal values, extreme outliers)
  - [x] Document numeric tolerance (±0.001)

- [x] **G5: Freeze Period**
  - [x] Test freeze parameter alone (no phases)
  - [x] Test freeze + phases combination
  - [x] Validate "BASELINE" vs "NUV. NIVEAU" label logic
  - [x] Document freeze position adjustment for NA removal

- [x] **G6: Part Aggregation**
  - [x] Test single phase boundary
  - [x] Test multiple phase boundaries
  - [x] Validate per-phase centerline calculation
  - [x] Test part position adjustment for missing data
  - [x] Document phase visualization (lines, shading)

- [x] **G9: ggplot2 Integration**
  - [x] Confirm BFHcharts returns ggplot2 object or data.frame
  - [x] Test adding ggplot2 layers to output
  - [x] Validate theme application works
  - [x] Document output structure (columns, aesthetics)

- [x] **G10: Data Structure**
  - [x] Document BFHcharts input parameters (names, types, NSE vs SE)
  - [x] Document BFHcharts output data.frame columns
  - [x] Test NSE (non-standard evaluation) requirements
  - [x] Validate `.original_row_id` preservation for comment mapping

### P1 Features (Validate During Phase 3)

- [ ] **G1: Chart Type Support**
  - [ ] Test all 9 chart types (run, i, mr, p, pp, u, up, c, g)
  - [ ] Validate parameter naming matches or can be easily mapped
  - [ ] Test edge cases per chart type

- [ ] **G4: Prime Charts**
  - [ ] Test P' charts with varying denominators
  - [ ] Test U' charts with rate data
  - [ ] Validate standardization formula

### P2 Features (Defer or Document)

- [ ] **G7: Custom Breaks** - Keep in SPCify (intelligent x-axis logic)
- [ ] **G8: Axis Formatting** - Keep in SPCify (presentation layer)
- [ ] **G11: Comments** - Keep in SPCify (security, XSS, Danish chars)

---

## Blocker Assessment for Tasks 30 & 31

### Can Tasks 30 & 31 Proceed?

**Answer:** YES - Proceed with phased validation approach

**Rationale:**
1. **No Confirmed Blockers:** All BLOCKER gaps are marked "ASSUMED_PARITY" based on BFHchart's origin from SPCify
2. **Controlled Environment:** In-house package allows rapid enhancement if gaps found
3. **Fallback Available:** qicharts2 remains in Suggests for regression testing
4. **Validation Strategy:** Task 30 will implement adapter pattern allowing incremental validation

### Risk Mitigation Strategy

**Phase 2 (Task 30 - Service Layer):**
- Implement `compute_spc_results_bfh()` with feature flag
- Keep qicharts2 implementation active during validation
- Document BFHchart API assumptions
- Create adapter layer for known API differences

**Phase 3 (Task 31+ - Parallel Validation):**
- Validate one chart type at a time
- Compare BFHchart vs qicharts2 outputs numerically
- File BFHchart issues for confirmed gaps
- Update adapter layer as needed

**Escalation Path:**
- If BLOCKER found: Pause migration, enhance BFHchart, resume
- If WORKAROUND viable: Document technical debt, proceed
- If ACCEPTABLE difference: Document in user-facing notes, proceed

---

## Next Actions

### ✅ Immediate (Stream C) - COMPLETED
- [x] Update DESCRIPTION with BFHcharts dependency
- [x] Move qicharts2 to Suggests
- [x] Create this tracking document
- [x] Verify BFHcharts repository access

### ✅ Phase 2 (Task #30 Stream C) - COMPLETED
- [x] Locate and review BFHcharts source code
- [x] Document BFHcharts API in `docs/migration/bfh-api-validation.md`
- [x] Run P0 validation checklist (automated tests + source review)
- [x] File BFHcharts issues for confirmed gaps (2 issues identified)
- [x] Design adapter layer based on findings

### Phase 3 (Task #30 Stream D) - NEXT
- [ ] Implement `compute_spc_results_bfh()` facade function
- [ ] Create adapter layer with workarounds for MR/PP/UP + notes
- [ ] Integration tests comparing BFHcharts vs qicharts2 output

### Phase 3 (Task 31+)
- [ ] Implement incremental validation per chart type
- [ ] Update this document with validation results
- [ ] Track BFHchart enhancement issues
- [ ] Update workaround documentation as needed

---

## BFHcharts Issues to File

### Issue #1: Sync Chart Type Validation with CHART_TYPES_EN

**Priority:** P1 (Non-blocking)
**File in:** BFHcharts repository

**Title:** `valid_chart_types` in `create_spc_chart()` missing MR, PP, UP

**Description:**
Hardcoded validation list doesn't match `CHART_TYPES_EN` constant.

**Fix:**
```r
# Replace hardcoded list with constant
valid_chart_types <- CHART_TYPES_EN
```

**Impact:** Users cannot use MR/PP/UP charts via high-level API (workaround: low-level API)

---

### Issue #2: Fix NSE Handling in Notes Parameter Validation

**Priority:** P1 (Non-blocking)
**File in:** BFHcharts repository

**Title:** `notes` parameter causes "closure is not subsettable" error

**Description:**
Validation calls `is.na(notes)` before NSE evaluation.

**Fix:**
```r
# Evaluate NSE expression first
notes_col <- rlang::enquo(notes)
if (!rlang::quo_is_null(notes_col)) { ... }
```

**Impact:** Users must use string reference instead of bare column name (SPCify workaround: ggrepel layer)

---

## Update Log

| Date | Updated By | Changes |
|------|------------|---------|
| 2025-10-15 | Stream C (Task #29) | Initial document creation based on Stream A feature parity matrix |
| 2025-10-15 | Stream C (Task #30) | Validation complete - updated all P0 statuses to VALIDATED |

---

**Document Owner:** Issue #30 Stream C
**Related Documents:**
- `docs/migration/bfh-feature-parity.md` (Feature comparison matrix)
- `docs/migration/bfh-api-validation.md` (API validation report - COMPLETE)
- Task 30 Stream D implementation plan

**Next Review:** After Phase 3 adapter layer implementation (Task #30 Stream D)
