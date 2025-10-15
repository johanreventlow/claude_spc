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
**Confirmed Blockers:** 0 (5 pending validation)
**Workarounds Available:** 6 (can be handled in SPCify layer)
**Acceptable Differences:** 2 (presentation logic)

**Risk Assessment:**
- Gap Risk: Medium
- Blocker Risk: Low (in-house package allows incremental enhancement)
- Migration Readiness: Proceed with phased validation

---

## Issue Tracking Table

| ID | Feature Area | Gap Category | Priority | Status | Workaround | BFHchart Issue | Notes |
|----|--------------|--------------|----------|--------|------------|----------------|-------|
| G1 | Chart Type Support (9 types) | WORKAROUND_BFHchart | P1 | VALIDATION_REQUIRED | Use qicharts2 fallback during validation | TBD | Validate API supports run, i, mr, p, pp, u, up, c, g chart types |
| G2 | Anhøj Rules (runs + crossings) | WORKAROUND_BFHchart | P0 | VALIDATION_REQUIRED | Calculate in SPCify service layer if not exposed | TBD | Critical: Check if BFHchart exposes per-point rule results |
| G3 | Control Limit Calculation | BLOCKER (if missing) | P0 | ASSUMED_PARITY | None - must exist in BFHchart | TBD | Validate calculations match qicharts2 across all chart types |
| G4 | Prime Charts (P', U') | WORKAROUND_BFHchart | P1 | VALIDATION_REQUIRED | Implement standardization in SPCify | TBD | Test P'/U' charts with varying denominators |
| G5 | Freeze Period Handling | BLOCKER (if missing) | P0 | ASSUMED_PARITY | None - complex workaround | TBD | Validate freeze parameter for baseline periods |
| G6 | Part Aggregation (Phases) | BLOCKER (if missing) | P0 | VALIDATION_REQUIRED | None - core functionality | TBD | Test multi-phase boundaries and per-phase calculations |
| G7 | Custom X-Axis Breaks | ACCEPTABLE | P2 | WORKAROUND_SPCify | Keep intelligent break logic in SPCify | N/A | Presentation logic - not a BFHchart concern |
| G8 | Axis Formatting (Y-axis units) | ACCEPTABLE | P2 | WORKAROUND_SPCify | Keep formatting utilities in SPCify | N/A | Presentation logic - apply via ggplot2 layers |
| G9 | ggplot2 Integration | BLOCKER (if incompatible) | P0 | VALIDATION_REQUIRED | Rebuild plot from data if needed | TBD | Critical: Confirm BFHchart returns ggplot-compatible output |
| G10 | Data Structure (input/output) | BLOCKER (if incompatible) | P0 | VALIDATION_REQUIRED | Create adapter functions | TBD | Document BFHchart API and create mapping layer |
| G11 | Comment/Notes Parameter | WORKAROUND_SPCify | P0 | CRITICAL_VALIDATION | Continue using ggrepel layer approach | N/A | Preferred: Keep comment handling in SPCify for security (XSS) and styling control |

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

### P0 Features (Must Validate Before Task 30)

- [ ] **G2: Anhøj Rules**
  - [ ] Confirm BFHchart calculates runs signal (≥8 consecutive points)
  - [ ] Confirm BFHchart calculates crossings signal
  - [ ] Validate per-phase rule application
  - [ ] Test algorithm matches qicharts2 methodology

- [ ] **G3: Control Limit Calculation**
  - [ ] Compare UCL/LCL calculations across all 9 chart types
  - [ ] Validate per-phase recalculation logic
  - [ ] Test edge cases (n<10, all equal values, extreme outliers)
  - [ ] Document numeric tolerance (±0.001)

- [ ] **G5: Freeze Period**
  - [ ] Test freeze parameter alone (no phases)
  - [ ] Test freeze + phases combination
  - [ ] Validate "BASELINE" vs "NUV. NIVEAU" label logic
  - [ ] Document freeze position adjustment for NA removal

- [ ] **G6: Part Aggregation**
  - [ ] Test single phase boundary
  - [ ] Test multiple phase boundaries
  - [ ] Validate per-phase centerline calculation
  - [ ] Test part position adjustment for missing data
  - [ ] Document phase visualization (lines, shading)

- [ ] **G9: ggplot2 Integration**
  - [ ] Confirm BFHchart returns ggplot2 object or data.frame
  - [ ] Test adding ggplot2 layers to output
  - [ ] Validate theme application works
  - [ ] Document output structure (columns, aesthetics)

- [ ] **G10: Data Structure**
  - [ ] Document BFHchart input parameters (names, types, NSE vs SE)
  - [ ] Document BFHchart output data.frame columns
  - [ ] Test NSE (non-standard evaluation) requirements
  - [ ] Validate `.original_row_id` preservation for comment mapping

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

### Immediate (Stream C)
- [x] Update DESCRIPTION with BFHchart dependency
- [x] Move qicharts2 to Suggests
- [x] Create this tracking document
- [ ] Verify BFHchart repository access

### Phase 2 (Task 30)
- [ ] Locate and review BFHchart source code
- [ ] Document BFHchart API in `docs/migration/bfh-api-reference.md`
- [ ] Run P0 validation checklist
- [ ] File BFHchart issues for confirmed gaps
- [ ] Design adapter layer based on findings

### Phase 3 (Task 31+)
- [ ] Implement incremental validation per chart type
- [ ] Update this document with validation results
- [ ] Track BFHchart enhancement issues
- [ ] Update workaround documentation as needed

---

## Update Log

| Date | Updated By | Changes |
|------|------------|---------|
| 2025-10-15 | Stream C | Initial document creation based on Stream A feature parity matrix |

---

**Document Owner:** Issue #29 Stream C
**Related Documents:**
- `docs/migration/bfh-feature-parity.md` (Feature comparison matrix)
- `docs/migration/bfh-api-reference.md` (BFHchart API documentation - TBD)
- Task 30 implementation plan

**Next Review:** After Phase 2 validation (Task 30 completion)
