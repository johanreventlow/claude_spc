# Archived Documentation

This directory contains historical documentation from project iterations, phase completions, and technical decisions that are no longer active development but preserved for reference.

## Directory Structure

### `/sprint/` - Sprint Planning & Reports
Sprint 2-5 planning documents and status reports from the initial development phases. These document the progression of the SPC Shiny app development but are historical reference only.

**Key files:**
- `SPRINT2_STATUS.md` - Sprint 2 completion status
- `SPRINT3_PLAN.md` - Sprint 3 planning
- `SPRINT4_PLAN.md` - Sprint 4 planning (code review phase)
- `SPRINT5_PLAN.md` - Sprint 5 planning

### `/phase/` - Phase Completion Reports
Technical analysis and findings from major project phases (Phase 0, 1, 2, 3, etc.)

**Key files:**
- `PHASE0_1_FINDINGS.md` - Initial phase findings
- `PHASE2_DEVICE_MATRIX_RESULTS.md` - Device testing results

### `/migration/` - BFHcharts Migration Reference
Complete migration documentation for the transition from qicharts2 to BFHcharts. Preserved as historical record and reference for similar migrations.

**Structure:**
- `bfh-api-reference.md` - BFHcharts API mapping
- `bfh-feature-parity.md` - Feature comparison matrix
- `bfh-issues.md` - Known issues encountered
- `bfh-workarounds.md` - Temporary workarounds used
- `clinical-validation-checklist.md` - Validation procedures
- `screenshots/` - Visual test results

### `/dev/` - Developer Reference (Archived from dev/archive/)
Legacy development guides and experimental documentation.

**Key files:**
- `AUTODETECT_DROPDOWN_ISSUE_RAPPORT.md` - Issue analysis
- `AUTODETECT_OPTIMERINGSPLAN.md` - Optimization planning (Danish)
- `UNIFIED_ARCHITECTURE_GAP_ANALYSIS.md` - Architecture evaluation
- `UNIFIED_ARCHITECTURE_NEXT_PHASE_PLAN.md` - Phase planning
- `STATE_MANAGEMENT_PLAN.md` - State management strategy (see ADR-015 for current)

## When to Use This Archive

✅ **Reference when:**
- Researching historical context for a bug or feature
- Understanding decision rationale (cross-reference with ADRs)
- Looking for previous testing approaches
- Reviewing BFHcharts integration lessons learned

❌ **Don't use for:**
- Current development guidance (use `docs/` instead)
- Configuration references (use `docs/CONFIGURATION.md`)
- Architecture decisions (use `docs/adr/` and `CLAUDE.md`)
- Testing approaches (use `tests/README.md`)

## Related Current Documentation

**Architecture & Decisions:**
- See `docs/adr/ADR-015-bfhchart-migrering.md` for current migration status
- See `docs/CROSS_REPO_COORDINATION.md` for BFHcharts coordination
- See `CLAUDE.md` for current development principles

**Testing:**
- See `tests/README.md` for current test structure
- See `tests/archived/` for archived test files (phase-specific tests)

**Configuration:**
- See `docs/CONFIGURATION.md` for all active configuration

---

## Cleanup Status

**Moved to archive on 2025-10-18:**
- Sprint 2-5 planning documents
- Phase completion reports
- BFHcharts migration documentation
- Legacy dev guides from `/dev/archive/`
- Remediation plans
- Legacy troubleshooting guides

**Deleted on 2025-10-18 (outdated):**
- `dev/archive/` (content moved here)
- `dev/johans_notes_to_self.md`
- `dev/koncept.md`
- `dev/kodegennemgang.md`
- `tests/MANUAL_TEST_GUIDE 2.md` (duplicate)
- `tests/testthat/TEST_REFACTORING_PLAN.md`
- `todo/archived_completed/` (10+ completed task docs)
- `35-analysis.md` (orphan file)

**Test Files Archived:**
Moved to `tests/testthat/archived/`:
- Phase-specific tests (`test-fase*.R`)
- QIC integration tests (now using BFHcharts)
- Device/optimization tests from Phase 1-3

---

**Last updated:** 2025-10-18
**Status:** Cleanup complete, ready for future maintenance reference
