# Archived Test Files

This directory contains test files from completed development phases that are no longer active but preserved for reference.

## Test Categories

### Phase-Specific Tests
Tests written for specific development phases (Fase 1-4, Phase 0-3). These tested migration strategies and optimization approaches that are now superseded by the current BFHcharts implementation.

**Files:**
- `test-fase1-refactoring.R` - Event-driven refactoring phase
- `test-fase2-reactive-chains.R` - Reactive chain consolidation
- `test-fase3-event-driven-state-machine.R` - State machine implementation
- `test-fase4-intelligent-heuristics.R` - Heuristic optimization
- `test-phase0-cache-isolation.R` - Cache isolation testing
- `test-phase1-qic-reproducibility.R` - QIC reproducibility verification
- `test-phase1-grob-measurement-stability.R` - Measurement stability
- `test-phase1-first-run-effect.R` - First-run behavior
- `test-phase2-device-size-matrix.R` - Device testing matrix
- `test-phase3-test-mode-optimization.R` - Test mode performance

### QIC Integration Tests (Pre-BFHcharts)
Tests for qicharts2 integration before migration to BFHcharts. Kept for reference on chart configuration approaches.

**Files:**
- `test-qicharts2-integration.R` - QIC integration approach
- `test-qic-preparation-sprint1.R` - Sprint 1 QIC preparation
- `test-qic-docall-nse-fix.R` - NSE (non-standard evaluation) fixes
- `test-qic-calculations.R` - QIC calculation testing
- `test-qic-cache-smart-invalidation.R` - Cache strategies
- `test-qic-cache-metrics.R` - Cache metrics monitoring

### Device & Optimization Tests
Tests for display optimization and device-specific behavior from earlier phases.

**Files:**
- `test-device-size-consistency.R` - Device size handling
- `test-device-batching.R` - Batch operations on devices

## Why These Are Archived

✅ **These tests are archived because:**
- They test now-superseded implementations (qicharts2 → BFHcharts)
- They test phase-specific migration strategies (no longer relevant)
- They focus on problems that have been solved or changed scope
- Keeping them active would be misleading for current developers

✅ **They're preserved because:**
- They document important lessons learned from migration
- They show how prior problems were approached
- They serve as reference for future similar migrations
- They contain useful testing patterns and edge cases

## Current Testing

For active test guidance, see:
- `../README.md` - Test suite overview
- `../../../tests/README.md` - Testing documentation
- `test-*.R` (main directory) - Active tests for current implementation

## Reference Usage

These files can be referenced to understand:
- How cache invalidation was previously handled
- Strategies for handling device-specific rendering
- QIC chart configuration approaches (historical)
- Event-driven reactive chain patterns from earlier implementation

---

**Last updated:** 2025-10-18
**Status:** All active tests remain in parent directory
