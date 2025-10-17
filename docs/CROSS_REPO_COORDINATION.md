# Cross-Repository Coordination: SPCify ↔ BFHcharts

Comprehensive guide for coordinating development, releases, and issue management between SPCify and BFHcharts repositories.

**Last Updated:** 2025-10-17
**Version:** 1.0.0

---

## Table of Contents

1. [Overview](#overview)
2. [Repository Relationships](#repository-relationships)
3. [Issue Escalation Workflow](#issue-escalation-workflow)
4. [Version Coordination](#version-coordination)
5. [Test Coordination](#test-coordination)
6. [Development Workflow](#development-workflow)
7. [Release Coordination](#release-coordination)
8. [Communication Protocols](#communication-protocols)
9. [Examples](#examples)
10. [Troubleshooting](#troubleshooting)

---

## Overview

### Purpose

This document establishes workflows and processes for coordinating development between SPCify and BFHcharts repositories. It defines clear boundaries for issue escalation, version management, test sharing, and release coordination.

### Goals

- **Clarity:** Unambiguous decision paths for issue ownership
- **Efficiency:** Minimize back-and-forth between repositories
- **Quality:** Ensure thorough testing across integration boundaries
- **Stability:** Prevent version conflicts and breaking changes
- **Communication:** Transparent cross-repository collaboration

### Principles

1. **BFHcharts owns visualization** - All chart rendering, statistical calculations, and core SPC functionality
2. **SPCify owns integration** - Data flow, UI, Shiny reactivity, and user experience
3. **Clear boundaries** - Well-defined facade layer separating concerns
4. **Explicit coordination** - Document cross-repository dependencies
5. **User focus** - Prioritize end-user experience over implementation convenience

---

## Repository Relationships

### Architecture

```
┌─────────────────────────────────────────────────────────┐
│                      SPCify App                         │
│  ┌──────────────────────────────────────────────────┐  │
│  │           Shiny UI Layer                         │  │
│  │  - File upload, data table, settings             │  │
│  │  - User interactions, state management           │  │
│  └──────────────────┬───────────────────────────────┘  │
│                     │                                   │
│  ┌──────────────────▼───────────────────────────────┐  │
│  │           Business Logic Layer                   │  │
│  │  - Data validation, preprocessing                │  │
│  │  - Parameter mapping (qicharts2 → BFHcharts)     │  │
│  │  - Error handling, logging                       │  │
│  └──────────────────┬───────────────────────────────┘  │
│                     │                                   │
│  ┌──────────────────▼───────────────────────────────┐  │
│  │           Facade Layer (Integration)             │  │
│  │  - BFHcharts API calls                           │  │
│  │  - Result transformation                         │  │
│  │  - Caching                                       │  │
│  └──────────────────┬───────────────────────────────┘  │
└────────────────────┬────────────────────────────────────┘
                     │ Package boundary
                     │ (DESCRIPTION: Imports: BFHcharts)
┌────────────────────▼────────────────────────────────────┐
│                  BFHcharts Package                      │
│  ┌──────────────────────────────────────────────────┐  │
│  │           SPC Chart Generation                   │  │
│  │  - Chart type implementations (I, P, C, U, etc.) │  │
│  │  - Statistical calculations                      │  │
│  │  - Control limit algorithms                      │  │
│  └──────────────────┬───────────────────────────────┘  │
│                     │                                   │
│  ┌──────────────────▼───────────────────────────────┐  │
│  │           Rendering Engine                       │  │
│  │  - ggplot2 integration                           │  │
│  │  - Theme application (BFHthemes)                 │  │
│  │  - Annotation, labels, target lines              │  │
│  └──────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────┘
```

### Ownership Boundaries

| Responsibility | Owner | Examples |
|----------------|-------|----------|
| **UI/UX** | SPCify | File upload, settings panel, modal dialogs |
| **Shiny Integration** | SPCify | Reactivity, observers, state management |
| **Data Preprocessing** | SPCify | CSV parsing, column detection, validation |
| **Parameter Mapping** | SPCify | qicharts2 → BFHcharts translation |
| **Error Handling (User-facing)** | SPCify | Danish error messages, user notifications |
| **Localization** | SPCify | Danish language support |
| **Chart Rendering** | BFHcharts | ggplot2 generation, geoms, scales |
| **Statistical Calculations** | BFHcharts | Control limits, centerlines, sigma calculations |
| **Chart Types** | BFHcharts | I, P, C, U, X̄, S, R, T charts |
| **Theme & Styling** | BFHcharts | Colors, fonts, hospital branding (via BFHthemes) |
| **Annotations** | BFHcharts | Target lines, freeze periods, comments |

---

## Issue Escalation Workflow

### Quick Reference

For rapid decision-making, use the decision tree:
**`.claude/ISSUE_ESCALATION_DECISION_TREE.md`**

### Escalation Process

#### Step 1: Identify Issue

When a bug or feature request arises:

1. **Create issue in SPCify repository first**
   - Even if you think it belongs in BFHcharts
   - SPCify context is valuable for prioritization
   - Easier to escalate than to de-escalate

2. **Apply initial label:** `needs-triage`

3. **Document user impact:**
   - Who is affected?
   - What is current workaround?
   - What is severity?

#### Step 2: Determine Ownership

Use decision tree to answer:

**Is this a facade layer issue?**
- Parameter mapping
- UI integration
- Data preprocessing
- SPCify-specific logic

→ **YES**: Label `facade-layer`, fix in SPCify

**Is this a BFHcharts core issue?**
- Chart rendering
- Statistical calculation
- Missing feature
- API limitation

→ **YES**: Proceed to escalation (Step 3)

**Does this require both?**
- API extension + integration
- Breaking change management
- Cross-cutting optimization

→ **YES**: Label `bfhchart-coordinated`, proceed to coordination (Step 4)

#### Step 3: Escalate to BFHcharts

1. **Update SPCify issue:**
   - Change label from `needs-triage` to `bfhchart-escalation`
   - Add context about escalation decision
   - Link to decision tree section that guided decision

2. **Create BFHcharts issue:**
   - Use template: `.github/ISSUE_TEMPLATE/bfhchart-feature-request.md`
   - Include minimal reproducible example (standalone R script)
   - Link to SPCify issue
   - Share test cases

3. **Cross-link issues:**
   ```markdown
   ## BFHcharts Escalation

   **BFHcharts issue:** https://github.com/user/BFHcharts/issues/XX
   **Status:** Awaiting BFHcharts implementation
   **Target BFHcharts version:** 0.3.0
   ```

4. **Add to SPCify backlog:**
   - Label: `bfhchart-blocked`
   - Milestone: Linked to target BFHcharts version
   - Monitor BFHcharts progress

#### Step 4: Coordinate Both Repositories

1. **Create coordination plan:**
   ```markdown
   ## Coordination Plan

   ### BFHcharts Changes
   - [ ] Add new parameter `show_target_line`
   - [ ] Implement target line rendering
   - [ ] Add tests for target line positioning

   ### SPCify Changes
   - [ ] Add UI control for target line
   - [ ] Map to BFHcharts parameter
   - [ ] Update facade layer caching
   - [ ] Add integration tests

   ### Release Sequence
   1. BFHcharts 0.3.0 (target: 2025-11-01)
   2. SPCify 1.5.0 (target: 2025-11-15)
   ```

2. **Assign owners:**
   - BFHcharts changes: BFHcharts maintainer
   - SPCify changes: SPCify maintainer
   - Integration testing: Both (shared responsibility)

3. **Schedule check-ins:**
   - Weekly sync during active development
   - Release coordination meeting before each release

### Issue Labels

#### SPCify Repository

| Label | Purpose | When to Use |
|-------|---------|-------------|
| `needs-triage` | Initial assessment needed | All new issues |
| `facade-layer` | Fix in SPCify facade | After triage, SPCify-owned |
| `bfhchart-escalation` | Requires BFHcharts changes | Escalated to BFHcharts |
| `bfhchart-blocked` | Waiting for BFHcharts release | After escalation, awaiting fix |
| `bfhchart-coordinated` | Both repos need changes | Coordinated development |

#### BFHcharts Repository (proposed)

| Label | Purpose | When to Use |
|-------|---------|-------------|
| `spcify-request` | Feature request from SPCify | All escalated issues |
| `spcify-coordinated` | Coordinated with SPCify | Coordinated development |
| `high-priority-integration` | Blocks SPCify release | Critical escalations |

---

## Version Coordination

### Semantic Versioning Policy

BFHcharts follows semantic versioning (semver):

- **Major (X.0.0):** Breaking API changes
- **Minor (0.X.0):** New features, backward compatible
- **Patch (0.0.X):** Bug fixes, backward compatible

SPCify DESCRIPTION must specify appropriate constraints.

### Dependency Strategies

#### Strategy 1: Minimum Version (Development Phase)

**Use during:** Active migration, frequent BFHcharts updates

```r
# DESCRIPTION
Imports:
    BFHcharts (>= 0.2.0)
```

**Pros:**
- Flexible, allows patch updates
- Easy to test with latest BFHcharts
- Minimal DESCRIPTION churn

**Cons:**
- Risk of breaking changes in minor updates
- Less predictable behavior across environments

**When to use:**
- Development and testing phase
- When BFHcharts is pre-1.0.0 (unstable API)
- When actively coordinating features

#### Strategy 2: Major Version Pinning (Stable Release)

**Use during:** Post-migration, stable production

```r
# DESCRIPTION
Imports:
    BFHcharts (>= 1.0.0, < 2.0.0)
```

**Pros:**
- Allows minor/patch updates
- Protects against breaking changes
- Follows semver conventions

**Cons:**
- Requires major version upgrade planning
- May delay access to new BFHcharts features

**When to use:**
- BFHcharts 1.0.0+ (stable API)
- Production releases
- Long-term support versions

#### Strategy 3: Exact Version Pinning (Critical Production)

**Use during:** Critical production, regulatory compliance

```r
# DESCRIPTION
Imports:
    BFHcharts (== 1.2.3)
```

**Pros:**
- Maximum stability
- Reproducible builds
- No surprise updates

**Cons:**
- Manual upgrade required for all updates
- May miss critical security patches
- High maintenance burden

**When to use:**
- Regulatory/compliance requirements
- Critical production environments
- Docker/containerized deployments

### Version Update Workflow

#### Minor/Patch Updates

1. **Monitor BFHcharts releases:**
   - Subscribe to BFHcharts repository notifications
   - Review CHANGELOG for each release
   - Assess impact on SPCify

2. **Evaluate update:**
   ```bash
   # In SPCify development environment
   remotes::install_github("user/BFHcharts@v0.2.1")

   # Run SPCify test suite
   devtools::test()

   # Manual testing
   # - Launch app
   # - Test critical paths
   # - Verify chart rendering
   ```

3. **Update if safe:**
   ```r
   # Update DESCRIPTION minimum version
   Imports:
       BFHcharts (>= 0.2.1)  # Was: >= 0.2.0
   ```

4. **Document in CHANGELOG:**
   ```markdown
   ## [Unreleased]

   ### Changed
   - Updated BFHcharts dependency to >= 0.2.1 for bug fixes
   ```

#### Major Version Updates

Major updates require coordinated planning:

1. **Pre-release planning:**
   - Review BFHcharts 2.0.0 migration guide
   - Identify breaking changes
   - Estimate facade layer changes
   - Plan testing strategy

2. **Create migration branch:**
   ```bash
   git checkout -b feat/bfhcharts-2.0-migration
   ```

3. **Update facade layer:**
   - Adapt to new API
   - Update parameter mappings
   - Refactor deprecated calls
   - Add compatibility shims if needed

4. **Comprehensive testing:**
   - All integration tests
   - Regression test suite
   - Manual UAT with stakeholders
   - Performance benchmarks

5. **Update DESCRIPTION:**
   ```r
   Imports:
       BFHcharts (>= 2.0.0, < 3.0.0)
   ```

6. **Document migration:**
   - Update CHANGELOG with breaking changes
   - Create migration guide if user-facing
   - Update documentation

---

## Test Coordination

### Test Case Sharing

When escalating issues to BFHcharts, share test cases to enable thorough validation.

#### Process

1. **Extract minimal test case from SPCify:**

   ```r
   # SPCify integration test (in tests/testthat/test-spc-chart-integration.R)
   test_that("Control limits calculated correctly for I-chart", {
     data <- data.frame(
       x = 1:30,
       y = c(45, 47, 46, 50, 48, 52, 49, 51, 47, 46,
             48, 50, 49, 51, 52, 48, 47, 50, 49, 51,
             53, 55, 54, 52, 56, 58, 57, 59, 61, 60)
     )

     result <- generate_spc_chart(
       data = data,
       x_column = "x",
       y_column = "y",
       chart_type = "i"
     )

     # Expected UCL = mean + 2.66 * mean moving range
     expect_equal(result$control_limits$ucl, 62.5, tolerance = 0.1)
   })
   ```

2. **Create standalone R script for BFHcharts:**

   Save to `tests/fixtures/bfhchart-shared-cases/i-chart-control-limits.R`:

   ```r
   # Standalone test case for BFHcharts issue #42
   # Shared from SPCify: https://github.com/user/SPCify/issues/123

   library(BFHcharts)

   # Test data
   data <- data.frame(
     x = 1:30,
     y = c(45, 47, 46, 50, 48, 52, 49, 51, 47, 46,
           48, 50, 49, 51, 52, 48, 47, 50, 49, 51,
           53, 55, 54, 52, 56, 58, 57, 59, 61, 60)
   )

   # Generate I-chart
   result <- bfhchart::spc_chart(
     data = data,
     x = x,
     y = y,
     chart = "i"
   )

   # Expected results
   # UCL should be approximately 62.5
   # CL should be approximately 51.5
   # LCL should be approximately 40.5

   print(result$limits)

   # Visual check
   print(result$plot)
   ```

3. **Include in BFHcharts issue:**

   ```markdown
   ## Test Case

   **Minimal reproducible example:**
   See attached: [i-chart-control-limits.R](link-to-gist)

   **Expected behavior:**
   - UCL ≈ 62.5
   - CL ≈ 51.5
   - LCL ≈ 40.5

   **Actual behavior (BFHcharts 0.1.5):**
   - UCL = 70.2 (incorrect)
   - CL = 51.5 (correct)
   - LCL = 32.8 (incorrect)
   ```

4. **Link test to issue in SPCify:**

   ```r
   test_that("Control limits calculated correctly for I-chart", {
     # BFHcharts issue: https://github.com/user/BFHcharts/issues/42
     # Expected fix in BFHcharts v0.2.0
     # Test case shared: tests/fixtures/bfhchart-shared-cases/i-chart-control-limits.R

     skip_if_bfhchart_version_below("0.2.0")

     # ... test code ...
   })
   ```

### Integration Test Ownership

**SPCify owns:**
- End-to-end integration tests
- Shiny interaction tests
- Data flow tests
- Facade layer tests

**BFHcharts owns:**
- Unit tests for chart functions
- Statistical calculation tests
- Rendering output tests
- API contract tests

**Shared responsibility:**
- Regression tests for escalated bugs
- Performance benchmarks
- Visual regression tests (if implemented)

### Regression Test Coordination

When BFHcharts fixes a bug escalated from SPCify:

1. **BFHcharts adds regression test:**
   ```r
   # In BFHcharts: tests/testthat/test-i-chart.R
   test_that("I-chart control limits calculation (issue #42)", {
     # Regression test for SPCify issue #123
     data <- data.frame(...)
     result <- spc_chart(data, chart = "i")
     expect_equal(result$limits$ucl, 62.5, tolerance = 0.1)
   })
   ```

2. **SPCify keeps integration test:**
   ```r
   # In SPCify: tests/testthat/test-spc-chart-integration.R
   test_that("Control limits calculated correctly for I-chart", {
     # Verifies BFHcharts issue #42 fix in integration context
     skip_if_bfhchart_version_below("0.2.0")

     # Full integration test with SPCify data flow
     # ...
   })
   ```

3. **Document test coordination:**
   ```r
   # Comment in SPCify test:
   # This integration test complements BFHcharts unit test
   # Changes to BFHcharts behavior should be coordinated
   # See: BFHcharts tests/testthat/test-i-chart.R:L45
   ```

### Helper: Version Check Function

Add to SPCify test helpers:

```r
# tests/testthat/helper-bfhcharts.R

#' Skip test if BFHcharts version is below threshold
#'
#' @param min_version Minimum required BFHcharts version (string)
skip_if_bfhchart_version_below <- function(min_version) {
  current_version <- as.character(packageVersion("BFHcharts"))

  if (utils::compareVersion(current_version, min_version) < 0) {
    testthat::skip(
      sprintf(
        "BFHcharts version %s required, current version is %s",
        min_version,
        current_version
      )
    )
  }
}

#' Check if specific BFHcharts feature is available
#'
#' @param feature Feature name (e.g., "target_lines")
has_bfhchart_feature <- function(feature) {
  features <- list(
    target_lines = "0.3.0",
    standardized_p_chart = "0.4.0",
    interactive_tooltips = "0.5.0"
  )

  if (!feature %in% names(features)) {
    stop("Unknown feature: ", feature)
  }

  required_version <- features[[feature]]
  current_version <- as.character(packageVersion("BFHcharts"))

  utils::compareVersion(current_version, required_version) >= 0
}
```

Usage in tests:

```r
test_that("Target line renders at specified value", {
  skip_if_bfhchart_version_below("0.3.0")
  # Or:
  skip_if(!has_bfhchart_feature("target_lines"), "Target lines not available")

  # ... test code ...
})
```

---

## Development Workflow

### Feature Development Spanning Both Repos

When a feature requires changes in both repositories:

#### Phase 1: Planning

1. **Create coordination issue in SPCify:**
   ```markdown
   # Feature: Add Target Line Support

   ## Overview
   Enable users to specify target lines on SPC charts.

   ## Coordination Required
   This feature requires changes in both repositories.

   ### BFHcharts Changes
   - Add `target_value` parameter to `spc_chart()`
   - Implement target line rendering
   - Add validation and error handling

   ### SPCify Changes
   - Add UI input for target value
   - Map to BFHcharts parameter
   - Update facade layer
   - Add integration tests

   ## Timeline
   - BFHcharts 0.3.0: 2025-11-01
   - SPCify 1.5.0: 2025-11-15
   ```

2. **Create corresponding BFHcharts issue**

3. **Define API contract:**
   ```r
   # Proposed BFHcharts API
   bfhchart::spc_chart(
     data = data,
     x = x,
     y = y,
     chart = "i",
     target_value = 50,           # NEW
     show_target_line = TRUE,     # NEW
     target_line_color = "blue"   # NEW (optional)
   )

   # Return structure (addition)
   result$target_line <- list(
     value = 50,
     shown = TRUE,
     color = "blue"
   )
   ```

#### Phase 2: Development

1. **BFHcharts implements first:**
   - Develop feature
   - Add tests
   - Update documentation
   - Create pre-release (e.g., 0.3.0-rc1)

2. **SPCify integrates against pre-release:**
   ```r
   # DESCRIPTION (temporary for testing)
   Remotes:
       user/BFHcharts@v0.3.0-rc1
   ```

3. **Iterative feedback:**
   - SPCify tests integration
   - Reports issues to BFHcharts
   - BFHcharts refines implementation
   - Repeat until stable

#### Phase 3: Release

1. **BFHcharts releases 0.3.0**

2. **SPCify updates dependency:**
   ```r
   # DESCRIPTION
   Imports:
       BFHcharts (>= 0.3.0)
   ```

3. **SPCify releases 1.5.0**

4. **Cross-link releases:**
   - BFHcharts release notes mention SPCify integration
   - SPCify release notes mention BFHcharts feature

### Hotfix Coordination

When critical bug requires immediate fix:

#### Option A: Fix in BFHcharts, SPCify Updates

**Scenario:** Bug in BFHcharts core logic

1. **BFHcharts hotfix:**
   ```bash
   # BFHcharts
   git checkout -b hotfix/control-limit-calculation
   # ... fix ...
   git commit -m "fix: correct I-chart control limit formula"
   # Tag: v0.2.1
   ```

2. **SPCify pulls update:**
   ```r
   # Update DESCRIPTION
   Imports:
       BFHcharts (>= 0.2.1)
   ```

3. **Test and release if needed:**
   - If SPCify users affected → Release SPCify patch
   - If transparent to users → Update in next regular release

#### Option B: Temporary Workaround in SPCify

**Scenario:** BFHcharts fix will take time, users need immediate relief

1. **Implement workaround in SPCify facade:**
   ```r
   # R/fct_spc_chart_generation.R

   generate_spc_chart <- function(...) {
     result <- bfhchart::spc_chart(...)

     # TEMPORARY WORKAROUND for BFHcharts issue #42
     # TODO: Remove when BFHcharts >= 0.2.1
     # Manually recalculate control limits until fix available
     if (packageVersion("BFHcharts") < "0.2.1") {
       result$limits$ucl <- calculate_correct_ucl(result$data)
       result$limits$lcl <- calculate_correct_lcl(result$data)
     }

     result
   }
   ```

2. **Document in code and issue:**
   ```markdown
   ## Temporary Workaround

   Implemented workaround in SPCify facade layer to correct control limits
   until BFHcharts 0.2.1 is released.

   **Workaround location:** `R/fct_spc_chart_generation.R:L45-L52`
   **Removal task:** #456
   **Target removal:** After BFHcharts 0.2.1 adoption
   ```

3. **Create removal task:**
   - Schedule for after BFHcharts fix released
   - Link to escalated issue
   - Add to technical debt backlog

---

## Release Coordination

### Pre-Release Checklist

Before releasing SPCify:

- [ ] **BFHcharts version identified**
  - Which version are we targeting?
  - Is it released or pre-release?

- [ ] **BFHcharts CHANGELOG reviewed**
  - Any breaking changes?
  - New features used in SPCify?
  - Bug fixes affecting SPCify?

- [ ] **Facade layer updated**
  - API changes adapted
  - Deprecated calls replaced
  - New features integrated (if applicable)

- [ ] **Integration tests passed**
  - Full test suite with target BFHcharts version
  - Manual testing of critical paths
  - Performance benchmarks acceptable

- [ ] **DESCRIPTION updated**
  - Version constraint appropriate
  - Remotes removed (if using pre-release)
  - Namespace imports correct

- [ ] **Documentation updated**
  - CHANGELOG mentions BFHcharts changes
  - User docs reflect new features (if applicable)
  - Developer docs updated (if API changed)

- [ ] **Coordination documented**
  - Cross-linked issues closed
  - Release notes cross-reference each other

### Release Coordination Process

#### Coordinated Release

When SPCify and BFHcharts releases are tightly coupled:

1. **Plan timeline:**
   ```
   Week 1: BFHcharts feature freeze
   Week 2: BFHcharts release candidate
   Week 3: SPCify integration testing
   Week 4: BFHcharts 0.3.0 release (Monday)
   Week 4: SPCify 1.5.0 release (Friday)
   ```

2. **Communication:**
   - Announce timeline in both repositories
   - Create release coordination issue
   - Schedule check-in meetings

3. **Testing:**
   - SPCify tests against BFHcharts RC
   - Report blockers immediately
   - Iterate until stable

4. **Release sequence:**
   - BFHcharts releases first (dependency)
   - SPCify releases shortly after (dependent)

5. **Post-release:**
   - Monitor for issues
   - Prepare hotfix process if needed
   - Retrospective on coordination

#### Independent Release

When SPCify releases independently of BFHcharts:

1. **Verify compatibility:**
   - Test with current BFHcharts version
   - Check for recent BFHcharts updates
   - Ensure DESCRIPTION constraints valid

2. **Document BFHcharts version:**
   ```markdown
   ## SPCify 1.4.2 - 2025-10-20

   ### Changed
   - Updated data validation logic

   ### Fixed
   - Fixed column detection for edge cases

   ### Dependencies
   - Tested with BFHcharts 0.2.5
   - Compatible with BFHcharts >= 0.2.0, < 1.0.0
   ```

3. **Release notes:**
   - Mention tested BFHcharts version
   - Note if specific BFHcharts features used
   - Link to BFHcharts releases if relevant

### Breaking Change Management

When BFHcharts introduces breaking changes:

#### BFHcharts Side

1. **Announce breaking change early:**
   - Create GitHub issue describing change
   - Tag SPCify maintainer
   - Provide migration timeline

2. **Provide migration guide:**
   ```markdown
   # BFHcharts 2.0.0 Migration Guide

   ## Breaking Changes

   ### Parameter Renames
   - `show.grid` → `show_grid`
   - `freeze.period` → `freeze_period`

   ### Deprecated Functions
   - `qic()` → Use `spc_chart()`

   ### API Changes
   - `spc_chart()` now returns structured list (was S3 class)
   ```

3. **Support transition:**
   - Deprecation warnings in pre-2.0 versions
   - Compatibility shims if feasible
   - Clear timeline for removal

#### SPCify Side

1. **Assess impact:**
   - Review breaking changes
   - Identify affected facade layer code
   - Estimate migration effort

2. **Plan migration:**
   - Create migration branch
   - Update facade layer
   - Adapt tests
   - Performance validation

3. **Coordinate timing:**
   - Align with SPCify release cycle
   - Communicate to SPCify users
   - Provide migration path if user-facing

4. **Update DESCRIPTION:**
   ```r
   # Before migration (SPCify 1.x)
   Imports:
       BFHcharts (>= 0.9.0, < 2.0.0)

   # After migration (SPCify 2.0)
   Imports:
       BFHcharts (>= 2.0.0, < 3.0.0)
   ```

---

## Communication Protocols

### Primary Channel: GitHub Issues

**All coordination happens in GitHub issues:**
- Transparent and auditable
- Preserves context
- Cross-linkable
- Searchable history

### Issue Linking Conventions

**In SPCify issue:**
```markdown
## BFHcharts Coordination

**BFHcharts issue:** https://github.com/user/BFHcharts/issues/XX
**Status:** In progress / Blocked / Resolved
**Target BFHcharts version:** 0.3.0
**SPCify integration planned:** 1.5.0
```

**In BFHcharts issue:**
```markdown
## SPCify Context

**SPCify issue:** https://github.com/user/SPCify/issues/YY
**Use case:** [Brief description]
**Priority:** High / Medium / Low
**User impact:** [Description]
```

### Mentioning and Notifications

**Tag maintainers when:**
- Escalating issue to BFHcharts
- Requesting prioritization
- Coordinating releases
- Reporting blocking issues
- Breaking change announcements

**Format:**
```markdown
@bfhcharts-maintainer - This issue blocks our Q4 release.
Can you provide timeline estimate for BFHcharts implementation?
```

### Response Time Expectations

**SPCify to BFHcharts escalations:**
- Initial acknowledgment: 1-2 business days
- Impact assessment: 1 week
- Timeline estimate: 2 weeks

**BFHcharts breaking change announcements:**
- SPCify impact assessment: 1 week
- Migration plan: 2 weeks
- Implementation: Coordinated with release cycles

**These are targets, not guarantees.** Adjust based on:
- Severity/priority
- Complexity
- Current workload
- Release schedules

### Regular Sync Meetings (Optional)

For active development periods:

**When to schedule:**
- Major coordinated features
- Breaking change migrations
- Quarterly planning

**Format:**
- 30-minute video call
- Agenda in shared doc
- Action items documented in issues

---

## Examples

### Example 1: New Chart Type (T-Chart)

**Scenario:** SPCify user requests T-chart support. This requires BFHcharts implementation.

#### Step 1: User Request

SPCify user creates issue:
```markdown
# Feature Request: Support for T-Chart

We need T-charts for time-between-events analysis in our
infection control monitoring.
```

#### Step 2: SPCify Triage

SPCify maintainer assesses:
- **Decision:** This requires BFHcharts implementation (new chart type)
- **Action:** Escalate to BFHcharts

#### Step 3: Create SPCify Issue

```markdown
# Support for T-Chart

## User Request
Request from infection control department for T-chart support
(time-between-events).

## Analysis
This requires BFHcharts to implement T-chart functionality.
SPCify facade layer will then integrate it.

## Escalation
**BFHcharts issue:** https://github.com/user/BFHcharts/issues/78
**Priority:** Medium
**Target versions:**
- BFHcharts 0.4.0 (Q1 2026)
- SPCify 1.6.0 (Q2 2026)

## SPCify Integration Tasks
- [ ] Add T-chart to chart type mappings
- [ ] Update UI to show T-chart option
- [ ] Add facade layer support
- [ ] Add integration tests
- [ ] Update documentation
```

Label: `bfhchart-escalation`, `enhancement`

#### Step 4: Create BFHcharts Issue

Using template `.github/ISSUE_TEMPLATE/bfhchart-feature-request.md`:

```markdown
# Feature Request: T-Chart Implementation

## SPCify Context

### Use Case in SPCify
SPCify users need T-charts for time-between-events analysis,
particularly for infection control monitoring.

**User story:**
As an infection control analyst, I need T-charts to monitor
time between infection events, so that I can identify improvements
in prevention protocols.

### Impact on SPCify Users
- **Severity:** Medium
- **Affected users:** Infection control departments
- **Current workaround:** None (feature gap)

## Feature Request for BFHcharts

### Problem Statement
No support for T-charts (time-between-events) in current
BFHcharts implementation.

### Desired Functionality
Implement T-chart following standard SPC conventions:
- Calculate time between events
- Show exponentially weighted moving average
- Display control limits based on exponential distribution

### Proposed API

```r
bfhchart::spc_chart(
  data = data,
  x = event_date,
  chart = "t",
  multiply = 1,  # Scale factor
  freeze = NULL,
  target = NULL
)
```

[... rest of template ...]

## Coordination

**SPCify issue:** https://github.com/user/SPCify/issues/234
**Target BFHcharts version:** 0.4.0
**SPCify version that will integrate:** 1.6.0
```

#### Step 5: BFHcharts Development

BFHcharts maintainer:
1. Acknowledges issue
2. Adds to 0.4.0 milestone
3. Implements T-chart
4. Releases 0.4.0

#### Step 6: SPCify Integration

SPCify maintainer:
1. Updates DESCRIPTION: `BFHcharts (>= 0.4.0)`
2. Adds facade layer support:
   ```r
   # R/fct_spc_chart_generation.R
   generate_spc_chart <- function(..., chart_type) {
     bfh_chart_type <- map_chart_type(chart_type)  # "t" → "t"

     result <- bfhchart::spc_chart(
       data = data,
       x = x,
       chart = bfh_chart_type,
       ...
     )

     result
   }
   ```
3. Updates UI to show T-chart option
4. Adds tests
5. Releases SPCify 1.6.0

#### Step 7: Close Issues

Both issues closed with cross-references:

**SPCify:**
```markdown
Resolved in SPCify 1.6.0, using BFHcharts 0.4.0.

See release notes: [link]
```

**BFHcharts:**
```markdown
Implemented in BFHcharts 0.4.0.

Integrated by SPCify 1.6.0: https://github.com/user/SPCify/issues/234
```

---

### Example 2: Rendering Bug (Target Line Missing)

**Scenario:** Target lines not rendering in BFHcharts output.

#### Step 1: Bug Identified

SPCify user reports chart issue:
```markdown
# Bug: Target Line Not Showing

When I specify a target line in settings, it doesn't appear on the chart.

**Steps to reproduce:**
1. Load data
2. Select I-chart
3. Enter target value = 50
4. Generate chart
5. Target line is missing

**Expected:** Blue target line at y=50
**Actual:** No target line visible
```

#### Step 2: SPCify Triage

Maintainer investigates:

```r
# Test in SPCify console
result <- generate_spc_chart(
  data = test_data,
  chart_type = "i",
  target = 50
)

# Check facade layer output
str(result)  # No target line in result

# Test BFHcharts directly
library(BFHcharts)
bfh_result <- bfhchart::spc_chart(
  data = test_data,
  x = x,
  y = y,
  chart = "i",
  target = 50
)

plot(bfh_result)  # Target line still missing!
```

**Conclusion:** Bug is in BFHcharts, not SPCify facade.

#### Step 3: Create SPCify Issue

```markdown
# Bug: Target Line Not Rendering (BFHcharts)

## Summary
Target lines specified via `target` parameter are not rendering
in BFHcharts output.

## Root Cause
Issue is in BFHcharts rendering logic, not SPCify facade layer.

**Evidence:**
- Direct BFHcharts call also shows bug
- Facade layer correctly passes `target` parameter
- Issue exists in BFHcharts 0.2.5

## Escalation
**BFHcharts issue:** https://github.com/user/BFHcharts/issues/92
**Severity:** High (core feature broken)
**Workaround:** None available

## User Impact
Users cannot display target lines, limiting chart utility for
goal-based monitoring.
```

Label: `bug`, `bfhchart-escalation`

#### Step 4: Escalate to BFHcharts

```markdown
# Bug: Target Line Not Rendering

## SPCify Context

### Use Case in SPCify
SPCify users specify target values for goal-based monitoring.
Target lines should appear on generated charts but are missing.

**Affected feature:** Target line rendering
**Severity:** High
**Current workaround:** None

### Impact on SPCify Users
- Cannot display target lines
- Limits chart utility
- Affects all chart types

## Bug Report for BFHcharts

### Problem Statement
Target lines specified via `target` parameter do not render
in chart output.

### Minimal Reproducible Example

```r
library(BFHcharts)

data <- data.frame(
  x = 1:30,
  y = rnorm(30, mean = 50, sd = 10)
)

result <- bfhchart::spc_chart(
  data = data,
  x = x,
  y = y,
  chart = "i",
  target = 50
)

plot(result)
# Expected: Blue line at y=50
# Actual: No target line visible

# Check result structure
str(result$target_line)  # NULL (should be list with value=50)
```

**Environment:**
- R version: 4.3.1
- BFHcharts version: 0.2.5
- ggplot2 version: 3.4.3

### Expected Behavior
- Target line rendered at y=50
- Blue color (default)
- Dashed line style

### Actual Behavior
- No target line in plot
- `result$target_line` is NULL

## Testing & Validation

### Test Case to Share

```r
test_that("Target line renders at specified value", {
  data <- data.frame(x = 1:30, y = rnorm(30, 50, 10))

  result <- bfhchart::spc_chart(
    data = data, x = x, y = y, chart = "i", target = 50
  )

  # Target line should be in result
  expect_false(is.null(result$target_line))
  expect_equal(result$target_line$value, 50)

  # Plot should contain target line geom
  plot_layers <- ggplot2::ggplot_build(result$plot)$data
  target_layer <- plot_layers[[which(...)]]  # Find target line layer
  expect_true(!is.null(target_layer))
})
```

### Acceptance Criteria
- [ ] Target line renders at specified value
- [ ] `result$target_line` contains metadata
- [ ] Visual appearance matches specification
- [ ] Works for all chart types

## Coordination

**SPCify issue:** https://github.com/user/SPCify/issues/245
**Expected in BFHcharts version:** 0.2.6 (hotfix)
**SPCify will adopt:** Immediately upon release
```

#### Step 5: BFHcharts Hotfix

BFHcharts maintainer:
1. Reproduces bug
2. Identifies root cause (missing geom_hline call)
3. Fixes bug
4. Adds regression test
5. Releases BFHcharts 0.2.6 (hotfix)

#### Step 6: SPCify Updates

SPCify maintainer:
1. Tests with BFHcharts 0.2.6
2. Updates DESCRIPTION: `BFHcharts (>= 0.2.6)`
3. Adds regression test in SPCify:
   ```r
   test_that("Target line renders correctly (BFHcharts #92)", {
     skip_if_bfhchart_version_below("0.2.6")

     result <- generate_spc_chart(..., target = 50)

     expect_false(is.null(result$target_line))
     expect_equal(result$target_line$value, 50)
   })
   ```
4. Releases SPCify 1.4.3 (patch)

---

### Example 3: Performance Optimization (Both Repos)

**Scenario:** Chart generation slow for large datasets (>5000 points).

#### Step 1: Performance Issue Identified

```markdown
# Performance Issue: Slow Chart Generation for Large Datasets

## Problem
Generating SPC charts for datasets with >5000 points takes 10-15 seconds,
making the app unresponsive.

## Analysis
Profiling shows time split between:
- SPCify data preprocessing: 2-3 seconds
- BFHcharts chart generation: 8-12 seconds

Both layers need optimization.
```

#### Step 2: Coordination Plan

```markdown
# Performance Optimization: Large Dataset Handling

## Coordinated Optimization

This performance issue requires optimization in BOTH repositories.

### SPCify Optimizations
- [ ] Implement data caching (avoid reprocessing)
- [ ] Add progress indicator for long operations
- [ ] Optimize column detection for large datasets
- [ ] Implement data sampling for preview (optional)

### BFHcharts Optimizations
- [ ] Optimize statistical calculations (vectorization)
- [ ] Reduce intermediate data copies
- [ ] Implement efficient control limit algorithms
- [ ] Add benchmarking suite

### Coordination
**BFHcharts issue:** https://github.com/user/BFHcharts/issues/105
**Target:**
- BFHcharts 0.3.1: <5 seconds for 10k points
- SPCify 1.5.1: <7 seconds total (including preprocessing)

## Benchmarking

**Current performance:**
- 1k points: ~2 seconds
- 5k points: ~12 seconds
- 10k points: ~25 seconds

**Target performance:**
- 1k points: <1 second
- 5k points: <3 seconds
- 10k points: <7 seconds
```

#### Step 3: Parallel Development

**BFHcharts:**
- Optimizes internal algorithms
- Reduces memory allocations
- Adds benchmarks

**SPCify:**
- Implements caching layer
- Optimizes preprocessing
- Adds progress indicators

#### Step 4: Integration and Validation

1. SPCify tests with optimized BFHcharts
2. Combined benchmarks run
3. Verify targets met

#### Step 5: Coordinated Release

- BFHcharts 0.3.1 released
- SPCify 1.5.1 released
- Joint announcement highlighting performance improvements

---

### Example 4: Breaking API Change

**Scenario:** BFHcharts 2.0 renames parameters from dot notation to snake_case.

#### Step 1: Breaking Change Announced

BFHcharts creates issue:

```markdown
# BFHcharts 2.0: Breaking API Changes

## Overview
BFHcharts 2.0 will standardize parameter names from dot notation
to snake_case for consistency with modern R practices.

## Breaking Changes

### Parameter Renames
- `show.grid` → `show_grid`
- `freeze.period` → `freeze_period`
- `part.labels` → `part_labels`
- `chart.title` → `chart_title`

### Deprecated Functions
- `qic()` → `spc_chart()`

## Timeline
- **Deprecation warnings:** BFHcharts 1.9.0 (2025-12-01)
- **Final 1.x release:** BFHcharts 1.10.0 (2026-02-01)
- **2.0.0 release:** 2026-03-01

## Migration Guide
[Link to detailed migration guide]

## Downstream Impact
**SPCify users:** This will require SPCify to update facade layer.
**Issue:** https://github.com/user/SPCify/issues/456

@spcify-maintainer - Please review migration guide and provide
timeline for SPCify 2.0 integration.
```

#### Step 2: SPCify Impact Assessment

SPCify maintainer creates issue:

```markdown
# BFHcharts 2.0 Migration

## Overview
BFHcharts 2.0 introduces breaking API changes requiring facade
layer updates.

## Impact Assessment

### Changes Required in SPCify

**Parameter mapping updates:**
```r
# Current (BFHcharts 1.x)
bfhchart::spc_chart(..., show.grid = TRUE)

# New (BFHcharts 2.0)
bfhchart::spc_chart(..., show_grid = TRUE)
```

**Files to update:**
- `R/fct_spc_chart_generation.R` - Main facade layer
- `R/utils_parameter_mapping.R` - Parameter translation
- `tests/testthat/test-spc-chart-integration.R` - Integration tests

**Estimated effort:** 8-16 hours

### Migration Strategy

#### Option A: Coordinated Major Release
- SPCify 2.0 released alongside BFHcharts 2.0
- Clean break, no backward compatibility
- Requires user communication

#### Option B: Gradual Migration
- SPCify 1.x supports BFHcharts 1.x
- SPCify 2.0 supports BFHcharts 2.0
- Longer support cycle

**Decision:** Option A (coordinated major release)

### Timeline

- **2025-12:** Review BFHcharts 1.9.0 with deprecation warnings
- **2026-01:** Create SPCify 2.0 migration branch
- **2026-02:** SPCify 2.0 beta testing
- **2026-03:** SPCify 2.0 release (after BFHcharts 2.0)

## Coordination
**BFHcharts issue:** https://github.com/user/BFHcharts/issues/120
```

#### Step 3: Migration Development

SPCify maintainer:

1. **Creates migration branch:**
   ```bash
   git checkout -b feat/bfhcharts-2.0-migration
   ```

2. **Updates facade layer:**
   ```r
   # R/fct_spc_chart_generation.R

   generate_spc_chart <- function(
     data,
     x_column,
     y_column,
     chart_type,
     show_grid = TRUE,      # Updated parameter name
     freeze_period = NULL,  # Updated parameter name
     ...
   ) {
     bfhchart::spc_chart(
       data = data,
       x = !!rlang::sym(x_column),
       y = !!rlang::sym(y_column),
       chart = map_chart_type(chart_type),
       show_grid = show_grid,       # Updated parameter
       freeze_period = freeze_period,  # Updated parameter
       ...
     )
   }
   ```

3. **Updates tests:**
   ```r
   # tests/testthat/test-spc-chart-integration.R

   test_that("Chart generates with grid", {
     result <- generate_spc_chart(
       data = test_data,
       x_column = "x",
       y_column = "y",
       chart_type = "i",
       show_grid = TRUE  # Updated parameter
     )

     expect_true(result$options$show_grid)  # Updated field
   })
   ```

4. **Updates DESCRIPTION:**
   ```r
   # DESCRIPTION
   Imports:
       BFHcharts (>= 2.0.0, < 3.0.0)
   ```

5. **Updates documentation:**
   - CHANGELOG
   - User migration guide
   - Developer documentation

#### Step 4: Testing and Release

1. **Beta testing:**
   - Internal testing
   - Selected user testing
   - Feedback incorporation

2. **Release SPCify 2.0:**
   - After BFHcharts 2.0 is stable
   - Communication to users
   - Migration support

3. **Post-release support:**
   - Monitor for issues
   - Support user migrations
   - Hotfixes if needed

---

## Troubleshooting

### Common Issues and Solutions

#### Issue: BFHcharts Update Breaks SPCify

**Symptoms:**
- Tests failing after BFHcharts update
- Runtime errors in chart generation
- Unexpected behavior in charts

**Diagnosis:**
1. Check BFHcharts CHANGELOG for breaking changes
2. Run SPCify tests with new BFHcharts version
3. Profile to identify affected code paths

**Solutions:**

**Option 1: Pin to previous version (immediate)**
```r
# DESCRIPTION
Imports:
    BFHcharts (>= 0.2.0, < 0.3.0)  # Pin to 0.2.x series
```

**Option 2: Update facade layer (permanent)**
- Adapt to new BFHcharts API
- Update parameter mappings
- Add tests for new behavior

**Option 3: Escalate to BFHcharts (if bug)**
- Create BFHcharts issue
- Provide minimal reproducible example
- Request hotfix or revert

#### Issue: Can't Determine Where Bug Belongs

**Symptoms:**
- Unclear if issue is in SPCify or BFHcharts
- Complex interaction between layers
- Hard to isolate

**Diagnosis Process:**

1. **Test with minimal BFHcharts example:**
   ```r
   # Bypass SPCify facade entirely
   library(BFHcharts)
   result <- bfhchart::spc_chart(
     data = data.frame(x = 1:30, y = rnorm(30)),
     x = x,
     y = y,
     chart = "i"
   )
   plot(result)
   ```

   - **Bug persists?** → BFHcharts issue
   - **Bug disappears?** → SPCify facade issue

2. **Check parameter transformation:**
   ```r
   # Log parameters before passing to BFHcharts
   log_debug(
     component = "[FACADE]",
     message = "Calling BFHcharts",
     details = list(
       parameters = list(
         chart = chart_type,
         show_grid = show_grid,
         ...
       )
     )
   )
   ```

3. **Compare outputs:**
   - Direct BFHcharts call output
   - SPCify facade layer output
   - Identify divergence point

**Solutions:**
- Create issue in SPCify (default)
- Label `needs-triage`
- Document investigation findings
- Tag maintainer for architecture decision

#### Issue: Version Conflict with Other Packages

**Symptoms:**
- Cannot install SPCify due to BFHcharts version
- Dependency resolution errors
- Version conflicts with ggplot2, scales, etc.

**Diagnosis:**
```r
# Check installed versions
packageVersion("BFHcharts")
packageVersion("ggplot2")

# Check SPCify requirements
remotes::package_deps("SPCify")$version
```

**Solutions:**

**Option 1: Update all packages**
```r
update.packages(ask = FALSE)
remotes::install_github("user/BFHcharts")
remotes::install_github("user/SPCify")
```

**Option 2: Use renv for isolation**
```r
renv::init()
renv::install("user/BFHcharts@v0.3.0")
renv::install("user/SPCify@v1.5.0")
```

**Option 3: Relax version constraints (SPCify developers)**
```r
# DESCRIPTION
Imports:
    BFHcharts (>= 0.2.0)  # Remove upper bound if compatible
```

#### Issue: Performance Regression After Update

**Symptoms:**
- Chart generation slower after BFHcharts update
- Increased memory usage
- UI lag

**Diagnosis:**
```r
# Benchmark current version
bench::mark(
  generate_spc_chart(data, ...),
  iterations = 10
)

# Downgrade BFHcharts to previous version
remotes::install_github("user/BFHcharts@v0.2.5")

# Benchmark again
bench::mark(
  generate_spc_chart(data, ...),
  iterations = 10
)

# Compare results
```

**Solutions:**

**Option 1: Escalate to BFHcharts**
- Create performance issue
- Share benchmarking results
- Provide profiling data

**Option 2: Optimize SPCify facade**
- Add caching
- Reduce data transformations
- Profile preprocessing

**Option 3: Pin to previous version temporarily**
- While waiting for BFHcharts optimization
- Communicate to users
- Plan upgrade path

---

## Appendix: Quick Reference

### Escalation Decision in 60 Seconds

1. **UI/Shiny issue?** → SPCify
2. **Data preprocessing?** → SPCify
3. **Chart rendering/statistical?** → BFHcharts
4. **Both needed?** → Coordinate
5. **Unsure?** → Create SPCify issue, label `needs-triage`

### Essential Links

- **Decision Tree:** `.claude/ISSUE_ESCALATION_DECISION_TREE.md`
- **Issue Template:** `.github/ISSUE_TEMPLATE/bfhchart-feature-request.md`
- **SPCify CLAUDE.md:** Section 5.3 - External Package Ownership
- **Test Helper:** `tests/testthat/helper-bfhcharts.R`

### Key Contacts

- **SPCify Maintainer:** [Name/GitHub handle]
- **BFHcharts Maintainer:** [Name/GitHub handle]

### Version Check Commands

```r
# Check installed versions
packageVersion("BFHcharts")

# Check SPCify's BFHcharts requirement
desc::desc_get_deps("SPCify") %>%
  filter(package == "BFHcharts")

# Test with specific version
remotes::install_github("user/BFHcharts@v0.3.0")
```

---

**Document History:**
- **v1.0.0** (2025-10-17): Initial version
- **Next review:** 2026-01-17 (quarterly)

**Feedback:**
Open an issue with label `documentation` for improvements to this guide.
