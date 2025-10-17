---
name: BFHcharts Feature Request
about: Request a feature or report a bug that requires changes in BFHcharts package
title: '[BFHcharts] '
labels: ['bfhchart-escalation', 'enhancement']
assignees: ''
---

# BFHcharts Feature Request / Bug Report

## SPCify Context

### Use Case in SPCify
<!-- Describe how this feature will be used in SPCify or how the bug affects SPCify users -->

**User story:**
As a [SPCify user/clinician/analyst], I need to [functionality], so that I can [benefit].

**Affected SPCify feature:**
<!-- Which part of SPCify is affected? (e.g., chart generation, data visualization, UI component) -->

### Current Workaround (if any)
<!-- Describe any temporary solution implemented in SPCify facade layer -->

```r
# Current workaround code (if applicable)

```

**Workaround limitations:**
- [ ] Performance impact
- [ ] Incomplete functionality
- [ ] Maintenance burden
- [ ] User experience degradation
- [ ] Other: _______________

### Impact on SPCify Users

**Severity:**
- [ ] Critical - Blocks core functionality
- [ ] High - Significant user impact
- [ ] Medium - Affects specific use cases
- [ ] Low - Minor improvement

**Affected users:**
<!-- How many users / which user groups are affected? -->

**Current user experience:**
<!-- What happens now? -->

**Desired user experience:**
<!-- What should happen? -->

---

## Feature Request for BFHcharts

### Problem Statement
<!-- Clear, concise description of the problem -->

### Desired Functionality
<!-- Detailed description of requested feature or bug fix -->

### Proposed API (if applicable)

**Desired BFHcharts function signature:**
```r
# Example of desired BFHcharts API
bfhchart::new_function(
  data = ...,
  parameter = ...,
  ...
)
```

**Expected behavior:**
```r
# Example usage showing expected output/behavior
result <- bfhchart::spc_chart(
  data = example_data,
  chart_type = "i",
  show_target_line = TRUE,  # <- New parameter
  target_value = 50
)

# Expected: Target line rendered at y = 50
```

### Minimal Reproducible Example

**Standalone R script demonstrating issue or desired behavior:**
```r
# Minimal reproducible example (standalone - no SPCify dependencies)
library(BFHcharts)

# Sample data
data <- data.frame(
  x = 1:30,
  y = rnorm(30, mean = 50, sd = 10)
)

# Current behavior (or desired behavior if feature request)
result <- bfhchart::spc_chart(data, chart_type = "i")

# Expected outcome:
# [Describe what should happen]

# Actual outcome (if bug):
# [Describe what actually happens]
```

**Environment:**
- R version: [e.g., 4.3.1]
- BFHcharts version: [e.g., 0.1.5]
- Operating system: [e.g., macOS 13.5, Windows 11, Ubuntu 22.04]
- Other relevant packages: [e.g., ggplot2 3.4.3, qicharts2 0.7.5]

---

## Alternatives Considered

### Option 1: [Alternative approach]
**Pros:**
-
**Cons:**
-
**Reason not chosen:**

### Option 2: [Another alternative]
**Pros:**
-
**Cons:**
-
**Reason not chosen:**

---

## Testing & Validation

### Test Cases to Share with BFHcharts

**Test case 1: [Description]**
```r
# Test case code
test_that("[description]", {
  # Arrange
  data <- ...

  # Act
  result <- bfhchart::spc_chart(...)

  # Assert
  expect_equal(result$..., expected_value)
})
```

**Test case 2: [Description]**
```r
# Additional test case if applicable
```

**Edge cases to consider:**
- [ ] Empty data
- [ ] Single data point
- [ ] Missing values (NA)
- [ ] Large datasets (>10,000 rows)
- [ ] Invalid parameter combinations
- [ ] Other: _______________

### Acceptance Criteria

**This feature/fix is complete when:**
- [ ] [Criterion 1: e.g., Target line renders correctly at specified value]
- [ ] [Criterion 2: e.g., Parameter validation works for edge cases]
- [ ] [Criterion 3: e.g., Documentation updated with examples]
- [ ] [Criterion 4: e.g., Backward compatibility maintained]

**Regression risk:**
- [ ] Low - Isolated change, no impact on existing features
- [ ] Medium - Could affect related functionality
- [ ] High - Core change affecting multiple chart types

---

## Coordination

### Cross-Repository Links

**SPCify issue:**
<!-- Link to related SPCify issue: https://github.com/user/SPCify/issues/XX -->

**BFHcharts issue:**
<!-- Link to BFHcharts issue once created: https://github.com/user/BFHcharts/issues/YY -->

### Version Planning

**Target BFHcharts version for this feature:**
<!-- e.g., 0.3.0 -->

**SPCify version that will integrate this feature:**
<!-- e.g., 1.5.0 -->

**Release coordination:**
- [ ] BFHcharts release planned
- [ ] SPCify facade integration planned
- [ ] DESCRIPTION dependency update planned
- [ ] Migration path documented (if breaking change)

### Communication

**SPCify maintainer notified:**
<!-- @mention SPCify maintainer -->

**BFHcharts maintainer notified:**
<!-- @mention BFHcharts maintainer if creating in SPCify repo -->

**Estimated timeline (if known):**
<!-- e.g., Target Q2 2025, or depends on BFHcharts roadmap -->

---

## Additional Context

### Screenshots / Visual Examples
<!-- Add screenshots, mockups, or example charts if applicable -->

### Related Issues
<!-- Links to related issues in either repository -->

### References
<!-- Links to documentation, papers, standards, or other relevant resources -->

---

## Checklist

Before submitting this issue:

- [ ] Verified this requires BFHcharts changes (not just SPCify facade)
- [ ] Tested with latest BFHcharts version
- [ ] Created minimal reproducible example
- [ ] Documented SPCify context and user impact
- [ ] Prepared test cases
- [ ] Cross-linked with SPCify issue
- [ ] Reviewed decision tree: `.claude/ISSUE_ESCALATION_DECISION_TREE.md`
- [ ] Consulted coordination guide: `docs/CROSS_REPO_COORDINATION.md`

---

**For SPCify developers:** See `.claude/ISSUE_ESCALATION_DECISION_TREE.md` for escalation decision guidance.
