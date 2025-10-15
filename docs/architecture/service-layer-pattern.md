# Service Layer Facade Pattern for BFHchart Integration

**Document Version:** 1.0
**Date:** 2025-10-15
**Status:** Design Specification
**Related Task:** Issue #30 - Service Layer Facade Implementation

---

## Overview

This document defines the **Facade Pattern** architecture for integrating BFHchart into SPCify. The facade provides a stable service layer that isolates the application from BFHchart API changes, standardizes outputs, and coordinates existing validation and error handling utilities.

**Primary Goal:** Enable seamless BFHchart integration without disrupting SPCify's established architecture, while maintaining flexibility for future backend changes.

---

## Pattern Overview

### What is the Facade Pattern?

The **Facade Pattern** is a structural design pattern that provides a simplified, unified interface to a complex subsystem. In SPCify's context:

- **Complex Subsystem:** BFHchart API (external package with evolving interface)
- **Facade:** `compute_spc_results_bfh()` and supporting functions in `R/fct_spc_bfh_service.R`
- **Client:** SPCify's plot rendering, UI observers, and export functions

**Benefits:**
1. **Isolation:** SPCify depends on facade interface, not BFHchart directly
2. **Stability:** BFHchart API changes contained to facade layer
3. **Testability:** Mock facade for unit tests without BFHchart dependency
4. **Consistency:** Standardized output format matching qicharts2 structure
5. **Maintainability:** Clear separation of concerns

---

## Architecture Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                      SPCify Application                      │
│  ┌─────────────────────────────────────────────────────┐    │
│  │          UI Layer (Shiny Modules)                   │    │
│  │  • mod_data_upload.R                                │    │
│  │  • mod_plot_rendering.R                             │    │
│  │  • mod_plot_customization.R                         │    │
│  └────────────────┬────────────────────────────────────┘    │
│                   │                                          │
│                   │ Uses standardized output                │
│                   ▼                                          │
│  ┌─────────────────────────────────────────────────────┐    │
│  │      FACADE LAYER (Service Interface)               │    │
│  │  ┌───────────────────────────────────────────────┐  │    │
│  │  │  compute_spc_results_bfh()                    │  │    │
│  │  │  • Primary interface function                 │  │    │
│  │  │  • Coordinates validation → transform →       │  │    │
│  │  │    invoke → standardize workflow              │  │    │
│  │  └───────────────┬───────────────────────────────┘  │    │
│  │                  │                                   │    │
│  │       ┌──────────┼──────────┐                       │    │
│  │       ▼          ▼          ▼                       │    │
│  │  ┌─────────┐ ┌─────────┐ ┌─────────┐               │    │
│  │  │ map_to_ │ │ call_   │ │ trans-  │               │    │
│  │  │ bfh_    │ │ bfh_    │ │ form_   │               │    │
│  │  │ params()│ │ chart() │ │ bfh_    │               │    │
│  │  │         │ │         │ │ output()│               │    │
│  │  └────┬────┘ └────┬────┘ └────┬────┘               │    │
│  └───────┼───────────┼───────────┼─────────────────────┘    │
│          │           │           │                          │
│  ┌───────▼───────────▼───────────▼─────────────────────┐    │
│  │    Integration Points (Existing SPCify Utilities)   │    │
│  │  • parse_and_validate_spc_data()                    │    │
│  │  • filter_complete_spc_data()                       │    │
│  │  • safe_operation()                                 │    │
│  │  • log_info() / log_error()                         │    │
│  │  • sanitize_user_input()                            │    │
│  │  • add_spc_labels() / applyHospitalTheme()          │    │
│  └─────────────────────────────────────────────────────┘    │
└───────────────────────────┬─────────────────────────────────┘
                            │
                            │ Calls
                            ▼
                ┌─────────────────────────┐
                │    BFHchart Package     │
                │  • bfhchart::spc_chart()│
                │  • External dependency  │
                └─────────────────────────┘
```

---

## Facade Responsibilities

### 1. Input Transformation (SPCify → BFHchart)

**Function:** `map_to_bfh_params()`

**Transformations:**
- Column name mapping (x_var, y_var, n_var → BFHchart parameters)
- Chart type translation (qicharts2 codes → BFHchart equivalents)
- Scale normalization (percentage ↔ decimal)
- Freeze/part position adjustment for NA-removed rows
- Row ID injection (`.original_row_id`) for comment mapping
- NSE (non-standard evaluation) handling if required

**Example:**
```r
# SPCify conventions
data <- hospital_data
x_var <- "Dato"
y_var <- "Tæller"
n_var <- "Nævner"
chart_type <- "p"
target_value <- 75  # Percentage scale

# Transform to BFHchart
bfh_params <- map_to_bfh_params(
  data = data,
  x_var = x_var,
  y_var = y_var,
  chart_type = chart_type,
  n_var = n_var,
  target_value = target_value
)

# bfh_params structure:
# list(
#   data = data_with_row_id,
#   x = "Dato",  # or rlang::sym("Dato") if NSE
#   y = "Tæller",
#   n = "Nævner",
#   chart = "p",
#   target = 0.75,  # Normalized to decimal
#   return_data = TRUE
# )
```

---

### 2. Validation Orchestration

**Reuses Existing Validators:**
- `parse_and_validate_spc_data()` - Type checking, required columns
- `filter_complete_spc_data()` - NA removal, row tracking
- Custom validators for BFHchart-specific constraints

**No Duplication:** Facade calls existing validators, does NOT reimplement validation logic.

**Error Propagation:** Validation errors bubble up with structured logging.

**Example Workflow:**
```r
compute_spc_results_bfh <- function(data, x_var, y_var, chart_type, ...) {
  # STEP 1: Validate inputs (reuse existing)
  validated_data <- parse_and_validate_spc_data(
    data = data,
    x_var = x_var,
    y_var = y_var,
    chart_type = chart_type,
    n_var = n_var
  )

  # STEP 2: Filter complete cases (reuse existing)
  complete_data <- filter_complete_spc_data(
    data = validated_data,
    x_var = x_var,
    y_var = y_var,
    n_var = n_var
  )

  # STEP 3: Transform to BFHchart
  bfh_params <- map_to_bfh_params(...)

  # STEP 4: Invoke BFHchart
  bfh_result <- call_bfh_chart(bfh_params)

  # STEP 5: Transform output
  standardized <- transform_bfh_output(bfh_result, ...)

  return(standardized)
}
```

---

### 3. BFHchart Invocation

**Function:** `call_bfh_chart()`

**Responsibilities:**
- Safe invocation wrapped in `safe_operation()`
- Structured logging (component: `[BFH_SERVICE]`)
- Performance measurement (execution time)
- Error capture and diagnostics

**Error Handling Strategy:**
```r
call_bfh_chart <- function(bfh_params) {
  safe_operation(
    operation_name = "BFHchart API call",
    code = {
      start_time <- Sys.time()

      log_info(
        message = "Invoking BFHchart",
        .context = "BFH_SERVICE",
        details = list(
          chart_type = bfh_params$chart,
          n_rows = nrow(bfh_params$data)
        )
      )

      result <- do.call(BFHchart::spc_chart, bfh_params)

      elapsed <- difftime(Sys.time(), start_time, units = "secs")
      log_info(
        message = "BFHchart call completed",
        .context = "BFH_SERVICE",
        details = list(execution_time_secs = as.numeric(elapsed))
      )

      return(result)
    },
    fallback = NULL,
    show_user = TRUE,
    error_type = "processing"
  )
}
```

---

### 4. Output Transformation (BFHchart → SPCify)

**Function:** `transform_bfh_output()`

**Standardization Tasks:**
- Column name mapping (BFHchart → qicharts2 conventions)
- Apply multiply scaling to y-axis values
- Calculate combined Anhøj signal if not provided
- Ensure required columns: x, y, cl, ucl, lcl, part, signal
- Preserve `.original_row_id` for comment mapping
- Extract or construct ggplot object
- Build metadata list with diagnostics

**Output Structure (qicharts2-compatible):**
```r
list(
  plot = ggplot2_object,
  qic_data = tibble(
    x = ...,              # X-axis values
    y = ...,              # Y-axis values (scaled if multiply ≠ 1)
    cl = ...,             # Centerline per point
    ucl = ...,            # Upper control limit
    lcl = ...,            # Lower control limit
    part = ...,           # Phase indicator (integer)
    signal = ...,         # Combined Anhøj signal (logical)
    .original_row_id = ...# Row ID for comment mapping
  ),
  metadata = list(
    chart_type = "p",
    n_points = 50,
    n_phases = 2,
    freeze_applied = FALSE,
    signals_detected = 3,
    bfh_version = "0.1.0"
  )
)
```

---

### 5. Error Recovery

**Graceful Fallbacks:**
- Validation failure → Return NULL, log error, show user notification
- BFHchart API error → Return NULL, log stack trace, suggest troubleshooting
- Transformation error → Return partial results if possible, log warning

**Structured Logging:**
```r
log_error(
  message = "BFHchart computation failed",
  .context = "BFH_SERVICE",
  details = list(
    operation = "compute_spc_results_bfh",
    chart_type = chart_type,
    n_rows = nrow(data),
    error_message = e$message,
    error_class = class(e)[1]
  )
)
```

---

## Integration Points

### Existing Validators (Reused)

**`R/fct_spc_validation.R`:**
- `parse_and_validate_spc_data()` - Type checking, column validation
- `filter_complete_spc_data()` - NA removal with position tracking
- `validate_x_column_format()` - Date/numeric validation

**Integration Pattern:**
```r
# Call existing validators BEFORE facade
validated_data <- parse_and_validate_spc_data(...)
complete_data <- filter_complete_spc_data(validated_data, ...)

# Pass to facade
result <- compute_spc_results_bfh(
  data = complete_data,  # Pre-validated
  ...
)
```

**No Modifications:** Existing validators remain unchanged. Facade uses them as-is.

---

### Existing Utilities (Reused)

**`R/utils_error_handling.R`:**
- `safe_operation()` - Wrap BFHchart calls
- `validate_exists()` - Check required objects

**`R/utils_logging.R`:**
- `log_info()`, `log_error()`, `log_debug()` - Structured logging
- Component tag: `[BFH_SERVICE]` for all facade logs

**`R/utils_input_sanitization.R`:**
- `sanitize_user_input()` - XSS protection for comments

**Integration Pattern:**
```r
# Wrap ALL BFHchart operations
bfh_result <- safe_operation(
  "BFHchart computation",
  code = { do.call(BFHchart::spc_chart, params) },
  fallback = NULL,
  show_user = TRUE,
  error_type = "processing"
)

# Log with structured context
log_info(
  message = "SPC computation started",
  .context = "BFH_SERVICE",
  details = list(chart_type = chart_type, n_points = nrow(data))
)

# Sanitize comments
comment_text <- sanitize_user_input(
  input_value = raw_comment,
  max_length = SPC_COMMENT_CONFIG$max_length,
  allowed_chars = "A-Za-z0-9_æøåÆØÅ .,-:!?",
  html_escape = TRUE
)
```

---

### Output Compatibility

**Must Work With Existing SPCify Functions:**

**`R/fct_spc_plot_generation.R`:**
- `render_spc_plot()` - Plot rendering
- `apply_y_axis_formatting()` - Axis formatters

**`R/fct_plot_customization.R`:**
- `add_spc_labels()` - Label placement
- `applyHospitalTheme()` - Branding

**`R/fct_export.R`:**
- `export_plot()` - PNG/PDF export
- `export_data()` - CSV export

**Compatibility Strategy:**
- Facade returns qicharts2-compatible structure
- Existing functions receive same input format as before
- NO modifications to downstream consumers

---

## Evolution Strategy

### How to Handle BFHchart API Changes

**Scenario 1: Parameter Rename**
```r
# BFHchart changes 'phases' → 'parts'
# ONLY modify facade:

map_to_bfh_params <- function(..., part_var = NULL, ...) {
  # Update mapping
  bfh_params$parts <- extract_part_positions(data, part_var)  # Changed
  # Previously: bfh_params$phases <- ...
}

# SPCify calls remain unchanged:
compute_spc_results_bfh(..., part_var = "Phase", ...)
```

**Scenario 2: Output Structure Change**
```r
# BFHchart renames 'ucl' → 'upper_cl'
# ONLY modify facade:

transform_bfh_output <- function(bfh_result, ...) {
  qic_data <- tibble(
    x = bfh_result$x,
    y = bfh_result$y,
    ucl = bfh_result$upper_cl,  # Map new name → qicharts2 convention
    lcl = bfh_result$lower_cl,
    ...
  )
}

# Downstream consumers see consistent 'ucl' column
```

**Scenario 3: Missing Feature**
```r
# BFHchart doesn't expose Anhøj signals
# Implement in facade:

transform_bfh_output <- function(bfh_result, ...) {
  qic_data <- bfh_result$data

  # Calculate missing feature
  qic_data$signal <- calculate_combined_anhoej_signal(qic_data)

  return(list(plot = ..., qic_data = qic_data, ...))
}

# SPCify always receives 'signal' column
```

**Key Principle:** Facade absorbs API changes. SPCify interface remains stable.

---

## Testing Strategy

### Unit Tests (TDD Approach)

**File:** `tests/testthat/test-spc-bfh-service.R`

**Test Categories:**

1. **Input Validation Tests**
   - Missing required parameters
   - Invalid chart types
   - Incompatible parameter combinations

2. **Parameter Transformation Tests**
   - Column name mapping correctness
   - Scale normalization (percentage → decimal)
   - Freeze/part position adjustment
   - Row ID injection

3. **BFHchart Invocation Tests**
   - Successful invocation (mock BFHchart)
   - Error handling (BFHchart throws error)
   - Performance benchmarking

4. **Output Transformation Tests**
   - Column name standardization
   - Multiply scaling application
   - Anhøj signal calculation
   - Metadata construction

5. **Integration Tests**
   - Full workflow (validation → transform → invoke → standardize)
   - Comment annotation integration
   - Compatibility with existing plot functions

**Mocking Strategy:**
```r
# Mock BFHchart for deterministic tests
test_that("compute_spc_results_bfh() handles run charts", {
  # Arrange
  mock_bfh_result <- list(
    data = tibble(x = 1:20, y = rnorm(20, 10, 2), cl = 10, ucl = 15, lcl = 5),
    plot = ggplot2::ggplot()
  )

  mockery::stub(
    call_bfh_chart,
    "BFHchart::spc_chart",
    mock_bfh_result
  )

  # Act
  result <- compute_spc_results_bfh(
    data = test_data,
    x_var = "month",
    y_var = "infections",
    chart_type = "run"
  )

  # Assert
  expect_s3_class(result$plot, "ggplot")
  expect_s3_class(result$qic_data, "tbl_df")
  expect_named(result$qic_data, c("x", "y", "cl", "ucl", "lcl", "signal", ".original_row_id"))
})
```

**Coverage Goal:** ≥95% for facade layer (Task 30 requirement)

---

### Integration Tests

**Test Against Real BFHchart:**
- Regression baseline comparison (qicharts2 vs BFHchart)
- Visual snapshot tests (`shinytest2::AppDriver`)
- Performance benchmarks (Task 33)

**Edge Cases:**
- Empty data → Graceful error
- Single point → Valid SPC calculations
- All NA values → NA removal with warnings
- Large datasets (>1000 rows) → Performance validation

---

## Comment/Notes Handling Strategy

### Critical Feature: `notes_column` Parameter

**Context:** SPCify's comment functionality is a **key feature** for clinical users. Comments provide contextual annotations for outliers, interventions, and data quality notes.

**Current Implementation (qicharts2):**
- Comments added **after** qic() call as ggrepel layer
- Stable row mapping via `.original_row_id` injection
- XSS sanitization with Danish character support (æøå)
- Intelligent truncation (40 char display, 100 char max)
- Collision avoidance with `ggrepel::geom_text_repel()`

**BFHchart Integration Options:**

**Option A: BFHchart Native Notes Support**
```r
# If BFHchart has 'notes' parameter
bfh_params$notes <- data[[notes_column]]

# Validate:
# - Row alignment stable (no reordering issues)
# - Danish characters render correctly
# - Styling customizable
```

**Option B: SPCify Layer Approach (RECOMMENDED)**
```r
# Continue existing pattern (PREFERRED)
# 1. Get BFHchart output
bfh_result <- call_bfh_chart(bfh_params)

# 2. Extract comment_data with .original_row_id join
comment_data <- extract_comment_data(
  original_data = data,
  notes_column = notes_column,
  qic_data = bfh_result$data
)

# 3. Add ggrepel layer
plot <- bfh_result$plot +
  ggrepel::geom_text_repel(
    data = comment_data,
    aes(x = x, y = y, label = comment),
    box.padding = 0.5,
    point.padding = 0.5,
    arrow = arrow(length = unit(0.015, "npc")),
    max.overlaps = Inf
  )

# 4. Apply sanitization (already done in extract_comment_data)
```

**Recommendation: Option B (SPCify Layer)**

**Rationale:**
1. **SPCify retains control** over comment presentation logic
2. **XSS sanitization** remains SPCify's responsibility (security best practice)
3. **Collision avoidance** handled by proven ggrepel logic
4. **Danish characters** (æøå) preserved with existing sanitization
5. **Flexibility** for future styling changes (hospital branding)

**Implementation in Facade:**
```r
compute_spc_results_bfh <- function(..., notes_column = NULL, ...) {
  # ... (validation, transform, invoke, standardize) ...

  # Add comments if specified
  if (!is.null(notes_column) && notes_column != "") {
    standardized$plot <- add_comment_annotations(
      plot = standardized$plot,
      qic_data = standardized$qic_data,
      original_data = data,
      notes_column = notes_column
    )
  }

  return(standardized)
}
```

**Helper Function:** `add_comment_annotations()`
- Encapsulates comment handling logic
- Reuses existing `extract_comment_data()` pattern
- Applies `sanitize_user_input()` for XSS protection
- Returns modified ggplot with ggrepel layer

---

## Performance Considerations

### Facade Overhead Target

**Goal:** Facade overhead < 50ms vs direct BFHchart call (Task 30 DoD)

**Overhead Sources:**
- Parameter transformation: ~5ms
- Output standardization: ~10ms
- Logging operations: ~5ms
- Comment annotation: ~10-20ms (if applied)

**Total Expected Overhead:** ~30-40ms (acceptable)

**Mitigation Strategies:**
1. **Minimize object copying** - Use references where safe
2. **Lazy comment processing** - Only if `notes_column` specified
3. **Efficient column renaming** - Use `dplyr::rename()` with static mapping
4. **Avoid redundant validation** - Trust upstream validators

**Benchmarking:**
```r
# Task 33: Performance & Observability
bench::mark(
  direct_bfhchart = BFHchart::spc_chart(...),
  facade_call = compute_spc_results_bfh(...),
  iterations = 100,
  check = FALSE
)
```

---

### Caching Strategy

**Responsibility:** Facade does NOT implement caching (already exists upstream).

**Existing Cache System:**
- Cache key: `digest::digest(list(data_dims, x_var, y_var, chart_type, ...))`
- Cache timeout: 300 seconds (5 minutes)
- Cache location: `app_state$cache$qic_results`

**Integration:**
```r
# Upstream (before facade)
cache_key <- generate_cache_key(data, x_var, y_var, chart_type, ...)
cached_result <- get_cached_result(cache_key)

if (!is.null(cached_result)) {
  return(cached_result$value)  # Skip facade
}

# Cache miss → Call facade
result <- compute_spc_results_bfh(...)

# Store in cache
store_cached_result(cache_key, result)
```

**Facade Assumption:** Data passed to facade is already cache-miss (not cached).

---

## Known Limitations and Trade-offs

### Limitations

1. **BFHchart Dependency:**
   - Facade cannot eliminate dependency on BFHchart package
   - BFHchart bugs propagate to SPCify (mitigated by error handling)

2. **API Assumptions:**
   - Facade design based on **expected** BFHchart API (not validated yet)
   - Task 29 (Stream A) documents assumptions, but actual API may differ
   - Adjustments may be needed in Task 32 (Stream D - Implementation)

3. **Performance Overhead:**
   - Transformation layers add ~30-40ms overhead
   - Acceptable for clinical use case (not real-time processing)

4. **Comment Handling:**
   - SPCify layer approach adds complexity
   - Requires stable `.original_row_id` mapping (tested in Task 29)

### Trade-offs

**Complexity vs. Flexibility:**
- **Trade-off:** Facade adds architectural complexity
- **Benefit:** Isolates BFHchart API changes, enables future backend swaps
- **Decision:** Complexity justified for long-term maintainability

**Performance vs. Validation:**
- **Trade-off:** Thorough validation adds overhead
- **Benefit:** Prevents crashes, provides informative errors
- **Decision:** Favor robustness over marginal performance gains

**Standardization vs. BFHchart Features:**
- **Trade-off:** Standardizing output may hide BFHchart-specific features
- **Benefit:** Consistent interface for SPCify consumers
- **Decision:** Expose BFHchart features via `...` parameter if needed

---

## Migration Path (qicharts2 → BFHchart)

### Phase-by-Phase Strategy

**Phase 1: Facade Skeleton (Current - Task 30)**
- Create facade functions with complete Roxygen documentation
- All functions return NULL (no implementation)
- Compile and generate documentation

**Phase 2: Implementation (Task 32 - Stream D)**
- Implement facade functions with BFHchart integration
- TDD approach: write tests first, implement to pass
- Validate against qicharts2 baseline

**Phase 3: Feature Parity Validation (Task 33)**
- Compare BFHchart output to qicharts2 baseline
- Visual regression tests (snapshot testing)
- Performance benchmarking

**Phase 4: UI Integration (Task 34)**
- Update plot rendering observers to use facade
- Feature flag: `use_bfhchart` for gradual rollout
- A/B testing with clinical users

**Phase 5: Transition Complete (Task 35)**
- Remove qicharts2 dependency (move to Suggests)
- Remove feature flag (BFHchart as default)
- Update documentation and user guide

---

### Backward Compatibility

**During Transition:**
```r
# Feature flag in app_runtime_config.R
USE_BFHCHART <- golem::get_golem_options("use_bfhchart", default = FALSE)

# Plot rendering logic
if (USE_BFHCHART) {
  result <- compute_spc_results_bfh(...)
} else {
  result <- compute_spc_results_qic(...)  # Existing qicharts2 code
}
```

**Post-Transition:**
```r
# Remove conditional, BFHchart as default
result <- compute_spc_results_bfh(...)
```

---

## Future Enhancements

### Potential Extensions

1. **Multi-Backend Support:**
   - Abstract facade further to support multiple SPC engines
   - `compute_spc_results()` dispatches to backend-specific facades
   - Enables experimentation with alternative SPC packages

2. **Async Processing:**
   - For large datasets, compute SPC in background job
   - Facade returns promise/future, UI polls for completion
   - Requires Shiny async support (`promises`, `future`)

3. **Custom SPC Rules:**
   - Allow users to define custom detection rules
   - Facade applies rules in post-processing
   - Independent of BFHchart limitations

4. **Enhanced Metadata:**
   - Return rich diagnostics (rule violations, trend analysis)
   - Enable advanced tooltips and drill-down features

---

## Summary

### Key Takeaways

1. **Facade Pattern:** Provides stable interface, isolates BFHchart API changes
2. **Reuse Existing Utilities:** No duplication of validation, error handling, or logging
3. **Comment Handling:** SPCify layer approach (Option B) recommended for control and security
4. **Output Compatibility:** qicharts2-compatible structure ensures seamless integration
5. **Evolution Strategy:** Facade absorbs API changes, SPCify interface remains stable
6. **Testing:** TDD approach with mocking for unit tests, real BFHchart for integration tests
7. **Performance:** Facade overhead target < 50ms (acceptable for clinical use case)

---

## References

### Related Documents

- **Feature Parity Matrix:** `docs/migration/bfh-feature-parity.md`
- **BFHchart API Reference:** `docs/migration/bfh-api-reference.md`
- **Task 29 (Stream A):** Feature analysis and API assumptions
- **Task 30 (Issue #30):** Service Layer Facade Implementation (this task)
- **Task 32 (Stream D):** Facade implementation with BFHchart integration

### External Resources

- **Facade Pattern:** [Refactoring Guru - Facade Pattern](https://refactoring.guru/design-patterns/facade)
- **R Package Design:** [R Packages (2e) - Wickham & Bryan](https://r-pkgs.org/)
- **SPCify Architecture:** `CLAUDE.md` - Sections 3.1 (Shiny Best Practices), 6.1 (File Organization)

---

**Document Status:** ✅ Complete - Ready for Task 30 Stream B handoff to Stream D
**Next Steps:** Stream D implements facade functions following this architecture
**Feedback:** Share with technical lead for architecture review before implementation

---

**Author:** Claude Code
**Version:** 1.0
**Last Updated:** 2025-10-15
