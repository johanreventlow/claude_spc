# Claude Code Instruktioner – SPC App

## 1) Projektoversigt

Dette er en **R Shiny** applikation til **Statistical Process Control (SPC)** med **qicharts2**. Appen anvendes i klinisk kvalitetsarbejde og skal forblive stabil, forståelig og på dansk.

**Udviklingsstatus:** Projektet følger industristandard mønstre for Shiny-udvikling med test-driven development, centraliseret state management og robust error handling.

---

## 2) Udviklingsprincipper (Fundamentale regler)

### 2.1 Test-First Development (TDD)

**OBLIGATORISK:** Al udvikling følger Test-Driven Development:

1. **Skriv tests først** - Før enhver kodeændring skrives tests for ønsket adfærd
2. **Kør tests kontinuerligt** - Tests skal køre efter hver ændring og altid bestå
3. **Refactor med test-sikkerhed** - Ingen kodeændring uden test-coverage
4. **Aldrig breaking changes** - Tests må ikke brydes uden eksplicit begrundelse

**Test-kommandoer:**
```r
# Kør alle tests
R -e "source('global.R'); testthat::test_dir('tests/testthat')"

# Kør specifik test-fil
R -e "source('global.R'); testthat::test_file('tests/testthat/test-fase1-refactoring.R')"

# Test-coverage verification
# Tests skal bestå før og efter hver ændring
```

### 2.2 Defensive Programming

* **Input validation** - Valider alle inputs ved entry points
* **Error handling** - Explicit fejlhåndtering med `tryCatch()` og `safe_operation()`
* **Scope guards** - Brug `exists()` checks for variabel-scope sikkerhed
* **Graceful degradation** - Fallback-mønstre når komponenter fejler
* **State consistency** - Dual-state sync patterns for migrationskompatibilitet

### 2.3 Observability & Debugging

**DEBUG-FIRST Approach:**

* **Detaljeret logging** - Strukturerede `cat()` statements med prefixes:
```r
cat("DEBUG: [COMPONENT] ===========================================\n")
cat("DEBUG: [COMPONENT] Status description\n")
cat("DEBUG: [COMPONENT] Variable name:", variable_value, "\n")
cat("DEBUG: [COMPONENT] ✅ Success message\n")
```

* **Kategoriserede debug-tags:**
  - `[APP_SERVER]` - Main server flow
  - `[FILE_UPLOAD]` - File operations
  - `[COLUMN_MGMT]` - Column management
  - `[PHASE4]` - Centralized state operations
  - `[AUTO_DETECT]` - Auto-detection logic
  - `[PLOT_DATA]` - Data processing for plots

* **Systematic error tracking** - Centralized logging med `log_error()` functionen
* **State inspection** - Debug reactive chains med value dumps

### 2.4 Modularity & Architecture

* **Single Responsibility** - Hver funktion har én klar opgave
* **Immutable data flow** - Undgå at mutere data in-place
* **Centralized state management** - Brug `app_state` schema i stedet for spredt `values$`
* **Event-driven patterns** - Erstat timing-baserede operationer med reactive events
* **Dependency injection** - Functions modtager dependencies som parameters

---

## 3) Tekniske Best Practices

### 3.1 Shiny Best Practices

**Unified Event Architecture (OBLIGATORISK for al ny udvikling):**
```r
# ✅ CORRECT: Brug unified reactive event-bus
emit$data_loaded()
emit$columns_detected()
emit$ui_sync_needed()

observeEvent(app_state$events$data_loaded, ignoreInit = TRUE, priority = 1000, {
  # Your logic here
})

# ❌ WRONG: Ad-hoc reactiveVal triggers
my_trigger <- reactiveVal(NULL)
observeEvent(old_system$trigger, { new_system$trigger(value) })
```

**Event Architecture Pattern:**
* **Data Change** → **Emit Event** → **Centralized Listeners** → **State Update** → **Cascade Events**
* **Events**: Add to `app_state$events` i `global.R`
* **Emit functions**: Add to `create_emit_api()` i `global.R`
* **Listeners**: Add to `setup_event_listeners()` i `utils_event_system.R`

**Unified State Management (OBLIGATORISK for al data):**
```r
# ✅ CORRECT: Brug centralized app_state
app_state$data$current_data <- new_data
app_state$columns$x_column <- detected_column
app_state$session$file_uploaded <- TRUE

# ❌ WRONG: Scattered reactive values
values$some_data <- data
local_reactive <- reactiveVal(value)
```

**State Management Patterns:**
* **Single source of truth** - Alt data i `app_state` structure
* **Reactive consistency** - Brug `reactiveValues()` for proper Shiny reactivity
* **Event-driven updates** - State changes trigger events, ikke direkte observere
* **Environment-based sharing** - By-reference sharing via `new.env()` for scope safety

**Reactive Programming Patterns:**
* **Event-based triggers** - Use event-bus instead of direct reactive dependencies
* **Priority-based execution** - `priority = OBSERVER_PRIORITIES$HIGH/MEDIUM/LOW`
* **Explicit dependencies** - Use `req()` guards for conditional execution
* **Dependency isolation** - Use `isolate()` for breaking reactive dependencies when needed
* **Error boundaries** - Wrap reactive expressions in error handling

### 3.2 R Code Quality

**Coding Standards:**
* **Danish comments** - Al funktionalitetsbeskrivelse på dansk
* **English function names** - Funktionsnavne og variable på engelsk
* **Consistent naming** - snake_case for funktioner, camelCase for UI komponenter
* **Type safety** - Explicit type checks med `is.numeric()`, `is.character()` osv.

**Performance Patterns:**
* **Lazy evaluation** - `reactive()` expressions der kun evalueres ved ændringer
* **Data caching** - Genberegn ikke data unødvendigt
* **Memory management** - Cleanup med `rm()` og garbage collection awareness

### 3.3 Error Handling Patterns

**Robust error handling:**
```r
# Centralized error wrapper
safe_operation <- function(operation_name, code, fallback = NULL, session = NULL, show_user = FALSE) {
  tryCatch({
    code
  }, error = function(e) {
    log_error(
      paste(operation_name, "fejlede:", e$message),
      level = "error",
      show_user = show_user,
      session = session
    )
    return(fallback)
  })
}

# Scope-safe variable access during transitions
variable_check <- if (exists("feature_flag") && feature_flag && exists("new_system")) {
  new_system$section$variable
} else {
  legacy_system$variable
}
```

---

## 4) Workflow & Integration

### 4.1 Development Lifecycle

**Obligatorisk arbejdsgang:**

1. **Problem definition** - Start med én linje problem statement
2. **Test design** - Skriv tests for ønsket adfærd FØRST
3. **Minimal implementation** - Implementer mindste mulige ændring
4. **Test verification** - Verificer at alle tests består
5. **Integration testing** - Test full application flow
6. **Commit preparation** - Dokumenter ændringer og rationale
7. **Code review** - Self-review via diff inspection

### 4.2 Testing Strategy

**Multi-Layer Testing:**

* **Unit tests** - Individuelle funktioner og components
* **Integration tests** - Reactive chains og data flow
* **Regression tests** - Sikre at tidligere fejl ikke genoptræder
* **Performance tests** - Memory usage og response times
* **User scenario tests** - End-to-end workflows

**Test Coverage Goals:**
* **100% critical path** - Core functionality skal være 100% testet
* **90%+ overall** - Generel test coverage
* **Edge case coverage** - Null values, empty data, error conditions
* **Cross-browser compatibility** - Test på different user agents

### 4.3 Version Control & Deployment

**Git Strategy:**
* **Atomic commits** - Hver commit repræsenterer én logisk ændring
* **Descriptive messages** - Følg conventional commit format på dansk
* **Test verification** - Commits inkluderer kun kode hvor tests består
* **No breaking changes** - Backward compatibility maintained

**Deployment Pipeline:**
* **Feature flags** - Test nye features sikkert med `TEST_MODE_*` flags
* **Staged rollout** - Test på separate ports før production
* **Monitoring** - Log-based monitoring af production issues
* **Rollback capability** - Altid mulighed for at revertrere til previous stable version

---

## 5) Configuration & Environment

### 5.1 Development Environment

**OBLIGATORISKE indstillinger under udvikling:**

* `TEST_MODE_AUTO_LOAD <- TRUE` - Auto-load test data
* `AUTO_RESTORE_ENABLED <- FALSE` - Disable session restore for clean testing
* **Debug logging enabled** - Verbose output for troubleshooting
* **Multiple port testing** - Test på ports 4040, 5050, 6060 etc.

### 5.2 Data Integrity

**Kritiske data-regler:**

* **CSV format preservation** - ALDRIG ændre encoding, CRLF, delimiter eller BOM
* **Windows compatibility** - Al CSV-håndtering skal virke på Windows
* **Unicode safety** - Danish characters (æ, ø, å) skal preserved
* **Backup før changes** - Git commits før enhver data-related ændring

---

## 6) Legacy Guidelines (Preserved from original)

### 6.1 Baseline Rules

* **ALDRIG** ændre globale konfigurationer uden eksplicit aftale
* Bevar **dansk interface** og **danske kommentarer**
* Udelad **kommentarer om claude i commit-beskeder**
* Reference commit `f05a97f` for stabil baseline version

### 6.2 Architecture Boundaries

* `/R/modules/` – Shiny-moduler (visualisering, status mv.)
* `/R/server/` – Server-logik, opdelt i filer
* `/R/ui/` – UI-komponenter
* `/R/data/` – Eksempeldata og testfiler
* `/tests/testthat/` – Test suites og fixtures

### 6.3 Constraints & Forbidden Changes

* Ingen automatiske commits uden eksplicit aftale
* Ingen stor refaktorering uden godkendelse
* Ingen ændringer af `brand.yml` eller hospitalskonfiguration
* Ingen nye dependencies (pakker) uden godkendelse
* Bevar eksisterende API'er medmindre opgaven kræver andet

---

## 7) Quality Assurance Framework

### 7.1 Pre-Commit Checklist

**OBLIGATORISK før hver commit:**

- [ ] **Tests kørt og bestået** - All tests green
- [ ] **Manual functionality test** - Core user flows verified
- [ ] **Debug logging reviewed** - Log statements provide adequate information
- [ ] **Error handling verified** - Edge cases handled gracefully
- [ ] **Performance impact assessed** - No significant regression
- [ ] **Documentation updated** - Code comments and README updated if needed
- [ ] **Data integrity confirmed** - No CSV or configuration file corruption

### 7.2 Code Review Criteria

**Self-review standards:**

* **Correctness** - Logic is sound and handles edge cases
* **Readability** - Code is self-documenting with appropriate comments
* **Maintainability** - Future developers can understand and modify
* **Performance** - No obvious inefficiencies or memory leaks
* **Security** - No hardcoded secrets or vulnerable patterns
* **Consistency** - Follows established patterns in codebase

### 7.3 Production Readiness

**Deployment criteria:**

* **Zero failing tests** - Complete test suite passes
* **Performance benchmarks met** - Response times within acceptable range
* **Error monitoring configured** - Proper logging and alerting
* **Rollback plan documented** - Clear procedure for reverting changes
* **User acceptance** - Key workflows tested by stakeholders

---

## 8) Troubleshooting & Problem Resolution

### 8.1 Debugging Methodology

**Systematic debugging approach:**

1. **Reproduce consistently** - Create minimal reproducible example
2. **Isolate component** - Identify specific module or function causing issue
3. **Check logs** - Review debug output and error messages
4. **Test assumptions** - Verify input data and state conditions
5. **Add instrumentation** - Insert additional logging if needed
6. **Test systematically** - Binary search through code to locate issue
7. **Document resolution** - Record solution for future reference

### 8.2 Common Issues & Solutions

**Reactive chain problems:**
* **Infinite loops** - Check for circular dependencies
* **Race conditions** - Implement proper observer priorities
* **State inconsistency** - Verify dual-state sync patterns

**Performance issues:**
* **Memory leaks** - Profile with `profvis` and check for retained objects
* **Slow reactives** - Add debouncing and caching
* **UI blocking** - Move heavy computation to background processes

**Data issues:**
* **CSV parsing failures** - Verify encoding and delimiter settings
* **Missing values** - Check `na.strings` and `colClasses` parameters
* **Type conversion errors** - Explicit type checking and conversion

---

## 9) Communication & Documentation

### 9.1 User Communication

* **Precise action items** - "Gør X i fil Y, linje Z"
* **Clear manual steps** - Mark with **[MANUELT TRIN]**
* **Factual reporting** - No filler, stick to diffs and checklists
* **Problem-solution format** - What was wrong, how it was fixed

### 9.2 Commit Message Format

**Danish conventional commits:**

```
type(scope): kort handle-orienteret beskrivelse

Længere beskrivelse af ændringen og dens rationale.
Inkluder kontekst for fremtidige udviklere.

- Bullet points for multiple changes
- Reference til issue numbers hvis relevant
- Breaking changes markeret tydeligt

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>
```

**Types:**
* `feat` - Ny funktionalitet
* `fix` - Bug fixes
* `refactor` - Code refactoring uden funktionalitetsændring
* `test` - Test tilføjelser eller ændringer
* `docs` - Documentation ændringer
* `chore` - Maintenance opgaver
* `perf` - Performance forbedringer

---

## 10) Advanced Patterns & Architecture

### 10.1 State Management Patterns

**Unified State Architecture Schema:**
```r
# Created via create_app_state() function
app_state <- new.env(parent = emptyenv())  # Environment for by-reference sharing

# Reactive Event Bus - Central event system for all triggers
app_state$events <- reactiveValues(
  data_loaded = 0L,
  auto_detection_started = 0L,
  auto_detection_completed = 0L,
  columns_detected = 0L,
  ui_sync_needed = 0L,
  ui_sync_completed = 0L,
  navigation_changed = 0L,
  session_reset = 0L,
  test_mode_ready = 0L
)

# Data Management - ReactiveValues for proper Shiny reactivity
app_state$data <- reactiveValues(
  current_data = NULL,
  original_data = NULL,
  file_info = NULL,
  updating_table = FALSE,
  table_operation_in_progress = FALSE,
  table_version = 0
)

# Column Management - ReactiveValues for consistency
app_state$columns <- reactiveValues(
  auto_detect_in_progress = FALSE,
  auto_detect_completed = FALSE,
  auto_detect_results = NULL,
  x_column = NULL,
  y_column = NULL,
  n_column = NULL,
  cl_column = NULL
)

# Session Management
app_state$session <- reactiveValues(
  auto_save_enabled = TRUE,
  restoring_session = FALSE,
  file_uploaded = FALSE,
  user_started_session = FALSE,
  last_save_time = NULL,
  file_name = NULL
)
```

**Event-Driven State Update Pattern:**
```r
# ✅ CORRECT: Event-driven state updates
handle_data_upload <- function(new_data) {
  # 1. Update state
  app_state$data$current_data <- new_data
  app_state$data$file_info <- attr(new_data, "file_info")

  # 2. Emit event to trigger downstream effects
  emit$data_loaded()  # This triggers auto-detection, UI sync, etc.
}

# ✅ CORRECT: Event listeners with proper priorities
observeEvent(app_state$events$data_loaded, ignoreInit = TRUE, priority = 1000, {
  if (!is.null(app_state$data$current_data)) {
    emit$auto_detection_started()
  }
})

observeEvent(app_state$events$auto_detection_completed, ignoreInit = TRUE, priority = 800, {
  if (!is.null(app_state$columns$auto_detect_results)) {
    emit$ui_sync_needed()
  }
})

# ❌ WRONG: Direct state observation (creates tight coupling)
observeEvent(app_state$data$current_data, {
  # Direct reactive dependency - avoid this pattern
})
```

### 10.2 Performance Optimization Patterns

**Lazy Loading:**
```r
# Create reactive that only updates when truly needed
expensive_computation <- debounce(reactive({
  req(input$data_source)
  # Heavy computation here
}), millis = 1000)
```

**Memory Management:**
```r
# Cleanup observers on session end
session$onSessionEnded(function() {
  # Destroy observers
  # Clear large objects
  # Release resources
})
```

### 10.3 Extension Points

**Adding New Features:**
1. **Start with tests** - Define expected behavior
2. **Implement incrementally** - Small, testable changes
3. **Follow existing patterns** - Maintain consistency
4. **Document thoroughly** - Update this file if needed
5. **Monitor impact** - Watch for performance regressions

---

## 11) Final Reminders

### Development Philosophy
* **Quality over speed** - Robust code is more valuable than fast delivery
* **Test-driven confidence** - Tests provide safety net for refactoring
* **Observability first** - Debug information is crucial for maintenance
* **User-focused design** - Clinical quality work demands reliability
* **Continuous improvement** - Always look for opportunities to improve code quality

### Project Goals
* **Stability** - System must be reliable for clinical quality work
* **Maintainability** - Code must be understandable and modifiable
* **Performance** - Responsive user experience
* **Danish language support** - Cultural and linguistic requirements
* **Best practice compliance** - Industry standard development patterns
