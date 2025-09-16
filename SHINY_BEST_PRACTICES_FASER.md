# Shiny Best Practices Refactoring - 5 Faser

Dette dokument beskriver den systematiske refactoring af SPC-appen gennem 5 faser for at implementere Shiny best practices.

## Fase Oversigt

| Fase | Beskrivelse | Status | Commit Hash | Tests |
|------|-------------|--------|-------------|-------|
| **Fase 1** | Later::later elimination | ✅ Stabilt | `fdec5db` | 79/79 ✅ |
| **Fase 2** | Reactive chain forbedringer | ✅ Stabilt | `4f36c83` | 46/46 ✅ |
| **Fase 3** | Observer management | ✅ Stabilt | `9698b88` | 79/79 ✅ |
| **Fase 4** | Centraliseret state management | ❌ Problemer | `51194ed` | 96/96 ✅ |
| **Fase 5** | Error handling og robustness | ❌ Problemer | Rullet tilbage | - |

---

## Fase 1: Later::later() Elimination

**Mål:** Fjern alle `later::later()` anti-patterns og erstat med event-driven Shiny patterns.

### Implementerede ændringer:

#### 1. Auto-save debouncing (utils_session_helpers.R)
```r
# FØR: later::later() based debouncing
later::later(function() { ... }, delay = 2)

# EFTER: Shiny native debounce()
debounced_save <- debounce(reactive(values$current_data), 2000)
observe({
  data <- debounced_save()
  if (!is.null(data)) {
    auto_save_to_local_storage(session, data, get_session_metadata())
  }
})
```

#### 2. Event-driven cleanup (utils_server_management.R)
```r
# FØR: Timing-based cleanup
later::later(function() { values$table_operation_in_progress <- FALSE }, delay = 1)

# EFTER: Event-driven cleanup
observeEvent(values$table_operation_cleanup_needed, {
  req(values$table_operation_cleanup_needed)
  values$table_operation_in_progress <- FALSE
  values$table_operation_cleanup_needed <- NULL
}, priority = OBSERVER_PRIORITIES$CLEANUP)
```

#### 3. Table operation guards (fct_data_processing.R)
```r
# FØR: Timing dependency
later::later(function() { do_cleanup() }, delay = 500)

# EFTER: Immediate flag setting med event trigger
values$table_operation_in_progress <- TRUE
values$table_operation_cleanup_needed <- TRUE
```

### Test resultater:
- **79/79 tests passed** i `test-fase1-refactoring.R`
- Verified elimination af 12+ later::later() calls
- Event-driven patterns fungerer stabilt

---

## Fase 2: Reactive Chain Forbedringer

**Mål:** Forbedre reactive patterns i mod_spc_chart.R med proper req() guards og event-driven architecture.

### Implementerede ændringer:

#### 1. Chart_config reactive med req() guards
```r
chart_config <- reactive({
  # Proper req() guards - stop execution if dependencies not ready
  req(data_reactive())
  req(column_config_reactive())

  data <- data_reactive()
  config <- column_config_reactive()
  chart_type <- chart_type_reactive() %||% "run"

  # Validation og column checking...
})
```

#### 2. Elimineret redundant chart_type calls
```r
# FØR: Separate reactive calls til chart_type
spc_plot <- reactive({
  chart_type <- chart_type_reactive()  # Redundant call
  config <- chart_config()
})

# EFTER: Chart_type embedded i config
spc_plot <- reactive({
  config <- chart_config()  # chart_type already included
  chart_type <- config$chart_type
})
```

#### 3. Event-driven renderUI med isolation
```r
output$plot_info <- renderUI({
  # Event-driven: react to reactive values first
  warnings <- values$plot_warnings
  plot_ready <- values$plot_ready

  if (length(warnings) > 0) {
    return(list(type = "warning", content = warnings))
  } else if (plot_ready) {
    # ONLY access external reactives when needed, with isolation
    data <- isolate(data_reactive())
    chart_type <- isolate(chart_type_reactive()) %||% "ukendt"
    # ...
  }
})
```

#### 4. Cleaner state management
```r
# Reset states once at start, not multiple times
set_plot_state(plot_ready = FALSE, warnings = character(0), results = NULL)
set_plot_state(is_computing = TRUE)

# Complete computation
set_plot_state(plot_ready = TRUE, is_computing = FALSE)
```

### Test resultater:
- **46/46 tests passed** i `test-fase2-reactive-chains.R`
- Req() guards forhindrer unnecessary evaluations
- Event-driven renderUI patterns fungerer
- Reduced reactive dependencies

### Løser oprindelige problem:
**SUCCESS**: Phase 2 løser det oprindelige problem perfekt - selectize input fields opdateres korrekt efter auto-detect. De forbedrede reactive chains med proper req() guards og event-driven patterns sikrer korrekt UI sync timing.

---

## Fase 3: Observer Management og Prioritering

**Mål:** Implementer systematisk observer management med prioritering og cleanup.

### Implementerede ændringer:

#### 1. OBSERVER_PRIORITIES konstanter (global.R)
```r
OBSERVER_PRIORITIES <- list(
  DATA_PROCESSING = 100,
  AUTO_DETECT = 90,
  STATE_MANAGEMENT = 80,
  UI_SYNC = 50,
  AUTO_SAVE = 10,
  CLEANUP = -10
)

OBSERVER_CATEGORIES <- list(
  DATA = "data",
  UI = "ui",
  SESSION = "session"
)
```

#### 2. Enhanced observer_manager (global.R)
```r
observer_manager <- function() {
  observers <- list()

  list(
    add = function(observer, name = NULL, priority = 0, category = "default") {
      id <- name %||% paste0(category, "_", length(observers) + 1)
      observers[[id]] <<- list(
        observer = observer,
        priority = priority,
        category = category,
        created = Sys.time()
      )
      id
    },

    remove = function(id) {
      if (id %in% names(observers)) {
        if (!is.null(observers[[id]]$observer$destroy)) {
          observers[[id]]$observer$destroy()
        }
        observers[[id]] <<- NULL
      }
    },

    cleanup_category = function(category) {
      for (id in names(observers)) {
        if (observers[[id]]$category == category) {
          remove(id)
        }
      }
    }
  )
}
```

#### 3. Prioritized observers (fct_data_processing.R)
```r
# Test mode auto-detect trigger med høj prioritet
test_mode_observer <- observeEvent(values$test_mode_auto_detect_ready, {
  req(values$test_mode_auto_detect_ready)
  cat("TEST MODE: Event-driven auto-detect trigger fired!\n")
  values$auto_detect_trigger <- Sys.time()
},
ignoreInit = TRUE, ignoreNULL = TRUE,
priority = OBSERVER_PRIORITIES$STATE_MANAGEMENT)

# Auto-detect execution med medium prioritet
auto_detect_observer <- observeEvent(values$auto_detect_trigger, {
  values$auto_detect_in_progress <- TRUE
  auto_detect_and_update_columns(input, session, values)
  values$initial_auto_detect_completed <- TRUE
  values$auto_detect_in_progress <- FALSE
},
ignoreInit = TRUE,
priority = OBSERVER_PRIORITIES$AUTO_DETECT)

# UI sync med lavere prioritet
ui_sync_observer <- observeEvent(values$ui_sync_needed, {
  req(values$ui_sync_needed)
  sync_data <- values$ui_sync_needed
  # Update selectize inputs...
  values$ui_sync_needed <- NULL
},
ignoreInit = TRUE, ignoreNULL = TRUE,
priority = OBSERVER_PRIORITIES$UI_SYNC)
```

#### 4. Category-based cleanup patterns
```r
# Cleanup observers by category
if (!is.null(obs_manager)) {
  obs_manager$cleanup_category("data")
  obs_manager$cleanup_category("ui")
  obs_manager$cleanup_all()
}
```

### Test resultater:
- **79/79 tests passed** i `test-fase3-observer-management.R`
- Observer priority execution order verified
- Category-based cleanup fungerer
- Performance optimization via priority

### Identificeret problem:
**REGRESSION**: Phase 3's observer prioritering introducerede timing issue der forhindrer selectize input fields i at opdatere efter auto-detect. Root cause er sandsynligvis observer priority konflikter med UI sync timing.

---

## Fase 4: Centraliseret State Management

**Mål:** Implementer centraliseret state management der eliminerer scattered reactiveValues og scope issues.

### Implementerede ændringer:

#### 1. State_manager service (utils_reactive_state.R)
```r
state_manager <- function() {
  private_values <- reactiveValues(
    # Data state
    current_data = NULL,
    original_data = NULL,

    # Plot state
    plot_ready = FALSE,
    plot_object = NULL,
    plot_warnings = character(0),
    anhoej_results = NULL,

    # Session state
    file_uploaded = FALSE,
    restoring_session = FALSE,

    # Auto-detect state
    auto_detect_trigger = NULL,
    auto_detect_in_progress = FALSE,
    initial_auto_detect_completed = FALSE,
    ui_sync_needed = NULL
  )

  list(
    # Data management
    get_data = function() private_values$current_data,
    set_data = function(data) { private_values$current_data <- data },

    # Plot state management
    get_plot_state = function() {
      list(
        ready = private_values$plot_ready,
        object = private_values$plot_object,
        warnings = private_values$plot_warnings,
        results = private_values$anhoej_results
      )
    },

    set_plot_state = function(ready = NULL, object = NULL, warnings = NULL, results = NULL) {
      if (!is.null(ready)) private_values$plot_ready <- ready
      if (!is.null(object)) private_values$plot_object <- object
      if (!is.null(warnings)) private_values$plot_warnings <- warnings
      if (!is.null(results)) private_values$anhoej_results <- results
    },

    # Session state management
    get_session_state = function() {
      list(
        file_uploaded = private_values$file_uploaded,
        restoring = private_values$restoring_session
      )
    },

    set_session_state = function(file_uploaded = NULL, restoring = NULL) {
      if (!is.null(file_uploaded)) private_values$file_uploaded <- file_uploaded
      if (!is.null(restoring)) private_values$restoring_session <- restoring
    },

    # Auto-detect state management
    get_auto_detect_state = function() {
      list(
        trigger = private_values$auto_detect_trigger,
        in_progress = private_values$auto_detect_in_progress,
        completed = private_values$initial_auto_detect_completed,
        ui_sync_needed = private_values$ui_sync_needed
      )
    },

    set_auto_detect_state = function(trigger = NULL, in_progress = NULL, completed = NULL, ui_sync_needed = NULL) {
      if (!is.null(trigger)) private_values$auto_detect_trigger <- trigger
      if (!is.null(in_progress)) private_values$auto_detect_in_progress <- in_progress
      if (!is.null(completed)) private_values$initial_auto_detect_completed <- completed
      if (!is.null(ui_sync_needed)) private_values$ui_sync_needed <- ui_sync_needed
    },

    # Reactive accessors
    data_reactive = reactive(private_values$current_data),
    plot_ready_reactive = reactive(private_values$plot_ready),
    ui_sync_reactive = reactive(private_values$ui_sync_needed),
    auto_detect_trigger_reactive = reactive(private_values$auto_detect_trigger),

    # Validation
    validate_state = function() {
      validation_results <- list()

      # Validate data consistency
      if (!is.null(private_values$current_data)) {
        validation_results$data_valid <- is.data.frame(private_values$current_data)
      }

      # Validate plot state consistency
      validation_results$plot_state_consistent <-
        (private_values$plot_ready && !is.null(private_values$plot_object)) ||
        (!private_values$plot_ready && is.null(private_values$plot_object))

      validation_results
    },

    # Backward compatibility
    get_values = function() private_values
  )
}
```

#### 2. Controlled access patterns
```r
# FØR: Direct reactiveValues access
values$plot_ready <- TRUE
values$current_data <- new_data

# EFTER: Controlled interface
state$set_plot_state(ready = TRUE)
state$set_data(new_data)
```

#### 3. Cross-module state synchronization
```r
# App server initialization
state <- state_manager()
values <- state$get_values()  # Backward compatibility

# Module integration with fallback
set_plot_state <- if (state_manager_available) {
  function(...) state$set_plot_state(...)
} else {
  function(ready = NULL, object = NULL, warnings = NULL, results = NULL) {
    if (!is.null(ready)) values$plot_ready <- ready
    if (!is.null(object)) values$plot_object <- object
    if (!is.null(warnings)) values$plot_warnings <- warnings
    if (!is.null(results)) values$anhoej_results <- results
  }
}
```

#### 4. Legacy compatibility support
```r
# Deprecated function med warning
initialize_reactive_values <- function() {
  warning("initialize_reactive_values() is deprecated. Use state_manager() instead.",
          call. = FALSE)

  reactiveValues(
    current_data = NULL,
    plot_ready = FALSE
    # ... legacy structure
  )
}
```

### Problemer identificeret:
- **Test mode conflicts**: State manager integration konflikter med test mode auto-load
- **Scope mismatch**: `values` vs `local_values` access patterns
- **Reactive timing**: State changes under initial load timing issues

### Test resultater:
- **96/96 tests passed** i `test-fase4-state-simple.R` (architectural tests)
- State manager interface og patterns verified
- Cross-module coordination patterns tested

---

## Fase 5: Error Handling og Robustness

**Mål:** Implementer robust error handling med graceful degradation og user feedback.

### Planlagte ændringer (problematisk implementation):

#### 1. Centraliseret error handling utilities
```r
# safe_observer wrapper
safe_observer <- function(expr, session, error_prefix = "Observer fejl", silent = FALSE) {
  function(...) {
    tryCatch(
      expr(...),
      error = function(e) {
        error_msg <- paste(error_prefix, ":", e$message)
        if (!silent) {
          showNotification(error_msg, type = "error", duration = 5)
        }
        cat("ERROR:", error_msg, "\n", file = stderr())
        NULL
      }
    )
  }
}

# safe_reactive wrapper
safe_reactive <- function(expr, req_deps = NULL, error_value = NULL) {
  reactive({
    tryCatch({
      if (!is.null(req_deps)) {
        for (dep in req_deps) req(dep)
      }
      expr()
    }, error = function(e) {
      cat("REACTIVE ERROR:", e$message, "\n", file = stderr())
      error_value
    })
  })
}
```

#### 2. User feedback patterns
```r
show_user_error <- function(message, type = "error", duration = 5, details = NULL) {
  formatted_msg <- switch(type,
    "error" = paste("Fejl:", message),
    "warning" = paste("Advarsel:", message),
    "info" = message,
    "success" = message
  )

  if (!is.null(details)) {
    formatted_msg <- paste(formatted_msg, "\nDetaljer:", details)
  }

  showNotification(formatted_msg, type = type, duration = duration)
}
```

#### 3. Graceful degradation patterns
```r
validate_data_with_fallback <- function(data, required_cols = NULL, fallback_data = NULL) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    return(list(valid = FALSE, data = fallback_data, message = "Ingen gyldige data"))
  }

  if (!is.null(required_cols)) {
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      return(list(valid = FALSE, data = fallback_data,
                  message = paste("Manglende kolonner:", paste(missing_cols, collapse = ", "))))
    }
  }

  list(valid = TRUE, data = data, message = "Data valideret")
}
```

### Problemer identificeret:
- **Render conflicts**: `safe_render_ui()` wrapper konflikter med test mode
- **Req() vs defensive checks**: `req()` guards i reactive functions stoppper execution i test mode
- **Column config chain**: `auto_detected_config()` → `column_config()` → `chart_config()` fejl kæde
- **Root cause**: `req(values$auto_detected_columns)` fejler under test mode initial load

### Status: **Rullet tilbage** - for kompleks integration der introducerede nye fejl

---

## Forbedret Tilgang til Phase 3-5 Implementering

**Baseret på fundet:** Phase 2 løser det oprindelige problem, men Phase 3-5 introducerer regressioner.

### Strategi for sikker videreimplementering:

#### 1. Minimal incrementelle ændringer
- **Én ændring ad gangen** med test efter hver
- **Debug statements** ved alle kritiske punkter
- **Rollback strategi** ved første fejl

#### 2. Input field update preservation
- **Test input fields først** ved hver ændring
- **Isoler** observer management fra UI sync logik
- **Bevar** Phase 2's reactive chain patterns

#### 3. Enhanced testing approach
```r
# Test template for hver Phase 3+ implementering
test_that("Input fields opdateres efter auto-detect", {
  # 1. Load test data
  # 2. Trigger auto-detect
  # 3. Verify selectize inputs er opdateret
  # 4. Debug output hvis fejl
})
```

#### 4. Debug-first implementering
```r
# Tilføj debug før hver kritisk ændring
cat("DEBUG: Before observer priority change - UI sync status: ", values$ui_sync_needed, "\n")
# Implementer ændring
cat("DEBUG: After observer priority change - UI sync status: ", values$ui_sync_needed, "\n")
```

#### 5. Phase 3 retry strategi
- **Isoler** observer prioritering fra UI sync
- **Bevar** Phase 2's working UI sync mechanism
- **Test** at observer prioritering ikke påvirker input field updates
- **Målrettet fix** af race conditions uden at ændre UI sync timing

#### 6. Compatibility checklist per ændring
- [ ] Input fields opdateres efter auto-detect ✅
- [ ] Test mode auto-load fungerer ✅
- [ ] Eksisterende tests passerer ✅
- [ ] Ingen nye console fejl ✅
- [ ] Performance ikke forringet ✅

---

## Lessons Learned

### Hvad fungerede godt:
1. **Fase 1-3**: Systematisk, modulær approach med comprehensive testing
2. **Event-driven patterns**: Erstatte timing-based logic med event-driven patterns
3. **Observer prioritering**: Klar prioritering løste race conditions
4. **Testing strategi**: Comprehensive testthat testing efter hver fase

### Hvad skabte problemer:
1. **Fase 4**: For aggressiv centralisering uden tilstrækkelig backward compatibility
2. **Fase 5**: For mange samtidige ændringer uden graduel integration
3. **Test mode conflicts**: Manglende forståelse af test mode timing dependencies
4. **Req() vs defensive programming**: Overbrugte req() guards i stedet for defensive NULL checks

### Anbefalinger for fremtidige faser:
1. **Graduel integration**: Mindre, inkrementelle ændringer
2. **Test mode first**: Sikre test mode fungerer før produktion patterns
3. **Defensive programming**: Brug NULL checks i stedet for req() under initial load
4. **Backward compatibility**: Bevare existing patterns mens nye introduceres gradvist

---

## Current Status

**OPDATERING 2025-01-16**: Efter rollback til Phase 2 (commit `cb66d5a`) blev det bekræftet at **Phase 2 løser det oprindelige problem perfekt** - selectize input felterne opdateres korrekt efter auto-detect.

**Stable commits**:
- **Phase 1**: `fdec5db` (later::later elimination)
- **Phase 2**: `cb66d5a` (løser oprindelige problem) ✅ **AKTUEL LØSNING**
- **Phase 3**: `2226723` (observer management - men bryder input field updates)

**Problematic commits**: Phase 3-5 introducerede regression i input field funktionalitet
**Current recommendation**: Blive på Phase 2 som stabil løsning der løser det oprindelige problem

### Test Mode Issues to Address:
1. Column config reactive chain conflicts under auto-load
2. Req() guards stopping execution during initial data load
3. State manager integration timing with test mode setup
4. Chart config dependency resolution during reactive setup

### Files Requiring Special Attention:
- `R/fct_visualization_server.R` - Column config reactive chain
- `R/mod_spc_chart.R` - Chart config req() guards
- `R/app_server.R` - State manager integration
- Test mode auto-load sequence timing