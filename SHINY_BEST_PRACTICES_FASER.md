# Shiny Best Practices Refactoring - 5 Faser

Dette dokument beskriver den systematiske refactoring af SPC-appen gennem 5 faser for at implementere Shiny best practices.

## Fase Oversigt

| Fase | Beskrivelse | Status | Commit Hash | Tests |
|------|-------------|--------|-------------|-------|
| **Fase 1** | Later::later elimination | ✅ Stabilt | `fdec5db` | 79/79 ✅ |
| **Fase 2** | Reactive chain forbedringer | ✅ Stabilt | `4f36c83` | 46/46 ✅ |
| **Fase 3** | Observer management + race condition guards | ✅ **Løser problem** | `ab48371` | 457/457 ✅ |
| **Fase 4** | Centraliseret state management | 🚧 **Planlagt** | - | - |
| **Fase 5** | Performance & cleanup | 🚧 Planlagt | - | - |

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
  # Høj prioritet - kritisk state management
  STATE_MANAGEMENT = 1000,

  # Medium prioritet - data processing
  AUTO_DETECT = 800,
  DATA_PROCESSING = 700,

  # Lav prioritet - UI updates og visuel feedback
  UI_SYNC = 500,
  PLOT_GENERATION = 400,
  STATUS_UPDATES = 300,

  # Meget lav prioritet - cleanup og logging
  CLEANUP = 100,
  LOGGING = 50
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

#### 5. Race Condition Guards - KRITISK LØSNING (fct_data_processing.R)
```r
# Column management observer med cooling-off periode
observe({
  # Skip hvis UI sync er pending for at undgå race condition
  if (!is.null(values$ui_sync_needed)) {
    cat("DEBUG: [COLUMN_MGMT] Skipping - UI sync pending, would override auto-detect results\n")
    return()
  }

  # Skip hvis UI sync netop er udført (1 sekund cooling-off periode)
  if (!is.null(values$last_ui_sync_time)) {
    time_since_sync <- as.numeric(difftime(Sys.time(), values$last_ui_sync_time, units = "secs"))
    if (time_since_sync < 1.0) {
      cat("DEBUG: [COLUMN_MGMT] Skipping - UI sync completed", round(time_since_sync, 2), "seconds ago, cooling off\n")
      return()
    }
  }

  # Fortsæt med normal column management...
})

# UI sync observer med timestamp protection
observeEvent(values$ui_sync_needed, {
  # Update selectize inputs...
  values$ui_sync_needed <- NULL
  values$last_ui_sync_time <- Sys.time()  # Prevent immediate override
  cat("DEBUG: [UI_SYNC] ✅ UI sync completed, set timestamp to prevent override\n")
}, priority = OBSERVER_PRIORITIES$UI_SYNC)
```

#### 6. Comprehensive Debug Infrastructure (~170 debug statements)
```r
# Auto-detect workflow
cat("DEBUG: [AUTO_DETECT] Starting column auto-detection\n")
cat("DEBUG: [AUTO_DETECT] Found date column:", date_col, "\n")
cat("DEBUG: [AUTO_DETECT] ✅ Auto-detection completed successfully\n")

# UI sync workflow
cat("DEBUG: [UI_SYNC] ✅ Event-driven auto-detect trigger fired!\n")
cat("DEBUG: [UI_SYNC] Updating selectize inputs with auto-detected values\n")
cat("DEBUG: [UI_SYNC] ✅ UI sync completed\n")

# Race condition protection
cat("DEBUG: [COLUMN_MGMT] Skipping - UI sync pending, would override auto-detect results\n")
cat("DEBUG: [COLUMN_MGMT] Skipping - auto-detect in progress\n")
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
- **457/457 tests passed** ✅ Alle eksisterende tests bevaret
- Observer priority execution order verified
- Race condition guards fungerer korrekt
- UI sync cooling-off periode forhindrer override
- Comprehensive debug coverage for troubleshooting

### Problem løst:
**SUCCESS**: Phase 3 løser det **oprindelige problem perfekt** - selectize input fields opdateres korrekt efter auto-detect. Kombinationen af:
1. **Observer priorities** (STATE_MANAGEMENT > AUTO_DETECT > UI_SYNC)
2. **Race condition guards** (pending UI sync check + cooling-off periode)
3. **Debug infrastructure** (170+ debug statements)

Sikrer at auto-detect resultater ikke overskrives af column management observer.

---

## Fase 4: Centraliseret State Management

**Mål:** Implementer centraliseret state management der eliminerer scattered reactiveValues og muliggør ren reactive architecture.

**Status:** Planlagt - vil bygge på Phase 3's stabile løsning

### Planlagte ændringer:

#### 1. Centraliseret App State Schema (global.R)
```r
create_app_state <- function() {
  list(
    # Data Management
    data = list(
      current_data = NULL,
      file_info = NULL,
      updating_table = FALSE
    ),

    # Column Management - Single source of truth
    columns = list(
      # Auto-detection state
      auto_detect = list(
        in_progress = FALSE,
        completed = FALSE,
        trigger = NULL,
        results = NULL
      ),

      # Current column mappings
      mappings = list(
        x_column = NULL,
        y_column = NULL,
        n_column = NULL,
        cl_column = NULL
      ),

      # UI sync state
      ui_sync = list(
        needed = NULL,
        last_sync_time = NULL
      )
    ),

    # Test Mode Management
    test_mode = list(
      auto_detect_ready = FALSE
    )
  )
}

#### 2. Refactor existing scattered state patterns
```r
# FØR: Scattered reactiveValues
values$auto_detect_in_progress <- TRUE
values$ui_sync_needed <- sync_data
values$last_ui_sync_time <- Sys.time()
values$current_data <- new_data

# EFTER: Centraliseret state access
state$columns$auto_detect$in_progress <- TRUE
state$columns$ui_sync$needed <- sync_data
state$columns$ui_sync$last_sync_time <- Sys.time()
state$data$current_data <- new_data
```

#### 3. Single source of truth for column configuration
```r
# Eliminerer scattered column mappings
state$columns$mappings$x_column <- "Dato"
state$columns$mappings$y_column <- "Antal"
state$columns$auto_detect$results <- auto_detect_results
```

#### 4. Backward compatibility ved graduel migration
```r
# Phase-wise migration strategy
# Step 1: Add centralized state alongside existing
# Step 2: Migrate observers one by one
# Step 3: Update reactive dependencies
# Step 4: Remove old scattered values
```

### Forventede fordele:
- **Eliminerer timing-based workarounds** fra Phase 3
- **Muliggør pure reactive patterns** med explicit dependencies
- **Forbedrer maintainability** med clear state ownership
- **Reducer race conditions** gennem controlled state access

### Test strategi:
- Gradual migration med test efter hver step
- Bevare Phase 3's funktionalitet under refactoring
- Verify at input field updates stadig fungerer

---

## Fase 5: Performance & Cleanup

**Mål:** Performance optimering og cleanup baseret på centraliseret state fra Phase 4.

**Status:** Planlagt - vil bygge på Phase 4's centraliserede state management

### Planlagte ændringer:

#### 1. Reactive chain optimization
```r
# Eliminated redundant reactive evaluations
# Use Phase 4's centralized state to reduce reactive dependencies
# Implement debouncing for expensive operations
# Cache computed values to avoid re-calculation
```

#### 2. Observer cleanup and consolidation
```r
# Remove duplicate observers identified in Phase 3 debug
# Consolidate overlapping functionality
# Optimize observer priority execution
# Clean up Phase 3's debug statements for production
```

#### 3. Memory management improvements
```r
# Clean up unused reactive values after state consolidation
# Implement proper cleanup on session end
# Optimize data storage patterns
# Remove temporary debug infrastructure
```

### Forventede fordele:
- **Reduced reactive chain complexity** efter Phase 4's state consolidation
- **Eliminated timing-based patterns** i favor af pure reactive patterns
- **Cleaner production code** ved fjernelse af debug infrastructure
- **Better performance** gennem optimized observer execution

### Test strategi:
- Performance benchmarking før og efter optimization
- Verify at Phase 3's funktionalitet bevares efter cleanup
- Memory usage monitoring under state consolidation

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

**OPDATERING 2025-09-16**: Efter omfattende Phase 3 debug og refactoring blev det oprindelige problem med selectize input field updates efter auto-detect **succesfuldt løst**.

**Stable commits**:
- **Phase 1**: `fdec5db` (later::later elimination) ✅
- **Phase 2**: `4f36c83` (reactive chain forbedringer) ✅
- **Phase 3**: `ab48371` (observer priorities + race condition guards) ✅ **AKTUEL LØSNING**

**Nuværende status**: Phase 3 er stabil og **løser det oprindelige problem** med selectize input field updates efter auto-detect. Observer priorities og race condition guards fungerer sammen for at sikre korrekt UI sync timing.

**Aktuel arkitektur beslutning**: User ønsker "mere ren reaktiv arkitektur" og har spurgt om det er bedst at:
1. Fortsætte med Phase 4-5 først (som kan gøre ren arkitektur lettere)
2. Eller refactore Phase 3 til pure reactive patterns først

**Anbefaling**: Fortsæt med Phase 4-5 da centraliseret state management vil eliminere timing-based workarounds og muliggøre ren reactive architecture.

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