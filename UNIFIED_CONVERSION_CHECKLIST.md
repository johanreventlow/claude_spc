# Unified Architecture Conversion Checklist
*Opdateret: Export-funktionalitet fjernet for at simplificere konvertering*

## ✅ EXPORT SYSTEM FJERNET
Export-funktionaliteten er blevet fjernet fra kodebasen og vil blive re-implementeret senere med korrekt unified architecture. Dette eliminerer de største blocker for konvertering.

## Fase 1: Core Architecture Fixes (🔴 - HØJESTE PRIORITET)

### A. Tilføj manglende emit calls til data strukturændringer

- [ ] **R/fct_data_processing.R:1125** - Column name changes
  ```r
  # EFTER:
  names(app_state$data$current_data) <- new_names
  # TILFØJ:
  emit$data_changed()  # eller emit$columns_changed()
  ```

- [ ] **R/fct_data_processing.R:1174-1180** - Column additions
  ```r
  # EFTER hver:
  app_state$data$current_data[[new_col_name]] <- ...
  # TILFØJ:
  emit$data_changed()
  ```

## Fase 2: Architecture Cleanup (🟡 - MEDIUM PRIORITET)

### A. Fjern mixed state management

- [ ] **R/utils_memory_management.R:328** - Fix `reset_app_to_clean_state`
  ```r
  # ÆNDRE FRA:
  reset_app_to_clean_state <- function(values, app_state = NULL, session = NULL) {
  # TIL:
  reset_app_to_clean_state <- function(app_state, session = NULL, emit = NULL) {
  ```

- [ ] **Find alle calls** til `reset_app_to_clean_state` og opdater

### B. Konverter direct reactive observations

- [ ] **R/fct_data_processing.R:51-132** - Stort observe block
  ```r
  # KONVERTER FRA:
  observe({
    # Direct data observation
    current_data_check <- app_state$data$current_data
    # ...
  })

  # TIL:
  observeEvent(app_state$events$data_changed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$DATA_PROCESSING, {
    # Event-driven logic
  })
  ```

### C. Setup funktioner mangler emit parameter

- [ ] **Check alle setup_* funktioner** for manglende emit parametre
- [ ] **Tilføj emit parameter** hvor det mangler
- [ ] **Opdater function calls** i app_server.R

## Fase 3: Hygiejne og Dokumentation (🟢 - LAV PRIORITET)

### A. Ryd op i forældede kommentarer

- [ ] **R/fct_data_processing.R** - Fjern/opdater legacy kommentarer:
  - Linje 484: `values$ui_sync_needed assignment skipped`
  - Linje 548: `Legacy values$ access removed`
  - Linje 761: `values$original_data bevares`
  - Linje 824: `Legacy values$current_data assignment removed`
  - Linje 979: `values$auto_detected_columns assignment removed`
  - Linje 1422: `emit$session_reset() events and app_state instead of values$`

- [ ] **R/utils_server_management.R** - Fjern/opdater legacy kommentarer:
  - Linje 560: `Legacy values$auto_detect_done assignment removed`
  - Linje 633: `Legacy values$auto_detect_done assignment removed`
  - Linje 635: `Legacy values$initial_auto_detect_completed assignment removed`

- [ ] **R/utils_memory_management.R** - Fjern/opdater legacy kommentarer:
  - Linje 344: `Legacy values$ data assignments removed`

### B. Test compatibility

- [ ] **Opdater test files** til nye function signatures
- [ ] **Verificer at alle tests** stadig virker efter changes
- [ ] **Tilføj tests** for event-driven patterns

## Validering af completion

### Criteria for Unified Architecture completion:

- [ ] **Ingen `values` parametre** i production functions (kun tests må have dem)
- [ ] **Alle data mutations** har tilsvarende emit events
- [ ] **Alle direct reactive observations** konverteret til event-driven
- [ ] **Alle setup funktioner** har emit parameter hvis de bruges
- [ ] **Export system** fungerer med app_state
- [ ] **Legacy kommentarer** ryddet op eller opdateret

### Test commands:

```bash
# Tjek for values usage i production code:
grep -r "values\$" R/ | grep -v "#"

# Tjek for direct reactive observations:
grep -r "observe({" R/

# Tjek for hardcoded priorities:
grep -r "priority.*[0-9]" R/

# Validér at app loader:
R -e "source('global.R')"

# Kør core tests:
R -e "source('global.R'); testthat::test_file('tests/testthat/test-fase1-refactoring.R')"
```

## Estimeret tidsforbrug:

- **Fase 1**: 1-2 timer (core architecture)
- **Fase 2**: 1-2 timer (reactive pattern cleanup)
- **Fase 3**: 1 time (dokumentation og tests)

**Total**: 3-5 timer for komplet unified architecture

## Notes:

- **Export system fjernet** - eliminerer kritiske blocker
- **Fokus på core event architecture** - data lifecycle og reactive patterns
- **Test kontinuerligt** under implementering
- **Commit efter hver fase** for at kunne rulle tilbage hvis nødvendigt