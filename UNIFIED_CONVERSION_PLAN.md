# Unified Event Architecture & State Management - Komplet Konverteringsplan

## Status: DELVIST IMPLEMENTERET
**Opdateret:** 2025-09-18

## Fase 1: KRITISK STATE UNIFICATION ✅ DELVIST FÆRDIG

### ✅ **FULDFØRT:**
1. **Fix inconsistent state paths i app_state schema**
   - ✅ Tilføjet `auto_detected_columns` til global.R schema
   - ✅ Opdateret utils_event_system.R til at sætte begge paths

2. **Eliminér dual state patterns (values$ til app_state$)**
   - ✅ Fjernet values parameter fra alle setup-funktioner
   - ✅ Konverteret app_server.R til kun at bruge app_state
   - ✅ Opdateret alle function signatures
   - ✅ Minimeret initialize_reactive_values() til tom reactiveValues()

### 🔄 **I GANG:**
3. **Konverter direct reactive observers til event-driven**
   - ✅ Tilføjet `data_changed` event listener i utils_event_system.R
   - ✅ Oprettet `update_column_choices_unified()` funktion
   - 🔄 **AKTUELT:** Kommenterer gammel column update observer ud i fct_data_processing.R
   - ⏳ **NÆSTE:** Tilføj emit$data_changed() calls til alle steder hvor data ændres

---

## Fase 2: EVENT SYSTEM EXPANSION

### ⏳ **MANGLER:**
1. **Tilføj emit$data_changed() til alle data assignments**
   ```r
   # Steder der skal have emit$data_changed() tilføjet:
   - fct_file_operations.R: 3 steder (after app_state$data$current_data <-)
   - utils_server_management.R: 5 steder
   - app_server.R: 1 sted (test mode)
   - fct_data_processing.R: 2 steder
   ```

2. **Konverter reactiveVal triggers til unified events**
   - `navigation_trigger` → `emit$navigation_changed()`
   - `autodetect_trigger` → fjern helt (erstattet af event system)
   - Alle manual reactive triggers i utils_session_helpers.R

3. **Implementer systematic observer management**
   - Centraliseret priority management
   - Unified error boundaries
   - Systematic dependency injection

---

## Fase 3: ARCHITECTURE POLISH

### ⏳ **MANGLER:**
1. **Event-driven module integration**
   - mod_spc_chart.R skal bruge unified events
   - Fjern alle `app_state$navigation_trigger()` calls
   - Erstat med `emit$navigation_changed()` patterns

2. **Cleanup legacy patterns**
   - Fjern alle kommenterede observers i fct_data_processing.R
   - Fjern gamle `trigger_needed_watcher` patterns
   - Fjern `app_state$columns$auto_detect$trigger` patterns

3. **Update test system til unified patterns**
   - Test-filer skal bruge emit API
   - Mock event system for testing
   - Verify event flow tests

4. **Function naming consistency og documentation**
   - Opdater function dokumentation
   - Konsistent naming across all unified functions

---

## DETALJERET IMPLEMENTERING - NÆSTE STEPS

### **AKTUELT STEP (Fase 1.3 - Direct Observers):**

1. **Færdiggør kommentering af gammel observer i fct_data_processing.R**
   ```r
   # Linje ~172: Kommentér ud slutningen af column update observer
   # Kontekst: efter "cat("DEBUG: [AUTO_DETECT] ❌ Conditions not met for auto-detect\n")"
   ```

2. **Tilføj emit$data_changed() calls systematisk:**

   **A. fct_file_operations.R (3 steder):**
   ```r
   # Linje ~XX: Efter app_state$data$current_data <- data_frame
   emit$data_changed()
   ```

   **B. utils_server_management.R (5 steder):**
   ```r
   # Session restore, demo data load, etc.
   # Efter hver app_state$data$current_data assignment
   ```

   **C. app_server.R (1 sted):**
   ```r
   # Test mode auto-load
   # Efter app_state$data$current_data <- test_data
   emit$data_loaded()  # Note: data_loaded, ikke data_changed
   ```

### **NÆSTE STEP (Fase 2.1 - ReactiveVal Conversion):**

1. **Fjern navigation_trigger reactiveVal system**
   - utils_session_helpers.R: Erstat `navigation_trigger` med event calls
   - Alle `navigation_trigger()` calls → `emit$navigation_changed()`

2. **Fjern autodetect_trigger system**
   - Helt fjern fra setup_column_management return value
   - Erstat med emit$auto_detection_started() calls

### **KRITISKE DEPENDENCIES:**
- Alle emit$data_changed() skal tilføjes FØR gammel observer fjernes
- Test at column choices updates virker efter hver ændring
- Verificer at auto-detection stadig fungerer

---

## PROBLEMATISKE OMRÅDER

### **KOMPLEKSE OBSERVERS DER SKAL KONVERTERES:**
1. **fct_data_processing.R linje 179-200+:** `trigger_needed_watcher` system
2. **fct_data_processing.R linje 200+:** Bridge observer for auto_detect$trigger
3. **utils_session_helpers.R:** Manual navigation_trigger patterns

### **FILER MED MANGE REACTIVE DEPENDENCIES:**
- fct_data_processing.R: Mest komplekse observers
- utils_session_helpers.R: Navigation trigger management
- fct_visualization_server.R: EventReactive patterns

---

## TEST STRATEGI

### **Efter hver fase:**
```bash
R -e "source('global.R'); testthat::test_file('tests/testthat/test-app-basic.R')"
```

### **Manual test scenarie:**
1. Start app
2. Upload fil → Verificer column choices opdateres
3. Auto-detection → Verificer UI sync
4. Manual column valg → Verificer plots opdateres
5. Session reset → Verificer cleanup

---

## ROLLBACK PLAN

Hvis der opstår kritiske fejl:
1. **Git rollback til seneste working commit**
2. **Gendan values parameter til setup-funktioner hvis nødvendigt**
3. **Kommentér event listeners ud i utils_event_system.R**
4. **Genaktiver legacy observers i fct_data_processing.R**

---

**VIGTIG RÆKKEFØLGE:**
Implementer altid ny funktionalitet FØR du fjerner gammel!
Test efter hver mindre ændring, ikke efter hele faser.