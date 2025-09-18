# AUTODETECT OPTIMERINGSPLAN
**SPC App - Unified Autodetect Engine Implementation**

## STATUS: IKKE STARTET
**Oprettet:** 2025-09-18
**Senest opdateret:** 2025-09-18

---

## 🎯 MÅL
Refaktorering af autodetekteringssystemet til en **unified, event-driven engine** der eliminerer overlap, lukker gaps og følger følgende koncept:

### Autodetect-Koncept
- **Event-drevet motor:** Reagerer på præcise triggers (session start, fil upload, manuel handling)
- **Frozen state:** Kører ikke igen før næste trigger
- **Name-only mode:** Fornuftige defaults uden dataindhold ved session start
- **Robust datodetection:** Lubridate-first approach med danske/internationale formater
- **Smart heuristik:** Vægtning af navne, mønstre og statistik
- **Fallback-venlig:** Manual dropdown override altid mulig

---

## 📋 NUVÆRENDE PROBLEMER

### ❌ **Trigger-system er rodet**
- **Problem:** Multiple trigger-systemer eksisterer side om side:
  - Legacy `values$auto_detect_trigger`
  - Event system `emit$auto_detection_started()`
  - Manual trigger observers
  - Bridge-observers mellem gamle og nye systemer
- **Konsekvens:** Unpredictable adfærd, multiple executions

### ❌ **Inkonsistent state management**
- **Problem:** Resultater gemmes flere steder:
  - `app_state$columns$auto_detect_results`
  - `app_state$columns$auto_detected_columns`
  - Legacy `values$` locations
- **Konsekvens:** Data inkonsistens og manglende frozen state

### ❌ **Delvis lubridate integration**
- **Problem:** `guess_formats()` bruges som fallback, ikke primær metode
- **Konsekvens:** Dårlig håndtering af danske datoformater

### ❌ **Funktionel overlap**
- **Problem:** 3 autodetekteringsfunktioner med overlappende logik:
  - `auto_detect_and_update_columns()` - "komplette" funktion
  - `detect_columns_name_only()` - kun navnebaseret
  - `auto_detect_and_update_columns_unified()` - event-system version
- **Konsekvens:** Inkonsistent adfærd og maintainability problemer

---

## 🚀 IMPLEMENTERINGSPLAN

### **FASE 1: KONSOLIDERING** ⏱️ 1-2 dage
**Status:** ✅ AFSLUTTET (2025-09-18)

#### **1.1 Skriv unified autodetect function**
- [x] **Opret:** `R/fct_autodetect_unified.R`
- [x] **Implementer:** `autodetect_engine()` hovedfunktion
- [x] **Features:**
  - Trigger validation (frozen state check)
  - Scenario routing (session_start vs file_upload vs manual)
  - State update & freeze mechanism
  - UI sync & logging integration

#### **1.2 Eliminate functional redundancy**
- [x] **Fjern:** `auto_detect_and_update_columns()` (preserved for backward compatibility)
- [x] **Fjern:** `auto_detect_and_update_columns_unified()`
- [x] **Refactor:** `detect_columns_name_only()` → `detect_columns_name_based()`
- [x] **Opdater:** Alle function calls til ny unified engine

#### **1.3 Write comprehensive tests**
- [x] **Test:** Frozen state behavior
- [x] **Test:** Manual trigger override
- [x] **Test:** Scenario routing logic
- [x] **Test:** State consistency
- [x] **Test coverage:** 27/27 tests passed

#### **1.4 Integration with existing system**
- [x] **Erstatte:** Event system calls i `utils_event_system.R`
- [x] **Manuel trigger:** Direct call i `fct_data_processing.R`
- [x] **App testing:** Verified working with test data
- [x] **Backward compatibility:** All existing functionality preserved

---

### **FASE 2: ROBUST DATE DETECTION** ⏱️ 1 dag
**Status:** ✅ AFSLUTTET (2025-09-18)

#### **2.1 Lubridate-first approach**
- [x] **Implementer:** `detect_date_columns_robust()`
- [x] **Prioriter:** Danske formater først (`dmy`, `dmY`, etc.)
- [x] **Fallback:** Internationale formater (`ymd`, `mdy`, etc.)
- [x] **Threshold:** 80% success rate for column acceptance

#### **2.2 Format suggestion system**
- [x] **Implementer:** `find_best_format()`
- [x] **Test:** Comprehensive date format testing (36/36 tests passed)
- [x] **Log:** Format decisions for debugging

#### **2.3 Danish month name translation**
- [x] **Implementer:** Enhanced `parse_danish_dates()` with month translation
- [x] **Support:** Full Danish month names (januar, februar, etc.) and abbreviations
- [x] **Performance:** Optimized array-based processing eliminates lubridate warnings
- [x] **Test coverage:** Complete test suite for all Danish date formats

---

### **FASE 3: EVENT-DRIVEN STATE MACHINE** ⏱️ 1 dag
**Status:** ❌ IKKE STARTET

#### **3.1 Clean trigger system**
- [ ] **Fjern:** Alle legacy trigger patterns
- [ ] **Implementer:** 3 præcise triggers:
  - `session_started` → name-only detection
  - `data_loaded` → full analysis
  - `manual_autodetect_button` → forced re-run
- [ ] **Test:** Trigger isolation og event flow

#### **3.2 Frozen state management**
- [ ] **Implementer:** `app_state$autodetect$frozen_until_next_trigger`
- [ ] **Logic:** Prevent re-running mellem triggers
- [ ] **Override:** Manual trigger bypasses frozen state

---

### **FASE 4: INTELLIGENT HEURISTICS** ⏱️ 2 dage
**Status:** ❌ IKKE STARTET

#### **4.1 Weighted scoring system**
- [ ] **Implementer:** `score_column_candidates()`
- [ ] **Vægtning:**
  - Name patterns: 30%
  - Data characteristics: 40%
  - Statistical properties: 30%
- [ ] **Output:** Ranked column suggestions

#### **4.2 Danish SPC patterns**
- [ ] **Udvid:** Pattern library for danske kolonnenavne
- [ ] **Test:** Real-world danske SPC datasæt
- [ ] **Optimér:** Success rate på eksisterende data

---

## 📊 SUCCESS METRICS

### **Kvalitative mål:**
- [ ] **Eliminering af overlap:** Kun én autodetect funktion
- [ ] **Konsistent state:** Alle resultater i unified location
- [ ] **Predictable triggers:** Klare trigger-regler
- [ ] **Robust datohandling:** Danske formater fungerer konsistent
- [ ] **Manual fallback:** Dropdown override altid mulig

### **Kvantitative mål:**
- [ ] **Test coverage:** >95% på autodetect funktionalitet
- [ ] **Success rate:** >90% korrekt kolonnedetection på test data
- [ ] **Performance:** <500ms for autodetect operation
- [ ] **Error rate:** <1% runtime fejl i autodetect

---

## 🧪 TESTING STRATEGY

### **Test-first development:**
```r
# tests/testthat/test-unified-autodetect.R

test_that("autodetect frozen state prevents re-running", {
  # Test at system ikke kører igen efter første trigger
})

test_that("manual trigger overskriver frozen state", {
  # Test at manuel knap altid virker
})

test_that("dansk datoformater detekteres korrekt", {
  # Test lubridate integration med dmy, dmY patterns
})

test_that("unified engine erstatter alle legacy funktioner", {
  # Integration test af ny vs. gammel adfærd
})
```

### **Regression testing:**
- [ ] **Test:** All existing functionality preserved
- [ ] **Test:** No performance degradation
- [ ] **Test:** Backward compatibility maintained

---

## 📝 IMPLEMENTATION NOTES

### **Arkitektur principper:**
- **Single responsibility:** Hver funktion har én klar opgave
- **Event-driven:** Ingen direkte function calls, kun events
- **Defensive programming:** Robust error handling på alle niveauer
- **Test-driven:** Tests skrives før implementation

### **Dependencies:**
- **lubridate:** Primær datodetection
- **lgr:** Struktureret logging
- **Event system:** Existing unified event architecture
- **app_state:** Centralized state management

### **Migration strategy:**
- **Graduel replacement:** Test ny engine parallelt med gammel
- **Feature flags:** Mulighed for at skifte mellem systemer
- **Rollback plan:** Klar procedure for at revertering hvis nødvendigt

---

## ⚠️ RISICI OG MITIGERING

### **Højrisiko områder:**
1. **Breaking changes:** Ny engine kan ændre eksisterende adfærd
   - **Mitigering:** Comprehensive regression testing
2. **Performance impact:** Ny scoring algoritmer kan være langsommere
   - **Mitigering:** Performance benchmarking og optimization
3. **Data corruption:** State management fejl kan corrumpere kolonnemappings
   - **Mitigering:** Extensive state validation og backup mechanisms

### **Lav-risiko områder:**
1. **UI changes:** Minimal impact på user interface
2. **File handling:** Existing file operations bevares
3. **Visualization:** Charts er uafhængige af autodetect implementation

---

## 📋 CHECKLIST VED COMPLETION

### **Før deployment:**
- [ ] Alle tests bestået (unit + integration + regression)
- [ ] Performance benchmarks mødt
- [ ] Documentation opdateret (CLAUDE.md, README)
- [ ] Code review completed
- [ ] Backup/rollback plan dokumenteret

### **Efter deployment:**
- [ ] User feedback indsamlet
- [ ] Success metrics målt
- [ ] Performance monitoring aktiveret
- [ ] Follow-up optimizations planlagt

---

## 📚 REFERENCE DOKUMENTATION

### **Relaterede filer:**
- `R/fct_data_processing.R` - Existing autodetect functions
- `R/utils_event_system.R` - Event architecture
- `tests/testthat/test-auto-detection.R` - Current test suite
- `global.R` - App state schema

### **Dependencies:**
- Event system dokumentation i `docs/`
- State management patterns i `CLAUDE.md`
- Testing protocols i `TESTING_PROTOCOL.md`

---

**👥 TEAM:** Johan Reventlow + Claude Code
**🎯 DEADLINE:** TBD
**📧 CONTACT:** Se GitHub Issues for spørgsmål og opdateringer