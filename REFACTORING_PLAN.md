# Refaktoreringsplan: Arkitektur Cleanup

## Overordnet mål
Transformere det nuværende legacy-projekt til en moderne, vedligeholdelsesvenlig R Shiny applikation med proper pakke-struktur, robuste afhængigheder og eliminering af kritiske runtime-fejl.

## Status: 🔄 I gang
**Branch:** `refactor/architecture-cleanup`
**Start dato:** 2025-09-21

---

## Fase 1: Kritiske Runtime-fejl (🔥 Højeste prioritet)

### 1.1 Fix isolate() fejl i server initialization
- **Problem:** `isolate()` kaldes uden reaktiv kontekst i server startup
- **Lokation:** Server initialization og `safe_operation()` kald
- **Løsning:** Fjern unødvendige `isolate()` kald udenfor reaktive kontekster
- **Test:** Verificer at server starter uden fejl
- **Commit:** `fix: fjern isolate() kald uden reaktiv kontekst i server init`

### 1.2 Fix later() callback fejl
- **Problem:** `isolate()` bruges i `later::later()` callbacks uden reaktiv kontekst
- **Lokation:** Periodiske callbacks i server
- **Løsning:** Wrap callbacks i `shiny::withReactiveDomain()` eller brug direkte læsning
- **Test:** Verificer at periodiske operationer fungerer
- **Commit:** `fix: ret later() callbacks til at fungere uden reaktiv kontekst`

### 1.3 Fix TEST_MODE_AUTO_LOAD miljøafhængig konfiguration
- **Problem:** Default til TRUE uden miljøvariabel er farligt i produktion
- **Lokation:** `global.R` linje 172
- **Løsning:** Implementer smart environment detection:
  - Development (RStudio/shiny::runApp): Default TRUE
  - Production (shinyapps.io/Connect): Default FALSE
  - Test environment: Respekter eksplicit miljøvariabel
- **Pattern:** Detect deployment context før default setting
- **Test:** Verificer korrekte defaults i forskellige miljøer
- **Commit:** `fix: implementer miljøafhængig TEST_MODE_AUTO_LOAD konfiguration`

**Fase 1 mål:** App starter og kører stabilt uden runtime-fejl

---

## Fase 2: Afhængighedsstyring (📦 Høj prioritet)

### 2.1 Inventarisering af dependencies
- **Handling:** Gennemgå alle `library()` kald i `global.R`
- **Dokumenter:** Liste over manglende pakker i DESCRIPTION
- **Output:** Komplet dependency audit
- **Commit:** `docs: tilføj dependency audit til refaktoreringsplan`

### 2.2 Opdater DESCRIPTION fil
- **Handling:** Tilføj alle manglende pakker til `Imports` sektion
- **Pakker:** ggplot2, ggrepel, stringi, shinyWidgets, zoo, scales, rlang, lubridate, openxlsx, later
- **Test:** `devtools::check()` skal passere dependency checks
- **Commit:** `feat: tilføj manglende dependencies til DESCRIPTION`

### 2.3 Konverter library() til namespace calls
- **Handling:** Erstat `library(pakke)` med `pakke::funktion` i kode
- **Scope:** Primært i modules og utility funktioner
- **Test:** App funktionalitet bevares efter ændring
- **Commit:** `refactor: konverter library() kald til namespace references`

**Fase 2 mål:** Alle dependencies eksplicit deklareret og kontrolleret

---

## Fase 3: Arkitektur Refaktorering (🏗️ Medium prioritet)

### 3.1 Analyse af global.R struktur
- **Handling:** Dokumenter alt hvad `global.R` gør i øjeblikket
- **Mapping:** Kortlæg alle `source_from_base()` kald og deres afhængigheder
- **Output:** Arkitektur diagram af nuværende struktur
- **Commit:** `docs: dokumenter nuværende global.R arkitektur`

### 3.2 Opret modulær initialisering
- **Handling:** Lav `R/app_config.R` til konfiguration
- **Handling:** Lav `R/app_dependencies.R` til dependency loading
- **Handling:** Lav `R/app_initialization.R` til app setup
- **Test:** Funktionalitet bevares med ny struktur
- **Commit:** `refactor: opdel global.R i modulære komponenter`

### 3.3 Refaktorér run_app() funktionen
- **Problem:** Afhængig af global state og manual source() kald
- **Løsning:** Gør `run_app()` selvstændig med eksplicitte parametre
- **Fjern:** `.rs.invokeShinyWindowViewer` RStudio-specifik kode
- **Test:** App kan startes i forskellige miljøer
- **Commit:** `refactor: gør run_app() uafhængig af global state`

### 3.4 Implementer proper golem pattern
- **Handling:** Brug `golem::with_golem_options()` for konfiguration
- **Handling:** Implementer `golem::run_dev()` for development
- **Handling:** Strukturer app som ægte R pakke
- **Test:** `devtools::check()` skal passere
- **Commit:** `refactor: implementer golem best practices`

**Fase 3 mål:** Ren, modulær arkitektur uden global state dependencies

---

## Fase 4: Konfiguration og Environment (⚙️ Medium prioritet)

### 4.1 Centraliseret konfigurationssystem
- **Handling:** Saml alle konfigurationsvariabler i `R/app_config.R`
- **Erstatter:** Spredte konstanter i `global.R`
- **Pattern:** Brug `options()` eller `golem::get_golem_options()`
- **Test:** Alle konfigurationer virker som forventet
- **Commit:** `refactor: centralisér app konfiguration`

### 4.2 Environment-specific indstillinger
- **Handling:** Opret development/production/test configs
- **Miljøer:** DEV, PROD, TEST med passende defaults
- **Sikkerhed:** Ingen farlige defaults i produktion
- **Test:** Verificer korrekt adfærd i hvert miljø
- **Commit:** `feat: implementer environment-specific konfiguration`

### 4.3 Logging og debug cleanup
- **Handling:** Konsolidér logging til `utils_logging.R` system
- **Fjern:** Forældede cat() debug statements
- **Standard:** Brug kun `log_debug()`, `log_info()`, etc.
- **Test:** Logging fungerer korrekt på alle niveauer
- **Commit:** `refactor: standardisér logging system`

**Fase 4 mål:** Konsistent, environment-aware konfiguration

---

## Fase 5: Code Cleanup og Dokumentation (📚 Lav prioritet)

### 5.1 Fjern legacy kommentarer
- **Handling:** Slet alle "DELETED/REMOVED/PHASE X" kommentarer
- **Handling:** Fjern uddaterede TODO comments
- **Handling:** Opdater aktive kommentarer til at være relevante
- **Test:** Kode er lettere at læse
- **Commit:** `cleanup: fjern forældede kommentarer og legacy noter`

### 5.2 Dokumentation opdatering
- **Handling:** Opret aktuel `ARCHITECTURE.md`
- **Handling:** Opdater README med korrekt installation/usage
- **Handling:** Fjern referencer til ikke-eksisterende filer
- **Test:** Dokumentation matcher faktisk arkitektur
- **Commit:** `docs: opdater arkitektur dokumentation`

### 5.3 Test coverage verification
- **Handling:** Kør alle eksisterende tests med ny arkitektur
- **Handling:** Tilføj tests for nye komponenter hvor nødvendigt
- **Handling:** Verificer at TDD workflow stadig fungerer
- **Test:** 100% test pass rate
- **Commit:** `test: verificer test coverage efter refaktorering`

**Fase 5 mål:** Ren, veldokumenteret kodebase

---

## Fase 6: Performance og Optimering (🚀 Lav prioritet)

### 6.1 Dependency loading optimering
- **Handling:** Analyser hvilke pakker der faktisk bruges
- **Handling:** Flyt sjældent brugte pakker til `Suggests`
- **Handling:** Implementer lazy loading hvor muligt
- **Test:** App startup tid forbedres
- **Commit:** `perf: optimér dependency loading`

### 6.2 Memory footprint reduktion
- **Handling:** Identificer unødvendige objekter i global environment
- **Handling:** Implementer proper cleanup i session lifecycle
- **Handling:** Optimér reactive expressions
- **Test:** Memory usage reduceres
- **Commit:** `perf: reducér memory footprint`

**Fase 6 mål:** Optimeret performance og resource usage

---

## Testing Strategy

### Per fase testing:
- **Unit tests:** Kør eksisterende test suite efter hver ændring
- **Integration tests:** Verificer core app funktionalitet
- **Manual testing:** Test key user workflows
- **Regression testing:** Sikr at ingen features brydes

### Test kommandoer:
```r
# Kør alle tests
R -e "source('global.R'); testthat::test_dir('tests/testthat')"

# Kør specific test
R -e "source('global.R'); testthat::test_file('tests/testthat/test-file.R')"

# Package check
devtools::check()
```

---

## Risk Management

### Høj risiko områder:
1. **Server initialization** - Kritisk for app funktionalitet
2. **Reactive system** - Komplekse dependencies kan brydes
3. **State management** - `app_state` struktur er central
4. **File loading** - CSV parsing og data handling

### Mitigation strategies:
- **Små, atomiske commits** for hver ændring
- **Backup før hver fase** via git commits
- **Rollback plan** hvis kritiske fejl opstår
- **Staged testing** på separate ports

---

## Definition of Done

### Per fase criteria:
- [ ] Alle tests passerer
- [ ] Manual funktionalitetstest gennemført
- [ ] Code review via git diff
- [ ] Dokumentation opdateret hvis relevant
- [ ] Performance regression check
- [ ] Git commit med beskrivende message

### Samlet projekt success:
- [ ] App starter uden runtime-fejl
- [ ] Alle dependencies korrekt deklareret
- [ ] `devtools::check()` passerer
- [ ] Arkitektur er modulær og vedligeholdelsesvenlig
- [ ] Dokumentation matcher implementering
- [ ] Test coverage bevaret eller forbedret

---

## Progress Tracking

| Fase | Status | Start | Slut | Commits |
|------|--------|-------|------|---------|
| 1 - Runtime fejl | 🔄 I gang | 2025-09-21 | | a4e33ea |
| 2 - Dependencies | ⏳ Afventer | | | |
| 3 - Arkitektur | ⏳ Afventer | | | |
| 4 - Konfiguration | ⏳ Afventer | | | |
| 5 - Cleanup | ⏳ Afventer | | | |
| 6 - Performance | ⏳ Afventer | | | |

**Nuværende fokus:** Fase 1.2 - Fix later() callback fejl ✅ (komplet som del af a4e33ea)

---

## Næste skridt
1. Start med Fase 1.1 - identificer og fix isolate() fejl
2. Test at server kan starte uden fejl
3. Commit ændringen
4. Fortsæt til Fase 1.2

**Klar til at begynde refaktorering!** 🚀