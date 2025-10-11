# Legacy Remediation Action Plan

Handlingsliste baseret på den seneste tekniske gældsanalyse. Alle punkter følger projektets konventioner (CLAUDE.md) og inkluderer konkrete løsningsskridt samt anbefalet testdækning.

---

## 1. Initialiser `visualization_update_needed` i event-bus (Kritisk)
- **Placering**: `R/state_management.R` (`create_app_state()`)
- **Problem**: Feltet mangler i `app_state$events`. Første kald til `emit$visualization_update_needed()` forsøger at evaluere `NULL + 1L`, hvilket returnerer `integer(0)` og stopper downstream-plotopdateringer.
- **Konsekvens**: Visualiseringer kan fryse på første opdatering, indtil modulet manuelt nulstiller værdien.
- **Løsningsskridt**:
  1. Tilføj `visualization_update_needed = 0L` i `shiny::reactiveValues(...)` blokken inde i `create_app_state()`.
  2. Dokumentér feltet i state-schema-kommentaren og evt. i Appendix D, så eventet er officielt del af kontrakten.
  3. Udvid unit-test (fx `tests/testthat/test-state-management.R`) med et assert, der verificerer, at feltet eksisterer og incrementerer fra `0L` til `1L` ved første emit.
- **Tests**:
  - `R -e "library(SPCify); testthat::test_file('tests/testthat/test-state-management.R')"`
  - Fuldt test-suite run før commit.

---

## 2. Flyt viewport-dimensioner til central state (Høj)
- **Placering**: `R/mod_spc_chart_server.R:16` og `R/state_management.R`
- **Problem**: Lokalt `shiny::reactiveVal()` bryder det centraliserede stateprincip og forhindrer andre services i at konsumere viewport-metadata.
- **Konsekvens**: Risiko for divergerende viewport-håndtering og dobbeltimplementering ved andre visualiseringsmoduler.
- **Løsningsskridt**:
  1. Udvid `app_state$visualization` med en `viewport_dims` subnode (`shiny::reactiveValues(width = NULL, height = NULL, updated_at = NULL)`).
  2. Implementér helper (fx `set_viewport_dims(app_state, width, height)`) der opdaterer state atomisk og emitter `visualization_update_needed()`.
  3. Refaktor `visualizationModuleServer()` til at benytte helperen og læse direkte fra `app_state`.
  4. Opdater integrationstest (`tests/testthat/test-mod-spc-chart-integration.R`) til at verificere, at viewport-opdateringer trigger eventet.
- **Tests**:
  - Modul-integrationstests (`test-mod-spc-chart-integration.R`)
  - Event-bus kædetest (`test-event-bus-full-chain.R`)

---

## 3. Konverter label-pipeline logging til struktureret API (Høj)
- **Placering**: `R/fct_add_spc_labels.R`
- **Problem**: Stort antal `message()` kald bryder logging-standarden, kan ikke filtreres efter `component`, og skaber støj i produktion.
- **Konsekvens**: Observability degraderes, og verbose-tilstand kan “bløde” direkte til brugerens konsol.
- **Løsningsskridt**:
  1. Erstat `message()` med `log_debug()`/`log_info()` og sæt `component = "[LABEL_PLACEMENT]"`. Brug `details = list(...)` til metadata (viewport, y_unit, verbose flag).
  2. Pak kritiske sektioner i `safe_operation()` hvis der er risiko for exceptions.
  3. Introducér (eller mock) logging i tests for at sikre, at central logger kaldes.
- **Tests**:
  - Udbyg evt. `tests/testthat/test-label-placement.R` med mocking af `log_debug`.
  - Snapshot-tests hvis log-output er del af forventet artefakt.

---

## 4. Konsolider NAMESPACE-håndtering (Høj)
- **Placering**: `NAMESPACE` (roten) og `R/NAMESPACE`
- **Problem**: To forskellige NAMESPACE-filer. Root-versionen eksporterer fantomobjekter (`"for"`, `Toggle`, m.fl.) pga. historiske roxygen-fejl.
- **Konsekvens**: `R CMD check` producerer NOTES, og nye bidragsydere risikerer at redigere den forkerte fil.
- **Løsningsskridt**:
  1. Arkivér eller slet `R/NAMESPACE` (den manuelle kopi).
  2. Gennemgå roxygen-kommentarer for utilsigtede `@export` (primært kapiteloverskrifter/eksempler).
  3. Kør `devtools::document()` for at regenerere den eneste NAMESPACE.
  4. Verificér via `R CMD check` at ingen dummy-eksporter er tilbage.
- **Tests**:
  - `R -e "devtools::document()"` (skal køre uden warnings).
  - `R -e "devtools::check()"` for at sikre, at NOTES forsvinder.

---

## 5. Afklar og håndter performance-optimeringsmodul (Medium)
- **Placering**: `R/utils_server_performance_opt.R` + tilhørende man-sider
- **Problem**: Eksporterede funktioner (`create_optimized_data_pipeline()`, `setup_optimized_event_listeners()` osv.) bliver aldrig kaldt.
- **Konsekvens**: Støjer i API’et og forvirrer udviklere; øger vedligeholdelsesbyrden.
- **Løsningsskridt**:
  1. Bekræft med produktteam om funktionerne stadig er planlagt til brug. Dokumentér beslutning i ADR hvis de droppes.
  2. Hvis funktionerne ikke skal bruges, fjern eksport-tags og flyt filen til `candidates_for_deletion/` eller slet den helt.
  3. Hvis funktionerne skal aktiveres, integrer kallene i `initialize_app_infrastructure()` og tilføj tests, der sikrer ingen dobbeltudførsel af pipelines.
- **Tests**:
  - `tests/testthat/test-package-initialization.R` (udvid med checks for aktive optimeringer)
  - Lintr/CI for at sikre, at døde exports er væk.

---

## 6. Fjern backup af event listener-fil (Medium)
- **Placering**: `R/utils_server_event_listeners.R.backup`
- **Problem**: Stale fil duplikerer aktiv implementering; høj risiko for, at changes lander i backup i stedet for primær fil.
- **Løsningsskridt**:
  1. Flyt filen til `candidates_for_deletion/` eller slet den. Dokumentér i PR/ADR, at versionering håndteres via git.
  2. Bekræft, at kun `R/utils_server_event_listeners.R` sources i `global.R`.
- **Tests**:
  - `R -e "pkgload::load_all('.')"` for at sikre, at sourcing virker uden backupfilen.
  - `R -e "library(SPCify); testthat::test_dir('tests/testthat')"` (regression).

---

## Quick Wins / Tæt på “trivial fix”
1. Tilføj `visualization_update_needed = 0L` og unit-test (se sektion 1).
2. Fjern `R/utils_server_event_listeners.R.backup` (sektion 6).
3. Start migrering af `message()` → `log_debug()` i label-pipelinen ved at udskifte de mest hyppige kald først.

---

## Anbefalet arbejdsgang
1. Opret feature branch per emne (fx `fix/visualization-event-init`).
2. Implementér ændringer efter TDD (tests først hvor muligt).
3. Kør fuld test-suite + `devtools::document()` før commit.
4. Opdater relevant dokumentation/ADR når beslutninger træffes (især for performance-modulet).

