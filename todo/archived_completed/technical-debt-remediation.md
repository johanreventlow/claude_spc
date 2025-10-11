# Technical Debt Remediation Plan (SPC App)

> Udarbejdet: 2025-02-14  
> Kontakt: Technical Debt Archaeologist (Codex)

Dette dokument samler de prioriterede tekniske gældspunkter identificeret i den seneste analyse. Hver sektion beskriver problemets kontekst, foreslået løsning, risikovurdering og konkrete næste trin inkl. tests.

---

## 1. Konsolider `track_memory_usage`
- **Kategori**: Observability / Performance instrumentation  
- **Filer**: `R/utils_performance_monitoring.R`, `R/utils_profiling.R`, `tests/performance/*`  
- **Problembeskrivelse**: Der findes to forskellige `track_memory_usage()`-definitioner (en der forventer `session`, en der forventer `context`). Performance-tests kalder kontekstvarianten, men når pakken loades overskrives funktionen af session-varianten, så målinger aldrig registreres.
- **Foreslået løsning**:
  1. Introdcer en samlet API (f.eks. `track_memory_usage(session = NULL, context = NULL, interval_seconds = 60, …)`) der kan håndtere både snapshots og Shiny-streaming, og returnerer et kendt resultatobjekt (fx en liste med `type = "snapshot"` / `reactiveLog`).
  2. Flyt evt. streamingfunktionalitet til `start_memory_monitoring()` for klar semantik, og lad `track_memory_usage()` være kompatibilitetswrapper med deprecation warning.
  3. Opdater performance-scripts til at teste at loggen indeholder ≥1 måling (`expect_gt(nrow(memory_log()), 0)` eller tilsvarende).
  4. Tilføj unit-test der sikrer at begge kaldetyper (med og uden session) fungerer.
- **Risiko**: Medium – påvirker kun monitorering; ingen runtime-funktionalitet.  
- **Estimeret indsats**: 0,5–1 dag.
- **Testforslag**:
  - `R -e "library(SPCify); testthat::test_file('tests/testthat/test-performance-monitoring.R')"`.
  - Manuel sanity check: kør `R -e "source('global.R'); track_memory_usage(context = 'manual'); get_memory_summary(memory_log)"`.

---

## 2. Tilføj `visualization_update_needed` til event-bus
- **Kategori**: State management / Reactive integrity  
- **Filer**: `R/state_management.R`, `R/mod_spc_chart_server.R`, `tests/testthat/test-event-bus-full-chain.R`  
- **Problembeskrivelse**: `create_app_state()` initialiserer ikke `visualization_update_needed`, hvilket giver `integer(0)` ved første emit og kræver ad-hoc guards i moduler. Andre komponenter (fx `fct_autodetect_unified`) kan trigge eventet før modulets guard kører.
- **Foreslået løsning**:
  1. Tilføj `visualization_update_needed = 0L` i `app_state$events`.  
  2. Fjern lokal initialisering i `mod_spc_chart_server.R` og opdater tests, så de ikke selv initialiserer.
  3. Udvid test-suite med check der sikrer at første emit faktisk trigger observer (`expect_equal(app_state$events$visualization_update_needed, 1L)` og at observer sideeffekt kører).
- **Risiko**: Høj – påvirker alle plotopdateringer, men ændringen er simpel.  
- **Estimeret indsats**: ≤0,5 dag.
- **Testforslag**:
  - `R -e "library(SPCify); testthat::test_file('tests/testthat/test-mod-spc-chart-integration.R')"`
  - Kør end-to-end workflow test `test-event-bus-full-chain.R` for at sikre eventsekvens.

---

## 3. Ryd op i `NAMESPACE`-eksporter
- **Kategori**: Build hygiene / CRAN compliance  
- **Filer**: `NAMESPACE`, roxygen-kommentarer (fx `R/NAMESPACE`, `R/*.R`)  
- **Problembeskrivelse**: `NAMESPACE` indeholder `export("for")`, `export(and)`, `export(Extract)` m.fl., som ikke er defineret. Dette udløser fejl ved `R CMD check` og gør pakken ikke-udgivelsesbar.
- **Foreslået løsning**:
  1. Find roxygen-kilder der forårsager eksporter (søg efter `@export` i dokumentationsblokke for tekst). Fjern eller erstat med `@keywords internal`.
  2. Kør `R -e "devtools::document()"` for at regenerere `NAMESPACE`.
  3. Verificér at `R CMD check` ikke rapporterer manglende objekter.
- **Risiko**: Medium – ændrer build-output men ingen runtime.  
- **Estimeret indsats**: 0,5 dag.
- **Testforslag**:
  - `R -e "devtools::document()"`, efterfulgt af `R -e "devtools::check()"`.
  - Tilføj CI-lint der kører `grep -n 'export(\"' NAMESPACE` og fejler hvis nogen findes.

---

## 4. Centralisér viewport state
- **Kategori**: Architecture compliance / State management  
- **Filer**: `R/mod_spc_chart_server.R`, `R/state_management.R`, `tests/testthat/test-visualization-state.R` (ny)  
- **Problembeskrivelse**: Modulet bruger lokal `shiny::reactiveVal` til viewport-dimensioner. Det omgår `app_state` og kan ikke observeres af andre komponenter.
- **Foreslået løsning**:
  1. Tilføj `app_state$visualization$viewport_dims <- list(width = NULL, height = NULL, last_updated = NULL)` ved state-init.
  2. Erstat `viewport_dims()` reactiveVal med helper-funktioner (`set_viewport_dims()`, `get_viewport_dims()`) der læser/skriver til `app_state`.
  3. Efter state-opdatering, emit `visualization_update_needed()` for at trigge downstream forbrugere.
  4. Skriv test der verificerer at modul-opdateringer ændrer `app_state$visualization$viewport_dims`.
- **Risiko**: Medium – ændrer modul-internals.  
- **Estimeret indsats**: 0,5 dag.
- **Testforslag**:
  - Udvid eksisterende modul-integrationstest (`test-mod-spc-chart-integration.R`) med viewport assertions.
  - Manuel QA: Kør appen, resiz vindue og se om dimensioner logger korrekt via `log_debug_kv`.

---

## 5. Beslut skæbne for `utils_server_performance_opt.R`
- **Kategori**: Codebase consolidation  
- **Filer**: `R/utils_server_performance_opt.R`, `man/setup_optimized_event_listeners.Rd`  
- **Problembeskrivelse**: Filen eksporterer et alternativt event-system med advarsler om dobbeltkørsel, men bruges ikke. Risiko for utilsigtet aktivering og dobbelt processering.
- **Foreslået løsning**:
  - **Option A (anbefalet)**: Flyt filen til `candidates_for_deletion/` og fjern exports/man-sider. Dokumentér i ADR hvorfor pipeline droppes.
  - **Option B**: Hvis pipeline skal bevares, pak den i feature flag (`getOption("spc.use_optimized_listeners")`) og tilføj tests for begge mode.
- **Risiko**: Medium (fejlkonfigurationsfare).  
- **Estimeret indsats**: 1–1,5 dage inkl. ADR/oprydning.
- **Testforslag**:
  - Hvis Option A: `R -e "devtools::check()"` for at sikre ingen man-sider refererer til fjernet funktion.
  - Hvis Option B: Ny testfil `test-optimized-event-listeners.R`.

---

## 6. Trim ubrugte profileringsværktøjer
- **Kategori**: API surface / Maintainability  
- **Filer**: `R/utils_profiling.R`, `man/profile_*.Rd`, `NAMESPACE`  
- **Problembeskrivelse**: `profile_reactive`, `benchmark_reactives`, `profile_data_pipeline` er eksporteret, men bruges ikke i runtime.
- **Foreslået løsning**:
  1. Flyt funktionerne til `dev/` eller markér som interne (`@keywords internal` + fjern eksport).
  2. Overvej at beskrive i udviklerdokumentation hvordan værktøjerne anvendes i TDD workflow, hvis de skal bevares.
- **Risiko**: Lav.  
- **Estimeret indsats**: <0,5 dag.
- **Testforslag**:
  - `devtools::document()` for at rengøre man-sider.
  - Sikr at tests der evt. bruger funktionerne bliver opdateret (ingen fundet pt.).

---

## 7. Fjern `utils_server_event_listeners.R.backup`
- **Kategori**: Repo hygiene  
- **Filer**: `R/utils_server_event_listeners.R.backup`  
- **Problembeskrivelse**: Legacy backupfil i `R/` skaber forvirring ved kodegennemlæsning.
- **Foreslået løsning**:
  1. Flyt filen til `candidates_for_deletion/` eller slet den helt efter bekræftet ubrugelighed.
  2. Dokumentér i commit message/ADR for historik.
- **Risiko**: Lav.  
- **Estimeret indsats**: <0,25 dag.
- **Testforslag**:
  - Kør `R -e "devtools::check()"` for at sikre ingen referencer tilbage.

---

## Efterfølgende trin
1. Prioritér Issues 1–3 i kommende sprint for at sikre stabil observability og build pipeline.  
2. Planlæg kort review-workshop for at beslutte fremtid for performance-pipeline (Issue 5).  
3. Efter ændringer: kør fuld test-suite (`R -e "library(SPCify); testthat::test_dir('tests/testthat')"`) samt `devtools::check()` og opdater coverage-rapport.

Når de ovenstående punkter er løst, opdater `CHANGELOG.md` og overvej ADR for større arkitekturvalg (event-bus, performance pipeline).

