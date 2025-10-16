# BFHchart Transition – GitHub Issue Pack

> **Formål:** Dokumentere de issues der skal oprettes på GitHub for at skifte Shiny-appens SPC-plot fra qicharts2 til BFHchart. Hvert issue følger projektets konventioner for opgaver, tests og afhængigheder, så de kan kopieres direkte over i GitHub.

---

## Issue 1 – BFHchart feasibility og kravafdækning

- **Titel:** `feat(spc): afdæk BFHchart feasibility og krav`
- **Labels:** `feature`, `analysis`, `spc`
- **Problemstatement:** Vi skal sikre, at BFHchart dækker alle eksisterende SPC-funktioner og identificere gaps før migrering.
- **Acceptance criteria:**
  - Dokumentation for BFHchart’s API og muligheder foreligger i `todo/spc_plot_modernization_plan.md`.
  - Klar mapping af feature-paritet (Anhøj-regler, faser, kommentarer, run/I/P-chart varianter).
  - Definerede TDD-scenarier for kritiske plottyper (inkl. edge cases) klar til næste issue.
- **Primære filer der skal undersøges:** `R/fct_spc_plot_generation.R`, `R/utils_qic_preparation.R`, `tests/testthat/test-spc-plot-generation-comprehensive.R`.
- **Tests (skrives, men forventes at fejle indtil senere issues):** Placeholder tests der beskriver forventet BFHchart-output for run- og I-chart med faser og kommentarer.
- **Afhængigheder:** Ingen.

---

## Issue 2 – Dependency og bootstrap-lag for BFHchart

- **Titel:** `chore(spc): etabler BFHchart dependency og bootstrap`
- **Labels:** `chore`, `infrastructure`, `spc`
- **Problemstatement:** Projektet skal kunne loade BFHchart deterministisk uden at påvirke eksisterende miljøopsætning.
- **Acceptance criteria:**
  - `DESCRIPTION` og `renv` planlagt til at inkludere BFHchart (ingen direkte ændring af `NAMESPACE`).
  - Design-notat for lazy loading og golem options tilgængeligt (refereret fra Issue 1 dokumentation).
  - Intern facade-funktion defineret (signatur, forventet output) så resten af appen kan skiftes uden yderligere coupling.
- **Primære filer:** `DESCRIPTION`, `global.R`, `R/utils_lazy_loading.R`, `R/app_initialization.R`.
- **Tests:** Skitser integrationstest der validerer at BFHchart kan initialiseres i test-mode (fx mock session). Ingen implementering endnu.
- **Afhængigheder:** Issue 1.

---

## Issue 3 – Ny SPC-renderingsservice med BFHchart

- **Titel:** `feat(spc): implementér BFHchart renderingsservice`
- **Labels:** `feature`, `spc`, `backend`
- **Problemstatement:** Vi skal erstatte den eksisterende qicharts2-baserede compute-/execute-pipeline med BFHchart og levere de samme dataobjekter til appen.
- **Acceptance criteria:**
  - Ny service (`compute_spc_results_bfh()`) etableret inkl. `safe_operation()` og `app_state`-integration.
  - Kommentar- og fasehåndtering understøttet og dokumenteret.
  - Anhøj-resultater og struktureret data returnerer i samme format som tidligere (eller dokumenteret ændring).
- **Primære filer:** `R/fct_spc_plot_generation.R`, `R/utils_spc_data_processing.R`, `R/state_management.R`.
- **Tests:** Udvid `tests/testthat/test-spc-plot-generation-comprehensive.R` til at validere BFHchart-dataframes og signaler; unit tests for servicefunktionerne.
- **Afhængigheder:** Issue 2.

---

## Issue 4 – Integration i Shiny-moduler og event-bus

- **Titel:** `feat(spc): integrer BFHchart i shiny-moduler og event-bus`
- **Labels:** `feature`, `spc`, `frontend`
- **Problemstatement:** Shiny-modulerne skal bruge BFHchart-servicen og respektere event-arkitekturen uden at introducere race conditions.
- **Acceptance criteria:**
  - `mod_spc_chart_server.R` og `fct_visualization_server.R` opdateret til nyt API.
  - Observer-prioriteter og guards verificeret i `R/utils_server_event_listeners.R`.
  - UI-dimensioner og label-helpers opdateret hvor BFHchart ændrer krav.
- **Primære filer:** `R/mod_spc_chart_server.R`, `R/fct_visualization_server.R`, `R/utils_visualization_dimensions.R`, `R/fct_add_spc_labels.R`.
- **Tests:** `shinytest2`-scenarier der emulerer `emit$data_updated()` og bekræfter at BFHchart-plot renderes; integrationstests i testthat.
- **Afhængigheder:** Issue 3.

---

## Issue 5 – Caching, performance og instrumentering

- **Titel:** `perf(spc): rekonfigurer caching og performance for BFHchart`
- **Labels:** `performance`, `spc`, `observability`
- **Problemstatement:** De nuværende qic-specifikke cache- og performanceinstrumenter skal moderniseres til BFHchart for at bevare responstid.
- **Acceptance criteria:**
  - Nye cache keys og invalidationsflow dokumenteret og implementeret uden regressioner.
  - Logging opdateret til at reflektere BFHchart-pipelinen (komponent tags, detaljer).
  - `benchmark_qic_cache.R` moderniseret eller erstattet med ny benchmark-fil.
- **Primære filer:** `R/utils_qic_caching.R`, `R/utils_qic_cache_invalidation.R`, `R/utils_logging.R`, `benchmark_qic_cache.R`.
- **Tests:** Performance regression-tests under `tests/performance/` der sammenligner kald med og uden cache; logging tests hvor det er relevant.
- **Afhængigheder:** Issue 3 (for dataformat) og Issue 4 (for event hooks).

---

## Issue 6 – Oprydning, dokumentation og regressionssikring

- **Titel:** `chore(spc): oprydning og dokumentation efter BFHchart-migrering`
- **Labels:** `chore`, `docs`, `spc`
- **Problemstatement:** Efter migreringen skal vi fjerne qicharts2-rester, opdatere dokumentation og sikre regression pipeline.
- **Acceptance criteria:**
  - qicharts2-referencer fjernet/erstattet i kode og dokumenter (`dev/DEVELOPER_GUIDE.md`, `dev/roadmap.md`, `README.md`, `CHANGELOG.md`).
  - Endelig regressionstest-suite (testthat, shinytest2, lintr) kørt og dokumenteret i issue.
  - Eventuelle eksportfunktioner og bruger-facing beskrivelser opdateret til BFHchart.
- **Primære filer:** `R/utils_qic_debug_logging.R`, `dev/DEVELOPER_GUIDE.md`, `dev/roadmap.md`, `README.md`, `CHANGELOG.md`.
- **Tests:** Kør hele test-suiten og dokumentér resultater; snapshot-tests for UI efter oprydning.
- **Afhængigheder:** Issue 1–5.

---

> **Næste skridt:** Kopiér hvert issue ind i GitHub med relevante labels og assign til ansvarlige udviklere. Brug denne fil som reference gennem migreringen og opdater den efterhånden som issues lukkes.
