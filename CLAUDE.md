# Claude Instruktioner – SPC App

## 1) Projektoversigt

Dette er en **R Shiny** applikation til **Statistical Process Control (SPC)** med **qicharts2**. Appen anvendes i klinisk kvalitetsarbejde og skal forblive stabil, forståelig og på dansk.

**Udviklingsstatus:** Projektet følger industristandard mønstre for Shiny-udvikling med test-driven development, centraliseret state management, robust error handling og moden build-/deploy-automation.

---

## 2) Udviklingsprincipper (Fundamentale regler)

### 2.1 Test-First Development (TDD)

✅ **OBLIGATORISK:** Al udvikling følger Test-Driven Development:

1. **Skriv tests først** – Definér den forventede adfærd, før kode ændres
2. **Kør tests kontinuerligt** – Tests køres efter hver ændring og skal altid bestå
3. **Refactor med test-sikkerhed** – Ingen kodeændring uden test-coverage
4. **Ingen breaking changes** – Eksisterende tests må ikke brydes uden eksplicit godkendelse

**Test-kommandoer:**
```r
# Kør alle tests
R -e "source('global.R'); testthat::test_dir('tests/testthat')"

# Kør specifik test-fil
grep "^test-.*\\.R$" tests/testthat -n
R -e "source('global.R'); testthat::test_file('tests/testthat/test-fase1-refactoring.R')"

# Package loading test (foretrukket)
R -e "library(SPCify); testthat::test_dir('tests/testthat')"

# Source loading test (debugging)
R -e "options(spc.debug.source_loading=TRUE); source('global.R'); testthat::test_dir('tests/testthat')"

# Performance benchmark
R -e "microbenchmark::microbenchmark(
  package = library(SPCify),
  source = source('global.R'),
  times = 5
)"

# Test-coverage verification
# Tests skal bestå før og efter hver ændring
```

### 2.2 Defensive Programming

* **Input validation** – Valider alle inputs ved entry points
* **Error handling** – Brug `safe_operation()` og eksplicit `tryCatch()` blokke
* **Scope guards** – Benyt `exists()` checks ved migrations/logiske skift
* **Graceful degradation** – Implementér fallback-mønstre hvor komponenter kan fejle
* **State consistency** – Sikr dual-state synkronisering for kompatibilitet

### 2.3 Git Workflow & Version Control (OBLIGATORISK)

✅ **KRITISKE GIT-REGLER** – Følg disse regler nøje:

1. **ALDRIG merge til master uden eksplicit bruger-godkendelse**
2. **ALDRIG push til remote uden eksplicit anmodning**
3. **ALTID stop efter feature branch commit og vent på instruktioner**
4. **ALTID spørg før merge, rebase eller andre git-operationer på master**

**Git workflow:**
```bash
# Korrekt: Opret feature branch og commit
git checkout -b feature/my-feature
# ... arbejd og commit ...
git commit -m "beskrivelse"
# STOP HER - Vent på bruger-instruktion

# Forkert: Automatisk merge uden tilladelse
git checkout master && git merge feature/my-feature  # ALDRIG GØR DETTE
```

**Undtagelser:** Kun simple git-operationer som `git status`, `git diff`, `git log` kan udføres frit.

### 2.4 Observability & Debugging

**DEBUG-FIRST Approach:**

* **Struktureret logging** – Brug det centrale logger-API i `R/utils/logging.R` (`log_debug()`, `log_info()`, `log_warn()`, `log_error()`). Rå `cat()`-kald må ikke anvendes.
* **Kontekst-tags** – Angiv `component`-felt (fx `[APP_SERVER]`, `[FILE_UPLOAD]`, `[COLUMN_MGMT]`, `[PHASE4]`, `[AUTO_DETECT]`, `[PLOT_DATA]`) for alle log-beskeder.
* **Struktureret payload** – Tilføj relevante data som named list i `details`-argumentet, så logs kan aggregeres.
* **Systematisk fejlsporing** – Brug `log_error()` og `safe_operation()` til konsistent fejlrapportering.
* **Reaktiv inspektion** – Debug reactive chains via `inspect_state()` utilities og målrettede test helpers.

**Logging eksempel:**
```r
log_debug(
  component = "[APP_SERVER]",
  message = "Initialiserer data-upload observer",
  details = list(session_id = session$token)
)
```

### 2.4 Modularity & Architecture

* **Single Responsibility** – Hver funktion løser én klart defineret opgave
* **Immutable data flow** – Undgå in-place mutation; returnér nye objekter
* **Centralized state management** – Brug `app_state` schema fremfor spredte `values$`
* **Event-driven patterns** – Udløs events via den fælles event-bus i stedet for ad-hoc triggers
* **Dependency injection** – Funktioner modtager deres afhængigheder som argumenter (se `R/utils/dependency_injection.R`)

---

## 3) Tekniske Best Practices

### 3.1 Shiny Best Practices

✅ **Unified Event Architecture (OBLIGATORISK for al ny udvikling):**
```r
# ✅ Korrekt brug af event-bus
emit$data_loaded()
emit$columns_detected()
emit$ui_sync_needed()

observeEvent(app_state$events$data_loaded, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$HIGH, {
  handle_data_loaded()
})

# ❌ Forkert: Ad-hoc reactiveVal triggers
legacy_trigger <- reactiveVal(NULL)
observeEvent(legacy_trigger(), { shiny::showNotification("Undgå dette mønster") })
```

**Event-arkitektur:**
* **Data change** → **Emit event** → **Centralized listener** → **State update** → **Cascade events**
* **Events** defineres i `global.R` (`app_state$events`)
* **Emit-funktioner** tilføjes i `create_emit_api()`
* **Lyttere** registreres i `R/utils_event_system.R` via `setup_event_listeners()`

**Unified State Management (OBLIGATORISK for al data):**
```r
# ✅ App state som single source of truth
app_state$data$current_data <- new_data
app_state$columns$x_column <- detected_column
app_state$session$file_uploaded <- TRUE

# ❌ Forkert: Lokale reactiveVal til delt state
values$some_data <- data
```

**Reactive Programming Patterns:**
* **Event-baserede triggere** – Undgå implicitte afhængigheder
* **Prioritetsstyring** – Brug `priority = OBSERVER_PRIORITIES$HIGH/MEDIUM/LOW`
* **Explicit dependencies** – `req()` og `validate()` før logik
* **Isolation når nødvendigt** – Brug `isolate()` med omtanke og kun i reaktiverede kontekster
* **Error boundaries** – Wrap komplekse reactive udtryk i `safe_operation()`

**File Loading Performance:**
* **Package loading primary** – Brug `library(SPCify)` som default
* **Source loading sekundært** – Kun til development debugging via option
* **Golem infrastructure** – Udnyt golem's package management
* **Lazy loading** – Load tunge komponenter on-demand hvor muligt

### 3.1.1 Race Condition Prevention (OBLIGATORISK)

✅ **Hybrid Anti-Race Strategy** – Kombination af flere lag for at eliminere race conditions:

**Niveau 1: Event Architecture (Fundament)**
```r
# Centraliserede event listeners med prioritering
setup_event_listeners() {
  observeEvent(app_state$events$data_loaded,
    ignoreInit = TRUE,
    priority = OBSERVER_PRIORITIES$STATE_MANAGEMENT, {
    # Kritisk logik først
  })

  observeEvent(app_state$events$data_changed,
    ignoreInit = TRUE,
    priority = OBSERVER_PRIORITIES$DATA_PROCESSING, {
    # Data behandling sekundært
  })
}
```

**Niveau 2: State-Baseret Atomicity**
```r
# Atomiske state-opdateringer via single source of truth
safe_operation("Update visualization cache", {
  app_state$visualization$cache_updating <- TRUE
  app_state$visualization$data <- get_module_data()
  app_state$visualization$cache_updating <- FALSE
})
```

**Niveau 3: Functional Guards (Overlap Prevention)**
```r
# Guard conditions forhindrer samtidige operationer
update_column_choices_unified() {
  if (app_state$data$updating_table ||
      app_state$columns$auto_detect$in_progress ||
      app_state$ui$sync_in_progress) {
    return()  # Skip hvis anden operation kører
  }
  # ... sikker opdatering
}
```

**Niveau 4: UI Atomicity (Interface Locks)**
```r
# UI opdateringer gennem sikre wrappere
safe_programmatic_ui_update() {
  # Låser UI-opdateringer
  # Registrerer tokens for programmatiske ændringer
  # Undgår feedback-loops mellem UI og server
}
```

**Niveau 5: Input Debouncing (Noise Reduction)**
```r
# Strategisk debouncing på hyppige events
debounced_search <- shiny::debounce(
  reactive({input$search_field}),
  millis = 800  # Standard app delay
)
```

**Event Consolidation Guidelines:**

✅ **KONSOLIDER events når:**
- Events har samme logiske outcome (fx data_loaded + data_changed → visualization update)
- Status tracking på tværs af flere events
- Form state synchronization

```r
# Konsolideret pattern:
observeEvent(list(
  app_state$events$data_loaded,
  app_state$events$data_changed
), ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$DATA_PROCESSING, {
  update_visualization_cache()
})
```

❌ **BEVAR SEPARATE observers når:**
- Dependency chains skal køre i bestemt rækkefølge
- Forskellige error handling kræves
- Performance-kritiske operationer med forskellige prioriteter

**Implementering af nye features:**
1. **Emit via centraliseret event-bus** (`emit$new_feature()`)
2. **Observer i `setup_event_listeners()`** med korrekt prioritet fra `OBSERVER_PRIORITIES`
3. **Guard conditions** først for at undgå overlap
4. **Atomisk state update** gennem `safe_operation()`
5. **UI opdatering** gennem `safe_programmatic_ui_update()`
6. **Debounce** hyppige inputs med standard delays

### 3.2 R Code Quality

* **Danske kommentarer** – Beskriv funktionalitet på dansk
* **Engelske funktionsnavne** – Funktioner, variabler mv. navngives på engelsk
* **Navngivningskonvention** – snake_case for logik, camelCase for UI-komponenter
* **Type safety** – Brug `is.numeric()`, `is.character()` etc. før beregninger
* **Statisk analyse** – Kør `lintr` via `devtools::lint()` før commits

### 3.3 Error Handling Patterns

`safe_operation()` flyttet til Appendix B

---

## 4) Workflow & Integration

### 4.1 Development Lifecycle

1. **Problem definition** – Én linje der beskriver problemet
2. **Test design** – Skriv tests der dokumenterer ønsket adfærd
3. **Minimal implementation** – Implementér mindste nødvendige ændring
4. **Test verification** – Kør hele test-suiten og dokumentér resultat
5. **Integration testing** – Test full app flow (manuelt og automatisk)
6. **Commit preparation** – Ryd op, opdater dokumentation, gennemfør self-review
7. **Code review** – Inspicér diffs, valider naming/arkitektur og log-niveauer

### 4.2 Testing Strategy

* **Unit tests** – Fokus på pure functions og service-lag
* **Integration tests** – Reactive chains, event-bus og state transitions
* **Snapshot tests** – Brug `shinytest2::AppDriver` til UI-regression
* **Performance tests** – Profilér kritiske flows (`profvis`, `bench::mark`)
* **User scenario tests** – Reproducer kliniske workflows i test scripts

**Coverage-mål:**
* **100%** på kritiske stier (data load, plot generation, state sync)
* **≥90%** samlet test coverage
* **Edge cases** – Null values, tomme datasæt, fejlbehæftede uploads, store filer

### 4.3 Version Control & Deployment

* **Atomic commits** – Én logisk ændring pr. commit
* **Conventional commits (dansk)** – Se sektion 9.2 for format
* **Tests før commit** – Ingen commits uden grønt test-resultat
* **Ingen breaking changes** – Backward compatibility er default
* **Feature flags** – Brug `TEST_MODE_*` og `FEATURE_FLAG_*` i konfiguration
* **Staged rollout** – Test på separate porte (4040, 5050, 6060) før produktion
* **CI/CD** – Integrér `devtools::check()`, tests og `lintr` i pipeline

---

## 5) Configuration & Environment

### 5.1 Miljøkonfiguration med `golem::get_golem_options()`

* **Konfiguration lagres i `inst/golem-config.yml`** (eller alternativt `config/production.yml` m.fl.).
* **Opsæt defaults** i `golem-config.yml` under sektionerne `default`, `dev`, `test`, `prod`.
* **Læsning af konfiguration:**
```r
config_value <- golem::get_golem_options("test_mode_auto_load", default = FALSE)
```
* **Initialisering:** I `global.R` eller `R/app_initialization.R`, kald `golem::set_golem_options()` baseret på miljø (fx `Sys.getenv("GOLEM_CONFIG_ACTIVE")`).
* **Miljøvælger:** Brug `Sys.setenv(GOLEM_CONFIG_ACTIVE = "dev")` i udvikling og efterlad blankt i produktion (default = `prod`).

### 5.2 Standardindstillinger pr. miljø

* **DEV:**
  * `test_mode_auto_load = TRUE`
  * `auto_restore_enabled = FALSE`
  * `logging.level = "debug"`
  * `api_endpoints.mock = TRUE`
* **TEST:**
  * `test_mode_auto_load = TRUE`
  * `logging.level = "info"`
  * `ui.launch_browser = FALSE`
* **PROD:**
  * `test_mode_auto_load = FALSE`
  * `logging.level = "warn"`
  * `ui.launch_browser = TRUE`

### 5.3 Dependency- og miljøstyring

* **`renv`** – Hold projektet låst til versionsspecifikke pakker
* **`pak::pkg_install()`** – Brug deterministisk installation i CI
* **`DESCRIPTION`** – Alle runtime-dependencies skal stå i `Imports`
* **Namespace calls** – Brug `pkg::fun()` fremfor `library()` i runtime-kode
* **`Sys.getenv()`** – Alle secrets indlæses via miljøvariabler

### 5.4 Data Integrity

* **CSV format preservation** – Ingen ændring af encoding, delimiter eller BOM
* **Windows compatibility** – Tests inkluderer Windows-lignende konfigurationer
* **Unicode safety** – Bevar æ/ø/å og andre locale-tegn uændret
* **Backup før dataændringer** – Git commits eller manuelle kopier før manipulation

### 5.6 Package Loading Strategy (Golem-Based)

**Standard Production Loading** (Default):
```r
# global.R - Optimized package loading
library(SPCify)  # ~50-100ms loading time
```

**Development Source Loading** (Kun til debugging):
```r
# Kun når explicit debugging er nødvendigt
options(spc.debug.source_loading = TRUE)
# Starter source-based loading (~400ms+)
```

**Performance Requirements:**
- **Production startup**: <100ms via package loading
- **Development debugging**: 400ms+ acceptable ved source loading
- **Default behavior**: Package loading medmindre explicit source_loading option

**Implementation Pattern:**
```r
# I global.R
if (isTRUE(getOption("spc.debug.source_loading", FALSE))) {
  # Source-based loading for development debugging
  message("DEBUG: Using source-based loading")
  # ... source loading logic ...
} else {
  # Standard package loading
  message("Loading SPCify package...")
  library(SPCify)
}
```

**Miljø Konfiguration:**
- **Development**: `options(spc.debug.source_loading = FALSE)` (test package loading)
- **Debugging**: `options(spc.debug.source_loading = TRUE)` (source loading)
- **Production**: Package loading (default)

**Migration fra Source til Package Loading:**
1. Verificer alle funktioner er exported i NAMESPACE
2. Test package loading: `devtools::check()`
3. Benchmark performance improvement
4. Opdater development workflow til primært package-baseret

---


---

## 📎 Appendix A: CLI-kommandoer og testkørsel

### Test-kommandoer

```r
# Kør alle tests
R -e "source('global.R'); testthat::test_dir('tests/testthat')"

# Kør specifik test-fil
grep "^test-.*\.R$" tests/testthat -n
R -e "source('global.R'); testthat::test_file('tests/testthat/test-fase1-refactoring.R')"
```

> Disse bruges ifm. test verification (jf. 2.1) og pre-commit (jf. 7.1)


## 6) Legacy Guidelines (Preserved from original)

### 6.1 Baseline Rules

* **Ingen ændring af globale konfigurationer** uden eksplicit godkendelse
* Bevar **dansk interface** og **danske kommentarer**
* Reference commit `f05a97f` som stabil baseline

### 6.2 Architecture Boundaries (Golem-Compatible)

**File Organization** følger golem's konventioner med flad struktur i `/R/`:

* **Shiny Modules**: `mod_*.R` – Shiny modules (visualization, status etc.)
  - `mod_spc_chart.R` – SPC chart module UI og server logic
  - `mod_[feature].R` – Andre feature modules

* **Utility Functions**: `utils_*.R` – Hjælpefunktioner organiseret efter domæne
  - `utils_server_*.R` – Server-specifikke utilities
  - `utils_ui_*.R` – UI-specifikke utilities
  - `utils_performance_*.R` – Performance og caching
  - `utils_logging.R` – Logging infrastructure

* **Business Logic**: `fct_*.R` – Kerneforretningslogik
  - `fct_autodetect_unified.R` – Auto-detection logik
  - `fct_file_operations.R` – File upload/download operations
  - `fct_visualization_*.R` – Chart generation logic

* **App Infrastructure**: `app_*.R` – Core app komponenter
  - `app_ui.R` – Main UI definition
  - `app_server.R` – Main server logic
  - `app_config.R` – App configuration
  - `run_app.R` – App launcher

* **Configuration**: `config_*.R` – Setup og konfiguration
  - `config_hospital_branding.R` – Hospital-specific branding
  - `config_observer_priorities.R` – Reactive priority management
  - `config_spc_config.R` – SPC-specific configuration

* **State Management**: `state_management.R` – Centralized app state

* **Data & Tests**:
  - `/R/data/` – Eksempeldata og testfiler
  - `/tests/testthat/` – Test suites og fixtures

**Naming Convention Rules:**
- **Modules**: `mod_[feature_name].R` (ikke `modules_mod_*`)
- **Server utils**: `utils_server_[domain].R` (ikke `server_utils_*`)
- **UI utils**: `utils_ui_[domain].R` (ikke `ui_utils_*`)
- **Functions**: `fct_[domain].R` for business logic
- **Config**: `config_[area].R` for setup/configuration

### 6.3 Constraints & Forbidden Changes

* Ingen automatiske commits uden eksplicit aftale
* Ingen stor refaktorering uden godkendelse
* Ingen ændringer af `brand.yml` eller hospitalskonfiguration
* Ingen nye dependencies uden godkendelse
* Bevar eksisterende API'er medmindre opgaven kræver andet
* **ALDRIG ændre NAMESPACE filen** – R pakke namespace skal forblive uændret

---

## 7) Quality Assurance Framework

### 7.1 Pre-Commit Checklist

- [ ] **Tests kørt og bestået** – Hele test-suiten
- [ ] **Manual functionality test** – Kerneflows verificeret
- [ ] **Logging output valideret** – Strukturerede logs uden rå `cat()`
- [ ] **Error handling verificeret** – Edge cases dækket
- [ ] **Performance vurderet** – Ingen regressioner
- [ ] **Dokumentation opdateret** – README, comments, ADRs
- [ ] **Data integrity** – Ingen utilsigtede dataændringer
- [ ] **`lintr`/`styler`** – Kør `devtools::lint()` og `styler::style_file()` hvis nødvendigt
- [ ] **Package loading verificeret** – `library(SPCify)` fungerer korrekt
- [ ] **Performance benchmark** – Startup time <100ms med package loading
- [ ] **File naming conventions** – Følger golem `mod_*`, `utils_*`, `fct_*` patterns
- [ ] **NAMESPACE opdateret** – `devtools::document()` kørt hvis nye exports

### 7.2 Code Review Criteria

* **Correctness** – Logik, edge cases og reaktive afhængigheder er konsistente
* **Readability** – Selvforklarende struktur, korte funktioner, tydelige navne
* **Maintainability** – Ingen skjulte sideeffekter, solid testdækning
* **Performance** – Effektive dataoperationer, caching anvendt hvor relevant
* **Security** – Input valideret, ingen secrets i kode
* **Consistency** – Færre mønstre, mere genbrug af utils og event-bus

### 7.3 Production Readiness

* **Zero failing tests** – inkl. integration/snapshot tests
* **Performance benchmarks** – Responstid og memory under tærskler
* **Error monitoring** – `shinylogs` eller ekstern log-monitoring aktiveret
* **Rollback plan** – Dokumenteret procedure i `docs/DEPLOYMENT.md`
* **User acceptance** – Kliniske nøgleflows godkendt af fagpersoner

---

## 8) Troubleshooting & Problem Resolution

### 8.1 Debugging Methodology

1. **Reproducer** – Opret minimal reproduktion
2. **Isolér komponent** – Identificér modul/funktion
3. **Analyser logs** – Læs strukturerede log entries
4. **Test antagelser** – Verificér input og state
5. **Instrumentér** – Tilføj midlertidige `log_debug()`-kald
6. **Binary search** – Deaktiver dele for at finde fault isolation point
7. **Dokumentér** – Opdater `docs/KNOWN_ISSUES.md` eller tests

### 8.2 Common Issues & Solutions

**Reactive chain problems:**
* **Infinite loops** – Tjek cirkulære event-afhængigheder
* **Race conditions** – Følg Hybrid Anti-Race Strategy (sektion 3.1.1) med prioritering, guard conditions og event consolidation
* **State inconsistency** – Sikr at `app_state` opdateres atomisk og via events

**Performance issues:**
* **Memory leaks** – Profilér med `profvis`, ryd store objekter ved `session$onSessionEnded`
* **Slow reactives** – Debounce/throttle, cache dyre operationer
* **UI blocking** – Flyt tunge beregninger til futurobjekter eller baggrundsjobs

**Data issues:**
* **CSV parsing** – Valider delimiter/encoding via `readr::problems()`
* **Missing values** – Tilføj eksplicit NA-håndtering
* **Type conversion** – Brug `col_types` og valider efter upload

---

## 9) Kommunikation & Dokumentation

### 9.1 Udviklerkommunikation

* **Præcise action items** – "Gør X i fil Y, linje Z"
* **[MANUELT TRIN]** – Marker manuelle skridt tydeligt
* **Faktuel rapportering** – Fokus på diffs, tests og next steps
* **Problem-løsning format** – Beskriv problem, analyse, løsning, tests
* **ADR'er** – Arkitekturvalg dokumenteres i `docs/adr/` (se Appendix C for skabelon)

### 9.2 Commit Message Format (uden eksterne referencer)

```
type(scope): kort handle-orienteret beskrivelse

Fritekst med kontekst, testresultater og rationale.

- Bullet points for flere ændringer
- Referencer til issues eller ADR'er
- Breaking changes markeres eksplicit
```

**Typer:**
* `feat` – Ny funktionalitet
* `fix` – Bugfix
* `refactor` – Omstrukturering uden funktionel ændring
* `test` – Nye eller ændrede tests
* `docs` – Dokumentation
* `chore` – Vedligehold
* `perf` – Performanceforbedring
* `arch` – Arkitektoniske ændringer (file reorganization, loading strategy)

**Test-noter i commit body:**
* `Tests: R -e "source('global.R'); testthat::test_dir('tests/testthat')"`
* `Lintr: devtools::lint()`

### 9.3 Branch Protection & Workflow

✅ **Master Branch Protection** - Pre-commit hook blokerer direkte commits på master:

**Sikker udvikling workflow:**
```bash
# 1. Opret feature branch
git checkout master
git pull origin master
git checkout -b fix/feature-name

# 2. Udvikl og commit på feature branch
git add .
git commit -m "fix: beskrivelse af ændring"

# 3. Merge til master (KUN efter code review)
git checkout master
git merge fix/feature-name
git branch -d fix/feature-name  # Clean up
```

**Emergency override** (kun i kritiske situationer):
```bash
# Bypass hook hvis absolut nødvendigt (frarådes!)
git commit --no-verify -m "emergency: kritisk hotfix"
```

**Feature branch naming conventions:**
* `fix/` - Bugfixes og små rettelser
* `feat/` - Nye features
* `refactor/` - Code refaktorering
* `docs/` - Dokumentation opdateringer
* `test/` - Test forbedringer

---

## 10) Advanced Patterns & Architecture

### 10.1 State Management Patterns

```r
# Oprettes via create_app_state()
app_state <- new.env(parent = emptyenv())

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

app_state$data <- reactiveValues(
  current_data = NULL,
  original_data = NULL,
  file_info = NULL,
  updating_table = FALSE,
  table_operation_in_progress = FALSE,
  table_version = 0
)

app_state$columns <- reactiveValues(
  # Hierarchical auto-detection sub-system
  auto_detect = reactiveValues(
    in_progress = FALSE,
    completed = FALSE,
    results = NULL,
    trigger = NULL,
    last_run = NULL,
    frozen_until_next_trigger = FALSE
  ),

  # Column mappings sub-system
  mappings = reactiveValues(
    x_column = NULL,
    y_column = NULL,
    n_column = NULL,
    cl_column = NULL,
    skift_column = NULL,
    frys_column = NULL,
    kommentar_column = NULL
  ),

  # UI synchronization sub-system
  ui_sync = reactiveValues(
    needed = FALSE,
    last_sync_time = NULL,
    pending_updates = list()
  )
)

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
handle_data_upload <- function(new_data, emit) {
  safe_operation(
    operation_name = "Data upload state update",
    code = {
      app_state$data$current_data <- new_data
      app_state$data$file_info <- attr(new_data, "file_info")
      emit$data_loaded()
    }
  )
}

observeEvent(app_state$events$data_loaded, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$HIGH, {
  req(app_state$data$current_data)
  emit$auto_detection_started()
})

observeEvent(app_state$events$auto_detection_completed, ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$MEDIUM, {
  req(app_state$columns$auto_detect$results)
  emit$ui_sync_needed()
})
```

### 10.2 Performance Optimization Patterns

```r
expensive_computation <- shiny::debounce(
  reactive({
    req(app_state$data$current_data)
    calculate_complex_metrics(app_state$data$current_data)
  }),
  millis = 750
)

session$onSessionEnded(function() {
  remove_observers()
  clear_large_objects()
})
```

### 10.3 Hierarchical State Access Guidelines

**✅ Korrekte mønstre for hierarkisk state access:**
```r
# Auto-detection results
auto_columns <- app_state$columns$auto_detect$results
app_state$columns$auto_detect$completed <- TRUE

# Column mappings
app_state$columns$mappings$x_column <- "Dato"
detected_x <- app_state$columns$mappings$x_column

# UI synchronization
app_state$columns$ui_sync$needed <- TRUE
last_sync <- app_state$columns$ui_sync$last_sync_time
```

**❌ Legacy mønstre at undgå:**
```r
# Direkte field access (FORÆLDET)
auto_columns <- app_state$columns$auto_detected_columns  # Brug i stedet: auto_detect$results
app_state$columns$x_column <- "Dato"                    # Brug i stedet: mappings$x_column
```

**Migration Best Practices:**
1. **Søg systematisk** – Brug `grep -r "auto_detected_columns"` til at finde legacy usage
2. **Test først** – Opdater tests før production kode
3. **Bevar kompatibilitet** – Brug graduel migration med backward compatibility
4. **Verificér reaktive kæder** – Sikr at hierarkiske ændringer ikke bryder reactive dependencies

### 10.4 Extension Points

1. **Start med tests** – Definér forventet adfærd
2. **Implementér inkrementelt** – Små, testbare commits
3. **Følg eksisterende patterns** – Event-bus, `app_state`, logging
4. **Dokumentér** – ADR, README, inline-kommentarer hvor nødvendigt
5. **Monitorér** – Profilér og log performance-impact

---

## 11) Final Reminders

### 11.1 Legacy File Pattern Migration

**Automatisk File Renaming** (til golem conventions):
```bash
# Server utilities
mv R/server_utils_*.R R/utils_server_*[domain].R

# UI utilities
mv R/ui_utils_*.R R/utils_ui_*[domain].R

# Modules
mv R/modules_mod_*.R R/mod_*[feature].R

# Verificer alle references opdateret
grep -r "server_utils_" R/ tests/ --exclude-dir=.git
```

**Reference Update Pattern:**
```r
# Før: source("R/server_utils_event_system.R")
# Efter: # Handled by package loading

# Før: server_utils_session_helpers.R
# Efter: utils_server_session.R
```

**Validation Steps:**
1. Run `devtools::check()` efter file renaming
2. Test package loading: `library(SPCify)`
3. Verificer alle tests bestå
4. Benchmark startup performance improvement

### Development Philosophy
* **Quality over speed** – Klinisk software kræver robusthed
* **Test-driven confidence** – Tests som sikkerhedsnet ved refaktorering
* **Observability først** – Logs og metrics sikrer hurtig fejlfindingscyklus
* **User-focused design** – UX og sprog skal understøtte danske klinikere
* **Continuous improvement** – Opsaml læring i ADR'er og retrospektiver

### Project Goals
* **Stabilitet** – Systemet skal være driftsikkert
* **Maintainability** – Koden skal være forståelig og udvidbar
* **Performance** – Responsiv brugeroplevelse selv med større datasæt
* **Danish language support** – Terminologi og labels på dansk
* **Best practice compliance** – Moderne Shiny- og softwareudviklingsstandarder

---

## 12) Samtale Guidelines

### Primært Mål
Deltag i ærlig, indsigtsfuld dialog der fremmer forståelse og produktiv udvikling.

### Kerneprincipper

**Intellektuel ærlighed:**
* Del ægte indsigter uden unødvendig smiger eller afvisning
* Vær direkte omkring begrænsninger og trade-offs
* Anerkend når du ikke ved noget eller er usikker

**Kritisk engagement:**
* Stil spørgsmålstegn ved vigtige overvejelser fremfor at acceptere idéer ukritisk
* Udfordre logik, antagelser og implementeringsdetaljer når relevant
* Fokusér på argumentets substans, ikke på at være behagelig

**Balanceret evaluering:**
* Præsentér både positive og negative vurderinger kun når de er velbegrundede
* Undgå tomme komplimenter eller kritik uden substans
* Vær specifik omkring hvad der virker og hvad der ikke gør

**Retningsklarhed:**
* Fokusér på om idéer bringer os fremad eller fører os på afveje
* Vær eksplicit omkring konsekvenser og alternativer
* Prioritér projektets langsigtede kvalitet over kortsigtede løsninger

### Hvad der skal undgås

* **Smigrende svar** eller ubegrundet positivitet
* **Afvisning af idéer** uden ordentlig overvejelse
* **Overfladisk enighed** eller uenighed
* **Smiger** der ikke tjener samtalen
* **Politisk korrekthed** på bekostning af teknisk præcision

### Succeskriterium

**Den eneste valuta der betyder noget:** Fremmer dette produktiv tænkning eller standser det?

Hvis samtalen bevæger sig i en uproduktiv retning, påpeg det direkte og foreslå et bedre spor. Kvaliteten af tekniske beslutninger og kodebase-forbedringer er vigtigere end at undgå ubehag.

---

## 📎 Appendix B: Centrale util-funktioner

### `safe_operation()` – mønster for tryg fejlindkapsling
```r
safe_operation <- function(operation_name, code, fallback = NULL, session = NULL, show_user = FALSE) {
  tryCatch({
    code
  }, error = function(e) {
    log_error(
      component = "[ERROR_HANDLER]",
      message = paste(operation_name, "fejlede"),
      details = list(error_message = e$message),
      session = session,
      show_user = show_user
    )
    return(fallback)
  })
}

variable_check <- if (exists("feature_flag") && isTRUE(feature_flag) && exists("new_system")) {
  new_system$section$variable
} else {
  legacy_system$variable
}
```

---

## 13) Startup Optimization & Performance Architecture

### 13.1 Smart Boot Flow (Implementeret 2025-09-26)

✅ **Unified Boot Path** – Projektet anvender nu en intelligent boot-strategi:

**Package-Based Loading (Standard/Production):**
```r
# Automatisk package loading
source('global.R')  # Standard opførsel
```

**Source-Based Loading (Development/Debug):**
```r
# Eksplicit development mode
options(spc.debug.source_loading = TRUE)
source('global.R')
```

**Environment Variable Control:**
```bash
# Tvang source loading via environment
SPC_SOURCE_LOADING=TRUE R -e "source('global.R')"
```

### 13.2 Performance Targets & Verification

✅ **Opnået Performance (Verified 2025-09-26):**

* **Target**: Startup < 100 ms
* **Actual**: 55-57 ms (subsequent runs) ⚠️ **OVEROPFYLDT**
* **First run**: ~488 ms (acceptable for initial setup)
* **Improvement**: 60-80% hurtigere ift. legacy ~400 ms baseline

**Performance Test Command:**
```r
R --vanilla -e "source('test_startup_performance.R')"
```

### 13.3 Lazy Loading Architecture

✅ **Heavy Module Management** – System loader kun nødvendige moduler:

**Lazy Loaded Modules:**
```r
LAZY_LOADING_CONFIG <- list(
  heavy_modules = list(
    file_operations = "R/fct_file_operations.R",      # 1058 lines
    advanced_debug = "R/utils_advanced_debug.R",      # 647 lines
    performance_monitoring = "R/utils_performance.R", # 687 lines
    plot_generation = "R/fct_spc_plot_generation.R"   # 940 lines
  )
)
```

**On-Demand Loading:**
```r
# Sikr modul er loaded før brug
ensure_module_loaded("file_operations")
```

### 13.4 Startup Cache System

✅ **Static Artifact Caching** – Cache statiske data for hurtigere genstart:

**Cached Artifacts:**
* Hospital branding (colors, logos, text) - TTL: 2 timer
* Observer priorities - TTL: 1 time
* Chart types configuration - TTL: 1 time
* System config snapshot - TTL: 30 min

**Cache Operations:**
```r
# Load cached data (automatisk ved startup)
cached_data <- load_cached_startup_data()

# Manually cache current state
cache_startup_data()

# Check cache status
get_startup_cache_status()
```

### 13.5 Golem Convention Implementation

✅ **File Organization** – Standard golem file structure:

**Naming Convention (Implemented):**
```
R/
├── app_*.R           # Application core files
├── mod_*.R           # Shiny modules (previously modules_mod_*)
├── utils_server_*.R  # Server utilities (previously server_utils_*)
├── utils_ui_*.R      # UI utilities (previously ui_utils_*)
├── utils_*.R         # General utilities
├── fct_*.R           # Feature functions
└── config_*.R        # Configuration files
```

**Migration Mapping:**
```
modules_mod_spc_chart_server.R → mod_spc_chart_server.R
server_utils_event_system.R   → utils_server_event_system.R
ui_utils_ui_updates.R         → utils_ui_ui_updates.R
```

### 13.6 Environment & Configuration Management

✅ **Unified Environment Variables** – Standardiseret på GOLEM_CONFIG_ACTIVE:

**Primary Environment Variable:**
```r
# Recommended approach
Sys.setenv(GOLEM_CONFIG_ACTIVE = "development")
```

**Backward Compatibility:**
```r
# R_CONFIG_ACTIVE maps automatically to GOLEM_CONFIG_ACTIVE
Sys.setenv(R_CONFIG_ACTIVE = "development")  # Works, but not preferred
```

**Single Config Source:**
```r
# Only config::get() used for configuration loading
get_golem_config("value_name")  # Standard approach
```

### 13.7 Advanced Error Handling

✅ **Function Fallbacks** – safe_operation() nu med korrekt fallback execution:

**Improved Error Handling:**
```r
safe_operation(
  operation_name = "Data processing",
  code = { risky_operation() },
  fallback = function(e) {
    log_error(paste("Fallback triggered:", e$message), "COMPONENT")
    return(safe_default_value())
  }
)
```

**Key Improvement**: Fallback functions bliver nu **kaldt** med error parameter, ikke returneret som closure.

### 13.8 Performance Monitoring & Optimization

✅ **Continuous Performance Tracking:**

**Benchmark Approach:**
```r
# Performance verification hver gang
source('test_startup_performance.R')

# Expected results:
# ✅ Source loading: ~55-200ms
# ✅ Target: < 100ms ← OVEROPFYLDT
```

**Performance Regression Detection:**
* Automated performance tests ved hver større ændring
* Target: Behold < 100ms startup tid
* Monitoring: Lazy loading effectiveness, cache hit rates

---

## 14) Migration Guide for Startup Optimization

### 14.1 For Udviklere

**Skift til Optimized Architecture:**

1. **Boot Loading:**
   ```r
   # Old: Always source everything
   source('global.R')

   # New: Smart loading (automatic fallback)
   source('global.R')                              # Package loading attempt
   options(spc.debug.source_loading = TRUE)        # Force source loading
   ```

2. **File References:**
   ```r
   # Old file names (find and replace)
   "modules_mod_spc_chart_server.R"  → "mod_spc_chart_server.R"
   "server_utils_event_system.R"    → "utils_server_event_system.R"
   "ui_utils_ui_updates.R"          → "utils_ui_ui_updates.R"
   ```

3. **Lazy Module Usage:**
   ```r
   # Before using heavy functionality
   ensure_module_loaded("file_operations")
   # Now safe to use file operation functions
   ```

### 14.2 For Deployment

**Production Configuration:**
```r
# Environment setup
Sys.setenv(GOLEM_CONFIG_ACTIVE = "production")
Sys.setenv(SPC_LOG_LEVEL = "WARN")
Sys.setenv(SPC_SOURCE_LOADING = "FALSE")  # Explicit package loading

# Start application
source('global.R')  # Package-based loading
```

**Development Configuration:**
```r
# Development setup
Sys.setenv(GOLEM_CONFIG_ACTIVE = "development")
Sys.setenv(SPC_LOG_LEVEL = "DEBUG")
options(spc.debug.source_loading = TRUE)  # Force source loading

# Start application
source('global.R')  # Source-based loading
```

### 14.3 Performance Verification Workflow

**After Major Changes:**
1. Run performance test: `source('test_startup_performance.R')`
2. Verify < 100ms target maintained
3. Check lazy loading status: `get_lazy_loading_status()`
4. Check cache effectiveness: `get_startup_cache_status()`
5. Verify all tests pass with new architecture

**Regression Prevention:**
* Performance tests inkluderet i pre-commit workflow
* Architecture verification ved code review
* Monitoring af startup metrics over tid

---

## 📎 Appendix C: ADR-template

```markdown
# ADR-001: [Navn på beslutning]

## Status
Accepted / Proposed / Deprecated / Superseded

## Kontekst
Beskriv baggrunden for beslutningen. Hvilket problem forsøger vi at løse?

## Beslutning
Forklar hvilken arkitektonisk beslutning der blev truffet og hvorfor.

## Konsekvenser
Beskriv fordele, ulemper og evt. nødvendige ændringer fremadrettet.

## Dato
[ÅÅÅÅ-MM-DD]
```
