# Codex Instruktioner – SPC App

## 1) Projektoversigt

Dette er en **R Shiny** applikation til **Statistical Process Control (SPC)** med **qicharts2**. Appen anvendes i klinisk kvalitetsarbejde og skal forblive stabil, forståelig og på dansk.

**Udviklingsstatus:** Projektet følger industristandard mønstre for Shiny-udvikling med test-driven development, centraliseret state management, robust error handling og moden build-/deploy-automation.

---

## 2) Udviklingsprincipper (Fundamentale regler)

### 2.1 Test-First Development (TDD)

**OBLIGATORISK:** Al udvikling følger Test-Driven Development:

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

# Test-coverage verification
# Tests skal bestå før og efter hver ændring
```

### 2.2 Defensive Programming

* **Input validation** – Valider alle inputs ved entry points
* **Error handling** – Brug `safe_operation()` og eksplicit `tryCatch()` blokke
* **Scope guards** – Benyt `exists()` checks ved migrations/logiske skift
* **Graceful degradation** – Implementér fallback-mønstre hvor komponenter kan fejle
* **State consistency** – Sikr dual-state synkronisering for kompatibilitet

### 2.3 Observability & Debugging

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

**Unified Event Architecture (OBLIGATORISK for al ny udvikling):**
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

### 3.2 R Code Quality

* **Danske kommentarer** – Beskriv funktionalitet på dansk
* **Engelske funktionsnavne** – Funktioner, variabler mv. navngives på engelsk
* **Navngivningskonvention** – snake_case for logik, camelCase for UI-komponenter
* **Type safety** – Brug `is.numeric()`, `is.character()` etc. før beregninger
* **Statisk analyse** – Kør `lintr` via `devtools::lint()` før commits

### 3.3 Error Handling Patterns

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

---

## 6) Legacy Guidelines (Preserved from original)

### 6.1 Baseline Rules

* **Ingen ændring af globale konfigurationer** uden eksplicit godkendelse
* Bevar **dansk interface** og **danske kommentarer**
* Reference commit `f05a97f` som stabil baseline

### 6.2 Architecture Boundaries

* `/R/modules/` – Shiny-moduler (visualisering, status mv.)
* `/R/server/` – Server-logik
* `/R/ui/` – UI-komponenter
* `/R/data/` – Eksempeldata og testfiler
* `/tests/testthat/` – Test suites og fixtures

### 6.3 Constraints & Forbidden Changes

* Ingen automatiske commits uden eksplicit aftale
* Ingen stor refaktorering uden godkendelse
* Ingen ændringer af `brand.yml` eller hospitalskonfiguration
* Ingen nye dependencies uden godkendelse
* Bevar eksisterende API'er medmindre opgaven kræver andet

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
* **Race conditions** – Brug `priority` og `req()` guards
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
* **ADR'er** – Arkitekturvalg dokumenteres i `docs/adr/`

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

**Test-noter i commit body:**
* `Tests: R -e "source('global.R'); testthat::test_dir('tests/testthat')"`
* `Lintr: devtools::lint()`

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
  auto_detect_in_progress = FALSE,
  auto_detect_completed = FALSE,
  auto_detect_results = NULL,
  x_column = NULL,
  y_column = NULL,
  n_column = NULL,
  cl_column = NULL
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
  req(app_state$columns$auto_detect_results)
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

### 10.3 Extension Points

1. **Start med tests** – Definér forventet adfærd
2. **Implementér inkrementelt** – Små, testbare commits
3. **Følg eksisterende patterns** – Event-bus, `app_state`, logging
4. **Dokumentér** – ADR, README, inline-kommentarer hvor nødvendigt
5. **Monitorér** – Profilér og log performance-impact

---

## 11) Final Reminders

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

