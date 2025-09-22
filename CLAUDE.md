# Claude Instruktioner – SPC App

## 1) Projektoversigt

Dette er en **R Shiny** applikation til **Statistical Process Control (SPC)** med **qicharts2**. Appen anvendes i klinisk kvalitetsarbejde og skal forblive stabil, forståelig og på dansk.

**Udviklingsstatus:** Projektet følger industristandard mønstre for Shiny-udvikling med test-driven development, centraliseret state management, robust error handling og moden build-/deploy-automation.

---

## 2) Udviklingsprincipper (Fundamentale regler)

### 2.0 Obligatorisk Commit-Workflow og Godkendelsesprotokol (KRITISK)

🚨 **ABSOLUT FORBUD MOD UKONTROLLEREDE KODEÆNDRINGER:**

✅ **ALDRIG ret kode uden eksplicit godkendelse og commit-plan**
✅ **ALTID commit efter hvert logisk trin**
✅ **ALDRIG foretag multiple ændringer uden mellemliggende commits**

**Obligatorisk procedure:**
1. **Spørg om hel implementeringsplan** – Få eksplicit godkendelse til hele opgaven og dens logiske trin
2. **Implementér et logisk trin ad gangen** – Et trin kan omfatte flere relaterede filer hvis de hører sammen
3. **Commit efter hvert trin** – Hvert logisk trin skal have sin egen commit
4. **Bekræft inden næste trin** – Spørg kun om lov til næste trin hvis det afviger væsentligt fra den godkendte plan
5. **Automatisk quality checks** – Git hooks kører automatisk lintr/styler
6. **Test mellem commits** – Kør tests efter hver commit
7. **Dokumentér ændringer** – Klar commit message med rationale

**Definition af "logisk trin":**
- En komplet feature implementation (fx "tilføj auto-detection for dato-kolonner")
- En funktionel refactoring (fx "konverter alle for-loops til purrr i autodetect-modulet")
- En bug fix med alle relaterede test-opdateringer
- Dependency opdateringer med tilhørende kode-ændringer

**Eksempel på korrekt workflow:**
```bash
# Trin 1: Spørg om hele planen
"Jeg vil implementere tidyverse konvertering i 3 logiske trin:
1. Tilføj dependencies + konverter fct_autodetect_unified.R (2 filer)
2. Konverter server_utils_column_management.R (1 fil)
3. Opdater tests og dokumentation (3 filer)
Må jeg fortsætte med denne plan?"

# Trin 2: Implementér første logiske trin
[Implementerer DESCRIPTION + fct_autodetect_unified.R]
git commit -m "feat: tilføj tidyverse deps og konverter autodetect til purrr"

# Trin 3: Fortsæt til næste trin (automatisk fra godkendt plan)
[Implementerer server_utils_column_management.R]
git commit -m "refactor: konverter column management til tidyverse"

# Trin 4: Afslut med tests (automatisk fra godkendt plan)
[Opdaterer tests + dokumentation]
git commit -m "test: opdater tests for tidyverse konvertering"
```

**Hvornår spørge igen:**
✅ **Spørg kun igen hvis:** Trinnet viser sig mere komplekst end ventet, nye problemer opdages, eller implementeringen skal afvige fra den godkendte plan
❌ **Spørg ikke igen hvis:** Du følger den godkendte plan og trinnet er som forventet

**ALDRIG gør:**
❌ Rette flere filer på samme tid uden commits imellem
❌ Starte kodeændringer uden eksplicit brugerregodkendelse
❌ Skip commits "fordi det bare er små ændringer"
❌ Fortsætte hvis tests fejler efter en commit

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

## 3) Code Quality & Formatting (OBLIGATORISK)

### 3.0 Automatisk Code Quality via Git Hooks

🔄 **AUTOMATISK SETUP**: Git hooks er allerede installeret og kører ved hver commit.

**Hvad sker der automatisk:**
```bash
git commit -m "din besked"
# → Git hook kører automatisk
# → Tjekker kun ændrede R filer
# → Kører lintr (code quality) + styler (formatting)
# → Blokerer commit hvis kritiske errors
# → Formaterer kode automatisk hvis nødvendigt
```

**Manuel kørsel:**
```bash
# Kør lintr + styler på alle filer
Rscript dev/lint_and_style.R

# Alternative: Pre-commit framework (valgfrit)
pip install pre-commit && ./dev/setup-precommit.sh
pre-commit run --all-files
```

**Exit codes og håndtering:**
- `Exit 0`: Alt OK → Commit tilladt
- `Exit 1`: Kritiske errors → **Commit blokeret**
- `Exit 2`: Warnings → Commit tilladt med advarsel

**Hvis styler ændrer filer:**
```bash
# Hook siger: "Filer blev ændret, stage dem igen"
git add .
git commit -m "din besked"  # Prøv igen
```

### 3.1 Code Quality Konfiguration

**Lintr konfiguration (`.lintr`):**
- Line length: 120 tegn
- Tillader danske kommentarer og funktionsnavne
- Tjekker library() calls (foretrækker `pkg::function()`)
- Undtager `golem_utils.R`, `dev/` mapper

**Styler konfiguration:**
- Følger tidyverse style guide
- Bevarer danske kommentarer
- 2-space indentation
- Automatisk spacing omkring operators

**Undtagelser:**
- `R/golem_utils.R` – Golem genereret kode
- `dev/` – Development scripts
- `tests/` – Mere afslappede regler

### 3.2 Fejlhåndtering og Troubleshooting

**Typiske scenarier:**
```bash
# Scenario 1: Kritiske lintr errors
❌ FEJL: Kritiske lintr errors fundet - skal rettes!
# → Fix errors manuelt og commit igen

# Scenario 2: Styler formaterede filer
📝 Styler har ændret 3 filer - husk at stage dem!
git add .
git commit

# Scenario 3: Kun warnings
⚠️ Warnings fundet, men commit tilladt
# → Commit går igennem, overvej at fixe warnings
```

**Dokumentation:**
- Komplet guide: `dev/README-code-quality.md`
- Setup scripts: `dev/setup-precommit.sh`
- Backup hook: `dev/pre-commit-hook.sh`

---

## 4) Tekniske Best Practices

### 4.1 Shiny Best Practices

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

### 4.2 R Code Quality (Automatisk Enforced)

✅ **AUTOMATISK HÅNDHÆVET VIA GIT HOOKS:**

* **Code formatting** – Styler formaterer automatisk efter tidyverse style guide
* **Code linting** – Lintr tjekker potentielle problemer og style violations
* **Line length** – Max 120 tegn (håndhævet automatisk)
* **Assignment operators** – `<-` foretrækkes over `=` (håndhævet automatisk)
* **Function calls** – `pkg::function()` foretrækkes over `library()` (håndhævet automatisk)

**Manuel standards (ikke automatisk håndhævet):**
* **Danske kommentarer** – Beskriv funktionalitet på dansk
* **Engelske funktionsnavne** – Funktioner, variabler mv. navngives på engelsk
* **Navngivningskonvention** – snake_case for logik, camelCase for UI-komponenter
* **Type safety** – Brug `is.numeric()`, `is.character()` etc. før beregninger

**Ved commit fejl:**
```bash
# Hvis lintr finder kritiske errors:
❌ FEJL: Kritiske lintr errors fundet - skal rettes!
# Fix errors og commit igen

# Hvis styler ændrer filer:
📝 Styler har ændret filer - stage dem igen:
git add .
git commit
```

### 4.3 Error Handling Patterns

`safe_operation()` flyttet til Appendix B

---

## 5) Workflow & Integration

### 5.1 Development Lifecycle

1. **Problem definition** – Én linje der beskriver problemet
2. **Test design** – Skriv tests der dokumenterer ønsket adfærd
3. **Minimal implementation** – Implementér mindste nødvendige ændring
4. **Code quality check** – Automatisk via git hooks (`Rscript dev/lint_and_style.R`)
5. **Test verification** – Kør hele test-suiten og dokumentér resultat
6. **Integration testing** – Test full app flow (manuelt og automatisk)
7. **Commit preparation** – Ryd op, opdater dokumentation, gennemfør self-review
8. **Code review** – Inspicér diffs, valider naming/arkitektur og log-niveauer

### 5.2 Testing Strategy

* **Unit tests** – Fokus på pure functions og service-lag
* **Integration tests** – Reactive chains, event-bus og state transitions
* **Snapshot tests** – Brug `shinytest2::AppDriver` til UI-regression
* **Performance tests** – Profilér kritiske flows (`profvis`, `bench::mark`)
* **User scenario tests** – Reproducer kliniske workflows i test scripts

**Coverage-mål:**
* **100%** på kritiske stier (data load, plot generation, state sync)
* **≥90%** samlet test coverage
* **Edge cases** – Null values, tomme datasæt, fejlbehæftede uploads, store filer

### 5.3 Version Control & Deployment

* **Atomic commits** – Én logisk ændring pr. commit
* **Conventional commits (dansk)** – Se sektion 9.2 for format
* **Automatisk code quality** – Git hooks kører lintr/styler ved hver commit
* **Tests før commit** – Ingen commits uden grønt test-resultat OG code quality OK
* **Ingen breaking changes** – Backward compatibility er default
* **Feature flags** – Brug `TEST_MODE_*` og `FEATURE_FLAG_*` i konfiguration
* **Staged rollout** – Test på separate porte (4040, 5050, 6060) før produktion
* **CI/CD** – Integrér `devtools::check()`, tests og code quality pipeline

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
* **Namespace calls** – Brug `pkg::fun()` fremfor `library()` (håndhævet automatisk af lintr)
* **`Sys.getenv()`** – Alle secrets indlæses via miljøvariabler

**Code Quality Dependencies (påkrævet for udvikling):**
* **`lintr`** – Code quality checking (påkrævet for git hooks)
* **`styler`** – Automatisk code formatering (påkrævet for git hooks)
* **`here`** – Path management i development scripts
* **Installation:** `install.packages(c("lintr", "styler", "here"))`

### 5.4 Data Integrity

* **CSV format preservation** – Ingen ændring af encoding, delimiter eller BOM
* **Windows compatibility** – Tests inkluderer Windows-lignende konfigurationer
* **Unicode safety** – Bevar æ/ø/å og andre locale-tegn uændret
* **Backup før dataændringer** – Git commits eller manuelle kopier før manipulation

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

### 6.2 Architecture Boundaries

**R/ directory structure (flat organization med prefix-baseret gruppering):**

* `app_*.R` – App initialization, configuration og main server logic
* `config_*.R` – Konfigurationsfiler (chart types, branding, system config)
* `core_*.R` – Centrale algoritmer og hjælpefunktioner
* `fct_*.R` – Business logic funktioner (autodetect, file operations, SPC plot generation)
* `modules_*.R` – Shiny moduler (UI og server components)
* `server_*.R` – Server utilities og management funktioner
* `ui_*.R` – UI components, helpers og update logic
* `utils_*.R` – Utility funktioner (debugging, error handling, performance)
* `state_management.R` – Centraliseret state management
* `logging.R` – Logging system
* `run_app.R` – App launcher
* `golem_utils.R` – Golem framework utilities

**Data og tests:**
* `/R/data/` – Eksempeldata og testfiler (CSV, Excel)
* `/tests/testthat/` – Test suites og fixtures

**Naming conventions:**
- Funktioner: snake_case (engelsk)
- Kommentarer: dansk
- Fil-præfikser angiver funktionel gruppering

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
- [ ] **Code quality checks** – Automatisk via git hooks eller manuel: `Rscript dev/lint_and_style.R`
- [ ] **Manual functionality test** – Kerneflows verificeret
- [ ] **Logging output valideret** – Strukturerede logs uden rå `cat()`
- [ ] **Error handling verificeret** – Edge cases dækket
- [ ] **Performance vurderet** – Ingen regressioner
- [ ] **Dokumentation opdateret** – README, comments, ADRs
- [ ] **Data integrity** – Ingen utilsigtede dataændringer

**Automatisk Code Quality (via git hooks):**
- ✅ **Lintr** – Code style og potentielle problemer
- ✅ **Styler** – Automatisk formatering efter tidyverse style
- ✅ **Commit blocking** – Kritiske errors forhindrer commits
- ✅ **Re-staging** – Automatisk formaterede filer skal stages igen

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
* `style` – Code formatting (kun hvis manuel styling nødvendig)

**Automatisk validering ved commit:**
* ✅ **Code quality** – Git hooks kører automatisk lintr/styler
* ✅ **Formatering** – Automatisk styling efter tidyverse guide
* ✅ **Linting** – Potentielle problemer fanges før commit

**Test-noter i commit body (hvis relevante):**
* `Tests: R -e "source('global.R'); testthat::test_dir('tests/testthat')"`
* `Manual code quality: Rscript dev/lint_and_style.R` (kun hvis hooks disabled)

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

### STOP-AND-ASK Protokol (OBLIGATORISK)

🛑 **Claude SKAL altid stoppe og spørge før kodeændringer:**

**Før enhver fil-ændring:**
1. **Beskriv præcist** hvilke filer der skal ændres
2. **Forklar rationale** for hver ændring
3. **Angiv commit-plan** - hvor mange commits og hvad hver indeholder
4. **Vent på eksplicit godkendelse** fra bruger
5. **Implementér kun ét trin ad gangen** med commit mellem hvert

**Eksempel på korrekt stop-and-ask:**
```
"Jeg har identificeret følgende områder til tidyverse konvertering:

1. DESCRIPTION fil: Tilføj dplyr, tidyr, purrr dependencies
2. fct_autodetect_unified.R: Konverter 3 for-loops til purrr::map()
3. server_utils_column_management.R: Erstat sapply() med map_lgl()

Dette vil resultere i 3 separate commits.
Git hooks vil automatisk køre lintr/styler ved hver commit.
Skal jeg fortsætte?"

[Venter på bruger svar før implementering]
```

**ALDRIG gør:**
❌ Implementér ændringer uden at spørge først
❌ Antag at "ja til projektet" betyder "ja til alle ændringer"
❌ Fortsæt med næste fil uden at vente på bekræftelse

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
