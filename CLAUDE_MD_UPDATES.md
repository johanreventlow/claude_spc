# CLAUDE.md Updates - Golem-Compatible Architecture

## **INSTRUKTIONER TIL LLM**

Denne fil indeholder specifikke ændringer der skal implementeres i `CLAUDE.md` for at reflektere golem-konventioner og løse arkitektoniske inkonsistenser identificeret i architecture review.

---

## **SEKTION 6.2: Architecture Boundaries (KRITISK OPDATERING)**

### **FJERN NUVÆRENDE TEKST:**
```markdown
### 6.2 Architecture Boundaries

* `/R/modules/` – Shiny-moduler (visualisering, status mv.)
* `/R/server/` – Server-logik
* `/R/ui/` – UI-komponenter
* `/R/data/` – Eksempeldata og testfiler
* `/tests/testthat/` – Test suites og fixtures
```

### **ERSTAT MED:**
```markdown
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
```

---

## **NY SEKTION 5.6: Package Loading Strategy**

### **TILFØJ EFTER SEKTION 5.5:**

```markdown
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
```

---

## **SEKTION 3.1: Shiny Best Practices (OPDATERING)**

### **TILFØJ TIL EKSISTERENDE "Reactive Programming Patterns":**

```markdown
**File Loading Performance:**
* **Package loading primary** – Brug `library(SPCify)` som default
* **Source loading sekundært** – Kun til development debugging via option
* **Golem infrastructure** – Udnyt golem's package management
* **Lazy loading** – Load tunge komponenter on-demand hvor muligt
```

---

## **SEKTION 2.1: Test-First Development (OPDATERING)**

### **TILFØJ TIL "Test-kommandoer":**

```r
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
```

---

## **SEKTION 7.1: Pre-Commit Checklist (OPDATERING)**

### **TILFØJ TIL EKSISTERENDE CHECKLIST:**

```markdown
- [ ] **Package loading verificeret** – `library(SPCify)` fungerer korrekt
- [ ] **Performance benchmark** – Startup time <100ms med package loading
- [ ] **File naming conventions** – Følger golem `mod_*`, `utils_*`, `fct_*` patterns
- [ ] **NAMESPACE opdateret** – `devtools::document()` kørt hvis nye exports
```

---

## **SEKTION 9.2: Commit Message Format (TILFØJELSE)**

### **TILFØJ NY COMMIT TYPE:**

```markdown
**Typer:**
* `feat` – Ny funktionalitet
* `fix` – Bugfix
* `refactor` – Omstrukturering uden funktionel ændring
* `test` – Nye eller ændrede tests
* `docs` – Dokumentation
* `chore` – Vedligehold
* `perf` – Performanceforbedring
* `arch` – Arkitektoniske ændringer (file reorganization, loading strategy)
```

---

## **NY SEKTION 11.1: Migration Guidelines**

### **TILFØJ NY SEKTION EFTER SEKTION 11:**

```markdown
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
```

---

## **IMPLEMENTERING INSTRUKTIONER FOR LLM**

**Når du implementerer disse ændringer:**

1. **BACKUP eksisterende CLAUDE.md** først
2. **Implementer ændringerne sektion for sektion** som specificeret ovenfor
3. **Test at alle file references** stadig fungerer efter navnændringer
4. **Verificer package loading** virker med `library(SPCify)`
5. **Kør full test suite** efter hver større ændring
6. **Benchmark performance** før og efter implementation
7. **Opdater eventuelle andre docs** der refererer til gamle file patterns

**Kritiske punkter:**
- Golem bruger **flad struktur** i `/R/` - ikke subdirectories
- **Package loading** skal være default, source loading kun til debugging
- **File naming conventions** skal følge golem standards
- **Performance krav**: <100ms startup tid

**Validation:**
Efter implementation skal følgende kommandoer fungere fejlfrit:
```bash
R -e "library(SPCify); testthat::test_dir('tests/testthat')"
R -e "devtools::check()"
```