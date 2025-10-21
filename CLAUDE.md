# Claude Instructions – SPCify

> ## ⚠️ BOOTSTRAP REQUIRED
>
> **Læs først:** `~/.claude/rules/CLAUDE_BOOTSTRAP_WORKFLOW.md`
>
> Denne fil instruerer hvilke globale standarder der skal læses baseret på projekttype.

---

## 1) Project Overview

- **Project Type:** Shiny Application
- **Purpose:** Statistical Process Control (SPC) applikation til klinisk kvalitetsarbejde ved Bispebjerg og Frederiksberg Hospital. Krav om stabilitet, forståelighed og dansk sprog.
- **Status:** Production (Industristandard mønstre med TDD, centraliseret state management, robust error handling)

**Technology Stack:**
- Shiny (Golem framework)
- BFHcharts (SPC visualization engine)
- BFHtheme (Hospital branding)
- qicharts2 (Anhøj rules beregning)

---

## 2) Project-Specific Architecture

### Unified Event Architecture

**SPCify bruger centraliseret event-bus:**

```r
# Events defineres i global.R
app_state$events <- reactiveValues(
  data_updated = 0L,
  auto_detection_completed = 0L,
  ui_sync_requested = 0L,
  ...
)

# Emit API
emit$data_updated(context = "upload")
emit$auto_detection_completed()

# Lyttere med prioritet
observeEvent(app_state$events$data_updated,
  ignoreInit = TRUE,
  priority = OBSERVER_PRIORITIES$HIGH, {
  handle_data_update()
})
```

**Event Infrastructure:**
- Events: `global.R` (`app_state$events`)
- Emit functions: `create_emit_api()`
- Listeners: `R/utils_event_system.R` via `setup_event_listeners()`

### App State Structure

**Hierarchisk state (se `R/state_management.R`):**

```r
app_state$events         # Event triggers
app_state$data           # current_data, original_data, file_info
app_state$columns        # Hierarkisk: auto_detect, mappings, ui_sync
app_state$session        # Session state
```

**Detaljeret schema:** Se Appendix i original CLAUDE.md.backup

### Golem Configuration

**Environment-specific settings:**

```r
# Læsning
config_value <- golem::get_golem_options("test_mode_auto_load", default = FALSE)

# Initialisering
Sys.setenv(GOLEM_CONFIG_ACTIVE = "dev")  # dev/test/prod
```

**Standard environments:**
- **DEV:** `test_mode_auto_load = TRUE`, `logging.level = "debug"`
- **TEST:** `test_mode_auto_load = TRUE`, `logging.level = "info"`
- **PROD:** `test_mode_auto_load = FALSE`, `logging.level = "warn"`

### Performance Architecture

**Boot strategy:**
- Production: `library(SPCify)` (~50-100ms)
- Debug: `source('global.R')` med `options(spc.debug.source_loading = TRUE)` (~400ms+)

**Lazy loading:** Tunge moduler (file_operations, advanced_debug, performance_monitoring) loaded on demand

**Target:** Startup < 100ms (achieved: 55-57ms)

---

## 3) Critical Project Constraints

### External Package Ownership

✅ **KRITISK:** Maintainer har fuld kontrol over:

- **BFHcharts** – SPC chart rendering og visualisering
- **BFHtheme** – Hospital branding, themes og fonts

❌ **ALDRIG implementer funktionalitet i SPCify som hører hjemme i BFHcharts eller BFHtheme**

✅ **I STEDET:**
1. Identificer manglende funktionalitet i ekstern pakke
2. Dokumentér behovet (issue, ADR, eller docs/)
3. Informér maintainer om feature request
4. Implementér midlertidig workaround i SPCify HVIS kritisk (marker tydeligt som temporary)
5. Fjern workaround når funktionalitet er tilgængelig i ekstern pakke

**Eksempler:**
- Target line rendering → BFHcharts ansvar
- Font fallback logic → BFHtheme ansvar
- Hospital branding colors → BFHtheme ansvar
- Chart styling defaults → BFHcharts ansvar

### Integration Pattern

- SPCify: **Integration layer + business logic**
- BFHcharts: **Visualization engine**
- BFHtheme: **Styling framework**

### Do NOT Modify

- `brand.yml` uden godkendelse
- **NAMESPACE** uden explicit godkendelse (brug `devtools::document()`)
- Breaking changes uden major version bump

---

## 4) Cross-Repository Coordination

### BFHcharts + qicharts2 Hybrid Architecture

✅ **KRITISK:** SPCify bruger **permanent hybrid arkitektur**:

| Komponent | Ansvar | Package | Rationale |
|-----------|--------|---------|-----------|
| **SPC Plotting** | Chart rendering, visual theming | BFHcharts | Modern ggplot2 med BFH branding |
| **Anhøj Rules** | Serielængde, antal kryds, special cause detection | qicharts2 | Valideret, klinisk accepteret |

**Implementation:**

```r
# BFHcharts: Primary plotting
plot <- BFHcharts::create_spc_chart(data, x, y, chart_type, notes_column, ...)

# qicharts2: Anhøj rules metadata (UI value boxes)
qic_result <- qicharts2::qic(x, y, chart = chart_type, return.data = TRUE)
anhoej_metadata <- extract_anhoej_metadata(qic_result)
```

**Constraints:**

❌ **qicharts2 KUN til:** Anhøj rules, metadata extraction
✅ **BFHcharts til:** Plot rendering, chart types, theming, notes, target lines, freezing

**Files involved:**
- `R/fct_spc_bfh_service.R` - BFHcharts service + qicharts2 Anhøj rules
- `R/utils_qic_preparation.R` - qicharts2 input prep
- `R/utils_qic_caching.R` - Anhøj rules caching
- `R/utils_qic_debug_logging.R` - qicharts2 debug logging

### Coordination Workflow

**Primær guide:** `docs/CROSS_REPO_COORDINATION.md`

**Quick references:**
- `.claude/ISSUE_ESCALATION_DECISION_TREE.md` - Beslutningsdiagram
- `.github/ISSUE_TEMPLATE/bfhchart-feature-request.md` - Issue template

**Eskalér til BFHcharts hvis:**
- Core chart rendering bugs
- Statistiske beregningsfejl
- Manglende chart types eller features
- BFHcharts API design limitations
- Performance issues i BFHcharts algoritmer

**Fix i SPCify hvis:**
- Parameter mapping (qicharts2 → BFHcharts)
- UI integration og Shiny reaktivitet
- Data preprocessing og validering
- Fejlbeskeder og dansk lokalisering
- SPCify-specifik caching

---

## 5) Project-Specific Configuration

### Configuration Files Overview

| Fil | Ansvar |
|-----|--------|
| `config_branding_getters.R` | Hospital branding (navn, logo, theme, farver) |
| `config_chart_types.R` | SPC chart type definitions (DA→EN mappings) |
| `config_observer_priorities.R` | Observer priorities (race condition prevention) |
| `config_spc_config.R` | SPC-specifikke konstanter (validation, colors) |
| `config_log_contexts.R` | Centraliserede log context strings |
| `config_label_placement.R` | Intelligent label placement (collision avoidance) |
| `config_system_config.R` | System constants (performance, timeouts, cache) |
| `config_ui.R` | UI layout (widths, heights, font scaling) |
| `inst/golem-config.yml` | Environment-based config (dev/prod/test) |

**Detaljeret guide:** `docs/CONFIGURATION.md`

### Test Commands

```r
# Alle tests
R -e "library(SPCify); testthat::test_dir('tests/testthat')"

# Specifik test
R -e "source('global.R'); testthat::test_file('tests/testthat/test-*.R')"

# Performance benchmark
R -e "microbenchmark::microbenchmark(package = library(SPCify), source = source('global.R'), times = 5)"
```

**Coverage targets:**
- 100% kritiske paths (data load, plot generation, state sync)
- ≥90% samlet coverage
- Edge cases (null, tomme datasæt, fejl, store filer)

---

## 6) Domain-Specific Guidance

### Issue Tracking (GitHub Issues)

✅ **OBLIGATORISK:** Alle fejl, rettelser, todo-emner og forbedringsforslag dokumenteres som GitHub Issues.

```bash
gh issue create --title "Beskrivelse" --body "Details"
git commit -m "fix: beskrivelse (fixes #123)"
```

**Labels:** `bug`, `enhancement`, `documentation`, `technical-debt`, `performance`, `testing`

### Gemini CLI for Large Codebase Analysis

**Brug `gemini -p` når:**
- Analysere hele Shiny-kodebase på tværs af mange filer
- Forstå sammenhæng mellem moduler, reaktive kæder
- Finde duplikerede mønstre eller anti-patterns
- Verificere arkitektur på tværs af hele projektet

**Eksempler:**

```bash
# Arkitektur verification
gemini -p "@R/ Analyze current state management patterns and identify areas for centralization"

# Test coverage check
gemini -p "@tests/ @R/ Are all critical paths covered by tests?"
```

**Integration med SPCify workflow:**
1. Arkitektur verification før større refaktorering
2. Code review på tværs af moduler
3. Pattern detection for inconsistencies
4. Dependency analysis før nye features
5. Test coverage gaps identifikation

### Danish Language

- **UI text:** Dansk
- **Error messages:** Dansk, brugervenlige
- **Code:** Engelsk (funktionsnavne, variabler)
- **Comments:** Dansk

**Key terms:**
- Serieplot = SPC chart
- Centrallinje = Center line
- Kontrolgrænser = Control limits

---

## 📚 Global Standards Reference

**Dette projekt følger:**
- **R Development:** `~/.claude/rules/R_STANDARDS.md`
- **Shiny Development:** `~/.claude/rules/SHINY_STANDARDS.md`
- **Shiny Advanced Patterns:** `~/.claude/rules/SHINY_ADVANCED_PATTERNS.md`
- **Git Workflow:** `~/.claude/rules/GIT_WORKFLOW.md`
- **Development Philosophy:** `~/.claude/rules/DEVELOPMENT_PHILOSOPHY.md`
- **Architecture Patterns:** `~/.claude/rules/ARCHITECTURE_PATTERNS.md`
- **Troubleshooting:** `~/.claude/rules/TROUBLESHOOTING_GUIDE.md`

**Globale agents:** tidyverse-code-reviewer, performance-optimizer, security-reviewer, test-coverage-analyzer, refactoring-advisor, legacy-code-detector, shiny-code-reviewer, architecture-validator

**Globale commands:** /boost, /code-review-recent, /double-check, /debugger

---

**Original documentation:** Se `CLAUDE.md.backup` for fuld dokumentation af alle patterns, appendices og detaljeret arkitektur.
