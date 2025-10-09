# Configuration Guide

## Oversigt

Dette dokument giver en komplet oversigt over alle configuration-filer i SPCify-projektet. Configuration er opdelt i modulære filer efter domæne for at følge Single Responsibility Principle og R package best practices.

**Total antal config-filer:** 9 (8 R-filer + 1 YAML)

---

## Quick Reference Tabel

| Fil | Ansvar | Typiske Use Cases | Linjer |
|-----|--------|-------------------|--------|
| **config_branding_getters.R** | Hospital branding (navn, logo, theme, farver) | Tilføj nyt hospital, ændre farver, custom theme | ~251 |
| **config_chart_types.R** | SPC chart type definitions og DA→EN mappings | Tilføj ny chart type, opdater beskrivelser | ~85 |
| **config_observer_priorities.R** | Observer execution priorities (race condition prevention) | Juster observer rækkefølge, debugging | ~56 |
| **config_spc_config.R** | SPC-specifikke konstanter (validation, visualization) | Ændre SPC defaults, tilføj nye enheder | ~115 |
| **config_log_contexts.R** | Centraliserede log context strings | Tilføj ny logging kategori, rename contexts | ~258 |
| **config_label_placement.R** | Intelligent label placement system (plot labels) | Tune label spacing, collision detection | ~226 |
| **config_system_config.R** | System-level constants (performance, timeouts, rate limits) | Juster debounce delays, cache timeouts | ~173 |
| **config_ui.R** | UI layout konstanter (widths, heights, styles, fonts) | Ændre UI spacing, font scaling | ~99 |
| **inst/golem-config.yml** | Environment-based config (dev/prod/test) | Miljø-specifikke settings, feature flags | ~295 |

---

## Detaljeret Beskrivelse

### 1. config_branding_getters.R

**Formål:** Centraliseret branding configuration med safe access via getter-funktioner.

**Indeholder:**
- `get_hospital_name()` - Hospital navn fra brand.yml
- `get_hospital_logo_path()` - Logo path
- `get_bootstrap_theme()` - bslib theme object
- `get_hospital_colors()` - Farve palette
- `get_hospital_ggplot_theme()` - ggplot2 theme function
- `create_plot_footer()` - Plot footer med hospital info

**Anvendes af:**
- UI rendering (logo, theme)
- Plot generation (farver, footer)
- Branding initialization

**Relateret:**
- `inst/config/brand.yml` - Brand configuration source
- `config_spc_config.R` - SPC visualization farver

**Typiske ændringer:**
```r
# Tilføj nyt hospital:
# 1. Opdater inst/config/brand.yml
# 2. Getter-funktioner loader automatisk

# Ændre farvepalette:
# Rediger brand.yml color.palette section
```

---

### 2. config_chart_types.R

**Formål:** SPC chart type definitions og mappings mellem danske labels og engelske qicharts2-koder.

**Indeholder:**
- `CHART_TYPES_DA` - Dansk-til-engelsk chart type mapping
- `CHART_TYPES_EN` - Engelsk chart type liste
- `get_qic_chart_type()` - Konverter dansk label → engelsk kode
- `chart_type_requires_denominator()` - Logik for nævner-relevans
- `CHART_TYPE_DESCRIPTIONS` - Beskrivelser til UI tooltips

**Anvendes af:**
- UI dropdowns (chart type selection)
- Plot generation (qicharts2 interface)
- Input validation

**Relateret:**
- `config_spc_config.R` - Y-akse enheder
- `fct_spc_plot_generation.R` - Plot rendering

**Typiske ændringer:**
```r
# Tilføj ny chart type:
CHART_TYPES_DA <- list(
  ...,
  "Nyt diagram (X-kort)" = "x"
)

CHART_TYPE_DESCRIPTIONS <- list(
  ...,
  "x" = "Beskrivelse af X-kort"
)
```

---

### 3. config_observer_priorities.R

**Formål:** Prioriterede observer execution order til at forhindre race conditions.

**Indeholder:**
- `OBSERVER_PRIORITIES` - Named list med priority levels
  - `STATE_MANAGEMENT = 2000` (highest)
  - `AUTO_DETECT = 1500`
  - `DATA_PROCESSING = 1250`
  - `UI_SYNC = 750`
  - `PLOT_GENERATION = 600`
  - `STATUS_UPDATES = 500`
  - `CLEANUP = 200`
  - `LOGGING = 100` (lowest)
- Convenience functions: `get_priority()`, `priority_high()`, etc.

**Anvendes af:**
- Alle `observeEvent()` calls med `priority = OBSERVER_PRIORITIES$...`
- Race condition prevention system
- Event-bus listeners

**Relateret:**
- `utils_event_system.R` - Event listeners
- CLAUDE.md Section 3.1.1 - Race Condition Prevention

**Typiske ændringer:**
```r
# Juster priorities hvis ny race condition opdages:
observeEvent(app_state$events$new_event,
  priority = OBSERVER_PRIORITIES$HIGH,  # Sæt korrekt priority
  { ... }
)
```

---

### 4. config_spc_config.R

**Formål:** SPC-specifikke konstanter for data validation og visualization.

**Indeholder:**
- **Data validation:**
  - `MIN_SPC_ROWS` - Minimum rækker for SPC (10)
  - `RECOMMENDED_SPC_POINTS` - Anbefalet minimum (20)
  - `MAX_MISSING_PERCENT` - Max missing values (20%)
  - `MIN_NUMERIC_PERCENT` - Min numeriske værdier (80%)

- **Column detection:**
  - `SPC_COLUMN_NAMES` - Standard kolonnenavne (x, y, n, cl, freeze, shift, comment)

- **Y-axis units:**
  - `Y_AXIS_UNITS_DA` - Dansk-til-engelsk enhed mapping
  - `Y_AXIS_UI_TYPES_DA` - Simplified UI valg

- **Visualization:**
  - `SPC_COLORS` - Farve palette (target_line, control_line, normal_point, etc.)
  - `SPC_ALPHA_VALUES` - Gennemsigtighed
  - `SPC_LINE_TYPES` - Linje typer
  - `SPC_LINE_WIDTHS` - Linje bredder

**Anvendes af:**
- Data validation logic
- Auto-detection algoritmer
- Plot rendering (colors, line styles)
- Y-axis formatting

**Relateret:**
- `config_chart_types.R` - Chart type logic
- `utils_auto_detect.R` - Column detection
- `fct_spc_plot_generation.R` - Visualization

**Typiske ændringer:**
```r
# Tilføj ny Y-akse enhed:
Y_AXIS_UNITS_DA <- list(
  ...,
  "Ny enhed" = "new_unit"
)

# Ændre SPC farver:
SPC_COLORS <- list(
  ...,
  target_line = "#NEW_COLOR"
)
```

---

### 5. config_log_contexts.R

**Formål:** Centraliserede log context strings for struktureret logging gennem hele applikationen.

**Indeholder:**
- `LOG_CONTEXTS` - Hierarchical list med 16 kategorier:
  - `data` - Data processing
  - `autodetect` - Auto-detection
  - `performance` - Performance monitoring
  - `qic` - QIC/SPC calculations
  - `ui` - UI & visualization
  - `column` - Column management
  - `app` - App lifecycle
  - `navigation` - Navigation
  - `test` - Test mode
  - `file` - File operations
  - `security` - Security
  - `config` - Configuration
  - `startup` - Startup & initialization
  - `cache` - Cache management
  - `debug` - Debug & development
  - `misc` - Miscellaneous

- Helper functions:
  - `get_log_context(category, context)` - Get specific context with validation
  - `list_all_log_contexts()` - Get all available contexts

**Anvendes af:**
- Alle logging calls: `log_debug()`, `log_info()`, `log_warn()`, `log_error()`
- Struktureret fejlfinding
- Log filtering og analyse

**Relateret:**
- `utils_logging.R` - Logging system
- CLAUDE.md Section 2.4 - Observability & Debugging

**Typiske ændringer:**
```r
# Tilføj ny log context:
LOG_CONTEXTS <- list(
  ...,
  new_category = list(
    context1 = "NEW_CONTEXT_1",
    context2 = "NEW_CONTEXT_2"
  )
)

# Brug i kode:
log_debug("Message", .context = LOG_CONTEXTS$new_category$context1)
```

---

### 6. config_label_placement.R

**Formål:** Intelligent label placement configuration for SPC plots med collision avoidance.

**Indeholder:**
- `LABEL_PLACEMENT_CONFIG` - Empirisk tunede konstanter:
  - **Gap configuration:**
    - `relative_gap_line = 0.05` (5% af label højde)
    - `relative_gap_labels = 0.30` (30% af label højde)
  - **Panel padding:**
    - `pad_top = 0.01` (1% NPC)
    - `pad_bot = 0.01` (1% NPC)
  - **Collision detection:**
    - `coincident_threshold_factor = 0.1` (10% threshold)
    - `tight_lines_threshold_factor = 0.5` (50% threshold)
  - **Fallback strategy:**
    - `gap_reduction_factors = c(0.5, 0.3, 0.15)` (multi-level)
  - **Marquee rendering:**
    - `marquee_size_factor = 6`
    - `marquee_lineheight = 0.9`
  - **Height estimation:**
    - `height_safety_margin = 1.0`
    - `height_fallback_npc = 0.13`

- Helper functions:
  - `get_label_placement_param(key, default)` - Get single parameter
  - `get_label_placement_config()` - Get entire config
  - `override_label_placement_config(...)` - Override for testing

**Anvendes af:**
- `fct_add_spc_labels.R` - Label placement logic
- Plot generation system
- Label collision avoidance

**Relateret:**
- `config_ui.R` - Font scaling
- Plot rendering functions

**Typiske ændringer:**
```r
# Tune label spacing:
LABEL_PLACEMENT_CONFIG$relative_gap_labels <- 0.25  # Tættere labels

# Ændre collision threshold:
LABEL_PLACEMENT_CONFIG$coincident_threshold_factor <- 0.15  # Mere konservativ

# Test override:
old_config <- override_label_placement_config(
  relative_gap_line = 0.10
)
# ... run tests ...
LABEL_PLACEMENT_CONFIG <<- old_config  # Restore
```

---

### 7. config_system_config.R

**Formål:** System-level constants for performance, logging, timeouts og rate limits.

**Indeholder:**
- **Application:**
  - `DEFAULT_PORT = 3838`
  - `AUTO_RESTORE_ENABLED = FALSE`

- **File processing:**
  - `DEFAULT_ENCODING = "ISO-8859-1"`
  - `UTF8_ENCODING = "UTF-8"`
  - `CSV_SEPARATORS` (semicolon, comma, tab)
  - `DECIMAL_SEPARATORS` (comma, period)

- **Logging:**
  - `LOG_COMPONENTS` (legacy, se config_log_contexts.R)

- **Performance:**
  - `OPERATION_TIMEOUTS` (file_read, chart_render, auto_detect, ui_update)
  - `DEBOUNCE_DELAYS` (input_change=150ms, chart_update=500ms, etc.)
  - `LOOP_PROTECTION_DELAYS` (default=500ms, conservative=800ms, etc.)
  - `PERFORMANCE_THRESHOLDS` (reactive_warning, memory_warning, cache_timeout)
  - `RATE_LIMITS` (file_upload, api_call, session_save)
  - `AUTOSAVE_DELAYS` (data_save, settings_save)

- **Test mode:**
  - `TEST_MODE_CONFIG` (ready_event_delay, startup_debounce, etc.)

- **Cache:**
  - `CACHE_CONFIG` (timeouts, size limits, cleanup interval)

- **UI updates:**
  - `UI_UPDATE_CONFIG` (immediate, fast, standard, safe delays)

**Anvendes af:**
- Performance optimization
- Debounce/throttle operations
- Rate limiting
- Cache management
- Test mode initialization

**Relateret:**
- `config_ui.R` - UI-specific settings
- CLAUDE.md Appendix B - Performance Architecture

**Typiske ændringer:**
```r
# Juster debounce for bedre responsiveness:
DEBOUNCE_DELAYS$chart_update <- 300  # Reducer fra 500ms

# Ændre cache timeout:
CACHE_CONFIG$default_timeout_seconds <- 600  # 10 minutter

# Tune test mode:
TEST_MODE_CONFIG$auto_detect_delay_ms <- 200  # Hurtigere
```

---

### 8. config_ui.R

**Formål:** UI layout konstanter, styles og responsive font scaling.

**Indeholder:**
- **Layout:**
  - `UI_COLUMN_WIDTHS` (quarter, half, thirds, sidebar)
  - `UI_HEIGHTS` (logo, modal_content, chart_container, etc.)
  - `UI_INPUT_WIDTHS` (full, half, quarter, etc.)
  - `UI_LAYOUT_PROPORTIONS` (half, third, quarter, etc.)

- **Styles:**
  - `UI_STYLES` (flex_column, scroll_auto, full_width, etc.)

- **Font scaling:**
  - `FONT_SCALING_CONFIG`:
    - `divisor = 42` - Viewport diagonal divisor (lavere = større fonts)
    - `min_size = 8` - Minimum base_size i points
    - `max_size = 64` - Maximum base_size i points
  - Algoritme: `base_size = max(min_size, min(max_size, sqrt(width × height) / divisor))`

**Anvendes af:**
- UI rendering (column widths, heights)
- Plot generation (font scaling)
- Responsive design
- CSS styling

**Relateret:**
- `config_label_placement.R` - Label sizing
- Plot rendering functions

**Typiske ændringer:**
```r
# Ændre font scaling for større fonts:
FONT_SCALING_CONFIG$divisor <- 36  # Lavere = større fonts

# Opdater standard heights:
UI_HEIGHTS$chart_container <- "calc(60vh - 80px)"

# Tilføj ny style:
UI_STYLES$new_style <- "custom: css; rules: here;"
```

---

### 9. inst/golem-config.yml

**Formål:** Environment-based configuration (development/production/testing) via Golem framework.

**Indeholder:**
- **Environments:**
  - `default` - Base configuration
  - `development` - Dev-optimized (verbose logging, fast feedback)
  - `production` - Prod-optimized (security, stability)
  - `testing` - Test-optimized (reproducibility, controlled conditions)

- **Configuration categories per environment:**
  - `environment` - Type identification flags
  - `security` - HTTPS, CSRF, timeouts, CSP
  - `performance` - Caching, debounce, file limits, profiling
  - `logging` - Log level, debug mode, metrics
  - `testing` - Auto-load test data, mock services
  - `ui` - Hot reload, debug panel, dev tools
  - `session` - Auto restore, save on exit, cleanup

**Anvendes af:**
- App initialization
- Environment detection
- Feature flags
- Runtime configuration

**Relateret:**
- `app_config.R` - Golem config access functions
- CLAUDE.md Section 5.1 - Miljøkonfiguration

**Typiske ændringer:**
```yaml
# Aktivér development mode:
Sys.setenv(GOLEM_CONFIG_ACTIVE = "development")

# Tilføj ny setting:
development:
  custom:
    new_setting: true

# Access i kode:
golem::get_golem_options("new_setting", default = FALSE)
```

---

## Configuration Best Practices

### 1. Hvor skal nye configs?

**Beslutningsdiagram:**

```
Er det environment-specifikt (dev/prod/test)?
├─ JA → `inst/golem-config.yml`
└─ NEJ ↓

Er det UI layout/styling?
├─ JA → `config_ui.R`
└─ NEJ ↓

Er det SPC-specifikt (chart types, colors, validation)?
├─ JA → `config_spc_config.R` eller `config_chart_types.R`
└─ NEJ ↓

Er det performance/timing (debounce, cache, timeouts)?
├─ JA → `config_system_config.R`
└─ NEJ ↓

Er det logging context?
├─ JA → `config_log_contexts.R`
└─ NEJ ↓

Er det branding (hospital navn, logo, farver)?
├─ JA → `config_branding_getters.R` eller `inst/config/brand.yml`
└─ NEJ ↓

Er det observer priorities?
├─ JA → `config_observer_priorities.R`
└─ NEJ ↓

Er det plot label placement tuning?
├─ JA → `config_label_placement.R`
└─ NEJ → Overvej at oprette ny config-fil eller tilføj til `config_system_config.R`
```

### 2. Naming Conventions

**Konstanter:**
```r
# ✅ Korrekt - ALL_CAPS med underscore
SPC_COLORS <- list(...)
CHART_TYPES_DA <- list(...)
OBSERVER_PRIORITIES <- list(...)

# ❌ Forkert
spc_colors <- list(...)
chartTypesDA <- list(...)
```

**Funktioner:**
```r
# ✅ Korrekt - snake_case
get_qic_chart_type()
chart_type_requires_denominator()
get_label_placement_param()

# ❌ Forkert
getQicChartType()
ChartTypeRequiresDenominator()
```

### 3. Documentation Standards

Hver config-fil skal have:

```r
# ============================================================
# [FILNAVN]
# ============================================================
# FORMÅL: [kort beskrivelse]
# ANVENDES AF: [hvilke moduler/funktioner]
# RELATERET: [andre config-filer]
# ============================================================

# Section header (hvis flere sections)
# ====================================

#' Roxygen documentation for exported constants
#' @export
CONSTANT_NAME <- value
```

### 4. Testing Configuration Changes

**Før du ændrer configs:**

```r
# 1. Kør eksisterende tests
R -e "library(SPCify); testthat::test_dir('tests/testthat')"

# 2. Lav din ændring

# 3. Kør tests igen
R -e "library(SPCify); testthat::test_dir('tests/testthat')"

# 4. Manuel test i app
run_app()

# 5. Performance benchmark (hvis relevant)
R -e "microbenchmark::microbenchmark(
  library(SPCify),
  times = 10
)"
```

### 5. Configuration Change Checklist

- [ ] Identificeret korrekt config-fil via beslutningsdiagram
- [ ] Fulgt naming conventions (ALL_CAPS for konstanter)
- [ ] Tilføjet Roxygen documentation med `@export` hvis nødvendigt
- [ ] Opdateret relaterede tests
- [ ] Kørt `devtools::document()` hvis NAMESPACE påvirket
- [ ] Manuel test i app
- [ ] Performance vurderet (hvis performance-relateret)
- [ ] Opdateret denne guide hvis ny config-fil eller major changes

---

## Troubleshooting

### "Config værdi ikke tilgængelig i runtime"

**Problem:** `get_golem_options("setting")` returnerer NULL

**Løsning:**
```r
# Check aktiv environment:
Sys.getenv("GOLEM_CONFIG_ACTIVE")

# Sæt korrekt environment:
Sys.setenv(GOLEM_CONFIG_ACTIVE = "development")

# Genstart app
```

### "Kan ikke finde config-fil"

**Problem:** `system.file("config", "brand.yml", package = "SPCify")` returnerer ""

**Løsning:**
```r
# Development (package ikke installeret):
# - Filer skal være i inst/ folder
# - Access via fallback paths i config_branding_getters.R

# Production (package installeret):
# - Kør devtools::install() eller pak::pkg_install()
```

### "Observer fires i forkert rækkefølge"

**Problem:** Race condition trods priorities

**Løsning:**
```r
# 1. Check at priority er sat:
observeEvent(trigger, priority = OBSERVER_PRIORITIES$HIGH, { ... })

# 2. Verificér priority værdi:
print(OBSERVER_PRIORITIES$HIGH)  # Skal være 2000

# 3. Check om andre observers har højere priority

# 4. Overvej at øge gap mellem priorities i config_observer_priorities.R
```

### "Font scaling virker ikke som forventet"

**Problem:** Fonts for små/store på forskellige devices

**Løsning:**
```r
# Tune FONT_SCALING_CONFIG i config_ui.R:
FONT_SCALING_CONFIG$divisor <- 36  # Lavere = større fonts
FONT_SCALING_CONFIG$min_size <- 10  # Højere minimum
FONT_SCALING_CONFIG$max_size <- 48  # Lavere maximum

# Test på forskellige devices:
# - Standard display (pixelratio = 1)
# - Retina display (pixelratio = 2)
# - 4K display (pixelratio varies)
```

---

## Relateret Dokumentation

- **CLAUDE.md** - Projektinstruktioner og udviklingsprincipper
  - Section 5.1: Miljøkonfiguration (Golem)
  - Section 10.2: Hierarchical State Access
  - Appendix B: Performance Architecture
  - Appendix D: App State Schema

- **README.md** - Projektoversigt og installation

- **docs/adr/** - Architecture Decision Records
  - Relevante ADR'er for configuration beslutninger

---

## Revision History

| Dato | Version | Ændringer | Forfatter |
|------|---------|-----------|-----------|
| 2025-10-09 | 1.0.0 | Initial configuration guide oprettet | Claude |

---

**Spørgsmål eller manglende information?** Opdater denne guide eller kontakt projektejer.
