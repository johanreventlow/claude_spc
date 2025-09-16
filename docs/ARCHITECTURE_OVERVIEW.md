# SPC App Architecture Overview

## Projektoversigt

SPC App er en **Shiny applikation** til **Statistical Process Control (SPC)** analyse bygget med **qicharts2**. Appen bruges i klinisk kvalitetsarbejde på Bispebjerg og Frederiksberg Hospital.

**Seneste commit:** `5af818b` (16. september 2025)
**Arkitektur fase:** Phase 4 - Centraliseret State Management

---

## Høj-niveau Arkitektur

```
┌─────────────────────────────────────────────────────────────────┐
│                        SPC APP ARKITEKTUR                      │
├─────────────────────────────────────────────────────────────────┤
│  UI Layer (Shiny UI)                                           │
│  ├── app.R (main application entry)                           │
│  ├── R/ui/ (UI components)                                    │
│  └── R/modules/ (Shiny modules - UI parts)                    │
├─────────────────────────────────────────────────────────────────┤
│  Server Layer (Business Logic)                                │
│  ├── R/server/ (Server logic components)                      │
│  ├── R/modules/ (Shiny modules - Server parts)                │
│  └── R/fct_*.R (Core functions)                              │
├─────────────────────────────────────────────────────────────────┤
│  State Management (Phase 4)                                   │
│  ├── Centralized App State (app_state)                       │
│  ├── Dual-State Sync (old/new system compatibility)          │
│  └── Reactive State Coordination                             │
├─────────────────────────────────────────────────────────────────┤
│  Support Systems                                              │
│  ├── Configurable Logging (utils_logging.R)                  │
│  ├── File Operations (CSV/Excel)                             │
│  ├── Data Processing & Auto-Detection                        │
│  └── SPC Calculations (qicharts2 integration)                │
├─────────────────────────────────────────────────────────────────┤
│  External Dependencies                                         │
│  ├── qicharts2 (SPC charts)                                  │
│  ├── shiny + bslib (Web UI framework)                        │
│  ├── dplyr + readr (Data processing)                         │
│  └── Hospital Branding (brand.yml)                           │
└─────────────────────────────────────────────────────────────────┘
```

---

## Filstruktur & Componenter

### Hovedfiler
- **`app.R`** - Applikationens entry point, starter Shiny appen
- **`global.R`** - Global konfiguration, libraries, utilities, constants

### R/ Bibliotek
```
R/
├── modules/           # Shiny modules (genbrugelige UI+Server komponenter)
├── server/           # Server logik komponenter
├── ui/               # UI komponenter
├── data/             # Test data og eksempler
├── fct_*.R           # Core funktioner (functional programming)
└── utils_*.R         # Utility funktioner
```

### Konfiguration & Dokumentation
```
/
├── _brand.yml        # Hospital branding konfiguration
├── tests/            # Test suite (617 tests)
├── docs/             # Dokumentation (denne fil)
├── *.md              # Projekt dokumentation
└── R/data/           # Testdata for udvikling
```

---

## State Management (Phase 4)

### Centralized App State Schema

```r
app_state = list(
  data = list(
    current_data = NULL,           # Aktuelle data fra fil upload
    original_data = NULL,          # Backup af original data
    file_info = NULL,             # Fil metadata
    updating_table = FALSE,       # Table update flag
    table_operation_in_progress = FALSE,
    table_operation_cleanup_needed = FALSE,
    table_version = 0             # Versioning for table updates
  ),

  columns = list(
    auto_detect = list(           # Auto-detection system
      in_progress = FALSE,
      completed = FALSE,
      trigger = NULL,
      results = NULL
    ),
    mappings = list(              # Column mappings
      x_column = NULL,
      y_column = NULL,
      n_column = NULL,
      cl_column = NULL
    ),
    ui_sync = list(               # UI synchronization
      needed = NULL,
      last_sync_time = NULL
    )
  ),

  test_mode = list(
    auto_detect_ready = FALSE
  ),

  session = list(                 # Session management
    auto_save_enabled = TRUE,
    restoring_session = FALSE,
    file_uploaded = FALSE,
    user_started_session = FALSE,
    last_save_time = NULL,
    file_name = NULL
  ),

  ui = list(
    hide_anhoej_rules = FALSE     # UI preferences
  )
)
```

### Dual-State Sync Pattern

For at bevare bagudkompatibilitet under Phase 4 migration bruger appen et dual-state sync pattern:

```r
# Pattern for at læse fra centralized state med fallback
current_data_check <- if (exists("use_centralized_state") &&
                         use_centralized_state &&
                         exists("app_state")) {
  app_state$data$current_data
} else {
  values$current_data  # Gamle reactive values
}
```

Dette tillader gradual migration fra gamle `values` til nye `app_state` strukturer.

---

## Logging System

### Konfigureret Logging (A) Code Quality)
Appen bruger et konfigureret logging system i stedet for `cat("DEBUG: ...")`:

```r
# Environment-baseret log level kontrol
SPC_LOG_LEVEL=DEBUG   # Viser alle log messages
SPC_LOG_LEVEL=INFO    # Viser INFO, WARN, ERROR
SPC_LOG_LEVEL=WARN    # Viser kun WARN, ERROR
SPC_LOG_LEVEL=ERROR   # Viser kun ERROR

# Komponens-baseret logging
log_debug("Processing data", "DATA_PROC")
log_info("Auto-detect completed", "AUTO_DETECT")
log_error("File upload failed", "FILE_UPLOAD")
```

### Log Komponenter
- **DATA_PROC** - Data processing operations
- **AUTO_DETECT** - Auto-detection af kolonner
- **FILE_UPLOAD** - Fil upload og læsning
- **VISUALIZATION** - Plot generation og rendering
- **ERROR_HANDLING** - Fejlhåndtering
- **TEST_MODE** - Test mode functionality

---

## Data Flow

### 1. File Upload & Processing
```
User uploads file → validate_and_read_file() → ensure_standard_columns() →
auto_detect_columns() → update_column_choices() → trigger_plot_generation()
```

### 2. Auto-Detection Workflow
```
Data available → analyze_date_columns() → analyze_numeric_columns() →
smart_y_n_detection() → update_ui_inputs() → log_results()
```

### 3. Plot Generation
```
Column mappings → validate_selections() → prepare_qic_data() →
apply_hospital_theme() → render_with_anhoej_rules() → display_plot()
```

---

## Shiny Modules

### Visualisering Module (`mod_spc_chart.R`)
**Ansvar:** SPC chart generation og rendering
**Input:** Data, column mappings, chart settings
**Output:** ggplot2 SPC chart med hospital theming

**Key funktioner:**
- `visualizationModuleUI()` - UI for chart display
- `visualizationModuleServer()` - Server logic for chart generation
- Integration med qicharts2 library
- Hospital branding application

### Status Module
**Ansvar:** Status information og user feedback
**Features:** Progress indicators, success messages, error display

---

## Test Infrastructure (B) Testing)

### Test Coverage: **617 tests, 0% fejlrate**

```
tests/testthat/
├── test-phase4-centralized-state.R    # 102 tests - Phase 4 specific
├── test-logging-system.R              # 58 tests - Logging system
├── test-auto-detection.R               # 53 tests - Auto-detection
├── test-end-to-end-app-flow.R         # 88 tests - Integration tests
├── test-ui-synchronization.R          # 107 tests - UI sync
├── test-error-handling.R              # 19 tests - Error handling
└── ... (additional test suites)
```

### Test Kategorier
- **Unit Tests** - Isolerede funktioner og komponenter
- **Integration Tests** - End-to-end workflows
- **Phase 4 Tests** - Centralized state management
- **Reactive Tests** - Shiny reactive patterns
- **Error Handling** - Robusthed og fejlscenarios

---

## Performance & Optimering

### Reactive Performance
- **Debounced reactives** for expensive operations
- **Observer priorities** for controlled execution order
- **Manual invalidation** for complex dependency chains
- **Session cleanup** for memory management

### State Management Performance
- **Centralized state** reduces reactive complexity
- **Dual-state sync** eliminates unnecessary updates
- **Version tracking** for table operations
- **Cleanup flags** for automated maintenance

---

## Security & Robusthed

### Input Validation
- File format validation (CSV, Excel)
- Data type validation for numerisk/dato kolonner
- Column name sanitization
- Memory limits for file upload

### Error Handling
- `safe_operation()` wrapper for critical operations
- Graceful degradation ved fejl
- User-friendly error messages
- Comprehensive logging for debugging

### Session Management
- Automatic session cleanup
- State persistence options
- Memory leak prevention
- Observer lifecycle management

---

## Hospital Integration

### Branding System (`_brand.yml`)
```yaml
meta:
  name: "Bispebjerg og Frederiksberg Hospital SPC"
color:
  palette:
    primary: "#1f4e79"
    hospitalblue: "#0066cc"
    regionhblue: "#004488"
    # ... additional colors
```

### Theme Integration
- ggplot2 hospital theme
- Consistent color palette
- Logo integration
- Typography standards

---

## Deployment & Environment

### Development Setup
- `TEST_MODE_AUTO_LOAD = TRUE` for automatic test data
- Auto-detection triggers på startup
- Debug logging enabled via `SPC_LOG_LEVEL=DEBUG`
- Port flexibility for parallel testing

### Production Configuration
- `AUTO_RESTORE_ENABLED = FALSE` under udvikling
- `TEST_MODE_AUTO_LOAD = FALSE` i produktion
- `SPC_LOG_LEVEL=INFO` for production logging
- Hospital branding enabled

---

## Extension Points

### Adding New Chart Types
1. Extend `CHART_TYPES_DA` i `global.R`
2. Update `get_qic_chart_type()` mapping
3. Add validation i chart generation logic
4. Update UI options

### Adding New File Formats
1. Extend `validate_and_read_file()` i `fct_file_operations.R`
2. Add format-specific parsing logic
3. Update file upload UI restrictions
4. Add corresponding tests

### Adding New Hospital Configurations
1. Extend `_brand.yml` with new hospital section
2. Update `HOSPITAL_COLORS` extraction logic
3. Add hospital-specific validation rules
4. Update theme generation logic

---

## Performance Metrics

- **Startup tid:** ~2-3 sekunder med test data
- **File processing:** <1 sekund for typiske datasets (<10MB)
- **Plot generation:** <500ms for standard SPC charts
- **Memory footprint:** ~50-100MB afhængigt af data størrelse
- **Test execution:** ~30 sekunder for hele test suite (617 tests)

---

## Vedligeholdelse

### Regular Tasks
- Test suite execution før hver release
- Log review for performance issues
- Memory profiling ved store data uploads
- Browser compatibility testing
- Hospital branding updates

### Code Quality
- Konsistent logging med komponens tags
- Comprehensive error handling
- Clear separation of concerns
- Reactive patterns best practices
- Danish code comments og documentation

---

## Videre Udvikling

Se **TECHNICAL_IMPROVEMENTS_PLAN.md** for:
- **D) Architecture Improvements** - Constants og reusable modules
- Future enhancement roadmap
- Performance optimization targets
- New feature integration guidelines