# SPC App Arkitektur Dokumentation

**Opdateret:** 2025-09-21
**Version:** Post-refaktorering (Fase 1-4 komplet)
**Status:** Aktuelt system efter moderne modulær refaktorering

---

## 🏗️ Overordnet Arkitektur

SPC App følger moderne R Shiny best practices med:
- **Modular struktur** med clear separation of concerns
- **Centraliseret state management** via unified app_state system
- **Event-driven architecture** med reactive event bus
- **Environment-aware configuration** med golem-style patterns
- **Comprehensive error handling** og graceful degradation

### Arkitektur Principper

1. **Single Source of Truth** - All application state i centraliseret `app_state`
2. **Event-Driven Design** - Reactive chains styret af events, ikke direct dependencies
3. **Environment Awareness** - Different behavior for development/production/testing
4. **Defensive Programming** - Robust error handling og input validation
5. **Test-Driven Development** - >95% test coverage med comprehensive testing

---

## 📁 Directory Structure

```
R/
├── config/                 # Configuration management
│   ├── app_runtime_config.R       # Centralized runtime configuration
│   ├── environment_profiles.R     # Environment-specific settings
│   ├── state_management.R         # App state schema definition
│   ├── hospital_branding.R        # Branding and themes
│   ├── chart_types.R              # SPC chart type definitions
│   ├── spc_config.R               # SPC-specific constants
│   ├── ui_config.R                # UI layout and styling
│   ├── system_config.R            # Performance and timeouts
│   └── observer_priorities.R      # Reactive execution priorities
├── core/                   # Core business logic
│   ├── spc_helpers.R              # SPC calculation utilities
│   ├── file_io.R                  # File reading/writing
│   └── autodetect_helpers.R       # Column detection logic
├── server/                 # Server-side components
│   ├── app_server.R               # Main server function
│   ├── utils_server_management.R  # Server lifecycle management
│   ├── utils_session_helpers.R    # Session management utilities
│   ├── utils_column_management.R  # Column selection logic
│   ├── utils_event_system.R       # Event bus implementation
│   ├── performance_helpers.R      # Performance monitoring
│   ├── performance_optimizations.R # Reactive optimizations
│   └── plot_optimizations.R       # Chart rendering optimizations
├── ui/                     # UI components
│   ├── app_ui.R                   # Main UI function
│   ├── utils_ui_helpers.R         # UI utility functions
│   ├── utils_ui_components.R      # Reusable UI components
│   └── utils_ui_updates.R         # UI update mechanisms
├── modules/                # Shiny modules
│   ├── mod_spc_chart_ui.R         # SPC chart UI module
│   └── mod_spc_chart_server.R     # SPC chart server module
├── utils/                  # Utility libraries
│   ├── logging.R                  # Centralized logging system
│   ├── dependency_injection.R     # DI framework
│   ├── shinylogs_config.R         # Web-based logging
│   ├── advanced_debug.R           # Debug infrastructure
│   ├── end_to_end_debug.R         # E2E testing utilities
│   ├── local_storage.R            # Browser storage interaction
│   ├── danish_locale.R            # Danish localization
│   ├── performance.R              # Performance utilities
│   └── memory_management.R        # Memory cleanup
├── fct_*.R                 # Function files
│   ├── fct_spc_plot_generation.R  # SPC plot generation
│   ├── fct_file_operations.R      # File upload/processing
│   ├── fct_autodetect_unified.R   # Unified auto-detection
│   └── fct_visualization_server.R # Visualization logic
├── golem_utils.R           # Golem-style utilities
├── app_initialization.R   # Modular app initialization
├── app_dependencies.R     # Dependency management
└── run_app.R              # Application launcher

inst/
└── golem-config.yml        # Standard golem configuration

global.R                    # Main application bootstrap
```

---

## 🔄 State Management Architecture

### Centralized App State

All application state managed via `app_state` environment:

```r
app_state <- new.env(parent = emptyenv())

# Reactive Event Bus
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

# Data Management
app_state$data <- reactiveValues(
  current_data = NULL,
  original_data = NULL,
  file_info = NULL,
  updating_table = FALSE
)

# Column Management
app_state$columns <- reactiveValues(
  auto_detect_in_progress = FALSE,
  auto_detect_completed = FALSE,
  auto_detect_results = NULL,
  x_column = NULL,
  y_column = NULL,
  n_column = NULL,
  cl_column = NULL
)

# Session Management
app_state$session <- reactiveValues(
  auto_save_enabled = TRUE,
  restoring_session = FALSE,
  file_uploaded = FALSE,
  user_started_session = FALSE
)
```

### Event-Driven Patterns

**Event Emission:**
```r
# Emit events to trigger downstream effects
emit$data_loaded()
emit$auto_detection_completed()
emit$ui_sync_needed()
```

**Event Listeners:**
```r
# Priority-based event handling
observeEvent(app_state$events$data_loaded, ignoreInit = TRUE, priority = 1000, {
  # High priority: Start auto-detection
  emit$auto_detection_started()
})

observeEvent(app_state$events$auto_detection_completed, ignoreInit = TRUE, priority = 800, {
  # Medium priority: Update UI
  emit$ui_sync_needed()
})
```

---

## ⚙️ Configuration System

### Environment Profiles

Configuration managed via environment-specific profiles:

- **Development**: Enhanced debugging, auto-load test data, verbose logging
- **Production**: Security-focused, minimal logging, stable defaults
- **Testing**: Controlled conditions, deterministic behavior, isolation
- **Unknown**: Conservative fallback with safe defaults

### Runtime Configuration

```r
# Environment detection
runtime_config <- initialize_runtime_config()

# Environment-specific behavior
if (runtime_config$environment$environment_type == "development") {
  # Development-specific logic
} else if (runtime_config$environment$environment_type == "production") {
  # Production-specific logic
}
```

### Golem Configuration

Standard `inst/golem-config.yml` provides objective comparison with golem best practices:

```yaml
default:
  golem_name: claudeSPC
  golem_version: 1.0.0
  app_prod: false

development:
  app_prod: false
  logging:
    level: "DEBUG"
    enable_debug_mode: true
  testing:
    auto_load_test_data: true

production:
  app_prod: true
  logging:
    level: "ERROR"
    enable_debug_mode: false
  testing:
    auto_load_test_data: false
```

---

## 🔧 Key Components

### 1. Application Initialization

**Sequential loading via `app_initialization.R`:**
1. Load foundation utilities (logging, DI framework)
2. Initialize runtime configuration
3. Manage app dependencies
4. Load core functions
5. Load server components
6. Load UI components
7. Initialize main app components
8. Setup performance optimizations
9. Load specialized functionality
10. Verify initialization completeness

### 2. Reactive System

**Event Bus Pattern:**
- Events emitted via `emit` API
- Listeners with priority-based execution
- Loop protection via token system
- Graceful error handling

**Observer Priorities:**
```r
OBSERVER_PRIORITIES <- list(
  CRITICAL = 2000,    # Data loading, error handling
  HIGH = 1000,        # Auto-detection, state updates
  MEDIUM = 500,       # UI updates, user feedback
  LOW = 100,          # Cleanup, analytics
  BACKGROUND = 10     # Non-essential tasks
)
```

### 3. Error Handling

**Defensive Programming Patterns:**
```r
# Centralized error wrapper
safe_operation <- function(operation_name, code, fallback = NULL) {
  tryCatch({
    code
  }, error = function(e) {
    log_error(paste(operation_name, "fejlede:", e$message), "ERROR_HANDLING")
    return(fallback)
  })
}

# Scope-safe variable access
if (exists("variable") && !is.null(variable)) {
  # Safe to use variable
}
```

### 4. Performance Optimizations

**Reactive Performance:**
- Debounced user inputs (200-800ms depending on environment)
- Cached computations with digest-based invalidation
- Memory cleanup on session end
- Background processing for heavy operations

**Plot Optimizations:**
- Cached plot data with smart invalidation
- Progressive rendering for large datasets
- Memory-efficient data structures

---

## 🧪 Testing Architecture

### Test-Driven Development

**Comprehensive test coverage:**
- Unit tests for individual functions
- Integration tests for reactive chains
- End-to-end tests for user workflows
- Performance tests for memory usage
- Regression tests for bug prevention

**Test Commands:**
```r
# Run all tests
R -e "source('global.R'); testthat::test_dir('tests/testthat')"

# Run specific test
R -e "source('global.R'); testthat::test_file('tests/testthat/test-fase1-refactoring.R')"
```

### Environment Testing

Tests run across different environments:
```bash
# Development environment
R_CONFIG_ACTIVE=development R -e "source('global.R'); run_tests()"

# Production environment
R_CONFIG_ACTIVE=production R -e "source('global.R'); run_tests()"

# Testing environment
R_CONFIG_ACTIVE=testing R -e "source('global.R'); run_tests()"
```

---

## 🔍 Debugging and Monitoring

### Logging System

**Centralized logging via `utils/logging.R`:**
```r
log_debug("Detailed development information", "COMPONENT_NAME")
log_info("General application flow", "COMPONENT_NAME")
log_warn("Potential issues requiring attention", "COMPONENT_NAME")
log_error("Critical errors requiring intervention", "COMPONENT_NAME")
```

**Environment-aware log levels:**
- Development: DEBUG level for verbose output
- Production: ERROR level for minimal logging
- Testing: INFO level for controlled visibility

### Advanced Debugging

**Debug utilities via `utils/advanced_debug.R`:**
- State snapshot comparison
- Performance timing utilities
- Workflow tracing
- Session lifecycle tracking

### Web-based Monitoring

**ShinyLogs integration:**
- Real-time log viewing in browser
- User interaction tracking
- Performance metrics collection
- Error reporting dashboard

---

## 🚀 Deployment Patterns

### Environment-Specific Deployment

**Development:**
```r
# Enhanced debugging and convenience
run_dev(port = 4040, debug_level = "DEBUG")
```

**Production:**
```r
# Security and stability focused
run_prod(port = NULL, host = "0.0.0.0")
```

### Application Launcher

**Flexible run_app() function:**
```r
run_app(
  port = 3838,
  launch_browser = TRUE,
  options = list()
)
```

**Environment-aware browser launch:**
- Development: RStudio viewer for convenience
- Production: System browser for deployment
- Testing: No browser for automated testing

---

## 📈 Performance Characteristics

### Memory Management

**Efficient resource usage:**
- Automatic cleanup on session end
- Garbage collection optimization
- Memory leak prevention
- Large dataset handling

### Reactive Performance

**Optimized reactive chains:**
- Priority-based execution order
- Loop prevention mechanisms
- Efficient dependency tracking
- Minimal re-computation

### Caching Strategy

**Smart caching system:**
- Digest-based cache invalidation
- Environment-specific cache timeouts
- Memory-efficient cache storage
- Cache cleanup automation

---

## 🔒 Security Considerations

### Environment-Specific Security

**Production hardening:**
- HTTPS requirement in production
- CSRF protection enabled
- Content Security Policy enforcement
- Error detail hiding
- Session timeout management

**Development convenience:**
- Relaxed security for development productivity
- Debug endpoints enabled
- Extended session timeouts
- Permissive content policies

### Input Validation

**Defensive programming:**
- All user inputs validated
- File upload security checks
- SQL injection prevention
- Path traversal protection

---

## 📊 Monitoring and Metrics

### Application Health

**Key metrics tracked:**
- Response times
- Memory usage
- Error rates
- User interactions
- Performance bottlenecks

### Logging Analytics

**Structured logging for analysis:**
- Component-based log categorization
- Performance timing data
- Error classification
- User workflow tracking

---

## 🔄 Migration and Compatibility

### Backward Compatibility

**Dual-state support during migration:**
- Legacy `values$` patterns supported
- Gradual migration to `app_state`
- No breaking changes during transition
- Comprehensive fallback mechanisms

### Future Evolution

**Extension points:**
- Modular component architecture
- Plugin system ready
- Configuration-driven features
- API-ready design patterns

---

## 🏆 Best Practices Implemented

### Code Quality

- **Danish comments** for functionality description
- **English function names** for international compatibility
- **Consistent naming conventions** (snake_case for functions)
- **Type safety** with explicit type checks
- **Comprehensive documentation** with examples

### Development Workflow

- **Test-first development** with TDD methodology
- **Atomic commits** with descriptive messages
- **Code review** via diff inspection
- **Performance monitoring** throughout development
- **Continuous integration** testing

### Architecture Patterns

- **Single Responsibility Principle** for functions
- **Immutable data flow** patterns
- **Event-driven architecture** for loose coupling
- **Dependency injection** for testability
- **Configuration externalization** for flexibility

---

## 📚 Reference Documentation

### Key Files for Understanding

1. **`global.R`** - Application bootstrap and dependency loading
2. **`R/config/app_runtime_config.R`** - Configuration management
3. **`R/config/state_management.R`** - State schema definition
4. **`R/server/utils_event_system.R`** - Event bus implementation
5. **`R/app_initialization.R`** - Modular initialization system
6. **`inst/golem-config.yml`** - Standard configuration reference

### External Dependencies

**Core frameworks:**
- **shiny** - Web application framework
- **bslib** - Modern Bootstrap UI components
- **qicharts2** - Statistical Process Control charts
- **dplyr** - Data manipulation
- **readr/readxl** - File reading capabilities

**Development tools:**
- **testthat** - Unit testing framework
- **shinylogs** - Web-based logging and monitoring
- **yaml** - Configuration file parsing

---

*This architecture documentation reflects the current state after comprehensive refactoring (Phases 1-4). The system successfully balances modern R Shiny best practices with practical requirements for clinical quality work in Danish healthcare environments.*