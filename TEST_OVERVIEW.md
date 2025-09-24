# Test Oversigt - SPC App

## Introduktion

Dette dokument giver et komplet overblik over test-suiten for SPC applikationen. Med 48 testfiler fordelt over `/tests/testthat/` mappen dækker test-suiten alle kritiske aspekter af applikationen fra grundlæggende infrastruktur til komplekse statistiske beregninger.

Test-suiten er designet efter Test-Driven Development (TDD) principper og fokuserer særligt på:
- **Dansk klinisk kontekst** - Understøttelse af æøå, danske datoformater, komma som decimalseparator
- **Event-driven arkitektur** - Moderne reaktive mønstre uden timing-afhængigheder
- **Robust fejlhåndtering** - Defensiv programmering med `safe_operation()` og proper guards
- **Performance og skalerbarhed** - Tests med store datasæt og concurrent operationer
- **Backward compatibility** - Sikring af eksisterende funktionalitet under refaktorering

## Test Kategorier

### 1. System & Infrastructure (12 filer)

#### `test-app-basic.R`
**Formål:** Grundlæggende app startup tests med `shinytest2::AppDriver`
- Tester at appen kan starte uden fejl
- Verificerer at velkomstindhold vises korrekt
- Kontrollerer navbar struktur og navigation

#### `test-app-initialization.R`
**Formål:** App initialization og konfigurationsindlæsning
- Tester `golem::get_golem_options()` konfiguration
- Verificerer hospital branding variables indlæsning
- Kontrollerer TEST_MODE miljøvariabler
- Tester unified state system initialization

#### `test-constants-architecture.R`
**Formål:** Arkitektur konstanter og globale indstillinger
- Tester OBSERVER_PRIORITIES konstanter
- Verificerer UI performance metrics struktur
- Kontrollerer memory limits og queue size indstillinger

#### `test-dependency-namespace.R`
**Formål:** Pakke namespace enforcement og `library()` kald elimination
- Scanner alle R filer for forbudte `library()` kald
- Sikrer at pakker kun bruges via `pkg::` namespace notation
- Omfatter 24 specifikke pakker (shiny, ggplot2, qicharts2, osv.)

#### `test-error-handling.R`
**Formål:** Central fejlhåndtering og logging system
- Tester `log_error()`, `log_info()`, `log_warn()`, `log_debug()` funktioner
- Verificerer `safe_operation()` fejlindkapsling med fallback værdier
- Kontrollerer `observer_manager()` lifecycle management
- Tester `create_empty_session_data()` struktur

#### `test-logging-debug-cat.R`
**Formål:** Elimination af rå `cat()` debug statements
- Scanner kodebasen for `cat("DEBUG` mønstre
- Sikrer at alle debug outputs bruger struktureret `log_debug()`
- Del af logging system modernisering

#### `test-logging-system.R`
**Formål:** Centraliseret logging system med component tagging
- Tester log level filtering (DEBUG, INFO, WARN, ERROR)
- Verificerer component-baserede tags (`[APP_SERVER]`, `[FILE_UPLOAD]`, osv.)
- Kontrollerer miljøvariabel håndtering og danske karakterer i logs

#### `test-no-file-dependencies.R`
**Formål:** Kritisk test for at appen kan instantiere uden filsystem afhængigheder
- Verificerer at `app_server` og `main_app_server` funktioner eksisterer
- Kontrollerer at ingen problematiske `source()` kald eksisterer
- Sikrer container-venlighed og deployment robusthed

#### `test-package-initialization.R`
**Formål:** R pakke initialization og namespace management
- Tester pakke loading og DESCRIPTION fil kompatibilitet
- Verificerer namespace exports og imports
- Kontrollerer `.onLoad()` og `.onAttach()` hooks

#### `test-run-app.R`
**Formål:** Main `run_app()` funktion og Shiny app launching
- Tester at custom ports forwards korrekt til `runApp()`
- Verificerer at globale variable requirements opfyldes
- Kontrollerer at app kan starte med mockede Shiny komponenter

#### `test-safe-operation-comprehensive.R`
**Formål:** Omfattende tests af `safe_operation()` error handling pattern
- Tester fejlindkapsling med forskellige error typer
- Verificerer fallback værdier og logging integration
- Kontrollerer nested operation error propagation
- Tester performance impact af error boundaries

#### `test-branding-globals.R` + `test-ui-exports.R`
**Formål:** UI sistem og branding
- Tester hospital theme variables og globale UI indstillinger
- Verificerer UI export funktioner og component availability
- Kontrollerer branding konsistens på tværs af moduler

### 2. Auto-Detection System (8 filer)

#### `test-auto-detection.R`
**Formål:** Grundlæggende auto-detection logik og data processing
- Tester identifikation af dato kolonner (Date objekter + navn-baseret detektion)
- Verificerer numerisk kolonne detektion og danske decimal formater
- Kontrollerer kolonne formål detektion (dato, tæller, nævner, kommentar, osv.)
- Tester data validering og tom kolonne detektion
- Verificerer UI sync data struktur generation og prioritering

#### `test-autodetect-algorithms.R`
**Formål:** Algoritmer og scoring system for column detection
- Omfattende tests af `autodetect_engine()` med forskellige trigger typer
- Tester `detect_columns_full_analysis()` med komplekse datasæt
- Verificerer column scoring funktioner og prioritering
- Kontrollerer state synchronization og både legacy/moderne parameter styles

#### `test-autodetect-core.R`
**Formål:** Kernefunktionalitet for automatisk kolonne detektion
- Tester kolonne type identifikation (Date, numeric, character)
- Verificerer dansk dato format håndtering og edge cases
- Kontrollerer column mapping logic med caching
- Tester integration med reactive system

#### `test-autodetect-unified-comprehensive.R`
**Formål:** Mest omfattende auto-detection tests (400+ linjer)
- Komplet `autodetect_engine()` funktionalitet med alle trigger typer
- Smart unfreeze logic og frozen state management
- Session start scenarios og name-only detection
- Robust dansk dato detektion og full column analysis
- Error handling, danske kliniske mønstre og performance

#### `test-autodetect-tidyverse-integration.R`
**Formål:** Integration af moderne tidyverse patterns i auto-detection
- Tester `purrr::map()` og `dplyr::` funktioner i column detection
- Verificerer functional programming patterns
- Kontrollerer performance af tidyverse vs base R approaches

#### `test-comprehensive-ui-sync.R`
**Formål:** Unified autodetect og kolonnemapping med UI synchronization
- Tester `autodetect_engine()` med alle kernekolonner ved filupload
- Verificerer smart-unfreezer logic for frozen state
- Kontrollerer manual trigger override af frozen state
- Tester `detect_columns_name_based()` robuste navnemønstre

#### `test-fase4-intelligent-heuristics.R`
**Formål:** Avancerede heuristikker og intelligent column matching
- Tester forbedrede algoritmer for dansk klinisk terminologi
- Verificerer contextual column detection baseret på data patterns
- Kontrollerer adaptive scoring baseret på data kvalitet

#### `test-dropdown-loop-prevention.R`
**Formål:** Forebyggelse af UI feedback loops og race conditions
- Tester `safe_programmatic_ui_update()` med token-baseret beskyttelse
- Verificerer update queuing under concurrent operations
- Kontrollerer programmatic input registration og metrics tracking

### 3. Plot Generation & Visualization (6 filer)

#### `test-plot-generation.R`
**Formål:** Omfattende ggplot2-baseret SPC chart generation
- Grundlæggende SPC chart konstruktion med target linjer
- Phase separation lines og kommentar annotationer
- Chart scaling, axis formattering og forskellige chart typer (P, Run, I-charts)
- Error handling, accessibility features og export considerations

#### `test-plot-core.R`
**Formål:** Kernefunktionalitet for plot generation
- Tester QIC chart type conversion (`get_qic_chart_type()`)
- Verificerer hospital theme application og qicharts2 integration
- Kontrollerer plot error handling patterns

#### `test-spc-plot-generation-comprehensive.R`
**Formål:** Mest omfattende plotting tests (725+ linjer)
- Komplet `generateSPCPlot()` funktion med dansk number parsing
- Alle chart typer, dato håndtering, phase/freeze funktionalitet
- Target linjer, kommentar annotations og error handling
- Performance/caching, hospital themes og danske kliniske data mønstre

#### `test-visualization-server.R`
**Formål:** Visualization server logic og reactive chains
- Tester `setup_visualization()` initialization
- Verificerer reactive state updates og chart type conversion
- Kontrollerer real data plotting og graceful handling af missing data

#### `test-visualization-dimensions.R`
**Formål:** Responsive plot dimension beregninger
- Tester `compute_spc_plot_height()` med client height usage
- Verificerer aspect ratio fallbacks og minimum height enforcement
- Kontrollerer default height handling

#### `test-spc-plot-tidyverse-performance.R`
**Formål:** Performance optimization med tidyverse patterns
- Tester `dplyr::` og `ggplot2::` performance patterns
- Verificerer memory efficiency i plot generation
- Kontrollerer caching strategies for store datasæt

### 4. Data Handling & Processing (8 filer)

#### `test-file-upload.R`
**Formål:** File upload functionality og data import
- Tester `validate_uploaded_file()` og `handle_csv_upload()`
- Verificerer `handle_excel_upload()` og edge cases
- Kontrollerer dansk encoding support og reactive chain setup

#### `test-csv-parsing.R`
**Formål:** CSV data parsing med dansk locale support
- Tester komma som decimal separator og danske karakterer
- Verificerer encoding detection (UTF-8, ISO-8859-1)
- Kontrollerer delimiter detection og quote handling

#### `test-data-validation.R`
**Formål:** Input data validation og sanitization
- Tester data type validation og missing value handling
- Verificerer column consistency checks
- Kontrollerer data quality metrics og warnings

#### `test-input-sanitization.R`
**Formål:** Robust håndtering af edge case inputs
- Tester `sanitize_selection()` med `character(0)`, NA, tomme inputs
- Verificerer reactive expressions med problematiske inputs
- Kontrollerer defensive programming patterns

#### `test-qic-calculations.R`
**Formål:** QIC beregninger og SPC computations
- Omfattende qicharts2 integration og `validate_x_column_format()`
- Tester `detect_date_interval()` og optimal formatering
- Verificerer unit labels, chart type calculations, phase/baseline functionality
- Kontrollerer target linjer, edge cases og dansk number parsing

#### `test-danish-clinical-edge-cases.R`
**Formål:** Danske kliniske data edge cases (mest omfattende clinical test)
- æøå character encoding og dansk number format parsing
- Klinisk terminologi, Windows file path compatibility
- Missing data patterns, dato format variations
- Klinisk data validation, SPC chart types for kliniske kontekster
- Locale indstillinger og komplette hospital workflow mønstre

#### `test-excelr-data-reconstruction.R`
**Formål:** Excel data reconstruction og editability
- Tester integration med `excelR` package for data editing
- Verificerer data reconstruction efter UI changes
- Kontrollerer Excel-lignende editing functionality

#### `test-foreign-column-names.R`
**Formål:** Håndtering af non-standard kolonnenavne
- Tester app functionality med udenlandske kolonnenavne
- Verificerer manual kolonne selection prioritering over auto-detect
- Kontrollerer auto-detect fallback behavior og foreign name display

### 5. State Management & Architecture (4 filer)

#### `test-state-management-hierarchical.R`
**Formål:** Hierarkisk state management system (mest omfattende arkitektur test)
- Komplet `create_app_state()` testing og event bus functionality
- Data management og hierarchical column structure
- Environment-baseret sharing, session management, reactive chains
- Event-driven workflows, error handling, performance considerations
- Komplekse state transitions, backward compatibility, danske kliniske workflows

#### `test-cross-component-reactive.R`
**Formål:** Cross-component reactive dependencies og data flow
- Tester reactive values initialization og data flow patterns
- File Upload → Auto Detection → UI Sync → Session Reset workflow
- Reactive dependency priorities og error handling i reactive chains
- State consistency på tværs af komponenter og dependency tracking

#### `test-event-driven-reactive.R`
**Formål:** Event-driven functionality og reactive patterns
- Simulering af reactive values logic uden Shiny context
- Event-driven pattern logic og chain execution verification
- Auto-detection flag management og UI sync data structure validation
- Observer priority concepts og timing-agnostic approaches

#### `test-ui-synchronization.R`
**Formål:** UI synchronization og input management
- Input choices struktur og UI sync request creation/processing
- Event-driven vs timing-baserede approaches
- Input validation, observer priorities, UI sync cleanup
- Auto-detect UI sync flow, reaktive observers, edge cases

### 6. Performance & Integration (4 filer)

#### `test-performance.R`
**Formål:** Performance tests og benchmarks
- File upload performance og auto-detection performance scaling
- Reactive performance under rapid updates
- Plot generation med store datasæt, memory usage bounds
- Concurrent operations handling

#### `test-integration-workflows.R`
**Formål:** Integration tests for kritiske user workflows
- Data upload → auto-detect → plot workflow simulation
- Event-driven state workflow simulation
- Graceful handling af kritiske error scenarios

#### `test-recent-functionality.R`
**Formål:** Nyligt udviklede funktionalitet
- ReactiveVal trigger patterns og Excel upload autodetection fixes
- UI sync trigger mechanisms og unified state management helpers
- Function signature compatibility og event-driven vs timing approaches

#### `test-fase5-performance.R`
**Formål:** Fase 5 performance forbedringer
- Advanced caching strategies og memory optimization
- Large dataset handling og concurrent user simulation
- Performance regression testing

### 7. Architecture & Refactoring Tests (3 filer)

#### `test-fase1-refactoring.R`
**Formål:** Fase 1 refactoring - elimination af `later::later()` anti-patterns (mest omfattende refactoring test)
- Auto-save debounce trigger struktur og event-driven cleanup
- Session restore event-driven cleanup og table operations guard cleanup
- Global debounce native implementation og integration af alle Fase 1 ændringer
- Eliminering af `later::later()` anti-patterns med event-driven patterns

#### `test-fase2-reactive-chains.R`
**Formål:** Fase 2 reactive chain refactoring i `mod_spc_chart.R`
- Chart_config reactive med proper `req()` guards
- SPC_plot reactive eliminering af redundant chart_type calls
- RenderUI funktioner med event-driven patterns og isolation
- State management cleanup og dependency loop elimination

#### `test-fase3-event-driven-state-machine.R`
**Formål:** Fase 3 Event-Driven State Machine
- Session_started trigger for name-only detection
- Manual_autodetect_button trigger bypass af frozen state
- Data_loaded event unfreezer og trigger isolation
- Frozen state metadata, error handling, event flow konsistens

### 8. Specialized Functionality (3 filer)

#### `test-tidyverse-purrr-operations.R`
**Formål:** Integration af moderne tidyverse patterns
- `purrr::map()` funktioner i data processing
- Functional programming patterns vs imperative approaches
- Performance comparison og code readability improvements

#### `test-file-operations-tidyverse.R`
**Formål:** File operations med tidyverse integration
- `readr::` functions for robust file reading
- `dplyr::` data manipulation i file processing
- Pipe operator usage og error handling patterns

#### `test-scoping-debug.R`
**Formål:** R scoping debug (minimal utility test)
- Simpel test for at forstå R list modification
- Parameter passing behavior investigation

### 9. Recent Additions & Bug Fixes (4 filer)

#### `test-parse-danish-target-unit-conversion.R`
**Formål:** Dansk target værdi parsing og unit conversion
- Parsing af danske number formats i target værdier
- Unit conversion logic og percentage handling
- Clinical target value validation

#### `test-comment-row-mapping.R`
**Formål:** Comment row mapping og annotation system
- Mapping af kommentarer til specifikke datapunkter
- Comment display i plots og data tables
- Annotation persistence across data changes

#### `test-cache-collision-fix.R`
**Formål:** Cache collision prevention og memory management
- Prevention af cache key collisions i concurrent operations
- Memory cleanup efter cache operations
- Performance optimization af caching system

## Sammenfatning

Test-suiten dækker alle kritiske aspekter af SPC applikationen med særlig vægt på:

### Styrker
- **Omfattende coverage** - 48 filer med over 1000+ individuelle tests
- **Dansk klinisk fokus** - Dyb integration af danske formater og klinisk terminologi
- **Modern arkitektur** - Event-driven patterns, tidyverse integration, defensive programming
- **Performance awareness** - Explicit testing af store datasæt og concurrent operations
- **Robust error handling** - Omfattende edge case coverage og graceful degradation

### Test Filosofi
Tests følger projektets udviklingsprincipper:
- **Test-Driven Development** - Tests definerer forventet adfærd før implementation
- **Defensive Programming** - Explicit testing af edge cases og error conditions
- **Event-Driven Architecture** - Tests verificerer moderne reactive patterns
- **Backward Compatibility** - Sikring af eksisterende funktionalitet under refactoring

### Kvalitetssikring
- **Modularity** - Tests er organiseret i logiske grupper med clear separation of concerns
- **Maintainability** - Clear naming conventions og comprehensive documentation
- **Integration** - Both unit tests og integration tests for critical workflows
- **Performance** - Explicit benchmarks og memory usage validation

Denne test-suite repræsenterer en moden, professionel tilgang til kvalitetssikring i klinisk software udvikling med fokus på både teknisk excellence og praktisk anvendelighed i danske hospitalsmiljøer.