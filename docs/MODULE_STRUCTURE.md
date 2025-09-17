# MODULE_STRUCTURE.md
# SPC App - Modulær Struktur

**OPDATERET: 2025-01-16** - Afspejler aktuelle filer efter Phase 1-2 refactoring

## 📁 Aktuel filstruktur

```
claude_spc/
├── app.R                           # Hovedfil der starter appen
├── global.R                        # Globale konfigurationer, hospital branding
├── CLAUDE.md                       # Udviklingsinstruktioner og regler
├── SHINY_BEST_PRACTICES_FASER.md   # Dokumentation af refactoring faser
│
├── R/
│   ├── run_app.R                   # App launcher funktionalitet
│   ├── app_ui.R                    # Hovedfil for UI sammensætning
│   ├── app_server.R                # Hovedfil for server sammensætning
│   │
│   ├── modules/                    # Shiny moduler
│   │   ├── mod_data_upload.R       # Data upload og fil håndtering modul
│   │   ├── mod_session_storage.R   # Session storage og auto-save modul
│   │   └── mod_spc_chart.R         # SPC chart visualisering modul
│   │
│   ├── fct_*.R                     # Funktionsfiler (funktionalitet)
│   │   ├── fct_chart_helpers.R     # SPC chart hjælpefunktioner
│   │   ├── fct_data_processing.R   # Auto-detect og databehandling
│   │   ├── fct_data_validation.R   # Data validering funktioner
│   │   ├── fct_file_io.R           # Fil input/output operationer
│   │   ├── fct_file_operations.R   # Upload/download handlers
│   │   ├── fct_spc_calculations.R  # SPC beregninger og qic integration
│   │   └── fct_visualization_server.R # Visualisering server logik
│   │
│   ├── utils_*.R                   # Utility filer (hjælpefunktioner)
│   │   ├── utils_app_setup.R       # App initialisering og setup
│   │   ├── utils_danish_locale.R   # Dansk lokalisering
│   │   ├── utils_local_storage.R   # Browser localStorage funktioner
│   │   ├── utils_local_storage_js.R # JavaScript localStorage interface
│   │   ├── utils_reactive_state.R  # Centraliseret state management (Phase 4)
│   │   ├── utils_server_management.R # Server management og cleanup
│   │   └── utils_session_helpers.R # Session hjælpefunktioner
│   │
│   └── data/                       # Test- og eksempeldata
│       ├── spc_exampledata.csv     # Primær testdata (auto-load i TEST_MODE)
│       ├── spc_exampledata1.csv    # Alternativ testdata
│       ├── test_infection.csv      # Infektionsdata eksempel
│       └── *.xlsx                  # Excel eksempler med session metadata
│
├── tests/                          # testthat test suite
│   └── testthat/                   # Test filer fra Phase 1-2
│       ├── test-fase1-refactoring.R    # Phase 1 tests (later::later elimination)
│       ├── test-fase2-reactive-chains.R # Phase 2 tests (reactive improvements)
│       └── test-*.R                    # Øvrige test filer
│
├── www/                            # Statiske filer
│   ├── logo.png                    # Hospital logo
│   └── custom.css                  # Custom CSS styling
│
└── _brand.yml                      # Hospital branding konfiguration
```

## 🎯 Designprincipper

### 1. **Separation of Concerns**
Hver fil har ét specifikt ansvar:
- UI filer håndterer kun brugergrænsefladen
- Server filer håndterer kun server logik
- Moduler er selvstændige genbrugelige komponenter

### 2. **Modularitet**
- Komponenter kan testes og udvikles isoleret
- Nemt at tilføje nye features uden at påvirke eksisterende
- Klar opdeling mellem UI og server logik

### 3. **Genbrugelighed**
- Moduler kan bruges i andre Shiny apps
- Komponenter er parameteriserede og fleksible
- Standardiserede interfaces mellem moduler

### 4. **Vedligeholdelse**
- Nemt at finde og rette specifikke fejl
- Klare navnekonventioner
- Dokumentation i hver fil

## 🔄 Aktuel Dataflow

```
1. app.R → Starter applikationen via run_app.R
2. app_ui.R → Sammensætter UI fra moduler og komponenter
3. app_server.R → Sammensætter server logik og moduler
4. global.R → Leverer konfiguration, branding og hjælpefunktioner
```

### UI Flow:
```
app_ui.R → Hovedlayout med navbar, sidebar og main content
├── mod_data_upload.R (UI) → File upload interface
├── mod_session_storage.R (UI) → Session management controls
└── mod_spc_chart.R (UI) → Chart visualization og controls
```

### Server Flow:
```
app_server.R → Server koordination og module calls
├── Setup: utils_app_setup.R → App initialisering
├── Data:
│   ├── fct_file_operations.R → File upload/download handlers
│   ├── fct_data_processing.R → Auto-detect og data transformation
│   └── fct_data_validation.R → Input validation
├── Visualization:
│   ├── fct_visualization_server.R → Plot generation coordination
│   ├── fct_spc_calculations.R → qicharts2 integration
│   └── mod_spc_chart.R (Server) → Chart rendering og interaction
├── Session:
│   ├── mod_session_storage.R (Server) → Auto-save og localStorage
│   └── utils_session_helpers.R → Session utilities
└── Management: utils_server_management.R → Cleanup og lifecycle
```

### Shiny Moduler (med namespace isolation):
```
mod_data_upload → File upload, Excel/CSV processing, data preview
mod_session_storage → Auto-save, localStorage, session restore
mod_spc_chart → Chart generation, column mapping, visualization controls
```

### Reactive State Management (Phase 2):
```
app_server.R → Hovedcoordination med reactive values
├── Event-driven patterns → Observer prioritering og cleanup
├── Debounced operations → Native Shiny debounce() patterns
├── Req() guards → Proper reactive chain management
└── Isolation patterns → Performance optimering
```

## 🚀 Fordele af aktuel struktur

1. **Færre bugs**: Funktionalitet opdelt i små, testbare filer
2. **Hurtigere udvikling**: Klar adskillelse mellem fct_, utils_ og mod_ filer
3. **Bedre tests**: Comprehensive testthat suite (Phase 1-2 med 125+ tests)
4. **Nem udvidelse**: Modulær struktur tillader nye features som isolerede komponenter
5. **Bedre performance**: Event-driven patterns og reactive optimering (Phase 2)
6. **Vedligeholdelse**: Klare navnekonventioner og dokumenterede phases

## 📝 Aktuelle konventioner

### Filnavne efter refactoring:
- `mod_*.R` - Shiny moduler med UI og Server funktioner
- `fct_*.R` - Funktionalitetsfiler (business logik)
- `utils_*.R` - Hjælpefunktioner og utilities
- `app_*.R` - Hovedapplikation sammensætning

### Funktionsnavne:
- `setup_*()` - Initialisering og setup funktioner
- `handle_*()` - Event handlers og file processing
- `create_*()` - UI konstruktører og objektoprettelse
- `*Module()` - Shiny modul funktioner (UI/Server)
- `auto_*()` - Auto-detect og automatiserede processer
- `safe_*()` - Error-safe wrappers (planlagt i Phase 5)

### Test patterns:
- `test-fase[N]-*.R` - Tests for hver refactoring phase
- Minimum 80% coverage for kritiske funktioner
- Event-driven test patterns for reactive flows

## 🔄 Phase 1-2 forbedringer

### Phase 1: Later::Later Elimination
- ✅ **Elimineret**: 12+ `later::later()` anti-patterns
- ✅ **Implementeret**: Event-driven cleanup patterns
- ✅ **Resultat**: Stabil timing og færre race conditions

### Phase 2: Reactive Chain Improvements ⭐ **AKTUEL LØSNING**
- ✅ **Forbedret**: req() guards og reactive dependencies
- ✅ **Løst**: Oprindelige problem med input field updates
- ✅ **Implementeret**: Event-driven renderUI patterns
- ✅ **Resultat**: Selectize fields opdateres korrekt efter auto-detect

### Næste phases (dokumenteret i SHINY_BEST_PRACTICES_FASER.md):
- **Phase 3**: Observer management (identificeret regression)
- **Phase 4-5**: State management og error handling (planlagt)

Dette design gør SPC appen robust, maintainable og klar til videre udvikling!
