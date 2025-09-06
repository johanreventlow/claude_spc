# MODULE_STRUCTURE.md
# SPC App - Modulær Struktur

## 📁 Oversigt over filstruktur

```
claude_spc/
├── app.R                           # Hovedfil der starter appen
├── ui.R                            # UI sammensætning
├── server.R                        # Server sammensætning
├── global.R                        # Globale konfigurationer
│
├── R/
│   ├── ui/                         # UI komponenter
│   │   ├── ui_header.R            # JavaScript, CSS, header
│   │   ├── ui_sidebar.R           # Sidebar med upload/config
│   │   └── ui_main_content.R      # Hovedindhold (tabel + graf)
│   │
│   ├── server/                     # Server komponenter
│   │   ├── server_reactive_values.R      # Reaktive værdier
│   │   ├── server_session_management.R   # Session save/load/clear
│   │   ├── server_file_upload.R          # Fil upload håndtering
│   │   ├── server_data_table.R           # Tabel rendering/editing
│   │   ├── server_column_management.R    # Kolonne auto-detect/validation
│   │   ├── server_visualization.R        # Graf setup og data prep
│   │   ├── server_download.R             # Download handlers
│   │   └── server_helpers.R              # Hjælpe funktioner
│   │
│   └── modules/                    # Genbrugelige moduler
│       ├── data_*/                 # Data håndtering moduler
│       │   ├── data_file_readers.R        # CSV/Excel læsning
│       │   ├── data_validation.R          # Data struktur validering
│       │   ├── data_editable_table_ui.R   # Redigerbar tabel UI
│       │   ├── data_editable_table_server.R  # Tabel server logik
│       │   └── data_module.R              # Main data modul
│       │
│       ├── visualization_*/        # Visualisering moduler
│       │   ├── visualization_module_ui.R      # Visualisering UI
│       │   ├── visualization_module_server.R  # Server logik
│       │   ├── visualization_helpers.R        # Helper funktioner
│       │   ├── visualization_spc.R            # SPC plot generering
# │       │   ├── visualization_anhoej.R         # Removed: Now using qic() built-in Anhøj analysis
│       │   └── visualization_module.R         # Main viz modul
│       │
│       └── local_storage_*/        # Browser storage moduler
│           ├── local_storage_js.R             # JavaScript funktioner
│           ├── local_storage_functions.R      # R funktioner
│           └── local_storage_module.R         # Main storage modul
│
└── www/                            # Statiske filer (logos, etc.)
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

## 🔄 Dataflow

```
1. app.R → Starter applikationen
2. ui.R → Sammensætter UI fra komponenter
3. server.R → Sammensætter server fra komponenter
4. global.R → Leverer konfiguration til alle
```

### UI Flow:
```
ui_header.R → JavaScript og CSS
ui_sidebar.R → Upload og konfiguration
ui_main_content.R → Data tabel og visualisering
```

### Server Flow:
```
server_reactive_values.R → Initialiserer state
server_session_management.R → Auto-restore, save, clear
server_file_upload.R → Excel/CSV håndtering  
server_data_table.R → Tabel rendering og editing
server_column_management.R → Auto-detect og validering
server_visualization.R → Graf setup
server_download.R → Export handlers
server_helpers.R → Utilities og auto-save
```

### Moduler:
```
data_module → Fil læsning, validering, redigerbar tabel
visualization_module → SPC plots, Anhøj regler
local_storage_module → Browser localStorage
```

## 🚀 Fordele

1. **Færre bugs**: Mindre filer er nemmere at debugge
2. **Hurtigere udvikling**: Parallelt arbejde på komponenter
3. **Bedre tests**: Isolerede komponenter kan unit testes
4. **Nem udvidelse**: Nye features tilføjes som moduler
5. **Bedre performance**: Kun nødvendige filer indlæses
6. **Teamwork**: Flere udviklere kan arbejde samtidigt

## 📝 Konventioner

### Filnavne:
- `ui_*.R` - UI komponenter
- `server_*.R` - Server komponenter  
- `*_module.R` - Genbrugelige moduler
- `*_ui.R` / `*_server.R` - Modul UI/server par

### Funktionsnavne:
- `create_*()` - UI konstruktører
- `setup_*()` - Server setup funktioner
- `handle_*()` - Event handlers
- `*Module()` - Shiny moduler

Dette design gør SPC appen meget mere maintainable og skalerbar!
