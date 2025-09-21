# Dependency Audit - SPC App

## Nuværende Status (2025-09-21)

### Pakker i global.R library() kald:
```r
library(shiny)              # ✅ I DESCRIPTION
library(bslib)              # ✅ I DESCRIPTION
# library(qicharts2)        # ✅ I DESCRIPTION - bruger qicharts2:: namespace
library(excelR)             # ✅ I DESCRIPTION
library(dplyr)              # ✅ I DESCRIPTION
# library(ggplot2)          # ✅ I DESCRIPTION - bruger ggplot2:: namespace
# library(ggrepel)          # ✅ I DESCRIPTION - kan bruge ggrepel:: namespace
library(stringi)            # ✅ I DESCRIPTION
library(readr)              # ✅ I DESCRIPTION
library(readxl)             # ✅ I DESCRIPTION
library(shinycssloaders)    # ✅ I DESCRIPTION
library(shinyWidgets)       # ✅ I DESCRIPTION
# library(shinyjs)          # ✅ I DESCRIPTION - bruger shinyjs:: namespace
library(zoo)                # ✅ I DESCRIPTION
library(scales)             # ✅ I DESCRIPTION
# library(rlang)            # ❌ FJERNET - %||% er lokalt defineret
library(lubridate)          # ✅ I DESCRIPTION
library(openxlsx)           # ✅ I DESCRIPTION
library(yaml)               # ✅ I DESCRIPTION
library(shinylogs)          # ✅ I DESCRIPTION
library(later)              # ✅ I DESCRIPTION
```

### Oprindelige manglende pakker:
- ggplot2 ✅ TILFØJET
- ggrepel ✅ TILFØJET
- stringi ✅ TILFØJET
- shinycssloaders ✅ TILFØJET
- shinyWidgets ✅ TILFØJET
- zoo ✅ TILFØJET
- scales ✅ TILFØJET
- lubridate ✅ TILFØJET
- openxlsx ✅ TILFØJET
- later ✅ TILFØJET

### Optimering udført:
- rlang ❌ FJERNET (ikke nødvendig)
- 4 pakker konverteret til namespace-only pattern

### Total DESCRIPTION Imports: 26 pakker

## Namespace Usage Analysis

### Pakker der bruger pkg:: pattern:
- `ggplot2::ggplot()`, `ggplot2::aes()`, `ggplot2::geom_*()`
- `qicharts2::qic()`
- `shinyjs::useShinyjs()`, `shinyjs::show()`, `shinyjs::hide()`

### Pakker med lokale definitioner:
- `%||%` operator defineret i `R/ui/utils_ui_components.R:281`

## Næste Skridt
1. ✅ DESCRIPTION komplet opdateret
2. ✅ Test devtools::check()
3. ✅ Konverter flere modules til namespace pattern (6 pakker konverteret)
4. ⏳ **FREMTIDIGE REFAKTORERINGER:** Komplet eliminering af resterende library() kald

---

## 🚨 FUTURE WORK: Resterende Library() → Namespace Konvertering

### Bevarede library() kald (13 pakker) - SKAL konverteres senere:

**Core Framework (høj prioritet for fremtidig refaktorering):**
- `library(shiny)` → `shiny::` pattern
  - Impact: Hundredvis af UI funktioner (fluidPage, actionButton, etc.)
  - Scope: Hele UI og server kode
  - Effort: 🔴 STOR - kræver systematic gennemgang af alle filer

**UI & Widgets (medium prioritet):**
- `library(bslib)` → `bslib::` pattern
  - Functions: page_navbar, card, sidebar
  - Scope: Primært UI komponenter
  - Effort: 🟡 MEDIUM

- `library(shinyWidgets)` → `shinyWidgets::` pattern
  - Functions: pickerInput, switchInput, etc.
  - Scope: Specific UI widgets
  - Effort: 🟡 MEDIUM

- `library(shinycssloaders)` → `shinycssloaders::` pattern
  - Functions: withSpinner
  - Scope: Loading states
  - Effort: 🟢 LAV

**Data Processing (høj prioritet - bruges meget):**
- `library(dplyr)` → `dplyr::` pattern
  - Functions: %>%, filter, mutate, select, etc.
  - Scope: Hele data processing pipeline
  - Effort: 🔴 STOR - pipe operator bruges overalt

- `library(readr)` → `readr::` pattern (DELVIST GJORT)
  - Functions: read_csv, read_csv2
  - Status: Nogle allerede bruger readr:: namespace
  - Effort: 🟢 LAV - mest allerede gjort

- `library(readxl)` → `readxl::` pattern (DELVIST GJORT)
  - Functions: read_excel
  - Status: Nogle allerede bruger readxl:: namespace
  - Effort: 🟢 LAV - mest allerede gjort

**Specialized Libraries (lav prioritet):**
- `library(stringi)` → `stringi::` pattern
  - Functions: stri_*
  - Scope: String manipulation
  - Effort: 🟡 MEDIUM

- `library(zoo)` → `zoo::` pattern
  - Functions: rollmean, na.fill
  - Scope: Time series calculations
  - Effort: 🟢 LAV

- `library(scales)` → `scales::` pattern
  - Functions: percent, comma, date_format
  - Scope: Plot formatting
  - Effort: 🟢 LAV

- `library(lubridate)` → `lubridate::` pattern
  - Functions: ymd, dmy, month, year
  - Scope: Date handling
  - Effort: 🟡 MEDIUM

- `library(openxlsx)` → `openxlsx::` pattern
  - Functions: write.xlsx, createWorkbook
  - Scope: Excel export functionality
  - Effort: 🟢 LAV

- `library(excelR)` → `excelR::` pattern
  - Functions: excelTable
  - Scope: Excel-like table widgets
  - Effort: 🟢 LAV

- `library(shinylogs)` → `shinylogs::` pattern
  - Functions: track_usage
  - Scope: Logging functionality
  - Effort: 🟢 LAV

### Anbefalet Fremtidig Strategi:

1. **Fase A (Lav effort):** Konverter 🟢 LAV pakker først
   - shinycssloaders, zoo, scales, openxlsx, excelR, shinylogs
   - Estimat: 2-3 timer

2. **Fase B (Medium effort):** Konverter 🟡 MEDIUM pakker
   - bslib, shinyWidgets, stringi, lubridate
   - Estimat: 4-6 timer

3. **Fase C (Stor effort):** Konverter 🔴 STOR pakker
   - shiny, dplyr (pipe operator problem)
   - Estimat: 8-12 timer + omfattende testing

**Bemærk:** Fuld namespace konvertering vil kræve update af alle R/ filer og omfattende test suite verifikation.