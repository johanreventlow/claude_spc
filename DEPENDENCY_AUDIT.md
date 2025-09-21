# Dependency Audit - SPC App

## Nuværende Status (2025-09-21)

### Pakker i global.R library() kald:
Ingen. Alle tidligere imports er migreret til `pkg::funktion`-mønster.

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
- ✅ `excelR`, `zoo`, `scales`, `openxlsx`, `shinylogs`, `readr`, `readxl`, `lubridate` anvendes nu kun via namespace
- ✅ `dplyr` fjernet fra global re-eksponering; base pipe `|>` og eksplicit `dplyr::case_when()` anvendes i stedet
- ✅ `stringi`, `shinyWidgets`, `shinycssloaders` fjernet fra Imports (ikke længere i brug)

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

### Bevarede library() kald:
Ingen – alle tidligere imports er nu fuldt erstattet af eksplicitte namespace-kald.

### Anbefalet Fremtidig Strategi:

- Afhold en targeted gennemgang for at sikre, at nye bidrag følger namespace-mønstret (tilføj evt. lintr-regler).
- Overvej at reducere afhængighedslisten i `R/app_dependencies.R` yderligere ved at fjerne inaktive pakker i feature-listen (done for stringi/shinyWidgets/shinycssloaders, hold øje med fremtidige tilføjelser).

**Bemærk:** Fuld namespace konvertering vil kræve update af alle R/ filer og omfattende test suite verifikation.
