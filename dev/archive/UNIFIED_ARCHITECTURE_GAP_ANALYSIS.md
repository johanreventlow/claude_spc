# Unified Architecture Gap Analysis
## Detaljeret gennemgang af manglende konverteringer

*Genereret: 2025-09-18*
*Status: Arbejde i gang på at identificere ALLE inconsistencies*

## Oversigt

Denne analyse identificerer alle steder i kodebasen hvor Unified Event Architecture og Unified State Management ikke er implementeret konsekvent. Infrastrukturen er etableret, men der mangler systematisk konvertering.

## Kategori 1: Funktioner der mangler `emit` parameter

### 🔴 Høj prioritet

#### A. `setup_download_handlers` (R/fct_file_operations.R:547)
**Problem**: Bruger `values` parameter der ikke eksisterer i funktionssignaturen
**Linje 559**: `create_complete_excel_export(file, input, values)`
**Fix**: Tilføj `values` parameter ELLER konverter til `app_state`

#### B. Export-relaterede funktioner bruger `values`
- `create_complete_excel_export` (R/fct_file_operations.R:598)
- `create_session_info_lines` (R/fct_file_operations.R:715)
**Problem**: Disse funktioner forventer `values` men burde konverteres til `app_state`

## Kategori 2: Mixed State Management

### 🟡 Medium prioritet

#### A. Funktioner med både `values` og `app_state` parametre

1. `reset_app_to_clean_state` (R/utils_memory_management.R:328)
   ```r
   reset_app_to_clean_state <- function(values, app_state = NULL, session = NULL)
   ```
   **Fix**: Fjern `values` parameter, brug kun `app_state`

## Kategori 3: Legacy reaktive mønstre

### 🟡 Medium prioritet

#### A. Direct reactive observations (R/fct_data_processing.R)
**Linje 51-132**: Stort `observe({...})` block der direkte observerer state changes
**Problem**: Burde konverteres til event-driven mønster
**Fix**: Konverter til `observeEvent(app_state$events$data_changed, ...)`

## Kategori 4: Kommentarer og dokumentation

### 🟢 Lav prioritet

#### A. Forældede kommentarer
Mange steder indeholder kommentarer der refererer til fjernet `values$` kode:
- R/fct_data_processing.R:484, 548, 761, 824, 979, 1422
- R/utils_server_management.R:560, 633, 635
- R/utils_memory_management.R:344

**Fix**: Ryd op i kommentarer for klarhed

## Kategori 5: Manglende emit calls

### 🔴 Høj prioritet

#### A. Column name changes (R/fct_data_processing.R:1125)
```r
names(app_state$data$current_data) <- new_names
```
**Problem**: Ingen emit call efter data struktur ændring
**Fix**: Tilføj `emit$data_changed()` eller `emit$columns_changed()`

#### B. Individual column assignments
Flere steder hvor kolonner tilføjes uden emit calls:
- R/fct_data_processing.R:1174-1180

## Kategori 6: Test incompatibilitet

### 🟡 Medium prioritet

Test filer bruger stadig `values` parametre i mange function calls, men dette er forventeligt under migration.

## Prioriteret handlingsplan

### Fase 1: Kritiske fixes (🔴)
1. Fix `setup_download_handlers` parameter problem
2. Konverter export funktioner til `app_state`
3. Tilføj manglende emit calls til data strukturændringer

### Fase 2: Architecture cleanup (🟡)
1. Fjern mixed `values`/`app_state` parametre
2. Konverter direct reactive observations til event-driven
3. Opdater test files til nye signatures

### Fase 3: Hygiejne (🟢)
1. Ryd op i forældede kommentarer
2. Opdater dokumentation
3. Valider komplet unified architecture

## Status oversigt

- ✅ **Event bus infrastruktur**: Komplet implementeret
- ✅ **Centralized state management**: Fungerer korrekt
- ⚠️ **Function signatures**: Delvist konverteret
- ❌ **Export system**: Bruger stadig legacy `values`
- ⚠️ **Data lifecycle events**: Mangler enkelte emit calls
- ❌ **Direct reactive patterns**: Ikke konverteret endnu

## Estimat

**Total indsats**: 4-6 timer
- Fase 1: 2-3 timer (kritisk)
- Fase 2: 1-2 timer
- Fase 3: 1 time

**Risiko**: Medium - export funktionalitet kan have problemer indtil fase 1 er færdig.