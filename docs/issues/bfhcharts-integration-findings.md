# BFHcharts Integration Findings

**Dato:** 2025-10-16
**Status:** Testing efter BFHthemes Mari font fix

## Test Resultater

### ✅ Font Rendering (Mari Fallback)
**Status:** VIRKER

Efter fix i BFHthemes pakken renderes BFHcharts plots nu korrekt selvom Mari font er specificeret. BFHthemes implementerer automatisk fallback til "sans" hvis Mari ikke er tilgængelig på systemet.

**Test:** `test_bfhthemes_minimal.R` (slettet efter verificering)
- Plot genereret succesfuldt med Mari font family
- Rendering til PNG succesfuldt (41KB fil)
- Ingen "invalid font type" fejl

### ⚠️ Bug #1: target_value
**Status:** DELVIST IMPLEMENTERET I BFHCHARTS

**Finding:** BFHcharts læser `target` kolonne fra qicharts2 data, men renderer **IKKE** target line i plottet.

**Dokumentation fra BFHcharts:**
- Optional columns: `target` - Target line
- qicharts2 `target` parameter tilføjer target kolonne til data ✓
- BFHcharts `plot_config$target_value` har ingen effekt på rendering ✗

**Test Resultat:**
```r
qic_data <- qic(..., target = 110, return.data = TRUE)
# target kolonne findes i qic_data ✓
# men plot indeholder INGEN GeomHline layer ✗
```

**Plot Layers Observeret:**
1. GeomRibbon (confidence ribbon)
2-3. GeomTextline (center line, limits labels)
4-5. GeomLine (center line, limit lines)
6. GeomPoint (data points)
7-9. GeomLine (control limits, etc)

**Mangler:** GeomHline for target line

**Konklusion:** Target line rendering er sandsynligvis ikke implementeret i BFHcharts endnu. Dette skal håndteres i BFHcharts pakken, ikke i SPCify integration layer.

### ⚠️ Bug #2: centerline_value
**Status:** IKKE IMPLEMENTERET I BFHCHARTS

**Finding:** BFHcharts accepterer ikke `centerline_value` parameter (custom baseline for control limits).

**SPCify Implementation:**
- ✅ Parameter ekstraheres fra `extra_params$centerline_value`
- ✅ Passes til `map_to_bfh_params()` (line 332)
- ✅ Scale normalisering virker (90 → 0.9 for percentage charts)
- ✅ Inkluderet i conservative filter `fields_to_keep`
- ✅ Debug logging bekræfter parameter sendes

**Test Resultat:**
```r
# SPCify sender korrekt:
[23:40:08] DEBUG: [BFH_SERVICE] Centerline parameter included: centerline_value = 0.9

# BFHcharts afviser:
[23:40:08] ERROR: [ERROR_HANDLING_BFH_API_CALL] BFHchart API call fejlede:
  unused argument (centerline_value = 0.9)
```

**Implementering i SPCify:**
- `R/fct_spc_bfh_service.R` lines 663-670: centerline_value tilføjet til params
- `R/fct_spc_bfh_service.R` line 825: centerline_value i conservative filter
- `R/fct_spc_bfh_service.R` lines 847-855: Debug logging

**Konklusion:** Centerline (baseline) parameter er ikke implementeret i BFHcharts API endnu. SPCify sender parameteren korrekt, men BFHcharts::create_spc_chart() understøtter ikke argumentet. Dette skal håndteres i BFHcharts pakken.

### ⚠️ Bug #4: y_axis_unit Validation
**Status:** BREAKING CHANGE I BFHCHARTS API

**Finding:** BFHcharts har implementeret **STRAM validering** af `y_axis_unit` parameter.

**Allowed Values:**
- `"count"`
- `"percent"`
- `"rate"`
- `"time"`

**Tidligere Opførsel:** Fri-tekst y_axis_unit accepteret (fx "Patienter", "Antal indlæggelser")

**Nuværende Opførsel:**
```r
spc_plot_config(y_axis_unit = "Patienter")
# Warning: Invalid y_axis_unit: 'Patienter'. Valid units are: count, percent, rate, time
```

**Impact på SPCify:**
- SPCify's UI tillader fri-tekst y_axis_unit input
- Dette vil fejle ved BFHcharts rendering
- Kræver enten:
  1. Mapping layer i SPCify (fri-tekst → valid unit)
  2. UI constraint til kun valid units
  3. BFHcharts API ændring til at acceptere custom units

### ✅ Bug #4: chart_title
**Status:** VIRKER

Chart title overføres korrekt til BFHcharts via `plot_config$chart_title`.

**Test:** Plot titel "Ventetid Pr. Måned" → `plot$labels$title` korrekt sat ✓

### ✅ Bug #3: Chart Type Switching
**Status:** IKKE REPRODUCERBAR I ISOLATION

**Test Resultat:** Testede switching mellem run, i, og p charts:
- Alle 3 chart types genereres succesfuldt ✓
- Rapid switching (run → i → run) uden crash ✓
- BFHcharts håndterer chart type ændringer korrekt

**Konklusion:** Crashet kan ikke reproduceres uden for Shiny reactive context. Potentielle årsager:
- Tidligere bug der er løst i BFHthemes/BFHcharts
- Shiny-specifik race condition (kræver app test)
- Specifik kombination af settings (fx med Skift/Frys kolonner)

**Anbefaling:** Test i appen med rigtige data og observer om crash opstår i production context.

## SPCify Integration Impact

### ✅ Ændringer Implementeret (2025-10-16)

#### 1. y_axis_unit Mapping
**Status:** IMPLEMENTERET

Mapping layer tilføjet i `R/fct_spc_bfh_service.R` for at mappe bruger-input til BFHcharts valid units:

**Implementering:**

Funktion `map_y_axis_unit_to_bfhcharts()` tilføjet i `R/fct_spc_bfh_service.R` (lines 1370-1473):

```r
#' @export
map_y_axis_unit_to_bfhcharts <- function(user_input) {
  # 1. Handle NULL or empty → "count"
  # 2. Exact match (case-insensitive)
  # 3. Heuristic patterns:
  #    - "procent|%|pct" → "percent"
  #    - "rate|andel|per" → "rate"
  #    - "tid|time|duration|varighed" → "time"
  # 4. Default fallback → "count"
}
```

**Integration i compute_spc_results_bfh()** (lines 326-339):

```r
# Map y_axis_unit from user input to BFHcharts valid units
y_axis_unit_raw <- extra_params$y_axis_unit %||% "count"
y_axis_unit <- map_y_axis_unit_to_bfhcharts(y_axis_unit_raw)

log_debug(
  paste("y_axis_unit =", y_axis_unit, "(mapped from:", y_axis_unit_raw, ")"),
  .context = "BFH_SERVICE"
)
```

**Støtter:**
- Exact matches: "count", "percent", "rate", "time" (case-insensitive)
- Heuristic patterns: "Procent" → "percent", "andel" → "rate", "tid" → "time"
- Danish language support: "procent", "andel", "tid", "varighed"
- Graceful fallback til "count" for fri-tekst input (fx "Patienter")

#### 2. Target Value & Centerline Handling
Siden BFHcharts ikke renderer target line eller centerline endnu:
- **Option A:** Vent på BFHcharts implementering (anbefalet)
- **Option B:** Tilføj target/centerline i SPCify (duplikation af ansvar)
- **Option C:** Fallback til qicharts2 når target/centerline er sat

**Status:**
- ✅ SPCify sender `target_value` korrekt (feat/target-line-rendering)
- ✅ SPCify sender `centerline_value` korrekt (fix/bfhcharts-core-features)
- ❌ BFHcharts renderer ikke target line endnu
- ❌ BFHcharts accepterer ikke centerline_value parameter endnu

Anbefaling: **Option A** - Dette er BFHcharts ansvar

### Test Plan

- [x] Mari font fallback
- [x] target_value overførsel (fungerer delvist - data korrekt, rendering mangler)
- [x] centerline_value overførsel (SPCify korrekt, BFHcharts accepterer ikke parameter)
- [x] y_axis_unit validation (breaking change identificeret)
- [x] chart_title overførsel (fungerer)
- [x] Bug #3: Chart type switching (IKKE reproducerbar i isolation - BFHcharts OK)
- [ ] Test i appen med rigtige data og Shiny reactive context
- [ ] Edge cases: Skift, Frys kolonner med BFHcharts

## Anbefalinger

### Kort Sigt (SPCify)
1. ✅ Ingen ændringer nødvendige til font handling (fixed i BFHthemes)
2. ⚠️ Implementér y_axis_unit mapping layer
3. ✅ chart_title virker - ingen ændringer

### Mellemlang Sigt (BFHcharts)
1. Implementér target line rendering fra `target` data kolonne
2. Implementér `centerline_value` parameter support for custom baseline
3. Overvej at tillade custom y_axis_unit strings (eller returner user input som label)

### Lang Sigt
1. Fuld migration fra qicharts2 til BFHcharts efter stabilisering
2. Fjern qicharts2 fallback branch

## Relaterede Filer

- `R/mod_spc_chart_server.R` - BFHcharts integration logic
- `docs/issues/bfhcharts-bugs-prioritering.md` - Bug tracking
- Test filer (slettet efter verificering):
  - `test_bfhthemes_minimal.R`
  - `test_bug1_bug4_bfhcharts.R`
  - `test_target_value_qic.R`
  - `test_target_debug.R`
