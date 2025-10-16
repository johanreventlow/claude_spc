# Target Line Manual Test Guide
**Branch:** `feat/target-line-rendering`
**Dato:** 2025-10-16
**Status:** Klar til manual test i Shiny app

## Baggrund

**Problem:** Target line blev ikke vist i BFHcharts plots fordi `target_value` blev filtreret væk af conservative parameter filter.

**Løsning:** `target_value` er nu inkluderet i `fields_to_keep` og sendes til BFHcharts.

## Ændringer Implementeret

### 1. Fjernet Conservative Filter for target_value
**Fil:** `R/fct_spc_bfh_service.R` (lines 812-835)

**Før:**
```r
fields_to_keep <- c("data", "x", "y", "n", "chart_type", "freeze", "part", "multiply")
```

**Efter:**
```r
fields_to_keep <- c("data", "x", "y", "n", "chart_type", "freeze", "part", "multiply", "target_value")
```

### 2. Tilføjet Debug Logging
Logging tilføjet for at bekræfte at `target_value` sendes til BFHcharts:

```r
# Log target_value if present
if ("target_value" %in% names(bfh_params_clean)) {
  log_debug(
    paste(
      "Target parameter included: target_value =", bfh_params_clean$target_value
    ),
    .context = "BFH_SERVICE"
  )
}
```

## Manual Test Procedure

### Test Setup

1. **Start Shiny App:**
   ```r
   source("global.R")
   run_app()
   ```

2. **Load Test Data:**
   - Upload eller load test datasæt med mindst 20 datapunkter
   - Vælg chart type: I-chart eller Run chart

### Test Case 1: Target Line Rendering (I-chart)

**Steps:**
1. Load data (fx infektioner pr. måned)
2. Vælg chart type: **I-chart**
3. Indtast target value i UI: `45`
4. Generer plot

**Forventet Resultat:**
- ✅ Plot vises korrekt
- ✅ Horizontal target line ved y = 45
- ✅ Target line har distinct styling (farve, linetype)
- ✅ Console log viser: `Target parameter included: target_value = 45`

**Failure Indicators:**
- ❌ Ingen target line synlig
- ❌ Plot fejler eller crasher
- ❌ Target line ved forkert y-værdi

### Test Case 2: Target Line med Percentage Chart

**Steps:**
1. Load data med numerator + denominator kolonner
2. Vælg chart type: **P-chart**
3. Indtast target value i UI: `75` (interpreteres som 75%)
4. Generer plot

**Forventet Resultat:**
- ✅ Plot vises korrekt
- ✅ Target line ved y = 0.75 (normaliseret værdi)
- ✅ Console log viser: `Target parameter included: target_value = 0.75`
- ✅ Scale normalization virker korrekt

### Test Case 3: Uden Target Value (Regression Test)

**Steps:**
1. Load data
2. Vælg chart type: **Run chart**
3. **UNDLAD** at indtaste target value (lad feltet være tomt)
4. Generer plot

**Forventet Resultat:**
- ✅ Plot vises korrekt uden target line
- ✅ Ingen fejl eller warnings
- ✅ Console log viser IKKE "Target parameter included"

### Test Case 4: Chart Type Switching med Target

**Steps:**
1. Load data og indtast target value: `50`
2. Generer **I-chart** → verificer target line
3. Skift til **Run chart** → verificer target line (eller ingen hvis ikke supported)
4. Skift tilbage til **I-chart** → verificer target line igen

**Forventet Resultat:**
- ✅ Ingen crash ved chart type switching
- ✅ Target line renderes konsistent
- ✅ Ingen reactive storms eller race conditions

## Logging Verification

**Enable Debug Logging:**
```r
options(spc.debug.source_loading = TRUE)
```

**Forventet Log Output:**
```
[BFH_SERVICE] Target parameter included: target_value = 45
```

**Check for Errors:**
```
# Ingen errors eller warnings relateret til target_value
```

## Bekendelse Checklist

Ved afsluttet test, bekræft:

- [ ] Test Case 1: Target line renderes korrekt på I-chart
- [ ] Test Case 2: Scale normalization virker for P-chart (75 → 0.75)
- [ ] Test Case 3: Plot uden target value virker (regression test)
- [ ] Test Case 4: Chart type switching uden crash
- [ ] Console logging bekræfter target_value sendes til BFHcharts
- [ ] Ingen errors eller warnings i console
- [ ] Target line styling matcher BFHcharts defaults

## Known Issues / Expected Behavior

### Hvis Target Line IKKE Renderes:

**Mulige Årsager:**
1. **BFHcharts version compatibility:** Target line rendering kan være ikke implementeret i nuværende BFHcharts version
   - **Verify:** Check `docs/issues/bfhcharts-integration-findings.md` line 18-44
   - **Dokumenteret:** "Target line rendering er sandsynligvis ikke implementeret i BFHcharts endnu"

2. **Parameter naming mismatch:** BFHcharts kan forvente `target` i stedet for `target_value`
   - **Debug:** Inspect BFHcharts function signature
   - **Fix:** Rename parameter i `map_to_bfh_params()` hvis nødvendigt

3. **BFHcharts silent failure:** BFHcharts accepterer parameter men ignorerer det
   - **Verify:** Check BFHcharts source code eller documentation

### Fallback Strategy

Hvis BFHcharts IKKE renderer target line:

**Option A (Anbefalet):** Vent på BFHcharts implementering
- Dette er BFHcharts ansvar
- Dokumenter issue i BFHcharts repo

**Option B:** Manual overlay i SPCify
- Tilføj `geom_hline()` efter BFHcharts plot generation
- Duplikation af ansvar - ikke ideel

**Option C:** Fallback til qicharts2 når target er sat
- Modsiger pure BFHcharts workflow (ADR-001)
- Kun som sidste udvej

## Next Steps After Testing

1. **Hvis target line renderes korrekt:**
   - ✅ Skriv automatiserede tests (testthat)
   - ✅ Opdater dokumentation
   - ✅ Merge til master efter code review

2. **Hvis target line IKKE renderes:**
   - ⚠️ Dokumenter findings i `bfhcharts-integration-findings.md`
   - ⚠️ Opret issue i BFHcharts repo
   - ⚠️ Beslut fallback strategy med team

## Referencer

- **Implementation:** `R/fct_spc_bfh_service.R` lines 812-835
- **Parameter Mapping:** `R/fct_spc_bfh_service.R` lines 654-661
- **Known Issue:** `docs/issues/bfhcharts-integration-findings.md` lines 18-44
- **Architecture:** `docs/adr/ADR-001-pure-bfhcharts-workflow.md`
- **Commit:** feat/target-line-rendering branch (commit ba238ad)
