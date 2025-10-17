# BFHchart Feasibility Findings
**Dato:** 2025-10-15
**Status:** CRITICAL BLOCKER IDENTIFIED

## Executive Summary

BFHcharts er funktionel og kan generere SPC plots, MEN der er en **kritisk blocker**: BFHcharts accepterer **IKKE danske tegn** (æ, ø, å) i kolonnenavne gennem NSE (Non-Standard Evaluation).

## Testresultater

### ✅ Succesfulde tests
- BFHcharts installeret og indlæses korrekt
- `create_spc_chart()` fungerer med engelske kolonnenavne
- Genererer ggplot2 objekter som forventet
- Integration med qicharts2 virker

### ❌ Kritiske problemer

#### 1. Danske tegn i kolonnenavne (BLOCKER)
```r
# FEJLER
test_data <- data.frame(Tæller = c(...))
create_spc_chart(data = test_data, y = Tæller)
# Fejl: y must be a simple column name, got: Tæller
# Avoid special characters, spaces, or expressions

# VIRKER
test_data <- data.frame(count = c(...))
create_spc_chart(data = test_data, y = count)
# SUCCESS
```

**Impact:** SPCify bruger konsekvent danske kolonnenavne:
- `Tæller` (numerator)
- `Nævner` (denominator)
- `Dato` (date)
- Plus user-defined kolonner med danske tegn

#### 2. Multiple warnings (50+)
Ved plot generation ses 50+ warnings. Kræver nærmere undersøgelse.

## Arkitektur Analyse

### BFHcharts Design
BFHcharts har to lag:

1. **High-level API:** `create_spc_chart(data, x, y, ...)`
   - End-to-end: raw data → færdigt plot
   - Bruger qicharts2 til beregninger
   - Bruger BFH styling til visualisering

2. **Low-level API:** `bfh_spc_plot(qic_data, ...)`
   - Input: qicharts2 output (data.frame)
   - Output: styled ggplot2 object
   - Ingen NSE problemer på dette lag

### Nuværende SPCify Workflow
```
Data (danske kolonner)
  ↓
generateSPCPlot()
  ↓
qicharts2::qic(x = Dato, y = Tæller, n = Nævner)
  ↓
qic_data (data.frame med standardiserede kolonner)
  ↓
applyHospitalTheme()
  ↓
ggplot2 object
```

## Løsningsforslag

### Option 1: Fix BFHcharts NSE (anbefalet langsigtet)
**Strategi:** Opdater BFHcharts til at håndtere ikke-ASCII tegn i NSE

**Fordele:**
- Ren integration med danske kolonnenavne
- Følger R best practices
- Ingen workarounds nødvendig

**Ulemper:**
- Kræver ændring af BFHcharts pakken
- Tidskrævende
- Afhængighed af eksterne maintainers

**Action items:**
1. Undersøg BFHcharts NSE implementation
2. Identificer hvorfor danske tegn fejler
3. Submit PR til BFHcharts med fix
4. Vent på merge og release

### Option 2: Low-level API Integration (anbefalet kortsigtet)
**Strategi:** Brug `bfh_spc_plot()` i stedet for `create_spc_chart()`

**Workflow:**
```
Data (danske kolonner)
  ↓
qicharts2::qic() direkte kald (virker med danske tegn)
  ↓
qic_data (standardiserede kolonner: x, y, cl, ucl, lcl, ...)
  ↓
BFHcharts::bfh_spc_plot(qic_data) ← Intet NSE problem
  ↓
ggplot2 object
```

**Fordele:**
- Virker NU uden BFHcharts ændringer
- qicharts2 håndterer danske tegn fint
- Minimal refactoring af eksisterende kode
- Bevarer separation mellem calculation og visualization

**Ulemper:**
- To-stage workflow
- Skal mappe konfiguration til både qic() og bfh_spc_plot()

**Implementation:**
```r
# BEFORE (nuværende)
generateSPCPlot <- function(data, config, ...) {
  # ... data preparation ...
  qic_data <- qicharts2::qic(..., return.data = TRUE)
  plot <- custom_ggplot_building(qic_data)
  plot <- applyHospitalTheme(plot)
  return(list(plot = plot, qic_data = qic_data))
}

# AFTER (BFHcharts low-level)
generateSPCPlot <- function(data, config, ...) {
  # ... data preparation ...
  qic_data <- qicharts2::qic(..., return.data = TRUE)

  # Configure BFH plot
  plot_config <- BFHcharts::spc_plot_config(
    chart_title = title_text,
    y_axis_unit = y_axis_unit,
    target_value = target_value,
    target_text = target_text
  )

  # Generate BFH plot
  plot <- BFHcharts::bfh_spc_plot(
    qic_data = qic_data,
    plot_config = plot_config,
    viewport = BFHcharts::viewport_dims(width, height)
  )

  return(list(plot = plot, qic_data = qic_data))
}
```

### Option 3: Column Name Translation (ikke anbefalet)
**Strategi:** Translate danske kolonner → engelske → BFHcharts → translate tilbage

**Fordele:**
- Kan bruge high-level API

**Ulemper:**
- Kompleks mapping logic
- Fejl-prone
- Performance overhead
- Bryder separation of concerns

## Feature Paritet Analyse

### Nuværende qicharts2 features i SPCify:
- [x] Run charts
- [x] I-charts
- [x] P-charts
- [x] C-charts
- [x] U-charts
- [x] Phase splits (`part`)
- [x] Baseline freeze (`freeze`)
- [x] Target values
- [x] Centerline override
- [x] Anhøj rules (runs + crossings)
- [x] Comment annotations
- [x] Danish date formatting
- [x] Y-axis unit formatting (count/percent/rate/time)

### BFHcharts understøttelse:
- [x] Alle chart types (run, i, p, c, u, xbar, s, t, g)
- [x] Phase configuration
- [x] Baseline freeze
- [x] Target values med text
- [x] Y-axis units (count/percent/rate/time)
- [x] Intelligent date formatting
- [x] Responsive font sizing
- [x] Label placement
- [?] Anhøj rules visualization (skal verificeres)
- [?] Comment annotations (skal verificeres)

## Anbefalinger

### Umiddelbar handling (Option 2)
1. Implementer low-level API integration (`bfh_spc_plot()`)
2. Test med eksisterende danske kolonnenavne
3. Verificer feature paritet (især Anhøj rules + comments)
4. Deploy til staging

### Langsigtet handling (Option 1)
1. Åbn issue i BFHcharts repo om NSE + danske tegn
2. Undersøg BFHcharts kildekode for fix
3. Submit PR hvis muligt
4. Når fixed, migrate til high-level API

### Næste skridt
1. ✅ Dokumenter findings (denne fil)
2. [ ] Test BFHcharts::bfh_spc_plot() med qic_data fra SPCify
3. [ ] Verificer Anhøj rules rendering
4. [ ] Verificer comment annotations
5. [ ] Implementer proof-of-concept i generateSPCPlot()
6. [ ] Kør full regression test suite
7. [ ] Opdater ADR-015 med findings og strategi

## Konklusion

**BFHcharts er IKKE klar til drop-in replacement** gennem high-level API pga. danske tegn problem.

**Anbefalet approach:**
- **Kortsigtet:** Low-level API (`bfh_spc_plot()`) - virker NU
- **Langsigtet:** Fix BFHcharts NSE + migrate til high-level API

Dette sikrer:
- ✅ Progressiv migration uden breaking changes
- ✅ Bevarer danske kolonnenavne
- ✅ Separation mellem calculation (qicharts2) og visualization (BFHcharts)
- ✅ Testbar i stages
