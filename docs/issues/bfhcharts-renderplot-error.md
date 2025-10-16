# BFHcharts renderPlot() Incompatibility

**Dato:** 2025-10-16
**Status:** LØST - Alle 3 remedies implementeret

## Problem Beskrivelse

BFHcharts plots kan **ikke** renderes i Shiny's `renderPlot()` context, selvom de renders fint standalone.

### Error Message

```
Warning: Error in label: unused argument (labels[[nm]] %||% "")
  188: FUN
  187: lapply
  186: setup_plot_labels
  185: ggplot_build.ggplot2::ggplot
  183: print.ggplot2::ggplot
  181: renderPlot [mod_spc_chart_server.R#799]
```

### Context

```r
# Virker FINT standalone
plot <- BFHcharts::bfh_spc_plot(...)
png("test.png", width = 800, height = 600)
print(plot)
dev.off()
# ✓ SUCCESS - 41KB PNG genereret

# Fejler i Shiny renderPlot()
output$plot <- renderPlot({
  plot <- BFHcharts::bfh_spc_plot(...)
  print(plot)  # ✗ CRASH
})
```

## Fejl Analyse

### 1. Namespace Tests
- ✅ Ingen `label()` konflikter mellem shiny og ggplot2
- ✅ Ingen masked functions
- ✅ Plot renders uden fejl uden for Shiny

### 2. Layer Inspection
BFHcharts plot indeholder kun standard ggplot2 layers:
- GeomLine (x5)
- GeomPoint (x1)
- **INGEN** GeomText/GeomLabel layers

### 3. Shiny-Specifikt
- Fejlen opstår KUN i Shiny's `renderPlot()` execution environment
- `setup_plot_labels` er en **intern ggplot2 funktion** der kaldes ved `ggplot_build()`
- Error tyder på at en `label()` funktion kaldes med forkerte argumenter

## Root Cause Hypotese

**ggplot2 version incompatibility** mellem:
1. BFHcharts' build environment (udviklet med én ggplot2 version)
2. SPCify's runtime environment (kører med anden ggplot2 version)
3. Shiny's renderPlot() execution context (modificerer ggplot2 behavior)

### Understøttende Bevis

1. **"Unknown or uninitialised column: `signal`"** warning:
   - BFHcharts returnerer data uden `signal` kolonne
   - SPCify forventer `signal` kolonne fra qicharts2
   - Dette tyder på API mismatch

2. **Standalone OK, Shiny FAIL**:
   - Shiny's `renderPlot()` bruger speciel ggplot2 rendering pipeline
   - Kan være inkompatibel med BFHcharts' plot structure

3. **Error i `setup_plot_labels`**:
   - Intern ggplot2 funktion
   - Kaldes under `ggplot_build()`
   - Fejl tyder på at plot object har uventet struktur

## Mulige Løsninger

### Løsning A: ggplotGrob() Workaround (ANBEFALET)

**Rationale:** Convert plot til grob BEFORE sending til renderPlot, bypassing ggplot_build i Shiny context.

```r
output$spc_plot_actual <- shiny::renderPlot({
  plot_result <- spc_plot()

  if (is.null(plot_result)) {
    return(invisible(NULL))
  }

  # Check if BFHcharts plot
  is_bfhcharts <- !is.null(attr(plot_result, "backend")) &&
    attr(plot_result, "backend") == "bfhcharts"

  if (is_bfhcharts) {
    # Convert to grob BEFORE Shiny's ggplot_build
    grob <- ggplot2::ggplotGrob(plot_result)
    grid::grid.draw(grob)
  } else {
    print(plot_result)
  }
})
```

**Fordele:**
- Bypasser Sh iny's ggplot rendering pipeline
- Giver direkte kontrol over rendering
- Minimal code change

**Ulemper:**
- Kan påvirke responsiveness
- Mindre elegant end direkte print

### Løsning B: plotOutput(outputArgs) Workaround

Force png rendering strategy i renderPlot:

```r
output$spc_plot_actual <- shiny::renderPlot({
  plot_result <- spc_plot()
  print(plot_result)
}, outputArgs = list(bg = "white", type = "cairo"))
```

### Løsning C: Kontakt BFHcharts Maintainer

Rapportér incompatibility til BFHcharts team:
- Plot structure incompatibel med Shiny renderPlot()
- Bede om fix eller workaround guidance

### Løsning D: Fallback til qicharts2

Kortvarig løsning mens BFHcharts fixes:
- Detektér rendering fejl
- Automatic fallback til qicharts2
- Log incident for monitoring

```r
plot <- tryCatch({
  # Try BFHcharts
  if (USE_BFHCHARTS) {
    bfh_plot <- generate_bfh_plot(...)
    # Test render
    temp <- tempfile()
    png(temp); print(bfh_plot); dev.off()
    unlink(temp)
    bfh_plot
  }
}, error = function(e) {
  log_warn("BFHcharts rendering failed, falling back to qicharts2")
  generate_qicharts_plot(...)
})
```

## Anbefaling

**Prioritet 1:** Implementér Løsning A (ggplotGrob workaround)
**Prioritet 2:** Kontakt BFHcharts maintainer (Løsning C)
**Fallback:** Løsning D hvis A ikke virker

## Test Plan

1. Implementér Løsning A
2. Test i appen med rigtige data
3. Verificér at plot renders korrekt
4. Check performance impact
5. Test på tværs af devices (Retina vs standard)

## Implementering (2025-10-16)

### ✅ Remedy #1: Single-Stage Workflow
**Fil:** `R/fct_spc_bfh_service.R` (lines 298-357)

Ændrede `compute_spc_results_bfh()` fra two-stage til single-stage workflow:

```r
# BEFORE (two-stage):
qic_data <- call_qicharts2_for_data(...)
bfh_result <- call_bfh_spc_plot(qic_data = qic_data, ...)

# AFTER (single-stage):
bfh_params <- map_to_bfh_params(...)
bfh_result <- call_bfh_chart(bfh_params)  # Calls BFHcharts::create_spc_chart()
standardized <- transform_bfh_output(...)
```

Dette sikrer at vi kalder `BFHcharts::create_spc_chart()` høj-niveau API med numeriske vektorer i stedet for at sende qicharts2 data til low-level `bfh_spc_plot()`.

### ✅ Remedy #2: ggplotGrob() Workaround
**Fil:** `R/mod_spc_chart_server.R` (lines 798-817)

Tilføjet backend-detection og grob-rendering for BFHcharts plots:

```r
result_metadata <- spc_results()$metadata
is_bfhcharts <- !is.null(result_metadata$backend) &&
  result_metadata$backend == "bfhcharts"

if (is_bfhcharts) {
  # Convert to grob to bypass Shiny's ggplot_build()
  grob <- ggplot2::ggplotGrob(plot_result)
  grid::grid.draw(grob)
} else {
  # qicharts2 plot - normal rendering
  print(plot_result)
}
```

Dette bypasser Shiny's `ggplot_build()` → `setup_plot_labels()` pipeline der fejler med "unused argument" error.

### ✅ Remedy #3: Signal Column Fallback
**Fil:** `R/fct_spc_bfh_service.R` (lines 349-353)

Tilføjet explicit check og fallback for signal column:

```r
if (!"signal" %in% names(standardized$qic_data)) {
  standardized$qic_data$signal <- rep(FALSE, nrow(standardized$qic_data))
  log_warn("signal column missing from BFHcharts output - added default FALSE", .context = "BFH_SERVICE")
}
```

SPCify forventer signal column fra qicharts2, men BFHcharts leverer den ikke altid.

## Relaterede Issues

- `docs/issues/bfhcharts-integration-findings.md` - BFHcharts API findings
- Bug #1: target_value (delvist)
- Bug #4: y_axis_unit validation (breaking change)
