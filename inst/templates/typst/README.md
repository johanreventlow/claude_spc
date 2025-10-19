# BFH Typst Templates for SPC Reports

This directory contains Typst templates for generating professional PDF reports with Bispebjerg og Frederiksberg Hospital branding.

## Files

- `bfh-template/bfh-template.typ` - Main template file with `bfh-diagram2()` function
- `bfh_horisonal.typ` - Example usage showing all configurable parameters
- `bfh-template/images/` - Hospital logos and branding assets
- `bfh-template/fonts/` - Mari and Arial fonts for hospital branding

## Template: bfh-diagram2

### Description

Generates A4 landscape PDF with:
- Hospital branding (logo, colors, fonts)
- SPC chart visualization
- Anhøj rules statistics table
- Metadata (title, department, analysis, data definition)

### Parameters

All parameters are optional except `chart`:

#### Branding
- `hospital` (string) - Hospital name (default: "Bispebjerg og Frederiksberg Hospital")
- `department` (content) - Department/unit name

#### Chart Metadata
- `title` (content) - Chart title (supports rich text and line breaks)
- `details` (content) - Period info, averages, current level
- `author` (string) - Author name
- `date` (datetime) - Report date (default: today)

#### Analysis & Interpretation
- `analysis` (content) - Analysis text with findings and recommendations
- `data_definition` (content) - Data definition explaining the indicator

#### SPC Statistics (Anhøj Rules)
- `runs_expected` (int) - Expected serielængde value
- `runs_actual` (int) - Actual serielængde value
- `crossings_expected` (int) - Expected antal kryds value
- `crossings_actual` (int) - Actual antal kryds value
- `outliers_expected` (int) - Expected obs. uden for kontrolgrænse
- `outliers_actual` (int) - Actual obs. uden for kontrolgrænse

**Note:** SPC statistics table only appears if at least one statistic is provided.

#### Chart Content
- `chart` (content) - **REQUIRED** Chart content (typically an image)

### Example Usage

```typst
#import "bfh-template/bfh-template.typ" : *

#show: chart => bfh-diagram2(
  hospital: "Bispebjerg og Frederiksberg Hospital",
  department: [Geriatrisk Sengeafsnit G16],
  title: [Medicinsikkert hospital \ *Scanning ved medicinadsministration*],
  details: [Periode: feb. 2019 – mar. 2022 • Gns. måned: 58938/97266],
  analysis: [*Analyse tekst her*...],
  data_definition: [Beskriv indikatoren og datakilde...],
  runs_expected: 12,
  runs_actual: 92,
  crossings_expected: 16,
  crossings_actual: 10,
  outliers_expected: 0,
  outliers_actual: 3,
  author: "Johan Reventlow",
  date: datetime(year: 2025, month: 1, day: 19),
  chart
)

// Chart image
#image("path/to/chart.png", height: 115mm)
```

## Compilation

### Via Quarto (Recommended)

```bash
quarto render bfh_horisonal.typ --to pdf
```

### Via Typst CLI

```bash
typst compile bfh_horisonal.typ
```

### From R

```r
library(quarto)
quarto_render("inst/templates/typst/bfh_horisonal.typ", output_format = "pdf")
```

## Integration with R/Shiny

See `R/fct_export_typst.R` for R functions that:
1. Generate `.typ` files programmatically from R data
2. Export SPC charts as PNG for embedding
3. Compile Typst to PDF via Quarto
4. Orchestrate the full export workflow

## Design Specifications

- **Page format:** A4 landscape (297mm × 210mm)
- **Margins:** 4.67mm all sides
- **Fonts:**
  - Headers: Mari (custom hospital font)
  - Body: Arial
  - Language: Danish (da)
- **Colors:**
  - Primary blue: `#007dbb` (BFH brand color)
  - Text gray: `#888888`
  - Background gray: `#e4e5ea`
- **Logo placement:** Hospital watermark at dy: 46.7mm, dx: 4.6mm

## Customization

To modify the template:

1. Edit `bfh-template/bfh-template.typ`
2. Test changes with `bfh_horisonal.typ`
3. Rebuild R package: `devtools::install()`
4. Changes are available via `system.file("templates/typst/...", package = "SPCify")`

## Requirements

- **Quarto** ≥ 1.4 (with Typst support) OR **Typst CLI**
- **Fonts:** Mari and Arial (included in `bfh-template/fonts/`)
- **Images:** Hospital logos (included in `bfh-template/images/`)

## Troubleshooting

### "Font not found" errors
Ensure fonts are in `bfh-template/fonts/` directory. Typst will look for fonts relative to the template file.

### Image paths
Image paths are relative to the `.typ` file location. From R, use absolute paths or ensure working directory is correct.

### Table not showing
SPC statistics table only appears if at least one of the six statistics (`runs_expected`, `runs_actual`, etc.) is provided.

### Data definition not showing
Data definition section only appears if `data_definition` parameter is provided.

## License

Internal use for Bispebjerg og Frederiksberg Hospital. Hospital branding assets (logos, fonts) are property of the hospital.
