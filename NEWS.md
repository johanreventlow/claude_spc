# SPCify (development version)

## New Features

### Typst PDF Export (Issue #43)

* Added professional PDF export functionality using Typst typesetting system via Quarto
* New export format available in Export module alongside PNG and PowerPoint
* Generates A4 landscape PDFs with hospital branding (BFH template)
* Includes comprehensive metadata:
  - Hospital name and department
  - Chart title and analysis text
  - Data definition and technical details
  - SPC statistics (Anhøj rules: runs, crossings, outliers)
  - Author and date
* Template system supports:
  - Danish language throughout
  - Hospital logos and brand colors (Mari + Arial fonts)
  - Conditional rendering (SPC table and data definition sections)
  - Professional layout optimized for clinical reports

**New Functions:**
- `export_chart_for_typst()` - Export ggplot to PNG for Typst embedding
- `create_typst_document()` - Generate .typ files programmatically from R
- `compile_typst_to_pdf()` - Compile Typst to PDF via Quarto CLI
- `export_spc_to_typst_pdf()` - High-level orchestrator for complete workflow
- `extract_spc_statistics()` - Extract Anhøj rules from app_state
- `generate_details_string()` - Generate period/statistics summary
- `quarto_available()` - Check Quarto CLI availability (with RStudio fallback)
- `get_hospital_name_for_export()` - Get hospital name with fallback chain

**Technical Implementation:**
- Uses Quarto's bundled Typst CLI (>= v1.4, includes Typst 0.13+)
- Template files bundled in `inst/templates/typst/`
- Automatic template copying to temp directory for compilation
- Compatible with RStudio's bundled Quarto (macOS + Windows)
- Comprehensive test suite with Quarto availability detection

**Requirements:**
- Quarto >= 1.4 (for Typst support)
- Available via system installation or RStudio bundled version

## Bug Fixes

* Fixed Typst template syntax errors in conditional rendering
* Fixed Quarto compilation strategy (now uses `quarto typst compile`)
* Fixed template path resolution for temp directory compilation

## Internal Changes

* Added comprehensive Roxygen2 documentation for all export functions
* Added template README with usage examples and troubleshooting
* Improved error messages for missing dependencies

---

# SPCify 0.1.0 (Initial Development)

* Initial package structure
* Basic SPC chart functionality
* Core modules: data upload, visualization, export
