# PowerPoint Templates

## Overview

This directory contains PowerPoint templates used by the export functionality in SPCify.

## Hospital Presentation Template

**File:** `hospital_presentation.pptx`

Place your hospital-branded PowerPoint template in this directory. The export module will automatically use it when generating PowerPoint exports.

### Template Requirements

Your template should have **at least one slide** with the following placeholders:

1. **Title Placeholder** - For the SPC chart title
2. **Content/Body Placeholder** - For the chart image

### Standard Layout Example

A typical "Title and Content" layout works perfectly:

```
+----------------------------------+
|  [Title Placeholder]             |
+----------------------------------+
|                                  |
|  [Content/Body Placeholder]      |
|  (Chart will be inserted here)   |
|                                  |
+----------------------------------+
```

### How It Works

1. **Template Detection:**
   - The export module checks if `inst/templates/hospital_presentation.pptx` exists
   - If found, it loads your template
   - If not found, it creates a blank default presentation

2. **Content Insertion:**
   - The SPC chart title is inserted into the title placeholder
   - The chart PNG is inserted into the content/body placeholder
   - Chart is rendered at PowerPoint-optimal size: 10×7.5 inches @ 96 DPI

3. **Fallback Positioning:**
   - If standard placeholders are not found, the module uses fallback positioning
   - This ensures exports work even with non-standard templates

### Inspecting Your Template

To inspect your template structure using R:

```r
library(officer)

# Load template
pptx_doc <- read_pptx("inst/templates/hospital_presentation.pptx")

# View available layouts
layout_summary(pptx_doc)

# View layout properties
layout_properties(pptx_doc)

# View slide properties
slide_summary(pptx_doc)
```

### Testing Your Template

After placing your template:

1. Restart the Shiny app
2. Go to the "Eksport" tab
3. Select "PowerPoint" format
4. Enter a title and download

The exported PowerPoint should use your hospital branding and layout.

### Troubleshooting

**Template not being used?**
- Check file path: `inst/templates/hospital_presentation.pptx`
- Ensure filename matches exactly (case-sensitive)
- Verify file is a valid PowerPoint file (.pptx)
- Restart R session after adding template

**Chart not positioning correctly?**
- Ensure your template has "Title and Content" layout
- Check if placeholders are properly configured
- The module will use fallback positioning if needed

**Template compatibility:**
- Works with PowerPoint 2007+ (.pptx format)
- Compatible with LibreOffice Impress
- Compatible with Keynote (with .pptx export)

### Custom Layouts

If you have custom layouts beyond "Title and Content":

1. The module will detect available placeholders automatically
2. It tries multiple fallback strategies:
   - `ph_location_type(type = "title")` for title
   - `ph_location_type(type = "body")` for content
   - Manual positioning if placeholders not found

### Hospital Branding

Your template can include:
- Hospital logo
- Brand colors
- Custom fonts
- Footer information
- Slide numbers
- Master slide formatting

All branding will be preserved when the chart is inserted.

## Notes

- Template file is not version controlled (add to `.gitignore` if contains sensitive branding)
- Update template by replacing the file in this directory
- No code changes needed when updating template
- Template is package-installed, so it works in deployed apps

## Example Template Creation

To create a minimal template:

```r
library(officer)

# Create new presentation
pptx_doc <- read_pptx()

# Add a slide with "Title and Content" layout
pptx_doc <- add_slide(pptx_doc, layout = "Title and Content")

# Save as template
print(pptx_doc, target = "inst/templates/hospital_presentation.pptx")
```

Then customize the saved file in PowerPoint with your hospital branding.
