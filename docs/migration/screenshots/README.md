# Screenshots Directory - BFHchart vs qicharts2 Comparison

**Purpose:** Side-by-side visual comparisons for clinical validation

---

## Directory Structure

```
screenshots/
├── README.md (this file)
├── qic-run-basic.png
├── bfh-run-basic.png
├── qic-run-anhoej.png
├── bfh-run-anhoej.png
├── qic-run-freeze.png
├── bfh-run-freeze.png
├── qic-i-basic.png
├── bfh-i-basic.png
... (42 total screenshots)
```

**Naming Convention:** `{package}-{chart_type}-{scenario}.png`

- **package:** `qic` (qicharts2) or `bfh` (BFHchart)
- **chart_type:** `run`, `i`, `p`, `c`, `u`, `xbar`, `s`
- **scenario:** `basic`, `anhoej`, `freeze`

**Total Screenshots:** 42 (21 pairs)

---

## Screenshot Generation Script

**Prerequisites:**
```r
# Ensure baselines generated
source("tests/testthat/fixtures/generate-qic-baselines.R")

# Install required packages
library(qicharts2)
library(BFHchart)
library(ggplot2)
```

**Generation Script:**

```r
# Create screenshots directory if needed
dir.create("docs/migration/screenshots", recursive = TRUE, showWarnings = FALSE)

# Chart types and scenarios
chart_types <- c("run", "i", "p", "c", "u", "xbar", "s")
scenarios <- c("basic", "anhoej", "freeze")

# Standard plot dimensions
plot_width <- 8  # inches
plot_height <- 6  # inches
plot_dpi <- 150  # resolution

# Function to generate qicharts2 screenshot
generate_qic_screenshot <- function(chart_type, scenario) {
  # Load baseline
  baseline_path <- sprintf(
    "tests/testthat/fixtures/qic-baseline/%s-%s.rds",
    chart_type,
    scenario
  )

  if (!file.exists(baseline_path)) {
    warning(sprintf("Baseline not found: %s", baseline_path))
    return(FALSE)
  }

  baseline <- readRDS(baseline_path)

  # Generate qicharts2 plot
  plot <- qic(
    data = baseline$input_data,
    x = names(baseline$input_data)[1],
    y = names(baseline$input_data)[2],
    chart = chart_type
  )

  # Save screenshot
  screenshot_path <- sprintf(
    "docs/migration/screenshots/qic-%s-%s.png",
    chart_type,
    scenario
  )

  ggsave(
    filename = screenshot_path,
    plot = plot,
    width = plot_width,
    height = plot_height,
    dpi = plot_dpi,
    bg = "white"
  )

  message(sprintf("Generated: %s", screenshot_path))
  return(TRUE)
}

# Function to generate BFHchart screenshot
generate_bfh_screenshot <- function(chart_type, scenario) {
  # Load baseline
  baseline_path <- sprintf(
    "tests/testthat/fixtures/qic-baseline/%s-%s.rds",
    chart_type,
    scenario
  )

  if (!file.exists(baseline_path)) {
    warning(sprintf("Baseline not found: %s", baseline_path))
    return(FALSE)
  }

  baseline <- readRDS(baseline_path)

  # Load service layer
  source("R/fct_spc_bfh_service.R")

  # Generate BFHchart plot
  result <- compute_spc_results_bfh(
    data = baseline$input_data,
    x_var = names(baseline$input_data)[1],
    y_var = names(baseline$input_data)[2],
    chart_type = chart_type
  )

  # Save screenshot
  screenshot_path <- sprintf(
    "docs/migration/screenshots/bfh-%s-%s.png",
    chart_type,
    scenario
  )

  ggsave(
    filename = screenshot_path,
    plot = result$plot,
    width = plot_width,
    height = plot_height,
    dpi = plot_dpi,
    bg = "white"
  )

  message(sprintf("Generated: %s", screenshot_path))
  return(TRUE)
}

# Generate all screenshots
for (chart_type in chart_types) {
  for (scenario in scenarios) {
    message(sprintf("\n=== Processing: %s - %s ===", chart_type, scenario))

    # Generate qicharts2 screenshot
    qic_success <- generate_qic_screenshot(chart_type, scenario)

    # Generate BFHchart screenshot
    bfh_success <- generate_bfh_screenshot(chart_type, scenario)

    if (qic_success && bfh_success) {
      message(sprintf("✓ Pair complete: %s-%s", chart_type, scenario))
    } else {
      warning(sprintf("✗ Pair incomplete: %s-%s", chart_type, scenario))
    }
  }
}

message("\n=== Screenshot generation complete ===")
message("Total pairs: 21 (42 individual screenshots)")
```

---

## Screenshot Validation Checklist

After generation, verify each screenshot:

**Technical Quality:**
- [ ] Resolution: 1200x900 pixels (8" × 6" at 150 DPI)
- [ ] File format: PNG with white background
- [ ] File size: <500 KB per image
- [ ] No clipping or truncation

**Visual Quality:**
- [ ] All text readable
- [ ] Axis labels visible
- [ ] Control limits clear
- [ ] Data points distinguishable
- [ ] Danish characters (æøå) rendered correctly

**Content Accuracy:**
- [ ] Matches expected scenario (basic/anhoej/freeze)
- [ ] Chart type correct
- [ ] Anhøj signals visible (if anhoej scenario)
- [ ] Freeze markers visible (if freeze scenario)

---

## Usage in Documentation

**Reference in Markdown:**
```markdown
| qicharts2 | BFHchart |
|-----------|----------|
| ![qicharts2](screenshots/qic-run-basic.png) | ![BFHchart](screenshots/bfh-run-basic.png) |
```

**Linked in:**
- `docs/migration/bfh-visual-differences.md` - Side-by-side comparisons
- `docs/migration/clinical-validation-checklist.md` - Appendix B reference

---

## Screenshot Refresh Schedule

**When to regenerate:**
1. BFHchart version upgrade
2. Service layer changes (`R/fct_spc_bfh_service.R`)
3. Workaround updates (`R/fct_anhoej_rules.R`)
4. Baseline fixture updates (Task #29 re-run)

**Regeneration process:**
1. Delete existing screenshots: `rm docs/migration/screenshots/*.png`
2. Run generation script (above)
3. Run validation checklist
4. Update `bfh-visual-differences.md` with observations
5. Commit updated screenshots

---

## File Size Management

**Current Status:** [To be updated after generation]

**Target:** <10 MB total directory size

**Optimization (if needed):**
```bash
# Use pngquant for lossless compression
for file in docs/migration/screenshots/*.png; do
  pngquant --quality=80-95 --ext .png --force "$file"
done
```

---

**Last Updated:** 2025-10-15
**Maintained By:** SPCify Development Team (Task #31 Stream C)
