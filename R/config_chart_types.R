# ==============================================================================
# CONFIG_CHART_TYPES.R
# ==============================================================================
# FORMÅL: SPC chart type definitions og mappings mellem danske UI labels og
#         engelske qicharts2-koder. Centraliserer chart type logik og
#         beskrivelser.
#
# ANVENDES AF:
#   - UI dropdowns (chart type selection)
#   - Plot generation (qicharts2::qic interface)
#   - Input validation (chart type → nævner relevans)
#
# RELATERET:
#   - config_spc_config.R - Y-axis units og SPC validation
#   - fct_spc_plot_generation.R - Plot rendering
#   - See: docs/CONFIGURATION.md for complete guide
# ==============================================================================

# DIAGRAM TYPER ================================

## Dansk oversættelse af chart typer -----
CHART_TYPES_DA <- list(
  "Seriediagram med SPC (Run Chart)" = "run",
  "I-kort (Individuelle værdier)" = "i",
  "MR-kort (Moving Range)" = "mr",
  "P-kort (Andele)" = "p",
  "P'-kort (Andele, standardiseret)" = "pp",
  "U-kort (Rater)" = "u",
  "U'-kort (Rater, standardiseret)" = "up",
  "C-kort (Tællinger)" = "c",
  "G-kort (Tid mellem hændelser)" = "g"
)

## Omvendt mapping til engelske koder -----
CHART_TYPES_EN <- list(
  "run" = "run",
  "i" = "i",
  "mr" = "mr",
  "p" = "p",
  "pp" = "pp",
  "u" = "u",
  "up" = "up",
  "c" = "c",
  "g" = "g"
)

## Hjælpefunktion til konvertering -----
# Konverter danske displaynavne til engelske qic-koder
get_qic_chart_type <- function(danish_selection) {
  if (is.null(danish_selection) || danish_selection == "") {
    return("run") # standard
  }

  # Hvis det allerede er en engelsk kode, returner som-den-er
  if (danish_selection %in% unlist(CHART_TYPES_EN)) {
    return(danish_selection)
  }

  # Find mapping fra dansk til engelsk
  for (da_name in names(CHART_TYPES_DA)) {
    if (da_name == danish_selection) {
      return(CHART_TYPES_DA[[da_name]])
    }
  }

  # Fallback
  return("run")
}

## Chart type beskrivelser -----
CHART_TYPE_DESCRIPTIONS <- list(
  "run" = "Seriediagram der viser data over tid med median centerlinje",
  "i" = "I-kort til individuelle målinger",
  "mr" = "Moving Range kort til variabilitet mellem på hinanden følgende målinger",
  "p" = "P-kort til andele og procenter",
  "pp" = "P'-kort til standardiserede andele",
  "u" = "U-kort til rater og hændelser per enhed",
  "up" = "U'-kort til standardiserede rater",
  "c" = "C-kort til tællinger af defekter eller hændelser",
  "g" = "G-kort til tid mellem sjældne hændelser"
)

#' Kræver diagramtype en nævner (n)?
#'
#' Hjælper der afgør om nævner-feltet (n_column) er relevant for den valgte
#' diagramtype. Bruges til at styre UI (enable/disable) og til at undlade at
#' sende `n` til qicharts2 for irrelevante typer.
#'
#' Acceptér både danske labels og engelske qicharts2-koder.
#'
#' @param chart_type Valgt diagramtype (dansk label eller engelsk kode)
#' @return TRUE hvis nævner er relevant (skal være aktiv i UI), ellers FALSE
chart_type_requires_denominator <- function(chart_type) {
  # Normalisér til qicharts2-kode
  ct <- get_qic_chart_type(chart_type)

  # Nævner er relevant for run, p, pp, u, up
  return(ct %in% c("run", "p", "pp", "u", "up"))
}
