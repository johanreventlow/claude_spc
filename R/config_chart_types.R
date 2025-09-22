# Chart Types Configuration
# Extracted from global.R for better modularity

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