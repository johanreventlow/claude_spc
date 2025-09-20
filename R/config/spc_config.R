# SPC Configuration
# Statistical Process Control specific constants and visualization settings

# DATA VALIDATION CONSTANTS ====================================================

#' Minimum antal rækker for SPC analyse
#' @export
MIN_SPC_ROWS <- 10

#' Anbefalet minimum antal punkter for SPC
#' @export
RECOMMENDED_SPC_POINTS <- 20

#' Maximum missing values procent før advarsel
#' @export
MAX_MISSING_PERCENT <- 20

#' Minimum procent numeriske værdier for kolonne detection
#' @export
MIN_NUMERIC_PERCENT <- 0.8

# SPC CONFIGURATION CONSTANTS ==================================================

#' Standard kolonne navne for SPC analyse
#' @export
SPC_COLUMN_NAMES <- list(
  x = c("Dato", "Date", "Tid", "Time", "Periode", "Period"),
  y = c("Tæller", "Count", "Værdi", "Value", "Antal", "Number"),
  n = c("Nævner", "Denominator", "Total", "Sum"),
  cl = c("Centerlinje", "CL", "Center", "Målværdi", "Target"),
  freeze = "Frys",
  shift = "Skift",
  comment = "Kommentar"
)

#' Y-aksen enheder for forskellige chart typer
#' @export
Y_AXIS_UNITS_DA <- list(
  "Antal" = "antal",
  "Procent" = "procent",
  "Rate pr. 1000" = "rate_1000",
  "Rate pr. 10000" = "rate_10000",
  "Promille" = "promille",
  "Brøk" = "fraction"
)

# SPC VISUALIZATION CONSTANTS ===================================================

#' Farve palette for SPC charts
#' @export
SPC_COLORS <- list(
  # Target linjer
  target_line = "#2E8B57",        # SeaGreen for målværdi linjer
  control_line = "#FF6B6B",       # Coral for kontrolgrænser

  # Data punkter
  normal_point = "#4A90E2",       # Blå for normale datapunkter
  special_cause = "#FF4444",      # Rød for special cause punkter

  # Chart baggrund
  chart_bg = "#FFFFFF",           # Hvid baggrund
  grid_line = "#E8E8E8",          # Lys grå for grid

  # UI elementer
  success = "#28A745",            # Grøn for success states
  warning = "#FFC107",            # Gul for warnings
  error = "#DC3545",              # Rød for errors
  info = "#17A2B8"                # Blå for info
)

#' Alpha værdier for gennemsigtighed
#' @export
SPC_ALPHA_VALUES <- list(
  target_line = 0.8,
  control_line = 0.7,
  data_point = 0.9,
  background = 0.1,
  highlight = 1.0
)

#' Linje typer for SPC charts
#' @export
SPC_LINE_TYPES <- list(
  solid = "solid",
  dashed = "dashed",
  dotted = "dotted",
  dot_dash = "dotdash"
)

#' Standard linje bredder
#' @export
SPC_LINE_WIDTHS <- list(
  thin = 0.8,
  normal = 1.0,
  thick = 1.2,
  extra_thick = 1.5
)