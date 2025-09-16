# constants.R
# Centraliserede konstanter for SPC App
# Organiseret efter funktionalitet for bedre vedligeholdelse

# APPLICATION CONFIGURATION ===================================================

#' Standard port for development server
#' @export
DEFAULT_PORT <- 3838

#' Test mode konfiguration
#' @export
TEST_MODE_AUTO_LOAD <- TRUE

#' Standard test data fil path
#' @export
TEST_MODE_FILE_PATH <- "R/data/spc_exampledata.csv"

#' Auto restore funktion
#' @export
AUTO_RESTORE_ENABLED <- FALSE

# FILE PROCESSING CONSTANTS ===================================================

#' Standard encoding for Windows kompatibilitet
#' @export
DEFAULT_ENCODING <- "ISO-8859-1"

#' Alternative encoding for UTF-8 filer
#' @export
UTF8_ENCODING <- "UTF-8"

#' Standard CSV separators
#' @export
CSV_SEPARATORS <- list(
  semicolon = ";",
  comma = ",",
  tab = "\t"
)

#' Standard decimal separators
#' @export
DECIMAL_SEPARATORS <- list(
  comma = ",",
  period = "."
)

# UI LAYOUT CONSTANTS ==========================================================

#' Standard kolonne bredder for UI
#' @export
UI_COLUMN_WIDTHS <- list(
  quarter = c(6, 6, 6, 6),
  half = c(6, 6),
  thirds = c(4, 4, 4),
  sidebar = c(3, 9)
)

#' Standard højder for UI komponenter
#' @export
UI_HEIGHTS <- list(
  logo = "40px",
  modal_content = "300px",
  chart_container = "calc(50vh - 60px)",
  table_max = "200px",
  sidebar_min = "130px"
)

#' CSS styles constants
#' @export
UI_STYLES <- list(
  flex_column = "display: flex; flex-direction: column; flex: 1 1 auto; min-height: 0;",
  scroll_auto = "max-height: 300px; overflow-y: auto;",
  full_width = "width: 100%;",
  right_align = "text-align: right;",
  margin_right = "margin-right: 10px;",
  position_absolute_right = "position: absolute; right: 20px; top: 20px; font-weight: bold;"
)

#' Standard UI input widths
#' @export
UI_INPUT_WIDTHS <- list(
  full = "100%",
  half = "50%",
  quarter = "25%",
  three_quarter = "75%",
  auto = "auto"
)

#' Layout proportions for consistent UI
#' @export
UI_LAYOUT_PROPORTIONS <- list(
  half = 1/2,
  third = 1/3,
  quarter = 1/4,
  two_thirds = 2/3,
  three_quarters = 3/4
)

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

#' Standard SPC chart typer mapping (dansk til engelsk)
#' @export
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

#' Tilsvarende engelske chart typer
#' @export
CHART_TYPES_EN <- list(
  "I-chart (individual values)" = "i",
  "MR-chart (moving range)" = "mr",
  "Xbar-chart (mean)" = "xbar",
  "S-chart (standard deviation)" = "s",
  "P-chart (proportions)" = "p",
  "PP-chart (standardized proportions)" = "pp",
  "C-chart (count)" = "c",
  "U-chart (rates)" = "u",
  "G-chart (geometric)" = "g",
  "T-chart (time between events)" = "t"
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

# OBSERVER PRIORITIES ===========================================================

#' Shiny observer prioriteter for kontrolleret execution orden
#' @export
OBSERVER_PRIORITIES <- list(
  highest = 1000,
  high = 800,
  medium = 500,
  low = 200,
  lowest = 100
)

# LOGGING CONSTANTS =============================================================

#' Logging komponenter for organiseret fejlfinding
#' @export
LOG_COMPONENTS <- list(
  DATA_PROC = "DATA_PROC",
  AUTO_DETECT = "AUTO_DETECT",
  FILE_UPLOAD = "FILE_UPLOAD",
  VISUALIZATION = "VISUALIZATION",
  ERROR_HANDLING = "ERROR_HANDLING",
  TEST_MODE = "TEST_MODE",
  SESSION_MGMT = "SESSION_MGMT",
  UI_SYNC = "UI_SYNC",
  STATE_MGMT = "STATE_MGMT"
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

# PERFORMANCE CONSTANTS =========================================================

#' Timeout værdier for forskellige operationer (millisekunder)
#' @export
OPERATION_TIMEOUTS <- list(
  file_read = 30000,      # 30 sekunder
  chart_render = 10000,   # 10 sekunder
  auto_detect = 5000,     # 5 sekunder
  ui_update = 2000        # 2 sekunder
)

#' Debounce delays for reactive operations
#' @export
DEBOUNCE_DELAYS <- list(
  input_change = 300,     # 300ms
  file_select = 500,      # 500ms
  chart_update = 800      # 800ms
)

#' Performance monitoring thresholds
#' @export
PERFORMANCE_THRESHOLDS <- list(
  reactive_warning = 0.5,    # 500ms for reactive expressions
  debounce_warning = 1.0,    # 1 second for debounced operations
  memory_warning = 10,       # 10MB memory change warning
  cache_timeout_default = 300, # 5 minutes default cache
  max_cache_entries = 50     # Maximum cached reactive results
)