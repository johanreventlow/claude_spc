# System Configuration
# System-level constants for performance, logging, and operation timeouts

# APPLICATION CONFIGURATION ===================================================

#' Standard port for development server
#'
#' Port number brugt til udvikling af Shiny applikationen.
#' @export
DEFAULT_PORT <- 3838

#' Auto restore funktion
#'
#' Bestemmer om session data automatisk skal gendannes ved app start.
#' @export
AUTO_RESTORE_ENABLED <- FALSE

# FILE PROCESSING CONSTANTS ===================================================

#' Standard encoding for Windows kompatibilitet
#'
#' ISO-8859-1 encoding bruges for at sikre kompatibilitet med Windows systemer
#' og danske specialtegn i CSV filer.
#' @export
DEFAULT_ENCODING <- "ISO-8859-1"

#' Alternative encoding for UTF-8 filer
#'
#' UTF-8 encoding til moderne tekst filer og internationale tegnsæt.
#' @export
UTF8_ENCODING <- "UTF-8"

#' Standard CSV separators
#'
#' Liste over understøttede kolonne separatorer til CSV fil parsing.
#' @format Named list med separator karakterer
#' @export
CSV_SEPARATORS <- list(
  semicolon = ";",
  comma = ",",
  tab = "\t"
)

#' Standard decimal separators
#'
#' Liste over understøttede decimal separatorer til numerisk parsing.
#' @format Named list med decimal separator karakterer
#' @export
DECIMAL_SEPARATORS <- list(
  comma = ",",
  period = "."
)

# OBSERVER PRIORITIES ===========================================================

# OBSERVER_PRIORITIES moved to R/config_observer_priorities.R to avoid duplication

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

# PERFORMANCE CONSTANTS =========================================================

#' Timeout værdier for forskellige operationer (millisekunder)
#' @export
OPERATION_TIMEOUTS <- list(
  file_read = 30000, # 30 sekunder
  chart_render = 10000, # 10 sekunder
  auto_detect = 5000, # 5 sekunder
  ui_update = 2000 # 2 sekunder
)

#' Debounce delays for reactive operations
#'
#' PERFORMANCE OPTIMIZATION (2025-01-07):
#' Reduced delays baseret på agent performance analysis:
#' - input_change: 300 → 150ms (hurtigere dropdown feedback)
#' - chart_update: 800 → 500ms (reduceret perceived lag)
#' - Bevaret file_select og table_cleanup for stabilitet
#'
#' FORVENTET IMPACT: 30-40% forbedring i perceived responsiveness
#'
#' @export
DEBOUNCE_DELAYS <- list(
  input_change = 150, # 150ms - rapid user input (dropdown, typing) - OPTIMIZED
  file_select = 500, # 500ms - file selection and complex inputs
  chart_update = 500, # 500ms - chart rendering (reduced from 800ms) - OPTIMIZED
  table_cleanup = 2000 # 2000ms - table operation cleanup (conservative delay)
)

#' Loop protection delays for UI updates (milliseconds)
#' @export
LOOP_PROTECTION_DELAYS <- list(
  default = 500, # Standard delay for programmatic UI updates
  conservative = 800, # Conservative delay for slower browsers
  minimal = 200, # Minimal delay for fast responses
  onFlushed_fallback = 1000 # Fallback delay if session$onFlushed not available
)

#' Performance monitoring thresholds
#' @export
PERFORMANCE_THRESHOLDS <- list(
  reactive_warning = 0.5, # 500ms for reactive expressions
  debounce_warning = 1.0, # 1 second for debounced operations
  memory_warning = 10, # 10MB memory change warning
  cache_timeout_default = 300, # 5 minutes default cache
  max_cache_entries = 50 # Maximum cached reactive results
)

#' Rate limiting thresholds for security
#' @export
RATE_LIMITS <- list(
  file_upload_seconds = 2, # Minimum seconds between file uploads (DoS protection)
  api_call_seconds = 1, # Minimum seconds between API calls
  session_save_seconds = 5 # Minimum seconds between session saves
)

#' Auto-save debounce delays (milliseconds)
#' @export
AUTOSAVE_DELAYS <- list(
  data_save = 2000, # 2 seconds debounce for data auto-save
  settings_save = 1000 # 1 second debounce for settings auto-save (faster response)
)
