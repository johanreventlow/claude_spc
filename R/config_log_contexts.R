# ==============================================================================
# CONFIG_LOG_CONTEXTS.R
# ==============================================================================
# FORMÅL: Centraliserede log context strings for struktureret logging gennem
#         hele applikationen. Eliminerer hardcoded strings og muliggør nem
#         refaktorering via hierarchical context organization.
#
# ANVENDES AF:
#   - Alle logging calls: log_debug(), log_info(), log_warn(), log_error()
#   - Struktureret fejlfinding og debugging
#   - Log filtering og analyse
#
# RELATERET:
#   - utils_logging.R - Logging system implementation
#   - CLAUDE.md Section 2.4 - Observability & Debugging
#   - See: docs/CONFIGURATION.md for complete guide
#
# BRUG:
#   log_debug("message", .context = LOG_CONTEXTS$data$process)
#   log_info("message", .context = LOG_CONTEXTS$performance$cache)
# ==============================================================================

#' Log Context Constants
#'
#' Centraliseret konfiguration for log context strings.
#' Organiseret i logiske kategorier for nem navigation.
#'
#' @format List med følgende kategorier:
#' \describe{
#'   \item{data}{Data processing contexts}
#'   \item{autodetect}{Column auto-detection contexts}
#'   \item{performance}{Performance monitoring contexts}
#'   \item{qic}{QIC/SPC calculation contexts}
#'   \item{ui}{UI and visualization contexts}
#'   \item{column}{Column management contexts}
#'   \item{app}{App lifecycle contexts}
#'   \item{navigation}{Navigation contexts}
#'   \item{test}{Test mode contexts}
#'   \item{file}{File operation contexts}
#'   \item{security}{Security contexts}
#'   \item{config}{Configuration contexts}
#'   \item{startup}{Startup and initialization contexts}
#'   \item{cache}{Cache management contexts}
#'   \item{debug}{Debug and development contexts}
#'   \item{misc}{Miscellaneous contexts}
#' }
#'
#' @export
LOG_CONTEXTS <- list(
  # === Data Processing ===
  data = list(
    process = "DATA_PROCESS",
    proc = "DATA_PROC", # Legacy, brug 'process'
    validation = "DATA_VALIDATION",
    table = "DATA_TABLE"
  ),

  # === Auto Detection ===
  autodetect = list(
    unified = "UNIFIED_AUTODETECT",
    cache = "AUTO_DETECT_CACHE",
    event = "AUTO_DETECT_EVENT",
    decisions = "AUTODETECT_DECISIONS",
    setup = "AUTODETECT_SETUP",
    name_based = "NAME_BASED_DETECT",
    full_data = "FULL_DATA_DETECT",
    date = "DATE_DETECT",
    numeric = "NUMERIC_DETECT",
    scoring = "COLUMN_SCORING"
  ),

  # === Performance Monitoring ===
  performance = list(
    general = "PERFORMANCE",
    benchmark = "PERFORMANCE_BENCHMARK",
    cache = "PERFORMANCE_CACHE",
    monitor = "PERFORMANCE_MONITOR",
    monitoring = "PERFORMANCE_MONITORING",
    opt = "PERFORMANCE_OPT",
    setup = "PERFORMANCE_SETUP",
    timing = "TIMING_MONITOR"
  ),

  # === QIC/SPC Calculations ===
  qic = list(
    general = "QIC",
    call = "QIC_CALL",
    error = "QIC_ERROR",
    input = "QIC_INPUT",
    preparation = "QIC_PREPARATION",
    result = "QIC_RESULT",
    timing = "QIC_TIMING",
    spc_debug = "SPC_CALC_DEBUG",
    pipeline = "SPC_PIPELINE"
  ),

  # === UI & Visualization ===
  ui = list(
    visualization = "VISUALIZATION",
    render = "RENDER_PLOT",
    plot_optimization = "PLOT_OPTIMIZATION",
    plot_comment = "PLOT_COMMENT",
    x_axis_format = "X_AXIS_FORMAT",
    y_axis_scaling = "Y_AXIS_SCALING",
    sync = "[UI_SYNC]", # Bracket format for consistency with legacy
    y_axis_ui = "[Y_AXIS_UI]"
  ),

  # === Column Management ===
  column = list(
    mgmt = "COLUMN_MGMT",
    choices_unified = "COLUMN_CHOICES_UNIFIED",
    scoring = "COLUMN_SCORING"
  ),

  # === App Lifecycle ===
  app = list(
    init = "APP_INIT",
    server = "APP_SERVER",
    config = "APP_CONFIG",
    session_cleanup = "SESSION_CLEANUP",
    session_reset = "SESSION_RESET",
    memory_mgmt = "MEMORY_MGMT",
    background_cleanup = "BACKGROUND_CLEANUP"
  ),

  # === Navigation ===
  navigation = list(
    unified = "NAVIGATION_UNIFIED",
    welcome = "WELCOME_PAGE"
  ),

  # === Test Mode ===
  test = list(
    general = "TEST_MODE",
    startup = "[TEST_MODE_STARTUP]",
    demo_data = "DEMO_DATA"
  ),

  # === File Operations ===
  file = list(
    upload = "FILE_UPLOAD",
    upload_security = "FILE_UPLOAD_SECURITY",
    validation = "[FILE_VALIDATION]"
  ),

  # === Security ===
  security = list(
    general = "[SECURITY]",
    input_sanitization = "[INPUT_SANITIZATION]"
  ),

  # === Configuration ===
  config = list(
    apply = "CONFIG_APPLY",
    convert = "CONFIG_CONVERT",
    registry = "CONFIG_REGISTRY",
    runtime = "RUNTIME_CONFIG"
  ),

  # === Startup & Golem ===
  startup = list(
    cache = "STARTUP_CACHE",
    optimization = "STARTUP_OPTIMIZATION",
    golem_apply = "GOLEM_APPLY",
    golem_env = "GOLEM_ENV",
    golem_fallback = "GOLEM_FALLBACK",
    lazy_loading = "LAZY_LOADING"
  ),

  # === Cache Management ===
  cache = list(
    generator = "CACHE_GENERATOR",
    invalidation = "CACHE_INVALIDATION",
    performance = "[PERFORMANCE_CACHE]"
  ),

  # === Debug & Development ===
  debug = list(
    general = "DEBUG",
    advanced = "ADVANCED_DEBUG",
    dev_mode = "DEV_MODE",
    prod_mode = "PROD_MODE",
    benchmark = "[BENCHMARK]",
    microbenchmark = "MICROBENCHMARK"
  ),

  # === Miscellaneous ===
  misc = list(
    emit_api = "EMIT_API",
    error_system = "ERROR_SYSTEM",
    loop_protection = "LOOP_PROTECTION",
    anhoej_comparison = "ANHOEJ_COMPARISON",
    branding_verification = "BRANDING_VERIFICATION",
    favicon = "FAVICON",
    package_verification = "PACKAGE_VERIFICATION",
    resource_paths = "RESOURCE_PATHS",
    shinylogs = "SHINYLOGS",
    title_processing = "TITLE_PROCESSING",
    user_interaction = "USER_INTERACTION",
    verification = "VERIFICATION",
    pipeline = "PIPELINE"
  )
)

#' Get log context by category and name
#'
#' Helper function to get a specific log context with validation.
#'
#' @param category Category name (fx "data", "performance", "qic")
#' @param context Context name within category
#' @return Context string
#'
#' @examples
#' get_log_context("data", "process") # Returns "DATA_PROCESS"
#' get_log_context("qic", "input") # Returns "QIC_INPUT"
#'
#' @export
get_log_context <- function(category, context) {
  if (!category %in% names(LOG_CONTEXTS)) {
    stop(
      "Unknown log context category: '", category, "'. ",
      "Available categories: ", paste(names(LOG_CONTEXTS), collapse = ", ")
    )
  }

  category_contexts <- LOG_CONTEXTS[[category]]

  if (!context %in% names(category_contexts)) {
    stop(
      "Unknown context '", context, "' in category '", category, "'. ",
      "Available contexts: ", paste(names(category_contexts), collapse = ", ")
    )
  }

  return(category_contexts[[context]])
}

#' List all available log contexts
#'
#' Returns a flat list of all available log context strings.
#' Useful for validation or documentation.
#'
#' @return Character vector of all log context strings
#'
#' @examples
#' all_contexts <- list_all_log_contexts()
#' length(all_contexts) # Total number of contexts
#'
#' @export
list_all_log_contexts <- function() {
  all_contexts <- character()

  for (category in names(LOG_CONTEXTS)) {
    category_contexts <- LOG_CONTEXTS[[category]]
    all_contexts <- c(all_contexts, unlist(category_contexts, use.names = FALSE))
  }

  return(unique(all_contexts))
}
