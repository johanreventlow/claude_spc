# utils_config_consolidation.R
# Centralized configuration management for SPC App
# Consoliderer spredte konfigurationsfiler til ensartet tilgang

#' Configuration Registry
#'
#' @description
#' Centralt registry for applikationskonfiguration. Samler alle
#' konfigurationsværdier fra spredte config-filer i et enkelt interface.
#'
#' @details
#' Dette system erstatter direct læsning fra multiple config-filer med
#' en ensartet konfigurationshandling. Alle konfigurationer loads on-demand
#' og caches for performance.
#'
#' @export
create_config_registry <- function() {
  # Private storage for cached config values
  .config_cache <- new.env(parent = emptyenv())
  .config_loaded <- new.env(parent = emptyenv())

  # Configuration loaders for each domain
  .load_chart_config <- function() {
    safe_operation(
      "Loading chart configuration",
      code = {
        # Package-baseret loading - chart types er nu hardcoded i stedet for sourced
        chart_types_da <- list(
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

        chart_type_descriptions <- list(
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

        list(
          chart_types_da = chart_types_da,
          chart_type_descriptions = chart_type_descriptions,
          default_chart = "i"
        )
      },
      fallback = list(chart_types_da = list(), chart_type_descriptions = list(), default_chart = "i"),
      error_type = "config_loading"
    )
  }

  .load_system_config <- function() {
    safe_operation(
      "Loading system configuration",
      code = {
        # Package-baseret loading - system constants hardcoded
        observer_priorities <- list(
          # Generic priorities (for compatibility)
          highest = 2000,    # Critical state management
          high = 1500,       # Data processing
          medium = 1000,     # Auto-detection
          low = 500,         # UI sync and updates
          lowest = 100,      # Cleanup operations

          # Specific named priorities used in event system
          STATE_MANAGEMENT = 1800,   # Data loading and state changes
          DATA_PROCESSING = 1500,    # Data transformation and processing
          AUTO_DETECT = 1200,        # Auto-detection operations
          UI_SYNC = 800,             # UI synchronization
          STATUS_UPDATES = 600,      # Status and navigation updates
          CLEANUP = 200,             # Cleanup operations
          MEDIUM = 1000              # Used in new input observers
        )

        system_settings <- list(
          DEFAULT_PORT = 3838,
          AUTO_RESTORE_ENABLED = FALSE,
          DEFAULT_ENCODING = "ISO-8859-1",
          UTF8_ENCODING = "UTF-8",
          CSV_SEPARATORS = list(
            semicolon = ";",
            comma = ",",
            tab = "\t"
          ),
          DECIMAL_SEPARATORS = list(
            comma = ",",
            period = "."
          ),
          OPERATION_TIMEOUTS = list(
            file_read = 30000,      # 30 sekunder
            chart_render = 10000,   # 10 sekunder
            auto_detect = 5000,     # 5 sekunder
            ui_update = 2000        # 2 sekunder
          ),
          DEBOUNCE_DELAYS = list(
            input_change = 300,     # 300ms
            file_select = 500,      # 500ms
            chart_update = 800      # 800ms
          ),
          LOOP_PROTECTION_DELAYS = list(
            default = 500,              # Standard delay for programmatic UI updates
            conservative = 800,         # Conservative delay for slower browsers
            minimal = 200,              # Minimal delay for fast responses
            onFlushed_fallback = 1000   # Fallback delay if session$onFlushed not available
          ),
          PERFORMANCE_THRESHOLDS = list(
            reactive_warning = 0.5,    # 500ms for reactive expressions
            debounce_warning = 1.0,    # 1 second for debounced operations
            memory_warning = 10,       # 10MB memory change warning
            cache_timeout_default = 300, # 5 minutes default cache
            max_cache_entries = 50     # Maximum cached reactive results
          ),
          LOG_COMPONENTS = list(
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
        )

        list(
          observer_priorities = observer_priorities,
          system_settings = system_settings
        )
      },
      fallback = list(observer_priorities = list(), system_settings = list()),
      error_type = "config_loading"
    )
  }

  .load_ui_config <- function() {
    safe_operation(
      "Loading UI configuration",
      code = {
        # Package-baseret loading - UI constants hardcoded
        ui_column_widths <- list(
          quarter = c(6, 6, 6, 6),
          half = c(6, 6),
          thirds = c(4, 4, 4),
          sidebar = c(3, 9)
        )

        ui_heights <- list(
          logo = "40px",
          modal_content = "300px",
          chart_container = "calc(50vh - 60px)",
          table_max = "200px",
          sidebar_min = "130px"
        )

        ui_styles <- list(
          flex_column = "display: flex; flex-direction: column; flex: 1 1 auto; min-height: 0;",
          scroll_auto = "max-height: 300px; overflow-y: auto;",
          full_width = "width: 100%;",
          right_align = "text-align: right;",
          margin_right = "margin-right: 10px;",
          position_absolute_right = "position: absolute; right: 20px; top: 20px; font-weight: bold;"
        )

        ui_input_widths <- list(
          full = "100%",
          half = "50%",
          quarter = "25%",
          three_quarter = "75%",
          auto = "auto"
        )

        ui_layout_proportions <- list(
          half = 1/2,
          third = 1/3,
          quarter = 1/4,
          two_thirds = 2/3,
          three_quarters = 3/4
        )

        list(
          ui_column_widths = ui_column_widths,
          ui_heights = ui_heights,
          ui_styles = ui_styles,
          ui_input_widths = ui_input_widths,
          ui_layout_proportions = ui_layout_proportions
        )
      },
      fallback = list(ui_column_widths = list(), ui_heights = list(), ui_styles = list(), ui_input_widths = list(), ui_layout_proportions = list()),
      error_type = "config_loading"
    )
  }

  .load_hospital_branding <- function() {
    safe_operation(
      "Loading hospital branding",
      code = {
        # Package-baseret loading - branding uses package getters
        list(
          hospital_name = get_package_hospital_name(),
          hospital_theme = get_package_theme(),
          hospital_logo_path = get_package_config("HOSPITAL_LOGO_PATH", default = "www/logo.png"),
          hospital_colors = get_package_config("HOSPITAL_COLORS", default = list()),
          hospital_ggplot_theme = get_package_config("HOSPITAL_THEME", default = NULL)
        )
      },
      fallback = list(hospital_name = "Unknown Hospital", hospital_theme = NULL, hospital_logo_path = "www/logo.png", hospital_colors = list(), hospital_ggplot_theme = NULL),
      error_type = "config_loading"
    )
  }

  .load_spc_config <- function() {
    safe_operation(
      "Loading SPC configuration",
      code = {
        # Package-baseret loading - SPC constants hardcoded
        spc_column_names <- list(
          x = c("Dato", "Date", "Tid", "Time", "Periode", "Period"),
          y = c("Tæller", "Count", "Værdi", "Value", "Antal", "Number"),
          n = c("Nævner", "Denominator", "Total", "Sum"),
          cl = c("Centerlinje", "CL", "Center", "Målværdi", "Target"),
          freeze = "Frys",
          shift = "Skift",
          comment = "Kommentar"
        )

        y_axis_units_da <- list(
          "Antal" = "count",
          "Procent (%)" = "percent",
          "Promille (‰)" = "permille",
          "Rate pr. 1000" = "rate_1000",
          "Rate pr. 100.000" = "rate_100000",
          "Dage" = "days",
          "Timer" = "hours",
          "Gram" = "grams",
          "Kilogram" = "kg",
          "Kroner" = "dkk"
        )

        spc_colors <- list(
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

        spc_settings <- list(
          MIN_SPC_ROWS = 10,
          RECOMMENDED_SPC_POINTS = 20,
          MAX_MISSING_PERCENT = 20,
          MIN_NUMERIC_PERCENT = 0.8,
          SPC_COLUMN_NAMES = spc_column_names,
          Y_AXIS_UNITS_DA = y_axis_units_da,
          SPC_COLORS = spc_colors,
          SPC_ALPHA_VALUES = list(
            target_line = 0.8,
            control_line = 0.7,
            data_point = 0.9,
            background = 0.1,
            highlight = 1.0
          ),
          SPC_LINE_TYPES = list(
            solid = "solid",
            dashed = "dashed",
            dotted = "dotted",
            dot_dash = "dotdash"
          ),
          SPC_LINE_WIDTHS = list(
            thin = 0.8,
            normal = 1.0,
            thick = 1.2,
            extra_thick = 1.5
          )
        )

        list(
          spc_settings = spc_settings,
          chart_defaults = list()
        )
      },
      fallback = list(spc_settings = list(), chart_defaults = list()),
      error_type = "config_loading"
    )
  }

  # Configuration domain loaders
  .config_loaders <- list(
    charts = .load_chart_config,
    system = .load_system_config,
    ui = .load_ui_config,
    branding = .load_hospital_branding,
    spc = .load_spc_config
  )

  # Public interface
  list(
    #' Get configuration for specific domain
    #'
    #' @param domain Configuration domain: "charts", "system", "ui", "branding", "spc"
    #' @param key Optional specific key within domain
    #' @param default Default value if key not found
    #' @return Configuration value or full domain config
    get = function(domain, key = NULL, default = NULL) {
      # Load domain config if not cached
      if (!exists(domain, envir = .config_loaded)) {
        if (domain %in% names(.config_loaders)) {
          log_debug("Loading configuration domain:", domain, .context = "CONFIG_REGISTRY")
          .config_cache[[domain]] <- .config_loaders[[domain]]()
          .config_loaded[[domain]] <- TRUE
        } else {
          log_warn(paste("Unknown configuration domain:", domain), "CONFIG_REGISTRY")
          return(default)
        }
      }

      config <- .config_cache[[domain]]

      if (is.null(key)) {
        return(config)
      }

      # Navigate nested keys (support dot notation)
      if (grepl("\\.", key)) {
        key_parts <- stringr::str_split(key, "\\.", simplify = TRUE)[1, ]
        value <- config
        for (part in key_parts) {
          if (is.list(value) && part %in% names(value)) {
            value <- value[[part]]
          } else {
            return(default)
          }
        }
        return(value)
      } else {
        return(config[[key]] %||% default)
      }
    },

    #' Reload specific configuration domain
    #'
    #' @param domain Domain to reload, or NULL for all domains
    reload = function(domain = NULL) {
      if (is.null(domain)) {
        # Reload all domains
        for (d in names(.config_loaders)) {
          if (exists(d, envir = .config_loaded)) {
            rm(list = d, envir = .config_loaded)
            rm(list = d, envir = .config_cache)
          }
        }
        log_info("All configuration domains reloaded", .context = "CONFIG_REGISTRY")
      } else {
        if (exists(domain, envir = .config_loaded)) {
          rm(list = domain, envir = .config_loaded)
          rm(list = domain, envir = .config_cache)
        }
        log_info(paste("Configuration domain reloaded:", domain), "CONFIG_REGISTRY")
      }
    },

    #' List available configuration domains
    #'
    #' @return Character vector of available domains
    domains = function() {
      names(.config_loaders)
    },

    #' Get configuration summary
    #'
    #' @return List with loading status and cache info
    summary = function() {
      loaded_domains <- ls(.config_loaded)
      cached_domains <- ls(.config_cache)

      list(
        available_domains = names(.config_loaders),
        loaded_domains = loaded_domains,
        cached_domains = cached_domains,
        cache_size = length(cached_domains)
      )
    }
  )
}

#' Get global configuration value with fallback
#'
#' @description
#' Convenience function for accessing configuration values with
#' automatic fallback handling and type conversion.
#'
#' @param path Configuration path in "domain.key" format (e.g., "charts.default_chart")
#' @param default Default value if configuration not found
#' @param type Expected type for conversion: "character", "logical", "numeric"
#' @param config_registry Optional config registry, uses global if NULL
#'
#' @return Configuration value converted to specified type
#' @export
#'
#' @examples
#' get_config("charts.default_chart", "i")
#' get_config("system.observer_priorities.HIGH", 1000, "numeric")
get_config <- function(path, default = NULL, type = "character", config_registry = NULL) {
  if (is.null(config_registry)) {
    # Use global registry if available
    if (exists(".spc_config_registry", envir = .GlobalEnv)) {
      config_registry <- get(".spc_config_registry", envir = .GlobalEnv)
    } else {
      log_warn("No global config registry found, creating temporary", .context = "CONFIG_REGISTRY")
      config_registry <- create_config_registry()
    }
  }

  parts <- stringr::str_split(path, "\\.", simplify = TRUE)[1, ]
  if (length(parts) < 2) {
    log_warn(paste("Invalid config path:", path), "CONFIG_REGISTRY")
    return(default)
  }

  domain <- parts[1]
  key <- paste(parts[-1], collapse = ".")

  value <- config_registry$get(domain, key, default)

  # Type conversion
  safe_operation(
    paste("Config type conversion:", path),
    code = {
      switch(type,
        "character" = as.character(value),
        "logical" = as.logical(value),
        "numeric" = as.numeric(value),
        value
      )
    },
    fallback = default,
    error_type = "config_conversion"
  )
}

#' Initialize global configuration registry
#'
#' @description
#' Sets up the global configuration registry for use throughout the application.
#' Should be called once during app initialization.
#'
#' @export
#'
#' @examples
#' init_global_config_registry()
init_global_config_registry <- function() {
  .spc_config_registry <- create_config_registry()
  assign(".spc_config_registry", .spc_config_registry, envir = .GlobalEnv)

  log_info("Global configuration registry initialized", .context = "CONFIG_REGISTRY")
  invisible(.spc_config_registry)
}

# Null coalescing operator defined in utils_logging.R