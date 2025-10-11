# utils_config_consolidation.R
# Centralized configuration management for SPC App
# Consoliderer spredte konfigurationsfiler til ensartet tilgang

#' Configuration Registry
#'
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
        # Genbruger autoritativ config fra config_chart_types.R
        list(
          chart_types_da = CHART_TYPES_DA,
          chart_type_descriptions = CHART_TYPE_DESCRIPTIONS,
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
        # Genbruger autoritative configs fra config_observer_priorities.R og config_system_config.R
        system_settings <- list(
          DEFAULT_PORT = DEFAULT_PORT,
          AUTO_RESTORE_ENABLED = AUTO_RESTORE_ENABLED,
          DEFAULT_ENCODING = DEFAULT_ENCODING,
          UTF8_ENCODING = UTF8_ENCODING,
          CSV_SEPARATORS = CSV_SEPARATORS,
          DECIMAL_SEPARATORS = DECIMAL_SEPARATORS,
          OPERATION_TIMEOUTS = OPERATION_TIMEOUTS,
          DEBOUNCE_DELAYS = DEBOUNCE_DELAYS,
          LOOP_PROTECTION_DELAYS = LOOP_PROTECTION_DELAYS,
          PERFORMANCE_THRESHOLDS = PERFORMANCE_THRESHOLDS,
          LOG_COMPONENTS = LOG_COMPONENTS
        )

        list(
          observer_priorities = OBSERVER_PRIORITIES,
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
        # Genbruger autoritativ config fra config_ui.R
        list(
          ui_column_widths = UI_COLUMN_WIDTHS,
          ui_heights = UI_HEIGHTS,
          ui_styles = UI_STYLES,
          ui_input_widths = UI_INPUT_WIDTHS,
          ui_layout_proportions = UI_LAYOUT_PROPORTIONS
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
        # Genbruger autoritativ config fra config_spc_config.R
        spc_settings <- list(
          MIN_SPC_ROWS = MIN_SPC_ROWS,
          RECOMMENDED_SPC_POINTS = RECOMMENDED_SPC_POINTS,
          MAX_MISSING_PERCENT = MAX_MISSING_PERCENT,
          MIN_NUMERIC_PERCENT = MIN_NUMERIC_PERCENT,
          SPC_COLUMN_NAMES = SPC_COLUMN_NAMES,
          Y_AXIS_UNITS_DA = Y_AXIS_UNITS_DA,
          SPC_COLORS = SPC_COLORS,
          SPC_ALPHA_VALUES = SPC_ALPHA_VALUES,
          SPC_LINE_TYPES = SPC_LINE_TYPES,
          SPC_LINE_WIDTHS = SPC_LINE_WIDTHS
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
