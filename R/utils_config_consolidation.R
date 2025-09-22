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
        source("R/config_chart_types.R", local = TRUE)
        list(
          chart_types = if (exists("CHART_TYPES")) CHART_TYPES else list(),
          default_chart = if (exists("DEFAULT_CHART_TYPE")) DEFAULT_CHART_TYPE else "i"
        )
      },
      fallback = list(chart_types = list(), default_chart = "i"),
      error_type = "config_loading"
    )
  }

  .load_system_config <- function() {
    safe_operation(
      "Loading system configuration",
      code = {
        source("R/config_system_config.R", local = TRUE)
        source("R/config_observer_priorities.R", local = TRUE)
        list(
          observer_priorities = if (exists("OBSERVER_PRIORITIES")) OBSERVER_PRIORITIES else list(),
          system_settings = if (exists("SYSTEM_CONFIG")) SYSTEM_CONFIG else list()
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
        source("R/config_ui_config.R", local = TRUE)
        list(
          ui_settings = if (exists("UI_CONFIG")) UI_CONFIG else list(),
          theme_settings = if (exists("THEME_CONFIG")) THEME_CONFIG else list()
        )
      },
      fallback = list(ui_settings = list(), theme_settings = list()),
      error_type = "config_loading"
    )
  }

  .load_hospital_branding <- function() {
    safe_operation(
      "Loading hospital branding",
      code = {
        source("R/config_hospital_branding.R", local = TRUE)
        source("R/config_branding_getters.R", local = TRUE)
        list(
          branding = if (exists("HOSPITAL_BRANDING")) HOSPITAL_BRANDING else list(),
          brand_functions = if (exists("get_brand_color", mode = "function")) {
            list(get_color = get_brand_color, get_logo = get_brand_logo)
          } else {
            list()
          }
        )
      },
      fallback = list(branding = list(), brand_functions = list()),
      error_type = "config_loading"
    )
  }

  .load_spc_config <- function() {
    safe_operation(
      "Loading SPC configuration",
      code = {
        source("R/config_spc_config.R", local = TRUE)
        list(
          spc_settings = if (exists("SPC_CONFIG")) SPC_CONFIG else list(),
          chart_defaults = if (exists("CHART_DEFAULTS")) CHART_DEFAULTS else list()
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
        key_parts <- strsplit(key, "\\.")[[1]]
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
        log_info("All configuration domains reloaded", "CONFIG_REGISTRY")
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
      log_warn("No global config registry found, creating temporary", "CONFIG_REGISTRY")
      config_registry <- create_config_registry()
    }
  }

  parts <- strsplit(path, "\\.", fixed = TRUE)[[1]]
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

  log_info("Global configuration registry initialized", "CONFIG_REGISTRY")
  invisible(.spc_config_registry)
}

# Null coalescing operator for internal use
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a