# utils_lazy_loading.R
# Lazy loading system for heavy modules and diagnostics

#' Lazy loading configuration
#' @description
#' Configuration for which modules should be loaded lazily vs. eagerly
#' based on application usage patterns and startup performance requirements.
#'
#' @details
#' Heavy modules that are loaded on-demand:
#' - File operations (1058 lines) - Only needed for file upload/export
#' - Advanced debug utilities (647 lines) - Only needed in debug mode
#' - Performance monitoring (687 lines) - Only needed when monitoring enabled
#' - SPC plot generation (940 lines) - Only needed when creating plots
#'
#' Always loaded modules:
#' - Core server management
#' - Event system
#' - Basic UI components
#' - Configuration system
LAZY_LOADING_CONFIG <- list(
  heavy_modules = list(
    file_operations = list(
      file = "R/fct_file_operations.R",
      condition = function() {
        result <- tryCatch(get_golem_config("enable_file_operations"), error = function(e) TRUE)
        return(isTRUE(result))
      },
      loaded = FALSE
    ),
    advanced_debug = list(
      file = "R/utils_advanced_debug.R",
      condition = function() {
        config_result <- tryCatch(get_golem_config("debug_mode_enabled"), error = function(e) FALSE)
        env_result <- Sys.getenv("SPC_DEBUG_MODE", "FALSE") == "TRUE"
        return(isTRUE(config_result) || isTRUE(env_result))
      },
      loaded = FALSE
    ),
    performance_monitoring = list(
      file = "R/utils_performance.R",
      condition = function() {
        result <- tryCatch(get_golem_config("enable_performance_monitoring"), error = function(e) FALSE)
        return(isTRUE(result))
      },
      loaded = FALSE
    ),
    plot_generation = list(
      file = "R/fct_spc_plot_generation.R",
      condition = function() TRUE, # Always needed for SPC app, but can be lazy loaded
      loaded = FALSE
    )
  ),

  # Cache for loaded modules to prevent reloading
  loaded_modules = new.env(parent = emptyenv())
)

#' Load module lazily
#' @description
#' Load a specific module only when needed, with caching to prevent reloading
#'
#' @param module_name Name of the module from LAZY_LOADING_CONFIG
#' @param force_reload Force reload even if already loaded
#' @return TRUE if loaded successfully, FALSE otherwise
#' @export
lazy_load_module <- function(module_name, force_reload = FALSE) {
  if (!module_name %in% names(LAZY_LOADING_CONFIG$heavy_modules)) {
    log_warn(paste("Unknown lazy module:", module_name), "LAZY_LOADING")
    return(FALSE)
  }

  module_config <- LAZY_LOADING_CONFIG$heavy_modules[[module_name]]

  # Check if already loaded and not forcing reload
  if (!force_reload && module_config$loaded) {
    log_debug(paste("Module", module_name, "already loaded"), .context = "LAZY_LOADING")
    return(TRUE)
  }

  # Check loading condition
  if (!module_config$condition()) {
    log_debug(paste("Module", module_name, "condition not met, skipping"), .context = "LAZY_LOADING")
    return(FALSE)
  }

  # Load the module using appropriate method for environment
  safe_operation(
    operation_name = paste("Lazy load module:", module_name),
    code = {
      # In production (package mode), functions are already loaded via namespace
      if (!"SPCify" %in% loadedNamespaces() || getOption("spc.debug.source_loading", FALSE)) {
        # Development mode: use source loading
        if (file.exists(module_config$file)) {
          source(module_config$file, local = FALSE)
          log_debug(paste("Source-loaded module:", module_name), "LAZY_LOADING")
        } else {
          log_warn(paste("Module file not found:", module_config$file), "LAZY_LOADING")
          return(FALSE)
        }
      } else {
        # Production mode: verify functions are available via namespace
        log_debug(paste("Module", module_name, "functions loaded via package namespace"), "LAZY_LOADING")
      }

      # Safe assignment that handles locked bindings
      tryCatch({
        LAZY_LOADING_CONFIG$heavy_modules[[module_name]]$loaded <<- TRUE
      }, error = function(e) {
        if (grepl("locked binding", e$message)) {
          log_debug(paste("Binding locked for", module_name, "- marking as loaded in cache only"), "LAZY_LOADING")
        } else {
          stop(e)
        }
      })

      # Always record load time in cache environment (this should work even if config is locked)
      tryCatch({
        LAZY_LOADING_CONFIG$loaded_modules[[module_name]] <<- Sys.time()
      }, error = function(e) {
        if (grepl("locked binding", e$message)) {
          log_debug(paste("Cannot record load time for", module_name, "- cache binding locked"), "LAZY_LOADING")
        } else {
          stop(e)
        }
      })

      log_info(paste("Lazy loaded module:", module_name), "LAZY_LOADING")
      return(TRUE)
    },
    fallback = function(e) {
      log_error(paste("Failed to lazy load module", module_name, ":", e$message), "LAZY_LOADING")
      return(FALSE)
    }
  )
}

#' Load all modules based on configuration
#' @description
#' Load all heavy modules that meet their loading conditions.
#' Used during startup to load only necessary modules.
#'
#' @param force_all Force load all modules regardless of conditions
#' @return List of successfully loaded modules
#' @export
lazy_load_modules <- function(force_all = FALSE) {
  log_info("Starting lazy module loading", .context = "LAZY_LOADING")

  loaded_modules <- character(0)

  for (module_name in names(LAZY_LOADING_CONFIG$heavy_modules)) {
    module_config <- LAZY_LOADING_CONFIG$heavy_modules[[module_name]]

    should_load <- force_all || module_config$condition()

    if (should_load) {
      if (lazy_load_module(module_name)) {
        loaded_modules <- c(loaded_modules, module_name)
      }
    } else {
      log_debug(paste("Skipping module", module_name, "- condition not met"), .context = "LAZY_LOADING")
    }
  }

  log_info(paste("Lazy loading complete. Loaded modules:", paste(loaded_modules, collapse = ", ")), "LAZY_LOADING")
  return(loaded_modules)
}

#' Get lazy loading status
#' @description
#' Get current status of all lazy loaded modules
#'
#' @return List with module loading status
#' @export
get_lazy_loading_status <- function() {
  status <- list()

  for (module_name in names(LAZY_LOADING_CONFIG$heavy_modules)) {
    module_config <- LAZY_LOADING_CONFIG$heavy_modules[[module_name]]
    status[[module_name]] <- list(
      loaded = module_config$loaded,
      condition_met = module_config$condition(),
      file_exists = file.exists(module_config$file),
      load_time = LAZY_LOADING_CONFIG$loaded_modules[[module_name]]
    )
  }

  return(status)
}

#' Ensure module is loaded
#' @description
#' Ensure a specific module is loaded before use. This is the main function
#' to call before using functionality from a lazy-loaded module.
#'
#' @param module_name Name of the module to ensure is loaded
#' @return TRUE if module is available, FALSE otherwise
#' @export
ensure_module_loaded <- function(module_name) {
  module_config <- LAZY_LOADING_CONFIG$heavy_modules[[module_name]]

  if (is.null(module_config)) {
    log_warn(paste("Unknown module for ensure_module_loaded:", module_name), "LAZY_LOADING")
    return(FALSE)
  }

  if (module_config$loaded) {
    return(TRUE)
  }

  log_debug(paste("Loading module on-demand:", module_name), "LAZY_LOADING")
  return(lazy_load_module(module_name))
}