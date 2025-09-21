# app_initialization.R
# Modular Application Initialization System
# Fase 3.2: Complete orchestration of app startup

#' Initialize Complete Shiny Application
#'
#' @description
#' Master initialization function som erstatter den nuværende global.R approach.
#' Orchestrerer all app initialization i en kontrolleret og testable måde.
#'
#' @param force_reload Boolean indicating if forced reload is needed
#' @param config_override Optional configuration override
#' @return List containing initialization results
#' @export
initialize_app <- function(force_reload = FALSE, config_override = NULL) {
  log_debug("=== STARTING MODULAR APP INITIALIZATION ===", "APP_INIT")

  init_results <- list()

  # 1. Load foundation utilities (must come first)
  init_results$foundation <- load_foundation_utilities()

  # 2. Initialize configuration system
  init_results$config <- if (!is.null(config_override)) {
    config_override
  } else {
    initialize_app_config()
  }

  # 3. Manage dependencies with config awareness
  init_results$dependencies <- manage_app_dependencies(init_results$config)

  # 4. Load core application functions
  init_results$core <- load_core_functions()

  # 5. Load server components
  init_results$server <- load_server_components()

  # 6. Load UI components
  init_results$ui <- load_ui_components()

  # 7. Initialize main app components
  init_results$main_app <- initialize_main_app()

  # 8. Setup performance optimizations
  init_results$performance <- setup_performance_optimizations(init_results$config)

  # 9. Setup specialized functionality
  init_results$specialized <- load_specialized_functionality()

  # 10. Verify initialization completeness
  init_results$verification <- verify_initialization_completeness()

  log_debug("✅ MODULAR APP INITIALIZATION COMPLETED", "APP_INIT")
  return(init_results)
}

#' Load Foundation Utilities
#'
#' @description
#' Load the absolutely essential utilities that other components depend on.
#' Diese must be loaded first to ensure dependency resolution.
#'
#' @return List with loading results
load_foundation_utilities <- function() {
  log_debug("Loading foundation utilities", "FOUNDATION")

  # Foundation files that must be loaded first
  foundation_files <- c(
    "R/utils/logging.R",                    # Logging system (already loaded via source)
    "R/utils/dependency_injection.R",      # DI framework
    "R/utils/shinylogs_config.R",          # Web logging config
    "R/utils/advanced_debug.R",            # Debug infrastructure
    "R/utils/end_to_end_debug.R"           # E2E debug tools
  )

  results <- list(
    loaded_files = c(),
    failed_files = c()
  )

  results$loading_time <- system.time({
    for (file_path in foundation_files) {
      result <- safe_source_file(file_path, "foundation utility")
      if (result$success) {
        results$loaded_files <- c(results$loaded_files, file_path)
      } else {
        results$failed_files <- c(results$failed_files, file_path)
      }
    }
  })[["elapsed"]]

  log_debug(paste("Foundation utilities loaded:", length(results$loaded_files), "files"), "FOUNDATION")
  return(results)
}

#' Load Core Application Functions
#'
#' @description
#' Load core business logic and configuration efter foundation er sat op.
#'
#' @return List with loading results
load_core_functions <- function() {
  log_debug("Loading core application functions", "CORE_FUNCTIONS")

  # Configuration files (no internal dependencies)
  config_files <- c(
    "R/config/hospital_branding.R",
    "R/config/chart_types.R",
    "R/config/observer_priorities.R",
    "R/config/state_management.R",
    "R/config/ui_config.R",
    "R/config/spc_config.R",
    "R/config/system_config.R"
  )

  # Core function files (depend on config)
  core_files <- c(
    "R/core/spc_helpers.R",
    "R/fct_spc_plot_generation.R",
    "R/core/file_io.R",
    "R/core/autodetect_helpers.R",
    "R/utils/local_storage.R"
  )

  results <- list(
    config_loaded = c(),
    core_loaded = c(),
    failed_files = c()
  )

  results$loading_time <- system.time({
    # Load configuration first
    for (file_path in config_files) {
      result <- safe_source_file(file_path, "configuration")
      if (result$success) {
        results$config_loaded <- c(results$config_loaded, file_path)
      } else {
        results$failed_files <- c(results$failed_files, file_path)
      }
    }

    # Then load core functions
    for (file_path in core_files) {
      result <- safe_source_file(file_path, "core function")
      if (result$success) {
        results$core_loaded <- c(results$core_loaded, file_path)
      } else {
        results$failed_files <- c(results$failed_files, file_path)
      }
    }
  })[["elapsed"]]

  log_debug(paste("Core functions loaded:",
                 length(results$config_loaded), "config files,",
                 length(results$core_loaded), "core files"), "CORE_FUNCTIONS")
  return(results)
}

#' Load Server Components
#'
#' @description
#' Load all server-side logic og utilities.
#'
#' @return List with loading results
load_server_components <- function() {
  log_debug("Loading server components", "SERVER_COMPONENTS")

  # Server utility files
  server_files <- c(
    "R/server/utils_session_helpers.R",
    "R/server/utils_server_management.R",  # Only loaded once now
    "R/server/utils_column_management.R",
    "R/fct_file_operations.R",
    "R/fct_visualization_server.R",
    "R/modules/mod_spc_chart_server.R"
  )

  # Performance optimization files
  performance_files <- c(
    "R/server/performance_helpers.R",
    "R/server/performance_optimizations.R",
    "R/server/plot_optimizations.R"
  )

  results <- list(
    server_loaded = c(),
    performance_loaded = c(),
    failed_files = c()
  )

  results$loading_time <- system.time({
    # Load server utilities
    for (file_path in server_files) {
      result <- safe_source_file(file_path, "server component")
      if (result$success) {
        results$server_loaded <- c(results$server_loaded, file_path)
      } else {
        results$failed_files <- c(results$failed_files, file_path)
      }
    }

    # Load performance optimizations
    for (file_path in performance_files) {
      result <- safe_source_file(file_path, "performance optimization")
      if (result$success) {
        results$performance_loaded <- c(results$performance_loaded, file_path)
      } else {
        results$failed_files <- c(results$failed_files, file_path)
      }
    }
  })[["elapsed"]]

  log_debug(paste("Server components loaded:",
                 length(results$server_loaded), "server files,",
                 length(results$performance_loaded), "performance files"), "SERVER_COMPONENTS")
  return(results)
}

#' Load UI Components
#'
#' @description
#' Load all UI-related files og components.
#'
#' @return List with loading results
load_ui_components <- function() {
  log_debug("Loading UI components", "UI_COMPONENTS")

  # UI module files
  ui_module_files <- c(
    "R/modules/mod_spc_chart_ui.R"
  )

  # UI utility files (loaded after modules)
  ui_utility_files <- c(
    "R/utils/danish_locale.R",
    "R/ui/utils_ui_helpers.R",
    "R/ui/utils_ui_components.R",
    "R/ui/utils_ui_updates.R"
  )

  # Additional utilities (loaded after UI utilities)
  additional_files <- c(
    "R/server/utils_event_system.R",
    "R/utils/performance.R",
    "R/utils/memory_management.R"
  )

  results <- list(
    ui_modules_loaded = c(),
    ui_utilities_loaded = c(),
    additional_loaded = c(),
    failed_files = c()
  )

  results$loading_time <- system.time({
    # Load UI modules first
    for (file_path in ui_module_files) {
      result <- safe_source_file(file_path, "UI module")
      if (result$success) {
        results$ui_modules_loaded <- c(results$ui_modules_loaded, file_path)
      } else {
        results$failed_files <- c(results$failed_files, file_path)
      }
    }

    # Then UI utilities
    for (file_path in ui_utility_files) {
      result <- safe_source_file(file_path, "UI utility")
      if (result$success) {
        results$ui_utilities_loaded <- c(results$ui_utilities_loaded, file_path)
      } else {
        results$failed_files <- c(results$failed_files, file_path)
      }
    }

    # Finally additional utilities
    for (file_path in additional_files) {
      result <- safe_source_file(file_path, "additional utility")
      if (result$success) {
        results$additional_loaded <- c(results$additional_loaded, file_path)
      } else {
        results$failed_files <- c(results$failed_files, file_path)
      }
    }
  })[["elapsed"]]

  log_debug(paste("UI components loaded:",
                 length(results$ui_modules_loaded), "modules,",
                 length(results$ui_utilities_loaded), "utilities,",
                 length(results$additional_loaded), "additional"), "UI_COMPONENTS")
  return(results)
}

#' Initialize Main Application Components
#'
#' @description
#' Load the main app server og UI functions. Disse skal loaded sidst
#' efter all dependencies er tilgængelige.
#'
#' @return List with loading results
initialize_main_app <- function() {
  log_debug("Initializing main application components", "MAIN_APP")

  # Main app files (must be loaded in order)
  main_app_files <- c(
    "R/server/app_server.R",    # Main server function
    "R/ui/app_ui.R",           # Main UI function
    "R/run_app.R"              # App launcher
  )

  results <- list(
    loaded_files = c(),
    failed_files = c()
  )

  results$loading_time <- system.time({
    for (file_path in main_app_files) {
      result <- safe_source_file(file_path, "main app component")
      if (result$success) {
        results$loaded_files <- c(results$loaded_files, file_path)
      } else {
        results$failed_files <- c(results$failed_files, file_path)
      }
    }
  })[["elapsed"]]

  log_debug(paste("Main app components loaded:", length(results$loaded_files), "files"), "MAIN_APP")
  return(results)
}

#' Setup Performance Optimizations
#'
#' @description
#' Configure performance-related settings baseret på app configuration.
#'
#' @param config App configuration
#' @return List with optimization results
setup_performance_optimizations <- function(config) {
  log_debug("Setting up performance optimizations", "PERFORMANCE_SETUP")

  optimizations <- list()

  # Setup global variables based on config
  if (!is.null(config$features)) {
    # Set global feature flags
    assign("TEST_MODE_AUTO_LOAD", config$features$test_mode_auto_load, envir = .GlobalEnv)
    assign("AUTO_RESTORE_ENABLED", config$features$auto_restore_enabled, envir = .GlobalEnv)
    assign("SHINY_DEBUG_MODE", config$features$debug_mode_enabled, envir = .GlobalEnv)

    optimizations$feature_flags_set <- TRUE
  }

  # Setup development-specific configurations
  if (!is.null(config$development)) {
    assign("TEST_MODE_FILE_PATH", config$development$test_file_path, envir = .GlobalEnv)
    optimizations$dev_config_set <- TRUE
  }

  # Memory management setup
  if (!is.null(config$performance)) {
    if (config$performance$aggressive_cleanup) {
      optimizations$aggressive_cleanup_enabled <- TRUE
    }
  }

  log_debug("Performance optimizations configured", "PERFORMANCE_SETUP")
  return(optimizations)
}

#' Load Specialized Functionality
#'
#' @description
#' Load specialized features som unified autodetect engine.
#'
#' @return List with loading results
load_specialized_functionality <- function() {
  log_debug("Loading specialized functionality", "SPECIALIZED")

  specialized_files <- c(
    "R/fct_autodetect_unified.R"    # Main unified autodetect engine
  )

  results <- list(
    loaded_files = c(),
    failed_files = c()
  )

  results$loading_time <- system.time({
    for (file_path in specialized_files) {
      result <- safe_source_file(file_path, "specialized functionality")
      if (result$success) {
        results$loaded_files <- c(results$loaded_files, file_path)
      } else {
        results$failed_files <- c(results$failed_files, file_path)
      }
    }
  })[["elapsed"]]

  log_debug(paste("Specialized functionality loaded:", length(results$loaded_files), "files"), "SPECIALIZED")
  return(results)
}

#' Verify Initialization Completeness
#'
#' @description
#' Post-initialization verification at all critical components er loaded.
#'
#' @return List with verification results
verify_initialization_completeness <- function() {
  log_debug("Verifying initialization completeness", "VERIFICATION")

  verification <- list()

  # Check critical functions are available
  critical_functions <- c(
    "app_ui", "app_server", "run_app",           # Main app functions
    "safe_operation", "log_debug", "log_error",  # Utility functions
    "create_app_state",                           # State management (emit created in server)
    "initialize_app_config"                       # Configuration
  )

  verification$missing_functions <- c()
  for (func_name in critical_functions) {
    if (!exists(func_name, mode = "function")) {
      verification$missing_functions <- c(verification$missing_functions, func_name)
    }
  }

  # Check critical global variables
  critical_globals <- c(
    "TEST_MODE_AUTO_LOAD", "AUTO_RESTORE_ENABLED", "SHINY_DEBUG_MODE"
  )

  verification$missing_globals <- c()
  for (var_name in critical_globals) {
    if (!exists(var_name)) {
      verification$missing_globals <- c(verification$missing_globals, var_name)
    }
  }

  # Summary
  verification$complete <- (
    length(verification$missing_functions) == 0 &&
    length(verification$missing_globals) == 0
  )

  if (verification$complete) {
    log_debug("✅ Initialization verification PASSED", "VERIFICATION")
  } else {
    log_error(paste("Initialization verification FAILED:",
                   length(verification$missing_functions), "missing functions,",
                   length(verification$missing_globals), "missing globals"), "VERIFICATION")
  }

  return(verification)
}

#' Safe File Sourcing with Error Handling
#'
#' @description
#' Robust file sourcing med comprehensive error handling.
#'
#' @param file_path Relative path to file to source
#' @param description Human-readable description of file type
#' @return List with sourcing results
safe_source_file <- function(file_path, description) {
  result <- list(
    file_path = file_path,
    description = description,
    success = FALSE,
    error = NULL,
    loading_time = NULL
  )

  loading_time <- system.time({
    tryCatch({
      # Use existing source_from_base function for consistency
      source_from_base(file_path)
      result$success <- TRUE
    }, error = function(e) {
      result$error <- e$message
      log_error(paste("Failed to source", description, "file", file_path, ":", e$message), "FILE_SOURCING")
    })
  })

  result$loading_time <- loading_time[["elapsed"]]

  if (result$success) {
    log_debug(paste("✅", description, "loaded:", file_path), "FILE_SOURCING")
  } else {
    log_error(paste("❌", description, "failed:", file_path), "FILE_SOURCING")
  }

  return(result)
}

#' Get Initialization Status Report
#'
#' @description
#' Generate comprehensive report of initialization status.
#'
#' @param init_results Results from initialize_app()
#' @return Data frame with initialization status
get_initialization_status_report <- function(init_results) {
  if (is.null(init_results)) {
    return(data.frame(
      component = "initialization",
      status = "not_started",
      details = "initialize_app() has not been called",
      stringsAsFactors = FALSE
    ))
  }

  # Extract status from each component
  components <- names(init_results)
  status_rows <- list()

  for (component in components) {
    component_data <- init_results[[component]]

    if (is.list(component_data)) {
      if ("failed_files" %in% names(component_data)) {
        status <- if (length(component_data$failed_files) == 0) "success" else "partial_failure"
        details <- paste("Loaded:", length(component_data$loaded_files %||% c()),
                        "Failed:", length(component_data$failed_files))
      } else if ("complete" %in% names(component_data)) {
        status <- if (component_data$complete) "success" else "failure"
        details <- paste("Missing functions:", length(component_data$missing_functions %||% c()),
                        "Missing globals:", length(component_data$missing_globals %||% c()))
      } else {
        status <- "unknown"
        details <- "Status format not recognized"
      }
    } else {
      status <- "success"
      details <- "Component loaded successfully"
    }

    status_rows[[component]] <- data.frame(
      component = component,
      status = status,
      details = details,
      stringsAsFactors = FALSE
    )
  }

  return(do.call(rbind, status_rows))
}