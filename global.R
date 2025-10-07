# DEVELOPMENT & TEST HARNESS ====================================================
#
# ⚠️  IMPORTANT: This file is a DEVELOPMENT AND TEST HELPER ONLY
#
# Production runtime does NOT depend on global.R:
# • Production uses: pkgload::load_all() → run_app()
# • Functions loaded via package namespace
# • Configuration via golem-config.yml
#
# global.R is ONLY used for:
# • Interactive development (source loading)
# • Test suites (controlled environment)
# • Debugging and performance profiling
#
# To start the application in production:
#   library(SPCify)  # or pkgload::load_all()
#   run_app()
#
# To start in development mode:
#   source('global.R')  # This file
#
# ==============================================================================

# SMART BOOT FLOW FOR DEVELOPMENT/TESTING ====================================
# Default to source loading for development to avoid package loading freeze
# To use package loading instead, run: options(spc.debug.source_loading = FALSE)
options(spc.debug.source_loading = TRUE)

# Check for development debug toggle
use_source_loading <- getOption("spc.debug.source_loading", FALSE) ||
  Sys.getenv("SPC_SOURCE_LOADING", "FALSE") == "TRUE"

if (use_source_loading) {
  message("Loading development configuration via source() loading...")

  # Clear any existing SPCify package conflicts for development
  if (exists("SPCify") && "package:SPCify" %in% search()) {
    detach("package:SPCify", unload = TRUE, force = TRUE)
  }
} else {
  message("Loading production configuration via package loading...")

  # Use package-based loading (standard production approach)
  if (!"SPCify" %in% loadedNamespaces()) {
    # Try to load the package with connection management, fallback to source loading if it fails
    tryCatch(
      {
        # SECURITY: Clean up connections before package loading to prevent exhaustion
        # Close any unused connections to prevent "all 128 connections are in use" error
        tryCatch(
          {
            open_connections <- showConnections(all = TRUE)
            if (nrow(open_connections) > 100) { # Threshold to prevent issues
              warning("High number of open connections detected, attempting cleanup")
              # Close connections that can be safely closed
              for (i in seq_len(nrow(open_connections))) {
                conn_info <- open_connections[i, ]
                if (conn_info$class %in% c("file", "textConnection")) {
                  tryCatch(close(getConnection(as.numeric(rownames(conn_info)))), error = function(e) NULL)
                }
              }
            }
          },
          error = function(e) {
            # Connection cleanup failed, continue but warn
            message("Warning: Could not clean up connections: ", e$message)
          }
        )

        suppressPackageStartupMessages({
          pkgload::load_all(".", quiet = TRUE)
        })
        message("✓ SPCify package loaded via pkgload")
      },
      error = function(e) {
        message("pkgload failed, falling back to source-based loading: ", e$message)
        # Force source loading for this session
        use_source_loading <<- TRUE
      }
    )
  } else {
    message("✓ SPCify package already loaded")
  }

  # If package loading failed, continue to source loading
  if (use_source_loading) {
    message("Switching to source-based loading...")
  } else {
    # Package loading complete - skip to configuration
    message("✓ Global configuration loaded successfully")

    # Initialize critical global variables
    if (exists("get_hospital_colors")) {
      HOSPITAL_COLORS <- get_hospital_colors()
      message("✓ Hospital branding loaded: ", ifelse(exists("get_hospital_name") && !is.null(get_hospital_name()), get_hospital_name(), "Default Hospital"), " SPC")
    } else {
      message("⚠ Hospital branding not available - check package installation")
    }

    # Set development log level if not set
    if (!nzchar(Sys.getenv("SPC_LOG_LEVEL", ""))) {
      Sys.setenv(SPC_LOG_LEVEL = "INFO")
    }

    # Package loading completed successfully - no source loading needed
    # Jump to final configuration section
  }
}

# Only execute source loading if use_source_loading is TRUE
if (use_source_loading) {
  # Load core functions via sourcing in dependency order
  source_files <- c(
    # Core utilities first
    "R/utils_logging.R",
    "R/utils_error_handling.R",
    "R/utils_startup_cache.R", # Add startup cache system early
    "R/utils_cache_generators.R", # Cache generators
    "R/utils_lazy_loading.R", # Add lazy loading system
    "R/utils_dependency_injection.R",
    # Note: utils_performance.R and utils_advanced_debug.R are now lazy loaded
    "R/utils_shinylogs_config.R",
    "R/utils_memory_management.R",
    "R/utils_danish_locale.R",
    "R/golem_utils.R",
    "R/zzz.R",

    # Configuration
    "R/config_system_config.R",
    "R/config_hospital_branding.R",
    "R/config_branding_getters.R",
    "R/config_observer_priorities.R",
    "R/config_chart_types.R",
    "R/config_spc_config.R",
    "R/app_ui.R",
    "R/app_server.R",
    "R/app_runtime_config.R",
    "R/state_management.R",

    # App initialization
    "R/app_dependencies.R",
    "R/app_initialization.R",
    "R/app_run.R",

    # UI components
    "R/config_ui_config.R",
    "R/utils_ui_ui_components.R",
    "R/utils_ui_ui_updates.R",
    "R/ui_app_ui.R",

    # Shiny modules
    "R/mod_spc_chart_ui.R",
    "R/utils_anhoej_results.R",
    "R/mod_spc_chart_server.R",

    # Server components
    "R/server_observer_manager.R",
    "R/utils_server_server_management.R",
    "R/app_server_main.R",
    "R/utils_server_session_helpers.R",
    "R/utils_server_event_listeners.R", # Split from utils_server_event_system.R
    "R/utils_ui_column_sync.R", # Split from utils_server_event_system.R
    "R/fct_visualization_server.R"
  )

  for (file in source_files) {
    if (file.exists(file)) {
      source(file, local = FALSE)
    }
  }

  message("✓ SPCify package loaded")

  # STARTUP CACHE FOR STATIC ARTIFACTS --------------------------------
  # Load cached static data if available, cache new data for next startup
  if (exists("load_cached_startup_data", mode = "function")) {
    cached_data <- load_cached_startup_data()
    if (length(cached_data) > 0) {
      message("✓ Loaded cached artifacts: ", paste(names(cached_data), collapse = ", "))

      # Apply cached data to global variables
      if ("hospital_branding" %in% names(cached_data)) {
        HOSPITAL_COLORS <- cached_data$hospital_branding$colors
        HOSPITAL_NAME <- cached_data$hospital_branding$name
      }
      if ("observer_priorities" %in% names(cached_data)) {
        OBSERVER_PRIORITIES <- cached_data$observer_priorities
      }
    } else {
      message("• No cached artifacts available")
    }

    # Cache current data for next startup (async, non-blocking)
    cached_artifacts <- cache_startup_data()
    if (length(cached_artifacts) > 0) {
      message("✓ Created cache for next startup: ", paste(cached_artifacts, collapse = ", "))
    }
  } else {
    message("⚠ Startup cache system not available")
  }

  # LAZY LOADING OF HEAVY MODULES --------------------------------
  # Load heavy modules only when needed, based on configuration
  if (exists("lazy_load_modules", mode = "function")) {
    loaded_modules <- lazy_load_modules()
    if (length(loaded_modules) > 0) {
      message("✓ Lazy loaded modules: ", paste(loaded_modules, collapse = ", "))
    } else {
      message("• No heavy modules loaded (lazy loading active)")
    }
  } else {
    message("⚠ Lazy loading system not available")
  }

  message("✓ Global configuration loaded successfully")

  # Initialize critical global variables for development
  if (exists("get_hospital_colors")) {
    HOSPITAL_COLORS <- get_hospital_colors()
    message("✓ Hospital branding loaded: ", ifelse(exists("get_hospital_name") && !is.null(get_hospital_name()), get_hospital_name(), "Default Hospital"), " SPC")
  } else {
    # Fallback hospital colors
    HOSPITAL_COLORS <- list(
      primary = "#375a7f",
      secondary = "#6c757d",
      success = "#28a745",
      warning = "#ffc107",
      danger = "#dc3545",
      accent = "#FF6B35"
    )
    message("⚠ Hospital branding not available - using fallback colors")
  }

  # BACKWARD COMPATIBILITY LAYER --------------------------------
  # Essential functions for backward compatibility - now provided by package

  # Enhanced debugging utilities - simplified for package use
  get_debug_mode_status <- function() {
    # Simplified version that works with package loading
    if (exists("runtime_config") && !is.null(runtime_config$logging$debug_mode_enabled)) {
      return(runtime_config$logging$debug_mode_enabled)
    }
    env_debug <- Sys.getenv("SHINY_DEBUG_MODE", "FALSE")
    return(env_debug == "TRUE")
  }

  # Legacy environment detection
  detect_environment <- function() {
    env_var <- Sys.getenv("TEST_MODE_AUTO_LOAD", "FALSE")
    if (env_var != "") {
      normalized_env_var <- tolower(trimws(env_var))
      true_values <- c("true", "t", "1", "yes", "y", "on")
      return(normalized_env_var %in% true_values)
    }
    return(FALSE)
  }

  # RUNTIME CONFIGURATION --------------------------------
  # Initialize runtime configuration - now handled by package loading
  if (exists("initialize_runtime_config", mode = "function")) {
    runtime_config <- initialize_runtime_config()
  } else {
    # Fallback basic config
    runtime_config <- list(
      logging = list(debug_mode_enabled = FALSE),
      testing = list(auto_load_enabled = FALSE)
    )
  }

  # DEVELOPMENT LOG LEVEL
  if (!nzchar(Sys.getenv("SPC_LOG_LEVEL", ""))) {
    Sys.setenv(SPC_LOG_LEVEL = "INFO")
  }

  # SOURCE LOADING INITIALIZATION COMPLETE --------------------------------
  message("✓ Global configuration loaded successfully")
  if (exists("HOSPITAL_NAME")) {
    message(paste("✓ Hospital branding loaded:", HOSPITAL_NAME))
  } else {
    message("⚠ Hospital branding not available - check package installation")
  }
} # End of source loading conditional block

# FINAL CONFIGURATION --------------------------------
# This section runs regardless of loading method (package or source)
# Log level is already set above in both branches, so no additional configuration needed
