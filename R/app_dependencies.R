# app_dependencies.R
# Centralized Dependency Management
# Fase 3.2: Modular dependency loading component

#' Manage Complete Application Dependencies
#'
#' @description
#' Centraliseret dependency management som kan erstatte library() kald i global.R.
#' Inkluderer environment-aware loading og fallback-strategier.
#'
#' @param config App configuration from initialize_app_config()
#' @return List with dependency loading results
#' @export
manage_app_dependencies <- function(config = NULL) {
  log_debug("Starting centralized dependency management", "DEPENDENCY_MGMT")

  # Default config if not provided
  if (is.null(config)) {
    config <- list(
      environment = list(is_development = TRUE, is_production = FALSE),
      features = list(debug_mode_enabled = FALSE)
    )
  }

  results <- list()

  # Core framework dependencies (always required)
  results$core <- load_core_packages()

  # Feature dependencies (conditional loading)
  results$features <- load_feature_packages(config)

  # Development dependencies (only in dev mode)
  results$development <- load_development_packages(config)

  # Performance optimizations
  results$performance <- setup_performance_packages(config)

  log_debug("✅ Dependency management completed", "DEPENDENCY_MGMT")
  return(results)
}

#' Load Core Framework Packages
#'
#' @description
#' Load the absolutely essential packages that cannot be namespace-referenced
#' due to their pervasive use throughout the application.
#'
#' @return List with loading results
load_core_packages <- function() {
  log_debug("Loading core framework packages", "CORE_PACKAGES")

  core_packages <- list(
    # Core Shiny Framework - MUST be globally available
    shiny = list(
      package = "shiny",
      min_version = "1.7.0",
      reason = "Core framework - functions used throughout app"
    ),

    # UI Framework - Extensive use in UI components
    bslib = list(
      package = "bslib",
      min_version = "0.4.0",
      reason = "UI framework - page_navbar, card, sidebar functions"
    ),

    # Data Manipulation - Pipe operator and dplyr verbs used extensively
    dplyr = list(
      package = "dplyr",
      min_version = "1.0.0",
      reason = "Data manipulation - %>% pipe and verbs used throughout"
    )
  )

  results <- list()

  for (pkg_name in names(core_packages)) {
    pkg_info <- core_packages[[pkg_name]]
    results[[pkg_name]] <- safe_load_package(
      pkg_info$package,
      pkg_info$min_version,
      pkg_info$reason,
      required = TRUE
    )
  }

  log_debug("Core packages loaded", "CORE_PACKAGES")
  return(results)
}

#' Load Feature-Specific Packages
#'
#' @description
#' Load packages required for specific features, with environment-aware loading.
#'
#' @param config App configuration
#' @return List with loading results
load_feature_packages <- function(config) {
  log_debug("Loading feature-specific packages", "FEATURE_PACKAGES")

  feature_packages <- list(
    # Excel-like Tables
    excelR = list(
      package = "excelR",
      min_version = "0.4.0",
      reason = "Excel-like editable tables - excelTable function",
      required = TRUE
    ),

    # String Operations
    stringi = list(
      package = "stringi",
      min_version = "1.7.0",
      reason = "String manipulation - stri_* functions",
      required = TRUE
    ),

    # File I/O
    readr = list(
      package = "readr",
      min_version = "2.0.0",
      reason = "CSV reading - read_csv functions",
      required = TRUE
    ),

    readxl = list(
      package = "readxl",
      min_version = "1.4.0",
      reason = "Excel reading - read_excel function",
      required = TRUE
    ),

    openxlsx = list(
      package = "openxlsx",
      min_version = "4.2.0",
      reason = "Excel export - write.xlsx function",
      required = TRUE
    ),

    # UI Enhancements
    shinycssloaders = list(
      package = "shinycssloaders",
      min_version = "1.0.0",
      reason = "Loading spinners - withSpinner function",
      required = TRUE
    ),

    shinyWidgets = list(
      package = "shinyWidgets",
      min_version = "0.7.0",
      reason = "Enhanced UI widgets - pickerInput, switchInput",
      required = TRUE
    ),

    # Data Analysis
    zoo = list(
      package = "zoo",
      min_version = "1.8.0",
      reason = "Time series analysis - rollmean, na.fill",
      required = TRUE
    ),

    scales = list(
      package = "scales",
      min_version = "1.2.0",
      reason = "Plot formatting - percent, comma, date_format",
      required = TRUE
    ),

    lubridate = list(
      package = "lubridate",
      min_version = "1.9.0",
      reason = "Date handling - ymd, dmy, month, year",
      required = TRUE
    ),

    # Logging System
    shinylogs = list(
      package = "shinylogs",
      min_version = "0.2.0",
      reason = "Advanced web-based logging - track_usage",
      required = TRUE
    )
  )

  results <- list()

  for (pkg_name in names(feature_packages)) {
    pkg_info <- feature_packages[[pkg_name]]
    results[[pkg_name]] <- safe_load_package(
      pkg_info$package,
      pkg_info$min_version,
      pkg_info$reason,
      required = pkg_info$required
    )
  }

  log_debug("Feature packages loaded", "FEATURE_PACKAGES")
  return(results)
}

#' Load Development-Specific Packages
#'
#' @description
#' Load packages only needed during development and debugging.
#'
#' @param config App configuration
#' @return List with loading results
load_development_packages <- function(config) {
  if (!config$environment$is_development) {
    log_debug("Skipping development packages (not in development mode)", "DEV_PACKAGES")
    return(list())
  }

  log_debug("Loading development packages", "DEV_PACKAGES")

  dev_packages <- list(
    # Development tools would go here
    # Currently no dev-only packages identified
  )

  results <- list()

  for (pkg_name in names(dev_packages)) {
    pkg_info <- dev_packages[[pkg_name]]
    results[[pkg_name]] <- safe_load_package(
      pkg_info$package,
      pkg_info$min_version,
      pkg_info$reason,
      required = FALSE
    )
  }

  log_debug("Development packages loaded", "DEV_PACKAGES")
  return(results)
}

#' Setup Performance Package Optimizations
#'
#' @description
#' Configure packages for optimal performance based on environment.
#'
#' @param config App configuration
#' @return List with optimization results
setup_performance_packages <- function(config) {
  log_debug("Setting up package performance optimizations", "PERF_PACKAGES")

  optimizations <- list()

  # Configure data.table threading if available
  if (requireNamespace("data.table", quietly = TRUE)) {
    # Set appropriate number of threads
    threads <- if (config$environment$is_production) 2 else 4
    optimizations$data_table_threads <- tryCatch({
      data.table::setDTthreads(threads)
      threads
    }, error = function(e) {
      log_debug(paste("Could not configure data.table threads:", e$message), "PERF_PACKAGES")
      NULL
    })
  }

  # Configure readr threading
  optimizations$readr_num_threads <- if (config$environment$is_production) 1 else 2

  log_debug("Package performance optimizations complete", "PERF_PACKAGES")
  return(optimizations)
}

#' Safe Package Loading with Error Handling
#'
#' @description
#' Robust package loading med error handling og fallback strategies.
#'
#' @param package_name Name of package to load
#' @param min_version Minimum required version (optional)
#' @param reason Human-readable reason for loading this package
#' @param required Whether this package is absolutely required
#' @return List with loading results
safe_load_package <- function(package_name, min_version = NULL, reason = "", required = TRUE) {
  log_debug(paste("Loading package:", package_name), "PACKAGE_LOADER")

  result <- list(
    package = package_name,
    loaded = FALSE,
    version = NULL,
    error = NULL,
    reason = reason,
    required = required
  )

  tryCatch({
    # Check if package is available
    if (!requireNamespace(package_name, quietly = TRUE)) {
      stop(paste("Package", package_name, "is not available"))
    }

    # Load the package
    library(package_name, character.only = TRUE)

    # Get version information
    result$version <- as.character(packageVersion(package_name))

    # Check minimum version if specified
    if (!is.null(min_version)) {
      if (compareVersion(result$version, min_version) < 0) {
        warning(paste("Package", package_name, "version", result$version,
                     "is below minimum required version", min_version))
      }
    }

    result$loaded <- TRUE
    log_debug(paste("✅", package_name, "loaded successfully, version:", result$version), "PACKAGE_LOADER")

  }, error = function(e) {
    result$error <<- e$message
    log_error(paste("Failed to load package", package_name, ":", e$message), "PACKAGE_LOADER")

    if (required) {
      # For required packages, this is a critical error
      stop(paste("Critical dependency", package_name, "could not be loaded:", e$message))
    } else {
      # For optional packages, log and continue
      log_debug(paste("Optional package", package_name, "not available, continuing"), "PACKAGE_LOADER")
    }
  })

  return(result)
}

#' Verify Critical Dependencies
#'
#' @description
#' Post-loading verification that all critical functions are available.
#'
#' @return Boolean indicating if all critical dependencies are satisfied
verify_critical_dependencies <- function() {
  log_debug("Verifying critical dependencies", "DEPENDENCY_VERIFY")

  critical_functions <- list(
    # Shiny core
    shiny = c("shinyApp", "fluidPage", "reactive", "observe"),

    # bslib UI
    bslib = c("page_navbar", "card", "sidebar"),

    # dplyr data manipulation
    dplyr = c("%>%", "filter", "mutate", "select")
  )

  missing_functions <- c()

  for (pkg in names(critical_functions)) {
    functions <- critical_functions[[pkg]]
    for (func in functions) {
      if (!exists(func, mode = "function")) {
        missing_functions <- c(missing_functions, paste0(pkg, "::", func))
      }
    }
  }

  if (length(missing_functions) > 0) {
    log_error(paste("Missing critical functions:", paste(missing_functions, collapse = ", ")), "DEPENDENCY_VERIFY")
    return(FALSE)
  }

  log_debug("✅ All critical dependencies verified", "DEPENDENCY_VERIFY")
  return(TRUE)
}

#' Get Dependency Status Report
#'
#' @description
#' Generate a comprehensive report of all loaded dependencies.
#'
#' @return Data frame with dependency status
get_dependency_status_report <- function() {
  loaded_packages <- (.packages())

  report <- data.frame(
    package = loaded_packages,
    version = sapply(loaded_packages, function(pkg) {
      tryCatch(as.character(packageVersion(pkg)), error = function(e) "unknown")
    }),
    namespace_available = sapply(loaded_packages, function(pkg) {
      requireNamespace(pkg, quietly = TRUE)
    }),
    stringsAsFactors = FALSE
  )

  return(report)
}