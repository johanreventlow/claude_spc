# app_dependencies.R
# Centralized Dependency Management
# Fase 3.2: Modular dependency loading component

#' Manage Complete Application Dependencies
#'
#' Centraliseret dependency management som kan erstatte library() kald i global.R.
#' Inkluderer environment-aware loading og fallback-strategier.
#'
#' @param config App configuration from initialize_runtime_config()
#' @return List with dependency loading results
#' @export
manage_app_dependencies <- function(config = NULL) {
  # Dependency operation completed

  # Default config if not provided
  if (is.null(config)) {
    config <- list(
      environment = list(is_development = TRUE, is_production = FALSE, environment_type = "development"),
      logging = list(debug_mode_enabled = FALSE)
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

  # Dependency operation completed
  return(results)
}

#' Load Core Framework Packages
#'
#' Load the absolutely essential packages that cannot be namespace-referenced
#' due to their pervasive use throughout the application.
#'
#' @return List with loading results
load_core_packages <- function() {
  # Dependency operation completed

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

    # Data Manipulation - Declarative helpers (case_when etc.)
    dplyr = list(
      package = "dplyr",
      min_version = "1.0.0",
      reason = "Data manipulation helpers - case_when and related utilities"
    )
  )

  results <- purrr::imap(core_packages, ~safe_load_package(
    .x$package,
    .x$min_version,
    .x$reason,
    required = TRUE
  ))

  # Dependency operation completed
  return(results)
}

#' Load Feature-Specific Packages
#'
#' Load packages required for specific features, with environment-aware loading.
#'
#' @param config App configuration
#' @return List with loading results
load_feature_packages <- function(config) {
  # Dependency operation completed

  feature_packages <- list(
    # Excel-like Tables
    excelR = list(
      package = "excelR",
      min_version = "0.4.0",
      reason = "Excel-like editable tables - excelTable function",
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

    # Data Analysis

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

  results <- purrr::imap(feature_packages, ~safe_load_package(
    .x$package,
    .x$min_version,
    .x$reason,
    required = .x$required
  ))

  # Dependency operation completed
  return(results)
}

#' Load Development-Specific Packages
#'
#' Load packages only needed during development and debugging.
#'
#' @param config App configuration
#' @return List with loading results
load_development_packages <- function(config) {
  if (!config$environment$is_development) {
    # Dependency operation completed
    return(list())
  }

  # Dependency operation completed

  dev_packages <- list(
    # Development tools would go here
    # Currently no dev-only packages identified
  )

  results <- purrr::imap(dev_packages, ~safe_load_package(
    .x$package,
    .x$min_version,
    .x$reason,
    required = FALSE
  ))

  # Dependency operation completed
  return(results)
}

#' Setup Performance Package Optimizations
#'
#' Configure packages for optimal performance based on environment.
#'
#' @param config App configuration
#' @return List with optimization results
setup_performance_packages <- function(config) {
  # Dependency operation completed

  optimizations <- list()

  # Configure data.table threading if available
  if (requireNamespace("data.table", quietly = TRUE)) {
    # Set appropriate number of threads
    threads <- if (config$environment$is_production) 2 else 4
    optimizations$data_table_threads <- tryCatch({
      data.table::setDTthreads(threads)
      threads
    }, error = function(e) {
      # Dependency operation completed
      NULL
    })
  }

  # Configure readr threading
  optimizations$readr_num_threads <- if (config$environment$is_production) 1 else 2

  # Dependency operation completed
  return(optimizations)
}

#' Safe Package Loading with Error Handling
#'
#' Robust package loading med error handling og fallback strategies.
#'
#' @param package_name Name of package to load
#' @param min_version Minimum required version (optional)
#' @param reason Human-readable reason for loading this package
#' @param required Whether this package is absolutely required
#' @return List with loading results
safe_load_package <- function(package_name, min_version = NULL, reason = "", required = TRUE) {
  # Dependency operation completed

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
    library(package_name, character.only = TRUE) # nolint

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
    # Dependency operation completed

  }, error = function(e) {
    result$error <<- e$message
    log_error(paste("Failed to load package", package_name, ":", e$message), "PACKAGE_LOADER")

    if (required) {
      # For required packages, this is a critical error
      stop(paste("Critical dependency", package_name, "could not be loaded:", e$message))
    } else {
      # For optional packages, log and continue
      # Dependency operation completed
    }
  })

  return(result)
}

#' Verify Critical Dependencies
#'
#' Post-loading verification that all critical functions are available.
#'
#' @return Boolean indicating if all critical dependencies are satisfied
verify_critical_dependencies <- function() {
  # Dependency operation completed

  critical_functions <- list(
    # Shiny core
    shiny = c("shinyApp", "fluidPage", "reactive", "observe"),

    # bslib UI
    bslib = c("page_navbar", "card", "sidebar"),

    # dplyr data manipulation
    dplyr = c("case_when")
  )

  missing_functions <- purrr::imap(critical_functions, ~{
    purrr::map(.x, ~if (!exists(.x, mode = "function")) paste0(.y, "::", .x) else NULL)
  }) %>%
    purrr::flatten() %>%
    purrr::discard(is.null) %>%
    unlist()

  if (length(missing_functions) > 0) {
    log_error(paste("Missing critical functions:", paste(missing_functions, collapse = ", ")), "DEPENDENCY_VERIFY")
    return(FALSE)
  }

  # Dependency operation completed
  return(TRUE)
}

#' Get Dependency Status Report
#'
#' Generate a comprehensive report of all loaded dependencies.
#'
#' @return Data frame with dependency status
get_dependency_status_report <- function() {
  loaded_packages <- (.packages())

  # Create report using tidyverse approach
  report <- tibble::tibble(
    package = loaded_packages,
    version = purrr::map_chr(loaded_packages, ~ {
      tryCatch(as.character(packageVersion(.x)), error = function(e) "unknown")
    }),
    namespace_available = purrr::map_lgl(loaded_packages, ~ {
      requireNamespace(.x, quietly = TRUE)
    })
  )

  return(report)
}
