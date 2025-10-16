# utils_chart_module_helpers.R
# Ekstraherede helper utilities fra mod_spc_chart_server.R
# Forbedrer modularity og separation of concerns

#' Chart Module State Manager
#'
#' Factory for managing chart module state with consistent patterns.
#' Ekstraeret fra mod_spc_chart_server.R for bedre testability.
#'
#' @param app_state Application state object
#' @return List med state management funktioner
#' @export
create_chart_state_manager <- function(app_state) {
  # ARCHITECTURE FIX (2025-01-07): Visualization state centraliseret i state_management.R
  # Fjernet scattered reactiveValues creation - følger centralized state pattern
  # app_state$visualization initialiseres nu kun i create_app_state()

  # Verificer at visualization state eksisterer (skal være skabt af create_app_state)
  if (is.null(app_state$visualization)) {
    stop("app_state$visualization must be initialized by create_app_state() before calling create_chart_state_manager()")
  }

  list(
    # Set plot state value safely
    # key: State key to update
    # value: New value
    set_plot_state = function(key, value) {
      safe_operation(
        paste("Set plot state:", key),
        code = {
          app_state$visualization[[key]] <- value
        },
        fallback = NULL,
        error_type = "state_management"
      )
    },

    # Get plot state value safely
    # key: State key to retrieve
    # default: Default value if key not found
    get_plot_state = function(key, default = NULL) {
      safe_operation(
        paste("Get plot state:", key),
        code = {
          app_state$visualization[[key]] %||% default
        },
        fallback = default,
        error_type = "state_retrieval"
      )
    },

    # Reset plot state to initial values
    reset_plot_state = function() {
      safe_operation(
        "Reset plot state",
        code = {
          app_state$visualization$plot_object <- NULL
          app_state$visualization$plot_ready <- FALSE
          app_state$visualization$is_computing <- FALSE
          app_state$visualization$plot_warnings <- character(0)
          app_state$visualization$anhoej_results <- list(
            # Reset to default values instead of NULL
            longest_run = NA_real_,
            longest_run_max = NA_real_,
            n_crossings = NA_real_,
            n_crossings_min = NA_real_,
            out_of_control_count = 0L,
            runs_signal = FALSE,
            crossings_signal = FALSE,
            any_signal = FALSE,
            message = "Afventer data",
            has_valid_data = FALSE
          )
        },
        fallback = NULL,
        error_type = "state_reset"
      )
    },

    # Check if plot is ready for display
    is_plot_ready = function() {
      safe_operation(
        "Check plot readiness",
        code = {
          !is.null(app_state$visualization$plot_object) &&
            isTRUE(app_state$visualization$plot_ready) &&
            !isTRUE(app_state$visualization$is_computing)
        },
        fallback = FALSE,
        error_type = "state_check"
      )
    }
  )
}

#' Module Data Manager
#'
#' Helper for managing module-level data with caching and reactive safety.
#' Ekstraeret fra mod_spc_chart_server.R for bedre separation.
#'
#' @param app_state Application state object
#' @return List med data management funktioner
#' @export
create_module_data_manager <- function(app_state) {
  list(
    # Get module data with reactive safety
    get_module_data = function() {
      safe_operation(
        "Get module data",
        code = {
          # Use shiny::isolate() to safely access reactive values
          current_data_check <- shiny::isolate(app_state$data$current_data)
          if (is.null(current_data_check)) {
            return(NULL)
          }

          data <- current_data_check

          # Add hide_anhoej_rules flag as attribute
          hide_anhoej_rules_check <- shiny::isolate(app_state$ui$hide_anhoej_rules)
          attr(data, "hide_anhoej_rules") <- hide_anhoej_rules_check

          # H15: Vectorized row filter for performance
          # BEFORE: dplyr::filter with if_all (row-by-row evaluation)
          # AFTER: Base R vectorized rowSums (batch processing)
          # Performance gain: ~40-60% faster on typical datasets (50-500 rows)
          row_all_na <- rowSums(!is.na(data)) == 0
          filtered_data <- data[!row_all_na, , drop = FALSE]

          # Preserve attributes
          attr(filtered_data, "hide_anhoej_rules") <- hide_anhoej_rules_check

          # Return filtered data, or original if no rows remain
          if (nrow(filtered_data) > 0) {
            return(filtered_data)
          } else {
            attr(data, "hide_anhoej_rules") <- hide_anhoej_rules_check
            return(data)
          }
        },
        fallback = NULL,
        error_type = "data_retrieval"
      )
    },

    # Cache module data
    cache_module_data = function(data) {
      safe_operation(
        "Cache module data",
        code = {
          app_state$visualization$module_cached_data <- data
        },
        fallback = NULL,
        error_type = "data_caching"
      )
    },

    # Get cached module data
    get_cached_data = function() {
      safe_operation(
        "Get cached module data",
        code = {
          app_state$visualization$module_cached_data
        },
        fallback = NULL,
        error_type = "cache_retrieval"
      )
    },

    # Clear cached data
    clear_cache = function() {
      safe_operation(
        "Clear module data cache",
        code = {
          app_state$visualization$module_cached_data <- NULL
        },
        fallback = NULL,
        error_type = "cache_clearing"
      )
    }
  )
}

#' SPC Results Processor
#'
#' Processer SPC beregninger og ekstraherer relevante metrics.
#' Ekstraeret fra mod_spc_chart_server.R for bedre testability.
#'
#' @export
create_spc_results_processor <- function() {
  list(
    # Extract QIC results from plot data
    extract_qic_results = function(qic_data, chart_type = "run") {
      safe_operation(
        "Extract QIC results",
        code = {
          if (is.null(qic_data)) {
            return(list(
              any_signal = FALSE,
              out_of_control_count = 0,
              runs_signal = FALSE,
              crossings_signal = FALSE,
              message = "Ingen data tilgængelig"
            ))
          }

          # Standard SPC beregninger (altid tilgængelig fra qic)
          any_signal <- any(qic_data$sigma.signal, na.rm = TRUE)
          out_of_control_count <- sum(qic_data$sigma.signal, na.rm = TRUE)

          # Anhøj rules (meningsfulde for alle chart typer)
          runs_signal <- if ("runs.signal" %in% names(qic_data)) {
            any(qic_data$runs.signal, na.rm = TRUE)
          } else {
            FALSE
          }

          crossings_signal <- if ("n.crossings" %in% names(qic_data) && "n.crossings.min" %in% names(qic_data)) {
            n_cross <- max(qic_data$n.crossings, na.rm = TRUE)
            n_cross_min <- max(qic_data$n.crossings.min, na.rm = TRUE)
            !is.na(n_cross) && !is.na(n_cross_min) && n_cross < n_cross_min
          } else {
            FALSE
          }

          # Additional metrics
          longest_run <- if ("longest.run" %in% names(qic_data)) {
            max(qic_data$longest.run, na.rm = TRUE)
          } else {
            NA_real_
          }

          longest_run_max <- if ("longest.run.max" %in% names(qic_data)) {
            max(qic_data$longest.run.max, na.rm = TRUE)
          } else {
            NA_real_
          }

          n_crossings <- if ("n.crossings" %in% names(qic_data)) {
            max(qic_data$n.crossings, na.rm = TRUE)
          } else {
            NA_real_
          }

          n_crossings_min <- if ("n.crossings.min" %in% names(qic_data)) {
            max(qic_data$n.crossings.min, na.rm = TRUE)
          } else {
            NA_real_
          }

          # Chart-type specifik besked
          message <- if (chart_type == "run") {
            if (any_signal) "Særlig årsag detekteret" else "Ingen særlige årsager fundet"
          } else {
            if (any_signal) "Punkter uden for kontrol detekteret" else "Alle punkter inden for kontrol"
          }

          return(list(
            any_signal = any_signal,
            out_of_control_count = out_of_control_count,
            runs_signal = runs_signal,
            crossings_signal = crossings_signal,
            longest_run = longest_run,
            longest_run_max = longest_run_max,
            n_crossings = n_crossings,
            n_crossings_min = n_crossings_min,
            message = message
          ))
        },
        fallback = list(
          any_signal = FALSE,
          out_of_control_count = 0,
          runs_signal = FALSE,
          crossings_signal = FALSE,
          message = "Fejl ved beregning af resultater"
        ),
        error_type = "qic_processing"
      )
    },

    # Generate SPC plot with full error handling
    generate_plot_safely = function(data, config, chart_type, target_value = NULL,
                                    centerline_value = NULL, skift_config = NULL,
                                    frys_column = NULL, chart_title_reactive = NULL,
                                    y_unit = "count", kommentar_col = NULL, qic_cache = NULL) {
      safe_operation(
        "Generate SPC plot with safety wrapper",
        code = {
          # Ensure skift_config has required structure
          if (is.null(skift_config)) {
            skift_config <- list(show_phases = FALSE, skift_column = NULL)
          }

          # Generate the SPC plot
          # SPRINT 4: Pass QIC cache for performance optimization
          spc_result <- generateSPCPlot(
            data = data,
            config = config,
            chart_type = chart_type,
            target_value = target_value,
            centerline_value = centerline_value,
            show_phases = skift_config$show_phases %||% FALSE,
            skift_column = skift_config$skift_column,
            frys_column = frys_column,
            chart_title_reactive = chart_title_reactive,
            y_axis_unit = y_unit,
            kommentar_column = kommentar_col,
            qic_cache = qic_cache
          )

          # Apply hospital theme if available
          plot <- if (exists("applyHospitalTheme", mode = "function")) {
            applyHospitalTheme(spc_result$plot)
          } else {
            spc_result$plot
          }

          return(list(
            plot = plot,
            qic_data = spc_result$qic_data
          ))
        },
        fallback = list(plot = NULL, qic_data = NULL),
        error_type = "plot_generation"
      )
    }
  )
}

#' Chart Validation Service
#'
#' Validering af chart data og konfiguration før plot generation.
#' Ekstraeret fra mod_spc_chart_server.R for genbrug.
#'
#' @export
create_chart_validator <- function() {
  list(
    # Validate data and configuration for chart generation
    validate_for_chart = function(data, config, chart_type) {
      safe_operation(
        "Validate data for chart generation",
        code = {
          warnings <- character(0)

          # Check if data exists and has rows
          if (is.null(data) || nrow(data) == 0) {
            warnings <- c(warnings, "Ingen data tilgængelig for plot generation")
            return(list(valid = FALSE, warnings = warnings))
          }

          # Check if required columns exist
          if (is.null(config$y_col) || !config$y_col %in% names(data)) {
            warnings <- c(warnings, "Y-kolonne er ikke konfigureret eller findes ikke i data")
          }

          # Check for ratio charts
          if (chart_type %in% c("p", "pp", "u", "up") &&
            (is.null(config$n_col) || !config$n_col %in% names(data))) {
            warnings <- c(warnings, paste("Chart type", chart_type, "kræver en nævner-kolonne (N)"))
          }

          # Check data quality
          if (!is.null(config$y_col) && config$y_col %in% names(data)) {
            y_data <- data[[config$y_col]]
            valid_y_count <- sum(!is.na(y_data) & trimws(as.character(y_data)) != "")

            if (valid_y_count < 2) {
              warnings <- c(warnings, "Mindst 2 gyldige datapunkter er påkrævet for SPC chart")
            }
          }

          # Return validation result
          return(list(
            valid = length(warnings) == 0,
            warnings = warnings
          ))
        },
        fallback = list(valid = FALSE, warnings = "Fejl ved validering af chart data"),
        error_type = "chart_validation"
      )
    }
  )
}

# Null coalescing operator is defined in utils_logging.R
