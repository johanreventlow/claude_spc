# fct_spc_plot_generation.R
# Main SPC plot generation logic using qicharts2
# Extracted from fct_spc_calculations.R for better maintainability
#
# Dependencies ----------------------------------------------------------------

# COMMENT PROCESSING UTILITIES ================================================

## Extract Comment Data for Plot Annotations
# Processer kommentar-data til brug i plot annotationer
extract_comment_data <- function(data, kommentar_column, qic_data) {
  # Returner NULL hvis ingen kommentar-kolonne er specificeret
  if (is.null(kommentar_column) || !kommentar_column %in% names(data)) {
    return(NULL)
  }

  # STABLE ROW MAPPING: Use row-id to correctly map comments to qic_data points
  # This prevents comment drift when qicharts2 reorders/filters rows

  # PHASE 1: STRATEGY 4 - Robust fallback for missing columns
  # Verify required columns exist before processing
  if (!all(c("x", "y") %in% names(qic_data))) {
    log_error("Missing required columns (x or y) in qic_data - cannot extract comments", .context = "PLOT_COMMENT")
    return(NULL)
  }

  # Regenerate .original_row_id if missing
  if (!".original_row_id" %in% names(qic_data)) {
    log_warn("Missing .original_row_id in qic_data - regenerating from row count", .context = "PLOT_COMMENT")
    qic_data$.original_row_id <- 1:nrow(qic_data)
  }

  # Build comment mapping with row IDs
  original_data_with_comments <- data.frame(
    .original_row_id = 1:nrow(data),
    comment = data[[kommentar_column]],
    stringsAsFactors = FALSE
  )

  # Safe subset: Only include columns that exist
  subset_cols <- c("x", "y", ".original_row_id")
  available_cols <- subset_cols[subset_cols %in% names(qic_data)]

  if (length(available_cols) < 3) {
    log_warn(
      paste("Incomplete qic_data structure for comment mapping. Available:", paste(available_cols, collapse = ", ")),
      .context = "PLOT_COMMENT"
    )
    return(NULL)
  }

  # Join qic_data med original kommentarer via row-id
  qic_data_with_comments <- tryCatch(
    {
      merge(
        qic_data[, available_cols, drop = FALSE],
        original_data_with_comments,
        by = ".original_row_id",
        all.x = TRUE,
        sort = FALSE
      )
    },
    error = function(e) {
      log_error(
        paste("Merge failed in extract_comment_data:", e$message),
        .context = "PLOT_COMMENT"
      )
      return(NULL)
    }
  )

  if (is.null(qic_data_with_comments)) {
    return(NULL)
  }

  comment_data <- data.frame(
    x = qic_data_with_comments$x,
    y = qic_data_with_comments$y,
    comment = qic_data_with_comments$comment,
    stringsAsFactors = FALSE
  )

  # Filtrer til kun ikke-tomme kommentarer
  comment_data <- comment_data[
    !is.na(comment_data$comment) &
      trimws(comment_data$comment) != "",
  ]

  # SPRINT 3: Sanitize comments for XSS protection
  if (nrow(comment_data) > 0) {
    # Apply XSS sanitization to all comments
    if (exists("sanitize_user_input") && is.function(sanitize_user_input)) {
      comment_data$comment <- sapply(comment_data$comment, function(cmt) {
        sanitize_user_input(
          input_value = cmt,
          max_length = SPC_COMMENT_CONFIG$max_length, # M3: Using config constant
          allowed_chars = "A-Za-z0-9_æøåÆØÅ .,-:!?",
          html_escape = TRUE
        )
      }, USE.NAMES = FALSE)
    }

    # Afkort meget lange kommentarer efter sanitization (M3: Using config constants)
    comment_data$comment <- dplyr::if_else(
      nchar(comment_data$comment) > SPC_COMMENT_CONFIG$display_length,
      stringr::str_c(substr(comment_data$comment, 1, SPC_COMMENT_CONFIG$truncate_length), "..."),
      comment_data$comment
    )
  }

  return(comment_data)
}

# DATA CLEANING UTILITIES =====================================================

## Clean QIC Call Arguments
# Renser QIC kald argumenter for komplette cases og justerer part positioner
clean_qic_call_args <- function(call_args) {
  # Tilføj return.data = TRUE for at få underliggende data frame i stedet for plot
  call_args$return.data <- TRUE

  # Rens data for komplette cases
  complete_cases <- complete.cases(call_args$x, call_args$y)

  if (!all(complete_cases)) {
    call_args$x <- call_args$x[complete_cases]
    call_args$y <- call_args$y[complete_cases]

    # Håndter n-værdier hvis de findes
    if ("n" %in% names(call_args)) {
      call_args$n <- call_args$n[complete_cases]
    }

    removed_positions <- which(!complete_cases)
    total_remaining <- sum(complete_cases)

    # Håndter part positioner: juster for fjernede rækker
    if ("part" %in% names(call_args) && length(removed_positions) > 0) {
      # Juster part positioner ved at trække fjernede rækker før hver position using tidyverse
      adjusted_part <- call_args$part |>
        purrr::map_dbl(~ {
          pos <- .x
          removed_before <- sum(removed_positions < pos)
          pos - removed_before
        })

      # Fjern ugyldige positioner
      if (length(adjusted_part) > 0) {
        valid_parts <- adjusted_part > 0 & adjusted_part <= total_remaining
        call_args$part <- adjusted_part[valid_parts]

        if (length(call_args$part) == 0) {
          call_args$part <- NULL
        }
      }
    }

    # Håndter freeze position: juster eller fjern hvis ugyldig
    if ("freeze" %in% names(call_args)) {
      adjusted_freeze <- call_args$freeze

      if (length(removed_positions) > 0) {
        removed_before_freeze <- sum(removed_positions <= call_args$freeze)
        adjusted_freeze <- adjusted_freeze - removed_before_freeze
      }

      if (is.null(adjusted_freeze) || is.na(adjusted_freeze) || adjusted_freeze <= 0) {
        call_args$freeze <- NULL
      } else if (adjusted_freeze > total_remaining) {
        if (total_remaining > 0) {
          call_args$freeze <- total_remaining
        } else {
          call_args$freeze <- NULL
        }
      } else {
        call_args$freeze <- adjusted_freeze
      }
    }
  }

  return(call_args)
}

# QIC ARGUMENTS UTILITIES =====================================================

## Build QIC Call Arguments
# Bygger argumenter til qicharts2::qic() kald dynamisk
build_qic_call_arguments <- function(x_data, y_data, chart_type, title_text, ylab_text,
                                     n_data = NULL, freeze_position = NULL,
                                     part_positions = NULL, target_value = NULL) {
  # Byg grundlæggende qic kald argumenter dynamisk
  call_args <- list(
    x = x_data,
    y = y_data,
    chart = chart_type,
    title = title_text,
    ylab = ylab_text
  )

  # NOTE: x.period og x.format parametre bruges ikke længere da vi anvender return.data=TRUE

  # Tilføj n når nævner er valgt af bruger (qic håndterer chart type validation)
  if (!is.null(n_data)) {
    call_args$n <- n_data
  }

  # Tilføj freeze for baseline - kan bruges sammen med part
  if (!is.null(freeze_position)) {
    call_args$freeze <- freeze_position
  }

  # Tilføj part for phase splits - kan bruges sammen med freeze
  if (!is.null(part_positions)) {
    call_args$part <- part_positions
  }

  # Tilføj target line hvis angivet
  if (!is.null(target_value) && is.numeric(target_value) && !is.na(target_value)) {
    call_args$target <- target_value
  }

  return(call_args)
}

# DATA PROCESSING UTILITIES ===================================================

## Process Ratio Chart Data
# Behandler data for ratio charts (med tæller/nævner)
process_ratio_chart_data <- function(data, config, chart_type, y_axis_unit) {
  # Ratio charts (with numerator/denominator)
  data <- filter_complete_spc_data(data, config$y_col, config$n_col, config$x_col)

  # Parse and validate numeric data
  parsed_data <- parse_and_validate_spc_data(
    data[[config$y_col]],
    data[[config$n_col]],
    config$y_col,
    config$n_col
  )

  # Get unit label
  y_unit_label <- get_safe_unit_label(y_axis_unit, Y_AXIS_UNITS_DA)

  # Calculate Y-axis data and generate label
  y_data <- calculate_y_axis_data(chart_type, parsed_data$y_data, parsed_data$n_data)
  n_data <- parsed_data$n_data # Keep for qicharts2
  ylab_text <- generate_y_axis_label(chart_type, y_unit_label, config$y_col, config$n_col)

  return(list(
    data = data,
    y_data = y_data,
    n_data = n_data,
    ylab_text = ylab_text,
    y_unit_label = y_unit_label
  ))
}

## Process Standard Chart Data
# Behandler data for standard numeriske charts (enkelt værdi)
process_standard_chart_data <- function(data, config, chart_type, y_axis_unit) {
  # Standard numeric charts (single value)
  data <- filter_complete_spc_data(data, config$y_col, NULL, config$x_col)

  # Parse and validate numeric data
  parsed_data <- parse_and_validate_spc_data(data[[config$y_col]], NULL, config$y_col)

  # Get unit label and calculate Y-axis data
  y_unit_label <- get_safe_unit_label(y_axis_unit, Y_AXIS_UNITS_DA)
  y_data <- calculate_y_axis_data(chart_type, parsed_data$y_data)
  ylab_text <- generate_y_axis_label(chart_type, y_unit_label, config$y_col)

  return(list(
    data = data,
    y_data = y_data,
    n_data = NULL, # No n_data for standard charts
    ylab_text = ylab_text,
    y_unit_label = y_unit_label
  ))
}

# QIC DATA GENERATION UTILITIES ===============================================

## Prepare QIC Data Parameters
# Forbereder data parametre til qicharts2 integration med NSE håndtering
prepare_qic_data_parameters <- function(data, config, x_validation, chart_type) {
  x_col_name <- config$x_col # Auto-detected date column or NULL
  y_col_name <- config$y_col # Should be "Tæller"
  # Kun medtag nævner-kolonne for diagramtyper der kræver den
  n_col_name <- if (chart_type_requires_denominator(chart_type)) config$n_col else NULL

  # Brug data fra x_validation i stedet for duplikeret logik
  log_debug(paste(
    "UPDATE CONDITION DEBUG:\n- x_col_name is not NULL:", !is.null(x_col_name),
    "\n- x_col_name in names(data):", if (!is.null(x_col_name)) x_col_name %in% names(data) else "N/A",
    "\n- x_validation$is_date:", x_validation$is_date,
    if (!is.null(x_col_name) && x_col_name %in% names(data)) {
      paste("\n- data[[x_col_name]] is character:", is.character(data[[x_col_name]]))
    } else {
      ""
    }
  ), "DATA_PROCESS")

  # UPDATED CONDITION: Accept both date columns AND character columns (like "Uge tekst")
  if (!is.null(x_col_name) && x_col_name %in% names(data) &&
    (x_validation$is_date || is.character(data[[x_col_name]]))) {
    # Debug logging før opdatering

    if (x_validation$is_date) {
      # DATE COLUMN: Use processed data from x_validation

      # Opdater kolonnen med de processerede data fra x_validation
      if (length(x_validation$x_data) == nrow(data)) {
        data[[x_col_name]] <- x_validation$x_data
        x_col_for_qic <- x_col_name
      } else {
        log_debug("Length mismatch - using observation sequence as fallback", .context = "DATA_PROCESS")
        # Fallback til observation sekvens
        if (!("obs_sequence" %in% names(data))) {
          data$obs_sequence <- 1:nrow(data)
        }
        x_col_for_qic <- "obs_sequence"
      }
    } else {
      # CHARACTER COLUMN: Convert to factor with original row order to prevent alphabetical sorting
      # Create factor with levels in dataset order using tidyverse approach
      data[[x_col_name]] <- data[[x_col_name]] |>
        forcats::fct_inorder()
      x_col_for_qic <- x_col_name
    }
  } else {
    # Brug observation sekvens som fallback
    if (!("obs_sequence" %in% names(data))) {
      data$obs_sequence <- 1:nrow(data)
    }
    x_col_for_qic <- "obs_sequence"
  }

  # Note: obs_sequence fjernes IKKE fra data da det måske bruges af andre komponenter

  # Note: Use raw column names for all chart types - let qic handle calculations

  return(list(
    data = data,
    x_col_for_qic = x_col_for_qic,
    x_col_name = x_col_name,
    y_col_name = y_col_name,
    n_col_name = n_col_name
  ))
}

## Build QIC Arguments with NSE
# Bygger qicharts2::qic() argumenter med non-standard evaluation
build_qic_arguments <- function(data, x_col_for_qic, y_col_name, n_col_name,
                                chart_type, freeze_position, part_positions, target_value = NULL, centerline_value = NULL) {
  # STABLE ROW ID: Add row identifier for comment mapping resilience
  # This prevents comment misalignment when qicharts2 reorders/filters rows
  data_with_row_id <- data
  if (!".original_row_id" %in% names(data)) {
    data_with_row_id$.original_row_id <- 1:nrow(data)
  }

  # Build qic call arguments
  qic_args <- list(
    data = data_with_row_id,
    chart = chart_type,
    return.data = TRUE
  )

  # Add column names using non-standard evaluation (NSE) approach
  if (!is.null(x_col_for_qic)) qic_args$x <- as.name(x_col_for_qic)
  if (!is.null(y_col_name)) qic_args$y <- as.name(y_col_name)
  if (!is.null(n_col_name)) qic_args$n <- as.name(n_col_name)

  # Add freeze for baseline - can be used together with part
  if (!is.null(freeze_position) && !is.na(freeze_position)) {
    qic_args$freeze <- freeze_position
  }

  # Add part for phase splits - can be used together with freeze
  if (!is.null(part_positions) && !all(is.na(part_positions))) {
    qic_args$part <- part_positions
  }

  # Add target value if provided
  if (!is.null(target_value) && is.numeric(target_value) && !is.na(target_value)) {
    adjusted_target <- target_value

    # RUN charts med nævner skal have target i decimal form til qicharts2
    if (!is.null(n_col_name) && chart_type == "run" && adjusted_target > 1) {
      adjusted_target <- adjusted_target / 100
    }

    qic_args$target <- adjusted_target
  }

  # Add centerline if provided
  if (!is.null(centerline_value) && is.numeric(centerline_value) && !is.na(centerline_value)) {
    adjusted_centerline <- centerline_value

    # RUN charts med nævner skal have centerline i decimal form til qicharts2
    if (!is.null(n_col_name) && chart_type == "run" && adjusted_centerline > 1) {
      adjusted_centerline <- adjusted_centerline / 100
    }

    qic_args$cl <- adjusted_centerline
  }

  return(qic_args)
}

## Execute QIC Call with Post-processing
# Udfører qicharts2::qic() kald og post-processerer resultaterne
execute_qic_call <- function(qic_args, chart_type, config, qic_cache = NULL) {
  # Call qic() with prepared arguments
  if (getOption("debug.mode", FALSE)) {
    log_debug("qic_args structure:", .context = "QIC_CALL")
    log_debug(qic_args, .context = "QIC_CALL")
  }

  log_debug(qic_args, .context = "QIC")

  # Get call number for debugging correlation (M1: using package environment)
  call_number <- get_qic_call_counter()

  # MICROBENCHMARK: Measure QIC calculation performance with statistical analysis
  # Feature flag check - disable benchmarking in production by default
  benchmark_enabled <- isTRUE(getOption("spc.benchmark_enabled", FALSE)) ||
    isTRUE(tryCatch(golem::get_golem_options("benchmark_enabled"), error = function(e) FALSE))

  if (benchmark_enabled && exists("benchmark_spc_operation") && requireNamespace("microbenchmark", quietly = TRUE)) {
    # Determine data size category for benchmarking
    data_size <- nrow(qic_args$data)
    size_category <- if (data_size < 50) "small" else if (data_size < 500) "medium" else "large"

    # Benchmark with fewer iterations for expensive operations
    benchmark_iterations <- if (data_size < 100) 3 else if (data_size < 1000) 2 else 1

    benchmark_result <- benchmark_spc_operation(
      expr = log_qic_call_wrapper(qic_args, "execute_qic_call_benchmark", call_number, qic_cache = qic_cache),
      times = benchmark_iterations,
      operation_name = paste0("qic_", chart_type, "_", size_category, "_", data_size, "_rows"),
      log_results = TRUE,
      capture_result = TRUE
    )

    # Use result from benchmark to eliminate redundant QIC call
    qic_data <- benchmark_result$captured_result
  } else {
    # SPRINT 4: Execute without benchmarking with caching support
    qic_data <- log_qic_call_wrapper(qic_args, "execute_qic_call_fallback", call_number, qic_cache = qic_cache)
  }

  qic_data
}

# PLOT ENHANCEMENT UTILITIES ==================================================
# Moved to BFHcharts - see BFHcharts:::add_plot_enhancements()
# Removed legacy functions: add_plot_enhancements()

#' Backend Wrapper for SPC Plot Generation
#'
#' Conditionally routes to BFHchart or qicharts2 backend based on feature flag.
#' This wrapper preserves the existing generateSPCPlot() interface, ensuring
#' zero changes required in mod_spc_chart_server.R.
#'
#' @inheritParams generateSPCPlot
#' @return List with plot and qic_data (consistent structure from both backends)
#' @export
generateSPCPlot_with_backend <- function(data, config, chart_type, target_value = NULL, centerline_value = NULL, show_phases = FALSE, skift_column = NULL, frys_column = NULL, chart_title_reactive = NULL, y_axis_unit = "count", kommentar_column = NULL, base_size = 14, viewport_width = NULL, viewport_height = NULL, target_text = NULL, qic_cache = NULL) {
  # Read feature flag configuration
  features_config <- tryCatch(
    {
      cfg <- golem::get_golem_options("features")
      log_debug(
        component = "[BACKEND_WRAPPER]",
        message = "Successfully read features config from golem",
        details = list(
          use_bfhchart = cfg$use_bfhchart,
          supported_types = paste(cfg$bfhchart_supported_types %||% c(), collapse = ", ")
        )
      )
      cfg
    },
    error = function(e) {
      log_warn(
        component = "[BACKEND_WRAPPER]",
        message = "Failed to read features config from golem, defaulting to qicharts2",
        details = list(
          error = e$message,
          GOLEM_CONFIG_ACTIVE = Sys.getenv("GOLEM_CONFIG_ACTIVE", "not_set")
        )
      )
      list(use_bfhchart = FALSE)
    }
  )

  use_bfhchart <- isTRUE(features_config$use_bfhchart)

  log_debug(
    component = "[BACKEND_WRAPPER]",
    message = sprintf("Backend selection: use_bfhchart = %s", use_bfhchart),
    details = list(chart_type = chart_type)
  )

  # Backend selection logic
  if (use_bfhchart) {
    # Check if chart type is supported by BFHchart
    supported_types <- features_config$bfhchart_supported_types %||% c("run", "i", "p", "c", "u")

    if (!chart_type %in% supported_types) {
      log_warn(
        component = "[BACKEND_WRAPPER]",
        message = sprintf("Chart type '%s' not in BFHchart supported types, falling back to qicharts2", chart_type),
        details = list(
          chart_type = chart_type,
          supported_types = paste(supported_types, collapse = ", ")
        )
      )
      use_bfhchart <- FALSE
    }
  }

  # Execute with selected backend
  if (use_bfhchart) {
    log_info(
      component = "[BACKEND_WRAPPER]",
      message = sprintf("Using BFHchart backend for chart type: %s", chart_type),
      details = list(chart_type = chart_type)
    )

    result <- tryCatch(
      {
        # Call BFHchart backend (compute_spc_results_bfh from Task #31)
        # Adapter: Map config object to individual parameters
        compute_spc_results_bfh(
          data = data,
          x_var = config$x_col,
          y_var = config$y_col,
          chart_type = chart_type,
          n_var = config$n_col,
          cl_var = NULL, # Not currently supported in SPCify
          freeze_var = frys_column,
          part_var = if (isTRUE(show_phases) && !is.null(skift_column)) skift_column else NULL,
          notes_column = kommentar_column,
          multiply = 1, # No scaling needed, handled in y_axis_unit
          # Pass through additional BFHcharts parameters from config
          target_value = target_value,
          target_text = target_text,
          centerline_value = centerline_value,
          chart_title_reactive = chart_title_reactive,
          y_axis_unit = y_axis_unit
        )
      },
      error = function(e) {
        log_error(
          component = "[BACKEND_WRAPPER]",
          message = "BFHchart backend failed, falling back to qicharts2",
          details = list(
            error = e$message,
            chart_type = chart_type
          ),
          session = NULL,
          show_user = TRUE
        )

        # Fallback to qicharts2
        generateSPCPlot_qicharts2(
          data = data, config = config, chart_type = chart_type,
          target_value = target_value, centerline_value = centerline_value,
          show_phases = show_phases, skift_column = skift_column,
          frys_column = frys_column, chart_title_reactive = chart_title_reactive,
          y_axis_unit = y_axis_unit, kommentar_column = kommentar_column,
          base_size = base_size, viewport_width = viewport_width,
          viewport_height = viewport_height, target_text = target_text,
          qic_cache = qic_cache
        )
      }
    )

    return(result)
  } else {
    # Use qicharts2 backend (default)
    log_debug(
      component = "[BACKEND_WRAPPER]",
      message = sprintf("Using qicharts2 backend for chart type: %s", chart_type),
      details = list(chart_type = chart_type)
    )

    return(generateSPCPlot_qicharts2(
      data = data, config = config, chart_type = chart_type,
      target_value = target_value, centerline_value = centerline_value,
      show_phases = show_phases, skift_column = skift_column,
      frys_column = frys_column, chart_title_reactive = chart_title_reactive,
      y_axis_unit = y_axis_unit, kommentar_column = kommentar_column,
      base_size = base_size, viewport_width = viewport_width,
      viewport_height = viewport_height, target_text = target_text,
      qic_cache = qic_cache
    ))
  }
}


# PLOT STYLING ===============================================================
# Moved to BFHcharts/BFHtheme - see BFHcharts:::apply_spc_theme() and BFHtheme::theme_bfh()
# Removed legacy functions: applyHospitalTheme()

#' Generate SPC Plot (Legacy Alias)
#'
#' Legacy alias for generateSPCPlot_with_backend(). Maintains backward
#' compatibility with existing code while enabling feature flag switching.
#'
#' @inheritParams generateSPCPlot_with_backend
#' @export
generateSPCPlot <- generateSPCPlot_with_backend

## Y-akse Skalering
# Automatisk detektering af passende Y-akse format (decimal, procent, heltal)
detectYAxisScale <- function(y_data) {
  if (is.null(y_data) || length(y_data) == 0) {
    return("integer")
  }

  # Remove NA values
  y_clean <- y_data[!is.na(y_data)]

  if (length(y_clean) == 0) {
    return("integer")
  }

  max_val <- max(y_clean)
  min_val <- min(y_clean)

  # Rule 1: Decimal scale (0-1)
  if (max_val <= 1.0) {
    return("decimal")
  }

  # Rule 2: Percent scale (0-100+ with most values looking like percentages)
  if (min_val >= 0 && max_val <= 200) {
    # Check if most values look like percentages (0-100 range)
    percent_like_count <- sum(y_clean >= 0 & y_clean <= 100)
    if (percent_like_count / length(y_clean) >= 0.7) { # 70% threshold
      return("percent")
    }
  }

  # Rule 3: Integer/rate scale
  return("integer")
}
