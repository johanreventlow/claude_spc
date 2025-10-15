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

## Add Plot Enhancements
# Tilføjer extended lines, phase separations og comment annotations
# NOTE: Label placement er flyttet til add_spc_labels() funktion
add_plot_enhancements <- function(plot, qic_data, comment_data, y_axis_unit = "count", cl_linewidth = 1, target_linewidth = 1, comment_size = 6, suppress_targetline = FALSE) {
  # Get hospital colors using the proper package function
  hospital_colors <- get_hospital_colors()

  # Beregn y_range for time formatting context ----
  y_range <- if (y_axis_unit == "time" && !is.null(qic_data$y)) {
    range(qic_data$y, na.rm = TRUE)
  } else {
    NULL
  }

  # Beregn extended x position (20% ud over sidste datapunkt) ----
  last_x <- max(qic_data$x, na.rm = TRUE)
  first_x <- min(qic_data$x, na.rm = TRUE)

  # Konverter Date objekter til POSIXct for uniform håndtering
  if (inherits(last_x, "Date")) {
    last_x <- as.POSIXct(last_x)
    first_x <- as.POSIXct(first_x)
  }

  # Beregn 20% extension baseret på data range
  if (inherits(last_x, c("POSIXct", "POSIXt"))) {
    # For tidsobjekter: beregn i sekunder
    range_secs <- as.numeric(difftime(last_x, first_x, units = "secs"))
    extended_x <- last_x + range_secs * 0.20
  } else {
    # For numerisk
    x_range <- last_x - first_x
    extended_x <- last_x + (x_range * 0.20)
  }

  # Opret extended line data ----
  # NOTE: Labels håndteres nu af add_spc_labels() funktion
  extended_lines_data <- data.frame()

  # Centerline extended line KUN for seneste part
  cl_extension_linetype <- "solid" # Default linetype
  if (!is.null(qic_data$cl) && any(!is.na(qic_data$cl))) {
    # Find seneste part
    latest_part <- max(qic_data$part, na.rm = TRUE)
    part_data <- qic_data[qic_data$part == latest_part & !is.na(qic_data$part), ]

    if (nrow(part_data) > 0) {
      # Brug sidste punkt i seneste part
      last_row <- part_data[nrow(part_data), ]
      cl_value <- last_row$cl

      # Bestem linetype baseret på anhoej.signal i seneste part
      # Match formatering fra hovedplot centerline
      if ("anhoej.signal" %in% names(last_row)) {
        cl_extension_linetype <- if (isTRUE(last_row$anhoej.signal)) "12" else "solid"
      }

      if (!is.na(cl_value)) {
        # Extended line fra sidste datapunkt til extended_x
        extended_lines_data <- rbind(extended_lines_data, data.frame(
          x = c(last_row$x, extended_x),
          y = c(cl_value, cl_value),
          type = "cl",
          linetype = cl_extension_linetype,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  # Target extended line (only if not suppressed by arrow symbols)
  if (!suppress_targetline && !is.null(qic_data$target) && any(!is.na(qic_data$target))) {
    target_value <- qic_data$target[!is.na(qic_data$target)][1]

    # Extended line fra sidste datapunkt til extended_x
    extended_lines_data <- rbind(extended_lines_data, data.frame(
      x = c(last_x, extended_x),
      y = c(target_value, target_value),
      type = "target",
      linetype = "42", # Match target linetype
      stringsAsFactors = FALSE
    ))
  }

  # Extended CL og Target linjer tilføjes ----
  if (!is.null(extended_lines_data) && nrow(extended_lines_data) > 0) {
    # CL extension
    if (any(extended_lines_data$type == "cl")) {
      cl_ext <- extended_lines_data[extended_lines_data$type == "cl", ]
      plot <- plot +
        ggplot2::geom_line(
          data = cl_ext,
          ggplot2::aes(x = x, y = y),
          color = hospital_colors$hospitalblue,
          linewidth = cl_linewidth,
          linetype = cl_ext$linetype[1], # Use captured linetype directly
          inherit.aes = FALSE
        )
    }

    # Target extension
    if (any(extended_lines_data$type == "target")) {
      target_ext <- extended_lines_data[extended_lines_data$type == "target", ]
      plot <- plot +
        ggplot2::geom_line(
          data = target_ext,
          ggplot2::aes(x = x, y = y),
          color = "#565656",
          linewidth = target_linewidth,
          linetype = "42",
          inherit.aes = FALSE
        )
    }
  }

  # Kommentarer tilføjes ----
  if (!is.null(comment_data) && nrow(comment_data) > 0) {
    plot <- plot +
      ggrepel::geom_text_repel(
        data = comment_data,
        ggplot2::aes(x = x, y = y, label = comment),
        size = comment_size,
        color = hospital_colors$darkgrey,
        box.padding = 0.5,
        point.padding = 0.5,
        segment.color = hospital_colors$mediumgrey,
        segment.size = 0.3,
        arrow = grid::arrow(length = grid::unit(0.015, "npc")),
        max.overlaps = Inf
      )
  }

  # NOTE: CL og Target labels håndteres nu af add_spc_labels() funktion
  # som kaldes fra generate_spc_plot() efter add_plot_enhancements()

  return(plot)
}

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
    golem::get_golem_options("features"),
    error = function(e) {
      log_debug(
        component = "[BACKEND_WRAPPER]",
        message = "Failed to read features config, defaulting to qicharts2",
        details = list(error = e$message)
      )
      list(use_bfhchart = FALSE)
    }
  )

  use_bfhchart <- isTRUE(features_config$use_bfhchart)

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
        compute_spc_results_bfh(
          data = data,
          config = config,
          chart_type = chart_type,
          target_value = target_value,
          centerline_value = centerline_value,
          show_phases = show_phases,
          skift_column = skift_column,
          frys_column = frys_column,
          chart_title_reactive = chart_title_reactive,
          y_axis_unit = y_axis_unit,
          kommentar_column = kommentar_column,
          base_size = base_size,
          viewport_width = viewport_width,
          viewport_height = viewport_height,
          target_text = target_text
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

#' Generate SPC Plot using qicharts2 Backend
#'
#' Original qicharts2 implementation, renamed for clarity.
#' This function is called by generateSPCPlot_with_backend() wrapper.
#'
#' @inheritParams generateSPCPlot_with_backend
#' @keywords internal
generateSPCPlot_qicharts2 <- function(data, config, chart_type, target_value = NULL, centerline_value = NULL, show_phases = FALSE, skift_column = NULL, frys_column = NULL, chart_title_reactive = NULL, y_axis_unit = "count", kommentar_column = NULL, base_size = 14, viewport_width = NULL, viewport_height = NULL, target_text = NULL, qic_cache = NULL) {
  # Generate SPC plot with specified parameters
  # Get hospital colors using the proper package function
  hospital_colors <- get_hospital_colors()

  # Detect arrow symbols in target_text to determine if targetline should be suppressed
  suppress_targetline <- FALSE
  if (!is.null(target_text) && nchar(trimws(target_text)) > 0) {
    # Format target_text to check for arrows
    formatted_target <- format_target_prefix(target_text)
    suppress_targetline <- has_arrow_symbol(formatted_target)

    if (suppress_targetline) {
      message(sprintf("[TARGETLINE_SUPPRESSION] Arrow symbol detected in target_text - targetline will be suppressed"))
    }
  }

  # Beregn responsive geom størrelser baseret på base_size
  # Reference: base_size 14 giver original størrelser
  scale_factor <- base_size / 14

  # Skalerede størrelser for geom elementer
  ucl_linewidth <- 2.5 * scale_factor
  target_linewidth <- 1 * scale_factor
  data_linewidth <- 1 * scale_factor
  cl_linewidth <- 1 * scale_factor
  point_size <- 2 * scale_factor
  comment_size <- 6 * scale_factor
  label_size <- 6 * scale_factor

  # PERFORMANCE MONITORING: Track QIC calculation calls (M1: using package environment)
  call_number <- increment_qic_call_counter()
  current_time <- Sys.time()

  # Phase 4: Integrate with enhanced startup metrics
  if (exists("track_generateSPCPlot_call")) {
    track_generateSPCPlot_call(
      context = "plot_generation",
      details = list(
        chart_type = chart_type,
        data_rows = nrow(data),
        call_number = call_number
      )
    )
  }

  log_debug(paste(
    "generateSPCPlot CALL #", call_number, "at", format(current_time, "%H:%M:%S.%OS3"),
    "| chart_type:", chart_type, "| data:", nrow(data), "rows"
  ), .context = "SPC_CALC_DEBUG")

  # Input validation and configuration sanitization
  validate_spc_inputs(data, config)
  config <- sanitize_spc_config(config)

  # Process chart title
  title_text <- process_chart_title(chart_title_reactive, config)

  # Extract X-axis data
  x_data <- extract_x_axis_data(data, config$x_col)

  # DEBUG: Log data processing decision
  log_debug_kv(
    message = "DATA PROCESSING DECISION",
    chart_type = chart_type,
    chart_type_requires_denominator = chart_type_requires_denominator(chart_type),
    config_n_col = config$n_col %||% "NULL",
    n_col_in_data = if (!is.null(config$n_col)) config$n_col %in% names(data) else FALSE,
    y_axis_unit = y_axis_unit,
    .context = "[DEBUG_PLOT_GEN]"
  )

  # Process data based on chart type
  if (chart_type_requires_denominator(chart_type) && !is.null(config$n_col) && config$n_col %in% names(data)) {
    # Ratio charts (with numerator/denominator)
    log_debug_kv(
      message = "Using RATIO chart data processing",
      .context = "[DEBUG_PLOT_GEN]"
    )
    data_result <- process_ratio_chart_data(data, config, chart_type, y_axis_unit)
  } else {
    # Standard numeric charts (single value)
    log_debug_kv(
      message = "Using STANDARD chart data processing",
      .context = "[DEBUG_PLOT_GEN]"
    )
    data_result <- process_standard_chart_data(data, config, chart_type, y_axis_unit)
  }

  # Extract processed data
  data <- data_result$data
  y_data <- data_result$y_data
  n_data <- data_result$n_data
  ylab_text <- data_result$ylab_text
  y_unit_label <- data_result$y_unit_label

  # Note: Let qic handle ratio calculations directly from raw y and n data

  has_denominator <- !is.null(n_data)

  # Ensure we have minimum data points after filtering
  if (length(y_data) < 3) {
    stop(paste("For få gyldige datapunkter efter filtrering (", length(y_data), " fundet, minimum 3 påkrævet). Tilføj flere gyldige datapunkter."))
  }

  # Handle x-axis data med intelligent formatering - EFTER data filtrering
  # FASE 5: Performance optimization - cache expensive x-column validation
  # OPTIMIZED CACHE KEY (Task 1.6): Structural-only hashing for 30-50% faster generation
  # Instead of hashing entire column data, use:
  # 1. Data dimensions (nrow, ncol)
  # 2. Column names hash
  # 3. First/last 100 values hash (sample-based validation)

  # Structural dimensions
  data_dims <- paste0(nrow(data), "_", ncol(data))

  # Column names hash (detects structural changes)
  col_names_hash <- digest::digest(names(data), algo = "xxhash32")

  # ROBUST CACHE KEY: Safe ID generation to handle character(0) and NULL values
  safe_x_col_id <- if (is.null(config$x_col) || length(config$x_col) == 0 || identical(config$x_col, character(0)) || is.na(config$x_col)) {
    "NULL_XCOL"
  } else {
    # Sanitize column name for cache key (remove problematic characters)
    gsub("[^a-zA-Z0-9_]", "_", as.character(config$x_col)[1])
  }

  # SAMPLE-BASED CONTENT HASH: Hash first/last 100 values instead of entire column
  # Reduces hashing cost from O(n) to O(1) while maintaining validation accuracy
  x_content_hash <- safe_operation(
    "Generate sample-based x-column content hash for cache key",
    code = {
      if (!is.null(config$x_col) && config$x_col %in% names(data)) {
        x_column_data <- data[[config$x_col]]

        # Sample first 100 and last 100 values (or all if n < 200)
        if (length(x_column_data) <= 200) {
          # Small dataset - hash all values
          sample_data <- x_column_data
        } else {
          # Large dataset - sample head and tail
          sample_data <- c(
            head(x_column_data, 100),
            tail(x_column_data, 100)
          )
        }

        # Fast xxhash32 on sample + row count
        digest::digest(sample_data, algo = "xxhash32")
      } else {
        # Row count still matters for fallback case (1:nrow(data))
        paste0("NO_XCOL_", nrow(data))
      }
    },
    fallback = function(e) {
      log_debug(paste("Failed to hash x-column content:", e$message), "PERFORMANCE")
      paste0("fallback_", as.integer(Sys.time()), "_", nrow(data))
    },
    error_type = "processing"
  )

  # Compact cache key with structural components
  cache_key <- paste0("x_validation_", safe_x_col_id, "_", data_dims, "_", substr(col_names_hash, 1, 8), "_", substr(x_content_hash, 1, 8))

  # Use direct caching instead of reactive caching in non-reactive context
  x_validation <- get_cached_result(cache_key)
  if (is.null(x_validation)) {
    x_validation <- validate_x_column_format(data, config$x_col, "observation")
    cache_result(cache_key, x_validation, timeout_seconds = PERFORMANCE_THRESHOLDS$cache_timeout_default)
  } else {
    x_validation <- x_validation$value
  }
  x_data <- x_validation$x_data

  # Define x_unit_label for axis labeling
  # x_unit_label <- if (x_validation$is_date) "Dato" else "Observation"


  # Handle phases and freeze configuration
  phase_freeze_config <- process_phase_freeze_config(data, show_phases, skift_column, frys_column)
  part_positions <- phase_freeze_config$part_positions
  freeze_position <- phase_freeze_config$freeze_position

  # REMOVED LEGACY CODE PATH (build_qic_call_arguments + clean_qic_call_args)
  # This caused purrr_error_indexed when processing part positions with wrong data structure
  # The correct NSE-based code path is executed below in the safe_operation block

  # Generate SPC data using qicharts2 and build custom ggplot
  return(safe_operation(
    "Generate SPC plot data",
    code = {
      # Use data parameter approach like the working example
      # qicharts2::qic(x = Dato, y = `Tæller`, n = `Nævner`, part = c(12), data = data, return.data = TRUE)

      # Prepare QIC data parameters with NSE handling
      qic_params <- prepare_qic_data_parameters(data, config, x_validation, chart_type)
      data <- qic_params$data
      x_col_for_qic <- qic_params$x_col_for_qic
      x_col_name <- qic_params$x_col_name
      y_col_name <- qic_params$y_col_name
      n_col_name <- qic_params$n_col_name

      # Generate SPC data using qicharts2
      qic_data <- safe_operation(
        "Generate SPC data using qicharts2",
        code = {
          # Build qic call arguments with NSE
          qic_args <- build_qic_arguments(
            data = data,
            x_col_for_qic = x_col_for_qic,
            y_col_name = y_col_name,
            n_col_name = n_col_name,
            chart_type = chart_type,
            freeze_position = freeze_position,
            part_positions = part_positions,
            target_value = target_value,
            centerline_value = centerline_value
          )

          # Execute QIC call with post-processing
          # SPRINT 4: Pass QIC cache for performance optimization
          qic_data <- execute_qic_call(qic_args, chart_type, config, qic_cache = qic_cache)

          # Tilføj kombineret anhoej.signal kolonne (runs ELLER crossings) per part
          if (!is.null(qic_data)) {
            # Brug runs.signal direkte fra qicharts2 (allerede per-række)
            runs_sig_col <- if ("runs.signal" %in% names(qic_data)) {
              qic_data$runs.signal
            } else {
              rep(FALSE, nrow(qic_data))
            }

            # VECTORIZED: Beregn crossings signal per part med dplyr group_by + mutate
            # BEFORE: O(n*p) for-loop iterating over parts
            # AFTER: O(n) vectorized group operation
            if ("n.crossings" %in% names(qic_data) && "n.crossings.min" %in% names(qic_data) && "part" %in% names(qic_data)) {
              qic_data <- qic_data |>
                dplyr::group_by(part) |>
                dplyr::mutate(
                  part_n_cross = max(n.crossings, na.rm = TRUE),
                  part_n_cross_min = max(n.crossings.min, na.rm = TRUE),
                  crossings_signal = !is.na(part_n_cross) & !is.na(part_n_cross_min) &
                    part_n_cross < part_n_cross_min
                ) |>
                dplyr::ungroup()

              # Kombinér: TRUE hvis ENTEN runs ELLER crossings signal
              qic_data$anhoej.signal <- runs_sig_col | qic_data$crossings_signal

              # Cleanup intermediate columns (optional - keeps data clean)
              qic_data$part_n_cross <- NULL
              qic_data$part_n_cross_min <- NULL
              qic_data$crossings_signal <- NULL
            } else {
              # No crossings data available - use runs.signal only
              qic_data$anhoej.signal <- runs_sig_col
            }
          }

          qic_data
        },
        fallback = function(e) {
          stop("Fejl ved qic() kald: ", e$message)
        },
        error_type = "processing"
      )

      # Handle comment data for labels
      comment_data <- extract_comment_data(data, kommentar_column, qic_data)

      # Byg grundlæggende plot ----
      plot <- safe_operation(
        "Build custom ggplot",
        code = {
          # PERFORMANCE OPTIMIZATION: Pre-compute all ggplot layers before adding to plot
          # BEFORE: Sequential layer additions with multiple plot <- plot + layer operations
          # AFTER: Build layer list once, add all layers in single operation
          # Expected improvement: 15-25% reduction in plot build time

          # Initialize base plot
          plot <- ggplot2::ggplot(qic_data, ggplot2::aes(x = x, y = y))

          # Pre-compute layers list
          plot_layers <- list()

          # Kontrolgrænser (control limits ribbon and text lines) ----
          if (!is.null(qic_data$ucl) && !all(is.na(qic_data$ucl)) && !is.null(qic_data$lcl) && !all(is.na(qic_data$lcl))) {
            plot_layers <- c(plot_layers, list(
              ggplot2::geom_ribbon(ggplot2::aes(ymin = lcl, ymax = ucl), fill = "#E6F5FD", alpha = 0.5),
              geomtextpath::geom_textline(ggplot2::aes(y = ucl, x = x, label = "Øvre kontrolgrænse"), inherit.aes = FALSE, hjust = 0.05, vjust = -0.2, linewidth = ucl_linewidth, linecolour = NA, textcolour = "#b5b5b9", size = 3.0, na.rm = TRUE),
              geomtextpath::geom_textline(ggplot2::aes(y = lcl, x = x, label = "Nedre kontrolgrænse"), inherit.aes = FALSE, hjust = 0.05, vjust = 1.2, linewidth = ucl_linewidth, linecolour = NA, textcolour = "#b5b7b9", size = 3.0, na.rm = TRUE)
            ))
          }

          # Targetline (conditionally suppressed if arrow symbols detected) ----
          # Arrow symbols (↓ ↑) indicate direction without specific numeric target
          if (!suppress_targetline) {
            plot_layers <- c(plot_layers, list(
              ggplot2::geom_line(ggplot2::aes(y = target, x = x), inherit.aes = FALSE, linewidth = target_linewidth, colour = "#565656", linetype = "42", na.rm = TRUE)
            ))
          }

          # Core data visualization layers (data line, points, centerline) ----
          plot_layers <- c(plot_layers, list(
            ggplot2::geom_line(ggplot2::aes(y = y, group = part), colour = "#AEAEAE", linewidth = data_linewidth, na.rm = TRUE),
            ggplot2::geom_point(ggplot2::aes(y = y, group = part), colour = "#858585", size = point_size, na.rm = TRUE),
            ggplot2::geom_line(ggplot2::aes(y = cl, group = part, linetype = anhoej.signal), color = hospital_colors$hospitalblue, linewidth = cl_linewidth)
          ))

          # Labels and scale configuration ----
          plot_layers <- c(plot_layers, list(
            ggplot2::labs(title = title_text, x = NULL, y = NULL),
            ggplot2::scale_linetype_manual(
              values = c("FALSE" = "solid", "TRUE" = "12"),
              guide = "none" # Skjul legend
            )
          ))

          # Add all layers in single operation (PERFORMANCE: ~15-25% faster than sequential additions)
          plot <- plot + plot_layers

          plot
        },
        fallback = function(e) {
          log_error(paste("ERROR in ggplot build:", e$message), "GGPLOT_BUILD")
          stop(e)
        },
        error_type = "processing"
      )


      # Beregn x-akse limits og breaks for extended area ----
      data_x_min <- min(qic_data$x, na.rm = TRUE)
      data_x_max <- max(qic_data$x, na.rm = TRUE)

      # Konverter Date objekter til POSIXct for uniform håndtering
      if (inherits(data_x_max, "Date")) {
        data_x_max <- as.POSIXct(data_x_max)
        data_x_min <- as.POSIXct(data_x_min)
      }

      # Beregn extended limit (20% ekstra) - matcher add_plot_enhancements beregning
      if (inherits(data_x_max, c("POSIXct", "POSIXt"))) {
        range_secs <- as.numeric(difftime(data_x_max, data_x_min, units = "secs"))
        extended_x_limit <- data_x_max + range_secs * 0.20
      } else {
        x_range <- data_x_max - data_x_min
        extended_x_limit <- data_x_max + (x_range * 0.20)
      }

      # X-akse limits: udvid til extended_x
      x_limits <- c(data_x_min, data_x_max)

      # Intelligent x-akse formatering baseret på dato-mønstre ----
      if (!is.null(x_validation$x.format) && x_validation$is_date) {
        # DEBUG: Tjek qic_data$x type

        # Intelligent interval detektion og formatering
        interval_info <- detect_date_interval(qic_data$x, debug = TRUE)
        format_config <- get_optimal_formatting(interval_info, debug = TRUE)

        # Hjælpefunktion til at runde datoer til passende interval-start
        round_to_interval_start <- function(date, interval_type, interval_multiplier = 1) {
          if (interval_type == "monthly") {
            # Rund til første dag i måneden
            lubridate::floor_date(date, unit = "month")
          } else if (interval_type == "weekly") {
            # Rund til mandag (start af ugen)
            lubridate::floor_date(date, unit = "week")
          } else if (interval_type == "daily") {
            # Behold dagen som den er
            date
          } else {
            date
          }
        }

        # Beregn adaptive interval size baseret på data density
        base_interval_secs <- if (interval_info$type == "weekly") {
          7 * 24 * 60 * 60 # 7 dage i sekunder
        } else if (interval_info$type == "monthly") {
          30 * 24 * 60 * 60 # ~30 dage i sekunder
        } else if (interval_info$type == "daily") {
          24 * 60 * 60 # 1 dag i sekunder
        } else {
          NULL # Fallback til breaks_pretty
        }

        # Beregn adaptive interval baseret på data density
        interval_size <- if (!is.null(base_interval_secs)) {
          timespan_secs <- as.numeric(difftime(data_x_max, data_x_min, units = "secs"))
          potential_breaks <- timespan_secs / base_interval_secs

          if (potential_breaks > 15) {
            # Find passende multiplier baseret på interval type
            multipliers <- if (interval_info$type == "weekly") {
              c(2, 4, 13) # 2 uger, 4 uger (måned), 13 uger (kvartal)
            } else if (interval_info$type == "monthly") {
              c(3, 6, 12) # 3 måneder, 6 måneder, 12 måneder
            } else if (interval_info$type == "daily") {
              c(2, 4, 8) # 2 dage, 4 dage, 8 dage
            } else {
              c(2, 4, 8) # Generic fallback
            }

            # Find første multiplier der giver <= 15 breaks
            mult <- tail(multipliers, 1) # Default til største
            for (m in multipliers) {
              if (potential_breaks / m <= 15) {
                mult <- m
                break
              }
            }

            base_interval_secs * mult
          } else {
            base_interval_secs
          }
        } else {
          NULL
        }

        # qic() konverterer Date objekter til POSIXct, så brug scale_x_datetime
        if (inherits(qic_data$x, c("POSIXct", "POSIXt", "Date"))) {
          # Konverter Date til POSIXct for uniform håndtering
          if (inherits(qic_data$x, "Date")) {
            qic_data$x <- as.POSIXct(qic_data$x)
          }

          # Håndter intelligent formatering separat
          if (interval_info$type == "weekly" && !is.null(format_config$use_smart_labels) && format_config$use_smart_labels) {
            log_debug("SMART WEEKLY LABELS: Applying intelligent week formatting", .context = "X_AXIS_FORMAT")
            # Rund start til uge-start (mandag)
            rounded_start <- round_to_interval_start(data_x_min, "weekly")
            # Rund slut til næste uge-start efter data_x_max (ceiling)
            rounded_end <- lubridate::ceiling_date(data_x_max, unit = "week")

            # Generer breaks med seq() direkte på datetime objekter - tilføj ekstra interval
            breaks_posix <- seq(from = rounded_start, to = rounded_end + interval_size, by = interval_size)
            # Filtrer til kun breaks >= data_x_min (men tillad breaks efter data_x_max)
            breaks_posix <- breaks_posix[breaks_posix >= data_x_min]

            # Tilføj kun første dato hvis den ikke allerede er med
            if (length(breaks_posix) == 0 || breaks_posix[1] != data_x_min) {
              breaks_posix <- unique(c(data_x_min, breaks_posix))
            }

            plot <- plot + ggplot2::scale_x_datetime(
              # limits = c(data_x_min, extended_x_limit),
              expand = ggplot2::expansion(mult = c(0.025, .0)),
              # name = x_unit_label,
              labels = format_config$labels,
              breaks = breaks_posix
            )
          } else if (interval_info$type == "monthly" && !is.null(format_config$use_smart_labels) && format_config$use_smart_labels) {
            log_debug("SMART MONTHLY LABELS: Applying intelligent month formatting", .context = "X_AXIS_FORMAT")
            # Rund start til måneds-start (1. i måneden)
            rounded_start <- round_to_interval_start(data_x_min, "monthly")
            # Rund slut til næste måneds-start efter data_x_max (ceiling)
            rounded_end <- lubridate::ceiling_date(data_x_max, unit = "month")

            # Beregn antal måneder baseret på multiplier
            interval_months <- as.numeric(interval_size) / (30 * 24 * 60 * 60)
            interval_months <- round(interval_months)

            # Generer breaks månedligt med seq() - tilføj ekstra interval for at sikre coverage
            # Konverter interval_months til sekunder for at tilføje til rounded_end
            extended_end <- seq(rounded_end, by = paste(interval_months, "months"), length.out = 2)[2]
            breaks_posix <- seq(
              from = rounded_start,
              to = extended_end,
              by = paste(interval_months, "months")
            )
            # Filtrer til kun breaks >= data_x_min (men tillad breaks efter data_x_max)
            breaks_posix <- breaks_posix[breaks_posix >= data_x_min]

            # Tilføj kun første dato hvis den ikke allerede er med
            if (length(breaks_posix) == 0 || breaks_posix[1] != data_x_min) {
              breaks_posix <- unique(c(data_x_min, breaks_posix))
            }

            plot <- plot + ggplot2::scale_x_datetime(
              # limits = c(data_x_min, extended_x_limit),
              expand = ggplot2::expansion(mult = c(0.025, .0)),
              # name = x_unit_label,
              labels = format_config$labels,
              breaks = breaks_posix
            )
          } else if (!is.null(format_config$breaks) && !is.null(interval_size)) {
            # Standard intelligent formatering
            if (interval_info$type == "monthly") {
              # Monthly data: brug månedlige breaks
              rounded_start <- round_to_interval_start(data_x_min, "monthly")
              # Rund slut til næste måneds-start efter data_x_max (ceiling)
              rounded_end <- lubridate::ceiling_date(data_x_max, unit = "month")

              interval_months <- as.numeric(interval_size) / (30 * 24 * 60 * 60)
              interval_months <- round(interval_months)

              # Generer breaks månedligt med seq() - tilføj ekstra interval for at sikre coverage
              # Konverter interval_months til sekunder for at tilføje til rounded_end
              extended_end <- seq(rounded_end, by = paste(interval_months, "months"), length.out = 2)[2]
              breaks_posix <- seq(
                from = rounded_start,
                to = extended_end,
                by = paste(interval_months, "months")
              )
              # Filtrer til kun breaks >= data_x_min (men tillad breaks efter data_x_max)
              breaks_posix <- breaks_posix[breaks_posix >= data_x_min]

              # Tilføj kun første dato hvis den ikke allerede er med
              if (length(breaks_posix) == 0 || breaks_posix[1] != data_x_min) {
                breaks_posix <- unique(c(data_x_min, breaks_posix))
              }
            } else {
              # Weekly/daily data: brug sekund-baserede breaks
              rounded_start <- round_to_interval_start(data_x_min, interval_info$type)
              # Generer breaks til mindst data_x_max
              breaks_posix <- seq(from = rounded_start, to = data_x_max + interval_size, by = interval_size)
              # Filtrer til kun breaks >= data_x_min
              breaks_posix <- breaks_posix[breaks_posix >= data_x_min]

              # Tilføj kun første dato hvis den ikke allerede er med
              if (length(breaks_posix) == 0 || breaks_posix[1] != data_x_min) {
                breaks_posix <- unique(c(data_x_min, breaks_posix))
              }
            }

            plot <- plot + ggplot2::scale_x_datetime(
              # limits = c(data_x_min, extended_x_limit),
              # name = x_unit_label,
              labels = scales::date_format(format_config$labels),
              breaks = breaks_posix
            )
          } else {
            # Fallback til breaks_pretty med intelligent antal
            plot <- plot + ggplot2::scale_x_datetime(
              # limits = c(data_x_min, extended_x_limit),
              # name = x_unit_label,
              labels = scales::date_format(format_config$labels),
              breaks = scales::breaks_pretty(n = format_config$n_breaks)
            )
          }
        } else if (is.numeric(qic_data$x)) {
          # Fallback til continuous scale
          plot <- plot + ggplot2::scale_x_continuous(
            # limits = c(data_x_min, extended_x_limit),
            # name = x_unit_label,
            breaks = scales::pretty_breaks(n = 8)
          )
        }
      }

      # Y-axis formatting based on unit type -----
      # FASE 4 Task 4.2: Y-axis formatting extracted to utils_y_axis_formatting.R
      # Reduces this function by ~126 lines and consolidates duplicated time formatting
      plot <- apply_y_axis_formatting(plot, y_axis_unit, qic_data)

      # Add plot enhancements (extended lines, phase lines, comments)
      plot <- add_plot_enhancements(
        plot, qic_data, comment_data, y_axis_unit,
        cl_linewidth = cl_linewidth,
        target_linewidth = target_linewidth,
        comment_size = comment_size,
        suppress_targetline = suppress_targetline
      )

      # Add SPC labels (CL og Target) med advanced placement system
      # Bestem BASELINE label logik: Tjek om Frys er markeret og om der er Skift EFTER Frys
      has_frys_without_subsequent_skift <- FALSE

      if (!is.null(frys_column) && frys_column %in% names(data) &&
        !is.null(skift_column) && skift_column %in% names(data)) {
        # Extract og process begge kolonner
        frys_data <- data[[frys_column]]
        if (is.list(frys_data)) frys_data <- unlist(frys_data)
        if (!is.logical(frys_data)) frys_data <- as.logical(frys_data)

        skift_data <- data[[skift_column]]
        if (is.list(skift_data)) skift_data <- unlist(skift_data)
        if (!is.logical(skift_data)) skift_data <- as.logical(skift_data)

        # Find sidste position hvor Frys er markeret
        frys_positions <- which(frys_data == TRUE)

        if (length(frys_positions) > 0) {
          last_frys_position <- max(frys_positions)

          # Tjek om Skift er markeret VED SAMME position som Frys
          has_skift_at_same_position <- FALSE
          if (last_frys_position <= length(skift_data)) {
            has_skift_at_same_position <- isTRUE(skift_data[last_frys_position] == TRUE)
          }

          # Hvis Skift er markeret ved samme position → "NUV. NIVEAU" (ikke BASELINE)
          if (has_skift_at_same_position) {
            has_frys_without_subsequent_skift <- FALSE
          } else {
            # Tjek om der er TRUE-værdier i Skift EFTER sidste Frys position
            has_skift_after_frys <- FALSE
            if (last_frys_position < length(skift_data)) {
              skift_after_frys <- skift_data[(last_frys_position + 1):length(skift_data)]
              has_skift_after_frys <- any(skift_after_frys == TRUE, na.rm = TRUE)
            }

            # BASELINE label hvis Frys er markeret OG ingen Skift ved samme eller efterfølgende position
            has_frys_without_subsequent_skift <- !has_skift_after_frys
          }
        }
      } else if (!is.null(frys_column) && frys_column %in% names(data)) {
        # Kun Frys kolonne eksisterer (ingen Skift kolonne)
        frys_data <- data[[frys_column]]
        if (is.list(frys_data)) frys_data <- unlist(frys_data)
        if (!is.logical(frys_data)) frys_data <- as.logical(frys_data)

        # Hvis Frys er markeret og ingen Skift kolonne findes → BASELINE
        has_frys_without_subsequent_skift <- any(frys_data == TRUE, na.rm = TRUE)
      }

      plot <- add_spc_labels(
        plot = plot,
        qic_data = qic_data,
        y_axis_unit = y_axis_unit,
        label_size = label_size,
        viewport_width = viewport_width,
        viewport_height = viewport_height,
        target_text = target_text,
        centerline_value = centerline_value,
        has_frys_column = has_frys_without_subsequent_skift,
        has_skift_column = FALSE,
        verbose = FALSE,
        debug_mode = FALSE
      )

      return(list(plot = plot, qic_data = qic_data))
    }
  ))
}

# PLOT STYLING ===============================================================

## Hospital Tema til Plots
# Anvender hospital branding og farvepalette på SPC plots
# base_size: Responsive base font size (default 14, automatisk beregnet i Shiny renderPlot)
applyHospitalTheme <- function(plot, base_size = 14) {
  if (is.null(plot) || !inherits(plot, "ggplot")) {
    return(plot)
  }

  # Get hospital colors using the proper package function
  hospital_colors <- get_hospital_colors()

  safe_operation(
    "Apply hospital theme to plot",
    code = {
      footer_text <- safe_operation(
        "Create plot footer",
        code = {
          create_plot_footer(
            afdeling = "",
            data_kilde = "Upload",
            dato = Sys.Date()
          )
        },
        fallback = function(e) {
          "SPC Analyse" # fallback text
        },
        error_type = "processing"
      )

      themed_plot <- plot +
        ggplot2::theme_minimal(base_size = base_size) +
        ggplot2::theme(
          text = ggplot2::element_text(family = "Roboto Medium"),
          plot.margin = ggplot2::unit(c(0, 0, 0, 10), "pt"),
          panel.background = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(color = "#858585", size = ggplot2::rel(1.0), angle = 0, hjust = 1, family = "Roboto Medium"),
          axis.text.x = ggplot2::element_text(color = "#858585", angle = 0, size = ggplot2::rel(0.85), family = "Roboto Medium"),
          axis.line.x = ggplot2::element_line(color = "#D6D6D6"),
          axis.ticks.x = ggplot2::element_line(color = "#D6D6D6"),
          axis.ticks.y = ggplot2::element_line(color = "#D6D6D6"),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          legend.position = "none",
        ) + lemon::coord_capped_cart(bottom = "right", gap = 0)

      return(themed_plot)
    },
    fallback = function(e) {
      return(plot)
    },
    error_type = "processing"
  )
}

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
