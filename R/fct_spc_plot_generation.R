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

  # Udtrækker kommentarer og synkroniserer med qic_data
  comments_raw <- data[[kommentar_column]]

  # Opret kommentar data frame aligned med qic_data
  comment_data <- data.frame(
    x = qic_data$x,
    y = qic_data$y,
    comment = comments_raw[1:nrow(qic_data)], # Sikr samme længde som qic_data
    stringsAsFactors = FALSE
  )

  # Filtrer til kun ikke-tomme kommentarer
  comment_data <- comment_data[
    !is.na(comment_data$comment) &
      trimws(comment_data$comment) != "",
  ]

  # Afkort meget lange kommentarer
  if (nrow(comment_data) > 0) {
    comment_data$comment <- dplyr::if_else(
      nchar(comment_data$comment) > 40,
      stringr::str_c(substr(comment_data$comment, 1, 37), "..."),
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

    # Håndter part positioner: juster for fjernede rækker
    if ("part" %in% names(call_args)) {
      removed_positions <- which(!complete_cases)

      if (length(removed_positions) > 0) {
        # Juster part positioner ved at trække fjernede rækker før hver position using tidyverse
        adjusted_part <- call_args$part |>
          purrr::map_dbl(~ {
            pos <- .x
            removed_before <- sum(removed_positions < pos)
            pos - removed_before
          })

        # Fjern ugyldige positioner
        valid_parts <- adjusted_part > 0 & adjusted_part <= sum(complete_cases)
        call_args$part <- adjusted_part[valid_parts]

        if (length(call_args$part) == 0) {
          call_args$part <- NULL
        }
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

  # Tilføj n for P/U charts
  if (chart_type %in% c("p", "pp", "u", "up") && !is.null(n_data)) {
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
  n_data <- parsed_data$n_data  # Keep for qicharts2
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
    n_data = NULL,  # No n_data for standard charts
    ylab_text = ylab_text,
    y_unit_label = y_unit_label
  ))
}

# QIC DATA GENERATION UTILITIES ===============================================

## Prepare QIC Data Parameters
# Forbereder data parametre til qicharts2 integration med NSE håndtering
prepare_qic_data_parameters <- function(data, config, x_validation) {
  x_col_name <- config$x_col # Auto-detected date column or NULL
  y_col_name <- config$y_col # Should be "Tæller"
  n_col_name <- config$n_col # Should be "Nævner"

  # Brug data fra x_validation i stedet for duplikeret logik
  log_debug(paste("UPDATE CONDITION DEBUG:\n- x_col_name is not NULL:", !is.null(x_col_name),
                  "\n- x_col_name in names(data):", if (!is.null(x_col_name)) x_col_name %in% names(data) else "N/A",
                  "\n- x_validation$is_date:", x_validation$is_date,
                  if (!is.null(x_col_name) && x_col_name %in% names(data))
                    paste("\n- data[[x_col_name]] is character:", is.character(data[[x_col_name]])) else ""), "DATA_PROCESS")

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
        log_debug("Length mismatch - using observation sequence as fallback", "DATA_PROCESS")
        # Fallback til observation sekvens
        if (!("obs_sequence" %in% names(data))) {
          data$obs_sequence <- 1:nrow(data)
        }
        x_col_for_qic <- "obs_sequence"
      }
    } else {
      # CHARACTER COLUMN: Convert to factor with original row order to prevent alphabetical sorting
      original_labels <- data[[x_col_name]]
      unique_labels <- unique(original_labels) # Preserves original order from dataset

      # Convert to factor with levels in dataset order (not alphabetical)
      data[[x_col_name]] <- factor(original_labels, levels = unique_labels)
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
                               chart_type, freeze_position, part_positions, centerline_value) {
  # Build qic call arguments
  qic_args <- list(
    data = data,
    chart = chart_type,
    return.data = TRUE
  )

  # Add column names using non-standard evaluation (NSE) approach
  if (!is.null(x_col_for_qic)) qic_args$x <- as.name(x_col_for_qic)
  if (!is.null(y_col_name)) qic_args$y <- as.name(y_col_name)
  if (!is.null(n_col_name)) qic_args$n <- as.name(n_col_name)

  # Add freeze for baseline - can be used together with part
  if (!is.null(freeze_position)) {
    qic_args$freeze <- freeze_position
  }

  # Add part for phase splits - can be used together with freeze
  if (!is.null(part_positions)) {
    qic_args$part <- part_positions
  }

  # Add centerline if provided
  if (!is.null(centerline_value) && is.numeric(centerline_value) && !is.na(centerline_value)) {
    qic_args$cl <- centerline_value
  }

  return(qic_args)
}

## Execute QIC Call with Post-processing
# Udfører qicharts2::qic() kald og post-processerer resultaterne
execute_qic_call <- function(qic_args, chart_type, config) {
  # Call qic() with prepared arguments
  if (getOption("debug.mode", FALSE)) {
    log_debug("qic_args structure:", "QIC_CALL")
    log_debug(qic_args, "QIC_CALL")
  }

  log_debug(qic_args, "QIC")

  qic_data <- do.call(qicharts2::qic, qic_args)

  # Convert proportions to percentages for run charts with rate data
  if (chart_type == "run" && !is.null(config$n_col) && config$n_col %in% names(qic_args$data)) {
    qic_data$y <- qic_data$y * 100
    qic_data$cl <- qic_data$cl * 100

    if (!is.null(qic_data$ucl) && !all(is.na(qic_data$ucl))) {
      qic_data$ucl <- qic_data$ucl * 100
    }
    if (!is.null(qic_data$lcl) && !all(is.na(qic_data$lcl))) {
      qic_data$lcl <- qic_data$lcl * 100
    }
  }

  return(qic_data)
}

# PLOT ENHANCEMENT UTILITIES ==================================================

## Add Plot Enhancements
# Tilføjer target lines, phase separations og comment annotations
add_plot_enhancements <- function(plot, qic_data, target_value, comment_data) {
  # Add phase separation lines if parts exist
  if ("part" %in% names(qic_data) && length(unique(qic_data$part)) > 1) {
    # Find phase change points
    phase_changes <- which(diff(as.numeric(qic_data$part)) != 0)
    # Add phase change lines using tidyverse approach
    if (length(phase_changes) > 0) {
      plot <- phase_changes |>
        purrr::reduce(function(p, change_point) {
          p +
            ggplot2::geom_vline(
              xintercept = qic_data$x[change_point + 1],
              color = HOSPITAL_COLORS$warning,
              linetype = "dotted", linewidth = 1, alpha = 0.7
            )
        }, .init = plot)
    }
  }

  # Add target line if provided
  if (!is.null(target_value) && is.numeric(target_value) && !is.na(target_value)) {
    plot <- plot +
      ggplot2::geom_hline(
        yintercept = target_value,
        color = HOSPITAL_COLORS$darkgrey, linetype = "42", linewidth = 1.2,
        alpha = 0.8
      )
  }

  # Add comment labels with ggrepel if comments exist
  if (!is.null(comment_data) && nrow(comment_data) > 0) {
    plot <- plot +
      ggrepel::geom_text_repel(
        data = comment_data,
        ggplot2::aes(x = x, y = y, label = comment),
        size = 3,
        color = HOSPITAL_COLORS$darkgrey,
        bg.color = "white",
        bg.r = 0.1,
        box.padding = 0.5,
        point.padding = 0.5,
        segment.color = HOSPITAL_COLORS$mediumgrey,
        segment.size = 0.3,
        nudge_x = .15,
        nudge_y = .5,
        segment.curvature = -1e-20,
        arrow = arrow(length = unit(0.015, "npc")),
        max.overlaps = Inf,
        inherit.aes = FALSE
      )
  }

  return(plot)
}

generateSPCPlot <- function(data, config, chart_type, target_value = NULL, centerline_value = NULL, show_phases = FALSE, skift_column = NULL, frys_column = NULL, chart_title_reactive = NULL, y_axis_unit = "count", kommentar_column = NULL) {
  # Generate SPC plot with specified parameters
  log_debug(paste("generateSPCPlot:", chart_type, "|", nrow(data), "rows"), "SPC_CALC_DEBUG")

  # Input validation and configuration sanitization
  validate_spc_inputs(data, config)
  config <- sanitize_spc_config(config)

  # Process chart title
  title_text <- process_chart_title(chart_title_reactive, config)

  # Extract X-axis data
  x_data <- extract_x_axis_data(data, config$x_col)

  # Process data based on chart type
  if (!is.null(config$n_col) && config$n_col %in% names(data)) {
    # Ratio charts (with numerator/denominator)
    data_result <- process_ratio_chart_data(data, config, chart_type, y_axis_unit)
  } else {
    # Standard numeric charts (single value)
    data_result <- process_standard_chart_data(data, config, chart_type, y_axis_unit)
  }

  # Extract processed data
  data <- data_result$data
  y_data <- data_result$y_data
  n_data <- data_result$n_data
  ylab_text <- data_result$ylab_text
  y_unit_label <- data_result$y_unit_label

  # Ensure we have minimum data points after filtering
  if (length(y_data) < 3) {
    stop(paste("For få gyldige datapunkter efter filtrering (", length(y_data), " fundet, minimum 3 påkrævet). Tilføj flere gyldige datapunkter."))
  }

  # Handle x-axis data med intelligent formatering - EFTER data filtrering
  # FASE 5: Performance optimization - cache expensive x-column validation
  data_hash <- paste0(nrow(data), "_", ncol(data), "_", paste(names(data), collapse = "_"))

  # ROBUST CACHE KEY: Safe ID generation to handle character(0) and NULL values
  safe_x_col_id <- if (is.null(config$x_col) || length(config$x_col) == 0 || identical(config$x_col, character(0)) || is.na(config$x_col)) {
    "NULL_XCOL"
  } else {
    # Sanitize column name for cache key (remove problematic characters)
    gsub("[^a-zA-Z0-9_]", "_", as.character(config$x_col)[1])
  }

  cache_key <- paste0("x_validation_", safe_x_col_id, "_", substr(data_hash, 1, 20))

  x_validation <- create_cached_reactive({
    validate_x_column_format(data, config$x_col, "observation")
  }, cache_key, cache_timeout = PERFORMANCE_THRESHOLDS$cache_timeout_default)()
  x_data <- x_validation$x_data
  # xlab_text <- if (x_unit_label != "") x_unit_label else {
  #   if (x_validation$is_date) "Dato" else "Observation"
  # }


  # Handle phases and freeze configuration
  phase_freeze_config <- process_phase_freeze_config(data, show_phases, skift_column, frys_column)
  part_positions <- phase_freeze_config$part_positions
  freeze_position <- phase_freeze_config$freeze_position

  # Build qic call arguments dynamically
  call_args <- build_qic_call_arguments(
    x_data = x_data,
    y_data = y_data,
    chart_type = chart_type,
    title_text = title_text,
    ylab_text = ylab_text,
    n_data = if (exists("n_data")) n_data else NULL,
    freeze_position = freeze_position,
    part_positions = part_positions,
    target_value = target_value
  )

  # Clean data and prepare arguments for QIC call
  call_args <- clean_qic_call_args(call_args)

  if (length(call_args$y) < 3) {
    stop("For få datapunkter efter rensning (minimum 3 påkrævet)")
  }

  # Generate SPC data using qicharts2

  # Generate SPC data using qicharts2 and build custom ggplot
  return(safe_operation(
    "Generate SPC plot data",
    code = {
      # Use data parameter approach like the working example
      # qicharts2::qic(x = Dato, y = `Tæller`, n = `Nævner`, part = c(12), data = data, return.data = TRUE)

      # Prepare QIC data parameters with NSE handling
      qic_params <- prepare_qic_data_parameters(data, config, x_validation)
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
            centerline_value = centerline_value
          )

          # Execute QIC call with post-processing
          qic_data <- execute_qic_call(qic_args, chart_type, config)

          qic_data
        },
        fallback = function(e) {
          stop("Fejl ved qic() kald: ", e$message)
        },
        error_type = "processing"
      )

      # Handle comment data for labels
      comment_data <- extract_comment_data(data, kommentar_column, qic_data)

      # Build custom ggplot using qic calculations

      plot <- safe_operation(
        "Build custom ggplot",
        code = {
          plot <- ggplot2::ggplot(qic_data, ggplot2::aes(x = x, y = y))

          plot <- plot + ggplot2::geom_line(color = HOSPITAL_COLORS$lightgrey, linewidth = 1)

          plot <- plot + ggplot2::geom_point(size = 2, color = HOSPITAL_COLORS$mediumgrey)

          plot <- plot + ggplot2::geom_line(ggplot2::aes(y = cl),
            color = HOSPITAL_COLORS$hospitalblue,
            linetype = "solid", linewidth = 1
          )

          plot <- plot + ggplot2::labs(title = call_args$title, x = "", y = "")

          plot <- plot + ggplot2::theme_minimal()

          plot
        },
        fallback = function(e) {
          log_error(paste("ERROR in ggplot build:", e$message), "GGPLOT_BUILD")
          stop(e)
        },
        error_type = "processing"
      )

      # Add control limits conditionally
      if (!is.null(qic_data$ucl) && !all(is.na(qic_data$ucl))) {
        plot <- plot +
          ggplot2::geom_line(ggplot2::aes(y = ucl),
            color = HOSPITAL_COLORS$danger,
            linetype = "dashed", linewidth = 0.8
          )
      }

      if (!is.null(qic_data$lcl) && !all(is.na(qic_data$lcl))) {
        plot <- plot +
          ggplot2::geom_line(ggplot2::aes(y = lcl),
            color = HOSPITAL_COLORS$danger,
            linetype = "dashed", linewidth = 0.8
          )
      }

      # Intelligent x-akse formatering baseret på dato-mønstre
      if (!is.null(x_validation$x.format) && x_validation$is_date) {
        # DEBUG: Tjek qic_data$x type

        # Intelligent interval detektion og formatering
        interval_info <- detect_date_interval(qic_data$x, debug = TRUE)
        format_config <- get_optimal_formatting(interval_info, debug = TRUE)

        # qic() konverterer Date objekter til POSIXct, så brug scale_x_datetime
        if (inherits(qic_data$x, c("POSIXct", "POSIXt"))) {
          # Håndter intelligent formatering separat
          if (interval_info$type == "weekly" && !is.null(format_config$use_smart_labels) && format_config$use_smart_labels) {
            log_debug("SMART WEEKLY LABELS: Applying intelligent week formatting", "X_AXIS_FORMAT")
            plot <- plot + ggplot2::scale_x_datetime(
              name = x_unit_label,
              labels = format_config$labels, # Smart scales::label_date_short()
              # breaks = scales::date_breaks(format_config$breaks)
              breaks = scales::breaks_pretty(n = format_config$n_breaks)
            )
          } else if (interval_info$type == "monthly" && !is.null(format_config$use_smart_labels) && format_config$use_smart_labels) {
            log_debug("SMART MONTHLY LABELS: Applying intelligent month formatting", "X_AXIS_FORMAT")
            plot <- plot + ggplot2::scale_x_datetime(
              name = x_unit_label,
              labels = format_config$labels, # Smart scales::label_date_short()
              breaks = scales::date_breaks(format_config$breaks)
            )
          } else if (!is.null(format_config$breaks)) {
            # Standard intelligent formatering
            plot <- plot + ggplot2::scale_x_datetime(
              name = x_unit_label,
              labels = scales::date_format(format_config$labels),
              breaks = scales::date_breaks(format_config$breaks)
            )
          } else {
            # Fallback til breaks_pretty med intelligent antal
            plot <- plot + ggplot2::scale_x_datetime(
              name = x_unit_label,
              labels = scales::date_format(format_config$labels),
              breaks = scales::breaks_pretty(n = format_config$n_breaks)
            )
          }
        } else if (inherits(qic_data$x, "Date")) {
          # Date objekter - tilsvarende intelligent håndtering
          if (interval_info$type == "weekly" && !is.null(format_config$use_smart_labels) && format_config$use_smart_labels) {
            log_debug("SMART WEEKLY LABELS: Applying intelligent week formatting for Date objects", "X_AXIS_FORMAT")
            plot <- plot + ggplot2::scale_x_date(
              name = x_unit_label,
              labels = format_config$labels, # Smart scales::label_date_short()
              breaks = scales::date_breaks(format_config$breaks)
            )
          } else if (interval_info$type == "monthly" && !is.null(format_config$use_smart_labels) && format_config$use_smart_labels) {
            log_debug("SMART MONTHLY LABELS: Applying intelligent month formatting for Date objects", "X_AXIS_FORMAT")
            plot <- plot + ggplot2::scale_x_date(
              name = x_unit_label,
              labels = format_config$labels, # Smart scales::label_date_short()
              breaks = scales::date_breaks(format_config$breaks)
            )
          } else if (!is.null(format_config$breaks)) {
            plot <- plot + ggplot2::scale_x_date(
              name = x_unit_label,
              labels = scales::date_format(format_config$labels),
              breaks = scales::date_breaks(format_config$breaks)
            )
          } else {
            plot <- plot + ggplot2::scale_x_date(
              name = x_unit_label,
              labels = scales::date_format(format_config$labels),
              breaks = scales::breaks_pretty(n = format_config$n_breaks)
            )
          }
        } else if (is.numeric(qic_data$x)) {
          # Fallback til continuous scale
          plot <- plot + ggplot2::scale_x_continuous(
            name = x_unit_label,
            breaks = scales::pretty_breaks(n = 8)
          )
        }
      }

      # Add plot enhancements (phase lines, target line, comments)
      plot <- add_plot_enhancements(plot, qic_data, target_value, comment_data)

      return(list(plot = plot, qic_data = qic_data))
    },
    fallback = function(e) {
      # Fallback to basic ggplot if qic() fails
      plot_data <- data.frame(x = call_args$x, y = call_args$y)

      plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(size = 2, color = HOSPITAL_COLORS$primary) +
        ggplot2::geom_line(color = HOSPITAL_COLORS$primary, alpha = 0.7) +
        ggplot2::geom_hline(
          yintercept = median(call_args$y, na.rm = TRUE),
          color = HOSPITAL_COLORS$secondary, linetype = "dashed"
        ) +
        ggplot2::labs(title = call_args$title, x = "", y = "") +
        ggplot2::theme_minimal()

      # Add target line if provided
      if (!is.null(target_value) && is.numeric(target_value) && !is.na(target_value)) {
        plot <- plot +
          ggplot2::geom_hline(
            yintercept = target_value,
            color = SPC_COLORS$target_line,
            linetype = SPC_LINE_TYPES$solid,
            linewidth = SPC_LINE_WIDTHS$thick,
            alpha = SPC_ALPHA_VALUES$target_line
          )
      }

      return(list(plot = plot, qic_data = NULL))
    },
    error_type = "processing"
  ))
}

# PLOT STYLING ===============================================================

## Hospital Tema til Plots
# Anvender hospital branding og farvepalette på SPC plots
applyHospitalTheme <- function(plot) {
  if (is.null(plot) || !inherits(plot, "ggplot")) {
    return(plot)
  }

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
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(color = HOSPITAL_COLORS$primary, size = 14, face = "bold"),
          plot.subtitle = ggplot2::element_text(color = HOSPITAL_COLORS$secondary, size = 12),
          axis.title = ggplot2::element_text(color = HOSPITAL_COLORS$dark, size = 11),
          axis.text = ggplot2::element_text(color = HOSPITAL_COLORS$dark, size = 10),
          legend.title = ggplot2::element_text(color = HOSPITAL_COLORS$dark, size = 11),
          legend.text = ggplot2::element_text(color = HOSPITAL_COLORS$dark, size = 10),
          panel.grid.major = ggplot2::element_line(color = HOSPITAL_COLORS$light),
          panel.grid.minor = ggplot2::element_line(color = HOSPITAL_COLORS$light),
          strip.text = ggplot2::element_text(color = HOSPITAL_COLORS$primary, face = "bold")
        ) +
        ggplot2::labs(caption = footer_text) +
        ggplot2::theme(
          plot.caption = ggplot2::element_text(size = 8, color = HOSPITAL_COLORS$secondary, hjust = 0)
        )

      return(themed_plot)
    },
    fallback = function(e) {
      return(plot)
    },
    error_type = "processing"
  )
}

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
