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
  if (!".original_row_id" %in% names(qic_data)) {
    log_warn("Missing .original_row_id in qic_data - falling back to positional mapping", .context = "PLOT_COMMENT")
    # Fallback til gamle positionsbaserede mapping
    comments_raw <- data[[kommentar_column]]
    comment_data <- data.frame(
      x = qic_data$x,
      y = qic_data$y,
      comment = comments_raw[1:min(length(comments_raw), nrow(qic_data))],
      stringsAsFactors = FALSE
    )
  } else {
    # ROBUST MAPPING: Join på .original_row_id for korrekt kommentar-punkt mapping
    original_data_with_comments <- data.frame(
      .original_row_id = 1:nrow(data),
      comment = data[[kommentar_column]],
      stringsAsFactors = FALSE
    )

    # Join qic_data med original kommentarer via row-id
    qic_data_with_comments <- merge(
      qic_data[, c("x", "y", ".original_row_id")],
      original_data_with_comments,
      by = ".original_row_id",
      all.x = TRUE,
      sort = FALSE
    )

    comment_data <- data.frame(
      x = qic_data_with_comments$x,
      y = qic_data_with_comments$y,
      comment = qic_data_with_comments$comment,
      stringsAsFactors = FALSE
    )
  }

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
          max_length = 100,  # Longer limit for comments before truncation
          allowed_chars = "A-Za-z0-9_æøåÆØÅ .,-:!?",
          html_escape = TRUE
        )
      }, USE.NAMES = FALSE)
    }

    # Afkort meget lange kommentarer efter sanitization
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
prepare_qic_data_parameters <- function(data, config, x_validation, chart_type) {
  x_col_name <- config$x_col # Auto-detected date column or NULL
  y_col_name <- config$y_col # Should be "Tæller"
  # Kun medtag nævner-kolonne for diagramtyper der kræver den
  n_col_name <- if (chart_type_requires_denominator(chart_type)) config$n_col else NULL

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
execute_qic_call <- function(qic_args, chart_type, config) {
  # Call qic() with prepared arguments
  if (getOption("debug.mode", FALSE)) {
    log_debug("qic_args structure:", .context = "QIC_CALL")
    log_debug(qic_args, .context = "QIC_CALL")
  }

  log_debug(qic_args, .context = "QIC")

  # Get call number for debugging correlation
  call_number <- if (exists("qic_call_counter", envir = .GlobalEnv)) {
    get("qic_call_counter", envir = .GlobalEnv)
  } else {
    NULL
  }

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
      expr = log_qic_call_wrapper(qic_args, "execute_qic_call_benchmark", call_number),
      times = benchmark_iterations,
      operation_name = paste0("qic_", chart_type, "_", size_category, "_", data_size, "_rows"),
      log_results = TRUE,
      capture_result = TRUE
    )

    # Use result from benchmark to eliminate redundant QIC call
    qic_data <- benchmark_result$captured_result
  } else {
    # Fallback: Execute without benchmarking with debug logging
    qic_data <- log_qic_call_wrapper(qic_args, "execute_qic_call_fallback", call_number)
  }

  qic_data
}

# PLOT ENHANCEMENT UTILITIES ==================================================

## Add Plot Enhancements
# Tilføjer target lines, phase separations og comment annotations
add_plot_enhancements <- function(plot, qic_data, comment_data, y_axis_unit = "count") {
  # Get hospital colors using the proper package function
  hospital_colors <- get_hospital_colors()

  # Hjælpefunktion til at formatere værdier PRÆCIS som y-aksen ----
  format_y_value <- function(val, y_unit) {
    if (is.na(val)) return(NA_character_)

    if (y_unit == "percent") {
      # Percent formatting - matcher scale_y_continuous(labels = scales::label_percent())
      scales::label_percent()(val)
    } else if (y_unit == "count") {
      # Count formatting with K/M notation - matcher y-akse logik præcist
      if (abs(val) >= 1e9) {
        scaled <- val / 1e9
        if (scaled == round(scaled)) {
          paste0(round(scaled), " mia.")
        } else {
          paste0(format(scaled, decimal.mark = ",", nsmall = 1), " mia.")
        }
      } else if (abs(val) >= 1e6) {
        scaled <- val / 1e6
        if (scaled == round(scaled)) {
          paste0(round(scaled), "M")
        } else {
          paste0(format(scaled, decimal.mark = ",", nsmall = 1), "M")
        }
      } else if (abs(val) >= 1e3) {
        scaled <- val / 1e3
        if (scaled == round(scaled)) {
          paste0(round(scaled), "K")
        } else {
          paste0(format(scaled, decimal.mark = ",", nsmall = 1), "K")
        }
      } else {
        # For values < 1000: show decimals only if present
        if (val == round(val)) {
          format(round(val), decimal.mark = ",", big.mark = ".")
        } else {
          format(val, decimal.mark = ",", big.mark = ".", nsmall = 1)
        }
      }
    } else if (y_unit == "rate") {
      # Rate formatting - kun decimaler hvis tilstede
      if (val == round(val)) {
        format(round(val), decimal.mark = ",")
      } else {
        format(val, decimal.mark = ",", nsmall = 1)
      }
    } else if (y_unit == "time") {
      # Time formatting (input: minutes)
      y_range <- range(qic_data$y, na.rm = TRUE)
      max_minutes <- max(y_range, na.rm = TRUE)

      if (max_minutes < 60) {
        # Minutes
        if (val == round(val)) {
          paste0(round(val), " min")
        } else {
          paste0(format(val, decimal.mark = ",", nsmall = 1), " min")
        }
      } else if (max_minutes < 1440) {
        # Hours
        hours <- val / 60
        if (hours == round(hours)) {
          paste0(round(hours), " timer")
        } else {
          paste0(format(hours, decimal.mark = ",", nsmall = 1), " timer")
        }
      } else {
        # Days
        days <- val / 1440
        if (days == round(days)) {
          paste0(round(days), " dage")
        } else {
          paste0(format(days, decimal.mark = ",", nsmall = 1), " dage")
        }
      }
    } else {
      # Default formatting
      if (val == round(val)) {
        format(round(val), decimal.mark = ",")
      } else {
        format(val, decimal.mark = ",", nsmall = 1)
      }
    }
  }

  # Opret label data for centerline og target ----
  label_data <- data.frame()

  # Centerline label KUN for seneste part
  if (!is.null(qic_data$cl) && any(!is.na(qic_data$cl))) {
    # Find seneste part
    latest_part <- max(qic_data$part, na.rm = TRUE)
    part_data <- qic_data[qic_data$part == latest_part & !is.na(qic_data$part), ]

    if (nrow(part_data) > 0) {
      # Brug sidste punkt i seneste part til label placering
      last_row <- part_data[nrow(part_data), ]
      cl_value <- last_row$cl
      if (!is.na(cl_value)) {
        label_data <- rbind(label_data, data.frame(
          x = last_row$x,
          y = cl_value,
          label = paste0("NUV. NIVEAU: ", format_y_value(cl_value, y_axis_unit)),
          type = "cl",
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  # Target label (kun én gang - target er konstant)
  if (!is.null(qic_data$target) && any(!is.na(qic_data$target))) {
    target_value <- qic_data$target[!is.na(qic_data$target)][1]
    # Placer ved sidste datapunkt
    last_x <- qic_data$x[nrow(qic_data)]
    label_data <- rbind(label_data, data.frame(
      x = last_x,
      y = target_value,
      label = paste0("Mål: ", format_y_value(target_value, y_axis_unit)),
      type = "target",
      stringsAsFactors = FALSE
    ))
  }

  # Fase tilfæjes - temporarily disabled ----
  # if ("part" %in% names(qic_data) && length(unique(qic_data$part)) > 1) {
  #   # Find phase change points
  #   phase_changes <- which(diff(as.numeric(qic_data$part)) != 0)
  #   # Add phase change lines using tidyverse approach
  #   if (length(phase_changes) > 0) {
  #     plot <- phase_changes |>
  #       purrr::reduce(function(p, change_point) {
  #         p +
  #           ggplot2::geom_vline(
  #             xintercept = qic_data$x[change_point + 1],
  #             color = hospital_colors$warning,
  #             linetype = "dotted", linewidth = 1, alpha = 0.7
  #           )
  #       }, .init = plot)
  #   }
  # }

  # Mållinje tilføjes ----
  # if ("target" %in% names(qic_data) && !all(is.na(qic_data$target))) {
  #   plot <- plot +
  #     ggplot2::geom_line(
  #       ggplot2::aes(y = target),
  #       color = hospital_colors$darkgrey,
  #       linetype = "42",
  #       linewidth = 1.2,
  #       alpha = 0.8
  #     )
  # }

  # Kommentarer tilføjes ----
  if (!is.null(comment_data) && is.data.frame(comment_data) && nrow(comment_data) > 0) {
    # Konverter til numerisk for ggpp kompatibilitet
    x_center_numeric <- mean(as.numeric(range(qic_data$x, na.rm = TRUE)))
    y_center <- mean(range(qic_data$y, na.rm = TRUE))

    # Tilføj numerisk x til comment_data
    comment_data$x_numeric <- as.numeric(comment_data$x)

    plot <- plot +
      ggrepel::geom_text_repel(
        data = comment_data,
        ggplot2::aes(x = x_numeric, y = y, label = comment),
        size = 6,
        color = hospital_colors$darkgrey,
        bg.color = "white",
        bg.r = 0.15,
        position = ggpp::position_nudge_center(
          x = x_center_numeric,
          y = y_center,
          direction = "radial"
        ),
        box.padding = 0.6,
        point.padding = 0.5,
        segment.color = hospital_colors$mediumgrey,
        segment.size = 0.4,
        segment.curvature = -0.05,
        segment.ncp = 3,
        segment.angle = 20,
        arrow = grid::arrow(length = grid::unit(0.015, "npc")),
        min.segment.length = 0,
        force = 3,
        force_pull = 0.5,
        max.overlaps = Inf,
        max.iter = 10000,
        inherit.aes = FALSE
      )
  }

  # CL og Target labels tilføjes ----
  if (!is.null(label_data) && nrow(label_data) > 0) {
    # Konverter til numerisk for ggpp kompatibilitet
    x_range <- range(qic_data$x, na.rm = TRUE)
    label_x_numeric <- as.numeric(x_range[2]) + as.numeric(diff(x_range)) * 0.02

    # Tilføj numerisk x til label_data
    label_data$x_numeric <- as.numeric(label_data$x)

    plot <- plot +
      ggrepel::geom_text_repel(
        data = label_data,
        ggplot2::aes(x = x_numeric, y = y, label = label),
        size = 4,
        color = hospital_colors$darkgrey,
        fontface = "bold",
        position = ggpp::position_nudge_to(x = label_x_numeric),
        direction = "y",
        hjust = 0,  # Venstrejuster tekst efter nudge
        segment.color = hospital_colors$mediumgrey,
        segment.size = 0.3,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        arrow = grid::arrow(length = grid::unit(0.01, "npc")),
        min.segment.length = 0,
        box.padding = 0.5,
        point.padding = 0.3,
        force = 2,
        max.overlaps = Inf,
        inherit.aes = FALSE
      )
  }

  return(plot)
}

generateSPCPlot <- function(data, config, chart_type, target_value = NULL, centerline_value = NULL, show_phases = FALSE, skift_column = NULL, frys_column = NULL, chart_title_reactive = NULL, y_axis_unit = "count", kommentar_column = NULL) {
  # Generate SPC plot with specified parameters
  # Get hospital colors using the proper package function
  hospital_colors <- get_hospital_colors()

  # PERFORMANCE MONITORING: Track QIC calculation calls
  if (!exists("qic_call_counter", envir = .GlobalEnv)) {
    assign("qic_call_counter", 0, envir = .GlobalEnv)
  }
  assign("qic_call_counter", get("qic_call_counter", envir = .GlobalEnv) + 1, envir = .GlobalEnv)

  current_time <- Sys.time()
  call_number <- get("qic_call_counter", envir = .GlobalEnv)

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

  log_debug(paste("generateSPCPlot CALL #", call_number, "at", format(current_time, "%H:%M:%S.%OS3"),
                  "| chart_type:", chart_type, "| data:", nrow(data), "rows"), .context = "SPC_CALC_DEBUG")

  # Input validation and configuration sanitization
  validate_spc_inputs(data, config)
  config <- sanitize_spc_config(config)

  # Process chart title
  title_text <- process_chart_title(chart_title_reactive, config)

  # Extract X-axis data
  x_data <- extract_x_axis_data(data, config$x_col)

  # Process data based on chart type
  if (chart_type_requires_denominator(chart_type) && !is.null(config$n_col) && config$n_col %in% names(data)) {
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

  # Note: Let qic handle ratio calculations directly from raw y and n data

  has_denominator <- !is.null(n_data)

  # Ensure we have minimum data points after filtering
  if (length(y_data) < 3) {
    stop(paste("For få gyldige datapunkter efter filtrering (", length(y_data), " fundet, minimum 3 påkrævet). Tilføj flere gyldige datapunkter."))
  }

  # Handle x-axis data med intelligent formatering - EFTER data filtrering
  # FASE 5: Performance optimization - cache expensive x-column validation
  # IMPROVED CACHE KEY: Include x-column content hash to invalidate cache when data content changes
  data_structure_hash <- paste0(nrow(data), "_", ncol(data), "_", paste(names(data), collapse = "_"))

  # ROBUST CACHE KEY: Safe ID generation to handle character(0) and NULL values
  safe_x_col_id <- if (is.null(config$x_col) || length(config$x_col) == 0 || identical(config$x_col, character(0)) || is.na(config$x_col)) {
    "NULL_XCOL"
  } else {
    # Sanitize column name for cache key (remove problematic characters)
    gsub("[^a-zA-Z0-9_]", "_", as.character(config$x_col)[1])
  }

  # CONTENT-AWARE CACHE KEY: Include hash of x-column + row count (used in validate_x_column_format fallback)
  x_content_hash <- safe_operation(
    "Generate x-column content hash for cache key",
    code = {
      if (!is.null(config$x_col) && config$x_col %in% names(data)) {
        x_column_data <- data[[config$x_col]]
        # Use fast hash of x-column content + row count to detect any changes affecting validation
        paste0(digest::digest(x_column_data, algo = "xxhash32"), "_", nrow(data))
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

  cache_key <- paste0("x_validation_", safe_x_col_id, "_", substr(data_structure_hash, 1, 12), "_", x_content_hash)

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
          qic_data <- execute_qic_call(qic_args, chart_type, config)

          # Tilføj kombineret anhoej.signal kolonne (runs ELLER crossings) per part
          if (!is.null(qic_data)) {
            # Brug runs.signal direkte fra qicharts2 (allerede per-række)
            runs_sig_col <- if ("runs.signal" %in% names(qic_data)) {
              qic_data$runs.signal
            } else {
              rep(FALSE, nrow(qic_data))
            }

            # Beregn crossings signal per part
            crossings_sig_col <- rep(FALSE, nrow(qic_data))
            if ("n.crossings" %in% names(qic_data) && "n.crossings.min" %in% names(qic_data) && "part" %in% names(qic_data)) {
              # For hver part, check om crossings signal er brudt
              for (p in unique(qic_data$part)) {
                part_rows <- which(qic_data$part == p)
                if (length(part_rows) > 0) {
                  part_data <- qic_data[part_rows, ]
                  n_cross <- max(part_data$n.crossings, na.rm = TRUE)
                  n_cross_min <- max(part_data$n.crossings.min, na.rm = TRUE)
                  has_crossing_signal <- !is.na(n_cross) && !is.na(n_cross_min) && n_cross < n_cross_min
                  crossings_sig_col[part_rows] <- has_crossing_signal
                }
              }
            }

            # Kombinér: TRUE hvis ENTEN runs ELLER crossings signal for den pågældende række
            qic_data$anhoej.signal <- runs_sig_col | crossings_sig_col
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
          plot <- ggplot2::ggplot(qic_data, ggplot2::aes(x = x, y = y)) 
            
          # Kontrolgrænser tilføjes conditionally ----
          if (!is.null(qic_data$ucl) && !all(is.na(qic_data$ucl)) && !is.null(qic_data$lcl) && !all(is.na(qic_data$lcl))) {
            plot <- plot +
              ggplot2::geom_ribbon(ggplot2::aes(ymin = lcl, ymax = ucl), fill = "#E6F5FD", alpha = 0.5) +
              geomtextpath::geom_textline(ggplot2::aes(y = ucl, x = x, label = "Øvre kontrolgrænse"), inherit.aes = FALSE, hjust = 0.05, vjust = -0.2, linewidth = 2.5, linecolour = NA, textcolour = "#b5b5b9", na.rm = TRUE) +
              geomtextpath::geom_textline(ggplot2::aes(y = lcl, x = x, label = "Nedre kontrolgrænse"), inherit.aes = FALSE, hjust = 0.05, vjust = 1.2, linewidth = 2.5, linecolour = NA, textcolour = "#b5b7b9", na.rm = TRUE) 
          }
          # Resten af plot tilføjes ------
          plot <- plot +  
            ggplot2::geom_line(ggplot2::aes(y = target, x = x), inherit.aes = FALSE, linewidth = 1, colour = "#565656", linetype="42", na.rm = TRUE) +
            ggplot2::geom_line(ggplot2::aes(y = y, group = part), colour = "#AEAEAE", linewidth = 1, na.rm = TRUE) +
            ggplot2::geom_point(ggplot2::aes(y = y, group = part), colour = "#858585", size = 2, na.rm = TRUE) +
            
            # ggplot2::geom_line(color = hospital_colors$lightgrey, linewidth = 1) +
            # ggplot2::geom_point(size = 2, color = hospital_colors$mediumgrey) +
            ggplot2::geom_line(ggplot2::aes(y = cl, group = part, linetype = anhoej.signal), color = hospital_colors$hospitalblue, linewidth = 1) + 
            
            
            ggplot2::labs(title = call_args$title, x = NULL, y = NULL) +
            ggplot2::scale_linetype_manual(
              values = c("FALSE" = "solid", "TRUE" = "12"),
              guide = "none"  # Skjul legend
            ) 
            

          plot
        },
        fallback = function(e) {
          log_error(paste("ERROR in ggplot build:", e$message), "GGPLOT_BUILD")
          stop(e)
        },
        error_type = "processing"
      )



      # Intelligent x-akse formatering baseret på dato-mønstre ----
      if (!is.null(x_validation$x.format) && x_validation$is_date) {
        # DEBUG: Tjek qic_data$x type

        # Intelligent interval detektion og formatering
        interval_info <- detect_date_interval(qic_data$x, debug = TRUE)
        format_config <- get_optimal_formatting(interval_info, debug = TRUE)

        # qic() konverterer Date objekter til POSIXct, så brug scale_x_datetime
        if (inherits(qic_data$x, c("POSIXct", "POSIXt"))) {
          # Håndter intelligent formatering separat
          if (interval_info$type == "weekly" && !is.null(format_config$use_smart_labels) && format_config$use_smart_labels) {
            log_debug("SMART WEEKLY LABELS: Applying intelligent week formatting", .context = "X_AXIS_FORMAT")
            plot <- plot + ggplot2::scale_x_datetime(
              # name = x_unit_label,
              labels = format_config$labels, # Smart scales::label_date_short()
              # breaks = scales::date_breaks(format_config$breaks)
              breaks = scales::breaks_pretty(n = format_config$n_breaks)
            )
          } else if (interval_info$type == "monthly" && !is.null(format_config$use_smart_labels) && format_config$use_smart_labels) {
            log_debug("SMART MONTHLY LABELS: Applying intelligent month formatting", .context = "X_AXIS_FORMAT")
            plot <- plot + ggplot2::scale_x_datetime(
              # name = x_unit_label,
              labels = format_config$labels, # Smart scales::label_date_short()
              breaks = scales::date_breaks(format_config$breaks)
            )
          } else if (!is.null(format_config$breaks)) {
            # Standard intelligent formatering
            plot <- plot + ggplot2::scale_x_datetime(
              # name = x_unit_label,
              labels = scales::date_format(format_config$labels),
              breaks = scales::date_breaks(format_config$breaks)
            )
          } else {
            # Fallback til breaks_pretty med intelligent antal
            plot <- plot + ggplot2::scale_x_datetime(
              # name = x_unit_label,
              labels = scales::date_format(format_config$labels),
              breaks = scales::breaks_pretty(n = format_config$n_breaks)
            )
          }
        } else if (inherits(qic_data$x, "Date")) {
          # Date objekter - tilsvarende intelligent håndtering
          if (interval_info$type == "weekly" && !is.null(format_config$use_smart_labels) && format_config$use_smart_labels) {
            log_debug("SMART WEEKLY LABELS: Applying intelligent week formatting for Date objects", .context = "X_AXIS_FORMAT")
            plot <- plot + ggplot2::scale_x_date(
              # name = x_unit_label,
              labels = format_config$labels, # Smart scales::label_date_short()
              breaks = scales::date_breaks(format_config$breaks)
            )
          } else if (interval_info$type == "monthly" && !is.null(format_config$use_smart_labels) && format_config$use_smart_labels) {
            log_debug("SMART MONTHLY LABELS: Applying intelligent month formatting for Date objects", .context = "X_AXIS_FORMAT")
            plot <- plot + ggplot2::scale_x_date(
              # name = x_unit_label,
              labels = format_config$labels, # Smart scales::label_date_short()
              breaks = scales::date_breaks(format_config$breaks)
            )
          } else if (!is.null(format_config$breaks)) {
            plot <- plot + ggplot2::scale_x_date(
              # name = x_unit_label,
              labels = scales::date_format(format_config$labels),
              breaks = scales::date_breaks(format_config$breaks)
            )
          } else {
            plot <- plot + ggplot2::scale_x_date(
              # name = x_unit_label,
              labels = scales::date_format(format_config$labels),
              breaks = scales::breaks_pretty(n = format_config$n_breaks)
            )
          }
        } else if (is.numeric(qic_data$x)) {
          # Fallback til continuous scale
          plot <- plot + ggplot2::scale_x_continuous(
            # name = x_unit_label,
            breaks = scales::pretty_breaks(n = 8)
          )
        }
      }

      # Y-axis formatting based on unit type -----
      if (y_axis_unit == "percent") {
        # Percent formatting with % suffix
        # Data from qic is in decimal form (0.9 for 90%), scale = 100 converts to percentage
        # Danish formatting: decimal.mark = "," (85,5 %), big.mark = "." (not used for %)
        plot <- plot + ggplot2::scale_y_continuous(
          expand = ggplot2::expansion(mult = c(.25, .25)),
          labels = scales::label_percent()
        )
      } else if (y_axis_unit == "count") {
        # Count formatting with intelligent K/M notation
        # K starts at 1.000+ for correct notation (K = 1.000, not 10.000)
        # Trade-off: loses thousand separator for 1.000-9.999 range
        # Only shows decimals if present (50K vs 50,5K)
        plot <- plot + ggplot2::scale_y_continuous(
          expand = ggplot2::expansion(mult = c(.25, .25)),
          labels = function(x) {
            # Apply scale cuts manually using sapply for vectorization
            sapply(x, function(val) {
              # Handle NA values
              if (is.na(val)) return(NA)

              if (abs(val) >= 1e9) {
                scaled <- val / 1e9
                if (scaled == round(scaled)) {
                  paste0(round(scaled), " mia.")
                } else {
                  paste0(format(scaled, decimal.mark = ",", nsmall = 1), " mia.")
                }
              } else if (abs(val) >= 1e6) {
                scaled <- val / 1e6
                if (scaled == round(scaled)) {
                  paste0(round(scaled), "M")
                } else {
                  paste0(format(scaled, decimal.mark = ",", nsmall = 1), "M")
                }
              } else if (abs(val) >= 1e3) {
                scaled <- val / 1e3
                if (scaled == round(scaled)) {
                  paste0(round(scaled), "K")
                } else {
                  paste0(format(scaled, decimal.mark = ",", nsmall = 1), "K")
                }
              } else {
                # No scaling - just format with thousand separator if needed
                if (val == round(val)) {
                  format(round(val), big.mark = ".", decimal.mark = ",")
                } else {
                  format(val, big.mark = ".", decimal.mark = ",", nsmall = 1)
                }
              }
            })
          }
        )
      } else if (y_axis_unit == "rate") {
        # Rate formatting (only shows decimals if present) ----
        plot <- plot + ggplot2::scale_y_continuous(
          expand = ggplot2::expansion(mult = c(.25, .25)),
          labels = function(x) {
            ifelse(x == round(x),
                   format(round(x), decimal.mark = ","),
                   format(x, decimal.mark = ",", nsmall = 1))
          }
        )
      } else if (y_axis_unit == "time") {
        # Intelligent time formatting based on data range (input: minutes) -----
        # Only shows decimals if present
        y_range <- range(qic_data$y, na.rm = TRUE)
        max_minutes <- max(y_range, na.rm = TRUE)

        if (max_minutes < 60) {
          # Less than 60 minutes -> show as minutes
          plot <- plot + ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(.25, .25)),
            labels = function(x) {
              sapply(x, function(val) {
                # Handle NA values
                if (is.na(val)) return(NA)

                if (val == round(val)) {
                  paste0(round(val), " min")
                } else {
                  paste0(format(val, decimal.mark = ",", nsmall = 1), " min")
                }
              })
            }
          )
        } else if (max_minutes < 1440) {
          # Less than 24 hours (1440 min) -> show as hours
          plot <- plot + ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(.25, .25)),
            labels = function(x) {
              sapply(x, function(val) {
                # Handle NA values
                if (is.na(val)) return(NA)

                scaled <- val / 60
                if (scaled == round(scaled)) {
                  paste0(round(scaled), " timer")
                } else {
                  paste0(format(scaled, decimal.mark = ",", nsmall = 1), " timer")
                }
              })
            }
          )
        } else {
          # 24 hours or more -> show as days
          plot <- plot + ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(.25, .25)),
            labels = function(x) {
              sapply(x, function(val) {
                # Handle NA values
                if (is.na(val)) return(NA)

                scaled <- val / 1440
                if (scaled == round(scaled)) {
                  paste0(round(scaled), " dage")
                } else {
                  paste0(format(val, decimal.mark = ",", nsmall = 1), " dage")
                }
              })
            }
          )
        }
      }
      # For other units - use default ggplot2 formatting

      # Add plot enhancements (phase lines, target line, comments)
      plot <- add_plot_enhancements(plot, qic_data, comment_data, y_axis_unit)

      return(list(plot = plot, qic_data = qic_data))
    }
  ))
}

# PLOT STYLING ===============================================================

## Hospital Tema til Plots
# Anvender hospital branding og farvepalette på SPC plots
applyHospitalTheme <- function(plot) {
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
        
        # ggplot2::theme_minimal()
        ggplot2::theme(
          plot.margin = ggplot2::unit(c(0, 0, 0, 10), "pt"),
          panel.background = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(color = "#858585", size = 16, angle = 0, hjust = 1),
          axis.text.x = ggplot2::element_text(color = "#858585", angle = 0, size = 11),
          axis.line.x = ggplot2::element_line(color = "#D6D6D6"),
          axis.ticks.x = ggplot2::element_line(color = "#D6D6D6"),
          axis.ticks.y = ggplot2::element_line(color = "#D6D6D6"),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          legend.position = "none",
        ) + lemon::coord_capped_cart(bottom='right', gap = 0)
        
        
        # ggplot2::theme_minimal() +
        # ggplot2::theme(
        #   plot.title = ggplot2::element_text(color = hospital_colors$primary, size = 14, face = "bold"),
        #   plot.subtitle = ggplot2::element_text(color = hospital_colors$secondary, size = 12),
        #   axis.title = ggplot2::element_text(color = hospital_colors$dark, size = 11),
        #   axis.text = ggplot2::element_text(color = hospital_colors$dark, size = 10),
        #   legend.title = ggplot2::element_text(color = hospital_colors$dark, size = 11),
        #   legend.text = ggplot2::element_text(color = hospital_colors$dark, size = 10),
        #   panel.grid.major = ggplot2::element_line(color = hospital_colors$light),
        #   panel.grid.minor = ggplot2::element_line(color = hospital_colors$light),
        #   strip.text = ggplot2::element_text(color = hospital_colors$primary, face = "bold")
        # ) +
        # ggplot2::labs(caption = footer_text) +
        # ggplot2::theme(
        #   plot.caption = ggplot2::element_text(size = 8, color = hospital_colors$secondary, hjust = 0)
        # )

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
