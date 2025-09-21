# fct_spc_plot_generation.R
# Main SPC plot generation logic using qicharts2
# Extracted from fct_spc_calculations.R for better maintainability
#
# Dependencies ----------------------------------------------------------------

generateSPCPlot <- function(data, config, chart_type, target_value = NULL, centerline_value = NULL, show_phases = FALSE, skift_column = NULL, frys_column = NULL, chart_title_reactive = NULL, y_axis_unit = "count", kommentar_column = NULL) {
  # DEBUG: Comprehensive input parameter logging
  log_debug("======================================", "SPC_CALC_DEBUG")
  log_debug("generateSPCPlot function called", "SPC_CALC_DEBUG")
  log_debug(paste("chart_type:", chart_type), "SPC_CALC_DEBUG")
  log_debug(paste("target_value:", if(is.null(target_value)) "NULL" else target_value), "SPC_CALC_DEBUG")
  log_debug(paste("show_phases:", show_phases), "SPC_CALC_DEBUG")
  log_debug(paste("skift_column:", if(is.null(skift_column)) "NULL" else paste("'", skift_column, "'", sep="")), "SPC_CALC_DEBUG")
  log_debug(paste("frys_column:", if(is.null(frys_column)) "NULL" else paste("'", frys_column, "'", sep="")), "SPC_CALC_DEBUG")
  log_debug(paste("y_axis_unit:", if(is.null(y_axis_unit)) "NULL" else paste("'", y_axis_unit, "'", sep="")), "SPC_CALC_DEBUG")
  log_debug(paste("kommentar_column:", if(is.null(kommentar_column)) "NULL" else paste("'", kommentar_column, "'", sep="")), "SPC_CALC_DEBUG")
  log_debug(paste("Data dimensions:", nrow(data), "x", ncol(data)), "SPC_CALC_DEBUG")
  log_debug(paste("Column names:", paste(names(data), collapse=", ")), "SPC_CALC_DEBUG")
  log_debug("======================================", "SPC_CALC_DEBUG")

  # Input validation
  validate_spc_inputs(data, config)

  # Get title with detailed debugging
  title_text <- process_chart_title(chart_title_reactive, config)

  # Prepare data with defensive checking
  log_debug("Preparing data variables...", "SPC_CALC_DEBUG")
  log_debug(paste("config$x_col:", if(is.null(config$x_col)) "NULL" else paste("'", config$x_col, "'", sep="")), "SPC_CALC_DEBUG")
  log_debug(paste("config$y_col:", if(is.null(config$y_col)) "NULL" else paste("'", config$y_col, "'", sep="")), "SPC_CALC_DEBUG")
  log_debug(paste("config$n_col:", if(is.null(config$n_col)) "NULL" else paste("'", config$n_col, "'", sep="")), "SPC_CALC_DEBUG")

  # DEFENSIVE: Check for character(0) in config values
  if (!is.null(config$x_col) && (length(config$x_col) == 0 || identical(config$x_col, character(0)))) {
    log_debug("⚠️ config$x_col is character(0) - setting to NULL", "SPC_CALC_DEBUG")
    config$x_col <- NULL
  }
  if (!is.null(config$y_col) && (length(config$y_col) == 0 || identical(config$y_col, character(0)))) {
    log_debug("⚠️ config$y_col is character(0) - this will cause errors", "SPC_CALC_DEBUG")
    stop("Y-kolonne kan ikke være character(0)")
  }
  if (!is.null(config$n_col) && (length(config$n_col) == 0 || identical(config$n_col, character(0)))) {
    log_debug("⚠️ config$n_col is character(0) - setting to NULL", "SPC_CALC_DEBUG")
    config$n_col <- NULL
  }

  x_data <- if (!is.null(config$x_col) && config$x_col %in% names(data)) data[[config$x_col]] else NULL
  log_debug(paste("x_data extracted:", if(is.null(x_data)) "NULL" else paste("length=", length(x_data))), "SPC_CALC_DEBUG")

  y_data_raw <- data[[config$y_col]]
  log_debug(paste("y_data_raw extracted:", if(is.null(y_data_raw)) "NULL" else paste("length=", length(y_data_raw))), "SPC_CALC_DEBUG")

  # Handle different chart types
  log_debug("Checking chart type routing...", "SPC_CALC_DEBUG")
  log_debug(paste("n_col check - config$n_col:", if(is.null(config$n_col)) "NULL" else paste("'", config$n_col, "'", sep="")), "SPC_CALC_DEBUG")
  log_debug(paste("n_col exists in data:", if(is.null(config$n_col)) "N/A" else (config$n_col %in% names(data))), "SPC_CALC_DEBUG")

  if (!is.null(config$n_col) && config$n_col %in% names(data)) {
    log_debug("✓ Using numerator/denominator chart type (ratio calculations)", "SPC_CALC_DEBUG")
    # Charts with numerator/denominator
    taeller_raw <- y_data_raw
    naevner_raw <- data[[config$n_col]]

    # Filter out rows with missing data BEFORE conversion
    complete_rows <- !is.na(taeller_raw) & !is.na(naevner_raw) &
      trimws(as.character(taeller_raw)) != "" &
      trimws(as.character(naevner_raw)) != ""

    if (!any(complete_rows)) {
      stop("Ingen komplette datarækker fundet. Tjek at både tæller og nævner kolonner har gyldige værdier.")
    }

    # Filter data to complete rows only
    data_filtered <- data[complete_rows, , drop = FALSE]

    # PRESERVE POSIXct/Date formats: If x_col was POSIXct/Date, ensure it remains so after filtering
    if (!is.null(config$x_col) && config$x_col %in% names(data)) {
      original_x_class <- class(data[[config$x_col]])
      filtered_x_class <- class(data_filtered[[config$x_col]])

      log_debug(paste("DATA FILTERING DEBUG:\n- Original", config$x_col, "class:", original_x_class[1], "\n- Filtered", config$x_col, "class:", filtered_x_class[1]), "DATA_FILTER")

      # If original was POSIXct/Date but filtered lost the class, restore it
      if (inherits(data[[config$x_col]], c("POSIXct", "POSIXt", "Date")) &&
        !inherits(data_filtered[[config$x_col]], c("POSIXct", "POSIXt", "Date"))) {
        log_debug(paste("- RESTORING: Filtered data lost", original_x_class[1], "format, restoring..."), "DATA_FILTER")

        # Restore the original class attributes
        data_filtered[[config$x_col]] <- data[[config$x_col]][complete_rows]
        class(data_filtered[[config$x_col]]) <- original_x_class
        attributes(data_filtered[[config$x_col]]) <- attributes(data[[config$x_col]])

        log_debug(paste("- ✓ RESTORED:", config$x_col, "to", class(data_filtered[[config$x_col]])[1]), "DATA_FILTER")
      }

      log_debug(paste("- Original sample:", paste(head(data[[config$x_col]], 3), collapse = ", "), "\n- Filtered sample:", paste(head(data_filtered[[config$x_col]], 3), collapse = ", ")), "DATA_FILTER")
    }

    taeller <- parse_danish_number(data_filtered[[config$y_col]])
    naevner <- parse_danish_number(data_filtered[[config$n_col]])

    # Check conversion success
    if (any(is.na(taeller)) || any(is.na(naevner))) {
      invalid_taeller <- sum(is.na(taeller))
      invalid_naevner <- sum(is.na(naevner))
      stop(paste(
        "Kunne ikke konvertere numeriske værdier:",
        if (invalid_taeller > 0) paste(invalid_taeller, "ugyldige tæller værdier"),
        if (invalid_naevner > 0) paste(invalid_naevner, "ugyldige nævner værdier")
      ))
    }

    # Check for zero denominators
    if (any(naevner == 0)) {
      stop("Nævner kan ikke være nul (division by zero)")
    }

    # Update data reference to filtered data
    data <- data_filtered

    # Get unit labels early - before they are used
    log_debug("Processing unit labels...", "SPC_CALC_DEBUG")
    log_debug(paste("y_axis_unit received:", if(is.null(y_axis_unit)) "NULL" else paste("'", y_axis_unit, "' (length:", length(y_axis_unit), ")", sep="")), "SPC_CALC_DEBUG")

    x_unit_label <- ""
    # DEFENSIVE: Check for character(0) before calling get_unit_label
    if (length(y_axis_unit) == 0 || identical(y_axis_unit, character(0))) {
      log_debug("⚠️ y_axis_unit is character(0) - using default 'count'", "SPC_CALC_DEBUG")
      y_unit_label <- get_unit_label("count", Y_AXIS_UNITS_DA)
    } else {
      y_unit_label <- get_unit_label(y_axis_unit, Y_AXIS_UNITS_DA)
    }
    log_debug(paste("y_unit_label result:", if(is.null(y_unit_label)) "NULL" else paste("'", y_unit_label, "'", sep="")), "SPC_CALC_DEBUG")

    log_debug("Building y_data and ylab_text...", "SPC_CALC_DEBUG")
    log_debug(paste("chart_type:", chart_type), "SPC_CALC_DEBUG")
    log_debug(paste("taeller length:", length(taeller)), "SPC_CALC_DEBUG")
    log_debug(paste("naevner length:", length(naevner)), "SPC_CALC_DEBUG")

    # DEFENSIVE: Check config values before using in paste
    y_col_safe <- if (is.null(config$y_col) || length(config$y_col) == 0 || identical(config$y_col, character(0))) "Y" else config$y_col
    n_col_safe <- if (is.null(config$n_col) || length(config$n_col) == 0 || identical(config$n_col, character(0))) "N" else config$n_col
    log_debug(paste("y_col_safe:", y_col_safe), "SPC_CALC_DEBUG")
    log_debug(paste("n_col_safe:", n_col_safe), "SPC_CALC_DEBUG")

    if (chart_type == "run") {
      log_debug("Processing run chart type...", "SPC_CALC_DEBUG")
      y_data <- (taeller / naevner) * 100
      log_debug("y_data calculated successfully", "SPC_CALC_DEBUG")
      ylab_text <- if (y_unit_label != "") y_unit_label else paste("Rate (", y_col_safe, "/", n_col_safe, ") %")
      log_debug("ylab_text created successfully", "SPC_CALC_DEBUG")
    } else if (chart_type %in% c("p", "pp", "u", "up")) {
      log_debug("Processing p/u chart type...", "SPC_CALC_DEBUG")
      y_data <- taeller
      n_data <- naevner
      ylab_text <- if (y_unit_label != "") y_unit_label else (if (chart_type %in% c("p", "pp")) "Proportion" else "Rate")
      log_debug("y_data and ylab_text created successfully", "SPC_CALC_DEBUG")
    } else {
      log_debug("Processing other chart type...", "SPC_CALC_DEBUG")
      y_data <- (taeller / naevner) * 100
      log_debug("y_data calculated successfully", "SPC_CALC_DEBUG")
      ylab_text <- if (y_unit_label != "") y_unit_label else paste("Rate (", y_col_safe, "/", n_col_safe, ") %")
      log_debug("ylab_text created successfully", "SPC_CALC_DEBUG")
    }
    log_debug("✅ y_data and ylab_text processing completed", "SPC_CALC_DEBUG")
  } else {
    # Get unit labels early - before they are used
    log_debug("Processing unit labels...", "SPC_CALC_DEBUG")
    log_debug(paste("y_axis_unit received:", if(is.null(y_axis_unit)) "NULL" else paste("'", y_axis_unit, "' (length:", length(y_axis_unit), ")", sep="")), "SPC_CALC_DEBUG")

    x_unit_label <- ""
    # DEFENSIVE: Check for character(0) before calling get_unit_label
    if (length(y_axis_unit) == 0 || identical(y_axis_unit, character(0))) {
      log_debug("⚠️ y_axis_unit is character(0) - using default 'count'", "SPC_CALC_DEBUG")
      y_unit_label <- get_unit_label("count", Y_AXIS_UNITS_DA)
    } else {
      y_unit_label <- get_unit_label(y_axis_unit, Y_AXIS_UNITS_DA)
    }
    log_debug(paste("y_unit_label result:", if(is.null(y_unit_label)) "NULL" else paste("'", y_unit_label, "'", sep="")), "SPC_CALC_DEBUG")

    # Standard numeric data - filter out missing values first
    complete_rows <- !is.na(y_data_raw) & trimws(as.character(y_data_raw)) != ""

    if (!any(complete_rows)) {
      stop(paste("Ingen gyldige værdier fundet i", config$y_col, "kolonnen. Tjek at kolonne indeholder numeriske værdier."))
    }

    # Filter data to complete rows only
    # PRESERVE POSIXct/Date formats in non-ratio case too
    if (!is.null(config$x_col) && config$x_col %in% names(data)) {
      original_x_class <- class(data[[config$x_col]])
      log_debug(paste("DATA FILTERING DEBUG (non-ratio):\n- Original", config$x_col, "class:", original_x_class[1]), "DATA_FILTER")
    }

    data_backup <- data # Keep reference to original data
    data <- data[complete_rows, , drop = FALSE]

    # Restore POSIXct/Date format if lost during filtering
    if (!is.null(config$x_col) && config$x_col %in% names(data)) {
      filtered_x_class <- class(data[[config$x_col]])
      log_debug(paste("- Filtered", config$x_col, "class:", filtered_x_class[1]), "DATA_FILTER")

      if (inherits(data_backup[[config$x_col]], c("POSIXct", "POSIXt", "Date")) &&
        !inherits(data[[config$x_col]], c("POSIXct", "POSIXt", "Date"))) {
        log_debug(paste("- RESTORING (non-ratio): Lost", original_x_class[1], "format, restoring..."), "DATA_FILTER")

        # Restore the original class and attributes
        data[[config$x_col]] <- data_backup[[config$x_col]][complete_rows]
        class(data[[config$x_col]]) <- original_x_class
        attributes(data[[config$x_col]]) <- attributes(data_backup[[config$x_col]])

        log_debug(paste("- ✓ RESTORED (non-ratio):", config$x_col, "to", class(data[[config$x_col]])[1]), "DATA_FILTER")
      }
    }

    y_data <- parse_danish_number(data[[config$y_col]])
    ylab_text <- if (y_unit_label != "") y_unit_label else config$y_col

    # Check conversion success
    if (all(is.na(y_data))) {
      stop(paste("Kunne ikke konvertere", config$y_col, "til numeriske værdier. Tjek at værdier er gyldige tal."))
    }
  }

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
  log_debug(paste("Safe X column ID for cache:", safe_x_col_id), "SPC_CALC_DEBUG")

  cache_key <- paste0("x_validation_", safe_x_col_id, "_", substr(data_hash, 1, 20))

  x_validation <- create_cached_reactive({
    validate_x_column_format(data, config$x_col, "observation")
  }, cache_key, cache_timeout = PERFORMANCE_THRESHOLDS$cache_timeout_default)()
  x_data <- x_validation$x_data
  # xlab_text <- if (x_unit_label != "") x_unit_label else {
  #   if (x_validation$is_date) "Dato" else "Observation"
  # }

  # DEBUG: Log x-validation results
  log_debug(paste("=== X-VALIDATION DEBUG ===\nx_col:", config$x_col, "\nx_data class:", class(x_data)[1], "\nx_data sample:", paste(head(x_data, 3), collapse = ", "), "\nis_date:", x_validation$is_date, "\nx.period:", x_validation$x.period, "\nx.format:", x_validation$x.format, "\n========================"), "X_VALIDATION")

  # Handle phases and freeze configuration
  phase_freeze_config <- process_phase_freeze_config(data, show_phases, skift_column, frys_column)
  part_positions <- phase_freeze_config$part_positions
  freeze_position <- phase_freeze_config$freeze_position

  # Build qic call arguments dynamically
  call_args <- list(
    x = x_data,
    y = y_data,
    chart = chart_type,
    title = title_text,
    ylab = ylab_text
  )

  # NOTE: x.period og x.format parametre bruges ikke længere da vi anvender return.data=TRUE

  # Add n for P/U charts
  if (chart_type %in% c("p", "pp", "u", "up") && exists("n_data")) {
    call_args$n <- n_data
  }

  # Add freeze for baseline - can be used together with part
  if (!is.null(freeze_position)) {
    call_args$freeze <- freeze_position
  }

  # Add part for phase splits - can be used together with freeze
  if (!is.null(part_positions)) {
    call_args$part <- part_positions
  }

  # Add target line if provided
  if (!is.null(target_value) && is.numeric(target_value) && !is.na(target_value)) {
    call_args$target <- target_value
  }

  # CRITICAL: Add return.data = TRUE to get underlying data frame instead of plot
  call_args$return.data <- TRUE

  # Clean data
  complete_cases <- complete.cases(call_args$x, call_args$y)
  if (!all(complete_cases)) {
    call_args$x <- call_args$x[complete_cases]
    call_args$y <- call_args$y[complete_cases]

    if ("n" %in% names(call_args)) {
      call_args$n <- call_args$n[complete_cases]
    }

    # Handle part positions: adjust for removed rows
    if ("part" %in% names(call_args)) {
      removed_positions <- which(!complete_cases)

      if (length(removed_positions) > 0) {
        # Adjust part positions by subtracting removed rows before each position
        adjusted_part <- call_args$part

        for (i in seq_along(adjusted_part)) {
          pos <- adjusted_part[i]
          removed_before <- sum(removed_positions < pos)
          adjusted_part[i] <- pos - removed_before
        }

        # Remove invalid positions
        valid_parts <- adjusted_part > 0 & adjusted_part <= sum(complete_cases)
        call_args$part <- adjusted_part[valid_parts]

        if (length(call_args$part) == 0) {
          call_args$part <- NULL
        }
      }
    }
  }

  if (length(call_args$y) < 3) {
    stop("For få datapunkter efter rensning (minimum 3 påkrævet)")
  }

  # Generate SPC data using qicharts2

  # Generate SPC data using qicharts2 and build custom ggplot
  safe_operation(
    "Generate SPC plot data",
    code = {
      # Use data parameter approach like the working example
      # qicharts2::qic(x = Dato, y = `Tæller`, n = `Nævner`, part = c(12), data = data, return.data = TRUE)

      # Get column names for qic() call - respecting auto-detection results
      x_col_name <- config$x_col # Auto-detected date column or NULL
      y_col_name <- config$y_col # Should be "Tæller"
      n_col_name <- config$n_col # Should be "Nævner"

      # Brug data fra x_validation i stedet for duplikeret logik
      log_debug(paste("UPDATE CONDITION DEBUG:\n- x_col_name is not NULL:", !is.null(x_col_name), "\n- x_col_name in names(data):", if (!is.null(x_col_name)) x_col_name %in% names(data) else "N/A", "\n- x_validation$is_date:", x_validation$is_date, if (!is.null(x_col_name) && x_col_name %in% names(data)) paste("\n- data[[x_col_name]] is character:", is.character(data[[x_col_name]])) else ""), "DATA_PROCESS")

      # UPDATED CONDITION: Accept both date columns AND character columns (like "Uge tekst")
      if (!is.null(x_col_name) && x_col_name %in% names(data) &&
        (x_validation$is_date || is.character(data[[x_col_name]]))) {
        # Debug logging før opdatering
        log_debug(paste("DATAFRAME UPDATE DEBUG:\n- data nrows:", nrow(data), "\n- Original", x_col_name, "class:", class(data[[x_col_name]])[1]), "DATA_PROCESS")

        if (x_validation$is_date) {
          # DATE COLUMN: Use processed data from x_validation
          log_debug(paste("- x_validation$x_data length:", length(x_validation$x_data), "\n- x_validation$x_data class:", class(x_validation$x_data)[1]), "DATA_PROCESS")

          # Opdater kolonnen med de processerede data fra x_validation
          if (length(x_validation$x_data) == nrow(data)) {
            data[[x_col_name]] <- x_validation$x_data
            x_col_for_qic <- x_col_name

            log_debug(paste("✓ SUCCESS: Opdaterede", x_col_name, "til", class(x_validation$x_data)[1], "\nQICDATA DEBUG: Bruger dato-kolonne", x_col_name, "med", length(x_validation$x_data), "datoer"), "DATA_PROCESS")
          } else {
            log_debug(paste("✗ ERROR: Længde mismatch - x_validation$x_data:", length(x_validation$x_data), "vs data rows:", nrow(data), "\nQICDATA DEBUG: Bruger obs_sequence som fallback"), "DATA_PROCESS")
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

          log_debug(paste("✓ SUCCESS: Konverterede tekstkolonne", x_col_name, "til factor med original rækkefølge\nQICDATA DEBUG: Bruger tekst-kolonne", x_col_name, "med", length(data[[x_col_name]]), "tekstlabels som factor\n- Sample labels:", paste(head(unique_labels, 3), collapse = ", "), "\n- Levels i korrekt rækkefølge:", paste(head(unique_labels, 5), collapse = ", ")), "DATA_PROCESS")
        }
      } else {
        # Brug observation sekvens som fallback
        if (!("obs_sequence" %in% names(data))) {
          data$obs_sequence <- 1:nrow(data)
        }
        x_col_for_qic <- "obs_sequence"

        log_debug("QICDATA DEBUG: Bruger obs_sequence som fallback", "DATA_PROCESS")
      }

      # Note: obs_sequence fjernes IKKE fra data da det måske bruges af andre komponenter

      # Generate SPC data using qicharts2
      qic_data <- safe_operation(
        "Generate SPC data using qicharts2",
        code = {
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

          # Call qic() with prepared arguments
          log_debug("=== QIC CALL DEBUG ===\nCalling qic()...", "QIC_CALL")
          if (getOption("debug.mode", FALSE)) {
            log_debug("qic_args structure:", "QIC_CALL")
            log_debug(qic_args, "QIC_CALL")
          }

          log_debug(qic_args, "QIC")

          qic_data <- do.call(qicharts2::qic, qic_args)
          log_debug(paste("QIC call successful, returned data dimensions:", nrow(qic_data), "x", ncol(qic_data)), "QIC_CALL")

          # DEBUG: Show complete qic_data structure and content
          log_debug_block("QIC_DATA", "qic_data structure")
          str(qic_data)
          log_debug("qic_data column names:", paste(names(qic_data), collapse = ", "), .context = "QIC_DATA")
          log_debug("qic_data head (first 10 rows):", .context = "QIC_DATA")
          print(head(qic_data, 10))
          log_debug_block("QIC_DATA", "qic_data structure completed", type = "stop")

          log_debug(qic_data, "QIC")

          # Convert proportions to percentages for run charts with rate data
          if (chart_type == "run" && !is.null(config$n_col) && config$n_col %in% names(data)) {
            qic_data$y <- qic_data$y * 100
            qic_data$cl <- qic_data$cl * 100

            if (!is.null(qic_data$ucl) && !all(is.na(qic_data$ucl))) {
              qic_data$ucl <- qic_data$ucl * 100
            }
            if (!is.null(qic_data$lcl) && !all(is.na(qic_data$lcl))) {
              qic_data$lcl <- qic_data$lcl * 100
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
      comment_data <- NULL
      if (!is.null(kommentar_column) && kommentar_column %in% names(data)) {
        # Extract comments and sync with qic_data
        comments_raw <- data[[kommentar_column]]

        # Create comment data frame aligned with qic_data
        comment_data <- data.frame(
          x = qic_data$x,
          y = qic_data$y,
          comment = comments_raw[1:nrow(qic_data)], # Ensure same length as qic_data
          stringsAsFactors = FALSE
        )

        # Filter to only non-empty comments
        comment_data <- comment_data[
          !is.na(comment_data$comment) &
            trimws(comment_data$comment) != "",
        ]

        # Truncate very long comments
        if (nrow(comment_data) > 0) {
          comment_data$comment <- ifelse(
            nchar(comment_data$comment) > 40,
            paste0(substr(comment_data$comment, 1, 37), "..."),
            comment_data$comment
          )
        }
      }

      # Build custom ggplot using qic calculations
      log_debug(paste("=== GGPLOT BUILD DEBUG ===\nqic_data dimensions:", nrow(qic_data), "x", ncol(qic_data), "\nqic_data columns:", paste(names(qic_data), collapse = ", ")), "GGPLOT_BUILD")

      plot <- safe_operation(
        "Build custom ggplot",
        code = {
          plot <- ggplot2::ggplot(qic_data, ggplot2::aes(x = x, y = y))
          log_debug("Base plot created successfully", "GGPLOT_BUILD")

          plot <- plot + ggplot2::geom_line(color = HOSPITAL_COLORS$lightgrey, linewidth = 1)
          log_debug("Line geom added successfully", "GGPLOT_BUILD")

          plot <- plot + ggplot2::geom_point(size = 2, color = HOSPITAL_COLORS$mediumgrey)
          log_debug("Point geom added successfully", "GGPLOT_BUILD")

          plot <- plot + ggplot2::geom_line(ggplot2::aes(y = cl),
            color = HOSPITAL_COLORS$hospitalblue,
            linetype = "solid", linewidth = 1
          )
          log_debug("Center line added successfully", "GGPLOT_BUILD")

          plot <- plot + ggplot2::labs(title = call_args$title, x = "", y = "")
          log_debug("Labels added successfully", "GGPLOT_BUILD")

          plot <- plot + ggplot2::theme_minimal()
          log_debug("Theme added successfully", "GGPLOT_BUILD")

          plot
        },
        fallback = function(e) {
          log_debug(paste("ERROR in ggplot build:", e$message), "GGPLOT_BUILD")
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
        log_debug(paste("QIC_DATA X DEBUG: class =", class(qic_data$x)[1], "\ninherits Date =", inherits(qic_data$x, "Date"), "\ninherits POSIXct =", inherits(qic_data$x, "POSIXct")), "X_AXIS_FORMAT")

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

      # Add phase separation lines if parts exist
      if ("part" %in% names(qic_data) && length(unique(qic_data$part)) > 1) {
        # Find phase change points
        phase_changes <- which(diff(as.numeric(qic_data$part)) != 0)
        if (length(phase_changes) > 0) {
          for (change_point in phase_changes) {
            plot <- plot +
              ggplot2::geom_vline(
                xintercept = qic_data$x[change_point + 1],
                color = HOSPITAL_COLORS$warning,
                linetype = "dotted", linewidth = 1, alpha = 0.7
              )
          }
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
  )
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
        theme_minimal() +
        theme(
          plot.title = element_text(color = HOSPITAL_COLORS$primary, size = 14, face = "bold"),
          plot.subtitle = element_text(color = HOSPITAL_COLORS$secondary, size = 12),
          axis.title = element_text(color = HOSPITAL_COLORS$dark, size = 11),
          axis.text = element_text(color = HOSPITAL_COLORS$dark, size = 10),
          legend.title = element_text(color = HOSPITAL_COLORS$dark, size = 11),
          legend.text = element_text(color = HOSPITAL_COLORS$dark, size = 10),
          panel.grid.major = element_line(color = HOSPITAL_COLORS$light),
          panel.grid.minor = element_line(color = HOSPITAL_COLORS$light),
          strip.text = element_text(color = HOSPITAL_COLORS$primary, face = "bold")
        ) +
        labs(caption = footer_text) +
        theme(
          plot.caption = element_text(size = 8, color = HOSPITAL_COLORS$secondary, hjust = 0)
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
