# R/modules/visualization_spc.R
# SPC plot generation functions

library(qicharts2)
# library(ggplot2)
library(plotly)
library(lubridate)

# Helper function: Generate SPC plot
generateSPCPlot <- function(data, config, chart_type, target_value = NULL, show_phases = FALSE, skift_column = NULL, chart_title_reactive = NULL) {
  
  # Safety checks
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    stop("Ingen gyldig data at visualisere")
  }
  
  if (is.null(config$y_col) || !config$y_col %in% names(data)) {
    stop("Y-kolonne ikke fundet i data")
  }
  
  # Get title
  custom_title <- tryCatch({
    if (!is.null(chart_title_reactive) && is.function(chart_title_reactive)) {
      title <- chart_title_reactive()
      if (!is.null(title) && title != "" && title != "SPC Analyse") {
        title
      } else {
        NULL
      }
    } else {
      NULL
    }
  }, error = function(e) NULL)
  
  title_text <- if (!is.null(custom_title)) {
    custom_title
  } else {
    paste("SPC Chart -", config$y_col)
  }
  
  # Prepare data
  x_data <- if (!is.null(config$x_col) && config$x_col %in% names(data)) data[[config$x_col]] else NULL
  y_data_raw <- data[[config$y_col]]
  
  # Handle different chart types
  if (!is.null(config$n_col) && config$n_col %in% names(data)) {
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
    taeller <- parse_danish_number(data_filtered[[config$y_col]])
    naevner <- parse_danish_number(data_filtered[[config$n_col]])
    
    # Check conversion success
    if (any(is.na(taeller)) || any(is.na(naevner))) {
      invalid_taeller <- sum(is.na(taeller))
      invalid_naevner <- sum(is.na(naevner))
      stop(paste("Kunne ikke konvertere numeriske værdier:", 
                 if(invalid_taeller > 0) paste(invalid_taeller, "ugyldige tæller værdier"), 
                 if(invalid_naevner > 0) paste(invalid_naevner, "ugyldige nævner værdier")))
    }
    
    # Check for zero denominators
    if (any(naevner == 0)) {
      stop("Nævner kan ikke være nul (division by zero)")
    }
    
    # Update data reference to filtered data
    data <- data_filtered
    
    if (chart_type == "run") {
      y_data <- (taeller / naevner) * 100
      ylab_text <- paste("Rate (", config$y_col, "/", config$n_col, ") %")
    } else if (chart_type %in% c("p", "pp", "u", "up")) {
      y_data <- taeller
      n_data <- naevner
      ylab_text <- if (chart_type %in% c("p", "pp")) "Proportion" else "Rate"
    } else {
      y_data <- (taeller / naevner) * 100
      ylab_text <- paste("Rate (", config$y_col, "/", config$n_col, ") %")
    }
  } else {
    # Standard numeric data - filter out missing values first
    complete_rows <- !is.na(y_data_raw) & trimws(as.character(y_data_raw)) != ""
    
    if (!any(complete_rows)) {
      stop(paste("Ingen gyldige værdier fundet i", config$y_col, "kolonnen. Tjek at kolonne indeholder numeriske værdier."))
    }
    
    # Filter data to complete rows only
    data <- data[complete_rows, , drop = FALSE]
    y_data <- parse_danish_number(data[[config$y_col]])
    ylab_text <- config$y_col
    
    # Check conversion success
    if (all(is.na(y_data))) {
      stop(paste("Kunne ikke konvertere", config$y_col, "til numeriske værdier. Tjek at værdier er gyldige tal."))
    }
  }
  
  # Ensure we have minimum data points after filtering
  if (length(y_data) < 3) {
    stop(paste("For få gyldige datapunkter efter filtrering (", length(y_data), " fundet, minimum 3 påkrævet). Tilføj flere gyldige datapunkter."))
  }
  
  # Handle x-axis data (update after data filtering)
  x_data <- if (!is.null(config$x_col) && config$x_col %in% names(data)) data[[config$x_col]] else NULL
  
  if (is.null(x_data)) {
    x_data <- 1:length(y_data)
    xlab_text <- "Observation"
  } else {
    if (is.character(x_data) || is.factor(x_data)) {
      x_char <- as.character(x_data)
      parsed_dates <- lubridate::parse_date_time(x_char, orders = c("dmy", "ymd", "mdy"), quiet = TRUE)
      
      if (!all(is.na(parsed_dates))) {
        x_data <- as.Date(parsed_dates)
        xlab_text <- "Dato"
      } else {
        x_data <- 1:length(y_data)
        xlab_text <- "Observation"
      }
    } else if (inherits(x_data, "Date")) {
      xlab_text <- "Dato"
    } else {
      xlab_text <- config$x_col %||% "X"
    }
  }
  
  # Handle phases from selected Skift column
  part_positions <- NULL
  if (show_phases && !is.null(skift_column) && skift_column %in% names(data)) {
    skift_data <- data[[skift_column]]
    
    cat("DEBUG: Phase detection:\n")
    cat("  - show_phases:", show_phases, "\n")
    cat("  - skift_column:", skift_column, "\n")
    cat("  - skift_data:", paste(skift_data, collapse = ", "), "\n")
    
    # Convert to logical if needed
    if (!is.logical(skift_data)) {
      skift_data <- as.logical(skift_data)
      cat("  - converted to logical:", paste(skift_data, collapse = ", "), "\n")
    }
    
    # Get positions where TRUE values occur (these are where new phases start)
    skift_points <- which(skift_data == TRUE)
    cat("  - TRUE positions found:", paste(skift_points, collapse = ", "), "\n")
    
    if (length(skift_points) > 0) {
      # qic() expects integer vector of positions where new phases start
      part_positions <- sort(skift_points)
      cat("  - part_positions (integer):", paste(part_positions, collapse = ", "), "\n")
    }
  }
  
  # Build qic call arguments dynamically
  call_args <- list(
    x = x_data,
    y = y_data,
    chart = chart_type,
    title = title_text,
    ylab = ylab_text,
    xlab = xlab_text
  )
  
  # Add n for P/U charts
  if (chart_type %in% c("p", "pp", "u", "up") && exists("n_data")) {
    call_args$n <- n_data
  }
  
  # Add part for phases (integer positions where new phases start)
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
  
  # Debug: Print detailed qic call arguments
  cat("DEBUG: Detailed qic call arguments:\n")
  cat("  - Chart type:", chart_type, "\n")
  cat("  - Data points:", length(call_args$y), "\n")
  
  # X data analysis
  cat("  - X data type:", class(call_args$x), "\n")
  cat("  - X data length:", length(call_args$x), "\n")
  cat("  - X data sample (first 10):", paste(head(call_args$x, 10), collapse = ", "), "\n")
  cat("  - X data has NA:", any(is.na(call_args$x)), "\n")
  
  # Y data analysis
  cat("  - Y data type:", class(call_args$y), "\n")
  cat("  - Y data length:", length(call_args$y), "\n")
  cat("  - Y data sample (first 10):", paste(head(call_args$y, 10), collapse = ", "), "\n")
  cat("  - Y data has NA:", any(is.na(call_args$y)), "\n")
  cat("  - Y data range:", paste(range(call_args$y, na.rm = TRUE), collapse = " to "), "\n")
  
  # N data analysis (if present)
  if ("n" %in% names(call_args)) {
    cat("  - N data type:", class(call_args$n), "\n")
    cat("  - N data length:", length(call_args$n), "\n")
    cat("  - N data sample (first 10):", paste(head(call_args$n, 10), collapse = ", "), "\n")
    cat("  - N data has NA:", any(is.na(call_args$n)), "\n")
    cat("  - N data has zeros:", any(call_args$n == 0, na.rm = TRUE), "\n")
  }
  
  # Part analysis
  if ("part" %in% names(call_args)) {
    cat("  - Part type:", class(call_args$part), "\n")
    cat("  - Part content:", paste(call_args$part, collapse = ", "), "\n")
    cat("  - Part length:", length(call_args$part), "\n")
  } else {
    cat("  - Part positions: None\n")
  }
  
  # Target analysis
  if ("target" %in% names(call_args)) {
    cat("  - Target value:", call_args$target, "\n")
    cat("  - Target type:", class(call_args$target), "\n")
  }
  
  # Additional parameters
  cat("  - Title:", if(is.null(call_args$title)) "NULL" else call_args$title, "\n")
  cat("  - X label:", if(is.null(call_args$xlab)) "NULL" else call_args$xlab, "\n")  
  cat("  - Y label:", if(is.null(call_args$ylab)) "NULL" else call_args$ylab, "\n")
  
  # Generate SPC data using qicharts2 and build custom ggplot
  tryCatch({
    
    # Use data parameter approach like the working example
    # qicharts2::qic(x = Dato, y = `Tæller`, n = `Nævner`, part = c(12), data = data, return.data = TRUE)
    
    # Get column names for qic() call - respecting auto-detection results
    x_col_name <- config$x_col  # Auto-detected date column or NULL
    y_col_name <- config$y_col  # Should be "Tæller"
    n_col_name <- config$n_col  # Should be "Nævner"
    
    # Handle X column: Use Date column if detected, otherwise create sequence
    if (!is.null(x_col_name) && x_col_name %in% names(data)) {
      # Check if this is actually a date column (as detected by auto-detect)
      x_data_sample <- data[[x_col_name]]
      is_date_column <- inherits(x_data_sample, "Date") || 
                       (is.character(x_data_sample) && 
                        !is.null(suppressWarnings(lubridate::parse_date_time(head(x_data_sample, 3), 
                                                   orders = c("dmy", "ymd", "mdy"), quiet = TRUE))))
      
      if (is_date_column) {
        cat("DEBUG: Using detected date column:", x_col_name, "\n")
        
        # Convert character dates to actual Date objects (like your working example)
        if (is.character(data[[x_col_name]])) {
          cat("DEBUG: Converting character dates to Date objects\n")
          data[[x_col_name]] <- lubridate::parse_date_time(
            data[[x_col_name]], 
            orders = c("dmy", "ymd", "mdy"), 
            quiet = TRUE
          ) |> as.Date()
          cat("DEBUG: Converted to Date class:", class(data[[x_col_name]]), "\n")
        }
        
        x_col_for_qic <- x_col_name
      } else {
        cat("DEBUG: X column", x_col_name, "is not a date, creating sequence\n")
        data$obs_sequence <- 1:nrow(data)
        x_col_for_qic <- "obs_sequence"
      }
    } else {
      cat("DEBUG: No X column detected, creating observation sequence\n")
      data$obs_sequence <- 1:nrow(data)
      x_col_for_qic <- "obs_sequence"
    }
    
    # Generate SPC data using qicharts2
    tryCatch({
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
      if (!is.null(part_positions)) qic_args$part <- part_positions
      
      cat("DEBUG: About to call qic() with these arguments:\n")
      cat("  - data dim:", paste(dim(data), collapse = "x"), "\n")
      cat("  - chart:", chart_type, "\n")
      cat("  - x column:", x_col_for_qic, "\n")
      cat("  - y column:", y_col_name, "\n")
      cat("  - n column:", n_col_name, "\n")
      cat("  - part positions:", paste(part_positions, collapse = ", "), "\n")
      cat("  - sample data rows (first 3):\n")
      print(head(data, 3))
      
      qic_data <- do.call(qicharts2::qic, qic_args)
      
      cat("SUCCESS: qic() completed successfully!\n")
      cat("  - qic_data dimensions:", paste(dim(qic_data), collapse = "x"), "\n")
      cat("  - qic_data columns:", paste(names(qic_data), collapse = ", "), "\n")
      cat("  - First few rows of qic_data:\n")
      print(head(qic_data, 3))
      
      # Convert proportions to percentages for run charts with rate data
      if (chart_type == "run" && !is.null(config$n_col) && config$n_col %in% names(data)) {
        cat("DEBUG: Converting qic proportions to percentages for display\n")
        qic_data$y <- qic_data$y * 100
        qic_data$cl <- qic_data$cl * 100
        if (!is.null(qic_data$ucl)) qic_data$ucl <- qic_data$ucl * 100
        if (!is.null(qic_data$lcl)) qic_data$lcl <- qic_data$lcl * 100
      }
    
    }, error = function(e) {
      cat("ERROR: qic() failed with message:", e$message, "\n")
      cat("ERROR: Full error details:\n")
      print(e)
      cat("ERROR: Data summary at time of failure:\n")
      cat("  - Data rows:", nrow(data), "\n")
      cat("  - Chart type:", chart_type, "\n")
      cat("  - Y column:", y_col_name, "exists:", y_col_name %in% names(data), "\n")
      cat("  - N column:", n_col_name, "exists:", !is.null(n_col_name) && n_col_name %in% names(data), "\n")
      cat("  - X column:", x_col_for_qic, "exists:", x_col_for_qic %in% names(data), "\n")
      cat("  - Part positions:", part_positions, "\n")
      stop("Fejl ved qic() kald: ", e$message)
    })
    
    # Build custom plotly plot using qic calculations
    cat("DEBUG: Starting plotly plot construction...\n")
    
    # Start with empty plot and add data points only
    tryCatch({
      plot <- plotly::plot_ly(qic_data) %>%
        plotly::add_trace(x = ~x, y = ~y, type = "scatter", mode = "markers+lines",
                         name = "Data", 
                         marker = list(color = HOSPITAL_COLORS$secondary, size = 8),
                         line = list(color = HOSPITAL_COLORS$secondary)) |> 
        
        # Center line - using add_trace with explicit mode=lines and no markers
        plotly::add_trace(x = ~x, y = ~cl, name = "Median", 
                         type = "scatter", mode = "lines",
                         line = list(color = HOSPITAL_COLORS$hospitalblue, width = 2, dash = "solid"),
                         marker = list(opacity = 0))
      
      cat("DEBUG: Basic plot and center line added successfully\n")
    }, error = function(e) {
      cat("ERROR: Failed to create basic plotly plot:", e$message, "\n")
      stop("Plotly basic construction failed: ", e$message)
    })
      
    # Only add control limits if they exist (not NA)
    if (!all(is.na(qic_data$ucl))) {
      plot <- plot %>%
        plotly::add_trace(x = ~x, y = ~ucl, name = "UCL", type = "scatter", mode = "lines",
                         line = list(color = HOSPITAL_COLORS$danger, width = 2, dash = "dash"),
                         marker = list(opacity = 0))
    }
    
    if (!all(is.na(qic_data$lcl))) {
      plot <- plot %>%
        plotly::add_trace(x = ~x, y = ~lcl, name = "LCL", type = "scatter", mode = "lines",
                         line = list(color = HOSPITAL_COLORS$danger, width = 2, dash = "dash"), 
                         marker = list(opacity = 0))
    }
    
    # Layout and labels - conditional % suffix
    y_axis_config <- list(title = "", showgrid = FALSE)
    if (grepl("%", call_args$ylab) || (!is.null(config$n_col) && config$n_col %in% names(data))) {
      y_axis_config$ticksuffix <- "%"
    }
    
    plot <- plot %>%
      plotly::layout(
        title = call_args$title,
        xaxis = list(title = "", showgrid = FALSE),
        yaxis = y_axis_config,
        hovermode = "x unified",
        showlegend = TRUE
      )
    
    # Add target line if provided
    if (!is.null(target_value) && is.numeric(target_value) && !is.na(target_value)) {
      plot <- plot %>%
        plotly::add_lines(y = target_value, name = "Target", 
                         line = list(color = "#2E8B57", width = 2, dash = "solid"))
    }
    
    # Add phase separation lines if parts exist
    if ("part" %in% names(qic_data) && length(unique(qic_data$part)) > 1) {
      # Find phase change points
      phase_changes <- which(diff(as.numeric(qic_data$part)) != 0)
      if (length(phase_changes) > 0) {
        for (change_point in phase_changes) {
          plot <- plot %>%
            plotly::add_lines(x = c(qic_data$x[change_point + 1], qic_data$x[change_point + 1]),
                             y = c(min(qic_data$y, na.rm = TRUE), max(qic_data$y, na.rm = TRUE)),
                             name = paste("Phase Change", change_point),
                             line = list(color = HOSPITAL_COLORS$warning, width = 1, dash = "dot"))
        }
      }
    }
    
    cat("DEBUG: Plotly plot construction completed successfully!\n")
    cat("DEBUG: Plot class:", class(plot), "\n")
    
    # Configure plotly toolbar
    plot <- plot |> 
      plotly::config(displayModeBar = FALSE,
        modeBarButtonsToRemove = c('zoom2d','pan2d','lasso2d','select','resetScale',
                                  'hoverClosestCartesian','hoverCompareCartesian'),
        displaylogo = FALSE
      ) |> layout(showlegend = FALSE)
    
    return(plot)
    
  }, error = function(e) {
    # Fallback to basic plotly plot if qic() fails
    plot_data <- data.frame(x = call_args$x, y = call_args$y)
    
    plot <- plotly::plot_ly(plot_data) %>%
      plotly::add_trace(x = ~x, y = ~y, type = "scatter", mode = "markers+lines",
                       name = "Data",
                       marker = list(color = HOSPITAL_COLORS$secondary, size = 8),
                       line = list(color = HOSPITAL_COLORS$secondary)) %>%
      plotly::add_trace(x = ~x, y = median(call_args$y, na.rm = TRUE), name = "Median",
                       type = "scatter", mode = "lines",
                       line = list(color = HOSPITAL_COLORS$hospitalblue, width = 2, dash = "dash"),
                       marker = list(size = 0))
    
    # Conditional % suffix for fallback too
    y_axis_config <- list(title = "", showgrid = FALSE)
    if (grepl("%", call_args$ylab)) {
      y_axis_config$ticksuffix <- "%"
    }
    
    plot <- plot %>%
      plotly::layout(
        title = call_args$title,
        xaxis = list(title = "", showgrid = FALSE),
        yaxis = y_axis_config,
        hovermode = "x unified",
        showlegend = TRUE
      )
    
    # Add target line if provided
    if (!is.null(target_value) && is.numeric(target_value) && !is.na(target_value)) {
      plot <- plot %>%
        plotly::add_lines(y = target_value, name = "Target",
                         line = list(color = "#2E8B57", width = 2, dash = "solid"))
    }
    
    # Configure plotly toolbar for fallback too
    plot <- plot %>%
      plotly::config(
        modeBarButtonsToRemove = c('zoom2d','pan2d','lasso2d','select','resetScale',
                                  'hoverClosestCartesian','hoverCompareCartesian'),
        displaylogo = FALSE
      )
    
    return(plot)
  })
}
