# visualization_spc.R
# SPC plotgenerering med qicharts2 og tilpasset ggplot styling

# Dependencies ----------------------------------------------------------------
library(qicharts2)
library(ggplot2)
library(lubridate)

# HJÆLPEFUNKTIONER ============================================================

## Konverter enheds-kode til dansk label
get_unit_label <- function(unit_code, unit_list) {
  if (is.null(unit_code) || unit_code == "") {
    return("")
  }
  
  # Find dansk navn baseret på værdi
  unit_names <- names(unit_list)[unit_list == unit_code]
  if (length(unit_names) > 0) {
    return(unit_names[1])
  }
  
  # Fallback til koden selv
  return(unit_code)
}

## Validering og formatering af X-kolonne data
validate_x_column_format <- function(data, x_col, x_axis_unit) {
  # Return default hvis ingen x-kolonne
  if (is.null(x_col) || !x_col %in% names(data)) {
    return(list(
      x_data = 1:nrow(data),
      x.format = NULL,
      is_date = FALSE
    ))
  }
  
  x_data <- data[[x_col]]
  
  # Tjek om data allerede er Date/POSIXct
  if (inherits(x_data, c("Date", "POSIXct", "POSIXt"))) {
    # Data er allerede formateret som dato/tid
    x_format <- get_x_format_string(x_axis_unit)
    return(list(
      x_data = x_data,
      x.format = x_format,
      is_date = TRUE
    ))
  }
  
  # Forsøg intelligent date detection med lubridate
  if (is.character(x_data) || is.factor(x_data)) {
    char_data <- as.character(x_data)[!is.na(x_data)]
    
    if (length(char_data) > 0) {
      # Test sample til date detection
      test_sample <- char_data[1:min(5, length(char_data))]
      
      # FØRST: Test danske dato-formater direkte (mest almindelige)
      danish_parsed <- suppressWarnings(lubridate::dmy(char_data))
      danish_success_rate <- sum(!is.na(danish_parsed)) / length(danish_parsed)
      
      if (danish_success_rate >= 0.7) {
        # Danske datoer fungerer - konverter til POSIXct for konsistens med qicharts2
        x_data_converted <- as.POSIXct(danish_parsed)
        x_format <- get_x_format_string(x_axis_unit)
        
        cat("ON-THE-FLY: Konverterede", x_col, "danske datoer til POSIXct (success rate:", round(danish_success_rate, 2), ")\n")
        
        return(list(
          x_data = x_data_converted,
          x.format = x_format,
          is_date = TRUE
        ))
      }
      
      # FALLBACK: Brug lubridate guess_formats for andre formater (med error handling)
      tryCatch({
        guessed_formats <- suppressWarnings(
          lubridate::guess_formats(test_sample, c("ymd", "dmy", "mdy", "dby", "dmY", "Ymd", "mdY"))
        )
        
        if (!is.null(guessed_formats) && length(guessed_formats) > 0) {
          # Filtrer ugyldige formater (undgå "n" format problem)
          valid_formats <- guessed_formats[!grepl("^n$|Unknown", guessed_formats)]
          
          if (length(valid_formats) > 0) {
            # Test konvertering med guessed formats
            parsed_dates <- suppressWarnings(
              lubridate::parse_date_time(char_data, orders = valid_formats, quiet = TRUE)
            )
            
            if (!is.null(parsed_dates)) {
              success_rate <- sum(!is.na(parsed_dates)) / length(parsed_dates)
              
              if (success_rate >= 0.7) { # 70% success rate threshold
                # Konverter til Date objekter
                x_data_converted <- as.Date(parsed_dates)
                x_format <- get_x_format_string(x_axis_unit)
                
                return(list(
                  x_data = x_data_converted,
                  x.format = x_format,
                  is_date = TRUE
                ))
              }
            }
          }
        }
      }, error = function(e) {
        # Skip denne parsing metode hvis den fejler
        cat("WARNING: guess_formats parsing fejlede:", e$message, "\n")
      })
    }
  }
  
  # Numerisk data eller tekst der ikke kunne parses som datoer
  if (is.numeric(x_data)) {
    return(list(
      x_data = x_data,
      x.format = NULL,
      is_date = FALSE
    ))
  } else {
    # Fallback til observation nummer
    return(list(
      x_data = 1:length(x_data),
      x.format = NULL,
      is_date = FALSE
    ))
  }
}

## Simpel formatering baseret på x_axis_unit
get_x_format_string <- function(x_axis_unit) {
  switch(x_axis_unit,
    "date" = "%Y-%m-%d",
    "month" = "%b %Y", 
    "year" = "%Y",
    "week" = "Uge %W",
    "hour" = "%H:%M",
    "%Y-%m-%d"  # default
  )
}

## Intelligent dato-interval detektion
detect_date_interval <- function(dates, debug = TRUE) {
  if (length(dates) < 2) {
    return(list(type = "insufficient_data", consistency = 0, timespan_days = 0))
  }
  
  # Sorter datoer og beregn intervaller
  sorted_dates <- sort(dates[!is.na(dates)])
  if (length(sorted_dates) < 2) {
    return(list(type = "insufficient_data", consistency = 0, timespan_days = 0))
  }
  
  # Beregn forskelle mellem konsekutive datoer (i dage)
  intervals <- as.numeric(diff(sorted_dates))
  
  if (length(intervals) == 0) {
    return(list(type = "insufficient_data", consistency = 0, timespan_days = 0))
  }
  
  median_interval <- median(intervals, na.rm = TRUE)
  interval_variance <- var(intervals, na.rm = TRUE)
  consistency <- 1 - (sqrt(interval_variance) / median_interval)  # Høj værdi = konsistent
  consistency <- max(0, min(1, consistency))  # Klamp til 0-1
  
  timespan_days <- as.numeric(max(sorted_dates) - min(sorted_dates))
  
  # Klassificer interval type baseret på median
  interval_type <- if (median_interval <= 1) {
    "daily"
  } else if (median_interval <= 10) {
    "weekly" 
  } else if (median_interval <= 40) {
    "monthly"
  } else if (median_interval <= 120) {
    "quarterly"
  } else if (median_interval <= 400) {
    "yearly"
  } else {
    "irregular"
  }
  
  if (debug) {
    cat("DATE INTERVAL DEBUG:\n")
    cat("- Observations:", length(sorted_dates), "\n")
    cat("- Median interval:", round(median_interval, 1), "days\n")
    cat("- Consistency:", round(consistency, 2), "\n")
    cat("- Type:", interval_type, "\n")
    cat("- Timespan:", round(timespan_days), "days\n")
  }
  
  return(list(
    type = interval_type,
    median_days = median_interval,
    consistency = consistency,
    timespan_days = timespan_days,
    n_obs = length(sorted_dates)
  ))
}

## Optimal formatering baseret på interval og antal observationer
get_optimal_formatting <- function(interval_info, debug = TRUE) {
  interval_type <- interval_info$type
  n_obs <- interval_info$n_obs
  timespan_days <- interval_info$timespan_days
  
  # Formatering matrix baseret på interval type og antal observationer
  config <- switch(interval_type,
    daily = {
      if (n_obs < 30) {
        list(labels = "%d %b", breaks = "1 week", n_breaks = 8)
      } else if (n_obs < 90) {
        list(labels = "%b %Y", breaks = "2 weeks", n_breaks = 10)
      } else {
        list(labels = "%b %Y", breaks = "1 month", n_breaks = 12)
      }
    },
    weekly = {
      if (n_obs <=36) {
        # Intelligent uge-formatering med scales::label_date_short()
        list(
          use_smart_labels = TRUE,
          labels = scales::label_date_short(),
          n_breaks = min(n_obs, 24)  # Max 15 breaks for læsbarhed
        )
      } else {
        # For mange uger - skift til månedlig visning
        list(
          use_smart_labels = FALSE,
          labels = "%b %Y", 
          breaks = "1 month", 
          n_breaks = 12
        )
      }
    },
    monthly = {
      if (n_obs < 12) {
        # Intelligent måneds-formatering med scales::label_date_short()
        list(
          use_smart_labels = TRUE,
          labels = scales::label_date_short(
            format = c("%Y", "%b"),  # År først, så måneder
            sep = "\n"
          ),
          breaks = "1 month", 
          n_breaks = n_obs
        )
      } else if (n_obs < 40) {
        # Intelligent måneds-formatering med scales::label_date_short()
        list(
          use_smart_labels = TRUE,
          labels = scales::label_date_short(),
          breaks = "3 months", 
          n_breaks = 8
        )
      } else {
        # For mange måneder - skift til årlig visning med smart labels
        list(
          use_smart_labels = TRUE,
          labels = scales::label_date_short(
            format = c("%Y", "", ""),  # Kun år
            sep = ""
          ),
          breaks = "6 months", 
          n_breaks = 10
        )
      }
    },
    quarterly = {
      list(labels = "Q%q %Y", breaks = "3 months", n_breaks = 8)
    },
    yearly = {
      list(labels = "%Y", breaks = "1 year", n_breaks = min(n_obs, 10))
    },
    # Default/irregular
    {
      # Tilpas til tidsspan
      if (timespan_days < 100) {
        list(labels = "%d %b %Y", breaks = "2 weeks", n_breaks = 8)
      } else if (timespan_days < 730) {
        list(labels = "%b %Y", breaks = "2 months", n_breaks = 10)
      } else {
        list(labels = "%Y", breaks = "1 year", n_breaks = 12)
      }
    }
  )
  
  if (debug) {
    cat("FORMATTING DEBUG:\n")
    cat("- Selected labels:", if(is.function(config$labels)) "function (smart labels)" else config$labels, "\n")
    cat("- Selected breaks:", config$breaks, "\n")
    cat("- N breaks:", config$n_breaks, "\n")
    if(!is.null(config$use_smart_labels)) cat("- Smart labels:", config$use_smart_labels, "\n")
  }
  
  return(config)
}


# SPC PLOT GENERERING =========================================================

## Generér SPC plot med tilpasset styling
generateSPCPlot <- function(data, config, chart_type, target_value = NULL, centerline_value = NULL, show_phases = FALSE, skift_column = NULL, frys_column = NULL, chart_title_reactive = NULL, x_axis_unit = "observation", y_axis_unit = "count", kommentar_column = NULL) {
  
  # DEBUG: Log input parameters
  cat("=== FUNCTION INPUT DEBUG ===\n")
  cat("x_axis_unit parameter received:", x_axis_unit, "\n")
  cat("y_axis_unit parameter received:", y_axis_unit, "\n")
  cat("============================\n")
  
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
    
    # Get unit labels early - before they are used
    x_unit_label <- get_unit_label(x_axis_unit, X_AXIS_UNITS_DA)
    y_unit_label <- get_unit_label(y_axis_unit, Y_AXIS_UNITS_DA)
    
    if (chart_type == "run") {
      y_data <- (taeller / naevner) * 100
      ylab_text <- if (y_unit_label != "") y_unit_label else paste("Rate (", config$y_col, "/", config$n_col, ") %")
    } else if (chart_type %in% c("p", "pp", "u", "up")) {
      y_data <- taeller
      n_data <- naevner
      ylab_text <- if (y_unit_label != "") y_unit_label else (if (chart_type %in% c("p", "pp")) "Proportion" else "Rate")
    } else {
      y_data <- (taeller / naevner) * 100
      ylab_text <- if (y_unit_label != "") y_unit_label else paste("Rate (", config$y_col, "/", config$n_col, ") %")
    }
  } else {
    # Get unit labels early - before they are used
    x_unit_label <- get_unit_label(x_axis_unit, X_AXIS_UNITS_DA)
    y_unit_label <- get_unit_label(y_axis_unit, Y_AXIS_UNITS_DA)
    
    # Standard numeric data - filter out missing values first
    complete_rows <- !is.na(y_data_raw) & trimws(as.character(y_data_raw)) != ""
    
    if (!any(complete_rows)) {
      stop(paste("Ingen gyldige værdier fundet i", config$y_col, "kolonnen. Tjek at kolonne indeholder numeriske værdier."))
    }
    
    # Filter data to complete rows only
    data <- data[complete_rows, , drop = FALSE]
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
  x_validation <- validate_x_column_format(data, config$x_col, x_axis_unit)
  x_data <- x_validation$x_data
  # xlab_text <- if (x_unit_label != "") x_unit_label else {
  #   if (x_validation$is_date) "Dato" else "Observation"
  # }
  
  # DEBUG: Log x-validation results
  cat("=== X-VALIDATION DEBUG ===\n")
  cat("x_col:", config$x_col, "\n")
  cat("x_axis_unit:", x_axis_unit, "\n")
  cat("x_data class:", class(x_data)[1], "\n")
  cat("x_data sample:", paste(head(x_data, 3), collapse = ", "), "\n")
  cat("is_date:", x_validation$is_date, "\n")
  cat("x.period:", x_validation$x.period, "\n")
  cat("x.format:", x_validation$x.format, "\n")
  cat("========================\n")
  
  # Handle phases from selected Skift column
  part_positions <- NULL
  if (show_phases && !is.null(skift_column) && skift_column %in% names(data)) {
    skift_data <- data[[skift_column]]
    
    # Convert to logical if needed
    if (!is.logical(skift_data)) {
      skift_data <- as.logical(skift_data)
    }
    
    # Get positions where TRUE values occur (these are where new phases start)
    skift_points <- which(skift_data == TRUE)
    
    if (length(skift_points) > 0) {
      # qic() expects integer vector of positions where new phases start
      part_positions <- sort(skift_points)
    }
  }
  
  # Handle baseline freeze from selected Frys column
  freeze_position <- NULL
  if (!is.null(frys_column) && frys_column %in% names(data)) {
    frys_data <- data[[frys_column]]
    
    # Convert to logical if needed
    if (!is.logical(frys_data)) {
      frys_data <- as.logical(frys_data)
    }
    
    # Get positions where TRUE values occur (baseline freeze points)
    frys_points <- which(frys_data == TRUE)
    
    if (length(frys_points) > 0) {
      # Use the last TRUE position as freeze point (baseline up to this point)
      freeze_position <- max(frys_points)
    }
  }
  
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
  tryCatch({
    
    # Use data parameter approach like the working example
    # qicharts2::qic(x = Dato, y = `Tæller`, n = `Nævner`, part = c(12), data = data, return.data = TRUE)
    
    # Get column names for qic() call - respecting auto-detection results
    x_col_name <- config$x_col  # Auto-detected date column or NULL
    y_col_name <- config$y_col  # Should be "Tæller"
    n_col_name <- config$n_col  # Should be "Nævner"
    
    # Brug data fra x_validation i stedet for duplikeret logik
    if (!is.null(x_col_name) && x_col_name %in% names(data) && x_validation$is_date) {
      # Debug logging før opdatering
      cat("DATAFRAME UPDATE DEBUG:\n")
      cat("- data nrows:", nrow(data), "\n")
      cat("- x_validation$x_data length:", length(x_validation$x_data), "\n")
      cat("- x_validation$x_data class:", class(x_validation$x_data)[1], "\n")
      cat("- Original", x_col_name, "class:", class(data[[x_col_name]])[1], "\n")
      
      # Opdater kolonnen med de processerede data fra x_validation
      if (length(x_validation$x_data) == nrow(data)) {
        data[[x_col_name]] <- x_validation$x_data
        x_col_for_qic <- x_col_name
        
        cat("✓ SUCCESS: Opdaterede", x_col_name, "til", class(x_validation$x_data)[1], "\n")
        cat("QICDATA DEBUG: Bruger dato-kolonne", x_col_name, "med", length(x_validation$x_data), "datoer\n")
      } else {
        cat("✗ ERROR: Længde mismatch - x_validation$x_data:", length(x_validation$x_data), "vs data rows:", nrow(data), "\n")
        # Fallback til observation sekvens
        if (!("obs_sequence" %in% names(data))) {
          data$obs_sequence <- 1:nrow(data)
        }
        x_col_for_qic <- "obs_sequence"
        cat("FALLBACK: Bruger obs_sequence i stedet\n")
      }
    } else {
      # Brug observation sekvens som fallback
      if (!("obs_sequence" %in% names(data))) {
        data$obs_sequence <- 1:nrow(data)
      }
      x_col_for_qic <- "obs_sequence"
      
      cat("QICDATA DEBUG: Bruger obs_sequence som fallback\n")
    }
    
    # Note: obs_sequence fjernes IKKE fra data da det måske bruges af andre komponenter
    
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
      cat("=== QIC CALL DEBUG ===\n")
      cat("qic_args structure:\n")
      str(qic_args)
      cat("Calling qic()...\n")
      
      qic_data <- do.call(qicharts2::qic, qic_args)
      cat("QIC call successful, returned data dimensions:", nrow(qic_data), "x", ncol(qic_data), "\n")
      
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
    
    }, error = function(e) {
      stop("Fejl ved qic() kald: ", e$message)
    })
    
    # Handle comment data for labels
    comment_data <- NULL
    if (!is.null(kommentar_column) && kommentar_column %in% names(data)) {
      # Extract comments and sync with qic_data
      comments_raw <- data[[kommentar_column]]
      
      # Create comment data frame aligned with qic_data
      comment_data <- data.frame(
        x = qic_data$x,
        y = qic_data$y,
        comment = comments_raw[1:nrow(qic_data)],  # Ensure same length as qic_data
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
    cat("=== GGPLOT BUILD DEBUG ===\n")
    cat("qic_data dimensions:", nrow(qic_data), "x", ncol(qic_data), "\n")
    cat("qic_data columns:", names(qic_data), "\n")
    
    tryCatch({
      plot <- ggplot2::ggplot(qic_data, ggplot2::aes(x = x, y = y))
      cat("Base plot created successfully\n")
      
      plot <- plot + ggplot2::geom_line(color = HOSPITAL_COLORS$lightgrey, linewidth = 1)
      cat("Line geom added successfully\n")
      
      plot <- plot + ggplot2::geom_point(size = 2, color = HOSPITAL_COLORS$mediumgrey)
      cat("Point geom added successfully\n")
      
      plot <- plot + ggplot2::geom_line(ggplot2::aes(y = cl), color = HOSPITAL_COLORS$hospitalblue, 
                        linetype = "solid", linewidth = 1)
      cat("Center line added successfully\n")
      
      plot <- plot + ggplot2::labs(title = call_args$title, x = "", y = "")
      cat("Labels added successfully\n")
      
      plot <- plot + ggplot2::theme_minimal()
      cat("Theme added successfully\n")
      
    }, error = function(e) {
      cat("ERROR in ggplot build:", e$message, "\n")
      stop(e)
    })
    
    # Add control limits conditionally
    if (!is.null(qic_data$ucl) && !all(is.na(qic_data$ucl))) {
      plot <- plot + 
        ggplot2::geom_line(ggplot2::aes(y = ucl), color = HOSPITAL_COLORS$danger, 
                          linetype = "dashed", linewidth = 0.8)
    }
    
    if (!is.null(qic_data$lcl) && !all(is.na(qic_data$lcl))) {
      plot <- plot + 
        ggplot2::geom_line(ggplot2::aes(y = lcl), color = HOSPITAL_COLORS$danger, 
                          linetype = "dashed", linewidth = 0.8)
    }
    
    # Intelligent x-akse formatering baseret på dato-mønstre
    if (!is.null(x_validation$x.format) && x_validation$is_date) {
      # DEBUG: Tjek qic_data$x type
      cat("QIC_DATA X DEBUG: class =", class(qic_data$x)[1], "\n")
      cat("QIC_DATA X DEBUG: inherits Date =", inherits(qic_data$x, "Date"), "\n")
      cat("QIC_DATA X DEBUG: inherits POSIXct =", inherits(qic_data$x, "POSIXct"), "\n")
      
      # Intelligent interval detektion og formatering
      interval_info <- detect_date_interval(qic_data$x, debug = TRUE)
      format_config <- get_optimal_formatting(interval_info, debug = TRUE)
      
      # qic() konverterer Date objekter til POSIXct, så brug scale_x_datetime
      if (inherits(qic_data$x, c("POSIXct", "POSIXt"))) {
        # Håndter intelligent formatering separat
        if (interval_info$type == "weekly" && !is.null(format_config$use_smart_labels) && format_config$use_smart_labels) {
          cat("SMART WEEKLY LABELS: Applying intelligent week formatting\n")
          plot <- plot + ggplot2::scale_x_datetime(
            name = x_unit_label,
            labels = format_config$labels,  # Smart scales::label_date_short()
            # breaks = scales::date_breaks(format_config$breaks)
            breaks = scales::breaks_pretty(n = format_config$n_breaks)
          )
        } else if (interval_info$type == "monthly" && !is.null(format_config$use_smart_labels) && format_config$use_smart_labels) {
          cat("SMART MONTHLY LABELS: Applying intelligent month formatting\n")
          plot <- plot + ggplot2::scale_x_datetime(
            name = x_unit_label,
            labels = format_config$labels,  # Smart scales::label_date_short()
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
          cat("SMART WEEKLY LABELS: Applying intelligent week formatting for Date objects\n")
          plot <- plot + ggplot2::scale_x_date(
            name = x_unit_label,
            labels = format_config$labels,  # Smart scales::label_date_short()
            breaks = scales::date_breaks(format_config$breaks)
          )
        } else if (interval_info$type == "monthly" && !is.null(format_config$use_smart_labels) && format_config$use_smart_labels) {
          cat("SMART MONTHLY LABELS: Applying intelligent month formatting for Date objects\n")
          plot <- plot + ggplot2::scale_x_date(
            name = x_unit_label,
            labels = format_config$labels,  # Smart scales::label_date_short()
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
            ggplot2::geom_vline(xintercept = qic_data$x[change_point + 1], 
                               color = HOSPITAL_COLORS$warning, 
                               linetype = "dotted", linewidth = 1, alpha = 0.7)
        }
      }
    }
    
    # Add target line if provided
    if (!is.null(target_value) && is.numeric(target_value) && !is.na(target_value)) {
      plot <- plot + 
        ggplot2::geom_hline(yintercept = target_value, 
                           color = HOSPITAL_COLORS$darkgrey, linetype="42", linewidth = 1.2,
                           alpha = 0.8)
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
    
  }, error = function(e) {
    # Fallback to basic ggplot if qic() fails
    plot_data <- data.frame(x = call_args$x, y = call_args$y)
    
    plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(size = 2, color = HOSPITAL_COLORS$primary) +
      ggplot2::geom_line(color = HOSPITAL_COLORS$primary, alpha = 0.7) +
      ggplot2::geom_hline(yintercept = median(call_args$y, na.rm = TRUE), 
                         color = HOSPITAL_COLORS$secondary, linetype = "dashed") +
      ggplot2::labs(title = call_args$title, x = "", y = "") +
      ggplot2::theme_minimal()
    
    # Add target line if provided
    if (!is.null(target_value) && is.numeric(target_value) && !is.na(target_value)) {
      plot <- plot + 
        ggplot2::geom_hline(yintercept = target_value, 
                           color = "#2E8B57", linetype = "solid", linewidth = 1.2,
                           alpha = 0.8)
    }
    
    return(list(plot = plot, qic_data = NULL))
  })
}
