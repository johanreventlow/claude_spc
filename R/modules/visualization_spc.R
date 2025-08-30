# R/modules/visualization_spc.R
# SPC plot generation functions

library(qicharts2)
library(ggplot2)
library(lubridate)

# Helper function: Generate SPC plot
generateSPCPlot <- function(data, config, chart_type, show_targets = FALSE, show_phases = FALSE, chart_title_reactive = NULL) {
  
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
    taeller <- suppressWarnings(as.numeric(gsub(",", ".", as.character(y_data_raw))))
    naevner <- suppressWarnings(as.numeric(gsub(",", ".", as.character(data[[config$n_col]]))))
    
    if (any(is.na(taeller)) || any(is.na(naevner)) || any(naevner == 0)) {
      stop("Kunne ikke konvertere tæller eller nævner til numeriske værdier")
    }
    
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
    # Standard numeric data
    y_data <- suppressWarnings(as.numeric(gsub(",", ".", as.character(y_data_raw))))
    ylab_text <- config$y_col
  }
  
  # Check conversion success
  if (all(is.na(y_data))) {
    stop(paste("Kunne ikke konvertere", config$y_col, "til numeriske værdier"))
  }
  
  # Handle x-axis data
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
  
  # Handle phases from Skift column
  part_vector <- NULL
  if ("Skift" %in% names(data)) {
    skift_col <- as.logical(data[["Skift"]])
    skift_points <- which(skift_col == TRUE)
    
    if (length(skift_points) > 0) {
      part_vector <- rep(1, length(y_data))
      current_phase <- 1
      for (shift_point in sort(skift_points)) {
        if (shift_point <= length(part_vector)) {
          current_phase <- current_phase + 1
          part_vector[shift_point:length(part_vector)] <- current_phase
        }
      }
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
  
  # Add part for phases
  if (!is.null(part_vector)) {
    call_args$part <- part_vector
  }
  
  # Clean data
  complete_cases <- complete.cases(call_args$x, call_args$y)
  if (!all(complete_cases)) {
    call_args$x <- call_args$x[complete_cases]
    call_args$y <- call_args$y[complete_cases]
    
    if ("n" %in% names(call_args)) {
      call_args$n <- call_args$n[complete_cases]
    }
    
    if ("part" %in% names(call_args)) {
      call_args$part <- call_args$part[complete_cases]
    }
  }
  
  if (length(call_args$y) < 3) {
    stop("For få datapunkter efter rensning (minimum 3 påkrævet)")
  }
  
  # Generate plot with qicharts2
  tryCatch({
    plot <- do.call(qicharts2::qic, call_args)
    return(plot)
  }, error = function(e) {
    cat("ERROR in qic generation:", e$message, "\n")
    
    # Fallback to basic ggplot
    plot_data <- data.frame(x = call_args$x, y = call_args$y)
    
    plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::geom_hline(yintercept = median(call_args$y, na.rm = TRUE), 
                         color = "red", linetype = "dashed") +
      ggplot2::labs(title = call_args$title, x = call_args$xlab, y = call_args$ylab) +
      ggplot2::theme_minimal()
    
    return(plot)
  })
}
