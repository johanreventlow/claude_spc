# R/modules/visualization_helpers.R
# Helper functions for the visualization module

library(ggplot2)
library(dplyr)
library(lubridate)

# Helper function: Detect chart configuration
detectChartConfiguration <- function(data, chart_type) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0 || ncol(data) == 0) {
    return(list(x_col = NULL, y_col = NULL, n_col = NULL, chart_type = chart_type))
  }
  
  # Filter out "Skift" column from detection
  available_cols <- names(data)[names(data) != "Skift"]
  
  # Find date/time column
  x_col <- NULL
  for (col_name in available_cols) {
    col_data <- data[[col_name]]
    
    if (all(is.na(col_data)) || length(col_data) == 0) {
      next
    }
    
    if (grepl("dato|date|tid|time", col_name, ignore.case = TRUE)) {
      x_col <- col_name
      break
    }
    
    tryCatch({
      char_data <- as.character(col_data)[!is.na(col_data)]
      if (length(char_data) > 0) {
        test_sample <- char_data[1:min(3, length(char_data))]
        
        date_test <- suppressWarnings(
          lubridate::parse_date_time(
            test_sample, 
            orders = c("dmy", "ymd", "mdy", "dby"),
            quiet = TRUE
          )
        )
        
        if (!is.null(date_test) && length(date_test) > 0) {
          success_rate <- sum(!is.na(date_test)) / length(date_test)
          if (success_rate >= 0.5) {
            x_col <- col_name
            break
          }
        }
      }
    }, error = function(e) {
      # Ignore errors in date detection
    })
  }
  
  if (is.null(x_col) && length(available_cols) > 0) {
    x_col <- available_cols[1]
  }
  
  # Find numeric columns
  numeric_cols <- character(0)
  for (col_name in available_cols) {
    if (col_name != x_col) {
      col_data <- data[[col_name]]
      if (is.numeric(col_data)) {
        numeric_cols <- c(numeric_cols, col_name)
      } else {
        numeric_test <- suppressWarnings(as.numeric(gsub(",", ".", as.character(col_data))))
        if (sum(!is.na(numeric_test)) > length(col_data) * 0.8) {
          numeric_cols <- c(numeric_cols, col_name)
        }
      }
    }
  }
  
  # Detect y and n columns
  y_col <- NULL
  n_col <- NULL
  
  # Look for Danish tæller/nævner patterns
  available_cols_lower <- tolower(available_cols)
  taeller_idx <- which(grepl("t.ller|tael|num|count", available_cols_lower, ignore.case = TRUE))
  naevner_idx <- which(grepl("n.vner|naev|denom|total", available_cols_lower, ignore.case = TRUE))
  
  if (length(taeller_idx) > 0 && length(naevner_idx) > 0) {
    y_col <- available_cols[taeller_idx[1]]
    n_col <- available_cols[naevner_idx[1]]
  } else if (length(numeric_cols) >= 2) {
    y_col <- numeric_cols[1]
    n_col <- numeric_cols[2]
  } else if (length(numeric_cols) >= 1) {
    y_col <- numeric_cols[1]
  }
  
  return(list(
    x_col = x_col,
    y_col = y_col,
    n_col = n_col,
    chart_type = chart_type
  ))
}

# Helper function: Validate data
validateDataForChart <- function(data, config, chart_type) {
  warnings <- character(0)
  
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    warnings <- c(warnings, "Ingen data tilgængelig")
    return(list(valid = FALSE, warnings = warnings))
  }
  
  if (is.null(config$y_col) || !config$y_col %in% names(data)) {
    warnings <- c(warnings, "Ingen numerisk kolonne fundet til Y-akse")
    return(list(valid = FALSE, warnings = warnings))
  }
  
  if (chart_type %in% c("p", "pp", "u", "up")) {
    if (is.null(config$n_col) || !config$n_col %in% names(data)) {
      warnings <- c(warnings, paste("Chart type", chart_type, "kræver en nævner-kolonne"))
      return(list(valid = FALSE, warnings = warnings))
    }
  }
  
  # Check for missing values
  y_data <- data[[config$y_col]]
  if (all(is.na(y_data))) {
    warnings <- c(warnings, "Alle værdier i Y-kolonnen er tomme")
    return(list(valid = FALSE, warnings = warnings))
  }
  
  # Validate Skift column if it exists
  if ("Skift" %in% names(data)) {
    skift_validation <- validateSkiftColumn(data)
    if (!skift_validation$valid) {
      warnings <- c(warnings, skift_validation$warnings)
    }
    
    if (skift_validation$phase_count > nrow(data) / 5) {
      warnings <- c(warnings, "For mange phases i forhold til antallet af datapunkter")
    }
  }
  
  if (nrow(data) < 8) {
    warnings <- c(warnings, paste("Kun", nrow(data), "datapunkter - SPC analyse er mest pålidelig med mindst 15-20 punkter"))
  }
  
  return(list(valid = TRUE, warnings = warnings))
}

# Helper function: Show placeholder when no plot
showPlaceholder <- function() {
  ggplot() + 
    xlim(0, 1) + ylim(0, 1) +
    annotate(
      "text", x = 0.5, y = 0.5,
      label = "Teknisk fejl ved graf-generering",
      size = 5,
      color = HOSPITAL_COLORS$danger
    ) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = HOSPITAL_COLORS$light, color = NA),
      plot.margin = margin(20, 20, 20, 20)
    )
}

# Helper function: Apply hospital theme to plots
applyHospitalTheme <- function(plot) {
  if (is.null(plot) || !inherits(plot, "ggplot")) {
    return(plot)
  }
  
  tryCatch({
    plot + 
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
      labs(
        caption = create_plot_footer(
          afdeling = "",
          data_kilde = "Upload",
          dato = Sys.Date()
        )
      ) +
      theme(
        plot.caption = element_text(size = 8, color = HOSPITAL_COLORS$secondary, hjust = 0)
      )
  }, error = function(e) {
    cat("ERROR applying hospital theme:", e$message, "\n")
    return(plot)
  })
}
