# R/modules/visualization_anhoej.R
# Anhøj rules analysis for run charts

library(shiny)

# Helper function: Calculate Anhøj rules for run charts
calculateAnhoejRules <- function(data, config) {
  if (is.null(data) || is.null(config$y_col) || !config$y_col %in% names(data)) {
    return(list(
      runs_signal = FALSE,
      crossings_signal = FALSE,
      any_signal = FALSE,
      message = "Ingen data tilgængelig til Anhøj-analyse"
    ))
  }
  
  tryCatch({
    # Get Y data - handle tæller/nævner for run charts
    if (!is.null(config$n_col) && config$n_col %in% names(data)) {
      taeller <- parse_danish_number(data[[config$y_col]])
      naevner <- parse_danish_number(data[[config$n_col]])
      
      if (any(is.na(taeller)) || any(is.na(naevner)) || any(naevner == 0)) {
        return(list(
          runs_signal = FALSE,
          crossings_signal = FALSE,
          any_signal = FALSE,
          message = "Kunne ikke beregne rate fra tæller/nævner data"
        ))
      }
      
      y_data <- (taeller / naevner) * 100
    } else {
      y_data_raw <- data[[config$y_col]]
      y_data <- parse_danish_number(y_data_raw)
    }
    
    # Remove missing values
    y_clean <- y_data[!is.na(y_data)]
    n <- length(y_clean)
    
    # SPC standard: minimum 10 datapunkter for pålidelig Anhøj analyse
    if (n < 10) {
      return(list(
        runs_signal = FALSE,
        crossings_signal = FALSE,
        any_signal = FALSE,
        message = "For få datapunkter til Anhøj-analyse (minimum 10 påkrævet)",
        n_total = n
      ))
    }
    
    # Calculate median
    median_val <- median(y_clean)
    
    # Remove points on median for runs analysis
    y_no_median <- y_clean[y_clean != median_val]
    n_useful <- length(y_no_median)
    
    if (n_useful < 6) {
      return(list(
        runs_signal = FALSE,
        crossings_signal = FALSE,
        any_signal = FALSE,
        message = "For mange punkter på median til pålidelig analyse",
        n_total = n,
        n_useful = n_useful
      ))
    }
    
    # Calculate runs and crossings
    above_median <- y_no_median > median_val
    runs <- rle(above_median)
    longest_run <- max(runs$lengths)
    crossings <- sum(diff(above_median) != 0)
    
    # Critical values (Anhøj rules)
    runs_critical_value <- ceiling(log2(n_useful)) + 3
    crossings_critical_value <- max(1, floor(n_useful / 2) - 2)
    
    # Signal detection
    runs_signal <- longest_run >= runs_critical_value
    crossings_signal <- crossings <= crossings_critical_value
    
    return(list(
      runs_signal = runs_signal,
      crossings_signal = crossings_signal,
      any_signal = runs_signal || crossings_signal,
      longest_run = longest_run,
      runs_critical_value = runs_critical_value,
      crossings_count = crossings,
      crossings_critical_value = crossings_critical_value,
      n_useful = n_useful,
      n_total = n,
      median_value = median_val
    ))
    
  }, error = function(e) {
    return(list(
      runs_signal = FALSE,
      crossings_signal = FALSE,
      any_signal = FALSE,
      message = paste("Fejl i Anhøj analyse:", e$message)
    ))
  })
}

# Helper function: Render Anhøj results
renderAnhoejResults <- function(results) {
  if (!is.null(results$message) &&
      (is.null(results$longest_run) || is.null(results$crossings_count))) {
    return(div(class="alert alert-info", icon("info-circle"), results$message))
  }
  
  div(
    # Runs test
    div(
      style = "margin-bottom: 10px;",
      span(
        class = paste("status-indicator", if(results$runs_signal) "status-error" else "status-ready"),
        style = "margin-right: 8px;"
      ),
      strong("Usædvanligt lange runs: "),
      if(results$runs_signal) {
        span("JA - Signal detekteret", style = paste("color:", HOSPITAL_COLORS$danger))
      } else {
        span("NEJ - Ingen signal", style = paste("color:", HOSPITAL_COLORS$success))
      },
      br(),
      span(sprintf("Længste run: %d punkter (kritisk værdi: %d)", 
                   results$longest_run, results$runs_critical_value),
           style = "font-size: 0.85rem; color: #666;")
    ),
    
    # Crossings test
    div(
      style = "margin-bottom: 10px;",
      span(
        class = paste("status-indicator", if(results$crossings_signal) "status-error" else "status-ready"),
        style = "margin-right: 8px;"
      ),
      strong("Usædvanligt få krydsninger: "),
      if(results$crossings_signal) {
        span("JA - Signal detekteret", style = paste("color:", HOSPITAL_COLORS$danger))
      } else {
        span("NEJ - Ingen signal", style = paste("color:", HOSPITAL_COLORS$success))
      },
      br(),
      span(sprintf("Krydsninger: %d (kritisk værdi: ≤%d)", 
                   results$crossings_count, results$crossings_critical_value),
           style = "font-size: 0.85rem; color: #666;")
    ),
    
    # Summary
    div(
      style = "margin-top: 15px; padding-top: 10px; border-top: 1px solid #ddd;",
      strong("Samlet vurdering: "),
      if(results$any_signal) {
        span("SIGNAL DETEKTERET", style = paste("color:", HOSPITAL_COLORS$danger, "; font-weight: bold;"))
      } else {
        span("Ingen signaler", style = paste("color:", HOSPITAL_COLORS$success, "; font-weight: bold;"))
      },
      br(),
      span(sprintf("Analyseret %d datapunkter (median: %.2f)", 
                   results$n_useful, results$median_value),
           style = "font-size: 0.85rem; color: #666;")
    )
  )
}
