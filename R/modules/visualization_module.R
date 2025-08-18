# R/modules/visualization_module.R
library(shiny)
library(qicharts2)
library(ggplot2)
library(dplyr)

# Visualization Module UI
visualizationModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Plot output with loading spinner
    div(
      id = ns("plot_container"),
      style = "position: relative;",
      
      # Loading overlay
      conditionalPanel(
        condition = paste0("output['", ns("plot_ready"), "'] == false"),
        div(
          style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); z-index: 1000;",
          div(
            class = "text-center",
            div(class = "spinner-border text-primary", role = "status"),
            br(),
            "Genererer graf..."
          )
        )
      ),
      
      # Main plot output
      plotOutput(
        ns("spc_plot"),
        height = "500px",
        brush = brushOpts(id = ns("plot_brush"), resetOnNew = TRUE),
        hover = hoverOpts(id = ns("plot_hover"), delay = 100, delayType = "debounce")
      )
    ),
    
    br(),
    
    # Plot information and warnings
    uiOutput(ns("plot_info")),
    
    # Anhøj rules results (Phase 3.2)
    conditionalPanel(
      condition = "true",  # Always show, but content will be conditional
      div(id = ns("anhoej_section"),
          uiOutput(ns("anhoej_rules_section"))
      )
    )
  )
}

# Visualization Module Server
visualizationModuleServer <- function(id, data_reactive, chart_type_reactive, show_targets_reactive, show_phases_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      plot_object = NULL,
      plot_ready = FALSE,
      anhoej_results = NULL,
      plot_warnings = character(0)
    )
    
    # Detect chart configuration based on data
    chart_config <- reactive({
      req(data_reactive())
      
      cat("DEBUG: chart_config() called\n")
      
      data <- data_reactive()
      chart_type <- chart_type_reactive() %||% "run"
      
      cat("DEBUG: chart_config - data rows:", nrow(data), "cols:", ncol(data), "\n")
      cat("DEBUG: chart_config - chart_type:", chart_type, "\n")
      cat("DEBUG: chart_config - column names:", paste(names(data), collapse = ", "), "\n")
      
      # Auto-detect appropriate columns
      config <- detectChartConfiguration(data, chart_type)
      
      cat("DEBUG: chart_config - x_col:", config$x_col, "y_col:", config$y_col, "\n")
      
      return(config)
    })
    
    # Generate SPC plot
    spc_plot <- reactive({
      req(data_reactive(), chart_config())
      
      cat("DEBUG: spc_plot() called\n")
      
      data <- data_reactive()
      config <- chart_config()
      chart_type <- chart_type_reactive() %||% "run"
      
      cat("DEBUG: spc_plot - Starting validation\n")
      
      # Validate data for selected chart type
      validation <- validateDataForChart(data, config, chart_type)
      
      cat("DEBUG: spc_plot - Validation result:", validation$valid, "\n")
      if(length(validation$warnings) > 0) {
        cat("DEBUG: spc_plot - Warnings:", paste(validation$warnings, collapse = "; "), "\n")
      }
      
      if (!validation$valid) {
        values$plot_warnings <- validation$warnings
        values$plot_ready <- FALSE
        cat("DEBUG: spc_plot - Validation failed, returning NULL\n")
        return(NULL)
      }
      
      # Clear warnings if validation passes
      values$plot_warnings <- character(0)
      
      cat("DEBUG: spc_plot - Starting plot generation\n")
      
      tryCatch({
        # Generate plot using qicharts2
        cat("DEBUG: spc_plot - Calling generateSPCPlot\n")
        plot <- generateSPCPlot(data, config, chart_type, 
                                show_targets = show_targets_reactive(),
                                show_phases = show_phases_reactive())
        
        cat("DEBUG: spc_plot - Plot generated successfully\n")
        
        # Apply hospital branding
        plot <- applyHospitalTheme(plot)
        
        values$plot_object <- plot
        values$plot_ready <- TRUE
        
        cat("DEBUG: spc_plot - Plot ready set to TRUE\n")
        
        # Calculate Anhøj rules for run charts
        if (chart_type == "run") {
          cat("DEBUG: spc_plot - Calculating Anhøj rules\n")
          values$anhoej_results <- calculateAnhoejRules(data, config)
        }
        
        return(plot)
        
      }, error = function(e) {
        cat("DEBUG: spc_plot - ERROR:", e$message, "\n")
        values$plot_warnings <- c(values$plot_warnings, paste("Fejl ved graf-generering:", e$message))
        values$plot_ready <- FALSE
        return(NULL)
      })
    })
    
    # Render plot
    output$spc_plot <- renderPlot({
      plot <- spc_plot()
      if (is.null(plot)) {
        # Show placeholder when no plot available
        showPlaceholder()
      } else {
        plot
      }
    }, res = 96)
    
    # Plot ready status for conditional panels
    output$plot_ready <- reactive({
      values$plot_ready
    })
    outputOptions(output, "plot_ready", suspendWhenHidden = FALSE)
    
    # Plot information and warnings
    output$plot_info <- renderUI({
      if (length(values$plot_warnings) > 0) {
        div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          strong(" Graf-advarsler:"),
          tags$ul(
            lapply(values$plot_warnings, function(warn) tags$li(warn))
          )
        )
      } else if (values$plot_ready && !is.null(chart_config()) && !is.null(data_reactive())) {
        config <- chart_config()
        data <- data_reactive()
        
        # Safety checks before accessing data
        if (!is.null(data) && is.data.frame(data) && nrow(data) > 0 && !is.null(config$x_col)) {
          tryCatch({
            x_data <- data[[config$x_col]]
            x_min <- if(inherits(x_data, "Date")) {
              format(min(x_data, na.rm = TRUE), "%d-%m-%Y")
            } else {
              "1"
            }
            x_max <- if(inherits(x_data, "Date")) {
              format(max(x_data, na.rm = TRUE), "%d-%m-%Y")
            } else {
              as.character(length(x_data))
            }
            
            div(
              class = "alert alert-success",
              style = "font-size: 0.9rem;",
              icon("check-circle"),
              strong(" Graf genereret succesfuldt! "),
              sprintf("Chart type: %s | Datapunkter: %d | Periode: %s til %s",
                      chart_type_reactive() %||% "unknown",
                      nrow(data),
                      x_min,
                      x_max)
            )
          }, error = function(e) {
            div(
              class = "alert alert-success",
              style = "font-size: 0.9rem;",
              icon("check-circle"),
              strong(" Graf genereret succesfuldt! "),
              sprintf("Chart type: %s | Datapunkter: %d",
                      chart_type_reactive() %||% "unknown",
                      nrow(data))
            )
          })
        } else {
          div(
            class = "alert alert-success",
            style = "font-size: 0.9rem;",
            icon("check-circle"),
            strong(" Graf genereret! ")
          )
        }
      }
    })
    
    # Anhøj rules results section
    output$anhoej_rules_section <- renderUI({
      # Only show for run charts
      if (!is.null(chart_type_reactive()) && chart_type_reactive() == "run" && !is.null(values$anhoej_results)) {
        card(
          card_header(
            div(
              icon("search-plus"),
              " Anhøj Regler (Run Chart)",
              style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 500;")
            )
          ),
          card_body(
            renderAnhoejResults()
          )
        )
      } else {
        return(NULL)
      }
    })
    
    # Function to render Anhøj results
    renderAnhoejResults <- function() {
      req(values$anhoej_results)
      
      results <- values$anhoej_results
      
      div(
        # Runs test
        div(
          style = "margin-bottom: 10px;",
          span(
            class = paste("status-indicator", 
                          if(results$runs_signal) "status-error" else "status-ready"),
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
            class = paste("status-indicator", 
                          if(results$crossings_signal) "status-error" else "status-ready"),
            style = "margin-right: 8px;"
          ),
          strong("Usædvanligt få krydsninger: "),
          if(results$crossings_signal) {
            span("JA - Signal detekteret", style = paste("color:", HOSPITAL_COLORS$danger))
          } else {
            span("NEJ - Ingen signal", style = paste("color:", HOSPITAL_COLORS$success))
          },
          br(),
          span(sprintf("Antal krydsninger: %d (kritisk værdi: %d)", 
                       results$crossings_count, results$crossings_critical_value),
               style = "font-size: 0.85rem; color: #666;")
        ),
        
        # Overall signal
        hr(),
        div(
          style = "text-align: center; padding: 10px; border-radius: 5px;",
          class = if(results$any_signal) "bg-warning" else "bg-success",
          strong(
            if(results$any_signal) {
              "⚠️ SIGNAL: Special cause variation detekteret"
            } else {
              "✅ INGEN SIGNAL: Kun common cause variation"
            },
            style = if(results$any_signal) paste("color:", HOSPITAL_COLORS$danger) else paste("color:", HOSPITAL_COLORS$success)
          )
        )
      )
    }
    
    # Return reactive values for use by other modules
    return(
      list(
        plot = reactive(values$plot_object),
        plot_ready = reactive(values$plot_ready),
        anhoej_results = reactive(values$anhoej_results),
        chart_config = chart_config
      )
    )
  })
}

# Helper function: Detect chart configuration from data
detectChartConfiguration <- function(data, chart_type) {
  
  cat("DEBUG: detectChartConfiguration called\n")
  cat("DEBUG: detectChartConfiguration - chart_type:", chart_type, "\n")
  cat("DEBUG: detectChartConfiguration - column names:", paste(names(data), collapse = ", "), "\n")
  
  # Safety checks
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0 || ncol(data) == 0) {
    cat("DEBUG: detectChartConfiguration - Invalid data\n")
    return(list(
      x_col = NULL,
      y_col = NULL,
      n_col = NULL,
      numeric_cols = character(0),
      chart_type = chart_type,
      error = "Ingen gyldig data tilgængelig"
    ))
  }
  
  # Find potential date/time column
  x_col <- NULL
  for (col_name in names(data)) {
    if (is.null(col_name) || col_name == "") next
    
    col_data <- data[[col_name]]
    if (length(col_data) == 0) next
    
    # Check for date patterns
    char_data <- as.character(col_data)[!is.na(col_data)]
    if (length(char_data) > 0) {
      if (any(grepl("\\d{4}-\\d{2}-\\d{2}|\\d{2}/\\d{2}/\\d{4}|\\d{2}-\\d{2}-\\d{4}", char_data))) {
        x_col <- col_name
        cat("DEBUG: detectChartConfiguration - Found date column:", col_name, "\n")
        break
      }
    }
  }
  
  # If no date column, use first column
  if (is.null(x_col) && length(names(data)) > 0) {
    x_col <- names(data)[1]
    cat("DEBUG: detectChartConfiguration - Using first column as x:", x_col, "\n")
  }
  
  # Find numeric columns
  numeric_cols <- character(0)
  for (col_name in names(data)) {
    if (is.null(col_name) || col_name == "") next
    
    col_data <- data[[col_name]]
    if (length(col_data) == 0) next
    
    # Check if numeric or can be converted to numeric
    if (is.numeric(col_data)) {
      numeric_cols <- c(numeric_cols, col_name)
    } else {
      # Try to convert (handling Danish decimals)
      numeric_test <- suppressWarnings(as.numeric(gsub(",", ".", as.character(col_data))))
      non_na_count <- sum(!is.na(numeric_test))
      if (non_na_count > length(col_data) * 0.8) {
        numeric_cols <- c(numeric_cols, col_name)
      }
    }
  }
  
  cat("DEBUG: detectChartConfiguration - Numeric columns:", paste(numeric_cols, collapse = ", "), "\n")
  
  # Remove x_col from numeric columns if it's there
  if (!is.null(x_col) && x_col %in% numeric_cols) {
    numeric_cols <- numeric_cols[numeric_cols != x_col]
    cat("DEBUG: detectChartConfiguration - Removed x_col from numeric_cols\n")
  }
  
  # Smart detection based on column names and chart type
  y_col <- NULL
  n_col <- NULL
  
  # Check for Danish hospital data patterns
  col_names_lower <- tolower(names(data))
  
  # Look for tæller/nævner patterns (Danish numerator/denominator)
  taeller_idx <- which(grepl("t.ller|tael|num", col_names_lower, ignore.case = TRUE))
  naevner_idx <- which(grepl("n.vner|naev|denom", col_names_lower, ignore.case = TRUE))
  
  if (length(taeller_idx) > 0 && length(naevner_idx) > 0) {
    cat("DEBUG: detectChartConfiguration - Found tæller/nævner pattern\n")
    y_col <- names(data)[taeller_idx[1]]
    n_col <- names(data)[naevner_idx[1]]
    
    # For run charts with tæller/nævner, we need to create a rate column
    if (chart_type == "run") {
      cat("DEBUG: detectChartConfiguration - Run chart with tæller/nævner detected\n")
      cat("DEBUG: detectChartConfiguration - Will use", y_col, "as numerator for rate calculation\n")
      # Note: We'll keep y_col as Taeller for now, but the plotting function should handle this
      # OR we could suggest P-chart instead in the warning
    }
  } else {
    # Standard detection
    if (length(numeric_cols) > 0) {
      y_col <- numeric_cols[1]
      cat("DEBUG: detectChartConfiguration - Using first numeric column as y:", y_col, "\n")
      
      # For proportion charts, try to detect numerator/denominator
      if (chart_type %in% c("p", "pp", "u", "up") && length(numeric_cols) >= 2) {
        potential_n_cols <- numeric_cols[numeric_cols != y_col]
        if (length(potential_n_cols) > 0) {
          n_col <- potential_n_cols[1]
          cat("DEBUG: detectChartConfiguration - Using second numeric column as n:", n_col, "\n")
        }
      }
    }
  }
  
  cat("DEBUG: detectChartConfiguration - Final config: x=", x_col, "y=", y_col, "n=", n_col, "\n")
  
  return(list(
    x_col = x_col,
    y_col = y_col,
    n_col = n_col,
    numeric_cols = numeric_cols,
    chart_type = chart_type
  ))
}

# Helper function: Validate data for chart type
validateDataForChart <- function(data, config, chart_type) {
  warnings <- character(0)
  
  # Check basic data existence
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    warnings <- c(warnings, "Ingen data tilgængelig")
    return(list(valid = FALSE, warnings = warnings))
  }
  
  # Check if required columns exist
  if (is.null(config$y_col) || length(config$y_col) == 0 || config$y_col == "") {
    warnings <- c(warnings, "Ingen numerisk kolonne fundet til Y-akse")
    return(list(valid = FALSE, warnings = warnings))
  }
  
  # Check if y column exists in data
  if (!config$y_col %in% names(data)) {
    warnings <- c(warnings, paste("Kolonne", config$y_col, "findes ikke i data"))
    return(list(valid = FALSE, warnings = warnings))
  }
  
  # Check data requirements for specific chart types
  if (chart_type %in% c("p", "pp", "u", "up")) {
    if (is.null(config$n_col) || length(config$n_col) == 0 || config$n_col == "") {
      warnings <- c(warnings, paste("Chart type", chart_type, "kræver en nævner-kolonne (n)"))
      return(list(valid = FALSE, warnings = warnings))
    }
    if (!config$n_col %in% names(data)) {
      warnings <- c(warnings, paste("Nævner-kolonne", config$n_col, "findes ikke i data"))
      return(list(valid = FALSE, warnings = warnings))
    }
  }
  
  # Special check for run charts with tæller/nævner data
  if (chart_type == "run" && !is.null(config$n_col) && config$n_col %in% names(data)) {
    warnings <- c(warnings, "Run chart med tæller/nævner data - vil beregne rate automatisk. Overvej P-kort for bedre analyse.")
  }
  
  # Check minimum data points
  if (nrow(data) < 8) {
    warnings <- c(warnings, paste("Kun", nrow(data), "datapunkter - SPC analyse er mest pålidelig med mindst 15-20 punkter"))
  }
  
  # Check for missing values
  y_data <- data[[config$y_col]]
  if (all(is.na(y_data))) {
    warnings <- c(warnings, "Alle værdier i Y-kolonnen er tomme")
    return(list(valid = FALSE, warnings = warnings))
  }
  
  missing_pct <- round(sum(is.na(y_data)) / length(y_data) * 100, 1)
  if (missing_pct > 20) {
    warnings <- c(warnings, paste("Høj andel manglende værdier i Y-kolonne:", missing_pct, "%"))
  }
  
  return(list(
    valid = TRUE,  # If we got here, data is usable despite warnings
    warnings = warnings
  ))
}

# Helper function: Generate SPC plot using qicharts2
generateSPCPlot <- function(data, config, chart_type, show_targets = FALSE, show_phases = FALSE) {
  
  cat("DEBUG: generateSPCPlot called\n")
  cat("DEBUG: generateSPCPlot - chart_type:", chart_type, "\n")
  cat("DEBUG: generateSPCPlot - x_col:", config$x_col, "y_col:", config$y_col, "n_col:", config$n_col, "\n")
  
  # Safety checks
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    stop("Ingen gyldig data at visualisere")
  }
  
  if (is.null(config$y_col) || !config$y_col %in% names(data)) {
    stop("Y-kolonne ikke fundet i data")
  }
  
  # Initialize title and label variables
  title_text <- paste("SPC Chart -", config$y_col)
  ylab_text <- config$y_col
  
  # Prepare data based on chart type and data structure
  x_data <- data[[config$x_col]]
  y_data_raw <- data[[config$y_col]]
  
  cat("DEBUG: generateSPCPlot - x_data type:", class(x_data), "\n")
  cat("DEBUG: generateSPCPlot - y_data_raw sample:", paste(head(y_data_raw, 3), collapse = ", "), "\n")
  
  # Special handling for run charts with tæller/nævner data
  if (chart_type == "run" && !is.null(config$n_col) && config$n_col %in% names(data)) {
    cat("DEBUG: generateSPCPlot - Run chart with tæller/nævner - calculating rate\n")
    
    # Calculate rate for run chart
    taeller <- suppressWarnings(as.numeric(gsub(",", ".", as.character(y_data_raw))))
    naevner <- suppressWarnings(as.numeric(gsub(",", ".", as.character(data[[config$n_col]]))))
    
    if (any(is.na(taeller)) || any(is.na(naevner))) {
      stop("Kunne ikke konvertere tæller eller nævner til numeriske værdier")
    }
    
    if (any(naevner == 0)) {
      stop("Nævner kan ikke være 0 - tjek dine data")
    }
    
    # Calculate proportion/rate
    y_data <- (taeller / naevner) * 100  # Convert to percentage
    cat("DEBUG: generateSPCPlot - Calculated rate (%) sample:", paste(head(round(y_data, 2), 3), collapse = ", "), "\n")
    
    # Update title and label
    title_text <- paste("Run Chart - Rate af", config$y_col, "per", config$n_col, "(%)")
    ylab_text <- paste("Rate (", config$y_col, "/", config$n_col, ") %")
    
  } else {
    # Standard numeric conversion
    y_data <- suppressWarnings(as.numeric(gsub(",", ".", as.character(y_data_raw))))
    cat("DEBUG: generateSPCPlot - y_data after conversion sample:", paste(head(y_data, 3), collapse = ", "), "\n")
    
    # Standard labels
    title_text <- paste("SPC Chart -", config$y_col)
    ylab_text <- config$y_col
  }
  
  # Check if conversion was successful
  if (all(is.na(y_data))) {
    stop(paste("Kunne ikke konvertere", config$y_col, "til numeriske værdier"))
  }
  
  # Convert x_data to date if possible and needed
  if (!is.null(x_data) && !inherits(x_data, "Date")) {
    cat("DEBUG: generateSPCPlot - attempting date conversion\n")
    x_data <- tryCatch({
      as.Date(x_data)
    }, error = function(e) {
      cat("DEBUG: generateSPCPlot - date conversion failed, using sequence\n")
      # If date conversion fails, use sequence numbers
      1:length(y_data)
    })
  } else if (is.null(x_data)) {
    cat("DEBUG: generateSPCPlot - x_data is NULL, using sequence\n")
    x_data <- 1:length(y_data)
  }
  
  cat("DEBUG: generateSPCPlot - x_data final type:", class(x_data), "\n")
  
  # Prepare arguments for qic()
  qic_args <- list(
    x = x_data,
    y = y_data,
    chart = chart_type,
    title = title_text,
    ylab = ylab_text,
    xlab = if(inherits(x_data, "Date")) "Dato" else "Observation"
  )
  
  cat("DEBUG: generateSPCPlot - basic qic_args prepared\n")
  
  # Add denominator for proportion/rate charts
  if (!is.null(config$n_col) && config$n_col %in% names(data) && chart_type %in% c("p", "pp", "u", "up")) {
    cat("DEBUG: generateSPCPlot - adding denominator for", chart_type, "chart\n")
    n_data_raw <- data[[config$n_col]]
    n_data <- suppressWarnings(as.numeric(gsub(",", ".", as.character(n_data_raw))))
    
    # Only add if conversion successful
    if (!all(is.na(n_data))) {
      qic_args$n <- n_data
      cat("DEBUG: generateSPCPlot - denominator added successfully\n")
    } else {
      cat("DEBUG: generateSPCPlot - WARNING: could not convert denominator to numeric\n")
    }
  }
  
  cat("DEBUG: generateSPCPlot - calling qicharts2::qic\n")
  cat("DEBUG: generateSPCPlot - qic_args:", paste(names(qic_args), collapse = ", "), "\n")
  
  # Generate plot
  plot <- do.call(qicharts2::qic, qic_args)
  
  cat("DEBUG: generateSPCPlot - qic() completed successfully\n")
  
  return(plot)
}

# Helper function: Apply hospital theme
applyHospitalTheme <- function(plot) {
  plot + 
    HOSPITAL_THEME() +
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
}

# Helper function: Calculate Anhøj rules for run charts
calculateAnhoejRules <- function(data, config) {
  
  # Safety checks
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    return(list(
      runs_signal = FALSE,
      crossings_signal = FALSE,
      any_signal = FALSE,
      message = "Ingen data tilgængelig til Anhøj-analyse"
    ))
  }
  
  if (is.null(config$y_col) || !config$y_col %in% names(data)) {
    return(list(
      runs_signal = FALSE,
      crossings_signal = FALSE,
      any_signal = FALSE,
      message = "Y-kolonne ikke fundet i data"
    ))
  }
  
  # Get Y data and convert to numeric - handle tæller/nævner for run charts
  if (!is.null(config$n_col) && config$n_col %in% names(data)) {
    cat("DEBUG: calculateAnhoejRules - calculating rate from tæller/nævner\n")
    
    # Calculate rate for Anhøj rules analysis
    taeller <- suppressWarnings(as.numeric(gsub(",", ".", as.character(data[[config$y_col]]))))
    naevner <- suppressWarnings(as.numeric(gsub(",", ".", as.character(data[[config$n_col]]))))
    
    if (any(is.na(taeller)) || any(is.na(naevner)) || any(naevner == 0)) {
      return(list(
        runs_signal = FALSE,
        crossings_signal = FALSE,
        any_signal = FALSE,
        message = "Kunne ikke beregne rate fra tæller/nævner data"
      ))
    }
    
    y_data <- (taeller / naevner) * 100  # Percentage rate
  } else {
    # Standard numeric conversion
    y_data_raw <- data[[config$y_col]]
    y_data <- suppressWarnings(as.numeric(gsub(",", ".", as.character(y_data_raw))))
  }
  
  # Remove missing values
  y_clean <- y_data[!is.na(y_data)]
  n <- length(y_clean)
  
  if (n < 8) {
    return(list(
      runs_signal = FALSE,
      crossings_signal = FALSE,
      any_signal = FALSE,
      message = "For få datapunkter til Anhøj-analyse (minimum 8 påkrævet)",
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
  
  # Calculate runs (consecutive points on same side of median)
  above_median <- y_no_median > median_val
  runs <- rle(above_median)
  longest_run <- max(runs$lengths)
  
  # Calculate crossings (median line crossings)
  crossings <- sum(diff(above_median) != 0)
  
  # Critical values (simplified Anhøj rules)
  runs_critical_value <- ceiling(log2(n_useful)) + 3
  crossings_critical_value <- max(1, floor(n_useful / 2) - 2)  # Ensure at least 1
  
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
}

# Helper function: Show placeholder
showPlaceholder <- function() {
  ggplot() + 
    annotate("text", x = 0.5, y = 0.5, label = "Venter på data eller juster indstillinger", 
             size = 6, color = HOSPITAL_COLORS$secondary) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = HOSPITAL_COLORS$light, color = NA)
    )
}