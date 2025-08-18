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
      condition = paste0("input['", ns("chart_type"), "'] == 'run'"),
      card(
        card_header(
          div(
            icon("search-plus"),
            " Anhøj Regler (Run Chart)",
            style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 500;")
          )
        ),
        card_body(
          uiOutput(ns("anhoej_results"))
        )
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
      
      data <- data_reactive()
      chart_type <- chart_type_reactive() %||% "run"
      
      # Auto-detect appropriate columns
      config <- detectChartConfiguration(data, chart_type)
      
      return(config)
    })
    
    # Generate SPC plot
    spc_plot <- reactive({
      req(data_reactive(), chart_config())
      
      data <- data_reactive()
      config <- chart_config()
      chart_type <- chart_type_reactive() %||% "run"
      
      # Validate data for selected chart type
      validation <- validateDataForChart(data, config, chart_type)
      
      if (!validation$valid) {
        values$plot_warnings <- validation$warnings
        values$plot_ready <- FALSE
        return(NULL)
      }
      
      # Clear warnings if validation passes
      values$plot_warnings <- character(0)
      
      tryCatch({
        # Generate plot using qicharts2
        plot <- generateSPCPlot(data, config, chart_type, 
                                show_targets = show_targets_reactive(),
                                show_phases = show_phases_reactive())
        
        # Apply hospital branding
        plot <- applyHospitalTheme(plot)
        
        values$plot_object <- plot
        values$plot_ready <- TRUE
        
        # Calculate Anhøj rules for run charts
        if (chart_type == "run") {
          values$anhoej_results <- calculateAnhoejRules(data, config)
        }
        
        return(plot)
        
      }, error = function(e) {
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
      } else if (values$plot_ready) {
        req(chart_config())
        config <- chart_config()
        data <- data_reactive()
        
        div(
          class = "alert alert-success",
          style = "font-size: 0.9rem;",
          icon("check-circle"),
          strong(" Graf genereret succesfuldt! "),
          sprintf("Chart type: %s | Datapunkter: %d | Tidsperiode: %s til %s",
                  chart_type_reactive(),
                  nrow(data),
                  format(min(data[[config$x_col]], na.rm = TRUE), "%d-%m-%Y"),
                  format(max(data[[config$x_col]], na.rm = TRUE), "%d-%m-%Y"))
        )
      }
    })
    
    # Anhøj rules results
    output$anhoej_results <- renderUI({
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
    })
    
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
  
  # Find potential date/time column
  x_col <- NULL
  for (col_name in names(data)) {
    col_data <- data[[col_name]]
    if (any(grepl("\\d{4}-\\d{2}-\\d{2}|\\d{2}/\\d{2}/\\d{4}|\\d{2}-\\d{2}-\\d{4}", 
                  as.character(col_data)[!is.na(col_data)]))) {
      x_col <- col_name
      break
    }
  }
  
  # If no date column, use row numbers or first column
  if (is.null(x_col)) {
    x_col <- names(data)[1]
  }
  
  # Find numeric columns
  numeric_cols <- names(data)[sapply(data, function(x) {
    is.numeric(x) || sum(!is.na(suppressWarnings(as.numeric(gsub(",", ".", as.character(x)))))) > length(x) * 0.8
  })]
  
  # Remove x_col from numeric columns if it's there
  numeric_cols <- numeric_cols[numeric_cols != x_col]
  
  # Detect appropriate y column based on chart type
  y_col <- if(length(numeric_cols) > 0) numeric_cols[1] else NULL
  
  # For proportion charts, try to detect numerator/denominator
  n_col <- NULL
  if (chart_type %in% c("p", "pp", "u", "up") && length(numeric_cols) >= 2) {
    # Look for columns that might be denominators (larger values, or named appropriately)
    potential_n_cols <- numeric_cols[numeric_cols != y_col]
    if (length(potential_n_cols) > 0) {
      n_col <- potential_n_cols[1]
    }
  }
  
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
  
  # Check if required columns exist
  if (is.null(config$y_col)) {
    warnings <- c(warnings, "Ingen numerisk kolonne fundet til Y-akse")
    return(list(valid = FALSE, warnings = warnings))
  }
  
  # Check data requirements for specific chart types
  if (chart_type %in% c("p", "pp", "u", "up")) {
    if (is.null(config$n_col)) {
      warnings <- c(warnings, paste("Chart type", chart_type, "kræver en nævner-kolonne (n)"))
    }
  }
  
  # Check minimum data points
  if (nrow(data) < 8) {
    warnings <- c(warnings, paste("Kun", nrow(data), "datapunkter - SPC analyse er mest pålidelig med mindst 15-20 punkter"))
  }
  
  # Check for missing values
  y_data <- data[[config$y_col]]
  missing_pct <- round(sum(is.na(y_data)) / length(y_data) * 100, 1)
  if (missing_pct > 20) {
    warnings <- c(warnings, paste("Høj andel manglende værdier i Y-kolonne:", missing_pct, "%"))
  }
  
  return(list(
    valid = length(warnings) == 0 || all(!grepl("kræver|Ingen", warnings)), # Allow warnings but not errors
    warnings = warnings
  ))
}

# Helper function: Generate SPC plot using qicharts2
generateSPCPlot <- function(data, config, chart_type, show_targets = FALSE, show_phases = FALSE) {
  
  # Prepare data
  x_data <- data[[config$x_col]]
  y_data <- as.numeric(gsub(",", ".", as.character(data[[config$y_col]])))
  
  # Convert x_data to date if possible
  if (!inherits(x_data, "Date")) {
    x_data <- tryCatch({
      as.Date(x_data)
    }, error = function(e) {
      1:length(y_data)  # Use sequence numbers if date conversion fails
    })
  }
  
  # Prepare arguments for qic()
  qic_args <- list(
    x = x_data,
    y = y_data,
    chart = chart_type,
    title = paste("SPC Chart -", config$y_col),
    ylab = config$y_col,
    xlab = if(inherits(x_data, "Date")) "Dato" else "Observation"
  )
  
  # Add denominator for proportion/rate charts
  if (!is.null(config$n_col) && chart_type %in% c("p", "pp", "u", "up")) {
    n_data <- as.numeric(gsub(",", ".", as.character(data[[config$n_col]])))
    qic_args$n <- n_data
  }
  
  # Generate plot
  plot <- do.call(qicharts2::qic, qic_args)
  
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
  
  y_data <- as.numeric(gsub(",", ".", as.character(data[[config$y_col]])))
  
  # Remove missing values
  y_clean <- y_data[!is.na(y_data)]
  n <- length(y_clean)
  
  if (n < 8) {
    return(list(
      runs_signal = FALSE,
      crossings_signal = FALSE,
      any_signal = FALSE,
      message = "For få datapunkter til Anhøj-analyse (minimum 8 påkrævet)"
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
      message = "For mange punkter på median til pålidelig analyse"
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
  crossings_critical_value <- floor(n_useful / 2) - 2
  
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