# R/modules/visualization_module.R
library(shiny)
library(qicharts2)
library(ggplot2)
library(dplyr)
library(scales)

# Visualization Module UI
visualizationModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Plot container - always visible
    div(
      id = ns("plot_container"),
      style = "position: relative; min-height: 500px;",
      
      # Dynamic content that switches between plot and placeholder
      uiOutput(ns("dynamic_content"))
    ),
    
    br(),
    
    # Plot information and warnings
    uiOutput(ns("plot_info")),
    
    # Anhøj rules results for run charts
    uiOutput(ns("anhoej_rules_section"))
  )
}

# Visualization Module Server
visualizationModuleServer <- function(id, data_reactive, column_config_reactive, chart_type_reactive, show_targets_reactive, show_phases_reactive, chart_title_reactive = NULL) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for state management
    values <- reactiveValues(
      plot_object = NULL,
      plot_ready = FALSE,
      anhoej_results = NULL,
      plot_warnings = character(0),
      is_computing = FALSE
    )
    
    chart_config <- reactive({
      # Don't use req() here - let it run with available data/config
      data <- data_reactive()
      config <- column_config_reactive()
      chart_type <- chart_type_reactive() %||% "run"
      
      if (is.null(data) || is.null(config)) {
        return(NULL)
      }
      
      # Validate columns exist in data - corrected boolean logic
      if (!is.null(config$x_col) && !(config$x_col %in% names(data))) {
        config$x_col <- NULL
      }
      if (!is.null(config$y_col) && !(config$y_col %in% names(data))) {
        config$y_col <- NULL
      }
      if (!is.null(config$n_col) && !(config$n_col %in% names(data))) {
        config$n_col <- NULL
      }
      
      # Auto-detect if needed
      if (is.null(config$x_col) || is.null(config$y_col)) {
        auto_config <- detectChartConfiguration(data, chart_type)
        if (is.null(config$x_col)) config$x_col <- auto_config$x_col
        if (is.null(config$y_col)) config$y_col <- auto_config$y_col
        if (is.null(config$n_col)) config$n_col <- auto_config$n_col
      }
      
      return(list(
        x_col = config$x_col,
        y_col = config$y_col,
        n_col = config$n_col,
        chart_type = chart_type
      ))
    })
    
    # Main plot generation reactive - FIXED: Better error handling and state management
    spc_plot <- reactive({
      # Don't use req() - check manually
      data <- data_reactive()
      config <- chart_config()
      
      # IMPORTANT: Reset computing state at the start
      values$is_computing <- FALSE
      
      if (is.null(data) || is.null(config)) {
        values$plot_ready <- FALSE
        values$plot_warnings <- character(0)
        return(NULL)
      }
      
      # Set computing flag ONLY when we actually start processing
      values$is_computing <- TRUE
      
      # CRITICAL: Ensure we reset the flag no matter what happens
      on.exit({
        values$is_computing <- FALSE
      }, add = TRUE)
      
      # Reset plot ready at start
      values$plot_ready <- FALSE
      
      chart_type <- chart_type_reactive() %||% "run"
      
      # Validate data
      validation <- validateDataForChart(data, config, chart_type)
      
      if (!validation$valid) {
        values$plot_warnings <- validation$warnings
        values$plot_ready <- FALSE
        return(NULL)
      }
      
      # Clear warnings if validation passes
      values$plot_warnings <- character(0)
      
      # Generate plot
      tryCatch({
        plot <- generateSPCPlot(
          data = data, 
          config = config, 
          chart_type = chart_type,
          show_targets = show_targets_reactive(),
          show_phases = show_phases_reactive(),
          chart_title_reactive = chart_title_reactive
        )
        
        # Apply hospital theme
        plot <- applyHospitalTheme(plot)
        
        values$plot_object <- plot
        values$plot_ready <- TRUE
        
        # Calculate Anhøj rules for run charts
        if (chart_type == "run") {
          values$anhoej_results <- calculateAnhoejRules(data, config)
        } else {
          values$anhoej_results <- NULL
        }
        
        return(plot)
        
      }, error = function(e) {
        cat("ERROR in spc_plot reactive:", e$message, "\n")
        values$plot_warnings <- c("Graf-generering fejlede:", e$message)
        values$plot_ready <- FALSE
        return(NULL)
      })
    })
    
    # Dynamic content output - FIXED: Always show plotOutput, handle empty state in renderPlot
    output$dynamic_content <- renderUI({
      # Always show the plotOutput - let renderPlot handle the logic
      plotOutput(ns("spc_plot"), height = 500, width = "100%")
    })
    
    # Render plot - FIXED: Handle all states properly
    output$spc_plot <- renderPlot({
      # Check if we have data first
      data <- data_reactive()
      config <- chart_config()
      
      if (is.null(data) || is.null(config) || is.null(config$y_col)) {
        # Show informative placeholder when no data/config
        return(ggplot() + 
          xlim(0, 1) + ylim(0, 1) +
          annotate(
            "text", x = 0.5, y = 0.5,
            label = "Indlæs eller indtast data og vælg kolonner for at se grafen.",
            size = 5,
            color = HOSPITAL_COLORS$secondary
          ) +
          theme_void() +
          theme(
            plot.background = element_rect(fill = HOSPITAL_COLORS$light, color = NA),
            plot.margin = margin(20, 20, 20, 20)
          ))
      }
      
      # Try to generate plot
      plot <- spc_plot()
      
      if (is.null(plot)) {
        # Show error/warning placeholder
        warning_text <- if (length(values$plot_warnings) > 0) {
          paste(values$plot_warnings, collapse = "\n")
        } else {
          "Kunne ikke generere graf - tjek data og indstillinger"
        }
        
        return(ggplot() + 
          xlim(0, 1) + ylim(0, 1) +
          annotate(
            "text", x = 0.5, y = 0.5,
            label = warning_text,
            size = 4,
            color = HOSPITAL_COLORS$danger
          ) +
          theme_void() +
          theme(
            plot.background = element_rect(fill = HOSPITAL_COLORS$light, color = NA),
            plot.margin = margin(20, 20, 20, 20)
          ))
      }
      
      if (inherits(plot, "ggplot")) {
        return(plot)
      } else {
        return(showPlaceholder())
      }
    }, height = 500, width = 800, res = 96)
    
    # Loading indicator - DISABLED to prevent overlay issues
    # output$show_loading <- reactive({
    #   result <- values$is_computing && !is.null(data_reactive())
    #   return(result)
    # })
    # outputOptions(output, "show_loading", suspendWhenHidden = FALSE)
    
    # Plot ready status
    output$plot_ready <- reactive({
      values$plot_ready
    })
    outputOptions(output, "plot_ready", suspendWhenHidden = FALSE)
    
    # Plot info and warnings
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
      } else if (values$plot_ready && !is.null(data_reactive())) {
        div(
          class = "alert alert-success",
          style = "font-size: 0.9rem;",
          icon("check-circle"),
          strong(" Graf genereret succesfuldt! "),
          sprintf("Chart type: %s | Datapunkter: %d", 
                  chart_type_reactive() %||% "unknown", 
                  nrow(data_reactive()))
        )
      }
    })
    
    # Anhøj rules section
    output$anhoej_rules_section <- renderUI({
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
            renderAnhoejResults(values$anhoej_results)
          )
        )
      } else {
        return(NULL)
      }
    })
    
    # Return reactive values
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

# Helper function: Detect chart configuration
detectChartConfiguration <- function(data, chart_type) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0 || ncol(data) == 0) {
    return(list(x_col = NULL, y_col = NULL, n_col = NULL, chart_type = chart_type))
  }
  
  # Find date/time column
  x_col <- NULL
  for (col_name in names(data)) {
    col_data <- data[[col_name]]
    char_data <- as.character(col_data)[!is.na(col_data)]
    if (length(char_data) > 0 && any(grepl("\\d{4}-\\d{2}-\\d{2}|\\d{2}/\\d{2}/\\d{4}", char_data))) {
      x_col <- col_name
      break
    }
  }
  
  # Use first column if no date found
  if (is.null(x_col) && length(names(data)) > 0) {
    x_col <- names(data)[1]
  }
  
  # Find numeric columns
  numeric_cols <- character(0)
  for (col_name in names(data)) {
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
  
  # Remove x_col from numeric columns
  if (!is.null(x_col) && x_col %in% numeric_cols) {
    numeric_cols <- numeric_cols[numeric_cols != x_col]
  }
  
  # Detect y and n columns
  y_col <- NULL
  n_col <- NULL
  
  # Look for Danish tæller/nævner patterns
  col_names_lower <- tolower(names(data))
  taeller_idx <- which(grepl("t.ller|tael|num", col_names_lower))
  naevner_idx <- which(grepl("n.vner|naev|denom", col_names_lower))
  
  if (length(taeller_idx) > 0 && length(naevner_idx) > 0) {
    y_col <- names(data)[taeller_idx[1]]
    n_col <- names(data)[naevner_idx[1]]
  } else if (length(numeric_cols) > 0) {
    y_col <- numeric_cols[1]
    if (chart_type %in% c("p", "pp", "u", "up") && length(numeric_cols) >= 2) {
      potential_n_cols <- numeric_cols[numeric_cols != y_col]
      if (length(potential_n_cols) > 0) {
        n_col <- potential_n_cols[1]
      }
    }
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
  
  if (nrow(data) < 8) {
    warnings <- c(warnings, paste("Kun", nrow(data), "datapunkter - SPC analyse er mest pålidelig med mindst 15-20 punkter"))
  }
  
  return(list(valid = TRUE, warnings = warnings))
}

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
    if (!is.null(chart_title_reactive)) {
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
  x_data <- data[[config$x_col]]
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
      if (is.null(custom_title)) {
        title_text <- paste("Run Chart - Rate af", config$y_col, "per", config$n_col, "(%)")
      }
    } else if (chart_type %in% c("p", "pp", "u", "up")) {
      y_data <- taeller
      n_data <- naevner
      ylab_text <- if (chart_type %in% c("p", "pp")) "Proportion" else "Rate"
      if (is.null(custom_title)) {
        chart_name <- if (chart_type %in% c("p", "pp")) "P Chart" else "U Chart"
        title_text <- paste(chart_name, "-", config$y_col, "af", config$n_col)
      }
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
  if (is.null(x_data) || !inherits(x_data, "Date")) {
    x_data <- 1:length(y_data)
    xlab_text <- "Observation"
  } else {
    # Convert dates to numeric for qicharts2 compatibility
    x_data <- 1:length(y_data)
    xlab_text <- "Observation (tidsserie)"
  }
  
  # Prepare qic arguments
  qic_args <- list(
    x = x_data,
    y = y_data,
    chart = chart_type,
    title = title_text,
    ylab = ylab_text,
    xlab = xlab_text
  )
  
  # Add denominator for P/U charts
  if (chart_type %in% c("p", "pp", "u", "up") && exists("n_data")) {
    qic_args$n <- n_data
  }
  
  # Clean data
  complete_cases <- complete.cases(qic_args$x, qic_args$y)
  if (!all(complete_cases)) {
    qic_args$x <- qic_args$x[complete_cases]
    qic_args$y <- qic_args$y[complete_cases]
    if ("n" %in% names(qic_args)) {
      qic_args$n <- qic_args$n[complete_cases]
    }
  }
  
  if (length(qic_args$y) < 3) {
    stop("For få datapunkter efter rensning (minimum 3 påkrævet)")
  }
  
  # Generate plot with qicharts2
  tryCatch({
    if (chart_type %in% c("p", "pp", "u", "up")) {
      plot <- qicharts2::qic(
        x = qic_args$x,
        y = qic_args$y,
        n = qic_args$n,
        chart = chart_type,
        title = qic_args$title,
        ylab = qic_args$ylab,
        xlab = qic_args$xlab
      )
    } else {
      plot <- qicharts2::qic(
        x = qic_args$x,
        y = qic_args$y,
        chart = chart_type,
        title = qic_args$title,
        ylab = qic_args$ylab,
        xlab = qic_args$xlab
      )
    }
  }, error = function(e) {
    # Fallback to basic ggplot
    plot_data <- data.frame(x = qic_args$x, y = qic_args$y)
    plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::geom_hline(yintercept = median(qic_args$y, na.rm = TRUE), color = "red", linetype = "dashed") +
      ggplot2::labs(title = qic_args$title, x = qic_args$xlab, y = qic_args$ylab) +
      ggplot2::theme_minimal()
  })
  
  return(plot)
}

# Helper function: Apply hospital theme
applyHospitalTheme <- function(plot) {
  tryCatch({
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
  }, error = function(e) {
    return(plot)
  })
}

# Helper function: Calculate Anhøj rules
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
  
  # Get Y data - handle tæller/nævner for run charts
  if (!is.null(config$n_col) && config$n_col %in% names(data)) {
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
    
    y_data <- (taeller / naevner) * 100
  } else {
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
}

# Helper function: Render Anhøj results with guard for edge cases
renderAnhoejResults <- function(results) {
  # Guard against incomplete results
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

# Helper function: Show placeholder with improved visibility
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