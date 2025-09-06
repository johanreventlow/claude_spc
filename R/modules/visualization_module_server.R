# R/modules/visualization_module_server.R
# Server logic for the visualization module

library(shiny)
library(bslib)
library(qicharts2)
library(ggplot2)
library(dplyr)
library(scales)

# Visualization Module Server
visualizationModuleServer <- function(id, data_reactive, column_config_reactive, chart_type_reactive, target_value_reactive, skift_config_reactive, chart_title_reactive = NULL) {
  
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
    
    waiter_plot <- waiter::Waiter$new(
      id = ns("plot_container"),
      html = WAITER_CONFIG$plot_generation$html,
      color = WAITER_CONFIG$plot_generation$color
    )
    
    chart_config <- reactive({
      data <- data_reactive()
      config <- column_config_reactive()
      chart_type <- if(is.null(chart_type_reactive())) "run" else chart_type_reactive()
      
      if (is.null(data) || is.null(config)) {
        return(NULL)
      }
      
      # Validate columns exist in data
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
    
    # Main plot generation reactive
    spc_plot <- reactive({
      data <- data_reactive()
      config <- chart_config()
      
      values$is_computing <- FALSE
      
      # Reset Anhøj results when no data or invalid config
      if (is.null(data) || is.null(config)) {
        values$plot_ready <- FALSE
        values$plot_warnings <- character(0)
        values$anhoej_results <- NULL  # CLEAR Anhøj results when no data
        return(NULL)
      }
      
      
      waiter_plot$show()
      
      on.exit({
        waiter_plot$hide()
        values$is_computing <- FALSE
      }, add = TRUE)
      
      values$is_computing <- TRUE
      values$plot_ready <- FALSE
      
      chart_type <- if(is.null(chart_type_reactive())) "run" else chart_type_reactive()
      
      # Validate data
      validation <- validateDataForChart(data, config, chart_type)
      
      if (!validation$valid) {
        values$plot_warnings <- validation$warnings
        values$plot_ready <- FALSE
        values$anhoej_results <- NULL  # CLEAR Anhøj results when validation fails
        return(NULL)
      }
      
      values$plot_warnings <- character(0)
      
      # Generate plot
      tryCatch({
        # Get phase configuration
        skift_config <- skift_config_reactive()
        
        spc_result <- generateSPCPlot(
          data = data, 
          config = config, 
          chart_type = chart_type,
          target_value = target_value_reactive(),
          show_phases = skift_config$show_phases,
          skift_column = skift_config$skift_column,
          chart_title_reactive = chart_title_reactive
        )
        
        plot <- applyHospitalTheme(spc_result$plot)
        qic_data <- spc_result$qic_data
        
        values$plot_object <- plot
        values$plot_ready <- TRUE
        
        # Extract Anhøj rules from qic() results (built-in analysis)
        if (chart_type == "run" && !is.null(qic_data)) {
          cat("DEBUG: Extracting Anhøj rules from qic() results\n")
          values$anhoej_results <- list(
            runs_signal = any(qic_data$runs.signal, na.rm = TRUE),
            crossings_signal = FALSE, # qic doesn't provide crossings signal directly  
            any_signal = any(qic_data$sigma.signal, na.rm = TRUE),
            longest_run = max(qic_data$longest.run, na.rm = TRUE),
            longest_run_max = max(qic_data$longest.run.max, na.rm = TRUE),
            n_crossings = max(qic_data$n.crossings, na.rm = TRUE),
            n_crossings_min = max(qic_data$n.crossings.min, na.rm = TRUE),
            message = if(any(qic_data$sigma.signal, na.rm = TRUE)) "Særlig årsag detekteret" else "Ingen særlige årsager fundet"
          )
          cat("DEBUG: Anhøj results extracted from qic data\n")
        } else {
          values$anhoej_results <- NULL
        }
        
        return(plot)
        
      }, error = function(e) {
        cat("ERROR in spc_plot reactive:", e$message, "\n")
        values$plot_warnings <- c("Graf-generering fejlede:", e$message)
        values$plot_ready <- FALSE
        values$anhoej_results <- NULL  # CLEAR Anhøj results on error
        return(NULL)
      })
    })
    
    # Dynamic content output
    output$dynamic_content <- renderUI({
      div(
        style = "width: 100%; height: 100%;",
        # This will contain either the plot or a message
        uiOutput(ns("plot_or_message"))
      )
    })
    
    # New unified output that handles both messages and plots
    output$plot_or_message <- renderUI({
      data <- data_reactive()
      config <- chart_config()
      
      # Smart message logic based on app state
      if (is.null(data) || nrow(data) == 0) {
        return(createPlotMessage("welcome"))
      }
      
      # Check if user has meaningful data vs empty session template
      # Exclude the "Skift" column which is always FALSE in new sessions
      data_without_skift <- data[, !names(data) %in% "Skift", drop = FALSE]
      has_meaningful_data <- any(!is.na(data_without_skift), na.rm = TRUE) && 
                            !all(sapply(data_without_skift, function(col) all(is.na(col))))
      
      # If table is completely empty (all NA except Skift column), show welcome message
      # This means user hasn't started entering data yet
      if (!has_meaningful_data) {
        return(createPlotMessage("welcome"))
      }
      
      if (is.null(config) || is.null(config$y_col)) {
        return(createPlotMessage("config_needed"))
      }
      
      plot <- spc_plot()
      
      if (is.null(plot)) {
        # Determine specific error type
        if (length(values$plot_warnings) > 0) {
          warning_details <- paste(values$plot_warnings, collapse = " • ")
          
          # Check for insufficient data
          if (any(grepl("datapunkter", values$plot_warnings, ignore.case = TRUE)) ||
              any(grepl("points", values$plot_warnings, ignore.case = TRUE))) {
            return(createPlotMessage("insufficient_data", details = warning_details))
          }
          
          # Check for validation errors
          if (any(grepl("kolonne|column|påkrævet|required", values$plot_warnings, ignore.case = TRUE))) {
            return(createPlotMessage("validation_error", details = warning_details))
          }
          
          # Generic validation error
          return(createPlotMessage("validation_error", details = warning_details))
        } else {
          return(createPlotMessage("technical_error"))
        }
      }
      
      if (inherits(plot, "ggplot")) {
        # Return the actual plot wrapped in a plotOutput
        return(
          plotOutput(ns("spc_plot_actual"), width = "100%", height = "100%")
        )
      } else {
        return(createPlotMessage("technical_error"))
      }
    })
    
    # Separate renderPlot for the actual SPC plot
    output$spc_plot_actual <- renderPlot({
      plot <- spc_plot()
      if (inherits(plot, "ggplot")) {
        return(plot)
      } else {
        return(NULL)  # This shouldn't happen due to logic above
      }
    }, res = 96)
    
    # Plot ready status
    output$plot_ready <- reactive({
      values$plot_ready
    })
    outputOptions(output, "plot_ready", suspendWhenHidden = FALSE)
    
    # Plot info and warnings - ALWAYS SHOW
    output$plot_info <- renderUI({
      data <- data_reactive()
      # Always show plot info - removed hide_anhoej_rules condition
      
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
    
    
    # Data summary value box (more SPC-relevant) - ALWAYS VISIBLE
    output$plot_status_boxes <- renderUI({
      data <- data_reactive()
      # Always show data summary - removed hide_anhoej_rules condition
      
      if (values$plot_ready && !is.null(data)) {
        config <- chart_config()
        data_count <- nrow(data)
        chart_name <- switch(chart_type_reactive() %||% "run",
          "run" = "Run Chart",
          "p" = "P-kort", 
          "u" = "U-kort",
          "i" = "I-kort",
          "mr" = "MR-kort",
          chart_type_reactive() %||% "Unknown"
        )
        
        value_box(
          title = "Data Overblik",
          value = paste(data_count, "punkter"),
          showcase = icon("chart-line"),
          theme = if(data_count >= 15) "info" else "warning",
          height = "120px",
          p(class = "fs-7 text-muted mb-0", paste(chart_name, if(data_count < 15) "| Få datapunkter" else "| Tilstrækkelig data"))
        )
      } else {
        # Default state
        value_box(
          title = "Data Status",
          value = "Ingen data",
          showcase = icon("database"),
          theme = "secondary",
          height = "120px",
          p(class = "fs-7 text-muted mb-0", "Upload eller indtast data")
        )
      }
    })
    
    # NEW: Data summary value box for error checking
    output$data_summary_box <- renderUI({
      data <- data_reactive()
      config <- chart_config()
      
      if (is.null(data) || nrow(data) == 0) {
        return(
          value_box(
            title = "Data Summary",
            value = "Ingen data",
            showcase = icon("table"),
            theme = "light",
            height = "120px",
            p(class = "fs-7 text-muted mb-0", "Vent på data load")
          )
        )
      }
      
      # Generate summary info
      total_rows <- nrow(data)
      total_cols <- ncol(data)
      
      # Check for configured columns
      summary_text <- ""
      if (!is.null(config)) {
        if (!is.null(config$y_col) && config$y_col %in% names(data)) {
          y_data <- data[[config$y_col]]
          valid_y <- sum(!is.na(y_data))
          summary_text <- paste0("Y: ", valid_y, "/", total_rows, " gyldige")
          
          if (!is.null(config$n_col) && config$n_col %in% names(data)) {
            n_data <- data[[config$n_col]]  
            valid_n <- sum(!is.na(n_data))
            zeros_n <- sum(n_data == 0, na.rm = TRUE)
            summary_text <- paste0(summary_text, " | N: ", valid_n, "/", total_rows)
            if (zeros_n > 0) summary_text <- paste0(summary_text, " (", zeros_n, " nuller)")
          }
        } else {
          summary_text <- "Kolonner ikke konfigureret"
        }
      } else {
        summary_text <- "Config ikke klar"
      }
      
      value_box(
        title = "Data Summary",
        value = paste0(total_rows, "×", total_cols),
        showcase = icon("table"),
        theme = "light",
        height = "120px",
        p(class = "fs-7 text-muted mb-0", summary_text)
      )
    })
    
    # NEW: Anhøj rules as value boxes - ALWAYS VISIBLE
    output$anhoej_rules_boxes <- renderUI({
      data <- data_reactive()
      # Always show value boxes - removed hide_anhoej_rules condition
      
      # Smart content based on current status - ALWAYS show boxes
      config <- chart_config()
      chart_type <- chart_type_reactive() %||% "run"
      anhoej <- values$anhoej_results
      
      
      # Simplified logic - if we have data with meaningful content, we're good to go
      has_meaningful_data <- !is.null(data) && nrow(data) > 0 && 
        any(sapply(data, function(x) {
          if (is.logical(x)) return(any(x, na.rm = TRUE))
          if (is.numeric(x)) return(any(!is.na(x)))
          if (is.character(x)) return(any(nzchar(x, keepNA = FALSE), na.rm = TRUE))
          return(FALSE)
        }))
      
      # Determine current status and appropriate content
      status_info <- if (!has_meaningful_data) {
        list(
          status = "no_data",
          message = "Upload data eller start ny session",
          theme = "secondary"
        )
      } else if (is.null(config) || is.null(config$y_col)) {
        list(
          status = "not_configured",
          message = "Vælg kolonner i indstillinger",
          theme = "warning"
        )
      } else if (chart_type != "run") {
        list(
          status = "not_run_chart",
          message = "Kun relevant for run charts", 
          theme = "light"
        )
      } else {
        # Check if we have enough meaningful data
        meaningful_count <- if (!is.null(config$n_col) && config$n_col %in% names(data)) {
          sum(!is.na(data[[config$y_col]]) & !is.na(data[[config$n_col]]) & data[[config$n_col]] > 0)
        } else {
          sum(!is.na(data[[config$y_col]]))
        }
        
        if (meaningful_count < 10) {
          list(
            status = "insufficient_data",
            message = "Mindst 10 datapunkter nødvendigt",
            theme = "warning"
          )
        } else if (!(values$plot_ready %||% FALSE)) {
          list(
            status = "processing",
            message = "Behandler data og beregner...",
            theme = "info"
          )
        } else {
          
          list(
            status = "ready",
            message = "SPC analyse klar",
            theme = "success"
          )
        }
      }
      
      # Always return the two key boxes with appropriate content
      tagList(
        # Series length box
        value_box(
          title = "Serielængde",
          value = if (status_info$status == "ready" && !is.null(anhoej$longest_run)) {anhoej$longest_run} else {
            tags$span(#style = "font-size: 1.2rem;", 
              switch(status_info$status,
                "no_data" = "Ingen data",
                "not_started" = "Afventer start",
                "not_configured" = "Ikke konfigureret",
                "not_run_chart" = "N/A",
                "insufficient_data" = "For få data",
                "processing" = "Behandler...",
                "calculating" = "Beregner...",
                "Afventer data"
              ))
          },
          # showcase = icon("trending-up"),
          # showcase = icon("trending-up"),
          theme = if (status_info$status == "ready" && !is.null(anhoej$runs_signal) && (anhoej$runs_signal %||% FALSE)) {
            "warning"
          } else if (status_info$status == "ready") {
            "light" 
          } else {
            status_info$theme
          },
          height = "120px",
          p(class = "fs-7 text-muted mb-0", 
            if (status_info$status == "ready" && !is.null(anhoej$longest_run_max)) {
              paste("Forventet (maks.):", anhoej$longest_run_max, "punkter")
            } else {
              status_info$message
            })
        ),
        
        # Number of crossings box  
        value_box(
          title = "Antal Kryds",
          value = if (status_info$status == "ready" && !is.null(anhoej$n_crossings)) {anhoej$n_crossings
          } else {
            tags$span(#style = "font-size: 1.2rem; color: #666;",
              switch(status_info$status,
                "no_data" = "Ingen data",
                "not_started" = "Afventer start", 
                "not_configured" = "Ikke konfigureret",
                "not_run_chart" = "N/A",
                "insufficient_data" = "For få data",
                "processing" = "Behandler...",
                "calculating" = "Beregner...",
                "Afventer data"
              ))
          },
          showcase = icon("exchange-alt"),
          theme = if (status_info$status == "ready") "light" else status_info$theme,
          height = "120px", 
          p(class = "fs-7 text-muted mb-0",
            if (status_info$status == "ready" && !is.null(anhoej$n_crossings_min)) {
              paste("Forventet (min.):", anhoej$n_crossings_min, "kryds")
            } else {
              status_info$message
            })
        )
      )
    })
    
    # Additional value box 1 - Data Quality
    output$data_quality_box <- renderUI({
      data <- data_reactive()
      if (is.null(data) || nrow(data) == 0) {
        return(div())
      }
      
      value_box(
        title = "Data Kvalitet",
        value = "God",
        showcase = icon("check-circle"),
        theme = "success",
        p(class = "fs-6 text-muted", "Automatisk kvalitetskontrol")
      )
    })
    
    # Additional value box 2 - Report Status  
    output$report_status_box <- renderUI({
      data <- data_reactive()
      if (is.null(data) || nrow(data) == 0) {
        return(div())
      }
      
      value_box(
        title = "Rapport Status",
        value = "Klar",
        showcase = icon("file-text"),
        theme = "info",
        p(class = "fs-6 text-muted", "Eksport og deling tilgængelig")
      )
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
