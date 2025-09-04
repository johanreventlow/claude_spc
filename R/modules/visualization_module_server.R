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
        
        plot <- generateSPCPlot(
          data = data, 
          config = config, 
          chart_type = chart_type,
          target_value = target_value_reactive(),
          show_phases = skift_config$show_phases,
          skift_column = skift_config$skift_column,
          chart_title_reactive = chart_title_reactive
        )
        
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
        values$anhoej_results <- NULL  # CLEAR Anhøj results on error
        return(NULL)
      })
    })
    
    # Dynamic content output
    output$dynamic_content <- renderUI({
      div(
        style = "width: 100%; height: 100%;",
        plotOutput(ns("spc_plot"), width = "100%", height = "100%")
      )
    })
    
    # Render plot
    output$spc_plot <- renderPlot({
      data <- data_reactive()
      config <- chart_config()
      
      if (is.null(data) || is.null(config) || is.null(config$y_col)) {
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
      
      plot <- spc_plot()
      
      if (is.null(plot)) {
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
    }, res = 96)
    
    # Plot ready status
    output$plot_ready <- reactive({
      values$plot_ready
    })
    outputOptions(output, "plot_ready", suspendWhenHidden = FALSE)
    
    # Plot info and warnings
    output$plot_info <- renderUI({
      # Check data first to get hide_anhoej_rules flag
      data <- data_reactive()
      
      # SOLUTION: Check hide_anhoej_rules flag passed via data attribute
      # This flag is set to TRUE when "start ny session" is clicked
      # and set to FALSE when real data is uploaded
      hide_flag <- attr(data, "hide_anhoej_rules")
      if (!is.null(hide_flag) && hide_flag) {
        return(NULL)
      }
      
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
    
    # Anhøj rules section - only for run charts with sufficient data
    output$anhoej_rules_section <- renderUI({
      # Check data first to get hide_anhoej_rules flag
      data <- data_reactive()
      
      # SOLUTION: Check hide_anhoej_rules flag passed via data attribute
      # This flag is set to TRUE when "start ny session" is clicked
      # and set to FALSE when real data is uploaded
      hide_flag <- attr(data, "hide_anhoej_rules")
      if (!is.null(hide_flag) && hide_flag) {
        return(NULL)
      }
      
      # Only show for run charts
      if (is.null(chart_type_reactive()) || chart_type_reactive() != "run") {
        return(NULL)
      }
      
      # Check data and config first
      data <- data_reactive()
      config <- chart_config()
      
      if (is.null(data) || is.null(config) || is.null(config$y_col)) {
        return(NULL)
      }
      
      
      # Only show if we have Anhøj results and enough data
      if (is.null(values$anhoej_results)) {
        return(NULL)
      }
      
      # Count meaningful data points (non-NA values)
      if (!is.null(config$n_col) && config$n_col %in% names(data)) {
        # For rate data: count where both numerator and denominator are valid
        taeller <- parse_danish_number(data[[config$y_col]])
        naevner <- parse_danish_number(data[[config$n_col]])
        meaningful_count <- sum(!is.na(taeller) & !is.na(naevner) & naevner > 0)
      } else {
        # For simple data: count non-NA values
        y_data_raw <- data[[config$y_col]]
        y_data <- parse_danish_number(y_data_raw)
        meaningful_count <- sum(!is.na(y_data))
      }
      
      # Only show Anhøj rules if we have minimum 10 meaningful observations
      if (meaningful_count < 10) {
        return(NULL)
      }
      
      # Show the Anhøj rules card
      card(
        card_header(
          div(
            icon("search"),
            " Anhøj Regler (Run Chart)",
            style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 500;")
          )
        ),
        card_body(
          renderAnhoejResults(values$anhoej_results)
        )
      )
    })
    
    # NEW: Plot status as value boxes
    output$plot_status_boxes <- renderUI({
      data <- data_reactive()
      hide_flag <- attr(data, "hide_anhoej_rules")
      if (!is.null(hide_flag) && hide_flag) {
        return(div()) # Return empty div instead of NULL
      }
      
      if (length(values$plot_warnings) > 0) {
        # Warning value box
        value_box(
          title = "Graf Status",
          value = "Advarsler",
          showcase = icon("exclamation-triangle"),
          theme = "warning",
          p(class = "fs-6 text-muted", paste(values$plot_warnings, collapse = " • "))
        )
      } else if (values$plot_ready && !is.null(data_reactive())) {
        # Success value box
        value_box(
          title = "Graf Status", 
          value = "Klar",
          showcase = icon("check-circle"),
          theme = "success",
          p(class = "fs-6 text-muted", 
            sprintf("Chart: %s | Punkter: %d", 
                    chart_type_reactive() %||% "unknown",
                    nrow(data_reactive())))
        )
      } else {
        # Default state
        value_box(
          title = "Graf Status",
          value = "Venter...",
          showcase = icon("hourglass"),
          theme = "secondary",
          p(class = "fs-6 text-muted", "Konfigurer data og indstillinger")
        )
      }
    })
    
    # NEW: Anhøj rules as value boxes
    output$anhoej_rules_boxes <- renderUI({
      data <- data_reactive()
      hide_flag <- attr(data, "hide_anhoej_rules")
      if (!is.null(hide_flag) && hide_flag) {
        return(div()) # Return empty div instead of NULL
      }
      
      # Only for run charts and sufficient data
      if (is.null(chart_type_reactive()) || chart_type_reactive() != "run") {
        # Default for non-run charts
        value_box(
          title = "Specielle Mønstre",
          value = "N/A",
          showcase = icon("chart-line"),
          theme = "light",
          p(class = "fs-6 text-muted", "Kun for run charts")
        )
      } else {
        config <- chart_config()
        if (is.null(config) || is.null(config$y_col) || !config$y_col %in% names(data)) {
          # No valid config yet
          value_box(
            title = "Anhøj Regler", 
            value = "Venter...",
            showcase = icon("search"),
            theme = "secondary",
            p(class = "fs-6 text-muted", "Konfigurer kolonner")
          )
        } else {
          # Check data count
          meaningful_count <- if (!is.null(config$n_col) && config$n_col %in% names(data)) {
            taeller <- parse_danish_number(data[[config$y_col]])
            naevner <- parse_danish_number(data[[config$n_col]])
            sum(!is.na(taeller) & !is.na(naevner) & naevner > 0)
          } else {
            y_data_raw <- data[[config$y_col]]
            y_data <- parse_danish_number(y_data_raw)
            sum(!is.na(y_data))
          }
          
          if (meaningful_count < 10) {
            value_box(
              title = "Anhøj Regler",
              value = "For få data",
              showcase = icon("search"),
              theme = "warning", 
              p(class = "fs-6 text-muted", "Min. 10 punkter påkrævet")
            )
          } else {
            # Count anhøj rules that were triggered
            anhoej_count <- if (!is.null(values$anhoej_results)) {
              sum(sapply(values$anhoej_results, function(x) length(x) > 0))
            } else {
              0
            }
            
            value_box(
              title = "Anhøj Regler",
              value = paste(anhoej_count, "fund"),
              showcase = icon("search"), 
              theme = if (anhoej_count > 0) "info" else "secondary",
              p(class = "fs-6 text-muted", "Run chart mønstre")
            )
          }
        }
      }
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
