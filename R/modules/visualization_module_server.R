# R/modules/visualization_module_server.R
# Server logic for the visualization module

library(shiny)
library(bslib)
library(qicharts2)
library(ggplot2)
library(dplyr)
library(scales)

# SPC Ikoner ----

#' Custom SVG ikoner til SPC value boxes
#' Definerede som HTML-variabler for genbrugelige visualiseringer

spc_run_chart_icon <- HTML('
  <svg xmlns="http://www.w3.org/2000/svg" width="64" height="64" fill="currentColor" viewBox="0 0 16 16">
    <path fill-rule="evenodd" d="M0 0h1v15h15v1H0z"/>
    <path d="M2 8h12" stroke="currentColor" stroke-width="0.5" stroke-dasharray="1,1" opacity="0.5" fill="none"/>
    <circle cx="3" cy="4" r="0.5"/>
    <circle cx="4.5" cy="3.5" r="0.5"/>
    <circle cx="6" cy="4.5" r="0.5"/>
    <circle cx="7.5" cy="3" r="0.5"/>
    <circle cx="9" cy="4" r="0.5"/>
    <circle cx="10.5" cy="3.5" r="0.5"/>
    <circle cx="12" cy="10" r="0.5"/>
    <circle cx="13" cy="11" r="0.5"/>
    <path d="M3 4 L4.5 3.5 L6 4.5 L7.5 3 L9 4 L10.5 3.5 L12 10 L13 11" stroke="currentColor" stroke-width="0.5" fill="none"/>
  </svg>
')

spc_median_crossings_icon <- HTML('
  <svg xmlns="http://www.w3.org/2000/svg" width="64" height="64" fill="currentColor" viewBox="0 0 16 16">
    <path fill-rule="evenodd" d="M0 0h1v15h15v1H0z"/>
    <path d="M2 8h12" stroke="currentColor" stroke-width="0.5" stroke-dasharray="1,1" opacity="0.6" fill="none"/>
    <circle cx="3" cy="6" r="0.5"/>
    <circle cx="4.5" cy="10" r="0.5"/>
    <circle cx="6" cy="5" r="0.5"/>
    <circle cx="7.5" cy="11" r="0.5"/>
    <circle cx="9" cy="6.5" r="0.5"/>
    <circle cx="10.5" cy="9.5" r="0.5"/>
    <circle cx="12" cy="5.5" r="0.5"/>
    <circle cx="13.5" cy="10.5" r="0.5"/>
    <path d="M3 6 L4.5 10 L6 5 L7.5 11 L9 6.5 L10.5 9.5 L12 5.5 L13.5 10.5" stroke="currentColor" stroke-width="0.5" fill="none"/>
  </svg>
')

spc_out_of_control_icon <- HTML('
  <svg xmlns="http://www.w3.org/2000/svg" width="64" height="64" fill="currentColor" viewBox="0 0 16 16">
    <path fill-rule="evenodd" d="M0 0h1v15h15v1H0z"/>
    <path d="M2 4h12" stroke="currentColor" stroke-width="0.5" stroke-dasharray="2,1" opacity="0.6" fill="none"/>
    <path d="M2 8h12" stroke="currentColor" stroke-width="0.5" opacity="0.7" fill="none"/>
    <path d="M2 12h12" stroke="currentColor" stroke-width="0.5" stroke-dasharray="2,1" opacity="0.6" fill="none"/>
    <circle cx="3" cy="7" r="0.5"/>
    <circle cx="5" cy="8.5" r="0.5"/>
    <circle cx="7" cy="6.5" r="0.5"/>
    <circle cx="9" cy="2.5" r="0.6" fill="red"/>
    <circle cx="11" cy="7.5" r="0.5"/>
    <circle cx="13" cy="8" r="0.5"/>
    <path d="M3 7 L5 8.5 L7 6.5 L9 2.5 L11 7.5 L13 8" stroke="currentColor" stroke-width="0.5" fill="none"/>
    <path d="M8.5 2 L9.5 2 M9 1.5 L9 2.5" stroke="red" stroke-width="0.5"/>
  </svg>
')

# Hovedfunktion ----

#' Visualization Module Server
#' 
#' Håndterer al server-logik for SPC visualisering inklusiv:
#' - Plot generering og konfiguration
#' - Anhøj rules analyse (for run charts)
#' - Value box status displays
#' - Fejlhåndtering og brugerfeedback
visualizationModuleServer <- function(id, data_reactive, column_config_reactive, chart_type_reactive, target_value_reactive, skift_config_reactive, chart_title_reactive = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # State Management ----
    
    # Reactive values til tilstandshåndtering
    values <- reactiveValues(
      plot_object = NULL,
      plot_ready = FALSE,
      anhoej_results = NULL,
      plot_warnings = character(0),
      is_computing = FALSE
    )
    
    # Waiter til plot loading feedback
    waiter_plot <- waiter::Waiter$new(
      id = ns("plot_container"),
      html = WAITER_CONFIG$plot_generation$html,
      color = WAITER_CONFIG$plot_generation$color
    )
    
    # Konfiguration og Validering ----
    
    ## Chart Configuration ----
    
    #' Reaktiv konfiguration for chart setup
    #' Håndterer kolonne-validering og auto-detection
    chart_config <- reactive({
      data <- data_reactive()
      config <- column_config_reactive()
      chart_type <- if(is.null(chart_type_reactive())) "run" else chart_type_reactive()
      
      if (is.null(data) || is.null(config)) {
        return(NULL)
      }
      
      # Valider at kolonner eksisterer i data
      if (!is.null(config$x_col) && !(config$x_col %in% names(data))) {
        config$x_col <- NULL
      }
      if (!is.null(config$y_col) && !(config$y_col %in% names(data))) {
        config$y_col <- NULL
      }
      if (!is.null(config$n_col) && !(config$n_col %in% names(data))) {
        config$n_col <- NULL
      }
      
      # Auto-detect hvis nødvendigt
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
    
    # Plot Generering ----
    
    ## Main SPC Plot Reactive ----
    
    #' Hovedfunktion for SPC plot generering
    #' Håndterer data validering, plot oprettelse og Anhøj rules analyse
    spc_plot <- reactive({
      data <- data_reactive()
      config <- chart_config()
      
      values$is_computing <- FALSE
      
      # Reset Anhøj resultater når der ikke er data eller ugyldig config
      if (is.null(data) || is.null(config)) {
        values$plot_ready <- FALSE
        values$plot_warnings <- character(0)
        values$anhoej_results <- NULL
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
      
      # Valider data
      validation <- validateDataForChart(data, config, chart_type)
      
      if (!validation$valid) {
        values$plot_warnings <- validation$warnings
        values$plot_ready <- FALSE
        values$anhoej_results <- NULL
        return(NULL)
      }
      
      values$plot_warnings <- character(0)
      
      # Generer plot
      tryCatch({
        # Hent fase konfiguration
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
        
        # Udtræk Anhøj rules og out-of-control punkter fra qic() resultater
        if (!is.null(qic_data)) {
          cat("DEBUG: Extracting results from qic() data for chart type:", chart_type, "\n")
          
          if (chart_type == "run") {
            # Run charts: Anhøj rules + out-of-control points
            values$anhoej_results <- list(
              runs_signal = any(qic_data$runs.signal, na.rm = TRUE),
              crossings_signal = FALSE, # qic leverer ikke crossings signal direkte
              any_signal = any(qic_data$sigma.signal, na.rm = TRUE),
              longest_run = max(qic_data$longest.run, na.rm = TRUE),
              out_of_control_count = sum(qic_data$sigma.signal, na.rm = TRUE),
              longest_run_max = max(qic_data$longest.run.max, na.rm = TRUE),
              n_crossings = max(qic_data$n.crossings, na.rm = TRUE),
              n_crossings_min = max(qic_data$n.crossings.min, na.rm = TRUE),
              message = if(any(qic_data$sigma.signal, na.rm = TRUE)) "Særlig årsag detekteret" else "Ingen særlige årsager fundet"
            )
          } else {
            # Alle andre chart typer (p, pp, u, up, i, mr, c, g): Kun out-of-control points
            values$anhoej_results <- list(
              runs_signal = FALSE,
              crossings_signal = FALSE,
              any_signal = any(qic_data$sigma.signal, na.rm = TRUE),
              longest_run = NA_real_,
              out_of_control_count = sum(qic_data$sigma.signal, na.rm = TRUE),
              longest_run_max = NA_real_,
              n_crossings = NA_real_,
              n_crossings_min = NA_real_,
              message = if(any(qic_data$sigma.signal, na.rm = TRUE)) "Punkter uden for kontrol detekteret" else "Alle punkter inden for kontrol"
            )
          }
          cat("DEBUG: Results extracted from qic data for chart type:", chart_type, "\n")
        } else {
          values$anhoej_results <- NULL
        }
        
        return(plot)
        
      }, error = function(e) {
        cat("ERROR in spc_plot reactive:", e$message, "\n")
        values$plot_warnings <- c("Graf-generering fejlede:", e$message)
        values$plot_ready <- FALSE
        values$anhoej_results <- NULL
        return(NULL)
      })
    })
    
    # UI Output Funktioner ----
    
    ## Plot Display Logic ----
    
    #' Dynamisk indhold output - håndterer både plot og beskeder
    output$dynamic_content <- renderUI({
      div(
        style = "width: 100%; height: 100%;",
        uiOutput(ns("plot_or_message"))
      )
    })
    
    #' Unified output der håndterer både beskeder og plots
    #' Smart besked logik baseret på app tilstand
    output$plot_or_message <- renderUI({
      data <- data_reactive()
      config <- chart_config()
      
      # Smart besked logik baseret på app tilstand
      if (is.null(data) || nrow(data) == 0) {
        return(createPlotMessage("welcome"))
      }
      
      # Check om bruger har meningsfuldt data vs empty session template
      # Ekskluder "Skift" kolonnen som altid er FALSE i nye sessioner
      data_without_skift <- data[, !names(data) %in% "Skift", drop = FALSE]
      has_meaningful_data <- any(!is.na(data_without_skift), na.rm = TRUE) && 
                            !all(sapply(data_without_skift, function(col) all(is.na(col))))
      
      # Hvis tabel er helt tom (alle NA undtagen Skift kolonne), vis velkomst besked
      if (!has_meaningful_data) {
        return(createPlotMessage("welcome"))
      }
      
      if (is.null(config) || is.null(config$y_col)) {
        return(createPlotMessage("config_needed"))
      }
      
      plot <- spc_plot()
      
      if (is.null(plot)) {
        # Bestem specifik fejl type
        if (length(values$plot_warnings) > 0) {
          warning_details <- paste(values$plot_warnings, collapse = " • ")
          
          # Check for utilstrækkelig data
          if (any(grepl("datapunkter", values$plot_warnings, ignore.case = TRUE)) ||
              any(grepl("points", values$plot_warnings, ignore.case = TRUE))) {
            return(createPlotMessage("insufficient_data", details = warning_details))
          }
          
          # Check for validerings fejl
          if (any(grepl("kolonne|column|påkrævet|required", values$plot_warnings, ignore.case = TRUE))) {
            return(createPlotMessage("validation_error", details = warning_details))
          }
          
          # Generisk validerings fejl
          return(createPlotMessage("validation_error", details = warning_details))
        } else {
          return(createPlotMessage("technical_error"))
        }
      }
      
      if (inherits(plot, "ggplot")) {
        # Returner det faktiske plot wrapped i plotOutput
        return(
          plotOutput(ns("spc_plot_actual"), width = "100%", height = "100%")
        )
      } else {
        return(createPlotMessage("technical_error"))
      }
    })
    
    ## Actual Plot Rendering ----
    
    #' Separat renderPlot for det faktiske SPC plot
    output$spc_plot_actual <- renderPlot({
      plot <- spc_plot()
      if (inherits(plot, "ggplot")) {
        return(plot)
      } else {
        return(NULL)
      }
    }, res = 96)
    
    # Status og Information ----
    
    ## Plot Status ----
    
    #' Plot klar status
    output$plot_ready <- reactive({
      values$plot_ready
    })
    outputOptions(output, "plot_ready", suspendWhenHidden = FALSE)
    
    ## Plot Information ----
    
    #' Plot info og advarsler - ALTID VIS
    output$plot_info <- renderUI({
      data <- data_reactive()
      
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
    
    # Value Boxes ----
    
    ## Plot Status Box ----
    
    #' Data summary value box (mere SPC-relevant) - ALTID SYNLIG
    output$plot_status_boxes <- renderUI({
      data <- data_reactive()
      
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
        # Default tilstand
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
    
    ## Data Summary Box ----
    
    #' Data summary value box til fejl kontrol
    output$data_summary_box <- renderUI({
      data <- data_reactive()
      config <- chart_config()
      
      if (is.null(data) || nrow(data) == 0) {
        return(
          value_box(
            title = "Data Summary",
            value = "Ingen data",
            showcase = spc_out_of_control_icon,
            showcase_layout = "top right",
            theme = "light",
            height = "120px",
            p(class = "fs-7 text-muted mb-0", "Vent på data load")
          )
        )
      }
      
      # Generer summary info
      total_rows <- nrow(data)
      total_cols <- ncol(data)
      
      # Check for konfigurerede kolonner
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
        showcase = spc_out_of_control_icon,
        showcase_layout = "top right",
        theme = "light",
        height = "120px",
        p(class = "fs-7 text-muted mb-0", summary_text)
      )
    })
    
    ## Anhøj Rules Value Boxes ----
    
    #' Anhøj rules som value boxes - ALTID SYNLIG
    #' Disse viser serielængde og antal kryds for run charts
    output$anhoej_rules_boxes <- renderUI({
      data <- data_reactive()
      
      # Smart indhold baseret på nuværende status - ALTID vis boxes
      config <- chart_config()
      chart_type <- chart_type_reactive() %||% "run"
      anhoej <- values$anhoej_results
      
      # Simplificeret logik - hvis vi har data med meningsfuldt indhold, er vi klar
      has_meaningful_data <- !is.null(data) && nrow(data) > 0 && 
        any(sapply(data, function(x) {
          if (is.logical(x)) return(any(x, na.rm = TRUE))
          if (is.numeric(x)) return(any(!is.na(x)))
          if (is.character(x)) return(any(nzchar(x, keepNA = FALSE), na.rm = TRUE))
          return(FALSE)
        }))
      
      # Bestem nuværende status og passende indhold
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
        # Check om vi har nok meningsfuldt data
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
      
      # Altid returner de to hovd boxes med passende indhold
      tagList(
        ### Serielængde Box ----
        value_box(
          title = "Serielængde",
          value = if (status_info$status == "ready" && !is.null(anhoej$longest_run)) {
            anhoej$longest_run
          } else {
            tags$span(
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
          showcase = spc_run_chart_icon,
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
        
        ### Antal Kryds Box ----
        value_box(
          title = "Antal Kryds",
          value = if (status_info$status == "ready" && !is.null(anhoej$n_crossings)) {
            anhoej$n_crossings
          } else {
            tags$span(
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
          showcase = spc_median_crossings_icon,
          theme = if (status_info$status == "ready") "light" else status_info$theme,
          height = "120px", 
          p(class = "fs-7 text-muted mb-0",
            if (status_info$status == "ready" && !is.null(anhoej$n_crossings_min)) {
              paste("Forventet (min.):", anhoej$n_crossings_min, "kryds")
            } else {
              status_info$message
            })
        ),
        
        ### Outliers/Out-of-Control Box ----
        value_box(
          title = if (chart_type == "run") "Outliers" else "Out-of-Control",
          value = if (status_info$status == "ready" && !is.null(anhoej$out_of_control_count)) {
            anhoej$out_of_control_count
          } else {
            tags$span(
              switch(status_info$status,
                "no_data" = "Ingen data",
                "not_started" = "Afventer start",
                "not_configured" = "Ikke konfigureret",
                "not_run_chart" = if (chart_type == "run") "N/A" else "N/A",
                "insufficient_data" = "For få data", 
                "processing" = "Behandler...",
                "calculating" = "Beregner...",
                "Afventer data"
              ))
          },
          showcase = spc_out_of_control_icon,
          theme = if (status_info$status == "ready" && !is.null(anhoej$out_of_control_count) && (anhoej$out_of_control_count > 0)) {
            "danger"
          } else if (status_info$status == "ready") {
            "light" 
          } else if (status_info$status == "not_run_chart" && chart_type == "run") {
            "secondary"
          } else {
            status_info$theme
          },
          height = "120px",
          p(class = "fs-7 text-muted mb-0", 
            if (status_info$status == "ready") {
              if (chart_type == "run") {
                "Ikke relevant for run charts"
              } else {
                paste("Punkter uden for kontrolgrænser")
              }
            } else {
              status_info$message
            })
        )
      )
    })
    
    ## Placeholder Value Boxes ----
    
    ### Data Quality Box (placeholder) ----
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
    
    ### Report Status Box (placeholder) ----
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
    
    # Return Values ----
    
    #' Returner reactive values til parent scope
    #' Giver adgang til plot objekt, status og Anhøj resultater
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