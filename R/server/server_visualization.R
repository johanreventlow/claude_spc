# server_visualization.R
# Server logik for visualisering og data forberedelse

# Dependencies ----------------------------------------------------------------

# VISUALISERING SETUP =========================================================

## Hovedfunktion for visualisering
# Opsætter al server logik relateret til visualisering og data forberedelse
setup_visualization <- function(input, output, session, values) {
  
  # Data til visualiserings modul
  active_data <- reactive({
    req(values$current_data)
    
    data <- values$current_data
    
    # Tilføj hide_anhoej_rules flag som attribut til data
    attr(data, "hide_anhoej_rules") <- values$hide_anhoej_rules
    
    # Tjek om dette er den tomme standard tabel fra session reset
    if (nrow(data) == 5 && all(c("Skift", "Dato", "Tæller", "Nævner", "Kommentar") %in% names(data))) {
      if (all(is.na(data$Dato)) && all(is.na(data$Tæller)) && all(is.na(data$Nævner))) {
        # Dette er tom standard tabel men vi skal stadig videregive hide flag
        attr(data, "hide_anhoej_rules") <- values$hide_anhoej_rules
        return(data)  # Retur nér data med flag i stedet for NULL
      }
    }
    
    non_empty_rows <- apply(data, 1, function(row) any(!is.na(row)))
    
    if (any(non_empty_rows)) {
      filtered_data <- data[non_empty_rows, ]
      attr(filtered_data, "hide_anhoej_rules") <- values$hide_anhoej_rules
      return(filtered_data)
    } else {
      # Selv når ingen meningsfuld data, videregiv flaget
      attr(data, "hide_anhoej_rules") <- values$hide_anhoej_rules
      return(data)
    }
  })
  
  # Kolonne konfiguration til visualisering
  column_config <- reactive({
    x_col <- if (!is.null(input$x_column) && input$x_column != "" && input$x_column != "BLANK") input$x_column else NULL
    y_col <- if (!is.null(input$y_column) && input$y_column != "" && input$y_column != "BLANK") input$y_column else NULL
    n_col <- if (!is.null(input$n_column) && input$n_column != "" && input$n_column != "BLANK") input$n_column else NULL
    
    return(list(
      x_col = x_col,
      y_col = y_col,
      n_col = n_col,
      chart_type = get_qic_chart_type(if(is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type)
    ))
  })
  
  
  # Initialiser visualiserings modul  
  visualization <- visualizationModuleServer(
    "visualization",
    data_reactive = active_data,
    column_config_reactive = column_config,
    chart_type_reactive = reactive({
      chart_selection <- if(is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type
      get_qic_chart_type(chart_selection)
    }),
    target_value_reactive = reactive({
      if (is.null(input$target_value) || input$target_value == "") {
        return(NULL)
      }
      
      # Hent y-akse data til smart konvertering
      data <- active_data()
      config <- column_config()
      
      if (!is.null(data) && !is.null(config) && !is.null(config$y_col) && config$y_col %in% names(data)) {
        y_data <- data[[config$y_col]]
        y_numeric <- parse_danish_number(y_data)
        return(parse_danish_target(input$target_value, y_numeric))
      } else {
        return(parse_danish_target(input$target_value, NULL))
      }
    }),
    skift_config_reactive = reactive({
      # Bestem om vi skal vise faser baseret på Skift kolonne valg og data
      data <- active_data()
      config <- column_config()
      
      if (is.null(data) || is.null(config)) {
        return(list(show_phases = FALSE, skift_column = NULL))
      }
      
      # Tjek om bruger har valgt en Skift kolonne
      skift_col <- if (!is.null(input$skift_column) && input$skift_column != "" && input$skift_column != "BLANK") {
        input$skift_column
      } else {
        NULL
      }
      
      # Hvis ingen Skift kolonne valgt, ingen faser
      if (is.null(skift_col) || !skift_col %in% names(data)) {
        return(list(show_phases = FALSE, skift_column = NULL))
      }
      
      # Tjek om Skift kolonne har nogen TRUE værdier
      skift_data <- data[[skift_col]]
      has_phase_shifts <- any(skift_data == TRUE, na.rm = TRUE)
      
      return(list(
        show_phases = has_phase_shifts,
        skift_column = skift_col
      ))
    }),
    chart_title_reactive = chart_title(input)
  )
  
  # Plot klar tjek
  output$plot_ready <- reactive({
    result <- !is.null(visualization$plot_ready()) && visualization$plot_ready()
    return(if(result) "true" else "false")
  })
  outputOptions(output, "plot_ready", suspendWhenHidden = FALSE)
  
  # Retur nér visualiserings objekt til brug i download handlers
  return(visualization)
}
