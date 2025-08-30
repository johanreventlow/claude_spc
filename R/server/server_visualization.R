# R/server/server_visualization.R
# Visualization setup and data preparation logic

setup_visualization <- function(input, output, session, values) {
  
  # Data for visualization module
  active_data <- reactive({
    req(values$current_data)
    
    data <- values$current_data
    non_empty_rows <- apply(data, 1, function(row) any(!is.na(row)))
    
    if (any(non_empty_rows)) {
      filtered_data <- data[non_empty_rows, ]
      return(filtered_data)
    } else {
      return(NULL)
    }
  })
  
  # Column configuration for visualization
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
  
  # Has data check
  output$has_data <- reactive({
    !is.null(active_data()) && nrow(active_data()) > 0
  })
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)
  
  # Initialize visualization module
  visualization <- visualizationModuleServer(
    "visualization",
    data_reactive = active_data,
    column_config_reactive = column_config,
    chart_type_reactive = reactive({
      chart_selection <- if(is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type
      get_qic_chart_type(chart_selection)
    }),
    show_targets_reactive = reactive(if(is.null(input$show_targets)) FALSE else input$show_targets),
    show_phases_reactive = reactive(if(is.null(input$show_phases)) FALSE else input$show_phases),
    chart_title_reactive = chart_title(input)
  )
  
  # Plot ready check
  output$plot_ready <- reactive({
    result <- !is.null(visualization$plot_ready()) && visualization$plot_ready()
    return(result)
  })
  outputOptions(output, "plot_ready", suspendWhenHidden = FALSE)
  
  # Return visualization object for use in download handlers
  return(visualization)
}
