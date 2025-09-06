# R/modules/visualization_module_ui.R
# UI components for the visualization module

library(shiny)

# Visualization Module UI - Only the plot
visualizationModuleUI <- function(id) {
  ns <- NS(id)
  
  # Only the plot - no status info
  div(
    id = ns("plot_container"),
    style = "position: relative; height: 100%;",
    
    # Dynamic content that switches between plot and placeholder
    uiOutput(ns("dynamic_content"))
  )
}

# Status elements UI - separate from plot
visualizationStatusUI <- function(id) {
  ns <- NS(id)
  
  # Use layout_column_wrap for proper value box display
  layout_column_wrap(
    width = 1/2,
    heights_equal = "row",
    
    # Plot information as value boxes
    uiOutput(ns("plot_status_boxes")),
    
    # Anhøj rules results for run charts as value boxes
    uiOutput(ns("anhoej_rules_boxes")),
    
    # Data summary for error checking
    uiOutput(ns("data_summary_box"))
  )
}
