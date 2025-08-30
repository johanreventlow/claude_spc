# R/modules/visualization_module_ui.R
# UI components for the visualization module

library(shiny)

# Visualization Module UI
visualizationModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Plot container - always visible
    div(
      id = ns("plot_container"),
      style = "position: relative; ",
      
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
