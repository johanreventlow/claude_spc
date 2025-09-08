# R/modules/visualization_module_ui.R
# UI components for the visualization module

library(shiny)

# Plot Modul UI ####

#' Visualization Module UI - Kun plot området
#' 
#' Håndterer selve plot visningen uden status elementer.
#' Status elementer er separeret i visualizationStatusUI for modularitet.
visualizationModuleUI <- function(id) {
  ns <- NS(id)
  
  # Kun plot området - ingen status info
  div(
    id = ns("plot_container"),
    style = "position: relative; height: 100%;",
    
    ## Dynamic Content Container ####
    # Dynamisk indhold der skifter mellem plot og placeholder beskeder
    uiOutput(ns("dynamic_content"))
  )
}

# Status Elementer UI ####

#' Visualization Status UI - Status og metrics displays
#' 
#' Separeret fra plot UI for bedre modularitet.
#' Håndterer value boxes og SPC metrics visning.
visualizationStatusUI <- function(id) {
  ns <- NS(id)
  
  # layout_column_wrap(
  #   width = 1/2,
  #   heights_equal = "row",
  #   value_box(
  #     title = "Box 1", value = "100",
  #     style = "flex: 1;"
  #   ),
  #   value_box(
  #     title = "Box 2", value = "200",
  #     style = "flex: 1;"
  #   ),
  #   value_box(
  #     title = "Box 3", value = "300",
  #     style = "flex: 1;"
  #   ),
  #   value_box(
  #     title = "Box 4", value = "400",
  #     style = "flex: 1;"
  #   ),
  #   value_box(
  #     title = "Box 5", value = "500",
  #     style = "flex: 1;"
  #   ),
  #   value_box(
  #     title = "Box 6", value = "600",
  #     style = "flex: 1;"
  #   )
  # )
  
  ## Value Box Layout ####
  # Brug layout_column_wrap for korrekt value box visning
  layout_column_wrap(
    width = 1/2,
    heights_equal = "row",

  ### Anhøj Rules Value Boxes ####
  # Hovedmetrics: Serielængde og Antal Kryds for run charts
    uiOutput(ns("anhoej_rules_boxes")),
     
  ### Data Summary Box ####
  # Data oversigt og fejl kontrol
  #   # uiOutput(ns("data_summary_box"))
  )
}