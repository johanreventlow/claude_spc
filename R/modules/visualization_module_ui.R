# R/modules/visualization_module_ui.R
# UI komponenter for visualiseringsmodulet

# Dependencies ----------------------------------------------------------------
library(shiny)

# PLOT MODUL UI ===============================================================

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

    # Dynamic Content Container -------------------------------------------
    # Dynamisk indhold der skifter mellem plot og placeholder beskeder
    uiOutput(ns("dynamic_content"))
  )
}

# STATUS ELEMENTER UI ========================================================

#' Visualization Status UI - Status og metrics displays
#'
#' Separeret fra plot UI for bedre modularitet.
#' Håndterer value boxes og SPC metrics visning.
visualizationStatusUI <- function(id) {
  ns <- NS(id)

  # Value Box Layout --------------------------------------------------------
  # Brug layout_column_wrap for korrekt value box visning
  layout_column_wrap(
    width = 1 / 2,
    heights_equal = "row",

    ## Anhøj Rules Value Boxes
    # Hovedmetrics: Serielængde og Antal Kryds for alle chart typer
    uiOutput(ns("anhoej_rules_boxes")),

    ## Data Summary Box
    # Data oversigt og fejl kontrol (udkommenteret)
    # uiOutput(ns("data_summary_box"))
  )
}
