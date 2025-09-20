# mod_spc_chart_ui.R
# UI components for SPC chart module
# Extracted from mod_spc_chart.R for better maintainability

# Dependencies ----------------------------------------------------------------
library(shiny)
library(shinyjs)

# Source required helper functions
# Helper functions now loaded globally in global.R for better performance

# PLOT MODUL UI ===============================================================

#' Visualization Module UI - Kun plot området
#'
#' Håndterer selve plot visningen uden status elementer.
#' Status elementer er separeret i visualizationStatusUI for modularitet.
#'
#' @param id Character. Namespace ID for modulet
#' @return Shiny UI element
#' @family visualization_modules
#' @export
visualizationModuleUI <- function(id) {
  ns <- NS(id)

  # PRODUCTION VERSION: Restored original styling with fixes applied
  div(
    id = ns("plot_container"),
    class = "spc-plot-main-container",
    style = "position: relative;",
    div(
      class = "spc-ggplot-output",
      plotOutput(ns("spc_plot_actual"), width = "100%", height = "500px")
    )
  )
}

# STATUS ELEMENTER UI ========================================================

#' Visualization Status UI - Status og metrics displays
#'
#' Separeret fra plot UI for bedre modularitet.
#' Håndterer value boxes og SPC metrics visning.
#'
#' @param id Character. Namespace ID for modulet
#' @return Shiny UI element med value boxes
#' @family visualization_modules
#' @export
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
# R/modules/visualization_module_server.R
# Server logik for visualiseringsmodulet

# Dependencies ----------------------------------------------------------------
library(shiny)
library(bslib)
library(qicharts2)
library(ggplot2)
library(dplyr)
library(scales)

# SPC Ikoner ------------------------------------------------------------------
# Tilpassede SVG ikoner til SPC value boxes
# Defineret som HTML-variabler for genbrugelige visualiseringer

## Run Chart Ikon
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

## Median Crossings Ikon
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

## Out-of-Control Ikon
spc_out_of_control_icon <- HTML('
  <svg xmlns="http://www.w3.org/2000/svg" width="64" height="64" fill="currentColor" viewBox="0 0 16 16">
    <path fill-rule="evenodd" d="M0 0h1v15h15v1H0z"/>
    <path d="M2 4h12" stroke="currentColor" stroke-width="0.5" stroke-dasharray="2,1" opacity="0.6" fill="none"/>
    <path d="M2 8h12" stroke="currentColor" stroke-width="0.5" opacity="0.7" fill="none"/>
    <path d="M2 12h12" stroke="currentColor" stroke-width="0.5" stroke-dasharray="2,1" opacity="0.6" fill="none"/>
    <circle cx="3" cy="7" r="0.5"/>
    <circle cx="5" cy="8.5" r="0.5"/>
    <circle cx="7" cy="6.5" r="0.5"/>
    <circle cx="9" cy="2.5" r="0.5" />
    <circle cx="11" cy="7.5" r="0.5"/>
    <circle cx="13" cy="8" r="0.5"/>
    <path d="M3 7 L5 8.5 L7 6.5 L9 2.5 L11 7.5 L13 8" stroke="currentColor" stroke-width="0.5" fill="none"/>
  </svg>
')

# HOVEDFUNKTION ==============================================================

#' Visualization Module Server
#'
#' Hovedserver funktion for SPC visualiseringsmodulet.
#' Håndterer al server-logik for SPC visualisering inklusiv:
#' - Plot generering og konfiguration
#' - Anhøj rules analyse for alle chart typer
#' - Value box status displays
#' - Fejlhåndtering og brugerfeedback
#'
#' @param id Module ID
#' @param data_reactive Reaktiv data source
#' @param column_config_reactive Reaktiv kolonne konfiguration
#' @param chart_type_reactive Reaktiv chart type
#' @param target_value_reactive Reaktiv målværdi
#' @param centerline_value_reactive Reaktiv centerline værdi
#' @param skift_config_reactive Reaktiv fase konfiguration
#' @param frys_config_reactive Reaktiv freeze konfiguration
#' @param chart_title_reactive Reaktiv chart titel (optional)
#'
#' @return Liste med reactive values for plot, status og resultater
