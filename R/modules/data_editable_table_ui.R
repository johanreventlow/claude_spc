# R/modules/data_editable_table_ui.R
# UI for editable table module

library(shiny)
library(rhandsontable)

# Editable Table Module UI
editableTableUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Data controls
    fluidRow(
      column(
        8,
        uiOutput(ns("table_status"))
      ),
      column(
        4,
        div(
          style = "text-align: right;",
          div(
            class = "btn-group",
            actionButton(
              ns("add_row"),
              icon = icon("plus"),
              label = "Tilføj række",
              class = "btn-outline-primary btn-sm"
            ),
            actionButton(
              ns("delete_row"),
              icon = icon("minus"), 
              label = "Slet række",
              class = "btn-outline-danger btn-sm"
            )
          ),
          
          br(), br(),
          
          div(
            class = "btn-group",
            actionButton(
              ns("undo"),
              icon = icon("undo"),
              label = "Fortryd",
              class = "btn-outline-secondary btn-sm"
            ),
            actionButton(
              ns("redo"),
              icon = icon("redo"),
              label = "Gentag",
              class = "btn-outline-secondary btn-sm"
            ),
            actionButton(
              ns("reset"),
              icon = icon("refresh"),
              label = "Reset",
              class = "btn-outline-warning btn-sm"
            )
          )
        )
      )
    ),
    
    br(),
    
    uiOutput(ns("edit_validation_messages")),
    
    # Editable table
    conditionalPanel(
      condition = paste0("output['", ns("has_data"), "']"),
      
      div(
        style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px; background-color: white;",
        
        div(
          style = "margin-bottom: 10px; padding: 8px; background-color: #f8f9fa; border-radius: 3px; font-size: 0.85rem; color: #666;",
          icon("info-circle"),
          strong(" Redigeringstips: "), 
          "Dobbeltklik for at redigere • Tab/Enter for næste celle • Ctrl+Z for fortryd • Højreklik for kontekstmenu"
        ),
        
        rhandsontable::rHandsontableOutput(ns("editable_table"), height = "100%")
      )
    ),
    
    # No data message
    conditionalPanel(
      condition = paste0("!output['", ns("has_data"), "']"),
      div(
        style = "text-align: center; margin-top: 50px; margin-bottom: 50px;",
        div(
          style = paste("background-color:", HOSPITAL_COLORS$light, "; padding: 30px; border-radius: 8px;"),
          icon("table", style = "font-size: 3em; color: #ccc; margin-bottom: 20px;"),
          h4("Ingen data at redigere", style = paste("color:", HOSPITAL_COLORS$primary)),
          p("Upload data i Analyse-fanen for at begynde redigering", 
            style = paste("color:", HOSPITAL_COLORS$secondary, "; font-size: 1.1rem;"))
        )
      )
    )
  )
}
