# R/ui/ui_main_content.R
# Main content area components

create_ui_main_content <- function() {
  tagList(
    # Welcome message when no meaningful data is loaded
    conditionalPanel(
      condition = "output.dataLoaded != 'TRUE'", 
      div(
        style = "margin-top: 20px;",
        h4("Velkommen til BFHs SPC værktøj"),
        p("For at komme i gang kan du:"),
        tags$ul(
          tags$li("Upload en Excel (.xlsx eller .xls) eller CSV-fil med dine data i menuen til venstre, eller"),
          tags$li('Klik på "Start ny session" i sidepanelet for at begynde med manuel indtastning')
        ),
        p("Når data er indlæst, vil du kunne se SPC-grafer og konfigurere dine analyser.")
      )
    ),
    
    # Data table and visualization - only when user has started
    conditionalPanel(
      condition = "output.dataLoaded == 'TRUE'",
      fluidRow(
        column(
          6,
          create_data_table_card(),
          br(),
          # Chart settings - only when we have meaningful data
          create_chart_settings_card()
        ),
        
        # Visualization and export - only when data is loaded
        column(
          6,
          create_visualization_card(),
          br(),
          create_export_card()
        )
      )
    )
  )
}

create_data_table_card <- function() {
  card(
    full_screen = TRUE,
    card_header(
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        div(
          icon("table"),
          " Data",
          style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 600;")
        ),
        div(
          class = "btn-group",
          actionButton(
            "edit_column_names",
            label = NULL,
            icon = icon("edit"),
            title = "Redigér kolonnenavne",
            class = "btn-outline-secondary btn-sm"
          ),
          actionButton(
            "add_column",
            label = NULL,
            icon = icon("plus"),
            title = "Tilføj kolonne",
            class = "btn-outline-primary btn-sm"
          ),
          actionButton(
            "add_row",
            label = NULL,
            icon = icon("plus-square"),
            title = "Tilføj række",
            class = "btn-outline-primary btn-sm"
          ),
          actionButton(
            "reset_table",
            label = NULL,
            icon = icon("refresh"),
            title = "Reset tabel", 
            class = "btn-outline-warning btn-sm"
          )
        )
      )
    ),
    card_body(
      style = "padding: 10px;",
      
      # Rhandsontable
      div(
        style = "border: 1px solid #ddd; border-radius: 5px; background-color: white; min-height: 400px;",
        rhandsontable::rHandsontableOutput("main_data_table")
      ),
      
      # Tabel info
      div(
        style = "margin-top: 10px; font-size: 0.85rem; color: #666; text-align: center;",
        icon("info-circle"),
        " Dobbeltklik på ", strong("kolonnenavn"), " for at redigere • Dobbeltklik på celle for data • Højreklik for menu",
        br(),
        " Alternativt: Brug redigér-knappen ", icon("edit"), " for modal dialog",
        br(),
        strong("Dato-formater:"), " 01-01-2024, 1/1/2024, 01.01.24, 1 jan 2024, 2024-01-01 og mange flere"
      )
    )
  )
}

create_chart_settings_card <- function() {
  conditionalPanel(
    condition = "output.has_data == 'true'",
    card(
      card_header(
        div(
          icon("sliders-h"), 
          " Graf Indstillinger",
          style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 500;")
        )
      ),
      card_body(
        style = "overflow: visible;",
        
        # Chart type selection
        selectInput(
          "chart_type",
          "Diagram type:",
          choices = CHART_TYPES_DA,
          selected = "Seriediagram (Run Chart)"
        ),
        
        # Column mapping section
        create_column_mapping_section(),
        
        # Additional options
        fluidRow(
          column(6,
                 checkboxInput(
                   "show_targets",
                   "Vis målsætninger",
                   value = FALSE
                 )
          ),
          column(6,
                 checkboxInput(
                   "show_phases",
                   "Vis faser",
                   value = FALSE
                 )
          )
        )
      )
    )
  )
}

create_column_mapping_section <- function() {
  div(
    style = "margin: 15px 0; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
    h6("Kolonne Mapping:", style = "font-weight: 500; margin-bottom: 10px;"),
    
    # X-axis column
    selectInput(
      "x_column",
      "X-akse (tid/observation):",
      choices = NULL,
      selected = NULL
    ),
    
    # Y-axis column  
    selectInput(
      "y_column",
      "Y-akse (værdi):",
      choices = NULL,
      selected = NULL
    ),
    
    # N column
    selectInput(
      "n_column",
      "Nævner (n):",
      choices = NULL,
      selected = NULL
    ),
    
    # Hjælpe-tekst for nævner
    conditionalPanel(
      condition = "input.chart_type == 'P-kort (Andele)' || input.chart_type == \"P'-kort (Andele, standardiseret)\" || input.chart_type == 'U-kort (Rater)' || input.chart_type == \"U'-kort (Rater, standardiseret)\"",
      div(
        class = "alert alert-info",
        style = "font-size: 0.8rem; padding: 6px; margin-top: 5px;",
        icon("info-circle"),
        " Nævner-kolonne er ", strong("påkrævet"), " for denne chart type"
      )
    ),
    
    conditionalPanel(
      condition = "input.chart_type != 'P-kort (Andele)' && input.chart_type != \"P'-kort (Andele, standardiseret)\" && input.chart_type != 'U-kort (Rater)' && input.chart_type != \"U'-kort (Rater, standardiseret)\"",
      div(
        style = "font-size: 0.8rem; color: #666; margin-top: 5px;",
        icon("info-circle"),
        " Nævner er valgfri for denne chart type. Vælg 'Ingen (tom)' hvis ikke relevant."
      )
    ),
    
    # Auto-detect button
    actionButton(
      "auto_detect_columns",
      "Auto-detektér kolonner",
      icon = icon("magic"),
      class = "btn-outline-secondary btn-sm w-100",
      style = "margin-top: 10px;"
    ),
    
    # Column validation feedback
    div(
      id = "column_validation",
      style = "margin-top: 10px;",
      uiOutput("column_validation_messages")
    )
  )
}

create_visualization_card <- function() {
  card(
    card_header(
      div(
        icon("chart-line"),
        " SPC Visualisering",
        style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 600;")
      )
    ),
    card_body(
      visualizationModuleUI("visualization")
    )
  )
}

create_export_card <- function() {
  conditionalPanel(
    condition = "output.plot_ready == 'true'",
    card(
      card_header(
        div(
          icon("download"), 
          " Eksport",
          style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 500;")
        )
      ),
      card_body(
        # KOMPLET EXPORT - Excel version
        div(
          downloadButton(
            "download_complete_excel",
            "📋 Komplet Export (Excel)",
            icon = icon("file-excel"),
            title = "Download hele sessionen som Excel fil med data og konfiguration",
            class = "btn-success w-100 mb-2"
          ),
          
          # Hjælpe-tekst for komplet export
          div(
            style = "font-size: 0.75rem; color: #666; text-align: center; margin-bottom: 8px; font-style: italic;",
            "Data + metadata i 2 Excel sheets - klar til brug og re-import"
          )
        ),
        
        hr(style = "margin: 15px 0;"),
        
        div(
          style = "text-align: center; font-size: 0.85rem; color: #666; margin-bottom: 10px;",
          strong("Graf eksporter:")
        ),
        
        downloadButton(
          "download_png",
          "Download PNG",
          icon = icon("image"),
          class = "btn-outline-primary w-100 mb-2"
        ),
        
        downloadButton(
          "download_pdf", 
          "Download PDF Rapport",
          icon = icon("file-pdf"),
          class = "btn-outline-primary w-100"
        )
      )
    )
  )
}
