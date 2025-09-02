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
      # tagList(
        # Status information as value boxes (top row)
        # create_status_value_boxes(),
        
        # br(),
        
        # Main content in columns
        layout_column_wrap(
          width = 1/2,
      
        # layout_columns(
          # col_widths = c(6, 6),
          height = "auto",
          max_height = "100%",
          
          # Left column: Data table and chart settings
          create_data_table_card(),
          create_plot_only_card()
        ),
          
      layout_column_wrap(
        width = 1/2,
        height = "auto",
        max_height = "100%",
          # Right column: Chart settings and export
          create_chart_settings_card(),
        layout_column_wrap(
          width = 1,
          heights_equal = "row",
          create_status_value_boxes()
        ),
        create_export_card(),
        # )
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
      style = "padding: 10px; height: 100%; display: flex; flex-direction: column;",
      
      # Rhandsontable with dynamic height
      div(
        id = "table_container",
        class = "dynamic-table-container",
        style = "border: 1px solid #ddd; border-radius: 5px; background-color: white; height: 100%; overflow: hidden; display: flex; flex-direction: column;",
        rhandsontable::rHandsontableOutput("main_data_table")
      ),
      
      # Tabel info
<<<<<<< HEAD
      # div(
      #   style = "margin-top: 10px; font-size: 0.85rem; color: #666; text-align: center;",
      #   icon("info-circle"),
      #   " Dobbeltklik på ", strong("kolonnenavn"), " for at redigere • Dobbeltklik på celle for data • Højreklik for menu",
      #   br(),
      #   " Alternativt: Brug redigér-knappen ", icon("edit"), " for modal dialog",
      #   br(),
      #   strong("Dato-formater:"), " 01-01-2024, 1/1/2024, 01.01.24, 1 jan 2024, 2024-01-01 og mange flere"
      # )
=======
      div(
        style = "margin-top: 10px; font-size: 0.85rem; color: #666; text-align: center;",
        icon("info-circle"),
        # " Dobbeltklik på ", strong("kolonnenavn"), " for at redigere • Dobbeltklik på celle for data • Højreklik for menu",
        # br(),
        # " Alternativt: Brug redigér-knappen ", icon("edit"), " for modal dialog",
        # br(),
        strong("Dato-formater:"), " 01-01-2024, 1/1/2024, 01.01.24, 1 jan 2024, 2024-01-01 og mange flere"
      )
>>>>>>> 5e4b32177e85caf04c58eda5ee8e975d2d5f861b
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
        
        # Chart type and target value side by side
        layout_columns(
          col_widths = c(7, 5),
          
          # Chart type selection
          selectInput(
            "chart_type",
            "Diagram type:",
            choices = CHART_TYPES_DA,
            selected = "Seriediagram (Run Chart)"
          ),
          
          # Target value input (moved up)
          div(
            textInput(
              "target_value",
              "Målværdi:",
              value = "",
              placeholder = "fx 85%, 0,85 eller 25"
            ),
            div(
              style = "font-size: 0.7rem; color: #666; margin-top: -5px;",
              icon("info-circle", style = "font-size: 0.6rem;"),
              " Valgfri målværdi"
            )
          )
        ),
        
        # Collapsible column mapping section
        create_column_mapping_section()
      )
    )
  )
}

create_column_mapping_section <- function() {
  bslib::accordion(
    id = "column_mapping_accordion",
    open = FALSE,  # Start collapsed
    bslib::accordion_panel(
      title = tagList(icon("columns"), " Kolonne Mapping"),
      value = "column_mapping",
      
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
    
    # Skift column
    selectInput(
      "skift_column",
      "Skift (fase-markering):",
      choices = NULL,
      selected = NULL
    ),
    
    div(
      style = "font-size: 0.8rem; color: #666; margin-top: 5px; margin-bottom: 10px;",
      icon("info-circle"),
      " Valgfri: Kolonne til markering af processkift eller faser"
    ),
    
    # Kommentar column  
    selectInput(
      "kommentar_column",
      "Kommentar (noter):",
      choices = NULL,
      selected = NULL
    ),
    
    div(
      style = "font-size: 0.8rem; color: #666; margin-top: 5px; margin-bottom: 15px;",
      icon("info-circle"),
      " Valgfri: Kolonne med kommentarer eller noter til datapunkter"
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
  )
}

# New function for plot-only card
create_plot_only_card <- function() {
  card(
    full_screen = TRUE,
    card_header(
      div(
        icon("chart-line"),
        " SPC Graf",
        style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 600;")
      )
    ),
    card_body(
      visualizationModuleUI("visualization")
    )
  )
}

# New function for status value boxes
create_status_value_boxes <- function() {
  visualizationStatusUI("visualization")
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