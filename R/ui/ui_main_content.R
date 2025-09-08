# R/ui/ui_main_content.R
# Main content area components

create_ui_main_content <- function() {
  tagList(
    # Welcome page when no meaningful data is loaded
    conditionalPanel(
      condition = "output.dataLoaded != 'TRUE'", 
      create_welcome_page()
    ),
    
    # Data table and visualization - only when user has started
    conditionalPanel(
      condition = "output.dataLoaded == 'TRUE'",
      
      
      # Main content in columns
      layout_columns(
        col_widths = c(6, 6, 6, 6),
        height = "auto",
        max_height = "100%",
        create_chart_settings_card(),
        # Left column: Data table and chart settings
        
        create_plot_only_card(),
        
        create_data_table_card(),
        # layout_column_wrap(
          # width = 1/2,  # 2 boxes per row - wider and easier to read
          # heights_equal = "row", 
          # fill = FALSE,
          # Status information as value boxes  
          create_status_value_boxes()
        # ),
        # create_export_card(),
      )
    )
  )
}




create_chart_settings_card <- function() {
  # conditionalPanel(
  #   condition = "output.has_data == 'true'",
  card(
    full_screen = TRUE,
    fillable = TRUE,
    card_header(
      div(
        icon("sliders-h"),
        " Indstillinger",
      )
    ),
    # Tab 1: Diagram settings
    card_body(
      class = "d-flex flex-column h-100",
      div(
        class = "flex-fill h-100",
        navset_tab(
        nav_panel(
          "Detaljer",
          # "Diagram",
          icon = icon("pen-to-square"),
          # icon = icon("cogs"),
          # icon = icon("chart-bar"),
          div(
            style = "padding: 10px 0;",
            #Chart type and target value side by side
            layout_column_wrap(
              width = 1/2,
              layout_column_wrap(
                width = 1,
                heights_equal = "row",
                # Indikator metadata
                textInput(
                  "indicator_title",
                  "Titel på indikator:",
                  value = "",
                  placeholder = "F.eks. 'Infektioner pr. 1000 sengedage'"
                ),
                
                # Target value input
                textInput(
                  "target_value",
                  "Målværdi:",
                  value = "",
                  placeholder = "fx 85%, 0,85 eller 25"
                ),
                
                
                # Chart type selection
                selectInput(
                  "chart_type",
                  "Diagram type:",
                  choices = CHART_TYPES_DA,
                  selected = "Seriediagram (Run Chart)"
                )
              ),
              
              # Beskrivelse
              textAreaInput(
                "indicator_description",
                "Datadefinition:",
                value = "",
                placeholder = "Beskriv kort hvad indikatoren måler, hvordan data indsamles, og hvad målsætningen er...",
                height = "200px",
                resize = "none"
              )
            )
          )
        ),
        
        # Tab 2: Organisatorisk enhed
        nav_panel(
          "Organisatorisk",
          icon = icon("building"),
          
          div(
            style = "padding: 10px 0;",
            # Organisatorisk enhed selection
            create_unit_selection()
          )
        ),
        
        # Tab 3: Column mapping (moved from accordion)
        nav_panel(
          "Kolonner",
          icon = icon("columns"),
          
          div(
            style = "padding: 10px 0;",
            
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
        ),
        # Tab 3: Additional settings (placeholder) 
        nav_panel(
          "Avanceret",
          icon = icon("cogs"),
          
          div(
            style = "padding: 20px; text-align: center; color: #666;",
            icon("wrench", style = "font-size: 2rem; margin-bottom: 10px;"),
            br(),
            "Yderligere indstillinger kommer her",
            br(),
            tags$small("Denne tab er reserveret til fremtidige features")
          )
        ) # nav_panel (Avanceret)
        ) # navset_tab
      ) # div wrapper
    ) # card_body
  ) # card
}


# New function for plot-only card
create_plot_only_card <- function() {
  card(
    full_screen = TRUE,
    fillable = TRUE,
    card_header(
      div(
        icon("chart-line"),
        " SPC Graf",
      )
    ),
    card_body(
      div(
        style ="height: 100%",
        fill = TRUE, 
        visualizationModuleUI("visualization")
      # ), 
      # div(
      #   style ="height: 10%",
      #   fill = TRUE,
      #   "TEKST"

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
        ),
        div(
          class = "btn-group",
          actionButton(
            "edit_column_names",
            label = NULL,
            icon = icon("edit"),
            title = "Redigér kolonnenavne",
            class = "btn-secondary btn-sm"
          ),
          actionButton(
            "add_column",
            label = NULL,
            icon = icon("plus"),
            title = "Tilføj kolonne",
            class = "btn-secondary btn-sm"
          ),
          actionButton(
            "add_row",
            label = NULL,
            icon = icon("plus-square"),
            title = "Tilføj række",
            class = "btn-secondary btn-sm"
          ),
          # actionButton(
          #   "reset_table",
          #   label = NULL,
          #   icon = icon("refresh"),
          #   title = "Reset tabel", 
          #   class = "btn-outline-warning btn-sm"
          # )
        )
      )
    ),
    card_body(
      # Data table using excelR
      excelR::excelOutput("main_data_table", height = "auto")
      
    )
  )
}


# New function for status value boxes
create_status_value_boxes <- function() {
  visualizationStatusUI("visualization")
}

create_unit_selection <- function() {
  div(
    style = "margin-bottom: 15px;",
    tags$label("Afdeling eller afsnit", style = "font-weight: 500;"),
    div(
      style = "margin-top: 5px;",
      radioButtons(
        "unit_type",
        NULL,
        choices = list(
          "Vælg fra liste" = "select",
          "Indtast selv" = "custom"
        ),
        selected = "select",
        inline = TRUE
      )
    ),
    
    # Dropdown for standard enheder
    conditionalPanel(
      condition = "input.unit_type == 'select'",
      selectInput(
        "unit_select",
        NULL,
        choices = list(
          "Vælg enhed..." = "",
          "Medicinsk Afdeling" = "med",
          "Kirurgisk Afdeling" = "kir",
          "Intensiv Afdeling" = "icu",
          "Ambulatorie" = "amb",
          "Akutmodtagelse" = "akut",
          "Pædiatrisk Afdeling" = "paed",
          "Gynækologi/Obstetrik" = "gyn"
        )
      )
    ),
    
    # Custom input
    conditionalPanel(
      condition = "input.unit_type == 'custom'",
      textInput(
        "unit_custom",
        NULL,
        placeholder = "Indtast enhedsnavn..."
      )
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