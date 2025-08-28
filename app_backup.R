source("global.R")
source("R/modules/data_module.R")
source("R/modules/visualization_module.R")
# NYT: Tilføj local storage modul
source("R/modules/local_storage_module.R")

#UI -----
# Define UI with enhanced bslib theming
ui <- page_navbar(
  title = tagList(
    # if (file.exists(HOSPITAL_LOGO_PATH)) {
    img(
      src = basename(HOSPITAL_LOGO_PATH),
      height = "40px",
      style = "margin-right: 10px;",
      onerror = "this.style.display='none'" # Hide if image fails to load
    ),
    # },
    div("BFH SPC-værktøj", style = "position: absolute; right: 20px; top: 20px; font-weight: bold;")
  ),
  theme = my_theme,
  inverse = FALSE,
  
  # NYT: Tilføj JavaScript til head section
  tags$head(
    tags$script(HTML(localStorage_js)),
    tags$script(HTML("
      // FIXED: Custom message handlers med bedre error handling
      Shiny.addCustomMessageHandler('saveAppState', function(message) {
        console.log('Received saveAppState message:', message);
        var success = window.saveAppState(message.key, message.data);
        if (!success) {
          console.error('saveAppState failed for key:', message.key);
        }
      });
      
      Shiny.addCustomMessageHandler('loadAppState', function(message) {
        console.log('Received loadAppState message:', message);
        var data = window.loadAppState(message.key);
        Shiny.setInputValue('loaded_app_state', data, {priority: 'event'});
      });
      
      Shiny.addCustomMessageHandler('clearAppState', function(message) {
        console.log('Received clearAppState message:', message);
        var success = window.clearAppState(message.key);
        if (!success) {
          console.error('clearAppState failed for key:', message.key);
        }
      });
      
      // FIXED: Bedre timing for check af existing data
      $(document).ready(function() {
        // Sæt app som initialiseret efter kort delay
        setTimeout(function() {
          Shiny.setInputValue('app_initialized', true, {priority: 'event'});
        }, 500);
        
        // UPDATED: Auto-load existing data ved app start
        if (window.hasAppState('current_session')) {
          setTimeout(function() {
            console.log('Auto-loading saved session data...');
            var data = window.loadAppState('current_session');
            if (data) {
              console.log('Found saved session data, triggering auto-restore');
              Shiny.setInputValue('auto_restore_data', data, {priority: 'event'});
            }
          }, 1500); // Delay for at sikre Shiny er klar
        }
      });
    ")),
    
    tags$style(HTML(
      paste0("
       .status-ready { background-color: ", HOSPITAL_COLORS$success, "; }
       .status-warning { background-color: ", HOSPITAL_COLORS$warning, "; }
       .status-error { background-color: ", HOSPITAL_COLORS$danger, "; }
       .status-processing { background-color: ", HOSPITAL_COLORS$primary, "; }
       
       /* Fix 1: Bredere kolonner i rhandsontable */
      .handsontable .htCore td {
        min-width: 120px !important;
        width: 120px !important;
        border-color: #ddd !important;
      }

      .handsontable .htCore th {
        min-width: 120px !important;
        width: 120px !important;
        background-color: ", HOSPITAL_COLORS$light, " !important;
        color: ", HOSPITAL_COLORS$dark, " !important;
        font-weight: 600 !important;
      }
             
      ")))
  ),
  
  # Enable shinyjs
  shinyjs::useShinyjs(),
  
  # -------------------------------------------------------------------------
  # TAB 1: ANALYSE
  # -------------------------------------------------------------------------
  nav_panel(
    title = NULL,
    
    # Layout med smal sidebar + 2-kolonne main area
    layout_sidebar(
      sidebar = sidebar(
        title = div(
          icon("upload"), 
          " Upload & Metadata",
          style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 600;")
        ),
        width = "400px",
        position = "left",
        open = TRUE,
        collapsible = TRUE,
        
        # Upload sektion
        div(
          h6("Data Upload:", style = "font-weight: 500; margin-bottom: 10px;"),
          
          fileInput(
            "data_file",
            NULL,
            accept = c(".csv", ".xlsx", ".xls"),
            placeholder = "Vælg fil..."
          ),
          
          # Toggle button for import settings
          div(
            actionButton(
              "toggle_import_settings",
              "Import indstillinger",
              icon = icon("cog"),
              class = "btn-outline-secondary btn-sm w-100"
            )
          ),
          
          # Import settings panel - completely hidden by default
          div(
            id = "import_settings_panel",
            style = "display: none; border: 1px solid #dee2e6; border-radius: 5px; padding: 15px; margin: 10px 0; background-color: #f8f9fa;",
            
            selectInput(
              "separator",
              "Separator:",
              choices = list(
                "Semikolon (;)" = ";",
                "Komma (,)" = ",", 
                "Tab" = "\t"
              ),
              selected = ";"
            ),
            
            selectInput(
              "decimal",
              "Decimal:",
              choices = list(
                "Komma (,)" = ",",
                "Punktum (.)" = "."
              ),
              selected = ","
            ),
            
            actionButton(
              "reimport",
              "Genindlæs fil",
              icon = icon("refresh"),
              class = "btn-outline-primary btn-sm w-100"
            )
          )
        ),
        
        hr(),
        
        # Indikator metadata
        div(
          h6("Indikator Information:", style = "font-weight: 500; margin-bottom: 15px;"),
          
          # Titel
          textInput(
            "indicator_title",
            "Indikator titel:",
            value = "",
            placeholder = "F.eks. Infektionsrate pr. 1000 patientdage"
          ),
          
          # Organisatorisk enhed
          div(
            style = "margin-bottom: 15px;",
            tags$label("Organisatorisk enhed:", style = "font-weight: 500;"),
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
          ),
          
          # Beskrivelse
          textAreaInput(
            "indicator_description",
            "Indikatorbeskrivelse:",
            value = "",
            placeholder = "Beskriv kort hvad indikatoren måler, hvordan data indsamles, og hvad målsætningen er...",
            height = "100px",
            resize = "vertical"
          )
        ),
        
        hr(),
        
        # NYT: Session management sektion
        div(
          h6("Session:", style = "font-weight: 500; margin-bottom: 10px;"),
          div(
            class = "btn-group w-100",
            actionButton(
              "manual_save", 
              "Gem session", 
              icon = icon("save"), 
              class = "btn-outline-primary btn-sm"
            ),
            actionButton(
              "clear_saved", 
              "Start ny session", 
              icon = icon("refresh"), 
              class = "btn-outline-warning btn-sm"
            )
          ),
          div(
            id = "save_status",
            style = "margin-top: 8px; font-size: 0.8rem; color: #666;",
            uiOutput("save_status_display")
          )
        ),
        
        hr(),
        
        # Data status
        div(
          uiOutput("data_status_display")
        )
      ),
      
      # Main content area - 2 kolonner
      fluidRow(
        # VENSTRE: Data tabel (~50%)
        column(
          6,
          card(
            full_screen = TRUE,
            card_header(
              div(
                style = "display: flex; justify-content: space-between; align-items: center;",
                div(
                  icon("table"),
                  " Data Tabel",
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
                style = "border: 1px solid #ddd; border-radius: 5px; background-color: white; height = '100%'",
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
          ),
          
          br(),
          
          # Graf indstillinger flyttet hertil
          conditionalPanel(
            condition = "output.has_data == true",
            card(
              card_header(
                div(
                  icon("sliders-h"), 
                  " Graf Indstillinger",
                  style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 500;")
                )
              ),
              card_body(
                style = "overflow: visible;",  # Fix for dropdown clipping
                
                # Chart type selection
                selectInput(
                  "chart_type",
                  "Diagram type:",
                  choices = CHART_TYPES_DA,
                  selected = "Seriediagram (Run Chart)"
                ),
                
                # Column mapping section
                div(
                  style = "margin: 15px 0; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
                  h6("Kolonne Mapping:", style = "font-weight: 500; margin-bottom: 10px;"),
                  
                  # X-axis column
                  selectInput(
                    "x_column",
                    "X-akse (tid/observation):",
                    choices = NULL,  # Will be populated server-side
                    selected = NULL
                  ),
                  
                  # Y-axis column  
                  selectInput(
                    "y_column",
                    "Y-akse (værdi):",
                    choices = NULL,
                    selected = NULL
                  ),
                  
                  # N column (for P/U charts)
                  conditionalPanel(
                    condition = "input.chart_type == 'P-kort (Andele)' || input.chart_type == \"P'-kort (Andele, standardiseret)\" || input.chart_type == 'U-kort (Rater)' || input.chart_type == \"U'-kort (Rater, standardiseret)\"",
                    selectInput(
                      "n_column",
                      "Nævner (n):",
                      choices = NULL,
                      selected = NULL
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
                ),
                
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
        ),
        
        # HØJRE: Graf og indstillinger (~50%)
        column(
          6,
          # Graf sektion
          card(
            card_header(
              div(
                icon("chart-line"),
                " SPC Visualisering",
                style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 600;")
              )
            ),
            card_body(
              min_height = "400px",
              
              # FIX: Always show visualization module UI statically
              visualizationModuleUI("visualization")
            )
          ),
          
          br(),
          
          # Eksport sektion
          conditionalPanel(
            condition = "output.plot_ready == true",
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
        )
      )
    )
  )
)

# SERVER ----