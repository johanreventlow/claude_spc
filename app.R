source("global.R")
source("R/modules/data_module.R")
source("R/modules/visualization_module.R")

# Define UI with enhanced bslib theming
ui <- page_navbar(
  title = div(
    style = "display: flex; align-items: center;",
    # Conditional logo display - only show if file exists
    conditionalPanel(
      condition = "true", # Will be made smarter later
      img(
        src = basename(HOSPITAL_LOGO_PATH), 
        height = "30px", 
        style = "margin-right: 10px;",
        onerror = "this.style.display='none'" # Hide if image fails to load
      )
    ),
    paste("SPC Analyse -", HOSPITAL_NAME)
  ),
  
  # Custom theme med hospital branding
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",  # Clean, professional base theme
    
    # Primary farver
    primary = HOSPITAL_COLORS$primary,
    secondary = HOSPITAL_COLORS$secondary,
    success = HOSPITAL_COLORS$success,
    info = HOSPITAL_COLORS$primary,
    warning = HOSPITAL_COLORS$warning,
    danger = HOSPITAL_COLORS$danger,
    light = HOSPITAL_COLORS$light,
    dark = HOSPITAL_COLORS$dark,
    
    # Typography
    base_font = font_google("Source Sans Pro"),
    heading_font = font_google("Source Sans Pro", wght = 600),
    code_font = font_google("Source Code Pro"),
    
    # Custom CSS variables
    "navbar-brand-font-size" = "1.1rem",
    "card-border-radius" = "0.5rem"
  ),
  
  # Custom CSS for additional styling
  tags$head(
    tags$style(HTML(paste0("
      /* Custom hospital branding */
      .navbar-brand {
        font-weight: 600 !important;
        color: ", HOSPITAL_COLORS$primary, " !important;
      }
      
      /* Sidebar styling */
      .bslib-sidebar-layout > .sidebar {
        background-color: #f8f9fa;
        border-right: 2px solid ", HOSPITAL_COLORS$light, ";
      }
      
      /* Card styling */
      .card {
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        border: 1px solid ", HOSPITAL_COLORS$light, ";
      }
      
      /* Button styling */
      .btn-outline-primary {
        color: ", HOSPITAL_COLORS$primary, ";
        border-color: ", HOSPITAL_COLORS$primary, ";
      }
      
      .btn-outline-primary:hover {
        background-color: ", HOSPITAL_COLORS$primary, ";
        border-color: ", HOSPITAL_COLORS$primary, ";
      }
      
      /* Loading spinner farver */
      .lds-dual-ring:after {
        border-color: ", HOSPITAL_COLORS$primary, " transparent ", HOSPITAL_COLORS$primary, " transparent;
      }
      
      /* Success/warning alerts */
      .alert-success {
        background-color: ", HOSPITAL_COLORS$success, "20;
        border-color: ", HOSPITAL_COLORS$success, ";
        color: ", HOSPITAL_COLORS$dark, ";
      }
      
      .alert-warning {
        background-color: ", HOSPITAL_COLORS$warning, "20;
        border-color: ", HOSPITAL_COLORS$warning, ";
        color: ", HOSPITAL_COLORS$dark, ";
      }
      
      /* Responsive design */
      @media (max-width: 768px) {
        .navbar-brand {
          font-size: 0.9rem;
        }
        .card {
          margin-bottom: 1rem;
        }
      }
      
      /* Status indicators */
      .status-indicator {
        display: inline-block;
        width: 12px;
        height: 12px;
        border-radius: 50%;
        margin-right: 8px;
      }
      
      .status-ready { background-color: ", HOSPITAL_COLORS$success, "; }
      .status-warning { background-color: ", HOSPITAL_COLORS$warning, "; }
      .status-error { background-color: ", HOSPITAL_COLORS$danger, "; }
      .status-processing { background-color: ", HOSPITAL_COLORS$primary, "; }
      
      /* rhandsontable styling */
      .handsontable {
        font-size: 12px !important;
      }
      
      .handsontable .htCore td {
        border-color: #ddd !important;
      }
      
      .handsontable .htCore th {
        background-color: ", HOSPITAL_COLORS$light, " !important;
        color: ", HOSPITAL_COLORS$dark, " !important;
        font-weight: 600 !important;
      }
      
      /* Validation styling for table cells */
      .htInvalid {
        background-color: ", HOSPITAL_COLORS$danger, "20 !important;
      }
      
      .htChanged {
        background-color: ", HOSPITAL_COLORS$warning, "20 !important;
      }
    ")))
  ),
  
  # Enable shinyjs
  shinyjs::useShinyjs(),
  
  # -------------------------------------------------------------------------
  # TAB 1: ANALYSE
  # -------------------------------------------------------------------------
  nav_panel(
    title = "Analyse",
    icon = icon("chart-line"),
    
    layout_sidebar(
      sidebar = sidebar(
        title = div(
          icon("sliders-h"), 
          " Kontrolpanel",
          style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 600;")
        ),
        width = 320,
        
        # DATA MODULE
        dataModuleUI("data_upload"),
        
        # VISUALISERING SEKTION  
        conditionalPanel(
          condition = "output.has_data == true",
          card(
            card_header(
              div(
                icon("chart-bar"), 
                " Visualisering",
                style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 500;")
              )
            ),
            card_body(
              selectInput(
                "chart_type",
                "Diagram type:",
                choices = CHART_TYPES_DA,
                selected = "run"
              ),
              
              checkboxInput(
                "show_targets",
                "Vis målsætninger",
                value = FALSE
              ),
              
              checkboxInput(
                "show_phases", 
                "Vis faser/interventioner",
                value = FALSE
              ),
              
              hr(),
              
              h6("Avancerede indstillinger:", style = "font-weight: 500; font-size: 0.9rem;"),
              div(
                style = "font-size: 0.85rem; color: #666;",
                icon("info-circle"),
                " Grafen opdateres automatisk når indstillinger ændres"
              )
            )
          )
        ),
        
        # EKSPORT SEKTION
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
              div(
                span(class = "status-indicator status-ready"),
                "Graf klar til eksport",
                style = "font-size: 0.9rem; color: #666; margin-bottom: 15px;"
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
                class = "btn-outline-primary w-100 mb-2"
              ),
              
              downloadButton(
                "download_data",
                "Download Data",
                icon = icon("table"),
                class = "btn-outline-primary w-100"
              )
            )
          )
        ),
        
        # EXPORT PLACEHOLDER når ingen graf
        conditionalPanel(
          condition = "output.plot_ready == false",
          card(
            card_header(
              div(
                icon("download"), 
                " Eksport",
                style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 500;")
              )
            ),
            card_body(
              div(
                span(class = "status-indicator status-warning"),
                "Venter på graf...",
                style = "font-size: 0.9rem; color: #666; margin-bottom: 15px;"
              ),
              
              div(
                style = "opacity: 0.5;",
                div("Download PNG", class = "btn btn-outline-secondary w-100 mb-2", style = "pointer-events: none;"),
                div("Download PDF Rapport", class = "btn btn-outline-secondary w-100 mb-2", style = "pointer-events: none;"),
                div("Download Data", class = "btn btn-outline-secondary w-100", style = "pointer-events: none;")
              )
            )
          )
        )
      ),
      
      # Main content
      card(
        full_screen = TRUE,
        card_header(
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            div(
              icon("chart-line"),
              " SPC Visualisering",
              style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 600;")
            ),
            div(
              # Dynamic status based on data and plot readiness
              conditionalPanel(
                condition = "output.plot_ready == true",
                span(class = "status-indicator status-ready"),
                "Graf klar",
                style = "font-size: 0.9rem;"
              ),
              conditionalPanel(
                condition = "output.has_data == true && output.plot_ready == false",
                span(class = "status-indicator status-processing"),
                "Genererer graf...",
                style = "font-size: 0.9rem;"
              ),
              conditionalPanel(
                condition = "output.has_data == false",
                span(class = "status-indicator status-warning"),
                "Venter på data",
                style = "font-size: 0.9rem;"
              )
            )
          )
        ),
        card_body(
          min_height = "500px",
          
          # Conditional content: show visualization when data is available
          conditionalPanel(
            condition = "output.has_data == true",
            visualizationModuleUI("visualization")
          ),
          
          # Placeholder when no data
          conditionalPanel(
            condition = "output.has_data == false",
            div(
              id = "plot_container",
              style = "text-align: center; margin-top: 80px;",
              
              img(
                src = basename(HOSPITAL_LOGO_PATH), 
                height = "80px", 
                style = "opacity: 0.2; margin-bottom: 30px;",
                onerror = "this.src='data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iODAiIGhlaWdodD0iODAiIHZpZXdCb3g9IjAgMCA4MCA4MCIgZmlsbD0ibm9uZSIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj4KPHJlY3Qgd2lkdGg9IjgwIiBoZWlnaHQ9IjgwIiBmaWxsPSIjRjhGOUZBIiBzdHJva2U9IiNEQ0REREYiIHN0cm9rZS13aWR0aD0iMiIvPgo8dGV4dCB4PSI0MCIgeT0iNDUiIGZvbnQtZmFtaWx5PSJBcmlhbCIgZm9udC1zaXplPSIxMiIgZmlsbD0iIzY2NiIgdGV4dC1hbmNob3I9Im1pZGRsZSI+TE9HTzwvdGV4dD4KPHN2Zz4K'; this.style.opacity='0.1';"
              ),
              
              h3(
                "SPC Analyse Dashboard", 
                style = paste("color:", HOSPITAL_COLORS$primary, "; margin-bottom: 20px;")
              ),
              
              p(
                "Upload data i sidepanelet for at begynde analysen",
                style = paste("color:", HOSPITAL_COLORS$secondary, "; font-size: 1.1rem;")
              ),
              
              br(),
              
              div(
                style = paste("background-color:", HOSPITAL_COLORS$light, "; padding: 20px; border-radius: 8px; max-width: 600px; margin: 0 auto;"),
                h5("SPC funktioner:", style = paste("color:", HOSPITAL_COLORS$primary)),
                tags$ul(
                  style = "text-align: left; color: #666;",
                  tags$li("✅ Automatisk seriediagram generering"),
                  tags$li("✅ Anhøj-regler for signaldetektion"), 
                  tags$li("✅ Interaktive kontrolkort"),
                  tags$li("✅ Real-time dataredigering"),
                  tags$li("⚙️ Professionel rapport-eksport")
                )
              )
            )
          )
        )
      )
    )
  ),
  
  # -------------------------------------------------------------------------
  # TAB 2: DATA REDIGERING
  # -------------------------------------------------------------------------
  nav_panel(
    title = "Data",
    icon = icon("table"),
    
    div(
      style = "padding: 20px;",
      
      # Editable table interface
      editableTableUI("editable_data")
    )
  ),
  
  # -------------------------------------------------------------------------
  # TAB 3: DIAGNOSTIK
  # -------------------------------------------------------------------------
  nav_panel(
    title = "Diagnostik",
    icon = icon("stethoscope"),
    
    div(
      style = "padding: 20px;",
      
      # Show diagnostics when plot is ready
      conditionalPanel(
        condition = "output.plot_ready == true",
        
        fluidRow(
          column(
            6,
            card(
              card_header(
                div(
                  icon("search"),
                  " Anhøj Regler",
                  style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 600;")
                )
              ),
              card_body(
                div(
                  span(class = "status-indicator status-ready"),
                  "Analyse komplet",
                  style = "font-size: 1rem; margin-bottom: 15px;"
                ),
                h6("Tests udført:", style = "font-weight: 500;"),
                tags$ul(
                  style = "color: #666;",
                  tags$li("✅ Unusually long runs"),
                  tags$li("✅ Unusually few crossings"),
                  tags$li("✅ Shift detection")
                ),
                br(),
                div(
                  style = paste("background-color:", HOSPITAL_COLORS$light, "; padding: 10px; border-radius: 5px;"),
                  icon("info-circle"),
                  " Se detaljerede resultater i Analyse-fanen under grafen."
                )
              )
            )
          ),
          
          column(
            6,
            card(
              card_header(
                div(
                  icon("exclamation-triangle"),
                  " Signal Detektion",
                  style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 600;")
                )
              ),
              card_body(
                div(id = "signal_summary"),
                h6("Signal typer:", style = "font-weight: 500;"),
                tags$ul(
                  style = "color: #666;",
                  tags$li("Special cause variation"),
                  tags$li("Systematic patterns"),
                  tags$li("Process shifts")
                ),
                br(),
                div(
                  style = paste("background-color:", HOSPITAL_COLORS$success, "20; padding: 10px; border-radius: 5px;"),
                  icon("check-circle"),
                  " Signaler markeres visuelt på grafen med farver og forklaringer."
                )
              )
            )
          )
        )
      ),
      
      # Show placeholder when no analysis available
      conditionalPanel(
        condition = "output.plot_ready == false",
        
        fluidRow(
          column(
            6,
            card(
              card_header(
                div(
                  icon("search"),
                  " Anhøj Regler",
                  style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 600;")
                )
              ),
              card_body(
                div(
                  span(class = "status-indicator status-warning"),
                  "Venter på data for analyse",
                  style = "font-size: 1rem; margin-bottom: 15px;"
                ),
                h6("Tests der vil blive udført:", style = "font-weight: 500;"),
                tags$ul(
                  style = "color: #666;",
                  tags$li("Unusually long runs"),
                  tags$li("Unusually few crossings"),
                  tags$li("Shift detection")
                )
              )
            )
          ),
          
          column(
            6,
            card(
              card_header(
                div(
                  icon("exclamation-triangle"),
                  " Signal Detektion",
                  style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 600;")
                )
              ),
              card_body(
                div(
                  span(class = "status-indicator status-warning"),
                  "Ingen signaler at rapportere",
                  style = "font-size: 1rem; margin-bottom: 15px;"
                ),
                h6("Signal typer:", style = "font-weight: 500;"),
                tags$ul(
                  style = "color: #666;",
                  tags$li("Points beyond control limits"),
                  tags$li("Systematic patterns"),
                  tags$li("Special cause variation")
                )
              )
            )
          )
        )
      ),
      
      br(),
      
      card(
        full_screen = TRUE,
        card_header(
          div(
            icon("clipboard-list"),
            " Detaljeret Diagnostik Rapport",
            style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 600;")
          )
        ),
        card_body(
          min_height = "300px",
          
          conditionalPanel(
            condition = "output.plot_ready == true",
            div(
              style = "text-align: center; margin-top: 50px;",
              h4("📊 SPC Analyse Komplet", style = paste("color:", HOSPITAL_COLORS$primary)),
              p("Detaljerede Anhøj-regel resultater vises under grafen i Analyse-fanen.", 
                style = paste("color:", HOSPITAL_COLORS$secondary)),
              br(),
              div(
                style = paste("background-color:", HOSPITAL_COLORS$success, "20; padding: 20px; border-radius: 8px; max-width: 600px; margin: 0 auto;"),
                h6("✅ Implementeret analyse:", style = paste("color:", HOSPITAL_COLORS$primary)),
                tags$ul(
                  style = "text-align: left; color: #666;",
                  tags$li("✅ Automatisk Anhøj-regel evaluering"),
                  tags$li("✅ Statistisk signifikans tests"),
                  tags$li("✅ Runs og crossings analyse"),
                  tags$li("✅ Visual signal markering"),
                  tags$li("✅ Forklarende tekst og anbefalinger")
                )
              )
            )
          ),
          
          conditionalPanel(
            condition = "output.plot_ready == false",
            div(
              style = "text-align: center; margin-top: 80px;",
              h4("SPC Diagnostik", style = paste("color:", HOSPITAL_COLORS$primary)),
              p("Upload data og vælg diagram-type for at starte analysen", 
                style = paste("color:", HOSPITAL_COLORS$secondary)),
              br(),
              div(
                style = paste("background-color:", HOSPITAL_COLORS$light, "; padding: 20px; border-radius: 8px; max-width: 600px; margin: 0 auto;"),
                h6("Kommende analyse:", style = paste("color:", HOSPITAL_COLORS$primary)),
                tags$ul(
                  style = "text-align: left; color: #666;",
                  tags$li("Automatisk Anhøj-regel evaluering"),
                  tags$li("Statistisk signifikans tests"),
                  tags$li("Process capability analyse"),
                  tags$li("Trend og shift detektion"),
                  tags$li("Forklarende tekst og anbefalinger")
                )
              )
            )
          )
        )
      )
    )
  ),
  
  # -------------------------------------------------------------------------
  # TAB 4: HJÆLP
  # -------------------------------------------------------------------------
  nav_panel(
    title = "Hjælp",
    icon = icon("question-circle"),
    
    div(
      style = "padding: 20px;",
      
      fluidRow(
        column(
          6,
          card(
            card_header(
              div(
                icon("book"),
                " SPC Grundprincipper",
                style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 600;")
              )
            ),
            card_body(
              h6("Statistisk Process Kontrol (SPC):", style = "font-weight: 500;"),
              p("SPC bruges til at monitere processer over tid og identificere variationer der kræver handling.", 
                style = "font-size: 0.9rem;"),
              
              h6("Anhøj Regler:", style = "font-weight: 500; margin-top: 15px;"),
              tags$ul(
                style = "font-size: 0.9rem;",
                tags$li(strong("Long runs:"), " Flere datapunkter i træk på samme side af centerlinjen"),
                tags$li(strong("Few crossings:"), " For få krydsninger af centerlinjen relativt til forventet")
              )
            )
          )
        ),
        
        column(
          6,
          card(
            card_header(
              div(
                icon("play-circle"),
                " Hurtig Start Guide",
                style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 600;")
              )
            ),
            card_body(
              h6("Sådan kommer du i gang:", style = "font-weight: 500;"),
              tags$ol(
                style = "font-size: 0.9rem;",
                tags$li("Upload din CSV/Excel fil i Analyse-fanen"),
                tags$li(strong("Redigér data"), " i Data-fanen efter behov"),
                tags$li("Vælg passende diagram-type"),
                tags$li("Gennemgå diagnostik resultaterne"),
                tags$li("Download graf eller rapport")
              ),
              
              div(
                style = paste("background-color:", HOSPITAL_COLORS$success, "20; padding: 10px; border-radius: 5px; margin-top: 15px;"),
                icon("lightbulb"),
                strong(" Nyt i v3.0: "), "Fuld SPC graf-generering med Anhøj-regler implementeret!",
                style = "font-size: 0.85rem;"
              )
            )
          )
        )
      ),
      
      br(),
      
      # Data redigering help section
      card(
        card_header(
          div(
            icon("edit"),
            " Data Redigering Guide",
            style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 600;")
          )
        ),
        card_body(
          fluidRow(
            column(
              6,
              h6("Grundlæggende redigering:", style = "font-weight: 500;"),
              tags$ul(
                style = "font-size: 0.9rem;",
                tags$li(strong("Dobbeltklik"), " på celle for at redigere"),
                tags$li(strong("Tab/Enter"), " for at gå til næste celle"),
                tags$li(strong("Ctrl+Z"), " for fortryd (eller brug Fortryd-knappen)"),
                tags$li(strong("Højreklik"), " for kontekstmenu med copy/paste")
              )
            ),
            column(
              6,
              h6("Avancerede funktioner:", style = "font-weight: 500;"),
              tags$ul(
                style = "font-size: 0.9rem;",
                tags$li(strong("Tilføj rækker"), " med plus-knappen"),
                tags$li(strong("Slet rækker"), " med minus-knappen"),
                tags$li(strong("Beregnede kolonner"), " (procent, rater, gennemsnit)"),
                tags$li(strong("Reset til original"), " hvis du fortryder alt")
              )
            )
          ),
          
          div(
            style = paste("background-color:", HOSPITAL_COLORS$warning, "20; padding: 10px; border-radius: 5px; margin-top: 15px;"),
            icon("exclamation-triangle"),
            strong(" Vigtigt: "), "Ændringer er midlertidige indtil du eksporterer den modificerede data. Brug Reset-knappen for at gå tilbage til original.",
            style = "font-size: 0.85rem;"
          )
        )
      ),
      
      br(),
      
      card(
        card_header(
          div(
            icon("info"),
            paste(" Om", HOSPITAL_NAME, "SPC App"),
            style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 600;")
          )
        ),
        card_body(
          fluidRow(
            column(
              8,
              h6("Version Information:", style = "font-weight: 500;"),
              p(paste("Udviklet til", HOSPITAL_NAME), style = "font-size: 0.9rem;"),
              p("Baseret på qicharts2 og Anhøj-metoderne", style = "font-size: 0.9rem;"),
              p(paste("Bygget med R Shiny, bslib og rhandsontable"), style = "font-size: 0.9rem;"),
              
              br(),
              
              div(
                style = paste("color:", HOSPITAL_COLORS$success, "; font-size: 0.9rem; font-weight: 500;"),
                "✅ Phase 3: SPC grafer og Anhøj-regler implementeret!"
              ),
              div(
                style = paste("color:", HOSPITAL_COLORS$secondary, "; font-size: 0.85rem;"),
                "Næste: PDF rapport-generering og avanceret eksport..."
              )
            ),
            column(
              4,
              div(
                style = "text-align: center;",
                img(
                  src = basename(HOSPITAL_LOGO_PATH), 
                  height = "100px", 
                  style = "opacity: 0.7;",
                  onerror = "this.src='data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCIgdmlld0JveD0iMCAwIDEwMCAxMDAiIGZpbGw9Im5vbmUiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI+CjxyZWN0IHdpZHRoPSIxMDAiIGhlaWdodD0iMTAwIiBmaWxsPSIjRjhGOUZBIiBzdHJva2U9IiNEQ0REREYiIHN0cm9rZS13aWR0aD0iMiIvPgo8dGV4dCB4PSI1MCIgeT0iNTUiIGZvbnQtZmFtaWx5PSJBcmlhbCIgZm9udC1zaXplPSIxNCIgZmlsbD0iIzY2NiIgdGV4dC1hbmNob3I9Im1pZGRsZSI+SE9TUElUQUw8L3RleHQ+CjwvdGV4dD4KPHN2Zz4K'; this.style.opacity='0.3';"
                ),
                br(), br(),
                div(
                  style = paste("font-size: 0.8rem; color:", HOSPITAL_COLORS$secondary, ";"),
                  format(Sys.Date(), "Version %Y.%m")
                )
              )
            )
          )
        )
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Initialize data module
  data_module <- dataModuleServer("data_upload")
  
  # Initialize editable table module
  edited_data <- editableTableServer("editable_data", data_module$data)
  
  # Use edited data if available, otherwise use original data
  active_data <- reactive({
    if(!is.null(edited_data()) && !is.null(data_module$data())) {
      edited_data()
    } else {
      data_module$data()
    }
  })
  
  # Initialize visualization module
  visualization <- visualizationModuleServer(
    "visualization",
    data_reactive = active_data,
    chart_type_reactive = reactive(input$chart_type),
    show_targets_reactive = reactive(input$show_targets),
    show_phases_reactive = reactive(input$show_phases)
  )
  
  # Has data check for conditional panels
  output$has_data <- reactive({
    !is.null(active_data())
  })
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)
  
  # Plot ready check for conditional panels
  output$plot_ready <- reactive({
    !is.null(visualization$plot_ready()) && visualization$plot_ready()
  })
  outputOptions(output, "plot_ready", suspendWhenHidden = FALSE)
  
  # Reaktiv status tracking
  status <- reactiveValues(
    data_uploaded = FALSE,
    plot_ready = FALSE,
    export_ready = FALSE
  )
  
  # Observe data upload success
  observe({
    if(!is.null(data_module$upload_success()) && data_module$upload_success()) {
      status$data_uploaded <- TRUE
      showNotification(
        div(
          icon("check-circle"),
          strong("Data uploadet succesfuldt!"),
          br(),
          "Gå til Data-fanen for at redigere tabellen direkte."
        ),
        type = "message",
        duration = 8
      )
    }
  })
  
  # Monitor plot readiness
  observe({
    if(!is.null(visualization$plot_ready()) && visualization$plot_ready()) {
      status$plot_ready <- TRUE
      status$export_ready <- TRUE
      showNotification(
        div(
          icon("chart-line"),
          strong("SPC graf genereret!"),
          br(),
          "Graf er klar til eksport og videre analyse."
        ),
        type = "message",
        duration = 5
      )
    }
  })
  
  # Monitor edited data changes
  observe({
    if(!is.null(edited_data()) && status$data_uploaded) {
      # Data has been edited - plot will regenerate automatically
      showNotification(
        "Data modificeret - graf opdateres automatisk",
        type = "message",
        duration = 3
      )
    }
  })
  
  # Welcome message kun én gang ved app start
  observe({
    showNotification(
      div(
        icon("check-circle"),
        paste("Velkommen til", HOSPITAL_NAME, "SPC Analyse App!"),
        br(),
        strong("Nyt: "), "Fuld SPC graf-generering med Anhøj-regler!"
      ),
      type = "message",
      duration = 10,
      closeButton = TRUE
    )
  }) %>% 
    bindEvent(session$clientData, once = TRUE)
  
  # Download handlers
  output$download_png <- downloadHandler(
    filename = function() paste0("spc_plot_", Sys.Date(), ".png"),
    content = function(file) {
      if(!is.null(visualization$plot()) && status$plot_ready) {
        ggsave(file, visualization$plot(), 
               width = 12, height = 8, dpi = 300, units = "in")
        showNotification("PNG eksporteret succesfuldt", type = "message", duration = 3)
      } else {
        showNotification("Ingen graf tilgængelig til eksport", type = "warning")
      }
    }
  )
  
  output$download_pdf <- downloadHandler(
    filename = function() paste0("spc_rapport_", Sys.Date(), ".pdf"),
    content = function(file) {
      showNotification("PDF rapport kommer i Phase 4", type = "warning")
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() paste0("spc_data_", Sys.Date(), ".csv"),
    content = function(file) {
      data_to_export <- active_data()
      
      if(!is.null(data_to_export)) {
        write.csv(data_to_export, file, row.names = FALSE)
        showNotification("Data eksporteret med alle redigeringer", type = "message", duration = 3)
      } else {
        showNotification("Ingen data at eksportere", type = "warning")
      }
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)