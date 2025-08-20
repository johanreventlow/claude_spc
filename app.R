source("global.R")

# Try different source paths to find modules
if(file.exists("R/modules/data_module.R")) {
  source("R/modules/data_module.R")
  source("R/modules/visualization_module.R")
  cat("✅ Loaded modules from R/modules/\n")
} else if(file.exists("data_module.R")) {
  source("data_module.R")
  source("visualization_module.R")
  cat("✅ Loaded modules from root directory\n")
} else {
  stop("❌ Could not find module files. Check your directory structure.")
}

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
    
    # Layout med collapsible sidebar
    layout_sidebar(
      sidebar = sidebar(
        title = div(
          icon("table"), 
          " Data & Upload",
          style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 600;")
        ),
        width = "60%",
        position = "left",
        open = TRUE,
        collapsible = TRUE,
        
        # Upload sektion
        fluidRow(
          column(
            6,
            fileInput(
              "data_file",
              "Upload CSV/Excel:",
              accept = c(".csv", ".xlsx", ".xls"),
              placeholder = "Vælg fil..."
            )
          ),
          column(
            6,
            # Collapsible import indstillinger
            div(
              actionButton(
                "toggle_import_settings",
                "Import indstillinger",
                icon = icon("cog"),
                class = "btn-outline-secondary btn-sm",
                style = "margin-top: 25px;"
              )
            )
          )
        ),
        
        # Collapsible import settings panel
        conditionalPanel(
          condition = "input.toggle_import_settings % 2 == 1",
          div(
            style = "border: 1px solid #dee2e6; border-radius: 5px; padding: 15px; margin: 10px 0; background-color: #f8f9fa;",
            
            fluidRow(
              column(6,
                     selectInput(
                       "separator",
                       "Separator:",
                       choices = list(
                         "Semikolon (;)" = ";",
                         "Komma (,)" = ",", 
                         "Tab" = "\t"
                       ),
                       selected = ";"
                     )
              ),
              column(6,
                     selectInput(
                       "decimal",
                       "Decimal:",
                       choices = list(
                         "Komma (,)" = ",",
                         "Punktum (.)" = "."
                       ),
                       selected = ","
                     )
              )
            ),
            
            actionButton(
              "reimport",
              "Genindlæs fil",
              icon = icon("refresh"),
              class = "btn-outline-primary btn-sm"
            )
          )
        ),
        
        hr(),
        
        # Data status
        div(
          id = "data_status",
          uiOutput("data_status_display")
        ),
        
        # Editérbar tabel
        div(
          style = "margin-top: 15px;",
          
          # Tabel kontrols
          fluidRow(
            column(
              8,
              h6("Data tabel:", style = "margin-bottom: 10px; font-weight: 500;")
            ),
            column(
              4,
              div(
                style = "text-align: right;",
                div(
                  class = "btn-group",
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
            )
          ),
          
          # Rhandsontable
          div(
            style = "border: 1px solid #ddd; border-radius: 5px; background-color: white;",
            rhandsontable::rHandsontableOutput("main_data_table", height = "400px")
          )
        )
      ),
      
      # Main content area (højre side)
      div(
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
            
            # Graf output
            conditionalPanel(
              condition = "output.has_data == true",
              visualizationModuleUI("visualization")
            ),
            
            # Placeholder når ingen data
            conditionalPanel(
              condition = "output.has_data == false",
              div(
                style = "text-align: center; margin-top: 80px;",
                icon("chart-line", style = "font-size: 3em; color: #ccc; margin-bottom: 20px;"),
                h5("Ingen graf endnu", style = paste("color:", HOSPITAL_COLORS$secondary)),
                p("Indtast data i tabellen eller upload en fil", style = "color: #666;")
              )
            )
          )
        ),
        
        br(),
        
        # Visualiserings indstillinger
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
              selectInput(
                "chart_type",
                "Diagram type:",
                choices = CHART_TYPES_DA,
                selected = "Seriediagram (Run Chart)"
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
              )
            )
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
        )
      )
    )
  ),
  
  # -------------------------------------------------------------------------
  # TAB 2: DATA REDIGERING
  # -------------------------------------------------------------------------
  
  
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
  
  # Reactive values for data håndtering
  values <- reactiveValues(
    current_data = NULL,
    original_data = NULL,
    file_uploaded = FALSE
  )
  
  # Initialize tom tabel med standard kolonner
  initialize_empty_table <- function() {
    data.frame(
      Dato = as.Date(character(0)),
      Taeller = numeric(0),
      Naevner = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  
  # Start med tom tabel
  observe({
    if (is.null(values$current_data)) {
      # Lav en tabel med 5 tomme rækker så brugeren kan se strukturen
      empty_data <- data.frame(
        Dato = rep(as.Date(NA), 5),
        Taeller = rep(NA_real_, 5), 
        Naevner = rep(NA_real_, 5),
        stringsAsFactors = FALSE
      )
      values$current_data <- empty_data
    }
  })
  
  # Data status display
  output$data_status_display <- renderUI({
    if (is.null(values$current_data)) {
      div(
        span(class = "status-indicator status-warning"),
        "Ingen data",
        style = "font-size: 0.9rem;"
      )
    } else if (values$file_uploaded) {
      data_rows <- sum(!is.na(values$current_data[[1]]))  # Count non-NA rows
      div(
        span(class = "status-indicator status-ready"),
        paste("Fil uploadet -", data_rows, "datapunkter"),
        style = "font-size: 0.9rem;"
      )
    } else {
      data_rows <- sum(!is.na(values$current_data[[1]]))
      if (data_rows > 0) {
        div(
          span(class = "status-indicator status-processing"),
          paste("Manuel indtastning -", data_rows, "datapunkter"),
          style = "font-size: 0.9rem;"
        )
      } else {
        div(
          span(class = "status-indicator status-warning"),
          "Tom tabel - indtast data eller upload fil",
          style = "font-size: 0.9rem;"
        )
      }
    }
  })
  
  # File upload handler
  observeEvent(input$data_file, {
    req(input$data_file)
    
    file_path <- input$data_file$datapath
    file_ext <- tools::file_ext(input$data_file$name)
    
    tryCatch({
      if (file_ext %in% c("xlsx", "xls")) {
        # Excel fil
        data <- readxl::read_excel(file_path, col_names = TRUE)
      } else {
        # CSV fil med danske indstillinger
        data <- readr::read_csv2(
          file_path,
          locale = readr::locale(
            decimal_mark = input$decimal %||% ",",
            grouping_mark = ".",
            encoding = "ISO-8859-1"
          ),
          show_col_types = FALSE
        )
      }
      
      # Konverter til data.frame og gem
      values$current_data <- as.data.frame(data)
      values$original_data <- as.data.frame(data)
      values$file_uploaded <- TRUE
      
      showNotification(
        paste("Fil uploadet:", nrow(data), "rækker,", ncol(data), "kolonner"),
        type = "message",
        duration = 3
      )
      
    }, error = function(e) {
      showNotification(
        paste("Fejl ved upload:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  # Re-import med nye indstillinger
  observeEvent(input$reimport, {
    req(input$data_file, values$original_data)
    
    file_path <- input$data_file$datapath
    
    tryCatch({
      data <- readr::read_delim(
        file_path,
        delim = input$separator,
        locale = readr::locale(
          decimal_mark = input$decimal,
          encoding = "ISO-8859-1"
        ),
        show_col_types = FALSE
      )
      
      values$current_data <- as.data.frame(data)
      values$original_data <- as.data.frame(data)
      
      showNotification("Fil genindlæst med nye indstillinger", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Fejl ved genindlæsning:", e$message), type = "error")
    })
  })
  
  # Hovedtabel rendering
  output$main_data_table <- rhandsontable::renderRHandsontable({
    req(values$current_data)
    
    data <- values$current_data
    
    hot <- rhandsontable::rhandsontable(
      data,
      height = 400,
      stretchH = "all",
      contextMenu = TRUE,
      manualColumnResize = TRUE,
      fillHandle = list(direction = "vertical", autoInsertRow = FALSE)
    ) %>%
      rhandsontable::hot_context_menu(
        allowRowEdit = TRUE,
        allowColEdit = TRUE
      ) %>%
      rhandsontable::hot_table(
        highlightCol = TRUE,
        highlightRow = TRUE
      )
    
    # Kolonne-specifik formatting
    for (i in 1:ncol(data)) {
      col_name <- names(data)[i]
      
      if (grepl("dato|date", col_name, ignore.case = TRUE)) {
        hot <- hot %>%
          rhandsontable::hot_col(col = i, type = "date", dateFormat = "DD-MM-YYYY")
      } else if (is.numeric(data[[i]])) {
        hot <- hot %>%
          rhandsontable::hot_col(col = i, type = "numeric", format = "0,0.00")
      } else {
        hot <- hot %>%
          rhandsontable::hot_col(col = i, type = "text")
      }
    }
    
    return(hot)
  })
  
  # Håndter tabel ændringer
  observeEvent(input$main_data_table, {
    req(input$main_data_table)
    values$current_data <- rhandsontable::hot_to_r(input$main_data_table)
  })
  
  # Tilføj kolonne
  observeEvent(input$add_column, {
    req(values$current_data)
    
    # Spørg brugeren om kolonnenavn
    showModal(modalDialog(
      title = "Tilføj ny kolonne",
      textInput("new_col_name", "Kolonnenavn:", value = "Ny_kolonne"),
      selectInput("new_col_type", "Type:", 
                  choices = list("Numerisk" = "numeric", "Tekst" = "text", "Dato" = "date")),
      footer = tagList(
        modalButton("Annuller"),
        actionButton("confirm_add_col", "Tilføj", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$confirm_add_col, {
    req(input$new_col_name, values$current_data)
    
    new_col_name <- input$new_col_name
    new_col_type <- input$new_col_type
    
    # Tilføj ny kolonne
    if (new_col_type == "numeric") {
      values$current_data[[new_col_name]] <- rep(NA_real_, nrow(values$current_data))
    } else if (new_col_type == "date") {
      values$current_data[[new_col_name]] <- rep(as.Date(NA), nrow(values$current_data))
    } else {
      values$current_data[[new_col_name]] <- rep(NA_character_, nrow(values$current_data))
    }
    
    removeModal()
    showNotification(paste("Kolonne", new_col_name, "tilføjet"), type = "message")
  })
  
  # Tilføj række
  observeEvent(input$add_row, {
    req(values$current_data)
    
    # Tilføj tom række
    new_row <- values$current_data[1, ]
    new_row[1, ] <- NA
    
    values$current_data <- rbind(values$current_data, new_row)
    
    showNotification("Ny række tilføjet", type = "message")
  })
  
  # Reset tabel
  observeEvent(input$reset_table, {
    if (values$file_uploaded && !is.null(values$original_data)) {
      values$current_data <- values$original_data
      showNotification("Tabel nulstillet til uploaded data", type = "message")
    } else {
      values$current_data <- data.frame(
        Dato = rep(as.Date(NA), 5),
        Taeller = rep(NA_real_, 5),
        Naevner = rep(NA_real_, 5),
        stringsAsFactors = FALSE
      )
      values$file_uploaded <- FALSE
      showNotification("Tabel nulstillet til tom tabel", type = "message")
    }
  })
  
  # Data for visualization modul
  active_data <- reactive({
    req(values$current_data)
    
    # Filtrer rækker hvor mindst én kolonne har data
    data <- values$current_data
    non_empty_rows <- apply(data, 1, function(row) any(!is.na(row)))
    
    if (any(non_empty_rows)) {
      return(data[non_empty_rows, ])
    } else {
      return(NULL)
    }
  })
  
  # Has data check
  output$has_data <- reactive({
    !is.null(active_data()) && nrow(active_data()) > 0
  })
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)
  
  # Initialize visualization module
  visualization <- visualizationModuleServer(
    "visualization",
    data_reactive = active_data,
    chart_type_reactive = reactive({
      chart_selection <- input$chart_type %||% "Seriediagram (Run Chart)"
      get_qic_chart_type(chart_selection)
    }),
    show_targets_reactive = reactive(input$show_targets %||% FALSE),
    show_phases_reactive = reactive(input$show_phases %||% FALSE)
  )
  
  # Plot ready check
  output$plot_ready <- reactive({
    !is.null(visualization$plot_ready()) && visualization$plot_ready()
  })
  outputOptions(output, "plot_ready", suspendWhenHidden = FALSE)
  
  # Download handlers (simpel versioner)
  output$download_png <- downloadHandler(
    filename = function() paste0("spc_plot_", Sys.Date(), ".png"),
    content = function(file) {
      if (!is.null(visualization$plot())) {
        ggsave(file, visualization$plot(), width = 12, height = 8, dpi = 300)
      }
    }
  )
  
  output$download_pdf <- downloadHandler(
    filename = function() paste0("spc_rapport_", Sys.Date(), ".pdf"),
    content = function(file) {
      showNotification("PDF rapport kommer i næste fase", type = "message")
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() paste0("spc_data_", Sys.Date(), ".csv"),
    content = function(file) {
      if (!is.null(active_data())) {
        write.csv(active_data(), file, row.names = FALSE)
      }
    }
  )
  
  # Velkommen besked
  observe({
    showNotification(
      paste("Velkommen til", HOSPITAL_NAME, "SPC App! Indtast data i tabellen eller upload en fil."),
      type = "message",
      duration = 5
    )
  }) %>% 
    bindEvent(session$clientData, once = TRUE)
  
  # Data overview outputs
  output$data_row_count <- renderText({
    data <- active_data()
    if (is.null(data)) "0" else as.character(nrow(data))
  })
  
  output$data_col_count <- renderText({
    data <- active_data()
    if (is.null(data)) "0" else as.character(ncol(data))
  })
  
  output$data_quality <- renderText({
    data <- active_data()
    if (is.null(data)) {
      "N/A"
    } else {
      missing_pct <- round(sum(is.na(data)) / (nrow(data) * ncol(data)) * 100)
      if (missing_pct < 10) {
        "God"
      } else if (missing_pct < 25) {
        "OK"
      } else {
        "Lav"
      }
    }
  })
  
  # Read-only data table
  output$readonly_data_table <- DT::renderDataTable({
    req(active_data())
    
    DT::datatable(
      active_data(),
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        scrollY = "300px",
        searching = TRUE,
        ordering = TRUE,
        info = TRUE,
        dom = 'frtip',
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover compact'
    )
  })
  
  # Navigation to Analyse tab
  observeEvent(input$go_to_analyse, {
    updateNavbarPage(session, inputId = "navbar", selected = "Analyse")
  })
}

# Run the application
shinyApp(ui = ui, server = server)