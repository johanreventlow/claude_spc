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
          
          # Collapsible import indstillinger
          div(
            actionButton(
              "toggle_import_settings",
              "Import indstillinger",
              icon = icon("cog"),
              class = "btn-outline-secondary btn-sm w-100"
            )
          ),
          
          # Collapsible import settings panel
          conditionalPanel(
            condition = "input.toggle_import_settings % 2 == 1",
            div(
              style = "border: 1px solid #dee2e6; border-radius: 5px; padding: 15px; margin: 10px 0; background-color: #f8f9fa;",
              
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
                style = "border: 1px solid #ddd; border-radius: 5px; background-color: white;",
                rhandsontable::rHandsontableOutput("main_data_table", height = "500px")
              ),
              
              # Tabel info
              div(
                style = "margin-top: 10px; font-size: 0.85rem; color: #666; text-align: center;",
                icon("info-circle"),
                " Dobbeltklik på ", strong("kolonnenavn"), " for at redigere • Dobbeltklik på celle for data • Højreklik for menu",
                br(),
                " Alternativt: Brug redigér-knappen ", icon("edit"), " for modal dialog"
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
                    condition = "input.chart_type == 'P-kort (Andele)' || input.chart_type == 'P\'-kort (Andele, standardiseret)' || input.chart_type == 'U-kort (Rater)' || input.chart_type == 'U\'-kort (Rater, standardiseret)'",
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
                  p("Indtast data i tabellen til venstre eller upload en fil", style = "color: #666;")
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
      
      # Kolonne mapping help section
      card(
        card_header(
          div(
            icon("columns"),
            " Kolonne Mapping Guide",
            style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 600;")
          )
        ),
        card_body(
          fluidRow(
            column(
              6,
              h6("Kolonne-valg:", style = "font-weight: 500;"),
              tags$ul(
                style = "font-size: 0.9rem;",
                tags$li(strong("X-akse"), " for tid/observation (datoer eller sekvensnumre)"),
                tags$li(strong("Y-akse"), " for værdier der skal analyseres"),
                tags$li(strong("Nævner (N)"), " kun for P/U-charts (totaler/denominators)"),
                tags$li(strong("Auto-detektér"), " finder intelligente forslag automatisk")
              )
            ),
            column(
              6,
              h6("Validering:", style = "font-weight: 500;"),
              tags$ul(
                style = "font-size: 0.9rem;",
                tags$li(strong("Numeriske kolonner"), " påkrævet for Y og N"),
                tags$li(strong("Unikke valg"), " - samme kolonne kan ikke bruges flere gange"),
                tags$li(strong("Chart-kompatibilitet"), " - P/U charts kræver nævner"),
                tags$li(strong("Øjeblikkelig feedback"), " viser advarsler eller bekræftelse")
              )
            )
          ),
          
          div(
            style = paste("background-color:", HOSPITAL_COLORS$primary, "10; padding: 10px; border-radius: 5px; margin-top: 15px;"),
            icon("magic"),
            strong(" Pro tip: "), "Brug Auto-detektér knappen for intelligente forslag baseret på kolonnenavne og datatyper. Du kan altid justere valgene manuelt bagefter.",
            style = "font-size: 0.85rem;"
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
                tags$li(strong("Vælg kolonner"), " for X-akse, Y-akse og Nævner (eller brug Auto-detektér)"),
                tags$li(strong("Redigér data"), " efter behov direkte i tabellen"),
                tags$li("Vælg passende diagram-type"),
                tags$li("Gennemgå diagnostik resultaterne"),
                tags$li("Download graf eller rapport")
              ),
              
              div(
                style = paste("background-color:", HOSPITAL_COLORS$success, "20; padding: 10px; border-radius: 5px; margin-top: 15px;"),
                icon("lightbulb"),
                strong(" Nyt i v3.1: "), "Eksplicit kolonne-valg! Vælg præcis hvilke kolonner der skal bruges til X, Y og N.",
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
                tags$li(strong("Dobbeltklik kolonnenavn"), " for at redigere headers direkte"),
                tags$li(strong("Dobbeltklik celle"), " for at redigere data"),
                tags$li(strong("Tab/Enter"), " for at gå til næste celle"),
                tags$li(strong("Højreklik"), " for kontekstmenu med copy/paste")
              )
            ),
            column(
              6,
              h6("Avancerede funktioner:", style = "font-weight: 500;"),
              tags$ul(
                style = "font-size: 0.9rem;",
                tags$li(strong("Redigér kolonnenavne"), " direkte i headers eller via modal"),
                tags$li(strong("Tilføj rækker/kolonner"), " med plus-knapperne"),
                tags$li(strong("Validering"), " forhindrer dubletter og tomme navne"),
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
# Forbedret server logic til app.R
# Erstat server function med denne kode

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
  
  # Reactive for aktuel organisatorisk enhed
  current_unit <- reactive({
    if (input$unit_type == "select") {
      unit_names <- list(
        "med" = "Medicinsk Afdeling",
        "kir" = "Kirurgisk Afdeling", 
        "icu" = "Intensiv Afdeling",
        "amb" = "Ambulatorie",
        "akut" = "Akutmodtagelse",
        "paed" = "Pædiatrisk Afdeling",
        "gyn" = "Gynækologi/Obstetrik"
      )
      selected_unit <- input$unit_select %||% ""
      if (selected_unit != "" && selected_unit %in% names(unit_names)) {
        return(unit_names[[selected_unit]])
      } else {
        return("")
      }
    } else {
      return(input$unit_custom %||% "")
    }
  })
  
  # Reactive for komplet graf titel
  chart_title <- reactive({
    base_title <- input$indicator_title %||% "SPC Analyse"
    unit_name <- current_unit()
    
    if (base_title != "" && unit_name != "") {
      return(paste(base_title, "-", unit_name))
    } else if (base_title != "") {
      return(base_title)
    } else if (unit_name != "") {
      return(paste("SPC Analyse -", unit_name))
    } else {
      return("SPC Analyse")
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
  
  # Hovedtabel rendering med aktiveret header-redigering
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
        allowColEdit = TRUE  # Tillader header-redigering
      ) %>%
      rhandsontable::hot_table(
        highlightCol = TRUE,
        highlightRow = TRUE
      ) %>%
      rhandsontable::hot_cols(
        columnHeaderHeight = 50,  # Gør headers højere for bedre redigering
        manualColumnResize = TRUE
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
  
  # Forbedret håndtering af tabel ændringer (både data og headers)
  observeEvent(input$main_data_table, {
    req(input$main_data_table)
    
    # Konverter hot til data.frame
    new_data <- rhandsontable::hot_to_r(input$main_data_table)
    
    # Tjek om kolonnenavne er ændret
    current_names <- names(values$current_data)
    new_names <- names(new_data)
    
    if (!identical(current_names, new_names)) {
      cat("DEBUG: Column names changed from:", paste(current_names, collapse = ", "), "\n")
      cat("DEBUG: Column names changed to:", paste(new_names, collapse = ", "), "\n")
      
      # Valider nye kolonnenavne
      if (length(new_names) != length(unique(new_names))) {
        showNotification(
          "Kolonnenavne skal være unikke. Ændring ignoreret.",
          type = "error",
          duration = 4
        )
        return()  # Ignorer ændringen hvis der er dubletter
      }
      
      # Tjek for tomme kolonnenavne
      if (any(is.na(new_names) | new_names == "" | trimws(new_names) == "")) {
        showNotification(
          "Kolonnenavne kan ikke være tomme. Ændring ignoreret.",
          type = "error", 
          duration = 4
        )
        return()
      }
      
      # Vis bekræftelse af kolonnenavn-ændringer
      changed_indices <- which(current_names != new_names)
      if (length(changed_indices) > 0) {
        change_summary <- paste(
          paste0("'", current_names[changed_indices], "' → '", new_names[changed_indices], "'"),
          collapse = ", "
        )
        
        showNotification(
          paste("Kolonnenavne opdateret:", change_summary),
          type = "message",
          duration = 4
        )
      }
    }
    
    # Opdater data med eventuelle nye kolonnenavne
    values$current_data <- new_data
  })
  
  # Redigér kolonnenavne modal
  observeEvent(input$edit_column_names, {
    req(values$current_data)
    
    current_names <- names(values$current_data)
    
    # Lav input felter for hver kolonne
    name_inputs <- lapply(1:length(current_names), function(i) {
      textInput(
        paste0("col_name_", i),
        paste("Kolonne", i, ":"),
        value = current_names[i],
        placeholder = paste("Navn for kolonne", i)
      )
    })
    
    showModal(modalDialog(
      title = "Redigér kolonnenavne",
      size = "m",
      
      div(
        style = "margin-bottom: 15px;",
        h6("Nuværende kolonnenavne:", style = "font-weight: 500;"),
        p(paste(current_names, collapse = ", "), style = "color: #666; font-style: italic;")
      ),
      
      div(
        style = "max-height: 300px; overflow-y: auto;",
        name_inputs
      ),
      
      footer = tagList(
        modalButton("Annuller"),
        actionButton("confirm_column_names", "Gem ændringer", class = "btn-primary")
      )
    ))
  })
  
  # Bekræft kolonnenavn-ændringer
  observeEvent(input$confirm_column_names, {
    req(values$current_data)
    
    current_names <- names(values$current_data)
    new_names <- character(length(current_names))
    
    # Saml nye navne fra input felter
    for (i in 1:length(current_names)) {
      input_value <- input[[paste0("col_name_", i)]]
      if (!is.null(input_value) && input_value != "") {
        new_names[i] <- trimws(input_value)
      } else {
        new_names[i] <- current_names[i]  # Bevar originalt navn hvis tomt
      }
    }
    
    # Tjek for dubletter
    if (any(duplicated(new_names))) {
      showNotification(
        "Kolonnenavne skal være unikke. Ret duplikater og prøv igen.",
        type = "error",
        duration = 5
      )
      return()
    }
    
    # Opdater kolonnenavne
    names(values$current_data) <- new_names
    
    removeModal()
    
    # Vis bekræftelse
    if (!identical(current_names, new_names)) {
      changed_cols <- which(current_names != new_names)
      change_summary <- paste(
        paste0("'", current_names[changed_cols], "' -> '", new_names[changed_cols], "'"),
        collapse = ", "
      )
      
      showNotification(
        paste("Kolonnenavne opdateret:", change_summary),
        type = "message",
        duration = 4
      )
      
      cat("DEBUG: Kolonnenavne ændret:", change_summary, "\n")
    } else {
      showNotification("Ingen ændringer i kolonnenavne", type = "message", duration = 2)
    }
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
  
  # Reset tabel - tøm helt for at starte forfra
  observeEvent(input$reset_table, {
    # Lav en helt tom tabel med basis struktur
    values$current_data <- data.frame(
      Dato = rep(as.Date(NA), 5),
      Taeller = rep(NA_real_, 5),
      Naevner = rep(NA_real_, 5),
      stringsAsFactors = FALSE
    )
    
    # Nulstil file upload status
    values$file_uploaded <- FALSE
    values$original_data <- NULL
    
    # Reset file upload felt
    shinyjs::reset("data_file")
    
    # Metadata (titel, beskrivelse etc.) bevares - de røres ikke
    
    showNotification(
      "Tabel og fil-upload tømt - indtast nye data eller upload ny fil. Titel og beskrivelse bevaret.", 
      type = "message", 
      duration = 4
    )
  })
  
  # Opdater kolonne-valg når data ændres
  observe({
    req(values$current_data)
    
    data <- values$current_data
    
    # Få alle kolonnenavne
    all_cols <- names(data)
    
    if (length(all_cols) > 0) {
      # Lav choices list med "Vælg..." som første option
      col_choices <- setNames(c("", all_cols), c("Vælg kolonne...", all_cols))
      
      # Opdater dropdown menuer
      updateSelectInput(session, "x_column", choices = col_choices)
      updateSelectInput(session, "y_column", choices = col_choices)
      updateSelectInput(session, "n_column", choices = col_choices)
      
      # Auto-detektér kolonner første gang data indlæses
      if (is.null(input$x_column) || input$x_column == "") {
        auto_detect_and_update_columns()
      }
    }
  })
  
  # Auto-detect kolonne funktion
  auto_detect_and_update_columns <- function() {
    req(values$current_data)
    
    data <- values$current_data
    col_names <- names(data)
    
    # Detektér potentielle dato-kolonner
    x_col <- NULL
    for (col_name in col_names) {
      col_data <- data[[col_name]]
      
      # Tjek for dato patterns eller "dato" i navnet
      if (grepl("dato|date|tid|time", col_name, ignore.case = TRUE)) {
        x_col <- col_name
        break
      }
      
      # Tjek for dato-format i data
      char_data <- as.character(col_data)[!is.na(col_data)]
      if (length(char_data) > 0 && 
          any(grepl("\\d{4}-\\d{2}-\\d{2}|\\d{2}/\\d{2}/\\d{4}|\\d{2}-\\d{2}-\\d{4}", char_data))) {
        x_col <- col_name
        break
      }
    }
    
    # Hvis ingen dato-kolonne fundet, brug første kolonne
    if (is.null(x_col) && length(col_names) > 0) {
      x_col <- col_names[1]
    }
    
    # Detektér numeriske kolonner (ekskludér x_col)
    numeric_cols <- character(0)
    for (col_name in col_names) {
      if (col_name != x_col) {
        col_data <- data[[col_name]]
        if (is.numeric(col_data) || 
            sum(!is.na(suppressWarnings(as.numeric(gsub(",", ".", as.character(col_data)))))) > length(col_data) * 0.8) {
          numeric_cols <- c(numeric_cols, col_name)
        }
      }
    }
    
    # Smart detektér tæller/nævner
    col_names_lower <- tolower(col_names)
    taeller_col <- NULL
    naevner_col <- NULL
    
    # Look for Danish tæller/nævner patterns
    taeller_idx <- which(grepl("t.ller|tael|num|count", col_names_lower, ignore.case = TRUE))
    naevner_idx <- which(grepl("n.vner|naev|denom|total", col_names_lower, ignore.case = TRUE))
    
    if (length(taeller_idx) > 0 && length(naevner_idx) > 0) {
      taeller_col <- col_names[taeller_idx[1]]
      naevner_col <- col_names[naevner_idx[1]]
    } else if (length(numeric_cols) >= 2) {
      # Hvis ikke tæller/nævner pattern, brug første to numeriske
      taeller_col <- numeric_cols[1]
      naevner_col <- numeric_cols[2]
    } else if (length(numeric_cols) >= 1) {
      taeller_col <- numeric_cols[1]
    }
    
    # Opdater UI med detekterede kolonner
    if (!is.null(x_col)) {
      updateSelectInput(session, "x_column", selected = x_col)
    }
    
    if (!is.null(taeller_col)) {
      updateSelectInput(session, "y_column", selected = taeller_col)
    }
    
    if (!is.null(naevner_col)) {
      updateSelectInput(session, "n_column", selected = naevner_col)
    }
    
    # Vis bekræftelse
    detected_msg <- paste0(
      "Auto-detekteret: ",
      "X=", x_col %||% "ingen", ", ",
      "Y=", taeller_col %||% "ingen",
      if (!is.null(naevner_col)) paste0(", N=", naevner_col) else ""
    )
    
    showNotification(
      detected_msg,
      type = "message",
      duration = 3
    )
  }
  
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
  
  # Kolonne konfiguration for visualization
  column_config <- reactive({
    req(values$current_data)
    
    # Brug brugerens valg hvis tilgængelige
    x_col <- if (!is.null(input$x_column) && input$x_column != "") input$x_column else NULL
    y_col <- if (!is.null(input$y_column) && input$y_column != "") input$y_column else NULL
    n_col <- if (!is.null(input$n_column) && input$n_column != "") input$n_column else NULL
    
    return(list(
      x_col = x_col,
      y_col = y_col,
      n_col = n_col,
      chart_type = get_qic_chart_type(input$chart_type %||% "Seriediagram (Run Chart)")
    ))
  })
  
  # Kolonne validering output
  output$column_validation_messages <- renderUI({
    req(values$current_data)
    
    # Kun vis hvis vi har nogle kolonnevalg
    if (is.null(input$x_column) || input$x_column == "" ||
        is.null(input$y_column) || input$y_column == "") {
      return(NULL)
    }
    
    chart_type <- get_qic_chart_type(input$chart_type %||% "Seriediagram (Run Chart)")
    messages <- character(0)
    warnings <- character(0)
    
    # Tjek om Y-kolonne er numerisk
    if (!is.null(input$y_column) && input$y_column != "" && input$y_column %in% names(values$current_data)) {
      y_data <- values$current_data[[input$y_column]]
      if (!is.numeric(y_data)) {
        # Prøv at konvertere
        numeric_test <- suppressWarnings(as.numeric(gsub(",", ".", as.character(y_data))))
        if (sum(!is.na(numeric_test)) < length(y_data) * 0.8) {
          warnings <- c(warnings, paste("Y-kolonne '", input$y_column, "' er ikke numerisk"))
        }
      }
    }
    
    # Tjek P/U chart requirements
    if (chart_type %in% c("p", "pp", "u", "up")) {
      if (is.null(input$n_column) || input$n_column == "") {
        warnings <- c(warnings, paste("Chart type", chart_type, "kræver en nævner-kolonne (N)"))
      } else if (input$n_column %in% names(values$current_data)) {
        n_data <- values$current_data[[input$n_column]]
        if (!is.numeric(n_data)) {
          numeric_test <- suppressWarnings(as.numeric(gsub(",", ".", as.character(n_data))))
          if (sum(!is.na(numeric_test)) < length(n_data) * 0.8) {
            warnings <- c(warnings, paste("Nævner-kolonne '", input$n_column, "' er ikke numerisk"))
          }
        }
      }
    }
    
    # Tjek for samme kolonne valgt flere gange
    selected_cols <- c(input$x_column, input$y_column, input$n_column)
    selected_cols <- selected_cols[!is.null(selected_cols) & selected_cols != ""]
    
    if (length(selected_cols) != length(unique(selected_cols))) {
      warnings <- c(warnings, "Samme kolonne kan ikke bruges til flere formål")
    }
    
    # Vis resultater
    if (length(warnings) > 0) {
      div(
        class = "alert alert-warning",
        style = "font-size: 0.85rem; padding: 8px; margin: 5px 0;",
        icon("exclamation-triangle"),
        strong(" Kolonne advarsler:"),
        tags$ul(
          style = "margin: 5px 0; padding-left: 20px;",
          lapply(warnings, function(warn) tags$li(warn))
        )
      )
    } else if (length(selected_cols) >= 2) {
      div(
        class = "alert alert-success",
        style = "font-size: 0.85rem; padding: 8px; margin: 5px 0;",
        icon("check-circle"),
        strong(" Kolonner valideret! "),
        sprintf("Klar til %s chart", chart_type)
      )
    }
  })
  
  # Auto-detect button handler
  observeEvent(input$auto_detect_columns, {
    auto_detect_and_update_columns()
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
    column_config_reactive = column_config,  # Eksplicit kolonne konfiguration
    chart_type_reactive = reactive({
      chart_selection <- input$chart_type %||% "Seriediagram (Run Chart)"
      get_qic_chart_type(chart_selection)
    }),
    show_targets_reactive = reactive(input$show_targets %||% FALSE),
    show_phases_reactive = reactive(input$show_phases %||% FALSE),
    chart_title_reactive = chart_title
  )
  
  # Plot ready check
  output$plot_ready <- reactive({
    !is.null(visualization$plot_ready()) && visualization$plot_ready()
  })
  outputOptions(output, "plot_ready", suspendWhenHidden = FALSE)
  
  # Download handlers med metadata
  output$download_png <- downloadHandler(
    filename = function() {
      title_clean <- gsub("[^A-Za-z0-9æøåÆØÅ ]", "", chart_title())
      title_clean <- gsub(" ", "_", title_clean)
      paste0(title_clean, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      if (!is.null(visualization$plot())) {
        ggsave(file, visualization$plot(), width = 12, height = 8, dpi = 300)
        
        # Show success message with metadata
        showNotification(
          paste("PNG eksporteret:", chart_title()),
          type = "message",
          duration = 3
        )
      }
    }
  )
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      title_clean <- gsub("[^A-Za-z0-9æøåÆØÅ ]", "", chart_title())
      title_clean <- gsub(" ", "_", title_clean)
      paste0("rapport_", title_clean, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      showNotification("PDF rapport kommer i næste fase", type = "message")
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() {
      title_clean <- gsub("[^A-Za-z0-9æøåÆØÅ ]", "", chart_title())
      title_clean <- gsub(" ", "_", title_clean)
      paste0("data_", title_clean, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!is.null(active_data())) {
        # Add metadata as comments in CSV
        metadata_header <- paste0(
          "# Indikator: ", input$indicator_title %||% "Ikke angivet", "\n",
          "# Enhed: ", current_unit(), "\n",
          "# Beskrivelse: ", input$indicator_description %||% "Ikke angivet", "\n",
          "# Eksporteret: ", Sys.time(), "\n",
          "# ---\n"
        )
        
        # Write metadata + data
        cat(metadata_header, file = file)
        write.table(active_data(), file = file, append = TRUE, sep = ",", 
                    row.names = FALSE, quote = TRUE)
        
        showNotification(
          paste("Data eksporteret med metadata:", chart_title()),
          type = "message",
          duration = 3
        )
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
}

# Run the application
shinyApp(ui = ui, server = server)