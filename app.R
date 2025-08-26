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
      // Custom message handlers
      Shiny.addCustomMessageHandler('saveAppState', function(message) {
        window.saveAppState(message.key, message.data);
      });
      
      Shiny.addCustomMessageHandler('loadAppState', function(message) {
        var data = window.loadAppState(message.key);
        Shiny.setInputValue('loaded_app_state', data, {priority: 'event'});
      });
      
      Shiny.addCustomMessageHandler('clearAppState', function(message) {
        window.clearAppState(message.key);
      });
      
      // Check for existing data on app start
      $(document).ready(function() {
        if (window.hasAppState('current_session')) {
          setTimeout(function() {
            var data = window.loadAppState('current_session');
            Shiny.setInputValue('check_saved_data', data, {priority: 'event'});
          }, 1000); // Delay to ensure Shiny is ready
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
              "Ryd gemt", 
              icon = icon("trash"), 
              class = "btn-outline-danger btn-sm"
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
  )
)

# SERVER ----
# Define server
server <- function(input, output, session) {
  
  # Reactive values for data håndtering inkl. auto-save
  values <- reactiveValues(
    current_data = NULL,
    original_data = NULL,
    file_uploaded = FALSE,
    import_settings_visible = FALSE,
    updating_table = FALSE,
    auto_detect_done = FALSE,
    # NYT: Auto-save relaterede values
    auto_save_enabled = TRUE,
    last_save_time = NULL
  )
  
  # NYT: Check for saved data ved app start
  observeEvent(input$check_saved_data, {
    req(input$check_saved_data)
    
    tryCatch({
      saved_state <- input$check_saved_data
      
      if (!is.null(saved_state$data)) {
        showModal(modalDialog(
          title = "Tidligere session fundet",
          size = "m",
          
          div(
            icon("clock"),
            " Du har gemt data fra ",
            strong(format(as.POSIXct(saved_state$timestamp), "%d-%m-%Y %H:%M")),
            br(), br(),
            
            if (!is.null(saved_state$metadata$title)) {
              p("Indikator: ", strong(saved_state$metadata$title))
            },
            
            if (!is.null(saved_state$metadata$unit_type)) {
              p("Enhed: ", strong(saved_state$metadata$unit_type))
            },
            
            p("Datapunkter: ", strong(nrow(saved_state$data))),
            
            br(),
            p("Vil du fortsætte med den gemte session eller starte forfra?")
          ),
          
          footer = tagList(
            actionButton("discard_saved", "Start forfra", class = "btn-outline-secondary"),
            actionButton("restore_saved", "Gendan session", class = "btn-primary")
          ),
          easyClose = FALSE
        ))
      }
      
    }, error = function(e) {
      cat("Error loading saved data:", e$message, "\n")
    })
  }, once = TRUE)
  
  # NYT: Gendan gemt session
  observeEvent(input$restore_saved, {
    req(input$check_saved_data)
    
    tryCatch({
      saved_state <- input$check_saved_data
      
      # Prevent loops during restore
      values$updating_table <- TRUE
      on.exit({ values$updating_table <- FALSE }, add = TRUE)
      
      # Gendan data
      values$current_data <- as.data.frame(saved_state$data)
      values$original_data <- as.data.frame(saved_state$data)
      values$file_uploaded <- TRUE
      values$auto_detect_done <- TRUE
      
      # Gendan metadata hvis tilgængelig
      if (!is.null(saved_state$metadata)) {
        isolate({
          if (!is.null(saved_state$metadata$title)) {
            updateTextInput(session, "indicator_title", value = saved_state$metadata$title)
          }
          if (!is.null(saved_state$metadata$unit_type)) {
            updateRadioButtons(session, "unit_type", selected = saved_state$metadata$unit_type)
          }
          if (!is.null(saved_state$metadata$unit_select)) {
            updateSelectInput(session, "unit_select", selected = saved_state$metadata$unit_select)
          }
          if (!is.null(saved_state$metadata$unit_custom)) {
            updateTextInput(session, "unit_custom", value = saved_state$metadata$unit_custom)
          }
          if (!is.null(saved_state$metadata$description)) {
            updateTextAreaInput(session, "indicator_description", value = saved_state$metadata$description)
          }
          if (!is.null(saved_state$metadata$chart_type)) {
            updateSelectInput(session, "chart_type", selected = saved_state$metadata$chart_type)
          }
          if (!is.null(saved_state$metadata$x_column)) {
            updateSelectInput(session, "x_column", selected = saved_state$metadata$x_column)
          }
          if (!is.null(saved_state$metadata$y_column)) {
            updateSelectInput(session, "y_column", selected = saved_state$metadata$y_column)
          }
          if (!is.null(saved_state$metadata$n_column)) {
            updateSelectInput(session, "n_column", selected = saved_state$metadata$n_column)
          }
        })
      }
      
      removeModal()
      showNotification("Session gendannet!", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Fejl ved gendan:", e$message), type = "error")
      removeModal()
    })
  })
  
  # NYT: Slet gemt session
  observeEvent(input$discard_saved, {
    clearDataLocally(session)
    removeModal()
    showNotification("Gemt session slettet - starter forfra", type = "message", duration = 3)
  })
  
  # NYT: Manual save handler
  observeEvent(input$manual_save, {
    req(values$current_data)
    
    metadata <- list(
      title = input$indicator_title,
      unit_type = input$unit_type,
      unit_select = input$unit_select,
      unit_custom = input$unit_custom,
      description = input$indicator_description,
      x_column = input$x_column,
      y_column = input$y_column,
      n_column = input$n_column,
      chart_type = input$chart_type
    )
    
    saveDataLocally(session, values$current_data, metadata)
    values$last_save_time <- Sys.time()
    showNotification("Session gemt lokalt!", type = "message", duration = 2)
  })
  
  # NYT: Clear saved handler
  observeEvent(input$clear_saved, {
    clearDataLocally(session)
    values$last_save_time <- NULL
    showNotification("Gemt data ryddet", type = "message", duration = 2)
  })
  
  # NYT: Save status display
  output$save_status_display <- renderUI({
    if (!is.null(values$last_save_time)) {
      time_diff <- as.numeric(difftime(Sys.time(), values$last_save_time, units = "mins"))
      if (time_diff < 1) {
        span(icon("check"), " Gemt lige nu", style = "color: green;")
      } else if (time_diff < 60) {
        span(icon("clock"), paste(" Gemt for", round(time_diff), "min siden"))
      } else {
        span(icon("clock"), " Gemt for mere end 1 time siden")
      }
    }
  })
  
  # Toggle import settings panel
  observeEvent(input$toggle_import_settings, {
    values$import_settings_visible <- !values$import_settings_visible
    
    if (values$import_settings_visible) {
      shinyjs::show("import_settings_panel")
    } else {
      shinyjs::hide("import_settings_panel")
    }
  })
  
  # Start med tom tabel
  observe({
    if (is.null(values$current_data)) {
      empty_data <- data.frame(
        Dato = rep(NA_character_, 5),
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
      data_rows <- sum(!is.na(values$current_data[[1]]))
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
    
    values$updating_table <- TRUE
    on.exit({ values$updating_table <- FALSE }, add = TRUE)
    
    file_path <- input$data_file$datapath
    file_ext <- tools::file_ext(input$data_file$name)
    
    tryCatch({
      if (file_ext %in% c("xlsx", "xls")) {
        data <- readxl::read_excel(file_path, col_names = TRUE)
      } else {
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
      
      values$current_data <- as.data.frame(data)
      values$original_data <- as.data.frame(data)
      values$file_uploaded <- TRUE
      values$auto_detect_done <- FALSE
      
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
      ) %>%
      rhandsontable::hot_cols(
        columnHeaderHeight = 50,
        manualColumnResize = TRUE
      )
    
    # Kolonne-specifik formatting
    for (i in 1:ncol(data)) {
      col_data <- data[[i]]
      
      if (grepl("dato|date", names(data)[i], ignore.case = TRUE)) {
        hot <- hot %>%
          rhandsontable::hot_col(col = i, type = "text")
      } else if (is.numeric(col_data)) {
        hot <- hot %>%
          rhandsontable::hot_col(col = i, type = "numeric", format = "0,0.00")
      } else {
        hot <- hot %>%
          rhandsontable::hot_col(col = i, type = "text")
      }
    }
    
    return(hot)
  })
  
  # Håndtering af tabel ændringer
  observeEvent(input$main_data_table, {
    if (values$updating_table) {
      return()
    }
    
    req(input$main_data_table)
    
    values$updating_table <- TRUE
    on.exit({ values$updating_table <- FALSE }, add = TRUE)
    
    tryCatch({
      new_data <- rhandsontable::hot_to_r(input$main_data_table)
      
      if (!is.null(values$current_data) && identical(values$current_data, new_data)) {
        return()
      }
      
      # Tjek om kolonnenavne er ændret
      current_names <- names(values$current_data)
      new_names <- names(new_data)
      
      if (!identical(current_names, new_names)) {
        # Valider nye kolonnenavne
        if (length(new_names) != length(unique(new_names))) {
          showNotification(
            "Kolonnenavne skal være unikke. Ændring ignoreret.",
            type = "error",
            duration = 4
          )
          return()
        }
        
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
      
      values$current_data <- new_data
      
    }, error = function(e) {
      cat("ERROR in main_data_table observer:", e$message, "\n")
      showNotification(
        paste("Fejl ved tabel-opdatering:", e$message),
        type = "error",
        duration = 3
      )
    })
  }, ignoreInit = TRUE)
  
  # Redigér kolonnenavne modal
  observeEvent(input$edit_column_names, {
    req(values$current_data)
    
    current_names <- names(values$current_data)
    
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
    
    for (i in 1:length(current_names)) {
      input_value <- input[[paste0("col_name_", i)]]
      if (!is.null(input_value) && input_value != "") {
        new_names[i] <- trimws(input_value)
      } else {
        new_names[i] <- current_names[i]
      }
    }
    
    if (any(duplicated(new_names))) {
      showNotification(
        "Kolonnenavne skal være unikke. Ret duplikater og prøv igen.",
        type = "error",
        duration = 5
      )
      return()
    }
    
    names(values$current_data) <- new_names
    
    removeModal()
    
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
    } else {
      showNotification("Ingen ændringer i kolonnenavne", type = "message", duration = 2)
    }
  })
  
  # Tilføj kolonne
  observeEvent(input$add_column, {
    req(values$current_data)
    
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
    
    if (new_col_type == "numeric") {
      values$current_data[[new_col_name]] <- rep(NA_real_, nrow(values$current_data))
    } else if (new_col_type == "date") {
      values$current_data[[new_col_name]] <- rep(NA_character_, nrow(values$current_data))
    } else {
      values$current_data[[new_col_name]] <- rep(NA_character_, nrow(values$current_data))
    }
    
    removeModal()
    showNotification(paste("Kolonne", new_col_name, "tilføjet"), type = "message")
  })
  
  # Tilføj række
  observeEvent(input$add_row, {
    req(values$current_data)
    
    new_row <- values$current_data[1, ]
    new_row[1, ] <- NA
    
    values$current_data <- rbind(values$current_data, new_row)
    
    showNotification("Ny række tilføjet", type = "message")
  })
  
  # Reset tabel
  observeEvent(input$reset_table, {
    values$updating_table <- TRUE
    
    values$current_data <- data.frame(
      Dato = rep(NA_character_, 5),
      Taeller = rep(NA_real_, 5),
      Naevner = rep(NA_real_, 5),
      stringsAsFactors = FALSE
    )
    
    values$file_uploaded <- FALSE
    values$original_data <- NULL
    values$auto_detect_done <- FALSE
    
    isolate({
      shinyjs::reset("data_file")
    })
    
    values$updating_table <- FALSE
    
    showNotification(
      "Tabel og fil-upload tømt - indtast nye data eller upload ny fil. Titel og beskrivelse bevaret.", 
      type = "message", 
      duration = 4
    )
  })
  
  # Opdater kolonne-valg når data ændres
  observe({
    if (values$updating_table) {
      return()
    }
    
    req(values$current_data)
    
    data <- values$current_data
    all_cols <- names(data)
    
    if (length(all_cols) > 0) {
      col_choices <- setNames(c("", all_cols), c("Vælg kolonne...", all_cols))
      
      isolate({
        updateSelectInput(session, "x_column", choices = col_choices)
        updateSelectInput(session, "y_column", choices = col_choices)
        updateSelectInput(session, "n_column", choices = col_choices)
      })
      
      if (!values$auto_detect_done && 
          (is.null(input$x_column) || input$x_column == "")) {
        values$auto_detect_done <- TRUE
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
      
      if (grepl("dato|date|tid|time", col_name, ignore.case = TRUE)) {
        x_col <- col_name
        break
      }
      
      char_data <- as.character(col_data)[!is.na(col_data)]
      if (length(char_data) > 0) {
        test_sample <- char_data[1:min(3, length(char_data))]
        danish_formats <- c("dmy", "ymd", "dby", "dbY")
        
        date_test <- lubridate::parse_date_time(
          test_sample, 
          orders = danish_formats,
          quiet = TRUE
        )
        
        success_rate <- sum(!is.na(date_test)) / length(date_test)
        if (success_rate >= 0.5) {
          x_col <- col_name
          break
        }
      }
    }
    
    if (is.null(x_col) && length(col_names) > 0) {
      x_col <- col_names[1]
    }
    
    # Detektér numeriske kolonner
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
    
    taeller_idx <- which(grepl("t.ller|tael|num|count", col_names_lower, ignore.case = TRUE))
    naevner_idx <- which(grepl("n.vner|naev|denom|total", col_names_lower, ignore.case = TRUE))
    
    if (length(taeller_idx) > 0 && length(naevner_idx) > 0) {
      taeller_col <- col_names[taeller_idx[1]]
      naevner_col <- col_names[naevner_idx[1]]
    } else if (length(numeric_cols) >= 2) {
      taeller_col <- numeric_cols[1]
      naevner_col <- numeric_cols[2]
    } else if (length(numeric_cols) >= 1) {
      taeller_col <- numeric_cols[1]
    }
    
    isolate({
      if (!is.null(x_col)) {
        updateSelectInput(session, "x_column", selected = x_col)
      }
      
      if (!is.null(taeller_col)) {
        updateSelectInput(session, "y_column", selected = taeller_col)
      }
      
      if (!is.null(naevner_col)) {
        updateSelectInput(session, "n_column", selected = naevner_col)
      }
      
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
    })
  }
  
  # Data for visualization modul
  active_data <- reactive({
    req(values$current_data)
    
    data <- values$current_data
    non_empty_rows <- apply(data, 1, function(row) any(!is.na(row)))
    
    if (any(non_empty_rows)) {
      filtered_data <- data[non_empty_rows, ]
      return(filtered_data)
    } else {
      return(NULL)
    }
  })
  
  # Kolonne konfiguration for visualization
  column_config <- reactive({
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
    
    if (is.null(input$x_column) || input$x_column == "" ||
        is.null(input$y_column) || input$y_column == "") {
      return(NULL)
    }
    
    chart_type <- get_qic_chart_type(input$chart_type %||% "Seriediagram (Run Chart)")
    warnings <- character(0)
    
    # Tjek om Y-kolonne er numerisk
    if (!is.null(input$y_column) && input$y_column != "" && input$y_column %in% names(values$current_data)) {
      y_data <- values$current_data[[input$y_column]]
      if (!is.numeric(y_data)) {
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
    column_config_reactive = column_config,
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
    result <- !is.null(visualization$plot_ready()) && visualization$plot_ready()
    return(result)
  })
  outputOptions(output, "plot_ready", suspendWhenHidden = FALSE)
  
  # NYT: Auto-save når data ændres  
  observeEvent(values$current_data, {
    if (values$auto_save_enabled && !is.null(values$current_data) && !values$updating_table) {
      
      if (nrow(values$current_data) > 0 && any(!is.na(values$current_data))) {
        
        metadata <- list(
          title = isolate(input$indicator_title),
          unit_type = isolate(input$unit_type),
          unit_select = isolate(input$unit_select),
          unit_custom = isolate(input$unit_custom),
          description = isolate(input$indicator_description),
          x_column = isolate(input$x_column),
          y_column = isolate(input$y_column),
          n_column = isolate(input$n_column),
          chart_type = isolate(input$chart_type)
        )
        
        autoSaveAppState(session, values$current_data, metadata)
        values$last_save_time <- Sys.time()
      }
    }
  }, ignoreInit = TRUE)
  
  # NYT: Auto-save når indstillinger ændres (debounced)
  observe({
    list(
      input$indicator_title,
      input$unit_type,
      input$unit_select,
      input$unit_custom,
      input$indicator_description,
      input$x_column,
      input$y_column,
      input$n_column,
      input$chart_type
    )
    
    if (values$auto_save_enabled && !is.null(values$current_data) && !values$updating_table) {
      
      metadata <- list(
        title = input$indicator_title,
        unit_type = input$unit_type,
        unit_select = input$unit_select,
        unit_custom = input$unit_custom,
        description = input$indicator_description,
        x_column = input$x_column,
        y_column = input$y_column,
        n_column = input$n_column,
        chart_type = input$chart_type
      )
      
      invalidateLater(2000)
      autoSaveAppState(session, values$current_data, metadata)
      values$last_save_time <- Sys.time()
    }
  }) %>% 
    bindEvent({
      list(
        input$indicator_title,
        input$unit_type,
        input$unit_select,
        input$unit_custom,
        input$indicator_description,
        input$x_column,
        input$y_column,
        input$n_column,
        input$chart_type
      )
    }, ignoreInit = TRUE)
  
  # Download handlers
  output$download_png <- downloadHandler(
    filename = function() {
      title_clean <- gsub("[^A-Za-z0-9æøåÆØÅ ]", "", chart_title())
      title_clean <- gsub(" ", "_", title_clean)
      paste0(title_clean, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      if (!is.null(visualization$plot())) {
        ggsave(file, visualization$plot(), width = 12, height = 8, dpi = 300)
        
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
        metadata_header <- paste0(
          "# Indikator: ", input$indicator_title %||% "Ikke angivet", "\n",
          "# Enhed: ", current_unit(), "\n",
          "# Beskrivelse: ", input$indicator_description %||% "Ikke angivet", "\n",
          "# Eksporteret: ", Sys.time(), "\n",
          "# ---\n"
        )
        
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
  
  # Anti-stuck mechanism
  observeEvent(values$current_data, {
    invalidateLater(200)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
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