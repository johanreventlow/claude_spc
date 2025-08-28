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
  
  header = tagList(
    waiter::use_waiter(),
    
  # NYT: Tilføj JavaScript til head section
  tags$head(
    tags$script(HTML("
      $(document).ready(function() {
        // Add waiter-shown class immediately when page loads
        $('body').addClass('waiter-shown');
        
        // Function to properly show UI when waiter hides
        window.showAppUI = function() {
          setTimeout(function() {
            $('body').css('opacity', '1');
          }, 100); // Small delay to ensure smooth transition
        };
      });
      
      // Add to existing JavaScript section
      Shiny.addCustomMessageHandler('showAppUI', function(message) {
        window.showAppUI();
      });
    ")),
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
              /* Pre-hide entire app body to prevent flash */
        body {
          opacity: 0 !important;
          transition: opacity 0.3s ease-in-out;
        }
        
        /* Show body when waiter is active */
        body.waiter-shown {
          opacity: 1 !important;
        }
        
        /* Ensure waiter overlay is on top */
        .waiter-overlay {
          z-index: 9999 !important;
        }
      
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

    # waiter::autoWaiter(),
  ),
  
  # Enable shinyjs
  shinyjs::useShinyjs(),
  
  nav_panel(
    title = NULL,
    
    # Layout med smal sidebar + 2-kolonne main area
    layout_sidebar(
      sidebar = sidebar(
        title = div(
          icon("upload"), 
          " Data upload & konfiguration",
          style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 600;")
        ),
        width = "400px",
        position = "left",
        open = TRUE,
        collapsible = TRUE,
        
        # Upload sektion
        # div(
          fileInput(
            "data_file",
            NULL,
            accept = c(".csv", ".xlsx", ".xls"),
            placeholder = "Vælg fil..."
          # )
        ),
        
        hr(),
        
        # Indikator metadata
        # div(
          # Titel
          textInput(
            "indicator_title",
            "Titel på indikator:",
            value = "",
            placeholder = "F.eks. 'Infektioner pr. 1000 sengedage'"
          ),
          
          # Organisatorisk enhed
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
          ),
          
          # Beskrivelse
          textAreaInput(
            "indicator_description",
            "Indikatorbeskrivelse:",
            value = "",
            placeholder = "Beskriv kort hvad indikatoren måler, hvordan data indsamles, og hvad målsætningen er...",
            height = "100px",
            resize = "vertical"
          # )
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
      
      ## Main content area - 2 kolonner -----
      fluidRow(
        ### VENSTRE: Data tabel (~50%) ----
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
                  
                  # N column - altid vist men med hjælpetekst
                  selectInput(
                    "n_column",
                    "Nævner (n):",
                    choices = NULL,
                    selected = NULL
                  ),
                  
                  # Hjælpe-tekst der forklarer hvornår nævner er påkrævet
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
        
        ### HØJRE: Graf og indstillinger (~50%) -------
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
              # min_height = "200px",
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
)

# SERVER ----
# Define server
server <- function(input, output, session) {
  
  # Reactive values for data håndtering inkl. auto-save
  values <- reactiveValues(
    current_data = NULL,
    original_data = NULL,
    file_uploaded = FALSE,

    updating_table = FALSE,
    auto_detect_done = FALSE,
    # FIXED: Auto-save relaterede values med restore guard
    auto_save_enabled = TRUE,
    last_save_time = NULL,
    restoring_session = FALSE,  # Ny guard for session restore
    app_initialized = FALSE,    # Track om app er fuldt initialiseret
    table_version = 0           # Force table re-render counter
  )
  
  # Initialize app start waiter
  # waiter_app_start <- waiter::Waiter$new(
  #   html = WAITER_CONFIG$app_start$html,
  #   color = WAITER_CONFIG$app_start$color
  # )
  
  waiter_app_start <- waiter::Waiter$new(
    id = NULL,  # NULL = hele skærmen
    html = WAITER_CONFIG$app_start$html,
    color = "rgba(248,249,250,0.95)"  # Halvgennemsigtig overlay
  )
  
  # Initialize file upload waiter
  waiter_file <- waiter::Waiter$new(
    html = WAITER_CONFIG$file_upload$html,
    color = WAITER_CONFIG$file_upload$color
  )
  
  # Show app start loading
  waiter_app_start$show()

  # OPTION 3: Kombination - minimum tid OG app initialized - ACTIVE
  observe({
    invalidateLater(1500)  # Minimum 1.5 sekunder
    if (!is.null(input$app_initialized) && input$app_initialized) {
      waiter_app_start$hide()
      
      # Trigger smooth UI reveal after waiter hides
      session$sendCustomMessage("showAppUI", list())
    }
  })
  
  # FIXED: App initialization tracker
  observeEvent(input$app_initialized, {
    cat("DEBUG: App marked as initialized\n")
    values$app_initialized <- TRUE
  })
  
  # UPDATED: Automatisk genindlæsning af gemt session ved app start
  observeEvent(input$auto_restore_data, {
    req(input$auto_restore_data)
    
    # Vent til app er initialiseret
    if (!values$app_initialized) {
      cat("DEBUG: App not yet initialized, waiting...\n")
      invalidateLater(500)
      return()
    }
    
    tryCatch({
      cat("DEBUG: Auto-restoring saved session data\n")
      saved_state <- input$auto_restore_data
      
      if (!is.null(saved_state$data)) {
        cat("DEBUG: Found saved data for auto-restore\n")
        
        # CRITICAL: Disable auto-save og set restore guard
        values$restoring_session <- TRUE
        values$updating_table <- TRUE
        values$auto_save_enabled <- FALSE
        
        # Cleanup function som altid kører
        on.exit({
          cat("DEBUG: Auto-restore cleanup - re-enabling auto-save\n")
          values$updating_table <- FALSE
          values$restoring_session <- FALSE
          values$auto_save_enabled <- TRUE
        }, add = TRUE)
        
        # CRITICAL: Reconstruct data.frame from saved structure
        saved_data <- saved_state$data
        
        if (!is.null(saved_data$values) && !is.null(saved_data$nrows) && !is.null(saved_data$ncols)) {
          cat("DEBUG: Reconstructing data.frame from structured format\n")
          
          # Reconstruct data.frame from saved components - MANUAL METHOD
          reconstructed_data <- data.frame(
            matrix(nrow = saved_data$nrows, ncol = saved_data$ncols),
            stringsAsFactors = FALSE
          )
          
          # Set column names first if available
          if (!is.null(saved_data$col_names)) {
            names(reconstructed_data) <- saved_data$col_names
          }
          
          # Populate columns one by one
          for (i in seq_along(saved_data$values)) {
            reconstructed_data[[i]] <- saved_data$values[[i]]
          }
          
          # Restore column classes if available
          if (!is.null(saved_data$class_info)) {
            for (col_name in names(saved_data$class_info)) {
              if (col_name %in% names(reconstructed_data)) {
                target_class <- saved_data$class_info[[col_name]]
                if (target_class == "numeric") {
                  reconstructed_data[[col_name]] <- as.numeric(reconstructed_data[[col_name]])
                } else if (target_class == "character") {
                  reconstructed_data[[col_name]] <- as.character(reconstructed_data[[col_name]])
                } else if (target_class == "logical") {
                  reconstructed_data[[col_name]] <- as.logical(reconstructed_data[[col_name]])
                }
              }
            }
          }
          
          values$current_data <- reconstructed_data
          values$original_data <- reconstructed_data
          
        } else {
          # Fallback for older save format
          values$current_data <- as.data.frame(saved_state$data)
          values$original_data <- as.data.frame(saved_state$data)
        }
        
        values$file_uploaded <- TRUE
        values$auto_detect_done <- TRUE
        
        # Gendan metadata hvis tilgængelig
        if (!is.null(saved_state$metadata)) {
          cat("DEBUG: Restoring metadata\n")
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
        
        # Vis notifikation om automatisk genindlæsning
        data_rows <- if (!is.null(saved_state$data$nrows)) {
          saved_state$data$nrows
        } else {
          nrow(saved_state$data)
        }
        
        showNotification(
          paste("Tidligere session automatisk genindlæst:", data_rows, "datapunkter fra", 
                format(as.POSIXct(saved_state$timestamp), "%d-%m-%Y %H:%M")),
          type = "message",
          duration = 5
        )
        
        cat("DEBUG: Auto-restore completed successfully\n")
        
        # CRITICAL: Force reset updating_table after a delay
        invalidateLater(1000)
        isolate({
          values$updating_table <- FALSE
          values$restoring_session <- FALSE
          values$table_version <- values$table_version + 1
        })
        
      } else {
        cat("DEBUG: No saved data found in session state\n")
      }
      
    }, error = function(e) {
      cat("ERROR during auto-restore:", e$message, "\n")
      showNotification(paste("Fejl ved automatisk genindlæsning:", e$message), type = "error")
    })
  }, once = TRUE)
  
  # NYT: Manual save handler
  observeEvent(input$manual_save, {
    req(values$current_data)
    
    metadata <- list(
      title = input$indicator_title,
      unit_type = input$unit_type,
      unit_select = input$unit_select,
      unit_custom = input$unit_custom,
      description = input$indicator_description,
      x_column = if(input$x_column == "BLANK") "" else input$x_column,
      y_column = if(input$y_column == "BLANK") "" else input$y_column,
      n_column = if(input$n_column == "BLANK") "" else input$n_column,
      chart_type = input$chart_type
    )
    
    saveDataLocally(session, values$current_data, metadata)
    values$last_save_time <- Sys.time()
    showNotification("Session gemt lokalt!", type = "message", duration = 2)
  })
  
  # NYT: Clear saved handler - nu med bekræftelse da den starter ny session
  observeEvent(input$clear_saved, {
    showModal(modalDialog(
      title = "Start ny session?",
      size = "m",
      
      div(
        icon("refresh"),
        " Er du sikker på at du vil starte en helt ny session?",
        br(), br(),
        p("Dette vil:"),
        tags$ul(
          tags$li("Slette al gemt data"),
          tags$li("Rydde tabellen"),  
          tags$li("Nulstille alle indstillinger")
        ),
        br(),
        p("Denne handling kan ikke fortrydes.")
      ),
      
      footer = tagList(
        modalButton("Annuller"),
        actionButton("confirm_clear_saved", "Ja, start ny session", class = "btn-warning")
      ),
      easyClose = FALSE
    ))
  })
  
  # Bekræft rydning af session
  observeEvent(input$confirm_clear_saved, {
    clearDataLocally(session)
    values$last_save_time <- NULL
    
    # Nulstil også data og UI
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
    
    # Reset UI inputs
    isolate({
      updateTextInput(session, "indicator_title", value = "")
      updateRadioButtons(session, "unit_type", selected = "select")
      updateSelectInput(session, "unit_select", selected = "")
      updateTextInput(session, "unit_custom", value = "")
      updateTextAreaInput(session, "indicator_description", value = "")
      updateSelectInput(session, "chart_type", selected = "Seriediagram (Run Chart)")
      updateSelectInput(session, "x_column", selected = "")
      updateSelectInput(session, "y_column", selected = "")
      updateSelectInput(session, "n_column", selected = "")
      shinyjs::reset("data_file")
    })
    
    values$updating_table <- FALSE
    
    removeModal()
    showNotification("Ny session startet - alt data og indstillinger nulstillet", type = "message", duration = 4)
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
      selected_unit <- if(is.null(input$unit_select)) "" else input$unit_select
      if (selected_unit != "" && selected_unit %in% names(unit_names)) {
        return(unit_names[[selected_unit]])
      } else {
        return("")
      }
    } else {
      return(if(is.null(input$unit_custom)) "" else input$unit_custom)
    }
  })
  
  # Reactive for komplet graf titel
  chart_title <- reactive({
    base_title <- if(is.null(input$indicator_title) || input$indicator_title == "") "SPC Analyse" else input$indicator_title
    unit_name <- current_unit()
    
    if (base_title != "SPC Analyse" && unit_name != "") {
      return(paste(base_title, "-", unit_name))
    } else if (base_title != "SPC Analyse") {
      return(base_title)
    } else if (unit_name != "") {
      return(paste("SPC Analyse -", unit_name))
    } else {
      return("SPC Analyse")
    }
  })
  
  # File upload handler - FORBEDRET til at håndtere komplette eksport-filer
  observeEvent(input$data_file, {
    req(input$data_file)
    
    # Show loading
    waiter_file$show()
    
    # Ensure loading is hidden no matter what happens
    on.exit({ waiter_file$hide() })
    
    values$updating_table <- TRUE
    on.exit({ values$updating_table <- FALSE }, add = TRUE)
    
    file_path <- input$data_file$datapath
    file_ext <- tools::file_ext(input$data_file$name)
    
    tryCatch({
      if (file_ext %in% c("xlsx", "xls")) {
        # Excel processing (existing code unchanged)
        excel_sheets <- readxl::excel_sheets(file_path)
        
        if ("Data" %in% excel_sheets && "Metadata" %in% excel_sheets) {
          cat("DEBUG: Detected complete export file - importing with session info\n")
          
          # Læs data fra Data sheet
          data <- readxl::read_excel(file_path, sheet = "Data", col_names = TRUE)
          
          # Læs session info fra Metadata sheet
          session_info_raw <- readxl::read_excel(
            file_path, 
            sheet = "Metadata", 
            col_names = FALSE
          )
          
          # Parse session info for konfiguration
          session_lines <- as.character(session_info_raw[[1]])
          
          # Udtrack metadata fra session info
          metadata <- list()
          
          # Parse titel
          title_line <- session_lines[grepl("^• Titel:", session_lines)]
          if (length(title_line) > 0) {
            metadata$title <- gsub("^• Titel: ", "", title_line[1])
            metadata$title <- gsub(" Ikke angivet$", "", metadata$title)
          }
          
          # Parse enhed 
          unit_line <- session_lines[grepl("^• Enhed:", session_lines)]
          if (length(unit_line) > 0) {
            unit_text <- gsub("^• Enhed: ", "", unit_line[1])
            if (unit_text != "Ikke angivet" && unit_text != "") {
              # Tjek om det er en standard enhed
              standard_units <- list(
                "Medicinsk Afdeling" = "med",
                "Kirurgisk Afdeling" = "kir", 
                "Intensiv Afdeling" = "icu",
                "Ambulatorie" = "amb",
                "Akutmodtagelse" = "akut",
                "Pædiatrisk Afdeling" = "paed",
                "Gynækologi/Obstetrik" = "gyn"
              )
              
              if (unit_text %in% names(standard_units)) {
                metadata$unit_type <- "select"
                metadata$unit_select <- standard_units[[unit_text]]
              } else {
                metadata$unit_type <- "custom"
                metadata$unit_custom <- unit_text
              }
            }
          }
          
          # Parse beskrivelse
          desc_line <- session_lines[grepl("^• Beskrivelse:", session_lines)]
          if (length(desc_line) > 0) {
            desc_text <- gsub("^• Beskrivelse: ", "", desc_line[1])
            if (desc_text != "Ikke angivet" && desc_text != "") {
              metadata$description <- desc_text
            }
          }
          
          # Parse graf konfiguration
          chart_line <- session_lines[grepl("^• Chart Type:", session_lines)]
          if (length(chart_line) > 0) {
            chart_text <- gsub("^• Chart Type: ", "", chart_line[1])
            if (chart_text %in% names(CHART_TYPES_DA)) {
              metadata$chart_type <- chart_text
            }
          }
          
          # Parse kolonne mapping
          x_line <- session_lines[grepl("^• X-akse:", session_lines)]
          if (length(x_line) > 0) {
            x_text <- gsub("^• X-akse: (.+) \\(.*\\)$", "\\1", x_line[1])
            if (x_text != "Ikke valgt" && x_text %in% names(data)) {
              metadata$x_column <- x_text
            }
          }
          
          y_line <- session_lines[grepl("^• Y-akse:", session_lines)]
          if (length(y_line) > 0) {
            y_text <- gsub("^• Y-akse: (.+) \\(.*\\)$", "\\1", y_line[1])
            if (y_text != "Ikke valgt" && y_text %in% names(data)) {
              metadata$y_column <- y_text
            }
          }
          
          n_line <- session_lines[grepl("^• Nævner:", session_lines)]
          if (length(n_line) > 0) {
            n_text <- gsub("^• Nævner: ", "", n_line[1])
            if (n_text %in% names(data)) {
              metadata$n_column <- n_text
            }
          }
          
          # Indlæs data
          values$current_data <- as.data.frame(data)
          values$original_data <- as.data.frame(data)
          values$file_uploaded <- TRUE
          values$auto_detect_done <- TRUE  # Skip auto-detect da vi har session info
          
          # Gendan metadata med delay for at sikre UI er klar
          invalidateLater(500)
          isolate({
            if (!is.null(metadata$title)) {
              updateTextInput(session, "indicator_title", value = metadata$title)
            }
            if (!is.null(metadata$unit_type)) {
              updateRadioButtons(session, "unit_type", selected = metadata$unit_type)
              if (metadata$unit_type == "select" && !is.null(metadata$unit_select)) {
                updateSelectInput(session, "unit_select", selected = metadata$unit_select)
              }
              if (metadata$unit_type == "custom" && !is.null(metadata$unit_custom)) {
                updateTextInput(session, "unit_custom", value = metadata$unit_custom)
              }
            }
            if (!is.null(metadata$description)) {
              updateTextAreaInput(session, "indicator_description", value = metadata$description)
            }
            if (!is.null(metadata$chart_type)) {
              updateSelectInput(session, "chart_type", selected = metadata$chart_type)
            }
            if (!is.null(metadata$x_column)) {
              updateSelectInput(session, "x_column", selected = metadata$x_column)
            }
            if (!is.null(metadata$y_column)) {
              updateSelectInput(session, "y_column", selected = metadata$y_column)
            }
            if (!is.null(metadata$n_column)) {
              updateSelectInput(session, "n_column", selected = metadata$n_column)
            }
          })
          
          showNotification(
            paste("Komplet session importeret:", nrow(data), "rækker,", ncol(data), "kolonner + konfiguration"),
            type = "message",
            duration = 4
          )
          
        } else {
          # Standard Excel file
          data <- readxl::read_excel(file_path, col_names = TRUE)
          
          values$current_data <- as.data.frame(data)
          values$original_data <- as.data.frame(data)
          values$file_uploaded <- TRUE
          values$auto_detect_done <- FALSE
          
          showNotification(
            paste("Excel fil uploadet:", nrow(data), "rækker,", ncol(data), "kolonner"),
            type = "message",
            duration = 3
          )
        }
        
      } else {
        # CSV processing with Danish defaults
        data <- readr::read_csv2(
          file_path,
          locale = readr::locale(
            decimal_mark = ",",
            grouping_mark = ".",
            encoding = "ISO-8859-1"
          ),
          show_col_types = FALSE
        )
        
        values$current_data <- as.data.frame(data)
        values$original_data <- as.data.frame(data)
        values$file_uploaded <- TRUE
        values$auto_detect_done <- FALSE
        
        showNotification(
          paste("CSV fil uploadet:", nrow(data), "rækker,", ncol(data), "kolonner"),
          type = "message",
          duration = 3
        )
      }
      
    }, error = function(e) {
      showNotification(
        paste("Fejl ved upload:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  # Hovedtabel rendering
  output$main_data_table <- rhandsontable::renderRHandsontable({
    req(values$current_data)
    
    # CRITICAL: Include table_version to force re-render after restore
    version_trigger <- values$table_version
    cat("DEBUG: Rendering table with version", version_trigger, "\n")
    
    data <- values$current_data
    
    hot <- rhandsontable::rhandsontable(
      data,
      height = 400,
      stretchH = "all",
      contextMenu = TRUE,
      manualColumnResize = TRUE,
      fillHandle = list(direction = "vertical", autoInsertRow = FALSE),
      useTypes = FALSE  # CRITICAL: Disable types for better editability after restore
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
    
    cat("DEBUG: Table rendered without column type definitions\n")
    
    return(hot)
  })
  
  # Håndtering af tabel ændringer
  observeEvent(input$main_data_table, {
    cat("DEBUG: Table change event triggered\n")
    
    if (values$updating_table) {
      cat("DEBUG: Skipping table change - updating_table is TRUE\n")
      return()
    }
    
    if (values$restoring_session) {
      cat("DEBUG: Skipping table change - restoring_session is TRUE\n")
      return()
    }
    
    req(input$main_data_table)
    
    values$updating_table <- TRUE
    on.exit({ 
      values$updating_table <- FALSE 
      cat("DEBUG: Table change processing complete\n")
    }, add = TRUE)
    
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
      # Tilføj altid en "Ingen (tom)" option sammen med "Vælg kolonne..."
      col_choices <- setNames(
        c("", "BLANK", all_cols), 
        c("Vælg kolonne...", "Ingen (tom)", all_cols)
      )
      
      isolate({
        updateSelectInput(session, "x_column", choices = col_choices)
        updateSelectInput(session, "y_column", choices = col_choices)
        updateSelectInput(session, "n_column", choices = col_choices)
      })
      
      # Kør auto-detect hvis det ikke er gjort endnu OG ingen kolonner er valgt
      if (!values$auto_detect_done && 
          (is.null(input$x_column) || input$x_column == "" || input$x_column == "BLANK")) {
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
    
    # ÆNDRING: Opdater dropdowns til at VISE de detekterede værdier
    isolate({
      if (!is.null(x_col)) {
        updateSelectInput(session, "x_column", selected = x_col)
      }
      
      if (!is.null(taeller_col)) {
        updateSelectInput(session, "y_column", selected = taeller_col)
      }
      
      if (!is.null(naevner_col)) {
        updateSelectInput(session, "n_column", selected = naevner_col)
      } else {
        # Hvis ingen nævner er detekteret, sæt til blank
        updateSelectInput(session, "n_column", selected = "BLANK")
      }
      
      detected_msg <- paste0(
        "Auto-detekteret og opdateret dropdowns: ",
        "X=", if(is.null(x_col)) "ingen" else x_col, ", ",
        "Y=", if(is.null(taeller_col)) "ingen" else taeller_col,
        if (!is.null(naevner_col)) paste0(", N=", naevner_col) else ", N=ingen"
      )
      
      showNotification(
        detected_msg,
        type = "message",
        duration = 4
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
    x_col <- if (!is.null(input$x_column) && input$x_column != "" && input$x_column != "BLANK") input$x_column else NULL
    y_col <- if (!is.null(input$y_column) && input$y_column != "" && input$y_column != "BLANK") input$y_column else NULL
    n_col <- if (!is.null(input$n_column) && input$n_column != "" && input$n_column != "BLANK") input$n_column else NULL
    
    return(list(
      x_col = x_col,
      y_col = y_col,
      n_col = n_col,
      chart_type = get_qic_chart_type(if(is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type)
    ))
  })
  
  # Kolonne validering output
  output$column_validation_messages <- renderUI({
    req(values$current_data)
    
    if ((is.null(input$x_column) || input$x_column == "" || input$x_column == "BLANK") ||
        (is.null(input$y_column) || input$y_column == "" || input$y_column == "BLANK")) {
      return(NULL)
    }
    
    chart_type <- get_qic_chart_type(if(is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type)
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
      if (is.null(input$n_column) || input$n_column == "" || input$n_column == "BLANK") {
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
    selected_cols <- selected_cols[!is.null(selected_cols) & selected_cols != "" & selected_cols != "BLANK"]
    
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
      chart_selection <- if(is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type
      get_qic_chart_type(chart_selection)
    }),
    show_targets_reactive = reactive(if(is.null(input$show_targets)) FALSE else input$show_targets),
    show_phases_reactive = reactive(if(is.null(input$show_phases)) FALSE else input$show_phases),
    chart_title_reactive = chart_title
  )
  
  # Plot ready check
  output$plot_ready <- reactive({
    result <- !is.null(visualization$plot_ready()) && visualization$plot_ready()
    return(result)
  })
  outputOptions(output, "plot_ready", suspendWhenHidden = FALSE)
  
  # FIXED: Auto-save når data ændres med bedre guards
  observeEvent(values$current_data, {
    # CRITICAL: Flere guards for at forhindre auto-save under restore
    if (!values$auto_save_enabled || 
        values$updating_table || 
        values$restoring_session || 
        !values$app_initialized) {
      cat("DEBUG: Auto-save skipped - guards active\n")
      return()
    }
    
    if (!is.null(values$current_data) && 
        nrow(values$current_data) > 0 && 
        any(!is.na(values$current_data))) {
      
      cat("DEBUG: Triggering auto-save for data change\n")
      
      metadata <- list(
        title = isolate(input$indicator_title),
        unit_type = isolate(input$unit_type),
        unit_select = isolate(input$unit_select),
        unit_custom = isolate(input$unit_custom),
        description = isolate(input$indicator_description),
        x_column = if(is.null(isolate(input$x_column)) || isolate(input$x_column) == "BLANK") "" else isolate(input$x_column),
        y_column = if(is.null(isolate(input$y_column)) || isolate(input$y_column) == "BLANK") "" else isolate(input$y_column),
        n_column = if(is.null(isolate(input$n_column)) || isolate(input$n_column) == "BLANK") "" else isolate(input$n_column),
        chart_type = isolate(input$chart_type)
      )
      
      autoSaveAppState(session, values$current_data, metadata)
      values$last_save_time <- Sys.time()
    }
  }, ignoreInit = TRUE)
  
  # FIXED: Auto-save når indstillinger ændres med guards og debounce
  observe({
    # CRITICAL: Samme guards som data auto-save
    if (!values$auto_save_enabled || 
        values$updating_table || 
        values$restoring_session || 
        !values$app_initialized) {
      return()
    }
    
    if (!is.null(values$current_data)) {
      cat("DEBUG: Settings changed - scheduling auto-save\n")
      
      metadata <- list(
        title = input$indicator_title,
        unit_type = input$unit_type,
        unit_select = input$unit_select,
        unit_custom = input$unit_custom,
        description = input$indicator_description,
        x_column = if(is.null(input$x_column) || input$x_column == "BLANK") "" else input$x_column,
        y_column = if(is.null(input$y_column) || input$y_column == "BLANK") "" else input$y_column,
        n_column = if(is.null(input$n_column) || input$n_column == "BLANK") "" else input$n_column,
        chart_type = input$chart_type
      )
      
      # Debounce med 2 sekunder delay
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
  
  # KOMPLET EXPORT - Excel version
  output$download_complete_excel <- downloadHandler(
    filename = function() {
      title_clean <- gsub("[^A-Za-z0-9æøåÆØÅ ]", "", chart_title())
      title_clean <- gsub(" ", "_", title_clean)
      if (nchar(title_clean) == 0) title_clean <- "SPC_Analyse"
      paste0("SPC_Session_", title_clean, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      if (!is.null(active_data())) {
        tryCatch({
          cat("DEBUG: Creating Excel complete export with 2 sheets\n")
          
          # Opret Excel workbook
          wb <- createWorkbook()
          
          # ============== SHEET 1: DATA ==============
          addWorksheet(wb, "Data")
          
          # Skriv data til sheet med professionel formatering
          writeData(wb, "Data", active_data(), startRow = 1, startCol = 1, 
                   headerStyle = createStyle(
                     textDecoration = "bold",
                     fgFill = HOSPITAL_COLORS$primary,
                     fontColour = "white",
                     border = "TopBottomLeftRight",
                     fontSize = 12
                   ))
          
          # Formatering af data sheet
          addStyle(wb, "Data", 
                  style = createStyle(
                    border = "TopBottomLeftRight", 
                    wrapText = TRUE
                  ), 
                  rows = 2:(nrow(active_data()) + 1), 
                  cols = 1:ncol(active_data()), 
                  gridExpand = TRUE)
          
          # Auto-width kolonner og frys header
          setColWidths(wb, "Data", cols = 1:ncol(active_data()), widths = "auto")
          freezePane(wb, "Data", firstActiveRow = 2)
          
          # ============== SHEET 2: METADATA ==============
          addWorksheet(wb, "Metadata")
          
          # Opret kombineret session information
          session_lines <- c(
            paste(HOSPITAL_NAME, "- SPC ANALYSE"),
            "════════════════════════════════════════════════════════",
            "",
            "INDIKATOR INFORMATION:",
            paste("• Titel:", if(is.null(input$indicator_title) || input$indicator_title == "") "Ikke angivet" else input$indicator_title),
            paste("• Enhed:", current_unit()),
            paste("• Beskrivelse:", if(is.null(input$indicator_description) || input$indicator_description == "") "Ikke angivet" else input$indicator_description),
            "",
            "GRAF KONFIGURATION:",
            paste("• Chart Type:", if(is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type),
            paste("• X-akse:", if(is.null(input$x_column) || input$x_column == "" || input$x_column == "BLANK") "Ikke valgt" else input$x_column, "(tid/observation)"),
            paste("• Y-akse:", if(is.null(input$y_column) || input$y_column == "" || input$y_column == "BLANK") "Ikke valgt" else input$y_column, "(værdier)"),
            if (!is.null(input$n_column) && input$n_column != "" && input$n_column != "BLANK") paste("• Nævner:", input$n_column) else NULL,
            paste("• Målsætninger:", ifelse(if(is.null(input$show_targets)) FALSE else input$show_targets, "Vist", "Skjult")),
            paste("• Faser:", ifelse(if(is.null(input$show_phases)) FALSE else input$show_phases, "Vist", "Skjult")),
            "",
            "DATA INFORMATION:",
            paste("• Rækker:", nrow(active_data())),
            paste("• Kolonner:", ncol(active_data())),
            paste("• Kolonnenavne:", paste(names(active_data()), collapse = ", ")),
            paste("• Data kilde:", if (values$file_uploaded) "File Upload" else "Manuel indtastning"),
            paste("• Eksporteret:", format(Sys.time(), "%d-%m-%Y %H:%M")),
            "",
            "TEKNISK INFORMATION:",
            paste("• App Version: BFH_SPC_v1.2"),
            paste("• Chart Type Code:", get_qic_chart_type(if(is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type)),
            "",
            "════════════════════════════════════════════════════════",
            "IMPORT INSTRUKTIONER:",
            "• For at re-importere: Rediger data i 'Data' sheet og gem filen",
            "• Bevar kolonnenavnene og strukturen",
            "• Slet eller tilføj rækker efter behov",
            "• Import filen i SPC appen som Excel fil",
            "",
            paste("Genereret af:", HOSPITAL_NAME, "SPC App")
          )
          
          # Fjern NULL values
          session_lines <- session_lines[!is.null(session_lines)]
          
          # Skriv session info
          session_df <- data.frame(Information = session_lines, stringsAsFactors = FALSE)
          writeData(wb, "Metadata", session_df, startRow = 1, startCol = 1, colNames = FALSE)
          
          # Formatering af session info
          setColWidths(wb, "Metadata", cols = 1, widths = 85)
          
          # Style til header og separatorer
          header_style <- createStyle(
            fontSize = 16, 
            textDecoration = "bold",
            fgFill = HOSPITAL_COLORS$primary,
            fontColour = "white",
            halign = "center"
          )
          addStyle(wb, "Metadata", header_style, rows = 1, cols = 1)
          
          # Style til section headers
          section_rows <- which(grepl("^[A-ZÆØÅ ]+:$", session_lines))
          if (length(section_rows) > 0) {
            section_style <- createStyle(
              fontSize = 12,
              textDecoration = "bold",
              fgFill = HOSPITAL_COLORS$light
            )
            addStyle(wb, "Metadata", section_style, rows = section_rows, cols = 1)
          }
          
          # Gem Excel fil
          saveWorkbook(wb, file, overwrite = TRUE)
          
          cat("DEBUG: Excel complete export created successfully\n")
          
          showNotification(
            paste("Komplet Excel session eksporteret:", basename(file)),
            type = "message",
            duration = 4
          )
          
        }, error = function(e) {
          cat("ERROR during Excel export:", e$message, "\n")
          showNotification(
            paste("Fejl ved Excel eksport:", e$message),
            type = "error",
            duration = 5
          )
        })
      } else {
        showNotification(
          "Ingen data at eksportere",
          type = "warning",
          duration = 3
        )
      }
    }
  )
  
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