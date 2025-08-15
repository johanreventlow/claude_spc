# R/modules/data_module.R
library(shiny)
library(DT)
library(readr)
library(readxl)
library(dplyr)
library(shinyjs)
library(rhandsontable)  # Add rhandsontable

# Helper functions
`%||%` <- function(x, y) if(is.null(x)) y else x

# Data Module UI
dataModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # File upload section
    card(
      card_header(
        div(
          icon("database"), 
          " Data Upload",
          style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 500;")
        )
      ),
      card_body(
        # Status indicator (reactive output)
        uiOutput(ns("upload_status")),
        
        # File input
        fileInput(
          ns("data_file"),
          "Upload CSV/Excel fil:",
          accept = c(".csv", ".xlsx", ".xls", ".tsv"),
          placeholder = "Vælg fil...",
          buttonLabel = "Browse...",
          multiple = FALSE
        ),
        
        # Import settings (initially hidden)
        conditionalPanel(
          condition = paste0("output['", ns("show_settings"), "']"),
          
          hr(),
          
          h6("Import Indstillinger:", style = "font-weight: 500; margin-bottom: 10px;"),
          
          fluidRow(
            column(6,
                   selectInput(
                     ns("separator"),
                     "Separator:",
                     choices = list(
                       "Semikolon (;)" = ";",    # Dansk standard først!
                       "Komma (,)" = ",",
                       "Tab" = "\t",
                       "Pipe (|)" = "|"
                     ),
                     selected = ";"  # Dansk standard
                   )
            ),
            column(6,
                   selectInput(
                     ns("decimal"),
                     "Decimal separator:",
                     choices = list(
                       "Komma (,)" = ",",        # Dansk standard først!
                       "Punktum (.)" = "."
                     ),
                     selected = ","  # Dansk standard
                   )
            )
          ),
          
          fluidRow(
            column(6,
                   selectInput(
                     ns("encoding"),
                     "Encoding:",
                     choices = list(
                       "ISO-8859-1 (Dansk standard)" = "ISO-8859-1",  # Dansk standard først
                       "UTF-8" = "UTF-8",
                       "Windows-1252" = "windows-1252"
                     ),
                     selected = "ISO-8859-1"  # Bedre for danske systemer
                   )
            ),
            column(6,
                   checkboxInput(
                     ns("header"),
                     "Første række er kolonnenavne",
                     value = TRUE
                   )
            )
          ),
          
          # Re-import button
          actionButton(
            ns("reimport"),
            "Genindlæs med nye indstillinger",
            icon = icon("refresh"),
            class = "btn-outline-primary btn-sm w-100"
          )
        )
      )
    ),
    
    # Data preview section (now with basic table)
    conditionalPanel(
      condition = paste0("output['", ns("show_preview"), "']"),
      
      br(),
      
      card(
        card_header(
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            div(
              icon("table"),
              " Data Preview",
              style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 500;")
            ),
            div(
              uiOutput(ns("data_info")),
              style = "font-size: 0.85rem; color: #666;"
            )
          )
        ),
        card_body(
          # Validation messages
          div(id = ns("validation_messages")),
          
          # Simple data table (not editable in sidebar)
          div(
            style = "max-height: 300px; overflow-y: auto;",
            DT::dataTableOutput(ns("preview_table"))
          ),
          
          br(),
          
          # Link to editable table
          div(
            style = "text-align: center; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
            icon("edit"),
            " Gå til ",
            strong("Data-fanen"),
            " for at redigere tabellen direkte",
            style = "color: #666; font-size: 0.9rem;"
          )
        )
      )
    )
  )
}

# NEW: Editable Table Module UI (for Data tab)
editableTableUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Data controls
    fluidRow(
      column(
        8,
        # Editable table status
        uiOutput(ns("table_status"))
      ),
      column(
        4,
        div(
          style = "text-align: right;",
          # Table controls
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
    
    # Validation messages for editable table
    uiOutput(ns("edit_validation_messages")),
    
    # Editable table
    conditionalPanel(
      condition = paste0("output['", ns("has_data"), "']"),
      
      div(
        style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px; background-color: white;",
        
        # Table controls info
        div(
          style = "margin-bottom: 10px; padding: 8px; background-color: #f8f9fa; border-radius: 3px; font-size: 0.85rem; color: #666;",
          icon("info-circle"),
          strong(" Redigeringstips: "), 
          "Dobbeltklik for at redigere • Tab/Enter for næste celle • Ctrl+Z for fortryd • Højreklik for kontekstmenu"
        ),
        
        # The editable table
        rhandsontable::rHandsontableOutput(ns("editable_table"), height = "500px")
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
    ),
    
    br(),
    
    # Calculated fields section (when data available)
    conditionalPanel(
      condition = paste0("output['", ns("has_data"), "']"),
      
      card(
        card_header(
          div(
            icon("calculator"),
            " Beregnede Felter",
            style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 500;")
          )
        ),
        card_body(
          fluidRow(
            column(4,
                   h6("Tilføj beregnet kolonne:", style = "font-weight: 500;"),
                   selectInput(
                     ns("calc_type"),
                     "Type:",
                     choices = list(
                       "Procent (tæller/nævner)" = "percent",
                       "Rate pr. 1000" = "rate_1000",
                       "Rate pr. 10000" = "rate_10000", 
                       "Rullende gennemsnit (3)" = "rolling_mean_3",
                       "Rullende gennemsnit (7)" = "rolling_mean_7"
                     )
                   )
            ),
            column(4,
                   h6("Vælg kolonner:", style = "font-weight: 500;"),
                   uiOutput(ns("calc_columns"))
            ),
            column(4,
                   h6("Handling:", style = "font-weight: 500; margin-bottom: 19px;"),
                   actionButton(
                     ns("add_calculated"),
                     "Tilføj kolonne",
                     icon = icon("plus"),
                     class = "btn-primary btn-sm w-100"
                   )
            )
          )
        )
      )
    )
  )
}

# Data Module Server (updated)
dataModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      raw_data = NULL,
      processed_data = NULL,
      file_info = NULL,
      validation_results = NULL,
      upload_success = FALSE,
      status_message = "Venter på data...",
      status_type = "warning"
    )
    
    # Show/hide conditional panels
    output$show_settings <- reactive({
      !is.null(values$file_info) && values$file_info$type == "csv"
    })
    outputOptions(output, "show_settings", suspendWhenHidden = FALSE)
    
    output$show_preview <- reactive({
      !is.null(values$processed_data)
    })
    outputOptions(output, "show_preview", suspendWhenHidden = FALSE)
    
    # Render status indicator
    output$upload_status <- renderUI({
      status_class <- switch(values$status_type,
                             "ready" = "status-ready",
                             "warning" = "status-warning", 
                             "error" = "status-error",
                             "processing" = "status-processing"
      )
      
      div(
        span(class = paste("status-indicator", status_class)),
        values$status_message,
        style = "font-size: 0.9rem; margin-bottom: 15px;"
      )
    })
    
    # Render data info
    output$data_info <- renderUI({
      req(values$processed_data, values$file_info)
      
      data <- values$processed_data
      file_info <- values$file_info
      
      info_text <- paste0(
        file_info$name, " • ",
        nrow(data), " rækker • ",
        ncol(data), " kolonner • ",
        format(file_info$size, units = "KB")
      )
      
      return(info_text)
    })
    
    # Helper function to update status indicator
    updateStatusIndicator <- function(status, message) {
      values$status_type <- status
      values$status_message <- message
    }
    
    # File upload handler
    observeEvent(input$data_file, {
      req(input$data_file)
      
      # Update status
      updateStatusIndicator("processing", "Indlæser fil...")
      
      # Get file info
      file_path <- input$data_file$datapath
      file_name <- input$data_file$name
      file_ext <- tools::file_ext(file_name)
      
      values$file_info <- list(
        path = file_path,
        name = file_name,
        type = if(file_ext %in% c("xlsx", "xls")) "excel" else "csv",
        size = file.size(file_path)
      )
      
      # Try to read the file
      tryCatch({
        if(values$file_info$type == "excel") {
          # Read Excel file
          data <- readExcelFile(file_path)
          showNotification("Excel fil indlæst med danske indstillinger", type = "message", duration = 3)
        } else {
          # Read CSV file with DANISH default settings
          data <- readCSVFile(file_path, 
                              sep = ";",           # Dansk standard
                              decimal = ",",       # Dansk standard
                              encoding = "ISO-8859-1",  # Bedre for danske filer
                              header = TRUE)
          showNotification("CSV fil indlæst med danske indstillinger (;-separeret, ,-decimal)", type = "message", duration = 3)
        }
        
        values$raw_data <- data
        processData()
        
      }, error = function(e) {
        updateStatusIndicator("error", paste("Fejl ved indlæsning:", e$message))
        showNotification(
          paste("Kunne ikke indlæse fil:", e$message),
          type = "error",
          duration = 10
        )
      })
    })
    
    # Re-import with new settings
    observeEvent(input$reimport, {
      req(values$file_info, values$file_info$type == "csv")
      
      updateStatusIndicator("processing", "Genindlæser fil...")
      
      tryCatch({
        data <- readCSVFile(values$file_info$path,
                            sep = input$separator,
                            decimal = input$decimal,
                            encoding = input$encoding,
                            header = input$header)
        
        values$raw_data <- data
        processData()
        
        showNotification("Fil genindlæst med nye indstillinger", type = "message", duration = 3)
        
      }, error = function(e) {
        updateStatusIndicator("error", paste("Fejl ved genindlæsning:", e$message))
        showNotification(
          paste("Kunne ikke genindlæse fil:", e$message),
          type = "error"
        )
      })
    })
    
    # Process and validate data
    processData <- function() {
      req(values$raw_data)
      
      data <- values$raw_data
      
      # Basic data processing
      # Remove completely empty rows
      data <- data[!apply(is.na(data) | data == "", 1, all), ]
      
      # Store processed data
      values$processed_data <- data
      
      # Validate data structure
      validation <- validateDataStructure(data)
      values$validation_results <- validation
      
      # Update status based on validation
      if(validation$valid) {
        updateStatusIndicator("ready", paste("Data indlæst:", nrow(data), "rækker,", ncol(data), "kolonner"))
        values$upload_success <- TRUE
      } else {
        updateStatusIndicator("warning", "Data indlæst med advarsler")
        values$upload_success <- FALSE
      }
    }
    
    # Render validation messages
    output$validation_messages <- renderUI({
      req(values$validation_results)
      
      validation <- values$validation_results
      
      if(length(validation$warnings) > 0 || length(validation$errors) > 0) {
        
        messages <- tagList()
        
        # Show errors
        if(length(validation$errors) > 0) {
          messages <- tagList(messages,
                              div(
                                class = "alert alert-danger",
                                style = "margin-bottom: 15px;",
                                icon("exclamation-triangle"),
                                strong(" Fejl i data:"),
                                tags$ul(
                                  lapply(validation$errors, function(err) tags$li(err))
                                )
                              )
          )
        }
        
        # Show warnings  
        if(length(validation$warnings) > 0) {
          messages <- tagList(messages,
                              div(
                                class = "alert alert-warning", 
                                style = "margin-bottom: 15px;",
                                icon("exclamation-circle"),
                                strong(" Advarsler:"),
                                tags$ul(
                                  lapply(validation$warnings, function(warn) tags$li(warn))
                                )
                              )
          )
        }
        
        return(messages)
      }
      
      # No issues
      return(
        div(
          class = "alert alert-success",
          style = "margin-bottom: 15px;",
          icon("check-circle"),
          strong(" Data ser godt ud! "), 
          "Ingen problemer fundet i datastrukturen."
        )
      )
    })
    
    # Render data preview table (simplified for sidebar)
    output$preview_table <- DT::renderDataTable({
      req(values$processed_data)
      
      data <- values$processed_data
      
      # Limit preview to first 50 rows for performance  
      preview_data <- if(nrow(data) > 50) {
        data[1:50, ]
      } else {
        data
      }
      
      DT::datatable(
        preview_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "200px",
          searching = FALSE,
          ordering = FALSE,
          info = FALSE,
          paging = FALSE,
          dom = 't',  # Only table
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover compact'
      )
    })
    
    # Return reactive data for use by other modules
    return(
      list(
        data = reactive(values$processed_data),
        file_info = reactive(values$file_info),
        validation = reactive(values$validation_results),
        upload_success = reactive(values$upload_success)
      )
    )
  })
}

# NEW: Editable Table Module Server
editableTableServer <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for editable table
    values <- reactiveValues(
      current_data = NULL,
      original_data = NULL,
      history = list(),
      history_position = 0,
      selected_row = NULL
    )
    
    # Initialize data when available
    observe({
      req(data_reactive())
      
      if(is.null(values$original_data)) {
        values$original_data <- data_reactive()
        values$current_data <- data_reactive()
        
        # Initialize history
        values$history <- list(data_reactive())
        values$history_position <- 1
      }
    })
    
    # Has data check
    output$has_data <- reactive({
      !is.null(values$current_data)
    })
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)
    
    # Table status
    output$table_status <- renderUI({
      if(is.null(values$current_data)) {
        div(
          span(class = "status-indicator status-warning"),
          "Ingen data tilgængelig",
          style = "font-size: 1rem;"
        )
      } else {
        changes_made <- !identical(values$current_data, values$original_data)
        status_class <- if(changes_made) "status-processing" else "status-ready"
        status_text <- if(changes_made) {
          "Data modificeret - ændringer er midlertidige"
        } else {
          "Data er synkroniseret med original"
        }
        
        div(
          span(class = paste("status-indicator", status_class)),
          status_text,
          style = "font-size: 1rem;"
        )
      }
    })
    
    # Render editable table
    output$editable_table <- rhandsontable::renderRHandsontable({
      req(values$current_data)
      
      data <- values$current_data
      
      # Create rhandsontable with Danish settings
      hot <- rhandsontable::rhandsontable(
        data,
        height = 500,
        stretchH = "all",
        contextMenu = TRUE,
        manualColumnResize = TRUE,
        manualRowResize = TRUE,
        fillHandle = list(direction = "vertical", autoInsertRow = FALSE)
      ) %>%
        rhandsontable::hot_context_menu(
          allowRowEdit = TRUE,
          allowColEdit = FALSE,
          allowReadOnly = FALSE,
          allowComments = FALSE
        ) %>%
        rhandsontable::hot_table(
          highlightCol = TRUE, 
          highlightRow = TRUE,
          stretchH = "all"
        )
      
      # Apply column-specific validation and formatting
      for(i in 1:ncol(data)) {
        col_name <- names(data)[i]
        col_data <- data[[i]]
        
        # Check if column is numeric (handle Danish decimals)
        if(is.numeric(col_data) || 
           sum(!is.na(suppressWarnings(as.numeric(gsub(",", ".", as.character(col_data)))))) > length(col_data) * 0.8) {
          
          # Numeric column
          hot <- hot %>%
            rhandsontable::hot_col(
              col = i,
              type = "numeric",
              format = "0,0.00",  # Danish number format
              validator = "
                function(value, callback) {
                  // Allow Danish decimal separator
                  var numericValue = parseFloat(value.toString().replace(',', '.'));
                  if (isNaN(numericValue) && value !== null && value !== '') {
                    callback(false);
                  } else {
                    callback(true);
                  }
                }
              "
            )
        } else {
          # Text column
          hot <- hot %>%
            rhandsontable::hot_col(
              col = i,
              type = "text"
            )
        }
      }
      
      return(hot)
    })
    
    # Handle table changes
    observeEvent(input$editable_table, {
      req(input$editable_table)
      
      # Get changed data
      new_data <- rhandsontable::hot_to_r(input$editable_table)
      
      # Update current data
      values$current_data <- new_data
      
      # Add to history (limit history to 50 steps)
      values$history_position <- values$history_position + 1
      values$history <- c(values$history[1:values$history_position - 1], list(new_data))
      
      if(length(values$history) > 50) {
        values$history <- values$history[-1]
        values$history_position <- values$history_position - 1
      }
    })
    
    # Add row
    observeEvent(input$add_row, {
      req(values$current_data)
      
      # Create new empty row
      new_row <- values$current_data[1, ]
      new_row[1, ] <- NA
      
      # Add to data
      values$current_data <- rbind(values$current_data, new_row)
      
      # Add to history
      addToHistory(values$current_data)
      
      showNotification("Ny række tilføjet", type = "message", duration = 2)
    })
    
    # Delete row (delete last row if none selected)
    observeEvent(input$delete_row, {
      req(values$current_data, nrow(values$current_data) > 1)
      
      # Remove last row
      values$current_data <- values$current_data[-nrow(values$current_data), ]
      
      # Add to history
      addToHistory(values$current_data)
      
      showNotification("Række slettet", type = "message", duration = 2)
    })
    
    # Undo
    observeEvent(input$undo, {
      if(values$history_position > 1) {
        values$history_position <- values$history_position - 1
        values$current_data <- values$history[[values$history_position]]
        showNotification("Ændring fortrudt", type = "message", duration = 2)
      }
    })
    
    # Redo
    observeEvent(input$redo, {
      if(values$history_position < length(values$history)) {
        values$history_position <- values$history_position + 1
        values$current_data <- values$history[[values$history_position]]
        showNotification("Ændring gentaget", type = "message", duration = 2)
      }
    })
    
    # Reset to original
    observeEvent(input$reset, {
      req(values$original_data)
      
      showModal(modalDialog(
        title = "Reset data",
        "Er du sikker på at du vil nulstille alle ændringer og gå tilbage til original data?",
        footer = tagList(
          modalButton("Annuller"),
          actionButton(ns("confirm_reset"), "Ja, reset", class = "btn-danger")
        )
      ))
    })
    
    observeEvent(input$confirm_reset, {
      values$current_data <- values$original_data
      values$history <- list(values$original_data)
      values$history_position <- 1
      
      removeModal()
      showNotification("Data nulstillet til original", type = "message", duration = 3)
    })
    
    # Helper function to add to history
    addToHistory <- function(data) {
      values$history_position <- values$history_position + 1
      values$history <- c(values$history[1:values$history_position - 1], list(data))
      
      if(length(values$history) > 50) {
        values$history <- values$history[-1]
        values$history_position <- values$history_position - 1
      }
    }
    
    # Validation messages for edited data
    output$edit_validation_messages <- renderUI({
      req(values$current_data)
      
      validation <- validateDataStructure(values$current_data)
      
      if(length(validation$warnings) > 0 || length(validation$errors) > 0) {
        
        messages <- tagList()
        
        # Show errors
        if(length(validation$errors) > 0) {
          messages <- tagList(messages,
                              div(
                                class = "alert alert-danger",
                                style = "margin-bottom: 15px;",
                                icon("exclamation-triangle"),
                                strong(" Fejl i redigeret data:"),
                                tags$ul(
                                  lapply(validation$errors, function(err) tags$li(err))
                                )
                              )
          )
        }
        
        # Show warnings  
        if(length(validation$warnings) > 0) {
          messages <- tagList(messages,
                              div(
                                class = "alert alert-warning", 
                                style = "margin-bottom: 15px;",
                                icon("exclamation-circle"),
                                strong(" Advarsler for redigeret data:"),
                                tags$ul(
                                  lapply(validation$warnings, function(warn) tags$li(warn))
                                )
                              )
          )
        }
        
        return(messages)
      }
      
      return(NULL)  # No validation messages if all good
    })
    
    # Calculated columns UI
    output$calc_columns <- renderUI({
      req(values$current_data)
      
      numeric_cols <- names(values$current_data)[sapply(values$current_data, function(x) {
        is.numeric(x) || sum(!is.na(suppressWarnings(as.numeric(gsub(",", ".", as.character(x)))))) > length(x) * 0.8
      })]
      
      calc_type <- input$calc_type %||% "percent"
      
      if(calc_type == "percent") {
        tagList(
          selectInput(ns("numerator_col"), "Tæller:", choices = numeric_cols),
          selectInput(ns("denominator_col"), "Nævner:", choices = numeric_cols)
        )
      } else if(calc_type %in% c("rate_1000", "rate_10000")) {
        tagList(
          selectInput(ns("events_col"), "Hændelser:", choices = numeric_cols),
          selectInput(ns("population_col"), "Population:", choices = numeric_cols)
        )
      } else {
        selectInput(ns("value_col"), "Værdi kolonne:", choices = numeric_cols)
      }
    })
    
    # Add calculated column
    observeEvent(input$add_calculated, {
      req(values$current_data, input$calc_type)
      
      tryCatch({
        data <- values$current_data
        calc_type <- input$calc_type
        
        if(calc_type == "percent") {
          req(input$numerator_col, input$denominator_col)
          num_data <- as.numeric(gsub(",", ".", as.character(data[[input$numerator_col]])))
          den_data <- as.numeric(gsub(",", ".", as.character(data[[input$denominator_col]])))
          new_col <- round((num_data / den_data) * 100, 2)
          col_name <- paste0("Procent_", input$numerator_col, "_", input$denominator_col)
          
        } else if(calc_type == "rate_1000") {
          req(input$events_col, input$population_col)
          events <- as.numeric(gsub(",", ".", as.character(data[[input$events_col]])))
          pop <- as.numeric(gsub(",", ".", as.character(data[[input$population_col]])))
          new_col <- round((events / pop) * 1000, 2)
          col_name <- paste0("Rate1000_", input$events_col)
          
        } else if(calc_type == "rate_10000") {
          req(input$events_col, input$population_col)
          events <- as.numeric(gsub(",", ".", as.character(data[[input$events_col]])))
          pop <- as.numeric(gsub(",", ".", as.character(data[[input$population_col]])))
          new_col <- round((events / pop) * 10000, 2)
          col_name <- paste0("Rate10000_", input$events_col)
          
        } else if(calc_type == "rolling_mean_3") {
          req(input$value_col)
          values_data <- as.numeric(gsub(",", ".", as.character(data[[input$value_col]])))
          new_col <- zoo::rollmean(values_data, k = 3, fill = NA, align = "right")
          col_name <- paste0("RullGns3_", input$value_col)
          
        } else if(calc_type == "rolling_mean_7") {
          req(input$value_col)
          values_data <- as.numeric(gsub(",", ".", as.character(data[[input$value_col]])))
          new_col <- zoo::rollmean(values_data, k = 7, fill = NA, align = "right")
          col_name <- paste0("RullGns7_", input$value_col)
        }
        
        # Add column to data
        data[[col_name]] <- new_col
        values$current_data <- data
        
        # Add to history
        addToHistory(data)
        
        showNotification(paste("Beregnet kolonne", col_name, "tilføjet"), type = "message", duration = 3)
        
      }, error = function(e) {
        showNotification(paste("Fejl ved beregning:", e$message), type = "error", duration = 5)
      })
    })
    
    # Return current data for use by other modules
    return(
      reactive(values$current_data)
    )
  })
}

# Keep all existing helper functions...
# (readCSVFile, readExcelFile, validateDataStructure remain the same)

# Read CSV file with error handling - Danish-optimized
readCSVFile <- function(file_path, sep = ";", decimal = ",", encoding = "UTF-8", header = TRUE) {
  
  # Try different reading approaches
  tryCatch({
    # For Danish CSV files, use read_csv2 when sep=";" and decimal=","
    if(sep == ";" && decimal == ",") {
      data <- readr::read_csv2(
        file_path,
        locale = readr::locale(
          decimal_mark = ",",
          grouping_mark = ".",
          encoding = encoding
        ),
        col_names = header,
        show_col_types = FALSE,
        trim_ws = TRUE
      )
    } else if(decimal == ",") {
      # Custom delimiter with comma decimal
      data <- readr::read_delim(
        file_path,
        delim = sep,
        locale = readr::locale(
          decimal_mark = ",",
          grouping_mark = ".",
          encoding = encoding
        ),
        col_names = header,
        show_col_types = FALSE,
        trim_ws = TRUE
      )
    } else {
      # Standard delimited file
      data <- readr::read_delim(
        file_path,
        delim = sep,
        locale = readr::locale(
          decimal_mark = decimal,
          grouping_mark = if(decimal == ",") "." else ",", 
          encoding = encoding
        ),
        col_names = header,
        show_col_types = FALSE,
        trim_ws = TRUE
      )
    }
    
    # Convert to data.frame and clean up
    data <- as.data.frame(data)
    
    # Clean column names (remove extra spaces, etc.)
    names(data) <- trimws(names(data))
    
    return(data)
    
  }, error = function(e1) {
    # Fallback: base R read functions with proper Danish settings
    tryCatch({
      data <- read.csv(
        file_path,
        sep = sep,
        dec = decimal,
        header = header,
        fileEncoding = encoding,
        stringsAsFactors = FALSE,
        strip.white = TRUE
      )
      
      # Clean column names
      names(data) <- trimws(names(data))
      
      return(data)
      
    }, error = function(e2) {
      # More specific error message for Danish files
      if(sep == ";" && decimal == ",") {
        stop(paste("Kunne ikke læse dansk CSV fil (;-separeret, ,-decimal).", 
                   "Prøv at ændre encoding til ISO-8859-1 eller kontroller filformat.", 
                   "Fejl:", e1$message))
      } else {
        stop(paste("Kunne ikke læse CSV fil. Prøv andre import-indstillinger.", 
                   "Fejl:", e1$message))
      }
    })
  })
}

# Read Excel file with error handling
readExcelFile <- function(file_path) {
  
  tryCatch({
    # Get sheet names
    sheets <- readxl::excel_sheets(file_path)
    
    # Read first sheet (or let user choose later)
    data <- readxl::read_excel(
      file_path,
      sheet = sheets[1],
      col_names = TRUE,
      .name_repair = "minimal"
    )
    
    # Convert to data.frame
    return(as.data.frame(data))
    
  }, error = function(e) {
    stop(paste("Kunne ikke læse Excel fil:", e$message))
  })
}

# Validate data structure for SPC analysis
validateDataStructure <- function(data) {
  
  errors <- character(0)
  warnings <- character(0)
  
  # Check if data exists and has content
  if(is.null(data) || nrow(data) == 0) {
    errors <- c(errors, "Ingen data fundet i filen")
    return(list(valid = FALSE, errors = errors, warnings = warnings))
  }
  
  if(ncol(data) < 2) {
    errors <- c(errors, "Data skal have mindst 2 kolonner (tid og værdi)")
  }
  
  # Check for potential time/date columns
  potential_date_cols <- character(0)
  potential_numeric_cols <- character(0)
  
  for(col_name in names(data)) {
    col_data <- data[[col_name]]
    
    # Skip completely empty columns
    if(all(is.na(col_data))) {
      warnings <- c(warnings, paste("Kolonne", col_name, "er helt tom"))
      next
    }
    
    # Check for potential date columns (fix grepl na.rm issue)
    if(any(grepl("\\d{4}-\\d{2}-\\d{2}|\\d{2}/\\d{2}/\\d{4}|\\d{2}-\\d{2}-\\d{4}", 
                 as.character(col_data)[!is.na(col_data)]))) {
      potential_date_cols <- c(potential_date_cols, col_name)
    }
    
    # Check for numeric columns (handle Danish decimal separator)
    if(is.numeric(col_data) || 
       sum(!is.na(suppressWarnings(as.numeric(gsub(",", ".", as.character(col_data)))))) > length(col_data) * 0.8) {
      potential_numeric_cols <- c(potential_numeric_cols, col_name)
    }
  }
  
  # Validate minimum requirements
  if(length(potential_numeric_cols) == 0) {
    errors <- c(errors, "Ingen numeriske kolonner fundet - mindst én numerisk kolonne er påkrævet for SPC analyse")
  }
  
  if(length(potential_date_cols) == 0 && nrow(data) > 1) {
    warnings <- c(warnings, "Ingen dato-kolonner fundet - overvej at tilføje tidsstempel for tidsserie-analyse")
  }
  
  # Check data size
  if(nrow(data) < 10) {
    warnings <- c(warnings, paste("Kun", nrow(data), "datapunkter fundet - SPC analyse er mest pålidelig med mindst 15-20 punkter"))
  }
  
  # Check for missing values
  missing_pct <- round(sum(is.na(data)) / (nrow(data) * ncol(data)) * 100, 1)
  if(missing_pct > 20) {
    warnings <- c(warnings, paste("Høj andel manglende værdier:", missing_pct, "% - dette kan påvirke analyse-kvaliteten"))
  }
  
  # Return validation results
  return(list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings,
    potential_date_cols = potential_date_cols,
    potential_numeric_cols = potential_numeric_cols,
    summary = list(
      rows = nrow(data),
      cols = ncol(data),
      missing_pct = missing_pct
    )
  ))
}