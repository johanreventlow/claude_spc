# R/modules/data_module.R
library(shiny)
library(DT)
library(readr)
library(readxl)
library(dplyr)
library(shinyjs)
library(rhandsontable)

# Read CSV file with error handling - Danish-optimized
readCSVFile <- function(file_path, sep = ";", decimal = ",", encoding = "UTF-8", header = TRUE) {
  
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
    # Fallback: base R read functions
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
      stop(paste("Kunne ikke læse CSV fil. Første fejl:", e1$message, "• Fallback-fejl:", e2$message))
    })
  })
}

# Read Excel file with error handling
readExcelFile <- function(file_path) {
  
  tryCatch({
    # Get sheet names
    sheets <- readxl::excel_sheets(file_path)
    
    # Read first sheet
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
    
    # Check for potential date columns
    char_data <- as.character(col_data)[!is.na(col_data)]
    if(length(char_data) > 0) {
      if(any(grepl("\\d{4}-\\d{2}-\\d{2}|\\d{2}/\\d{2}/\\d{4}|\\d{2}-\\d{2}-\\d{4}", char_data))) {
        potential_date_cols <- c(potential_date_cols, col_name)
      }
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
        # Status indicator
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
                       "Semikolon (;)" = ";",
                       "Komma (,)" = ",",
                       "Tab" = "\t",
                       "Pipe (|)" = "|"
                     ),
                     selected = ";"
                   )
            ),
            column(6,
                   selectInput(
                     ns("decimal"),
                     "Decimal separator:",
                     choices = list(
                       "Komma (,)" = ",",
                       "Punktum (.)" = "."
                     ),
                     selected = ","
                   )
            )
          ),
          
          fluidRow(
            column(6,
                   selectInput(
                     ns("encoding"),
                     "Encoding:",
                     choices = list(
                       "ISO-8859-1 (Dansk standard)" = "ISO-8859-1",
                       "UTF-8" = "UTF-8",
                       "Windows-1252" = "windows-1252"
                     ),
                     selected = "ISO-8859-1"
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
    
    # Data preview section
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
          
          # Simple data table
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

# Data Module Server
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
    
    # Helper function to update status indicator
    updateStatusIndicator <- function(status, message) {
      values$status_type <- status
      values$status_message <- message
    }
    
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
    
    # File upload handler
    observeEvent(input$data_file, {
      req(input$data_file)
      
      # Update status
      updateStatusIndicator("processing", "Indlæser fil...")
      
      # DEBUG: Print file info
      cat("DEBUG: File upload started\n")
      cat("DEBUG: File name:", input$data_file$name, "\n")
      cat("DEBUG: File size:", input$data_file$size, "\n")
      cat("DEBUG: File path:", input$data_file$datapath, "\n")
      
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
      
      cat("DEBUG: File type detected:", values$file_info$type, "\n")
      
      # Try to read the file
      tryCatch({
        if(values$file_info$type == "excel") {
          # Read Excel file
          cat("DEBUG: Reading Excel file\n")
          data <- readExcelFile(file_path)
          showNotification("Excel fil indlæst med danske indstillinger", type = "message", duration = 3)
        } else {
          # Read CSV file with DANISH default settings
          cat("DEBUG: Reading CSV file with Danish settings\n")
          data <- readCSVFile(file_path, 
                              sep = ";",
                              decimal = ",",
                              encoding = "ISO-8859-1",
                              header = TRUE)
          showNotification("CSV fil indlæst med danske indstillinger (;-separeret, ,-decimal)", type = "message", duration = 3)
        }
        
        cat("DEBUG: Data read successfully. Rows:", nrow(data), "Cols:", ncol(data), "\n")
        cat("DEBUG: Column names:", paste(names(data), collapse = ", "), "\n")
        
        values$raw_data <- data
        processData()
        
      }, error = function(e) {
        cat("DEBUG: ERROR during file reading:", e$message, "\n")
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
      
      cat("DEBUG: processData() called\n")
      
      data <- values$raw_data
      cat("DEBUG: Raw data has", nrow(data), "rows and", ncol(data), "columns\n")
      
      # Robust removal of empty rows
      is_empty <- apply(as.data.frame(lapply(data, function(col) {
        if (is.character(col)) trimws(col) == "" else is.na(col)
      })), 1, all)
      data <- data[!is_empty, , drop = FALSE]
      cat("DEBUG: After removing empty rows:", nrow(data), "rows remain\n")
      
      # Store processed data
      values$processed_data <- data
      cat("DEBUG: Processed data stored\n")
      
      # Validate data structure
      validation <- validateDataStructure(data)
      cat("DEBUG: Validation completed. Valid:", validation$valid, "\n")
      values$validation_results <- validation
      
      # Update status based on validation
      if(validation$valid) {
        updateStatusIndicator("ready", paste("Data indlæst:", nrow(data), "rækker,", ncol(data), "kolonner"))
        values$upload_success <- TRUE
        cat("DEBUG: Upload success set to TRUE\n")
      } else {
        updateStatusIndicator("warning", "Data indlæst med advarsler")
        values$upload_success <- FALSE
        cat("DEBUG: Upload success set to FALSE due to validation issues\n")
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
    
    # Render data preview table
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
          dom = 't',
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
        data = reactive({ 
          cat("DEBUG: data() reactive called. processed_data is null:", is.null(values$processed_data), "\n")
          values$processed_data 
        }),
        file_info = reactive(values$file_info),
        validation = reactive(values$validation_results),
        upload_success = reactive({
          cat("DEBUG: upload_success() reactive called. Value:", values$upload_success, "\n") 
          values$upload_success
        })
      )
    )
  })
}

# Editable Table Module UI
editableTableUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Data controls
    fluidRow(
      column(
        8,
        uiOutput(ns("table_status"))
      ),
      column(
        4,
        div(
          style = "text-align: right;",
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
    
    uiOutput(ns("edit_validation_messages")),
    
    # Editable table
    conditionalPanel(
      condition = paste0("output['", ns("has_data"), "']"),
      
      div(
        style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px; background-color: white;",
        
        div(
          style = "margin-bottom: 10px; padding: 8px; background-color: #f8f9fa; border-radius: 3px; font-size: 0.85rem; color: #666;",
          icon("info-circle"),
          strong(" Redigeringstips: "), 
          "Dobbeltklik for at redigere • Tab/Enter for næste celle • Ctrl+Z for fortryd • Højreklik for kontekstmenu"
        ),
        
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
    )
  )
}

# Editable Table Module Server
editableTableServer <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      current_data = NULL,
      original_data = NULL,
      history = list(),
      history_position = 0
    )
    
    # Initialize data when available
    observe({
      cat("DEBUG: editableTableServer - observe triggered\n")
      
      incoming_data <- data_reactive()
      
      if (!is.null(incoming_data)) {
        cat("DEBUG: editableTableServer - received data with", nrow(incoming_data), "rows\n")
        
        if(is.null(values$original_data)) {
          cat("DEBUG: editableTableServer - initializing original_data\n")
          values$original_data <- incoming_data
          values$current_data <- incoming_data
          
          # Initialize history
          values$history <- list(incoming_data)
          values$history_position <- 1
          cat("DEBUG: editableTableServer - initialization complete\n")
        }
      } else {
        cat("DEBUG: editableTableServer - received NULL data\n")
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
      
      # Apply column-specific validation
      for(i in 1:ncol(data)) {
        col_data <- data[[i]]
        
        if(is.numeric(col_data) || 
           sum(!is.na(suppressWarnings(as.numeric(gsub(",", ".", as.character(col_data)))))) > length(col_data) * 0.8) {
          
          hot <- hot %>%
            rhandsontable::hot_col(
              col = i,
              type = "numeric",
              format = "0,0.00"
            )
        } else {
          hot <- hot %>%
            rhandsontable::hot_col(
              col = i,
              type = "text"
            )
        }
      }
      
      return(hot)
    })
    
    # Handle table changes with corrected history handling
    observeEvent(input$editable_table, {
      req(input$editable_table)
      
      new_data <- rhandsontable::hot_to_r(input$editable_table)
      values$current_data <- new_data
      
      # Add to history with corrected parentheses
      values$history_position <- values$history_position + 1
      values$history <- c(values$history[1:(values$history_position - 1)], list(new_data))
      
      if(length(values$history) > 50) {
        values$history <- values$history[-1]
        values$history_position <- values$history_position - 1
      }
    })
    
    # Implement Undo functionality
    observeEvent(input$undo, {
      if (values$history_position > 1) {
        values$history_position <- values$history_position - 1
        values$current_data <- values$history[[values$history_position]]
      }
    })
    
    # Implement Redo functionality
    observeEvent(input$redo, {
      if (values$history_position < length(values$history)) {
        values$history_position <- values$history_position + 1
        values$current_data <- values$history[[values$history_position]]
      }
    })
    
    # Add row
    observeEvent(input$add_row, {
      req(values$current_data)
      
      new_row <- values$current_data[1, ]
      new_row[1, ] <- NA
      
      values$current_data <- rbind(values$current_data, new_row)
      
      showNotification("Ny række tilføjet", type = "message", duration = 2)
    })
    
    # Delete row
    observeEvent(input$delete_row, {
      req(values$current_data, nrow(values$current_data) > 1)
      
      values$current_data <- values$current_data[-nrow(values$current_data), ]
      
      showNotification("Række slettet", type = "message", duration = 2)
    })
    
    # Reset to original
    observeEvent(input$reset, {
      req(values$original_data)
      
      values$current_data <- values$original_data
      values$history <- list(values$original_data)
      values$history_position <- 1
      
      showNotification("Data nulstillet til original", type = "message", duration = 3)
    })
    
    # Validation messages for edited data
    output$edit_validation_messages <- renderUI({
      req(values$current_data)
      
      validation <- validateDataStructure(values$current_data)
      
      if(length(validation$warnings) > 0 || length(validation$errors) > 0) {
        
        messages <- tagList()
        
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
      
      return(NULL)
    })
    
    # Return current data
    return(
      reactive(values$current_data)
    )
  })
}