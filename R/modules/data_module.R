# R/modules/data_module.R
library(shiny)
library(DT)
library(readr)
library(readxl)
library(dplyr)
library(shinyjs)  # Add shinyjs for html updates

# Add this helper function at the top of the module file
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
        # Status indicator (make it a reactive output)
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
    
    # Data preview section
    conditionalPanel(
      condition = paste0("output['", ns("show_preview"), "']"),
      
      br(),
      
      card(
        full_screen = TRUE,
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
          
          # Data table
          DT::dataTableOutput(ns("preview_table"))
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
      
      # Update data info - no longer needed as it's now a reactive output
      # updateDataInfo()
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
    
    # Helper function to update status indicator (simplified)
    updateStatusIndicator <- function(status, message) {
      values$status_type <- status
      values$status_message <- message
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
      
      # Limit preview to first 100 rows for performance
      preview_data <- if(nrow(data) > 100) {
        data[1:100, ]
      } else {
        data
      }
      
      DT::datatable(
        preview_data,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          scrollY = "400px",
          searching = TRUE,
          ordering = TRUE,
          info = TRUE,
          dom = 'frtip',  # Remove length menu
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover'
      ) %>%
        DT::formatStyle(
          columns = names(preview_data),
          backgroundColor = HOSPITAL_COLORS$light,
          borderColor = '#ddd'
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

# -----------------------------------------------------------------------------
# HELPER FUNCTIONS FOR FILE READING AND VALIDATION
# -----------------------------------------------------------------------------

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