# server_file_upload.R
# Server logik til håndtering af fil uploads og import

# Dependencies ----------------------------------------------------------------
# Bruger readxl og readr til fil-import

# UPLOAD HÅNDTERING ===========================================================

## Setup fil upload funktionalitet
setup_file_upload <- function(input, output, session, values, waiter_file) {
  
  # File upload handler
  observeEvent(input$data_file, {
    req(input$data_file)
    
    # Show loading
    waiter_file$show()
    
    # Ensure loading is hidden no matter what happens
    on.exit({ waiter_file$hide() })
    
    # Close upload modal automatically
    on.exit({ removeModal() }, add = TRUE)
    
    values$updating_table <- TRUE
    on.exit({ values$updating_table <- FALSE }, add = TRUE)
    
    file_path <- input$data_file$datapath
    file_ext <- tools::file_ext(input$data_file$name)
    
    tryCatch({
      if (file_ext %in% c("xlsx", "xls")) {
        handle_excel_upload(file_path, session, values)
      } else {
        handle_csv_upload(file_path, values)
      }
      
    }, error = function(e) {
      showNotification(
        paste("Fejl ved upload:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
}

## Håndter Excel fil upload
handle_excel_upload <- function(file_path, session, values) {
  excel_sheets <- readxl::excel_sheets(file_path)
  
  if ("Data" %in% excel_sheets && "Metadata" %in% excel_sheets) {
    cat("DEBUG: Detected complete export file - importing with session info\n")
    
    # Read data from Data sheet
    data <- readxl::read_excel(file_path, sheet = "Data", col_names = TRUE)
    
    # Ensure standard columns are present and in correct order
    data <- ensure_standard_columns(data)
    
    # Read session info from Metadata sheet
    session_info_raw <- readxl::read_excel(
      file_path, 
      sheet = "Metadata", 
      col_names = FALSE
    )
    
    # Parse session info for configuration
    session_lines <- as.character(session_info_raw[[1]])
    
    # Extract metadata from session info
    metadata <- parse_session_metadata(session_lines, names(data))
    
    # Load data
    values$current_data <- as.data.frame(data)
    values$original_data <- as.data.frame(data)
    values$file_uploaded <- TRUE
    values$auto_detect_done <- TRUE  # Skip auto-detect since we have session info
    values$hide_anhoej_rules <- FALSE  # Re-enable Anhøj rules when real data is uploaded
    
    # Restore metadata with delay to ensure UI is ready
    invalidateLater(500)
    isolate({
      restore_metadata(session, metadata)
    })
    
    showNotification(
      paste("Komplet session importeret:", nrow(data), "rækker,", ncol(data), "kolonner + konfiguration"),
      type = "message",
      duration = 4
    )
    
  } else {
    # Standard Excel file
    data <- readxl::read_excel(file_path, col_names = TRUE)
    
    # Ensure standard columns are present and in correct order
    data <- ensure_standard_columns(data)
    
    values$current_data <- as.data.frame(data)
    values$original_data <- as.data.frame(data)
    values$file_uploaded <- TRUE
    values$auto_detect_done <- FALSE
    values$hide_anhoej_rules <- FALSE  # Re-enable Anhøj rules when real data is uploaded
    
    showNotification(
      paste("Excel fil uploadet:", nrow(data), "rækker,", ncol(data), "kolonner"),
      type = "message",
      duration = 3
    )
  }
}

## Håndter CSV fil upload
handle_csv_upload <- function(file_path, values) {
  # CSV behandling med danske standarder
  data <- readr::read_csv2(
    file_path,
    locale = readr::locale(
      decimal_mark = ",",
      grouping_mark = ".",
      encoding = "ISO-8859-1"
    ),
    show_col_types = FALSE
  )
  
  # Ensure standard columns are present and in correct order
  data <- ensure_standard_columns(data)
  
  values$current_data <- as.data.frame(data)
  values$original_data <- as.data.frame(data)
  values$file_uploaded <- TRUE
  values$auto_detect_done <- FALSE
  values$hide_anhoej_rules <- FALSE  # Re-enable Anhøj rules when real data is uploaded
  
  showNotification(
    paste("CSV fil uploadet:", nrow(data), "rækker,", ncol(data), "kolonner"),
    type = "message",
    duration = 3
  )
}

## Parse session metadata fra importerede filer
parse_session_metadata <- function(session_lines, data_cols) {
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
      # Check if it's a standard unit
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
    if (x_text != "Ikke valgt" && x_text %in% data_cols) {
      metadata$x_column <- x_text
    }
  }
  
  y_line <- session_lines[grepl("^• Y-akse:", session_lines)]
  if (length(y_line) > 0) {
    y_text <- gsub("^• Y-akse: (.+) \\(.*\\)$", "\\1", y_line[1])
    if (y_text != "Ikke valgt" && y_text %in% data_cols) {
      metadata$y_column <- y_text
    }
  }
  
  n_line <- session_lines[grepl("^• Nævner:", session_lines)]
  if (length(n_line) > 0) {
    n_text <- gsub("^• Nævner: ", "", n_line[1])
    if (n_text %in% data_cols) {
      metadata$n_column <- n_text
    }
  }
  
  return(metadata)
}
