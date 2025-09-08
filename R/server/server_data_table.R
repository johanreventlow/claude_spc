# server_data_table.R  
# Server logik for data tabel rendering og interaktion

# Dependencies ----------------------------------------------------------------

# DATATABEL SETUP =============================================================

## Hovedfunktion for datatabel
# Opsætter al server logik relateret til data-tabel håndtering
setup_data_table <- function(input, output, session, values) {
  
  # Hovedtabel rendering med excelR
    output$main_data_table <- excelR::renderExcel({
      req(values$current_data)
      
      # Inkluder table_version for at tvinge re-render efter gendannelse
      version_trigger <- values$table_version
      
      data <- values$current_data
      
      # Behold logiske kolonner som logiske for excelR checkbox
      # excelR håndterer logiske værdier direkte for checkbox type
      
      excelR::excelTable(
        data = data,
        columns = data.frame(
          title = names(data),
          type = case_when(
            names(data) == "Skift" ~ "checkbox",
            names(data) == "Frys" ~ "radio",
            TRUE ~ "text"
          ),
          width = case_when(
            names(data) == "Skift" ~ 60,
            names(data) == "Frys" ~ 60,
            names(data) == "Dato" ~ 100,
            names(data) %in% c("Tæller", "Nævner") ~ 80,
            names(data) == "Kommentar" ~ 300,
            TRUE ~ 120
          ),
          stringsAsFactors = FALSE
        ),
        allowInsertRow = FALSE,
        allowInsertColumn = FALSE,
        allowDeleteRow = FALSE,
        allowDeleteColumn = FALSE,
        allowRenameColumn = FALSE,
        columnSorting = FALSE,
        rowDrag = FALSE,
        columnDrag = FALSE,
        autoFill = TRUE
      )
    })
    
    # Håndtér excelR tabel ændringer
    observeEvent(input$main_data_table, {
      
      if (values$updating_table || values$restoring_session) {
        return()
      }
      
      values$updating_table <- TRUE
      values$table_operation_in_progress <- TRUE
      
      on.exit({ 
        values$updating_table <- FALSE 
      }, add = TRUE)
      
      # Clear persistent flag after delay
      later::later(function() {
        values$table_operation_in_progress <- FALSE
      }, delay = 2)
      
      tryCatch({
        new_data <- input$main_data_table
        
        if (is.null(new_data) || length(new_data) == 0) {
          return()
        }
        
        
        # excelR sender data i new_data$data som liste af rækker
        if (!is.null(new_data$data) && length(new_data$data) > 0) {
          # Hent kolonnenavne fra colHeaders
          col_names <- unlist(new_data$colHeaders)
          
          # Konvertér liste af rækker til data frame
          row_list <- new_data$data
          
          # Opret tom data frame med korrekt struktur
          new_df <- data.frame(matrix(NA, nrow = length(row_list), ncol = length(col_names)))
          names(new_df) <- col_names
          
          # Fyld data frame række for række
          for (i in seq_along(row_list)) {
            row_data <- row_list[[i]]
            for (j in seq_along(row_data)) {
              if (j <= length(col_names)) {
                new_df[i, j] <- row_data[[j]]
              }
            }
          }
          
          # Konvertér datatyper korrekt
          # Skift kolonne (logisk) - excelR sender checkbox som logisk allerede
          if ("Skift" %in% names(new_df)) {
            # Håndtér både logiske og streng repræsentationer
            skift_values <- new_df$Skift
            if (is.character(skift_values)) {
              new_df$Skift <- skift_values == "TRUE" | skift_values == "true" | skift_values == TRUE
            } else {
              new_df$Skift <- as.logical(skift_values)
            }
          }
          
          # Frys kolonne (logisk) - excelR sender radio som logisk
          if ("Frys" %in% names(new_df)) {
            # Håndtér både logiske og streng repræsentationer
            frys_values <- new_df$Frys
            if (is.character(frys_values)) {
              new_df$Frys <- frys_values == "TRUE" | frys_values == "true" | frys_values == TRUE
            } else {
              new_df$Frys <- as.logical(frys_values)
            }
          }
          
          # Numeriske kolonner
          numeric_cols <- c("Tæller", "Nævner")
          for (col in numeric_cols) {
            if (col %in% names(new_df)) {
              new_df[[col]] <- as.numeric(new_df[[col]])
            }
          }
          
          # Dato kolonne
          if ("Dato" %in% names(new_df)) {
            new_df$Dato <- as.character(new_df$Dato)
          }
          
          # Karakter kolonner
          if ("Kommentar" %in% names(new_df)) {
            new_df$Kommentar <- as.character(new_df$Kommentar)
          }
          
        } else {
          return()
        }
        
        values$current_data <- new_df
        
        showNotification("Tabel opdateret", type = "message", duration = 2)
        
      }, error = function(e) {
        cat("ERROR in excelR table change:\n", e$message, "\n")
        showNotification(
          paste("Fejl ved tabel-opdatering:", e$message),
          type = "error",
          duration = 3
        )
      })
    }, ignoreInit = TRUE)
  
  # Tilføj række
  observeEvent(input$add_row, {
    req(values$current_data)
    
    # Sæt vedvarende flag for at forhindre auto-save interferens
    values$table_operation_in_progress <- TRUE
    
    new_row <- values$current_data[1, ]
    new_row[1, ] <- NA
    
    values$current_data <- rbind(values$current_data, new_row)
    
    showNotification("Ny række tilføjet", type = "message")
    
    # Ryd vedvarende flag efter forsinkelse
    later::later(function() {
      values$table_operation_in_progress <- FALSE
    }, delay = 1)
  })
  
  # Nulstil tabel
  observeEvent(input$reset_table, {
    values$updating_table <- TRUE
    values$table_operation_in_progress <- TRUE
    
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
    
    # Ryd vedvarende flag efter forsinkelse
    later::later(function() {
      values$table_operation_in_progress <- FALSE
    }, delay = 1)
    
    showNotification(
      "Tabel og fil-upload tømt - indtast nye data eller upload ny fil. Titel og beskrivelse bevaret.", 
      type = "message", 
      duration = 4
    )
  })
}
