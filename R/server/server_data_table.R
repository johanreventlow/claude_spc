# R/server/server_data_table.R
# Data table rendering and interaction logic

setup_data_table <- function(input, output, session, values) {
  
  # Main table rendering using excelR
    output$main_data_table <- excelR::renderExcel({
      req(values$current_data)
      
      # Include table_version to force re-render after restore
      version_trigger <- values$table_version
      cat("DEBUG: Rendering excelR table with version", version_trigger, "\n")
      
      data <- values$current_data
      
      # Keep logical column as logical for excelR checkbox
      # excelR handles logical values directly for checkbox type
      
      excelR::excelTable(
        data = data,
        columns = data.frame(
          title = names(data),
          type = ifelse(names(data) == "Skift", "checkbox", "text"),
          width = case_when(
            names(data) == "Skift" ~ 60,
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
    
    # Handle excelR table changes
    observeEvent(input$main_data_table, {
      cat("DEBUG: excelR table change event triggered\n")
      
      if (values$updating_table || values$restoring_session) {
        cat("DEBUG: Skipping excelR change - table updating or restoring\n")
        return()
      }
      
      values$updating_table <- TRUE
      values$table_operation_in_progress <- TRUE
      
      on.exit({ 
        values$updating_table <- FALSE 
        cat("DEBUG: excelR change processing complete\n")
      }, add = TRUE)
      
      # Clear persistent flag after delay
      later::later(function() {
        cat("DEBUG: Clearing table_operation_in_progress flag\n")
        values$table_operation_in_progress <- FALSE
      }, delay = 2)
      
      tryCatch({
        new_data <- input$main_data_table
        
        if (is.null(new_data) || length(new_data) == 0) {
          cat("DEBUG: No data received from excelR\n")
          return()
        }
        
        # Debug excelR data structure
        cat("DEBUG: excelR data class:", class(new_data), "\n")
        cat("DEBUG: excelR data structure:", str(new_data), "\n")
        
        # excelR sends data in new_data$data as list of rows
        if (!is.null(new_data$data) && length(new_data$data) > 0) {
          # Get column names from colHeaders
          col_names <- unlist(new_data$colHeaders)
          
          # Convert list of rows to data frame
          row_list <- new_data$data
          
          # Create empty data frame with correct structure
          new_df <- data.frame(matrix(NA, nrow = length(row_list), ncol = length(col_names)))
          names(new_df) <- col_names
          
          # Fill data frame row by row
          for (i in seq_along(row_list)) {
            row_data <- row_list[[i]]
            for (j in seq_along(row_data)) {
              if (j <= length(col_names)) {
                new_df[i, j] <- row_data[[j]]
              }
            }
          }
          
          # Convert data types properly
          # Skift column (logical) - excelR sends checkbox as logical already
          if ("Skift" %in% names(new_df)) {
            # Handle both logical and string representations
            skift_values <- new_df$Skift
            if (is.character(skift_values)) {
              new_df$Skift <- skift_values == "TRUE" | skift_values == "true" | skift_values == TRUE
            } else {
              new_df$Skift <- as.logical(skift_values)
            }
          }
          
          # Numeric columns
          numeric_cols <- c("Tæller", "Nævner")
          for (col in numeric_cols) {
            if (col %in% names(new_df)) {
              new_df[[col]] <- as.numeric(new_df[[col]])
            }
          }
          
          # Date column
          if ("Dato" %in% names(new_df)) {
            new_df$Dato <- as.character(new_df$Dato)
          }
          
          # Character columns
          if ("Kommentar" %in% names(new_df)) {
            new_df$Kommentar <- as.character(new_df$Kommentar)
          }
          
        } else {
          cat("DEBUG: No data found in excelR structure\n")
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
  
  # Add row
  observeEvent(input$add_row, {
    req(values$current_data)
    
    # Set persistent flag to prevent auto-save interference
    values$table_operation_in_progress <- TRUE
    
    new_row <- values$current_data[1, ]
    new_row[1, ] <- NA
    
    values$current_data <- rbind(values$current_data, new_row)
    
    showNotification("Ny række tilføjet", type = "message")
    
    # Clear persistent flag after delay
    later::later(function() {
      cat("DEBUG: Clearing table_operation_in_progress flag (add_row)\n")
      values$table_operation_in_progress <- FALSE
    }, delay = 1)
  })
  
  # Reset table
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
    
    # Clear persistent flag after delay
    later::later(function() {
      cat("DEBUG: Clearing table_operation_in_progress flag (reset_table)\n")
      values$table_operation_in_progress <- FALSE
    }, delay = 1)
    
    showNotification(
      "Tabel og fil-upload tømt - indtast nye data eller upload ny fil. Titel og beskrivelse bevaret.", 
      type = "message", 
      duration = 4
    )
  })
}
