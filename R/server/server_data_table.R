# R/server/server_data_table.R
# Data table rendering and interaction logic

setup_data_table <- function(input, output, session, values) {
  
  # Main table rendering
  output$main_data_table <- rhandsontable::renderRHandsontable({
    req(values$current_data)
    
    # Include table_version to force re-render after restore
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
      useTypes = FALSE  # Disable types for better editability after restore
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
    
    # Configure checkbox column if Skift exists
    if ("Skift" %in% names(data)) {
      skift_col_index <- which(names(data) == "Skift")
      hot <- hot %>% rhandsontable::hot_col(skift_col_index, type = "checkbox")
    }
    
    cat("DEBUG: Table rendered without column type definitions\n")
    
    return(hot)
  })
  
  # Handle table changes
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
      
      # Check if column names are changed
      current_names <- names(values$current_data)
      new_names <- names(new_data)
      
      if (!identical(current_names, new_names)) {
        # Validate new column names
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
        
        # Show confirmation of column name changes
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
  
  # Add row
  observeEvent(input$add_row, {
    req(values$current_data)
    
    new_row <- values$current_data[1, ]
    new_row[1, ] <- NA
    
    values$current_data <- rbind(values$current_data, new_row)
    
    showNotification("Ny række tilføjet", type = "message")
  })
  
  # Reset table
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
}
