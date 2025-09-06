# R/server/server_welcome_page.R
# Server logic for welcome page interactions

setup_welcome_page_handlers <- function(input, output, session, values, waiter_file) {
  
  # Handle "Start ny analyse" button from welcome page
  observeEvent(input$start_new_session, {
    cat("Welcome page: Start new session clicked\n")
    
    # Same logic as existing start_new_session
    values$current_data <- create_empty_session_data()
    values$original_data <- values$current_data
    values$file_uploaded <- TRUE
    values$hide_anhoej_rules <- TRUE
    values$session_file_name <- NULL
    
    # Reset configurations
    values$auto_detect_done <- FALSE
    updateSelectInput(session, "x_column", selected = "")
    updateSelectInput(session, "y_column", selected = "")
    updateSelectInput(session, "n_column", selected = "")
    
    cat("Welcome page: New empty session created\n")
  })
  
  # Handle "Upload data" button from welcome page  
  observeEvent(input$upload_data_welcome, {
    cat("Welcome page: Upload data clicked\n")
    # Focus on file input or open file dialog
    shinyjs::click("file_upload")
  })
  
  # Handle "Quick start demo" button
  observeEvent(input$quick_start_demo, {
    cat("Welcome page: Quick start demo clicked\n")
    
    # Load example data
    test_file_path <- "R/data/spc_exampledata.csv"
    
    if (file.exists(test_file_path)) {
      tryCatch({
        cat("Welcome page: Starting demo data load...\n")
        
        # Show loading waiter
        waiter_file$show()
        
        # Load demo data using readr::read_csv2 (same as working file upload)
        cat("Loading demo data with readr::read_csv2...\n")
        demo_data <- readr::read_csv2(
          test_file_path,
          locale = readr::locale(
            decimal_mark = ",",
            grouping_mark = ".",
            encoding = "ISO-8859-1"
          ),
          show_col_types = FALSE
        )
        
        cat("Successfully loaded with read_csv2\n")
        cat("Column names:", paste(names(demo_data), collapse = ", "), "\n")
        cat("Rows loaded:", nrow(demo_data), "\n")
        
        if (is.null(demo_data) || nrow(demo_data) == 0) {
          stop("No data loaded from file")
        }
        
        cat("Demo data column names before ensure_standard_columns:", paste(names(demo_data), collapse = ", "), "\n")
        
        # Ensure standard columns are present
        demo_data <- ensure_standard_columns(demo_data)
        
        cat("Demo data column names after ensure_standard_columns:", paste(names(demo_data), collapse = ", "), "\n")
        cat("Final data dimensions:", paste(dim(demo_data), collapse = "x"), "\n")
        
        # Set reactive values
        values$current_data <- demo_data
        values$original_data <- demo_data
        values$file_uploaded <- TRUE
        values$auto_detect_done <- FALSE  # Will trigger auto-detect
        values$hide_anhoej_rules <- FALSE  # Show Anhøj rules for real data
        values$session_file_name <- "Eksempel data (SPC demo)"
        
        # Hide waiter
        later::later(function() {
          waiter_file$hide()
        }, 0.5)
        
        cat("Welcome page: Demo data loaded successfully\n")
        
        # Show success notification
        showNotification(
          "Eksempel data indlæst! Du kan nu se SPC analysen.",
          type = "message",
          duration = 3
        )
        
      }, error = function(e) {
        cat("ERROR: Failed to load demo data:", e$message, "\n")
        cat("ERROR: Stacktrace:", "\n")
        traceback()
        
        waiter_file$hide()
        
        # Show error message to user
        showNotification(
          paste("Kunne ikke indlæse eksempel data:", e$message),
          type = "error",
          duration = 8
        )
      })
    } else {
      cat("WARNING: Demo data file not found at:", test_file_path, "\n")
      showNotification(
        "Eksempel data ikke tilgængelig. Prøv at uploade dine egne data.",
        type = "warning",
        duration = 5
      )
    }
  })
}