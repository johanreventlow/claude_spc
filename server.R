# server.R
# Main server definition

library(shiny)
library(openxlsx)

source("R/server/server_reactive_values.R")
source("R/server/server_session_management.R")
source("R/server/server_file_upload.R")
source("R/server/server_data_table.R")
source("R/server/server_column_management.R")
source("R/server/server_visualization.R")
source("R/server/server_download.R")
source("R/server/server_helpers.R")

# Main server function
server <- function(input, output, session) {
  
  # Initialize reactive values
  values <- initialize_reactive_values()
  
  # TEST MODE: Auto-load example data if enabled
  if (TEST_MODE_AUTO_LOAD) {
    test_file_path <- "R/data/spc_exampledata.csv"
    
    if (file.exists(test_file_path)) {
      tryCatch({
        # Load test data with proper encoding
        test_data <- tryCatch({
          read.csv(test_file_path, stringsAsFactors = FALSE, sep = ";", dec = ",", encoding = "UTF-8")
        }, error = function(e1) {
          read.csv(test_file_path, stringsAsFactors = FALSE, sep = ";", dec = ",", encoding = "latin1")
        })
        
        # Ensure standard columns are present
        test_data <- ensure_standard_columns(test_data)
        
        # Set reactive values
        values$current_data <- test_data
        values$original_data <- test_data
        values$file_uploaded <- TRUE
        values$auto_detect_done <- FALSE  # Will trigger auto-detect
        values$hide_anhoej_rules <- FALSE  # Show Anhøj rules for real data
        
      }, error = function(e) {
        cat("ERROR: Failed to load test data:", e$message, "\n")
      })
    }
  }
  
  # Initialize file upload waiter
  waiter_file <- waiter::Waiter$new(
    html = WAITER_CONFIG$file_upload$html,
    color = WAITER_CONFIG$file_upload$color
  )
  
  # Session management logic
  setup_session_management(input, output, session, values, waiter_file)
  
  # File upload logic
  setup_file_upload(input, output, session, values, waiter_file)
  
  # Data table logic  
  setup_data_table(input, output, session, values)
  
  # Column management logic
  setup_column_management(input, output, session, values)
  
  # Visualization logic
  visualization <- setup_visualization(input, output, session, values)
  
  # Download handlers
  setup_download_handlers(input, output, session, values)
  
  # Helper observers
  setup_helper_observers(input, output, session, values)
  
  # Welcome message
  # observe({
  #   showNotification(
  #     paste("Velkommen til", HOSPITAL_NAME, "SPC App! Indtast data i tabellen eller upload en fil."),
  #     type = "message",
  #     duration = 5
  #   )
  # }) %>% 
  #   bindEvent(TRUE, once = TRUE)
}
