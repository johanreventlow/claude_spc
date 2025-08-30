# server.R
# Main server definition

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
  setup_visualization(input, output, session, values)
  
  # Download handlers
  setup_download_handlers(input, output, session, values)
  
  # Helper observers
  setup_helper_observers(input, output, session, values)
  
  # Welcome message
  observe({
    showNotification(
      paste("Velkommen til", HOSPITAL_NAME, "SPC App! Indtast data i tabellen eller upload en fil."),
      type = "message",
      duration = 5
    )
  }) %>% 
    bindEvent(TRUE, once = TRUE)
}
