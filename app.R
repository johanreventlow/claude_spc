# app.R
# Main application file that loads all components

# Load global configuration and utilities
source("global.R")

# Load all modules
source("R/modules/data_module.R")
source("R/modules/visualization_module.R")
source("R/modules/local_storage_module.R")

# Load UI components
source("ui.R")

# Load server components  
source("server.R")

# Create the Shiny application
shinyApp(ui = ui, server = server)
