# R/modules/data_module.R
# Main data module - loads all components

library(shiny)
library(dplyr)
library(shinyjs)

# Source all data module components
source("R/modules/data_file_readers.R")
source("R/modules/data_validation.R") 
source("R/modules/data_editable_table_ui.R")
source("R/modules/data_editable_table_server.R")

# Re-export main functions for backwards compatibility
# The actual functions are defined in the component files above
