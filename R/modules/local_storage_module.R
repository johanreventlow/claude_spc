# R/modules/local_storage_module.R
# Main local storage module - loads all components

library(shiny)
library(shinyjs)

# Source all local storage module components
source("R/modules/local_storage_js.R")
source("R/modules/local_storage_functions.R")

# Re-export main functions for backwards compatibility
# The actual functions are defined in the component files above
