# R/modules/visualization_module.R
# Main visualization module - loads all components

# Source all visualization module components
source("R/modules/visualization_module_ui.R")
source("R/modules/visualization_module_server.R")
source("R/modules/visualization_helpers.R")
source("R/modules/visualization_spc.R")
# source("R/modules/visualization_anhoej.R")  # No longer needed - using qic built-in Anhøj analysis

# Re-export main functions for backwards compatibility
# The actual functions are defined in the component files above
