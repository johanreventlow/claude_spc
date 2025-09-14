# app.R
# Hovedapplikationsfil der indlæser alle komponenter

# KONFIGURATION ===============================================================
# Indlæs globale indstillinger og hjælpefunktioner
source("global.R")

# MODULER =====================================================================
# Indlæs server-moduler (UI indlæses i ui.R)
source("R/modules/data_module.R")
source("R/modules/visualization_module_server.R")
source("R/modules/visualization_helpers.R")
source("R/modules/visualization_spc.R")
source("R/modules/local_storage_functions.R")

# UI/SERVER KOMPONENTER =======================================================
# Indlæs UI og server komponenter
source("ui.R")
source("server.R")

# SHINY APPLIKATION ===========================================================
# Opret og start Shiny applikationen
shinyApp(ui = ui, server = server)
