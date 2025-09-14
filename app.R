# app.R
# Hovedapplikationsfil der indlæser alle komponenter

# KONFIGURATION ===============================================================
# Indlæs globale indstillinger og hjælpefunktioner
source("global.R")

# MODULER =====================================================================
# Indlæs data og local storage moduler (visualization indlæses i server.R)
source("R/modules/data_module.R")
source("R/modules/local_storage_functions.R")

# UI/SERVER KOMPONENTER =======================================================
# Indlæs UI og server komponenter
source("ui.R")
source("server.R")

# SHINY APPLIKATION ===========================================================
# Opret og start Shiny applikationen
shinyApp(ui = ui, server = server)
