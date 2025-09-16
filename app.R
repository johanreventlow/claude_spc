# app.R
# Modernized Golem-style entry point for SPC App

# KONFIGURATION ===============================================================
# Indlæs globale indstillinger og hjælpefunktioner
source("global.R")

# GOLEM LAUNCHER ==============================================================
# Load Golem components
source("R/run_app.R")

# Run the application using Golem pattern
run_app()

