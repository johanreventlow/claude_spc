# local_storage_module.R
# Hovedfil for local storage modul - koordinerer alle komponenter

# Dependencies ----------------------------------------------------------------
library(shiny)
library(shinyjs)

# MODULKOMPONENTER ============================================================

## Kilde alle local storage modul komponenter
source("R/modules/local_storage_js.R")
source("R/modules/local_storage_functions.R")

# BAGUDKOMPATIBILITET =========================================================

## Re-eksporter hovedfunktioner for bagudkompatibilitet
# De faktiske funktioner er defineret i komponentfilerne ovenfor
