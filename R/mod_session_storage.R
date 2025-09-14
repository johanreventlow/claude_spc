# local_storage_module.R
# Hovedfil for local storage modul - koordinerer alle komponenter

# Dependencies ----------------------------------------------------------------
library(shiny)
library(shinyjs)

# MODULKOMPONENTER ============================================================

## Kilde alle local storage modul komponenter
source("R/utils_local_storage_js.R")
source("R/utils_local_storage.R")

# BAGUDKOMPATIBILITET =========================================================

## Re-eksporter hovedfunktioner for bagudkompatibilitet
# De faktiske funktioner er defineret i komponentfilerne ovenfor
