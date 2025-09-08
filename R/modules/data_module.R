# data_module.R
# Hovedfil for data modul - koordinerer alle databehandlingskomponenter

# Dependencies ----------------------------------------------------------------
library(shiny)
library(dplyr)
library(shinyjs)

# MODULKOMPONENTER ============================================================

## Kilde alle data modul komponenter
source("R/modules/data_file_readers.R")
source("R/modules/data_validation.R")

# BAGUDKOMPATIBILITET =========================================================

## Re-eksporter hovedfunktioner for bagudkompatibilitet
# De faktiske funktioner er defineret i komponentfilerne ovenfor
