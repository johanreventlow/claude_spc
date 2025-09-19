# data_module.R
# Hovedfil for data modul - koordinerer alle databehandlingskomponenter

# Dependencies ----------------------------------------------------------------
library(shiny)
library(dplyr)
library(shinyjs)

# MODULKOMPONENTER ============================================================

## Kilde alle data modul komponenter
# File I/O and validation functions now loaded globally in global.R for better performance

# BAGUDKOMPATIBILITET =========================================================

## Re-eksporter hovedfunktioner for bagudkompatibilitet
# De faktiske funktioner er defineret i komponentfilerne ovenfor
