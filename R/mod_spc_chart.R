# visualization_module.R
# Hovedfil for visualization modul - koordinerer alle visualiseringskomponenter

# Dependencies ----------------------------------------------------------------
# Ingen direkte dependencies - komponenterne indlæser deres egne

# MODULKOMPONENTER ============================================================

## Kilde alle visualization modul komponenter
source("R/modules/visualization_module_ui.R")
source("R/modules/visualization_module_server.R")
source("R/fct_chart_helpers.R")
source("R/modules/visualization_spc.R")
# source("R/modules/visualization_anhoej.R")  # Ikke længere nødvendig - bruger qic indbygget Anhøj analyse

# BAGUDKOMPATIBILITET =========================================================

## Re-eksporter hovedfunktioner for bagudkompatibilitet
# De faktiske funktioner er defineret i komponentfilerne ovenfor
