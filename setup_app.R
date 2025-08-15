# =============================================================================
# SPC APP - PHASE 1.1: PROJECT SETUP
# =============================================================================

# -----------------------------------------------------------------------------
# 1. PACKAGE SETUP - Installer kun hvis ikke allerede installeret
# -----------------------------------------------------------------------------

# Note: renv kan tilføjes senere med:
# renv::init() # Kun hvis du vil have package management senere

# -----------------------------------------------------------------------------
# 2. GLOBAL.R - Globale variabler og hospital branding
# -----------------------------------------------------------------------------

# global.R


# -----------------------------------------------------------------------------
# 3. APP.R - Main application entry point
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# MAPPESTRUKTUR at oprette manuelt:
# -----------------------------------------------------------------------------

# spc_app/
# ├── app.R                    # Main app file (ovenstående kode)
# ├── global.R                 # Global variables (ovenstående kode)  
# ├── .gitignore              # (ovenstående indhold)
# ├── spc_app.Rproj           # RStudio projekt fil
# ├── R/
# │   ├── modules/            # Shiny modules (næste fase)
# │   ├── utils/              # Helper functions
# │   └── data/               # Test data
# ├── www/                    # Static assets
# │   ├── hospital_logo.png   # Hospital logo (skal tilføjes)
# │   └── custom.css          # Custom CSS (senere)
# ├── reports/                # R Markdown templates (senere)
# │   ├── pdf_template.Rmd
# │   └── png_template.Rmd
# ├── tests/                  # Unit tests (senere)
# │   └── testthat/
# └── docs/                   # Documentation
#     └── README.md

print("🎯 PHASE 1.1 SETUP COMPLETE!")
print("Næste skridt:")
print("1. Opret mappestrukturen som vist ovenfor")
print("2. Tilføj hospital logo til www/hospital_logo.png") 
print("3. Installer packages med install.packages() hvis nødvendigt")
print("4. Test at appen starter med shiny::runApp()")
print("5. Gå videre til Phase 1.2 - Basic UI Framework")
print("")
print("💡 Tip: renv kan tilføjes senere med renv::init() hvis ønsket")