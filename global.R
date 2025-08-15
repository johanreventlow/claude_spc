library(shiny)
library(bslib)
library(qicharts2)
library(rhandsontable)
library(DT)
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)

# -----------------------------------------------------------------------------
# HOSPITAL BRANDING CONFIGURATION
# -----------------------------------------------------------------------------

# Hospital information
HOSPITAL_NAME <- "Bispebjerg og Frederiksberg Hospital"
HOSPITAL_LOGO_PATH <- "www/RegionH_hospital.png"  # Skal placeres i www/ mappen

# Farvepalette (skal matches med hospital branding)
HOSPITAL_COLORS <- list(
  primary = "#0066CC",      # Primær blå
  secondary = "#004D99",    # Mørkere blå  
  accent = "#FF6B35",       # Orange accent
  success = "#28A745",      # Grøn for positive signals
  warning = "#FFC107",      # Gul for advarsler
  danger = "#DC3545",       # Rød for alerts
  light = "#F8F9FA",        # Lys baggrund
  dark = "#343A40"          # Mørk tekst
)

# ggplot2 tema for alle grafer
HOSPITAL_THEME <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(color = HOSPITAL_COLORS$primary, size = 14, face = "bold"),
      plot.subtitle = element_text(color = HOSPITAL_COLORS$secondary, size = 12),
      axis.title = element_text(color = HOSPITAL_COLORS$dark, size = 11),
      axis.text = element_text(color = HOSPITAL_COLORS$dark, size = 10),
      legend.title = element_text(color = HOSPITAL_COLORS$dark, size = 11),
      legend.text = element_text(color = HOSPITAL_COLORS$dark, size = 10),
      panel.grid.major = element_line(color = HOSPITAL_COLORS$light),
      panel.grid.minor = element_line(color = HOSPITAL_COLORS$light),
      strip.text = element_text(color = HOSPITAL_COLORS$primary, face = "bold")
    )
}

# Standard footer til alle grafer
create_plot_footer <- function(afdeling = "", data_kilde = "", dato = Sys.Date()) {
  paste0(
    HOSPITAL_NAME, 
    if(afdeling != "") paste0(" - ", afdeling) else "",
    " | Datakilde: ", data_kilde, 
    " | Genereret: ", format(dato, "%d-%m-%Y"),
    " | SPC analyse med Anhøj-regler"
  )
}

# Dansk oversættelse af chart typer
CHART_TYPES_DA <- list(
  "run" = "Seriediagram (Run Chart)",
  "i" = "I-kort (Individuelle værdier)", 
  "mr" = "MR-kort (Moving Range)",
  "p" = "P-kort (Andele)",
  "pp" = "P'-kort (Andele, standardiseret)",
  "u" = "U-kort (Rater)",
  "up" = "U'-kort (Rater, standardiseret)",
  "c" = "C-kort (Tællinger)",
  "g" = "G-kort (Tid mellem hændelser)"
)

# Validation functions
validate_numeric_column <- function(data, column_name) {
  if (!column_name %in% names(data)) {
    return(paste("Kolonne", column_name, "ikke fundet"))
  }
  if (!is.numeric(data[[column_name]])) {
    return(paste("Kolonne", column_name, "skal være numerisk"))
  }
  return(NULL)
}

validate_date_column <- function(data, column_name) {
  if (!column_name %in% names(data)) {
    return(paste("Kolonne", column_name, "ikke fundet"))
  }
  # Forsøg at konvertere til dato hvis det ikke allerede er det
  tryCatch({
    as.Date(data[[column_name]])
    return(NULL)
  }, error = function(e) {
    return(paste("Kolonne", column_name, "kunne ikke konverteres til dato"))
  })
}