library(shiny)
library(bslib)  # For page_navbar, card, sidebar etc.
library(qicharts2)
library(excelR)  # For Excel-like editable table implementation
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(zoo)  # For rolling means in calculated fields
library(scales)  # For percent formatting in plots
library(rlang)     # For %||% operator
library(lubridate)
library(openxlsx)  # For Excel export functionality
library(waiter)
library(yaml)  # For reading brand.yml
library(later)  # For delayed execution

# DEVELOPMENT FLAGS
# TEST MODE: Auto-load example data for qic() debugging
# Set to FALSE to disable auto-loading and return to normal user-controlled data loading
TEST_MODE_AUTO_LOAD <- TRUE

# AUTO-RESTORE: Automatically restore previous sessions
# Set to FALSE during development, TRUE for production
AUTO_RESTORE_ENABLED <- FALSE

# TABLE TYPE: Using excelR for Excel-like editable tables

# Load utility functions
source("R/utils/danish_numbers.R")

# -----------------------------------------------------------------------------
# 100% BRAND.YML BASERET KONFIGURATION
# -----------------------------------------------------------------------------

# Read brand.yml configuration
brand_config <- yaml::read_yaml("_brand.yml")

# Create theme using brand.yml (auto-discovered as _brand.yml)
my_theme <- bs_theme(brand = "_brand.yml")

# Hospital information from brand.yml
HOSPITAL_NAME <- brand_config$meta$name
HOSPITAL_LOGO_PATH <- brand_config$logo$image

# Extract ALL colors from brand.yml (via bs_theme)
HOSPITAL_COLORS <- list(
  primary = brand_config$color$palette$primary,
  secondary = brand_config$color$palette$secondary,
  accent = brand_config$color$palette$accent,  # From brand.yml palette
  success = brand_config$color$palette$success,
  warning = brand_config$color$palette$warning,
  danger = brand_config$color$palette$danger,
  info = brand_config$color$palette$info,
  light = brand_config$color$palette$light,
  dark = brand_config$color$palette$dark
)

# Waiter configuration med hospital branding
# Waiter configuration med hospital branding
WAITER_CONFIG <- list(
  # App start loading - FULLSCREEN OVERLAY
  app_start = list(
    html = tagList(
      div(
        style = "text-align: center; padding: 50px;",
        img(src = basename(HOSPITAL_LOGO_PATH), height = "80px", style = "margin-bottom: 30px;"),
        h2(paste(HOSPITAL_NAME, "SPC App"), 
           style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 300; margin-bottom: 20px;")),
        h4("Indlæser...", 
           style = paste("color:", HOSPITAL_COLORS$secondary, "; font-weight: 300; margin-bottom: 30px;")),
        waiter::spin_6(), # Hospital-friendly spinner
        br(),br(),br(),
        p("Vent venligst mens appen initialiseres", 
          style = paste("color:", HOSPITAL_COLORS$secondary, "; font-size: 0.9rem;"))
      )
    )
  ),
  
  # File upload loading
  file_upload = list(
    html = tagList(
      h4("Behandler fil...", style = paste("color:", HOSPITAL_COLORS$primary)),
      br(),
      waiter::spin_3(),
      br(),br(),
      p("Læser og validerer data", style = paste("color:", HOSPITAL_COLORS$secondary))
    ),
    color = "rgba(255,255,255,0.9)"
  ),
  
  # Plot generation loading  
  plot_generation = list(
    html = tagList(
      h4("Genererer SPC graf...", style = paste("color:", HOSPITAL_COLORS$primary)),
      br(),
      waiter::spin_4(),
      br(),br(),
      p("Anvender Anhøj-regler og beregner kontrolgrænser", 
        style = paste("color:", HOSPITAL_COLORS$secondary, "; font-size: 0.9rem;"))
    ),
    color = "rgba(248,249,250,0.95)"
  )
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

# Dansk oversættelse af chart typer - UPDATED STRUCTURE
CHART_TYPES_DA <- list(
  "Seriediagram (Run Chart)" = "run",
  "I-kort (Individuelle værdier)" = "i", 
  "MR-kort (Moving Range)" = "mr",
  "P-kort (Andele)" = "p",
  "P'-kort (Andele, standardiseret)" = "pp",
  "U-kort (Rater)" = "u",
  "U'-kort (Rater, standardiseret)" = "up",
  "C-kort (Tællinger)" = "c",
  "G-kort (Tid mellem hændelser)" = "g"
)

# Reverse mapping for converting back to English codes
CHART_TYPES_EN <- list(
  "run" = "run",
  "i" = "i", 
  "mr" = "mr",
  "p" = "p",
  "pp" = "pp",
  "u" = "u",
  "up" = "up",
  "c" = "c",
  "g" = "g"
)

# Helper function to convert Danish display names to English qic codes
get_qic_chart_type <- function(danish_selection) {
  if (is.null(danish_selection) || danish_selection == "") {
    return("run")  # default
  }
  
  # If it's already an English code, return as-is
  if (danish_selection %in% names(CHART_TYPES_EN)) {
    return(danish_selection)
  }
  
  # Look up in the Danish-to-English mapping
  english_code <- CHART_TYPES_DA[[danish_selection]]
  if (!is.null(english_code)) {
    return(english_code)
  }
  
  # Special handling for exact matches
  if (danish_selection == "Seriediagram (Run Chart)") {
    return("run")
  }
  if (danish_selection == "P-kort (Andele)") {
    return("p")
  }
  
  # Fallback
  cat("WARNING: Unknown chart type selection:", danish_selection, "- using 'run' as fallback\n")
  return("run")
}

# Helper function to ensure standard columns are present and in correct order
ensure_standard_columns <- function(data) {
  # Define standard columns in the correct order
  standard_cols <- c("Skift", "Dato", "Tæller", "Nævner", "Kommentar")
  
  # Add missing standard columns
  for (col in standard_cols) {
    if (!col %in% names(data)) {
      if (col == "Skift") {
        data[[col]] <- FALSE
      } else if (col %in% c("Tæller", "Nævner")) {
        data[[col]] <- NA_real_
      } else {
        data[[col]] <- NA_character_
      }
    }
  }
  
  # Get non-standard columns (user's additional columns)
  extra_cols <- setdiff(names(data), standard_cols)
  
  # Reorder: standard columns first (in correct order), then user's columns
  final_order <- c(standard_cols, extra_cols)
  
  # Return data with correct column order
  return(data[, final_order, drop = FALSE])
}

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

