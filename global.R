# GLOBAL KONFIGURATIONSFIL ================================================

# BIBLIOTEKER OG DEPENDENCIES --------------------------------

library(shiny)
library(bslib)  # Til page_navbar, card, sidebar etc.
library(qicharts2)
library(excelR)  # Til Excel-lignende redigerbare tabeller
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(zoo)  # Til rullende gennemsnit i beregnede felter
library(scales)  # Til procent-formatering i plots
library(rlang)     # Til %||% operatoren
library(lubridate)
library(openxlsx)  # Til Excel export funktionalitet
library(waiter)
library(yaml)  # Til læsning af brand.yml
library(later)  # Til forsinket udførelse

# UDVIKLINGSINDSTILLINGER --------------------------------

## Testmodus -----
# TEST MODE: Auto-indlæs eksempeldata til qic() fejlfinding
# Sæt til FALSE for at deaktivere auto-indlæsning og vende tilbage til normal brugerstyret dataindlæsning
TEST_MODE_AUTO_LOAD <- FALSE

## Auto-gendannelse -----
# AUTO-RESTORE: Gendan automatisk tidligere sessioner
# Sæt til FALSE under udvikling, TRUE til produktion
AUTO_RESTORE_ENABLED <- FALSE

## Tabeltype -----
# TABLE TYPE: Bruger excelR til Excel-lignende redigerbare tabeller

# HJÆLPEFUNKTIONER --------------------------------

source("R/utils/danish_numbers.R")

# HOSPITAL BRANDING ================================

## Brand.yml konfiguration -----
# Læs brand.yml konfiguration
brand_config <- yaml::read_yaml("_brand.yml")

# Opret tema baseret på brand.yml (auto-opdaget som _brand.yml)
my_theme <- bs_theme(brand = "_brand.yml")

## Hospital information -----
HOSPITAL_NAME <- brand_config$meta$name
HOSPITAL_LOGO_PATH <- brand_config$logo$image

## Hospital farver -----
# Hent ALLE farver fra brand.yml (via bs_theme)
HOSPITAL_COLORS <- list(
  primary = brand_config$color$palette$primary,
  secondary = brand_config$color$palette$secondary,
  accent = brand_config$color$palette$accent,  # Fra brand.yml palette
  success = brand_config$color$palette$success,
  warning = brand_config$color$palette$warning,
  danger = brand_config$color$palette$danger,
  info = brand_config$color$palette$info,
  light = brand_config$color$palette$light,
  dark = brand_config$color$palette$dark,
  
  hospitalblue = brand_config$color$palette$hospitalblue,
  darkgrey = brand_config$color$palette$darkgrey,
  lightgrey = brand_config$color$palette$lightgrey,
  mediumgrey = brand_config$color$palette$mediumgrey,
  regionhblue = brand_config$color$palette$regionhblue
  
)

# WAITER KONFIGURATION ================================

## Hospital branding til loading screens -----
WAITER_CONFIG <- list(
  # App start loading - FULDSKÆRM OVERLAY
  app_start = list(
    html = tagList(
      div(
        style = "text-align: center; padding: 50px;",
        img(src = basename(HOSPITAL_LOGO_PATH), height = "80px", style = "margin-bottom: 30px;"),
        h2(paste(HOSPITAL_NAME, "SPC App"), 
           style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 300; margin-bottom: 20px;")),
        h4("Indlæser...", 
           style = paste("color:", HOSPITAL_COLORS$secondary, "; font-weight: 300; margin-bottom: 30px;")),
        waiter::spin_6(), # Hospital-venligt spinner
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

# GRAFIK TEMAER ================================

## ggplot2 hospital tema -----
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

## Standard footer til alle grafer -----
create_plot_footer <- function(afdeling = "", data_kilde = "", dato = Sys.Date()) {
  paste0(
    HOSPITAL_NAME, 
    if(afdeling != "") paste0(" - ", afdeling) else "",
    " | Datakilde: ", data_kilde, 
    " | Genereret: ", format(dato, "%d-%m-%Y"),
    " | SPC analyse med Anhøj-regler"
  )
}

# DIAGRAM TYPER ================================

## Dansk oversættelse af chart typer -----
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

## Omvendt mapping til engelske koder -----
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

## Hjælpefunktion til konvertering -----
# Konverter danske displaynavne til engelske qic-koder
get_qic_chart_type <- function(danish_selection) {
  if (is.null(danish_selection) || danish_selection == "") {
    return("run")  # standard
  }
  
  # Hvis det allerede er en engelsk kode, returner som-den-er
  if (danish_selection %in% names(CHART_TYPES_EN)) {
    return(danish_selection)
  }
  
  # Slå op i dansk-til-engelsk mapping
  english_code <- CHART_TYPES_DA[[danish_selection]]
  if (!is.null(english_code)) {
    return(english_code)
  }
  
  # Særlig håndtering for eksakte match
  if (danish_selection == "Seriediagram (Run Chart)") {
    return("run")
  }
  if (danish_selection == "P-kort (Andele)") {
    return("p")
  }
  
  # Fallback
  return("run")
}

# DATABEHANDLING ================================

## Standardkolonner hjælpefunktion -----
# Sikrer at standardkolonner er til stede og i korrekt rækkefølge
ensure_standard_columns <- function(data) {
  # Definer standardkolonner i den korrekte rækkefølge - Skift og Frys altid først
  standard_cols <- c("Skift", "Frys", "Dato", "Tæller", "Nævner", "Kommentar")
  
  # Tilføj manglende standardkolonner
  for (col in standard_cols) {
    if (!col %in% names(data)) {
      if (col == "Skift" || col == "Frys") {
        data[[col]] <- FALSE
      } else if (col %in% c("Tæller", "Nævner")) {
        data[[col]] <- NA_real_
      } else {
        data[[col]] <- NA_character_
      }
    }
  }
  
  # Hent ikke-standard kolonner (brugerens ekstra kolonner)
  extra_cols <- setdiff(names(data), standard_cols)
  
  # Omorganiser: standardkolonner først (i korrekt rækkefølge), derefter brugerens kolonner
  final_order <- c(standard_cols, extra_cols)
  
  # Returner data med korrekt kolonnerækkefølge
  return(data[, final_order, drop = FALSE])
}

# VALIDERINGSFUNKTIONER ================================

## Numerisk kolonnevalidering -----
validate_numeric_column <- function(data, column_name) {
  if (!column_name %in% names(data)) {
    return(paste("Kolonne", column_name, "ikke fundet"))
  }
  if (!is.numeric(data[[column_name]])) {
    return(paste("Kolonne", column_name, "skal være numerisk"))
  }
  return(NULL)
}

## Dato kolonnevalidering -----
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

