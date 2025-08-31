library(shiny)
library(bslib)  # For page_navbar, card, sidebar etc.
library(qicharts2)
library(rhandsontable)
# library(DT)
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

# TEST MODE: Auto-load example data for qic() debugging
# Set to FALSE to disable auto-loading and return to normal user-controlled data loading
TEST_MODE_AUTO_LOAD <- FALSE

# Load utility functions
source("R/utils/danish_numbers.R")

# -----------------------------------------------------------------------------
# HOSPITAL BRANDING CONFIGURATION
# -----------------------------------------------------------------------------

# Hospital information
HOSPITAL_NAME <- "Bispebjerg og Frederiksberg Hospital"
HOSPITAL_LOGO_PATH <- "www/Logo_Bispebjerg_og Frederiksberg_RGB_neg.png"  # Skal placeres i www/ mappen

# Farvepalette (skal matches med hospital branding)
theme_name <- "flatly"
my_theme <- bs_theme(bootswatch = theme_name)

HOSPITAL_COLORS <- list(
  primary = bs_get_variables(my_theme, "primary") |> as.character(),      # Primær blå
  secondary = bs_get_variables(my_theme, "secondary") |> as.character(),    # Mørkere blå  
  accent = "#FF6B35",       # Orange accent
  success = bs_get_variables(my_theme, "success"),      # Grøn for positive signals
  warning = bs_get_variables(my_theme, "warning"),      # Gul for advarsler
  danger = bs_get_variables(my_theme, "danger"),       # Rød for alerts
  light = bs_get_variables(my_theme, "light"),        # Lys baggrund
  dark = bs_get_variables(my_theme, "dark")          # Mörk tekst
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

# Test data generator for udvikling og demo
generate_test_data <- function(type = "infection_rates", n = 50) {
  
  # Create date sequence
  dates <- seq(from = as.Date("2023-01-01"), by = "week", length.out = n)
  
  if (type == "infection_rates") {
    # Hospital infection rates (per 1000 patient days)
    set.seed(123)
    baseline_rate <- 2.5
    rates <- baseline_rate + rnorm(n, 0, 0.5)
    
    # Add some special cause variation
    rates[20:25] <- rates[20:25] + 1.5  # Outbreak period
    rates[35:40] <- rates[35:40] - 0.8  # Improvement
    
    # Ensure non-negative
    rates <- pmax(rates, 0.1)
    
    data.frame(
      Dato = dates,
      Infektionsrate = round(rates, 2),
      Patientdage = sample(800:1200, n),
      Afdeling = sample(c("Medicinsk", "Kirurgisk", "Intensiv"), n, replace = TRUE),
      stringsAsFactors = FALSE
    )
    
  } else if (type == "waiting_times") {
    # Waiting times in minutes
    set.seed(456)
    baseline_wait <- 45
    wait_times <- baseline_wait + rnorm(n, 0, 10)
    
    # Add trend and variation
    wait_times[1:20] <- wait_times[1:20] + seq(0, 15, length.out = 20)  # Increasing trend
    wait_times[30:n] <- wait_times[30:n] - 5  # Improvement after intervention
    
    data.frame(
      Dato = dates,
      Ventetid_minutter = round(pmax(wait_times, 5), 1),
      Antal_patienter = sample(50:150, n),
      Klinik = sample(c("Ambulatorie", "Akutmodtagelse"), n, replace = TRUE),
      stringsAsFactors = FALSE
    )
    
  } else if (type == "medication_errors") {
    # Medication errors (count data)
    set.seed(789)
    baseline_errors <- 3
    errors <- rpois(n, baseline_errors)
    
    # Add special variation
    errors[15:18] <- errors[15:18] + sample(3:8, 4)  # Bad period
    
    data.frame(
      Dato = dates,
      Medicinfejl = errors,
      Udskrivninger = sample(200:400, n),
      Afdeling = sample(c("Medicinsk", "Kirurgisk", "Pædiatrisk"), n, replace = TRUE),
      stringsAsFactors = FALSE
    )
  } else if (type == "proportion_data") {
    # Generate tæller/nævner data like user's CSV
    set.seed(999)
    baseline_rate <- 0.15  # 15% base rate
    
    dates <- seq(from = as.Date("2023-01-01"), by = "week", length.out = n)
    naevner <- sample(500:800, n)  # Population sizes
    
    # Generate realistic proportions with some variation
    rates <- baseline_rate + rnorm(n, 0, 0.03)
    rates[20:25] <- rates[20:25] + 0.05  # Increase period
    rates[35:40] <- rates[35:40] - 0.03  # Improvement
    rates <- pmax(rates, 0.01)  # Ensure positive
    rates <- pmin(rates, 0.3)   # Cap at reasonable level
    
    taeller <- round(naevner * rates)
    
    data.frame(
      Dato = dates,
      Taeller = taeller,
      Naevner = naevner,
      stringsAsFactors = FALSE
    )
  }
}

# Helper function to validate Skift column for center line phases
validateSkiftColumn <- function(data) {
  if (!"Skift" %in% names(data)) {
    return(list(
      valid = TRUE,
      warnings = character(0),
      info = "Ingen Skift kolonne - én center line for alle data",
      phase_count = 1,
      shift_points = integer(0)
    ))
  }
  
  skift_col <- data[["Skift"]]
  warnings <- character(0)
  info_messages <- character(0)
  
  # Konverter til logical og tjek for problemer
  if (!is.logical(skift_col)) {
    # Prøv at konvertere fra forskellige formater
    if (is.character(skift_col)) {
      # Håndter tekst som "TRUE", "FALSE", "1", "0", "ja", "nej"
      skift_col_clean <- tolower(trimws(skift_col))
      logical_col <- ifelse(skift_col_clean %in% c("true", "1", "ja", "yes", "x"), TRUE,
                           ifelse(skift_col_clean %in% c("false", "0", "nej", "no", ""), FALSE, NA))
      
      if (any(is.na(logical_col) & !is.na(skift_col))) {
        warnings <- c(warnings, "Nogle værdier i Skift kolonnen kunne ikke forstås (brug TRUE/FALSE eller 1/0)")
      }
      skift_col <- logical_col
    } else {
      converted <- as.logical(skift_col)
      if (any(is.na(converted) & !is.na(skift_col))) {
        warnings <- c(warnings, "Nogle værdier i Skift kolonnen kunne ikke konverteres til TRUE/FALSE")
      }
      skift_col <- converted
    }
  }
  
  # Tæl skift
  skift_count <- sum(skift_col == TRUE, na.rm = TRUE)
  total_points <- length(skift_col)
  
  if (skift_count == 0) {
    info_messages <- c(info_messages, "Ingen phase shifts markeret - én center line beregnes")
    phase_count <- 1
  } else if (skift_count > total_points / 2) {
    warnings <- c(warnings, paste("Meget mange phase shifts (", skift_count, ") - overvej om dette er hensigtsmæssigt"))
    phase_count <- skift_count + 1
  } else {
    info_messages <- c(info_messages, paste(skift_count, "phase shift(s) detekteret -", skift_count + 1, "separate center lines beregnes"))
    phase_count <- skift_count + 1
  }
  
  # Find skift punkter
  shift_points <- which(skift_col == TRUE)
  
  # Tjek for skift på første observation
  if (length(shift_points) > 0 && 1 %in% shift_points) {
    warnings <- c(warnings, "Phase shift på første observation ignoreres - phase 1 starter altid ved observation 1")
    shift_points <- shift_points[shift_points != 1]
    if (length(shift_points) == 0) {
      phase_count <- 1
      info_messages <- c("Phase shift kun på første observation ignoreret - én center line beregnes")
    }
  }
  
  return(list(
    valid = length(warnings) == 0,
    warnings = warnings,
    info = info_messages,
    phase_count = phase_count,
    shift_points = shift_points
  ))
}

# Quick access to test data
get_demo_infection_data <- function() generate_test_data("infection_rates", 40)
get_demo_waiting_data <- function() generate_test_data("waiting_times", 52)  # 1 year weekly
get_demo_error_data <- function() generate_test_data("medication_errors", 30)
get_demo_proportion_data <- function() generate_test_data("proportion_data", 36)  # Like user's data