# R/modules/visualization_helpers.R
# Hjælpefunktioner for visualiseringsmodulet

# Dependencies ----------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(lubridate)

# CHART KONFIGURATION =========================================================

## Auto-detektering af Chart Konfiguration
# Automatisk identifikation af kolonner til X/Y-akser og nævner
detectChartConfiguration <- function(data, chart_type) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0 || ncol(data) == 0) {
    return(list(x_col = NULL, y_col = NULL, n_col = NULL, chart_type = chart_type))
  }

  # Filter out "Skift" column from detection
  available_cols <- names(data)[names(data) != "Skift"]

  # Find date/time column
  x_col <- NULL
  for (col_name in available_cols) {
    col_data <- data[[col_name]]

    if (all(is.na(col_data)) || length(col_data) == 0) {
      next
    }

    # Enhanced name-based detection including time units
    if (grepl("dato|date|tid|time|år|year|måned|month|uge|week|dag|day", col_name, ignore.case = TRUE)) {
      x_col <- col_name
      break
    }

    safe_operation(
      "Detect date column",
      code = {
        char_data <- as.character(col_data)[!is.na(col_data)]
        if (length(char_data) > 0) {
          test_sample <- char_data[1:min(3, length(char_data))]

          # Use guess_formats for more intelligent date detection (with error handling)
          safe_operation(
            "Parse dates with lubridate",
            code = {
              guessed_formats <- suppressWarnings(
                lubridate::guess_formats(test_sample, c("dmy", "ymd", "mdy", "dby", "dmY", "Ymd", "mdY"))
              )

              if (!is.null(guessed_formats) && length(guessed_formats) > 0) {
                # Filter out invalid formats
                valid_formats <- guessed_formats[!grepl("^n$|Unknown", guessed_formats)]

                if (length(valid_formats) > 0) {
                  # Test the valid formats
                  date_test <- suppressWarnings(
                    lubridate::parse_date_time(test_sample, orders = valid_formats, quiet = TRUE)
                  )

                  if (!is.null(date_test) && length(date_test) > 0) {
                    success_rate <- sum(!is.na(date_test)) / length(date_test)
                    if (success_rate >= 0.5) {
                      x_col <- col_name
                      break
                    }
                  }
                }
              }
            },
            fallback = function(e) {
              # Skip this column if parsing fails
              log_warn("Date parsing failed for column", col_name, ":", e$message, "DATE_DETECTION")
            },
            error_type = "processing"
          )
        }
      },
      fallback = function(e) {
        # Ignore errors in date detection
      },
      error_type = "processing"
    )
  }

  if (is.null(x_col) && length(available_cols) > 0) {
    x_col <- available_cols[1]
  }

  # Find numeric columns
  numeric_cols <- character(0)
  for (col_name in available_cols) {
    if (col_name != x_col) {
      col_data <- data[[col_name]]
      if (is.numeric(col_data)) {
        numeric_cols <- c(numeric_cols, col_name)
      } else {
        numeric_test <- parse_danish_number(col_data)
        if (sum(!is.na(numeric_test)) > length(col_data) * 0.8) {
          numeric_cols <- c(numeric_cols, col_name)
        }
      }
    }
  }

  # Detect y and n columns
  y_col <- NULL
  n_col <- NULL

  # Look for Danish tæller/nævner patterns
  available_cols_lower <- tolower(available_cols)
  taeller_idx <- which(grepl("t.ller|tael|num|count", available_cols_lower, ignore.case = TRUE))
  naevner_idx <- which(grepl("n.vner|naev|denom|total", available_cols_lower, ignore.case = TRUE))

  if (length(taeller_idx) > 0 && length(naevner_idx) > 0) {
    y_col <- available_cols[taeller_idx[1]]
    n_col <- available_cols[naevner_idx[1]]
  } else if (length(numeric_cols) >= 2) {
    y_col <- numeric_cols[1]
    n_col <- numeric_cols[2]
  } else if (length(numeric_cols) >= 1) {
    y_col <- numeric_cols[1]
  }

  return(list(
    x_col = x_col,
    y_col = y_col,
    n_col = n_col,
    chart_type = chart_type
  ))
}

# DATA VALIDERING =============================================================

## Validering af Data til Chart Generering
# Tjekker data kvalitet og kompatibilitet med valgt chart type
validateDataForChart <- function(data, config, chart_type) {
  warnings <- character(0)

  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    warnings <- c(warnings, "Ingen data tilgængelig")
    return(list(valid = FALSE, warnings = warnings))
  }

  if (is.null(config$y_col) || !config$y_col %in% names(data)) {
    warnings <- c(warnings, "Ingen numerisk kolonne fundet til Y-akse")
    return(list(valid = FALSE, warnings = warnings))
  }

  if (chart_type %in% c("p", "pp", "u", "up")) {
    if (is.null(config$n_col) || !config$n_col %in% names(data)) {
      warnings <- c(warnings, paste("Chart type", chart_type, "kræver en nævner-kolonne"))
      return(list(valid = FALSE, warnings = warnings))
    }
  }

  # Check for missing values
  y_data <- data[[config$y_col]]
  if (all(is.na(y_data))) {
    warnings <- c(warnings, "Alle værdier i Y-kolonnen er tomme")
    return(list(valid = FALSE, warnings = warnings))
  }

  # Skift column validation handled by qicharts2::qic() internally

  if (nrow(data) < 8) {
    warnings <- c(warnings, paste("Kun", nrow(data), "datapunkter - SPC analyse er mest pålidelig med mindst 15-20 punkter"))
  }

  return(list(valid = TRUE, warnings = warnings))
}

# PLOT BESKEDER ==============================================================

## HTML-formaterede Plot Beskeder
# Opretter brugervenlige beskeder til forskellige plot tilstande
createPlotMessage <- function(type = "welcome", message = NULL, details = NULL) {
  # Define message types
  message_config <- list(
    welcome = list(
      icon = "chart-line",
      title = "Velkommen til SPC Analyse",
      default_message = "Kom i gang ved at uploade data eller starte en ny session",
      theme = "primary",
      suggestions = c("Upload en Excel eller CSV fil", "Klik 'Start ny session' for manuel indtastning")
    ),
    no_data = list(
      icon = "folder",
      title = "Ingen data fundet",
      default_message = "Upload data eller indtast værdier i tabellen for at fortsætte",
      theme = "secondary",
      suggestions = c("Tjek at dine data er i korrekt format", "Excel filer skal have kolonneoverskrifter")
    ),
    config_needed = list(
      icon = "sliders-h",
      title = "Konfiguration påkrævet",
      default_message = "Vælg kolonner for X- og Y-akse for at generere grafen",
      theme = "warning",
      suggestions = c("Klik 'Auto-detektér kolonner' i fanen Kolonner", "Vælg X- og Y-akser manuelt i Indstillinger")
    ),
    validation_error = list(
      icon = "exclamation-triangle",
      title = "Data validering fejlede",
      default_message = "Der er problemer med dine data der forhindrer graf-generering",
      theme = "danger",
      suggestions = c("Tjek at numeriske kolonner kun indeholder tal", "P- og U-kort kræver både tæller og nævner kolonner")
    ),
    insufficient_data = list(
      icon = "chart-bar",
      title = "For få datapunkter",
      default_message = "SPC analyse kræver mindst 8-10 datapunkter for pålidelige resultater",
      theme = "warning",
      suggestions = c("Tilføj flere observationer til dit dataset", "SPC er mest pålidelig med 15-20+ datapunkter")
    ),
    technical_error = list(
      icon = "tools",
      title = "Teknisk fejl",
      default_message = "Der opstod en uventet fejl ved graf-generering",
      theme = "danger",
      suggestions = c("Prøv at genindlæse siden", "Kontakt support hvis problemet fortsætter")
    )
  )

  config <- message_config[[type]]
  if (is.null(config)) {
    config <- message_config[["technical_error"]] # fallback
  }

  # Use provided message or default
  display_message <- if (!is.null(message)) message else config$default_message

  # Create Bootstrap card with message
  div(
    class = "d-flex align-items-center justify-content-center h-100",
    div(
      class = paste("card border-", config$theme, "text-center"),
      style = "max-width: 500px; width: 90%;",
      div(
        class = paste("card-header bg-", config$theme, "text-white"),
        div(
          class = "d-flex align-items-center justify-content-center",
          icon(config$icon, class = "me-2"),
          h5(config$title, class = "mb-0")
        )
      ),
      div(
        class = "card-body",
        p(display_message, class = "card-text mb-3"),

        # Show details if provided
        if (!is.null(details)) {
          div(
            class = paste("alert alert-", config$theme, "alert-sm"),
            style = "font-size: 0.9rem;",
            details
          )
        },

        # Suggestions list
        div(
          class = "mt-3",
          h6("Næste skridt:", class = "text-muted mb-2"),
          tags$ul(
            class = "list-unstyled text-start",
            lapply(config$suggestions, function(suggestion) {
              tags$li(
                class = "mb-1",
                icon("check-circle", class = "text-success me-2"),
                suggestion
              )
            })
          )
        )
      )
    )
  )
}

## Legacy Kompatibilitet
# Bagudkompatibilitet for ældre kode
showPlaceholder <- function() {
  createPlotMessage("technical_error")
}

# PLOT STYLING ===============================================================

## Hospital Tema til Plots
# Anvender hospital branding og farvepalette på SPC plots
applyHospitalTheme <- function(plot) {
  if (is.null(plot) || !inherits(plot, "ggplot")) {
    return(plot)
  }

  safe_operation(
    "Apply hospital theme to plot",
    code = {
      footer_text <- safe_operation(
        "Create plot footer",
        code = {
          create_plot_footer(
            afdeling = "",
            data_kilde = "Upload",
            dato = Sys.Date()
          )
        },
        fallback = function(e) {
          "SPC Analyse" # fallback text
        },
        error_type = "processing"
      )

      themed_plot <- plot +
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
        ) +
        labs(caption = footer_text) +
        theme(
          plot.caption = element_text(size = 8, color = HOSPITAL_COLORS$secondary, hjust = 0)
        )

      return(themed_plot)
    },
    fallback = function(e) {
      return(plot)
    },
    error_type = "processing"
  )
}


## Y-akse Skalering
# Automatisk detektering af passende Y-akse format (decimal, procent, heltal)
detectYAxisScale <- function(y_data) {
  if (is.null(y_data) || length(y_data) == 0) {
    return("integer")
  }

  # Remove NA values
  y_clean <- y_data[!is.na(y_data)]

  if (length(y_clean) == 0) {
    return("integer")
  }

  max_val <- max(y_clean)
  min_val <- min(y_clean)

  # Rule 1: Decimal scale (0-1)
  if (max_val <= 1.0) {
    return("decimal")
  }

  # Rule 2: Percent scale (0-100+ with most values looking like percentages)
  if (min_val >= 0 && max_val <= 200) {
    # Check if most values look like percentages (0-100 range)
    percent_like_count <- sum(y_clean >= 0 & y_clean <= 100)
    if (percent_like_count / length(y_clean) >= 0.7) { # 70% threshold
      return("percent")
    }
  }

  # Rule 3: Integer/rate scale
  return("integer")
}
