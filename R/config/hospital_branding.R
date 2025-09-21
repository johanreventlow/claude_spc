# Hospital Branding Configuration
# Extracted from global.R for better modularity

# HOSPITAL BRANDING ================================

## Brand.yml konfiguration -----
# Læs brand.yml konfiguration
brand_config <- yaml::read_yaml("_brand.yml")

# Opret tema baseret på brand.yml (auto-opdaget som _brand.yml)
my_theme <- bslib::bs_theme(brand = "_brand.yml")

## Hospital information -----
HOSPITAL_NAME <- brand_config$meta$name
HOSPITAL_LOGO_PATH <- brand_config$logo$image

## Hospital farver -----
# Hent ALLE farver fra brand.yml (via bs_theme)
HOSPITAL_COLORS <- list(
  primary = brand_config$color$palette$primary,
  secondary = brand_config$color$palette$secondary,
  accent = brand_config$color$palette$accent, # Fra brand.yml palette
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

# GRAFIK TEMAER ================================

## ggplot2 hospital tema -----
HOSPITAL_THEME <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = HOSPITAL_COLORS$primary, size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(color = HOSPITAL_COLORS$secondary, size = 12),
      axis.title = ggplot2::element_text(color = HOSPITAL_COLORS$dark, size = 11),
      axis.text = ggplot2::element_text(color = HOSPITAL_COLORS$dark, size = 10),
      legend.title = ggplot2::element_text(color = HOSPITAL_COLORS$dark, size = 11),
      legend.text = ggplot2::element_text(color = HOSPITAL_COLORS$dark, size = 10),
      panel.grid.major = ggplot2::element_line(color = HOSPITAL_COLORS$light),
      panel.grid.minor = ggplot2::element_line(color = HOSPITAL_COLORS$light),
      strip.text = ggplot2::element_text(color = HOSPITAL_COLORS$primary, face = "bold")
    )
}

## Standard footer til alle grafer -----
create_plot_footer <- function(afdeling = "", data_kilde = "", dato = Sys.Date()) {
  paste0(
    HOSPITAL_NAME,
    if (afdeling != "") paste0(" - ", afdeling) else "",
    " | Datakilde: ", data_kilde,
    " | Genereret: ", format(dato, "%d-%m-%Y"),
    " | SPC analyse med Anhøj-regler"
  )
}