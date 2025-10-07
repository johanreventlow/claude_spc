# Package-Internal Branding Getters
# Safe access to branding configuration without global environment pollution

# Package environment for storing initialized branding
claudespc_branding <- new.env(parent = emptyenv())

#' Get Brand Configuration File Path
#'
#' @noRd
get_brand_config_path <- function() {
  # Try package installation first
  brand_path <- system.file("config", "brand.yml", package = "SPCify")

  if (brand_path == "" || !file.exists(brand_path)) {
    # Fallback for development (package not installed)
    fallback_paths <- c(
      "inst/config/brand.yml",
      "_brand.yml"
    )

    for (path in fallback_paths) {
      if (file.exists(path)) {
        return(path)
      }
    }

    # If no brand file found, return NULL and use defaults
    warning("Brand configuration file not found. Using default branding.")
    return(NULL)
  }

  return(brand_path)
}

#' Load Brand Configuration
#'
#' @noRd
load_brand_config <- function() {
  brand_path <- get_brand_config_path()

  if (is.null(brand_path)) {
    # Default configuration if brand.yml not found
    return(list(
      meta = list(
        name = "SPC Hospital",
        description = "Statistical Process Control værktøj"
      ),
      logo = list(
        image = "www/SPCify.png"
      ),
      color = list(
        palette = list(
          primary = "#375a7f",
          secondary = "#6c757d",
          accent = "#FF6B35",
          success = "#00891a",
          warning = "#f9b928",
          danger = "#c10000",
          info = "#009ce8",
          light = "#f8f8f8",
          dark = "#202020",
          hospitalblue = "#009ce8",
          darkgrey = "#565656",
          lightgrey = "#AEAEAE",
          mediumgrey = "#858585",
          regionhblue = "#00293d"
        )
      )
    ))
  }

  tryCatch(
    {
      yaml::read_yaml(brand_path)
    },
    error = function(e) {
      warning(paste("Failed to read brand configuration:", e$message, ". Using defaults."))
      # Return default config on error
      load_brand_config() # Recursive call will return defaults
    }
  )
}

#' Create Bootstrap Theme from Brand Configuration
#'
#' @noRd
create_brand_theme <- function() {
  brand_path <- get_brand_config_path()

  if (is.null(brand_path)) {
    # Create default theme if no brand file
    return(bslib::bs_theme(
      version = 5,
      preset = "flatly"
    ))
  }

  tryCatch(
    {
      # Use bslib's brand parameter with the config file
      bslib::bs_theme(brand = brand_path)
    },
    error = function(e) {
      warning(paste("Failed to create theme from brand file:", e$message, ". Using default theme."))
      bslib::bs_theme(
        version = 5,
        preset = "flatly"
      )
    }
  )
}

#' Initialize Branding Configuration
#'
#' Called during package loading to set up branding configuration
#'
#' @noRd
initialize_branding <- function() {
  # Load brand configuration
  brand_config <- load_brand_config()

  # Store in package environment
  claudespc_branding$config <- brand_config
  claudespc_branding$theme <- create_brand_theme()
  claudespc_branding$hospital_name <- brand_config$meta$name
  claudespc_branding$logo_path <- brand_config$logo$image

  # Build hospital colors
  claudespc_branding$colors <- list(
    primary = brand_config$color$palette$primary,
    secondary = brand_config$color$palette$secondary,
    accent = brand_config$color$palette$accent,
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

  invisible()
}

#' Get Hospital Name
#'
#' @return Character string with hospital name
#' @noRd
get_hospital_name <- function() {
  if (is.null(claudespc_branding$hospital_name)) {
    initialize_branding()
  }
  claudespc_branding$hospital_name %||% "SPC Hospital"
}

#' Get Hospital Logo Path
#'
#' @return Character string with logo path
#' @noRd
get_hospital_logo_path <- function() {
  if (is.null(claudespc_branding$logo_path)) {
    initialize_branding()
  }
  claudespc_branding$logo_path %||% "www/SPCify.png"
}

#' Get Bootstrap Theme
#'
#' @return bslib bootstrap theme object
#' @noRd
get_bootstrap_theme <- function() {
  if (is.null(claudespc_branding$theme)) {
    initialize_branding()
  }
  claudespc_branding$theme %||% bslib::bs_theme(version = 5, preset = "flatly")
}

#' Get Hospital Colors
#'
#' @return List of hospital colors
#' @noRd
get_hospital_colors <- function() {
  if (is.null(claudespc_branding$colors)) {
    initialize_branding()
  }
  claudespc_branding$colors %||% list(
    primary = "#375a7f",
    secondary = "#6c757d",
    accent = "#FF6B35"
  )
}

#' Get Full Brand Configuration
#'
#' @return Complete brand configuration list
#' @noRd
get_brand_config <- function() {
  if (is.null(claudespc_branding$config)) {
    initialize_branding()
  }
  claudespc_branding$config
}

#' Create Hospital ggplot2 Theme
#'
#' @return ggplot2 theme function
#' @noRd
get_hospital_ggplot_theme <- function() {
  colors <- get_hospital_colors()

  function() {
    ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(color = colors$primary, size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(color = colors$secondary, size = 12),
        axis.title = ggplot2::element_text(color = colors$dark, size = 11),
        axis.text = ggplot2::element_text(color = colors$dark, size = 10),
        legend.title = ggplot2::element_text(color = colors$dark, size = 11),
        legend.text = ggplot2::element_text(color = colors$dark, size = 10),
        panel.grid.major = ggplot2::element_line(color = colors$light),
        panel.grid.minor = ggplot2::element_line(color = colors$light),
        strip.text = ggplot2::element_text(color = colors$primary, face = "bold")
      )
  }
}

#' Create Plot Footer
#'
#' @param afdeling Department name (optional)
#' @param data_kilde Data source (optional)
#' @param dato Date (default: today)
#' @return Character string with formatted footer
#' @noRd
create_plot_footer <- function(afdeling = "", data_kilde = "", dato = Sys.Date()) {
  hospital_name <- get_hospital_name()

  paste0(
    hospital_name,
    if (afdeling != "") paste0(" - ", afdeling) else "",
    " | Datakilde: ", data_kilde,
    " | Genereret: ", format(dato, "%d-%m-%Y"),
    " | SPC analyse med Anhøj-regler"
  )
}

# Null-coalescing operator is defined in utils_logging.R
