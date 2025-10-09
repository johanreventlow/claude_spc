# UI Configuration
# UI layout constants, styles, and sizing configuration

# UI LAYOUT CONSTANTS ==========================================================

#' Standard kolonne bredder for UI
#'
#' Prædefinerede kolonne bredde kombinationer til bslib layout systemer.
#' @format Named list med numeriske vektorer for kolonne bredder
#' @export
UI_COLUMN_WIDTHS <- list(
  quarter = c(6, 6, 6, 6),
  half = c(6, 6),
  thirds = c(4, 4, 4),
  sidebar = c(3, 9)
)

#' Standard højder for UI komponenter
#'
#' CSS højde værdier til konsistent UI layout på tværs af komponenter.
#' @format Named list med CSS højde strings
#' @export
UI_HEIGHTS <- list(
  logo = "40px",
  modal_content = "300px",
  chart_container = "calc(50vh - 60px)",
  table_max = "200px",
  sidebar_min = "130px"
)

#' CSS styles constants
#'
#' Genbrugelige CSS style strings til konsistent styling.
#' @format Named list med CSS style strings
#' @export
UI_STYLES <- list(
  flex_column = "display: flex; flex-direction: column; flex: 1 1 auto; min-height: 0;",
  scroll_auto = "max-height: 300px; overflow-y: auto;",
  full_width = "width: 100%;",
  right_align = "text-align: right;",
  margin_right = "margin-right: 10px;",
  position_absolute_right = "position: absolute; right: 20px; top: 20px; font-weight: bold;"
)

#' Standard UI input widths
#' @export
UI_INPUT_WIDTHS <- list(
  full = "100%",
  half = "50%",
  quarter = "25%",
  three_quarter = "75%",
  auto = "auto"
)

#' Layout proportions for consistent UI
#' @export
UI_LAYOUT_PROPORTIONS <- list(
  half = 1 / 2,
  third = 1 / 3,
  quarter = 1 / 4,
  two_thirds = 2 / 3,
  three_quarters = 3 / 4
)

# FONT SCALING CONFIGURATION ===================================================

#' Responsive font scaling configuration
#'
#' Styrer hvordan base_size skaleres baseret på viewport bredde og pixelratio.
#'
#' @details
#' base_size beregnes som: max(min_size, min(max_size, width_px / divisor)) / pixelratio
#'
#' - divisor: Lavere værdi = større fonts (50 = ~40% større end 70)
#' - min_size: Minimum font size uanset viewport
#' - max_size: Maximum font size selv på store skærme
#'
#' Eksempler ved divisor = 50:
#' - 500px bred: base_size = 10pt (Standard) / 5pt (Retina)
#' - 700px bred: base_size = 14pt (Standard) / 7pt (Retina)
#' - 1000px bred: base_size = 14pt (capped) / 7pt (capped)
#'
#' @format Named list med scaling parametre
#' @export
FONT_SCALING_CONFIG <- list(
  divisor = 56,     # Viewport width divisor (lower = larger fonts)
  min_size = 8,     # Minimum base_size i points
  max_size = 64     # Maximum base_size i points
)
