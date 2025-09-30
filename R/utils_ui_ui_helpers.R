# utils_ui_helpers.R
# Genanvendelige UI helper funktioner og konstanter

#' Opret standard flex container style
#'
#' Returnerer standard CSS flex container style for kolonnelayout
#'
#' @return Karakter string med CSS styles
#' @export
get_flex_column_style <- function() {
  UI_STYLES$flex_column
}

#' Opret scrollbar container style med maksimal højde
#'
#' @param max_height Maksimal højde (default bruger konstant)
#' @return Karakter string med CSS styles
#' @export
get_scroll_container_style <- function(max_height = UI_HEIGHTS$modal_content) {
  paste0("max-height: ", max_height, "; overflow-y: auto;")
}

#' Standard UI kolonne bredder
#'
#' @param layout Type af layout ("quarter", "half", "thirds", "sidebar")
#' @return Numerisk vektor med kolonne bredder
#' @export
get_column_widths <- function(layout = "quarter") {
  UI_COLUMN_WIDTHS[[layout]]
}

#' Opret CSS style for absolute positioning (logo/brand)
#'
#' @return Karakter string med CSS styles for højre side positioning
#' @export
get_absolute_right_style <- function() {
  UI_STYLES$position_absolute_right
}

#' Opret CSS style for fuld bredde elementer
#'
#' @return Karakter string med CSS styles
#' @export
get_full_width_style <- function() {
  UI_STYLES$full_width
}

#' Standard højder for forskellige UI komponenter
#'
#' @param component Type af komponent ("logo", "modal_content", "chart_container", etc.)
#' @return Karakter string med højde
#' @export
get_ui_height <- function(component) {
  UI_HEIGHTS[[component]]
}

#' Opret responsive chart container stil
#'
#' @param base_height Grundlæggende højde (default: "50vh")
#' @param offset Offset fra toppen (default: "60px")
#' @return Karakter string med CSS calc() stil
#' @export
get_chart_container_style <- function(base_height = "50vh", offset = "60px") {
  paste0("height: calc(", base_height, " - ", offset, ");")
}

#' Standard spacing konstanter for UI
#'
#' @export
UI_SPACING <- list(
  small = "10px",
  medium = "20px",
  large = "30px",
  margin_right = "10px"
)

#' Opret standard kort (card) configuration
#'
#' @param title Titel for kortet
#' @param height Højde af kortet (valgfri)
#' @param full_screen Om kortet skal være fuld skærm
#' @return Liste med kort konfiguration
#' @export
create_card_config <- function(title, height = NULL, full_screen = FALSE) {
  config <- list(title = title)

  if (!is.null(height)) {
    config$height <- height
  }

  if (full_screen) {
    config$max_height <- "100%"
    config$min_height <- "100%"
  }

  return(config)
}

# INPUT SANITIZATION UTILITIES ================================

#' Robust input sanitization for character(0) and NA handling
#'
#' Centralized input sanitization function that handles character(0),
#' NA values, empty strings, and vectors. Used throughout the app
#' for consistent dropdown and input validation.
#'
#' @param input_value Input value to sanitize (can be any type)
#'
#' @return Sanitized value or NULL if invalid/empty
#'
#' @details
#' Handles the following cases:
#' - NULL values → NULL
#' - character(0) → NULL
#' - Vectors with all NA → NULL
#' - Vectors longer than 1 → first element only
#' - Single NA values → NULL
#' - Empty or whitespace-only strings → NULL
#' - Valid values → unchanged
#'
#' @examples
#' sanitize_selection(NULL)           # → NULL
#' sanitize_selection(character(0))   # → NULL
#' sanitize_selection(c(NA, NA))      # → NULL
#' sanitize_selection(c("a", "b"))    # → "a"
#' sanitize_selection("")             # → NULL
#' sanitize_selection("  ")           # → NULL
#' sanitize_selection("valid")        # → "valid"
#'
#' @family input_validation
#' @export
sanitize_selection <- function(input_value) {
  if (is.null(input_value) || length(input_value) == 0 || identical(input_value, character(0))) {
    return(NULL)
  }
  # Handle vectors with all NA values
  if (all(is.na(input_value))) {
    return(NULL)
  }
  # Handle vectors - use first element only
  if (length(input_value) > 1) {
    input_value <- input_value[1]
  }
  # Handle single NA value
  if (is.na(input_value)) {
    return(NULL)
  }
  # Handle empty strings and whitespace-only strings
  if (is.character(input_value) && (input_value == "" || trimws(input_value) == "")) {
    return(NULL)
  }
  return(input_value)
}