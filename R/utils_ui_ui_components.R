# utils_ui_components.R
# Genbrugelige UI komponenter til konsistent interface design

#' Opret standard kolonne selectize input
#'
#' Genererer et standard selectizeInput element til kolonnevalg med
#' konsistent styling og placeholder text. Reducerer code duplication
#' og sikrer ensartet brugeroplevelse.
#'
#' @param inputId Character string med input ID
#' @param label Character string med label tekst
#' @param choices Named vector eller list med valgmuligheder (kan være NULL)
#' @param selected Selected value (kan være NULL)
#' @param placeholder Character string med placeholder tekst
#' @param multiple Logical - tillad multiple selections (default FALSE)
#' @param width Character string med CSS width (default full width)
#'
#' @return Shiny selectizeInput element
#'
#' @examples
#' \dontrun{
#' # Standard kolonne valg
#' create_column_selectize("x_col", "X-akse kolonne:", col_choices)
#'
#' # Multiple selection
#' create_column_selectize("multi_cols", "Vælg kolonner:",
#'                         col_choices, multiple = TRUE)
#' }
#'
#' @family ui_components
#' @export
create_column_selectize <- function(inputId, label, choices = NULL, selected = NULL,
                                   placeholder = "Vælg kolonne...", multiple = FALSE,
                                   width = UI_INPUT_WIDTHS$full) {
  shiny::selectizeInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    multiple = multiple,
    width = width,
    options = list(
      placeholder = placeholder,
      searchField = c("label", "value"),
      maxOptions = 1000,
      closeAfterSelect = !multiple
    )
  )
}

#' Opret standard text input med validering
#'
#' Genererer et text input element med standard styling og
#' optional validation patterns. Konsistent interface design.
#'
#' @param inputId Character string med input ID
#' @param label Character string med label tekst
#' @param value Initial value (default "")
#' @param placeholder Character string med placeholder tekst
#' @param width Character string med CSS width (default full)
#' @param pattern Character string med regex validation pattern (optional)
#'
#' @return Shiny textInput element
#'
#' @examples
#' \dontrun{
#' # Standard text input
#' create_text_input("title", "Titel:", placeholder = "Indtast titel...")
#'
#' # Med validation pattern
#' create_text_input("percentage", "Procent:", pattern = "^[0-9]+(\\.[0-9]+)?$")
#' }
#'
#' @family ui_components
#' @export
create_text_input <- function(inputId, label, value = "", placeholder = "",
                             width = UI_INPUT_WIDTHS$full, pattern = NULL) {
  input_elem <- shiny::textInput(
    inputId = inputId,
    label = label,
    value = value,
    placeholder = placeholder,
    width = width
  )

  # Tilføj pattern validation hvis angivet
  if (!is.null(pattern)) {
    input_elem$attribs$pattern <- pattern
  }

  return(input_elem)
}

#' Opret standard numeric input med range validation
#'
#' Genererer et numeric input element med standard styling og
#' optional min/max validation. Konsistent interface design.
#'
#' @param inputId Character string med input ID
#' @param label Character string med label tekst
#' @param value Initial numeric value (default NULL)
#' @param min Minimum værdi (optional)
#' @param max Maximum værdi (optional)
#' @param step Step størrelse (default 1)
#' @param width Character string med CSS width (default full)
#'
#' @return Shiny numericInput element
#'
#' @examples
#' \dontrun{
#' # Standard numeric input
#' create_numeric_input("target", "Målværdi:", value = 80, min = 0, max = 100)
#'
#' # Decimal input
#' create_numeric_input("rate", "Rate:", step = 0.1, min = 0)
#' }
#'
#' @family ui_components
#' @export
create_numeric_input <- function(inputId, label, value = NULL, min = NA, max = NA,
                                step = 1, width = UI_INPUT_WIDTHS$full) {
  shiny::numericInput(
    inputId = inputId,
    label = label,
    value = value,
    min = min,
    max = max,
    step = step,
    width = width
  )
}

#' Opret standard checkbox med styling
#'
#' Genererer et checkbox element med konsistent styling
#' og optional help text.
#'
#' @param inputId Character string med input ID
#' @param label Character string med label tekst
#' @param value Initial boolean value (default FALSE)
#' @param help_text Optional help text der vises under checkbox
#'
#' @return Shiny div element med checkbox og optional help text
#'
#' @examples
#' \dontrun{
#' # Standard checkbox
#' create_checkbox("enable_feature", "Aktiver feature")
#'
#' # Med help text
#' create_checkbox("advanced", "Avancerede indstillinger",
#'                help_text = "Vis ekstra konfigurationsmuligheder")
#' }
#'
#' @family ui_components
#' @export
create_checkbox <- function(inputId, label, value = FALSE, help_text = NULL) {
  checkbox_elem <- shiny::checkboxInput(
    inputId = inputId,
    label = label,
    value = value
  )

  if (!is.null(help_text)) {
    shiny::div(
      checkbox_elem,
      shiny::tags$small(class = "text-muted", help_text)
    )
  } else {
    checkbox_elem
  }
}

#' Opret standard action button med styling
#'
#' Genererer en action button med konsistent styling baseret på type.
#' Understøtter forskellige button styles for forskellige actions.
#'
#' @param inputId Character string med input ID
#' @param label Character string med button tekst
#' @param style Character string med button style ("primary", "secondary", "success", "warning", "danger")
#' @param icon Optional icon for button (shiny::icon())
#' @param disabled Logical - om button skal være disabled (default FALSE)
#' @param width Character string med CSS width (default auto)
#'
#' @return Shiny actionButton element
#'
#' @examples
#' \dontrun{
#' # Primary action button
#' create_action_button("submit", "Gem", "primary", shiny::icon("save"))
#'
#' # Warning button
#' create_action_button("reset", "Nulstil", "warning", shiny::icon("refresh"))
#' }
#'
#' @family ui_components
#' @export
create_action_button <- function(inputId, label, style = "primary", icon = NULL,
                                disabled = FALSE, width = "auto") {

  # Map styles til Bootstrap klasser
  class_map <- list(
    primary = "btn-primary",
    secondary = "btn-secondary",
    success = "btn-success",
    warning = "btn-warning",
    danger = "btn-danger"
  )

  button_class <- class_map[[style]] %||% "btn-primary"

  shiny::actionButton(
    inputId = inputId,
    label = label,
    icon = icon,
    class = paste("btn", button_class),
    disabled = disabled,
    style = if (width != "auto") paste0("width: ", width, ";") else NULL
  )
}

#' Opret info panel med consistent styling
#'
#' Genererer et information panel med standard styling for
#' bruger feedback, warnings eller info beskeder.
#'
#' @param content Character string eller HTML content
#' @param type Character string med panel type ("info", "success", "warning", "danger")
#' @param dismissible Logical - om panel kan lukkes (default FALSE)
#' @param icon Optional icon for panel
#'
#' @return Shiny div element med styled panel
#'
#' @examples
#' \dontrun{
#' # Info panel
#' create_info_panel("Data loaded successfully", "success", icon = shiny::icon("check"))
#'
#' # Warning panel
#' create_info_panel("Check your data format", "warning", icon = shiny::icon("warning"))
#' }
#'
#' @family ui_components
#' @export
create_info_panel <- function(content, type = "info", dismissible = FALSE, icon = NULL) {

  # Map types til Bootstrap alert klasser
  class_map <- list(
    info = "alert-info",
    success = "alert-success",
    warning = "alert-warning",
    danger = "alert-danger"
  )

  alert_class <- class_map[[type]] %||% "alert-info"
  dismissible_class <- if (dismissible) " alert-dismissible" else ""

  panel_content <- if (!is.null(icon)) {
    list(icon, " ", content)
  } else {
    content
  }

  shiny::div(
    class = paste0("alert ", alert_class, dismissible_class),
    role = "alert",
    if (dismissible) {
      shiny::tags$button(
        type = "button",
        class = "btn-close",
        `data-bs-dismiss` = "alert",
        `aria-label` = "Close"
      )
    },
    panel_content
  )
}

# Null coalescing operator is defined in utils_logging.R