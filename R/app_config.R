#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @export
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      title = shiny::tagList(
        shiny::img(
          src = get_hospital_logo_path(),
          height = "40px",
          style = "margin-right: 10px;",
          onerror = "this.style.display='none'"
        ),
        shiny::div("BFH SPC-værktøj", style = "position: absolute; right: 20px; top: 20px; font-weight: bold;")
      ),
      theme = get_bootstrap_theme(),
      navbar_options = bslib::navbar_options(theme = "light", underline = FALSE),

      # Header-komponenter
      header = create_ui_header(),

      # Sidebar
      sidebar = create_ui_sidebar(),

      # Hovedindhold
      bslib::nav_panel(
        title = NULL,
        create_ui_main_content()
      )
    )
  )
}

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @export
app_server <- function(input, output, session) {
  # Call the main server function directly (no more file dependencies!)
  main_app_server(input, output, session)
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @import golem
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'claudespc'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#'
#' @param ... character. Path to the file, relative to the app's root directory.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "claudespc")
}

#' Read App Config
#'
#' @param value Name of the value to read
#' @param config Config file to read from. Default is "default"
#' @param use_parent Should the config file inherit from parent?
#'
#' @noRd
get_golem_config <- function(
  value,
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE",
    "default"
  ),
  use_parent = TRUE
) {
  config::get(
    value = value,
    config = config,
    file = app_sys("golem-config.yml"),
    use_parent = use_parent
  )
}# Hook test
