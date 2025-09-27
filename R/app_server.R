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