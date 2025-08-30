# ui.R
# Main UI definition

library(shiny)
library(bslib)
library(rhandsontable)

source("R/ui/ui_header.R")
source("R/ui/ui_sidebar.R") 
source("R/ui/ui_main_content.R")

# Main UI structure
ui <- page_navbar(
  title = tagList(
    img(
      src = basename(HOSPITAL_LOGO_PATH),
      height = "40px", 
      style = "margin-right: 10px;",
      onerror = "this.style.display='none'"
    ),
    div("BFH SPC-værktøj", style = "position: absolute; right: 20px; top: 20px; font-weight: bold;")
  ),
  theme = my_theme,
  inverse = FALSE,
  
  # Header components
  header = create_ui_header(),
  
  # Sidebar
  sidebar = create_ui_sidebar(),
  
  # Main content
  nav_panel(
    title = NULL,
    create_ui_main_content()
  )
)
