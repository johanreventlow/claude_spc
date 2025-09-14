# UI HOVEDFIL ================================================

# BIBLIOTEKER OG DEPENDENCIES --------------------------------

library(shiny)
library(bslib)

# MODULER --------------------------------

## Funktionalitetsmoduler (kun UI-komponenter) -----
source("R/visualization_module_ui.R")
source("R/utils_local_storage_js.R")

## UI-komponenter -----
source("R/ui/ui_header.R")
source("R/ui/ui_sidebar.R")
source("R/ui/ui_main_content.R")
source("R/ui/ui_welcome_page.R")

# HOVED-UI STRUKTUR --------------------------------
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
  navbar_options = navbar_options(theme = "light", underline = FALSE),

  # Header-komponenter
  header = create_ui_header(),

  # Sidebar
  sidebar = create_ui_sidebar(),

  # Hovedindhold
  nav_panel(
    title = NULL,
    create_ui_main_content()
  )
)
