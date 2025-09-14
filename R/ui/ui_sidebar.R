# ui_sidebar.R
# UI sidebar komponenter

# Dependencies ----------------------------------------------------------------

# UI SIDEBAR KOMPONENTER ======================================================

## Hovedfunktion for UI sidebar
# Opretter komplet sidebar med data upload og konfiguration
create_ui_sidebar <- function() {
  sidebar(
    # title = div(
    #   icon("upload"),
    #   " Data upload & konfiguration",
    #   style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 600;")
    # ),
    width = "300px",
    position = "left",
    open = TRUE,
    collapsible = TRUE,

    # Upload eller start ny session sektion - ALTID SYNLIG
    # div(
    # style = "margin-bottom: 15px;",
    # h6("Vælg handling:", style = "font-weight: 500; margin-bottom: 15px;"),

    # Start ny session knap
    actionButton(
      "clear_saved",
      "Start ny session",
      icon = icon("refresh"),
      class = "btn-primary w-100 mb-2",
      title = "Start med tom standardtabel"
    ),

    # Upload fil knap - åbner modal
    actionButton(
      "show_upload_modal",
      "Upload datafil",
      icon = icon("upload"),
      class = "btn-secondary w-100",
      title = "Upload Excel eller CSV fil"
      # )
    ),

    # REST OF SIDEBAR - only when data is loaded
    # conditionalPanel(
    #   condition = "output.dataLoaded == 'TRUE'",
    #
    #   hr(),
    #
    #
    #
    #
    #
    #   # hr(),
    #
    #   # Session management
    #   div(
    #     h6("Session:", style = "font-weight: 500; margin-bottom: 10px;"),
    #     actionButton(
    #       "manual_save",
    #       "Gem session",
    #       icon = icon("save"),
    #       class = "btn-outline-primary btn-sm w-100"
    #     ),
    #     div(
    #       id = "save_status",
    #       style = "margin-top: 8px; font-size: 0.8rem; color: #666;",
    #       uiOutput("save_status_display")
    #     )
    #   ),
    #
    #   hr(),
    #
    #   # Data status
    #   div(
    #     uiOutput("data_status_display")
    #   )
    # )
  )
}
