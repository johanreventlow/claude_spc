# R/ui/ui_sidebar.R
# UI sidebar components

create_ui_sidebar <- function() {
  sidebar(
    title = div(
      icon("upload"), 
      " Data upload & konfiguration",
      style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 600;")
    ),
    width = "300px",
    position = "left",
    open = TRUE,
    collapsible = TRUE,
    
    # Upload eller start ny session sektion - ALWAYS VISIBLE
    div(
      style = "margin-bottom: 15px;",
      h6("Vælg handling:", style = "font-weight: 500; margin-bottom: 15px;"),
      
      # Start ny session knap
      actionButton(
        "clear_saved",
        "Start ny session",
        icon = icon("refresh"),
        class = "btn-outline-primary w-100 mb-2",
        title = "Start med tom standardtabel"
      ),
      
      # Upload fil knap - opens modal
      actionButton(
        "show_upload_modal",
        "Upload datafil",
        icon = icon("upload"),
        class = "btn-outline-secondary w-100",
        title = "Upload Excel eller CSV fil"
      )
    ),

    # REST OF SIDEBAR - only when data is loaded
    conditionalPanel(
      condition = "output.dataLoaded == 'TRUE'",
      
      hr(),

      # Indikator metadata
      textInput(
        "indicator_title",
        "Titel på indikator:",
        value = "",
        placeholder = "F.eks. 'Infektioner pr. 1000 sengedage'"
      ),

      # Organisatorisk enhed
      create_unit_selection(),

      # Beskrivelse
      textAreaInput(
        "indicator_description",
        "Beskrivelse:",
        value = "",
        placeholder = "Beskriv kort hvad indikatoren måler, hvordan data indsamles, og hvad målsætningen er...",
        height = "100px",
        resize = "vertical"
      ),

      hr(),

      # Session management
      div(
        h6("Session:", style = "font-weight: 500; margin-bottom: 10px;"),
        actionButton(
          "manual_save",
          "Gem session",
          icon = icon("save"),
          class = "btn-outline-primary btn-sm w-100"
        ),
        div(
          id = "save_status",
          style = "margin-top: 8px; font-size: 0.8rem; color: #666;",
          uiOutput("save_status_display")
        )
      ),

      hr(),

      # Data status
      div(
        uiOutput("data_status_display")
      )
    )
  )
}

create_unit_selection <- function() {
  div(
    style = "margin-bottom: 15px;",
    tags$label("Afdeling eller afsnit", style = "font-weight: 500;"),
    div(
      style = "margin-top: 5px;",
      radioButtons(
        "unit_type",
        NULL,
        choices = list(
          "Vælg fra liste" = "select",
          "Indtast selv" = "custom"
        ),
        selected = "select",
        inline = TRUE
      )
    ),

    # Dropdown for standard enheder
    conditionalPanel(
      condition = "input.unit_type == 'select'",
      selectInput(
        "unit_select",
        NULL,
        choices = list(
          "Vælg enhed..." = "",
          "Medicinsk Afdeling" = "med",
          "Kirurgisk Afdeling" = "kir",
          "Intensiv Afdeling" = "icu",
          "Ambulatorie" = "amb",
          "Akutmodtagelse" = "akut",
          "Pædiatrisk Afdeling" = "paed",
          "Gynækologi/Obstetrik" = "gyn"
        )
      )
    ),

    # Custom input
    conditionalPanel(
      condition = "input.unit_type == 'custom'",
      textInput(
        "unit_custom",
        NULL,
        placeholder = "Indtast enhedsnavn..."
      )
    )
  )
}
