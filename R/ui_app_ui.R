# app_ui.R
# Consolidated UI components for SPC app

# UI components now loaded globally in global.R for better performance

# UI HEADER KOMPONENTER =======================================================

## Hovedfunktion for UI header
# Opretter alle header komponenter inklusive scripts og styles
#' @export
create_ui_header <- function() {
  # Get hospital colors using the proper package function
  hospital_colors <- get_hospital_colors()

  shiny::tagList(
    # Aktivér shinyjs
    shinyjs::useShinyjs(),
    shiny::tags$head(
      # CSS files for plot debugging
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "plot-debug.css"),

      # JavaScript files
      shiny::tags$script(src = "local-storage.js"),
      shiny::tags$script(src = "ui-helpers.js"),
      shiny::tags$script(src = "shiny-handlers.js"),
      # Inline CSS styles
      shiny::tags$style(htmltools::HTML(paste0("

      /* Navigation and Tab Styling */
    .nav-link {
      padding: .5rem 1rem !important;
    }

    /* Tab styling - ikke-aktive tabs */
    .nav-tabs .nav-link:not(.active) {
      color: #009ce8 !important;
    }

    /* Tab styling - aktive tabs (behold standard) */
    .nav-tabs .nav-link.active {
      color: inherit;
    }


    /* --- Excel-lignende tema til excelR --- */
    .jexcel_container {
      /*font-family: Calibri, 'Segoe UI', Arial, sans-serif;
      font-size: 13px;*/
      width: none !important;
      height: auto !important;
      padding-bottom: 25px !important;
      position: none !important;
      border: none !important;
    }

    .jexcel thead td {
      background: #f3f3f3;
      border-bottom: 2px solid #bfbfbf;
      font-weight: 600;
      white-space: nowrap;
    }

    /* Excel-lignende styling */
    .jexcel tbody tr:nth-child(odd) {
      background-color: #f9f9f9;
    }

    .jexcel tbody tr:nth-child(even) {
      background-color: #ffffff;
    }

    .jexcel tbody tr:hover {
      background-color: #f0f8ff !important;
    }

    .jexcel td {
      border: 1px solid #d9d9d9;
      padding: 4px 8px;
    }

    /* Aktiv celle styling */
    .jexcel .highlight {
      background-color: #cce7ff !important;
      border: 2px solid #0066cc !important;
    }

    .jexcel_content {
      overflow-y: unset !important;
      max-height: none !important;
      margin-bottom: 25px !important;
    }

    .jexcel > thead > tr > td {
      position: unset !important;
    }

    /* Neutraliser bslib spacing omkring textarea wrapper */
    .bslib-grid:has(#indicator-description-wrapper) {
      margin-bottom: 0 !important;
      padding-bottom: 0 !important;
    }

    .bslib-mb-spacing:has(#indicator-description-wrapper) {
      margin-bottom: 0 !important;
    }



    /* Parent container skal være fleksibel */
    #indicator-description-wrapper {
      display: flex !important;
      flex-direction: column !important;
      flex: 1 1 auto !important;
      min-height: 0 !important;
      margin-bottom: 0 !important;
      padding-bottom: 0 !important;
    }

    /* Textarea skal fylde tilgængelig højde */
    #indicator_description {
      flex: 1 1 auto !important;
      min-height: 130px !important;
      height: 100% !important;
      resize: none !important;
      overflow: auto !important;
      margin-bottom: 0 !important;
    }

    /* Fjern margin på form-group omkring textarea */
    #indicator-description-wrapper .form-group,
    #indicator_div {
      margin-bottom: 0 !important;
      flex: 1 1 auto !important;
      display: flex !important;
      flex-direction: column !important;
    }

    /* Selectize dropup styling */
    .selectize-dropup .selectize-control .selectize-dropdown {
      position: absolute !important;
      top: auto !important;
      bottom: 100% !important;
      border-top: 1px solid #d0d7de !important;
      border-bottom: none !important;
      border-radius: 4px 4px 0 0 !important;
      box-shadow: 0 -2px 8px rgba(0, 0, 0, 0.1) !important;
      margin-bottom: 2px !important;
    }

    .selectize-dropup {
      position: relative !important;
    }

    .selectize-dropup .selectize-control {
      position: relative !important;
    }

    .selectize-dropdown {
      max-height: 200px !important;
      overflow-y: auto !important;
      z-index: 1050 !important;
    }

    /* Dynamic hospital color styles that need R variables */
    .status-ready { background-color: ", hospital_colors$success, "; }
    .status-warning { background-color: ", hospital_colors$warning, "; }
    .status-error { background-color: ", hospital_colors$danger, "; }
    .status-processing { background-color: ", hospital_colors$primary, "; }




        ")))
    )
  )
}
# R/ui/ui_main_content.R
# Main content area components

#' @export
create_ui_main_content <- function() {
  shiny::tagList(
    # Welcome page when no meaningful data is loaded - DISABLED FOR DEVELOPMENT
    # shiny::conditionalPanel(condition = "output.dataLoaded != 'TRUE'", create_welcome_page()),

    # Main content in 2x2 grid layout - ALWAYS VISIBLE FOR DEVELOPMENT
    bslib::layout_columns(
      col_widths = c(6, 6, 6, 6),
      height = "auto",
      max_height = "100%",
      create_plot_only_card(),
      create_status_value_boxes(),
      create_data_table_card(),
      create_chart_settings_card()
    )
  )
}




create_chart_settings_card <- function() {
  bslib::navset_card_tab(
    title = shiny::span(shiny::icon("sliders-h"), " Indstillinger", ),
    full_screen = TRUE,
    height = "calc(50vh - 60px)",
    # Tab 1: Detaljer ----
    bslib::nav_panel(
      max_height = "100%",
      min_height = "100%",
      title = "Detaljer",
      icon = shiny::icon("pen-to-square"),
      # Chart type and target value side by side
      bslib::layout_column_wrap(
        width = 1 / 2,
        shiny::div(
          id = "indicator_div",
          # Indikator metadata
          shiny::textInput(
            "indicator_title",
            "Titel på indikator:",
            width = UI_INPUT_WIDTHS$full,
            value = "",
            placeholder = "F.eks. 'Infektioner pr. 1000 sengedage'"
          ),
          bslib::layout_column_wrap(
            width = UI_LAYOUT_PROPORTIONS$half,

            # Target value input
            shiny::textInput(
              "target_value",
              "Udviklingsmål:",
              value = "",
              placeholder = "fx >=90%, <25 eller >",
              width = UI_INPUT_WIDTHS$full
            ),

            # Centerline input
            shiny::textInput(
              "centerline_value",
              "Evt. baseline:",
              value = "",
              placeholder = "fx 68%, 0,7 el. 22",
              width = UI_INPUT_WIDTHS$full
            )
          ),

          # Beskrivelse
          shiny::div(
            id = "indicator-description-wrapper",
            style = "display: flex; flex-direction: column; flex: 1 1 auto; min-height: 0;",
            shiny::textAreaInput(
              "indicator_description",
              "Datadefinition:",
              value = "",
              placeholder = "Angiv kort, hvad indikatoren udtrykker, og hvordan data opgøres – fx beregning af tæller og nævner.",
              resize = "none",
              width = UI_INPUT_WIDTHS$full,
            )
          ),
        ),
        shiny::div(
          # Chart type selection
          shiny::selectizeInput(
            "chart_type",
            "Diagram type:",
            choices = CHART_TYPES_DA,
            selected = "run"
          ),


          # Y-axis UI type (simpel datamodel)
          shiny::selectizeInput(
            "y_axis_unit",
            "Y-akse enhed:",
            choices = Y_AXIS_UI_TYPES_DA,
            selected = "count"
          )
        )
      )
    ),

    # Tab 2: Column mapping -----
    bslib::nav_panel(
      "Kolonnematch",
      icon = shiny::icon("columns"),
      bslib::layout_column_wrap(
        width = 1 / 2,
        shiny::div(
          # X-axis column
          shiny::selectizeInput(
            "x_column",
            "X-akse (vandret tids-/observationsakse):",
            choices = NULL,
            selected = NULL
          ),

          # Y-axis column
          shiny::selectizeInput(
            "y_column",
            "Y-akse (lodret værdiakse):",
            choices = NULL,
            selected = NULL
          ),

          # N column - wrapped for dropup behavior
          shiny::div(
            class = "selectize-dropup",
            shiny::selectizeInput(
              "n_column",
              shiny::span(
                "Nævner (n):",
                shiny::icon("info-circle"),
                shiny::span(
                  id = "n_column_ignore_tt",
                  style = "display: none; margin-left: 6px; color: #6c757d;",
                  shiny::icon("circle-info")
                ) |>
                  bslib::tooltip("Ignoreres for denne type")
              ),
              choices = NULL,
              selected = NULL
            ) |>
              bslib::tooltip("Run: valgfri nævner. P, P′, U, U′: kræver nævner. I, MR, C, G: nævner ignoreres.")
          ),
          # Hint vises når diagramtype ikke anvender nævner
          shiny::div(
            id = "n_column_hint",
            class = "text-muted",
            style = "display: none; font-size: 0.85rem; margin-top: 4px;",
            shiny::icon("circle-info"),
            shiny::HTML("&nbsp;Nævner ignoreres for den valgte diagramtype.")
          )
        ),
        shiny::div(
          # Skift column
          shiny::selectizeInput(
            "skift_column",
            shiny::span("Opdel proces:", shiny::icon("info-circle")),
            choices = NULL,
            selected = NULL
          ),

          # Frys column
          shiny::selectizeInput(
            "frys_column",
            shiny::span("Fastfrys niveau:", shiny::icon("info-circle")),
            choices = NULL,
            selected = NULL
          ),

          # Kommentar column
          shiny::div(
            class = "selectize-dropup",
            shiny::selectizeInput(
              "kommentar_column",
              shiny::span("Kommentar (noter):", shiny::icon("info-circle")),
              choices = NULL,
              selected = NULL
            ) |>
              bslib::tooltip("Valgfri: Kolonne med kommentarer eller noter til datapunkter")
          ),
          shiny::div(
            style = "padding: 10px 0;",
            shiny::div(
              class = "text-center",
              # Auto-detect button
              shiny::actionButton(
                "auto_detect_columns",
                "Auto-detektér kolonner",
                icon = shiny::icon("magic"),
                class = "btn-secondary btn-sm w-100",
                style = "margin-top: 25px;"
              )
            ),
          )
        ),

        # Column validation feedback
        # shiny::div(
        #   id = "column_validation",
        #   style = "margin-top: 10px;",
        #   shiny::uiOutput("column_validation_messages")
        # )
      )
    ),

    # Tab 3: Organisatorisk enhed ----
    bslib::nav_panel(
      "Organisatorisk",
      icon = shiny::icon("building"),
      max_height = "100%",
      min_height = "100%",
      shiny::div(
        style = "padding: 10px 0;",
        # Organisatorisk enhed selection
        create_unit_selection()
      )
    ),

    # Tab 4: Additional settings (placeholder) ----
    # bslib::nav_panel(
    #   "Avanceret",
    #   icon = shiny::icon("cogs"),
    #   max_height = "100%",
    #   min_height = "100%",
    #
    #   shiny::div(
    #     style = "padding: 20px; text-align: center; color: #666;",
    #     shiny::icon("wrench", style = "font-size: 2rem; margin-bottom: 10px;"),
    #     shiny::br(),
    #     "Yderligere indstillinger kommer her",
    #     shiny::br(),
    #     shiny::tags$small("Denne tab er reserveret til fremtidige features")
    #   )
    # ) # bslib::nav_panel(Avanceret)
  ) # navset_card_tab
}


# New function for plot-only card

create_plot_only_card <- function() {
  bslib::card(
    full_screen = TRUE,
    fillable = TRUE,
    max_height = "100%",
    # min_height = "100%",
    min_height = "calc(50vh - 60px)",
    bslib::card_header(
      shiny::div(shiny::icon("chart-line"), " SPC Preview")
    ),
    bslib::card_body(
      fill = TRUE,
      shiny::div(
        style = "height: 100%",
        visualizationModuleUI("visualization")
      )
    )
  )
}

create_data_table_card <- function() {
  bslib::card(
    full_screen = TRUE,
    min_height = "calc(50vh - 60px)",
    bslib::card_header(
      shiny::div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        shiny::div(shiny::icon("table"), " Data", ),
        shiny::div(
          class = "btn-group-sm",
          shiny::actionButton(
            "edit_column_names",
            label = "Redigér kolonnenavne",
            icon = shiny::icon("edit"),
            title = "Redigér kolonnenavne",
            class = "btn-secondary btn-sm"
          ),
          shiny::actionButton(
            "add_column",
            label = "Tilføj kolonne",
            icon = shiny::icon("plus"),
            title = "Tilføj kolonne",
            class = "btn-secondary btn-sm"
          ),
          shiny::actionButton(
            "add_row",
            label = "Tilføj række",
            icon = shiny::icon("plus-square"),
            title = "Tilføj række",
            class = "btn-secondary btn-sm"
          ),
          # shiny::actionButton(
          #   "reset_table",
          #   label = NULL,
          #   icon = shiny::icon("refresh"),
          #   title = "Reset tabel",
          #   class = "btn-outline-warning btn-sm"
          # )
        )
      )
    ),
    bslib::card_body(
      # Data table using excelR
      excelR::excelOutput("main_data_table", height = "auto")
    )
  )
}


# New function for status value boxes
create_status_value_boxes <- function() {
  visualizationStatusUI("visualization")
}

create_unit_selection <- function() {
  shiny::div(
    style = "margin-bottom: 15px;",
    shiny::tags$label("Afdeling eller afsnit", style = "font-weight: 500;"),
    shiny::div(
      style = "margin-top: 5px;",
      shiny::radioButtons(
        "unit_type",
        NULL,
        choices = list("Vælg fra liste" = "select", "Indtast selv" = "custom"),
        selected = "select",
        inline = TRUE
      )
    ),

    # Dropdown for standard enheder
    shiny::conditionalPanel(condition = "input.unit_type == 'select'", shiny::selectizeInput(
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
    )),

    # Custom input
    shiny::conditionalPanel(
      condition = "input.unit_type == 'custom'",
      shiny::textInput("unit_custom", NULL, placeholder = "Indtast enhedsnavn...")
    )
  )
}

create_export_card <- function() {
  shiny::conditionalPanel(condition = "output.plot_ready == 'true'", bslib::card(
    bslib::card_header(shiny::div(shiny::icon("download"), " Eksport", )),
    bslib::card_body(
      # KOMPLET EXPORT - Excel version
      shiny::div(
        shiny::downloadButton(
          "download_complete_excel",
          "📋 Komplet Export (Excel)",
          icon = shiny::icon("file-excel"),
          title = "Download hele sessionen som Excel fil med data og konfiguration",
          class = "btn-success w-100 mb-2"
        ),

        # Hjælpe-tekst for komplet export
        shiny::div(
          style = "font-size: 0.75rem; color: #666; text-align: center; margin-bottom: 8px; font-style: italic;",
          "Data + metadata i 2 Excel sheets - klar til brug og re-import"
        )
      ),
      shiny::hr(style = "margin: 15px 0;"),
      shiny::div(style = "text-align: center; font-size: 0.85rem; color: #666; margin-bottom: 10px;", shiny::strong("Graf eksporter:")),
      shiny::downloadButton(
        "download_png",
        "Download PNG",
        icon = shiny::icon("image"),
        class = "btn-outline-primary w-100 mb-2"
      ),
      shiny::downloadButton(
        "download_pdf",
        "Download PDF Rapport",
        icon = shiny::icon("file-pdf"),
        class = "btn-outline-primary w-100"
      )
    )
  ))
}
# ui_sidebar.R
# UI sidebar komponenter

# Dependencies ----------------------------------------------------------------

# UI SIDEBAR KOMPONENTER ======================================================

## Hovedfunktion for UI sidebar
# Opretter komplet sidebar med data upload og konfiguration
#' @export
create_ui_sidebar <- function() {
  bslib::sidebar(
    # title = shiny::div(
    #   shiny::icon("upload"),
    #   " Data upload & konfiguration",
    #   style = paste("color:", HOSPITAL_COLORS$primary, "; font-weight: 600;")
    # ),
    width = "300px",
    position = "left",
    open = TRUE,
    collapsible = TRUE,

    # Upload eller start ny session sektion - ALTID SYNLIG
    # shiny::div(
    # style = "margin-bottom: 15px;",
    # shiny::h6("Vælg handling:", style = "font-weight: 500; margin-bottom: 15px;"),

    # Start ny session knap
    shiny::actionButton(
      "clear_saved",
      "Start ny session",
      icon = shiny::icon("refresh"),
      class = "btn-primary w-100 mb-2",
      title = "Start med tom standardtabel"
    ),

    # Upload fil knap - åbner modal
    shiny::actionButton(
      "show_upload_modal",
      "Upload datafil",
      icon = shiny::icon("upload"),
      class = "btn-secondary w-100",
      title = "Upload Excel eller CSV fil"
      # )
    ),

    # REST OF SIDEBAR - only when data is loaded
    # shiny::conditionalPanel(
    #   condition = "output.dataLoaded == 'TRUE'",
    #
    #   shiny::hr(),
    #
    #
    #
    #
    #
    #   # shiny::hr(),
    #
    #   # Session management
    #   shiny::div(
    #     shiny::h6("Session:", style = "font-weight: 500; margin-bottom: 10px;"),
    #     shiny::actionButton(
    #       "manual_save",
    #       "Gem session",
    #       icon = shiny::icon("save"),
    #       class = "btn-outline-primary btn-sm w-100"
    #     ),
    #     shiny::div(
    #       id = "save_status",
    #       style = "margin-top: 8px; font-size: 0.8rem; color: #666;",
    #       shiny::uiOutput("save_status_display")
    #     )
    #   ),
    #
    #   shiny::hr(),
    #
    #   # Data status
    #   shiny::div(
    #     shiny::uiOutput("data_status_display")
    #   )
    # )
  )
}
# ui_welcome_page.R
# UI komponenter for velkomstside

# Dependencies ----------------------------------------------------------------

# UI VELKOMSTSIDE KOMPONENTER =================================================

## Hovedfunktion for velkomstside
# Opretter komplet velkomstside med hero sektion og handlingsknapper
#' @export
create_welcome_page <- function() {
  # Get hospital colors using the proper package function
  hospital_colors <- get_hospital_colors()

  shiny::div(
    class = "welcome-page",
    style = "min-height: 100vh; background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);",

    # Hero Sektion
    shiny::div(
      class = "hero-section py-5",
      shiny::div(
        class = "container-fluid",
        shiny::div(
          class = "row align-items-center mb-5",
          shiny::div(
            class = "col-12 text-center",
            shiny::h1(
              class = "display-4 fw-bold",
              style = paste0("color: ", hospital_colors$primary, "; margin-bottom: 1rem;"),
              "Velkommen til BFH SPC-værktøj"
            ),
            shiny::p(
              class = "lead text-muted",
              style = "font-size: 1.25rem; max-width: 800px; margin: 0 auto;",
              "Transformér dine data til indsigter med Statistical Process Control. ",
              "Identificer mønstre, spot trends og træf bedre beslutninger baseret på dine healthcare data."
            )
          )
        )
      )
    ),

    # Main Content - Two Column Layout
    shiny::div(
      class = "container-fluid px-4",
      shiny::div(
        class = "row g-4 mb-5",

        # LEFT COLUMN - Getting Started Guide
        shiny::div(
          class = "col-lg-6",
          create_getting_started_card()
        ),

        # RIGHT COLUMN - Understanding SPC
        shiny::div(
          class = "col-lg-6",
          create_understanding_spc_card()
        )
      )
    ),

    # Call to Action Section
    shiny::div(
      class = "cta-section py-5",
      style = paste0("background-color: ", hospital_colors$primary, "; color: white;"),
      shiny::div(
        class = "container text-center",
        shiny::div(
          class = "row",
          shiny::div(
            class = "col-lg-8 mx-auto",
            shiny::h2(class = "mb-4", "Klar til at komme i gang?"),
            shiny::p(class = "mb-4 fs-5", "Start din første SPC-analyse på under 5 minutter."),
            shiny::div(
              class = "d-grid gap-2 d-md-flex justify-content-md-center",
              shiny::actionButton(
                "start_new_session",
                "🚀 Start ny analyse",
                class = "btn btn-light btn-lg me-md-2",
                style = "font-weight: 600; padding: 12px 30px;"
              ),
              shiny::actionButton(
                "upload_data_welcome",
                "Upload data",
                class = "btn btn-outline-light btn-lg",
                style = "font-weight: 600; padding: 12px 30px;"
              )
            )
          )
        )
      )
    )
  )
}

# Left Column - Getting Started Guide
create_getting_started_card <- function() {
  # Get hospital colors using the proper package function
  hospital_colors <- get_hospital_colors()
  bslib::card(
    class = "h-100 shadow-sm",
    style = "border: none; border-radius: 15px;",
    bslib::card_header(
      class = "bg-white border-0 pb-0",
      style = "border-radius: 15px 15px 0 0;",
      shiny::div(
        class = "d-flex align-items-center",
        shiny::div(
          class = "me-3",
          style = paste0("background: ", hospital_colors$primary, "; width: 50px; height: 50px; border-radius: 12px; display: flex; align-items: center; justify-content: center;"),
          shiny::icon("rocket", style = "color: white; font-size: 1.5rem;")
        ),
        shiny::div(
          shiny::h3(class = "card-title mb-1", "Kom i gang på 3 trin"),
          shiny::p(class = "text-muted mb-0", "Din vej fra data til indsigt")
        )
      )
    ),
    bslib::card_body(
      class = "px-4 py-4",

      # Step 1
      create_step_item(
        number = "1",
        icon = "upload",
        title = "Upload dine data",
        description = "Excel (.xlsx/.xls) eller CSV-fil med kolonneoverskrifter. Vi understøtter danske tal og datoformater.",
        example = "Eksempel: Dato, Tæller, Nævner, Kommentar"
      ),

      # Step 2
      create_step_item(
        number = "2",
        icon = "sliders-h",
        title = "Konfigurer din analyse",
        description = "Automatisk kolonnedetektering eller manuel opsætning. Vælg chart type baseret på dine data.",
        example = "Run chart, P-chart, U-chart, X̄-chart"
      ),

      # Step 3
      create_step_item(
        number = "3",
        icon = "chart-line",
        title = "Få dine insights",
        description = "Interaktiv SPC-graf med centerlinjer, kontrolgrænser og specialle mønstre (Anhøj regler).",
        example = "Eksporter som Excel, PDF eller PNG"
      ),

      # Quick Start Button
      shiny::div(
        class = "mt-4 pt-3 border-top",
        shiny::div(
          class = "d-grid",
          shiny::actionButton(
            "quick_start_demo",
            "👆 Prøv med eksempel-data",
            class = "btn btn-outline-primary btn-lg",
            style = "border-radius: 10px; font-weight: 500;"
          )
        ),
        shiny::p(
          class = "text-center text-muted mt-2 mb-0",
          style = "font-size: 0.9rem;",
          "Eller upload dine egne data direkte"
        )
      )
    )
  )
}

# Right Column - Understanding SPC
create_understanding_spc_card <- function() {
  # Get hospital colors using the proper package function
  hospital_colors <- get_hospital_colors()
  bslib::card(
    class = "h-100 shadow-sm",
    style = "border: none; border-radius: 15px;",
    bslib::card_header(
      class = "bg-white border-0 pb-0",
      style = "border-radius: 15px 15px 0 0;",
      shiny::div(
        class = "d-flex align-items-center",
        shiny::div(
          class = "me-3",
          style = paste0("background: ", hospital_colors$secondary, "; width: 50px; height: 50px; border-radius: 12px; display: flex; align-items: center; justify-content: center;"),
          shiny::icon("lightbulb", style = "color: white; font-size: 1.5rem;")
        ),
        shiny::div(
          shiny::h3(class = "card-title mb-1", "Forstå SPC"),
          shiny::p(class = "text-muted mb-0", "Værktøjet der transformerer data til handling")
        )
      )
    ),
    bslib::card_body(
      class = "px-4 py-4",

      # What is SPC?
      create_info_section(
        icon = "question-circle",
        title = "Hvad er Statistical Process Control?",
        content = "SPC hjælper dig med at skelne mellem normal variation og særlige årsager i dine processer. I sundhedsvæsenet betyder det bedre patientpleje gennem data-drevet beslutningstagning."
      ),

      # Why SPC in Healthcare?
      create_info_section(
        icon = "heartbeat",
        title = "Hvorfor SPC i sundhedsvæsenet?",
        content = htmltools::HTML("
          <ul class='list-unstyled'>
            <li><strong>🎯 Spot trends tidligt:</strong> Identificer problemer før de bliver kritiske</li>
            <li><strong>Forstå variation:</strong> Normal udsving vs. særlige årsager</li>
            <li><strong>💡 Træf bedre beslutninger:</strong> Baseret på statistisk evidens</li>
            <li><strong>🚀 Forbedre kontinuerligt:</strong> Måle effekt af ændringer</li>
          </ul>
        ")
      ),

      # Healthcare Examples
      create_info_section(
        icon = "hospital",
        title = "Konkrete eksempler fra BFH",
        content = htmltools::HTML("
          <div class='row g-2'>
            <div class='col-6'>
              <div class='example-item p-2 rounded' style='background: #f8f9fa;'>
                <small class='fw-bold text-primary'>Infektionsrater</small><br>
                <small class='text-muted'>Monitor og reducer HAI</small>
              </div>
            </div>
            <div class='col-6'>
              <div class='example-item p-2 rounded' style='background: #f8f9fa;'>
                <small class='fw-bold text-primary'>Ventetider</small><br>
                <small class='text-muted'>Optimér patientflow</small>
              </div>
            </div>
            <div class='col-6'>
              <div class='example-item p-2 rounded mt-2' style='background: #f8f9fa;'>
                <small class='fw-bold text-primary'>Medicinfejl</small><br>
                <small class='text-muted'>Forbedre patientsikkerhed</small>
              </div>
            </div>
            <div class='col-6'>
              <div class='example-item p-2 rounded mt-2' style='background: #f8f9fa;'>
                <small class='fw-bold text-primary'>Genindlæggelser</small><br>
                <small class='text-muted'>Kvalitetsindikatorer</small>
              </div>
            </div>
          </div>
        ")
      )
    )
  )
}

# Helper function for step items
create_step_item <- function(number, icon, title, description, example = NULL) {
  # Get hospital colors using the proper package function
  hospital_colors <- get_hospital_colors()
  shiny::div(
    class = "step-item d-flex mb-4",
    # Step Number Circle
    shiny::div(
      class = "step-number me-3 flex-shrink-0",
      style = paste0("width: 40px; height: 40px; background: ", hospital_colors$primary, "; color: white; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-weight: bold; font-size: 1.1rem;"),
      number
    ),
    # Step Content
    shiny::div(
      class = "step-content flex-grow-1",
      shiny::div(
        class = "d-flex align-items-center mb-2",
        shiny::icon(icon, class = "text-primary me-2"),
        shiny::h5(class = "mb-0 fw-semibold", title)
      ),
      shiny::p(class = "text-muted mb-1", description),
      if (!is.null(example)) {
        shiny::p(
          class = "small text-primary mb-0",
          style = "font-style: italic;",
          example
        )
      }
    )
  )
}

# Helper function for info sections
create_info_section <- function(icon, title, content) {
  shiny::div(
    class = "info-section mb-4",
    shiny::div(
      class = "d-flex align-items-start mb-2",
      shiny::div(
        class = "me-2 flex-shrink-0",
        shiny::icon(icon, class = "text-secondary", style = "font-size: 1.2rem; margin-top: 2px;")
      ),
      shiny::div(
        class = "flex-grow-1",
        shiny::h5(class = "fw-semibold mb-2", title),
        shiny::div(class = "text-muted", content)
      )
    )
  )
}
