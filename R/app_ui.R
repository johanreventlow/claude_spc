# app_ui.R  
# Consolidated UI components for SPC app

# Dependencies ----------------------------------------------------------------
source("R/mod_spc_chart.R")
source("R/mod_session_storage.R")

# UI HEADER KOMPONENTER =======================================================

## Hovedfunktion for UI header
# Opretter alle header komponenter inklusive scripts, styles og waiter
create_ui_header <- function() {
  tagList(
    waiter::use_waiter(),
    # Aktivér shinyjs
    shinyjs::useShinyjs(),
    tags$head(
      tags$script(HTML("
      $(document).ready(function() {
        // Funktion til korrekt at vise UI når waiter skjules
        window.showAppUI = function() {
          setTimeout(function() {
            $('body').css('opacity', '1');
          }, 100); // Small delay to ensure smooth transition
        };
      });

      // Add to existing JavaScript section
      Shiny.addCustomMessageHandler('showAppUI', function(message) {
        window.showAppUI();
      });
    ")),
      tags$script(HTML(localStorage_js)),
      tags$script(HTML("
      // FIKSET: Tilpassede besked handlers med bedre fejlhåndtering
      Shiny.addCustomMessageHandler('saveAppState', function(message) {
        console.log('Received saveAppState message:', message);
        var success = window.saveAppState(message.key, message.data);
        if (!success) {
          console.error('saveAppState failed for key:', message.key);
        }
      });

      Shiny.addCustomMessageHandler('loadAppState', function(message) {
        console.log('Received loadAppState message:', message);
        var data = window.loadAppState(message.key);
        Shiny.setInputValue('loaded_app_state', data, {priority: 'event'});
      });

      Shiny.addCustomMessageHandler('clearAppState', function(message) {
        console.log('Received clearAppState message:', message);
        var success = window.clearAppState(message.key);
        if (!success) {
          console.error('clearAppState failed for key:', message.key);
        }
      });

      // OPDATERET: Auto-indlæs eksisterende data ved app start
      if (window.hasAppState('current_session')) {
        setTimeout(function() {
          console.log('Auto-loading saved session data...');
          var data = window.loadAppState('current_session');
          if (data) {
            console.log('Found saved session data, triggering auto-restore');
            Shiny.setInputValue('auto_restore_data', data, {priority: 'event'});
          }
        }, 500); // Reduced delay since no waiter
      }

      // Selectize dropup behavior
      $(document).ready(function() {
        // Force dropup behavior for selectize inputs in .selectize-dropup containers
        $('.selectize-dropup').find('select').each(function() {
          if (this.selectize) {
            // Override the dropdown positioning
            var selectize = this.selectize;
            var originalSetup = selectize.setup;
            selectize.setup = function() {
              originalSetup.call(this);
              // Force dropdown to open upward
              this.$dropdown.addClass('dropup-forced');
            };
          }
        });
      });
    ")),
      tags$style(HTML(
        paste0("
       .nav-link {padding: .5rem 1rem !important}

       /* Tab styling - ikke-aktive tabs */
       .nav-tabs .nav-link:not(.active) {
         color: #009ce8 !important;
       }

       /* Tab styling - aktive tabs (behold standard) */
       .nav-tabs .nav-link.active {
         color: inherit;
       }

       .status-ready { background-color: ", HOSPITAL_COLORS$success, "; }
       .status-warning { background-color: ", HOSPITAL_COLORS$warning, "; }
       .status-error { background-color: ", HOSPITAL_COLORS$danger, "; }
       .status-processing { background-color: ", HOSPITAL_COLORS$primary, "; }


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
      ")
      ))
    )
  )
}
# R/ui/ui_main_content.R
# Main content area components

create_ui_main_content <- function() {
  tagList(
    # Welcome page when no meaningful data is loaded
    conditionalPanel(condition = "output.dataLoaded != 'TRUE'", create_welcome_page()),

    # Data table and visualization - only when user has started
    conditionalPanel(
      condition = "output.dataLoaded == 'TRUE'",


      # Main content in 2x2 grid layout
      layout_columns(
        col_widths = c(6, 6, 6, 6),
        height = "auto",
        max_height = "100%",
        create_plot_only_card(),
        create_status_value_boxes(),
        create_data_table_card(),
        create_chart_settings_card()
      )
    )
  )
}




create_chart_settings_card <- function() {
  navset_card_tab(
    title = span(icon("sliders-h"), " Indstillinger", ),
    full_screen = TRUE,
    height = "calc(50vh - 60px)",
    # Tab 1: Detaljer ----
    nav_panel(
      max_height = "100%",
      min_height = "100%",
      title = "Detaljer",
      icon = icon("pen-to-square"),
      # Chart type and target value side by side
      layout_column_wrap(
        width = 1 / 2,
        div(
          id = "indicator_div",
          # Indikator metadata
          textInput(
            "indicator_title",
            "Titel på indikator:",
            width = UI_INPUT_WIDTHS$full,
            value = "",
            placeholder = "F.eks. 'Infektioner pr. 1000 sengedage'"
          ),
          layout_column_wrap(
            width = UI_LAYOUT_PROPORTIONS$half,

            # Target value input
            textInput(
              "target_value",
              "Målværdi:",
              value = "",
              placeholder = "fx 80%, 0,8 el. 25",
              width = UI_INPUT_WIDTHS$full
            ),

            # Centerline input
            textInput(
              "centerline_value",
              "Evt. baseline:",
              value = "",
              placeholder = "fx 68%, 0,7 el. 22",
              width = UI_INPUT_WIDTHS$full
            )
          ),

          # Beskrivelse
          div(
            id = "indicator-description-wrapper",
            style = "display: flex; flex-direction: column; flex: 1 1 auto; min-height: 0;",
            textAreaInput(
              "indicator_description",
              "Datadefinition:",
              value = "",
              placeholder = "Angiv kort, hvad indikatoren udtrykker, og hvordan data opgøres – fx beregning af tæller og nævner.",
              resize = "none",
              width = UI_INPUT_WIDTHS$full,
            )
          ),
        ),
        div(
          # Chart type selection
          selectizeInput(
            "chart_type",
            "Diagram type:",
            choices = CHART_TYPES_DA,
            selected = "run"
          ),


          # Y-axis unit selection
          selectizeInput(
            "y_axis_unit",
            "Y-akse enhed:",
            choices = Y_AXIS_UNITS_DA,
            selected = "percent"
          )
        )
      )
    ),

    # Tab 2: Column mapping -----
    nav_panel(
      "Kolonnematch",
      icon = icon("columns"),
      layout_column_wrap(
        width = 1 / 2,
        div(
          # X-axis column
          selectizeInput(
            "x_column",
            "X-akse (vandret tids-/observationsakse):",
            choices = NULL,
            selected = NULL
          ),

          # Y-axis column
          selectizeInput(
            "y_column",
            "Y-akse (lodret værdiakse):",
            choices = NULL,
            selected = NULL
          ),

          # N column - wrapped for dropup behavior
          div(
            class = "selectize-dropup",
            selectizeInput(
              "n_column",
              span("Nævner (n):", icon("info-circle")),
              choices = NULL,
              selected = NULL
            ) |>
              tooltip("Valgfri: Nævner-kolonne til beregning af andele og rater")
          )
        ),
        div(
          # Skift column
          selectizeInput(
            "skift_column",
            span("Opdel proces:", icon("info-circle")),
            choices = NULL,
            selected = NULL
          ),

          # Frys column
          selectizeInput(
            "frys_column",
            span("Fastfrys niveau:", icon("info-circle")),
            choices = NULL,
            selected = NULL
          ),

          # Kommentar column
          div(
            class = "selectize-dropup",
            selectizeInput(
              "kommentar_column",
              span("Kommentar (noter):", icon("info-circle")),
              choices = NULL,
              selected = NULL
            ) |>
              tooltip("Valgfri: Kolonne med kommentarer eller noter til datapunkter")
          ),
          div(
            style = "padding: 10px 0;",
            div(
              class = "text-center",
              # Auto-detect button
              actionButton(
                "auto_detect_columns",
                "Auto-detektér kolonner",
                icon = icon("magic"),
                class = "btn-secondary btn-sm w-100",
                style = "margin-top: 25px;"
              )
            ),
          )
        ),

        # Column validation feedback
        # div(
        #   id = "column_validation",
        #   style = "margin-top: 10px;",
        #   uiOutput("column_validation_messages")
        # )
      )
    ),

    # Tab 3: Organisatorisk enhed ----
    nav_panel(
      "Organisatorisk",
      icon = icon("building"),
      max_height = "100%",
      min_height = "100%",
      div(
        style = "padding: 10px 0;",
        # Organisatorisk enhed selection
        create_unit_selection()
      )
    ),

    # Tab 4: Additional settings (placeholder) ----
    # nav_panel(
    #   "Avanceret",
    #   icon = icon("cogs"),
    #   max_height = "100%",
    #   min_height = "100%",
    #
    #   div(
    #     style = "padding: 20px; text-align: center; color: #666;",
    #     icon("wrench", style = "font-size: 2rem; margin-bottom: 10px;"),
    #     br(),
    #     "Yderligere indstillinger kommer her",
    #     br(),
    #     tags$small("Denne tab er reserveret til fremtidige features")
    #   )
    # ) # nav_panel (Avanceret)
  ) # navset_card_tab
}


# New function for plot-only card
create_plot_only_card <- function() {
  card(
    full_screen = TRUE,
    fillable = TRUE,
    max_height = "100%",
    min_height = "100%",
    card_header(div(icon("chart-line"), " SPC Graf", )),
    card_body(
      div(
        style = "height: 100%",
        fill = TRUE,
        visualizationModuleUI("visualization")
      )
    )
  )
}

create_data_table_card <- function() {
  card(
    full_screen = TRUE,
    min_height = "calc(50vh - 60px)",
    card_header(
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        div(icon("table"), " Data", ),
        div(
          class = "btn-group-sm",
          actionButton(
            "edit_column_names",
            label = "Redigér kolonnenavne",
            icon = icon("edit"),
            title = "Redigér kolonnenavne",
            class = "btn-secondary btn-sm"
          ),
          actionButton(
            "add_column",
            label = "Tilføj kolonne",
            icon = icon("plus"),
            title = "Tilføj kolonne",
            class = "btn-secondary btn-sm"
          ),
          actionButton(
            "add_row",
            label = "Tilføj række",
            icon = icon("plus-square"),
            title = "Tilføj række",
            class = "btn-secondary btn-sm"
          ),
          # actionButton(
          #   "reset_table",
          #   label = NULL,
          #   icon = icon("refresh"),
          #   title = "Reset tabel",
          #   class = "btn-outline-warning btn-sm"
          # )
        )
      )
    ),
    card_body(
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
  div(
    style = "margin-bottom: 15px;",
    tags$label("Afdeling eller afsnit", style = "font-weight: 500;"),
    div(
      style = "margin-top: 5px;",
      radioButtons(
        "unit_type",
        NULL,
        choices = list("Vælg fra liste" = "select", "Indtast selv" = "custom"),
        selected = "select",
        inline = TRUE
      )
    ),

    # Dropdown for standard enheder
    conditionalPanel(condition = "input.unit_type == 'select'", selectizeInput(
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
    conditionalPanel(
      condition = "input.unit_type == 'custom'",
      textInput("unit_custom", NULL, placeholder = "Indtast enhedsnavn...")
    )
  )
}

create_export_card <- function() {
  conditionalPanel(condition = "output.plot_ready == 'true'", card(
    card_header(div(icon("download"), " Eksport", )),
    card_body(
      # KOMPLET EXPORT - Excel version
      div(
        downloadButton(
          "download_complete_excel",
          "📋 Komplet Export (Excel)",
          icon = icon("file-excel"),
          title = "Download hele sessionen som Excel fil med data og konfiguration",
          class = "btn-success w-100 mb-2"
        ),

        # Hjælpe-tekst for komplet export
        div(
          style = "font-size: 0.75rem; color: #666; text-align: center; margin-bottom: 8px; font-style: italic;",
          "Data + metadata i 2 Excel sheets - klar til brug og re-import"
        )
      ),
      hr(style = "margin: 15px 0;"),
      div(style = "text-align: center; font-size: 0.85rem; color: #666; margin-bottom: 10px;", strong("Graf eksporter:")),
      downloadButton(
        "download_png",
        "Download PNG",
        icon = icon("image"),
        class = "btn-outline-primary w-100 mb-2"
      ),
      downloadButton(
        "download_pdf",
        "Download PDF Rapport",
        icon = icon("file-pdf"),
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
# ui_welcome_page.R
# UI komponenter for velkomstside

# Dependencies ----------------------------------------------------------------

# UI VELKOMSTSIDE KOMPONENTER =================================================

## Hovedfunktion for velkomstside
# Opretter komplet velkomstside med hero sektion og handlingsknapper
create_welcome_page <- function() {
  div(
    class = "welcome-page",
    style = "min-height: 100vh; background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);",

    # Hero Sektion
    div(
      class = "hero-section py-5",
      div(
        class = "container-fluid",
        div(
          class = "row align-items-center mb-5",
          div(
            class = "col-12 text-center",
            h1(
              class = "display-4 fw-bold",
              style = paste0("color: ", HOSPITAL_COLORS$primary, "; margin-bottom: 1rem;"),
              "Velkommen til BFH SPC-værktøj"
            ),
            p(
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
    div(
      class = "container-fluid px-4",
      div(
        class = "row g-4 mb-5",

        # LEFT COLUMN - Getting Started Guide
        div(
          class = "col-lg-6",
          create_getting_started_card()
        ),

        # RIGHT COLUMN - Understanding SPC
        div(
          class = "col-lg-6",
          create_understanding_spc_card()
        )
      )
    ),

    # Call to Action Section
    div(
      class = "cta-section py-5",
      style = paste0("background-color: ", HOSPITAL_COLORS$primary, "; color: white;"),
      div(
        class = "container text-center",
        div(
          class = "row",
          div(
            class = "col-lg-8 mx-auto",
            h2(class = "mb-4", "Klar til at komme i gang?"),
            p(class = "mb-4 fs-5", "Start din første SPC-analyse på under 5 minutter."),
            div(
              class = "d-grid gap-2 d-md-flex justify-content-md-center",
              actionButton(
                "start_new_session",
                "🚀 Start ny analyse",
                class = "btn btn-light btn-lg me-md-2",
                style = "font-weight: 600; padding: 12px 30px;"
              ),
              actionButton(
                "upload_data_welcome",
                "📊 Upload data",
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
  card(
    class = "h-100 shadow-sm",
    style = "border: none; border-radius: 15px;",
    card_header(
      class = "bg-white border-0 pb-0",
      style = "border-radius: 15px 15px 0 0;",
      div(
        class = "d-flex align-items-center",
        div(
          class = "me-3",
          style = paste0("background: ", HOSPITAL_COLORS$primary, "; width: 50px; height: 50px; border-radius: 12px; display: flex; align-items: center; justify-content: center;"),
          icon("rocket", style = "color: white; font-size: 1.5rem;")
        ),
        div(
          h3(class = "card-title mb-1", "Kom i gang på 3 trin"),
          p(class = "text-muted mb-0", "Din vej fra data til indsigt")
        )
      )
    ),
    card_body(
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
      div(
        class = "mt-4 pt-3 border-top",
        div(
          class = "d-grid",
          actionButton(
            "quick_start_demo",
            "👆 Prøv med eksempel-data",
            class = "btn btn-outline-primary btn-lg",
            style = "border-radius: 10px; font-weight: 500;"
          )
        ),
        p(
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
  card(
    class = "h-100 shadow-sm",
    style = "border: none; border-radius: 15px;",
    card_header(
      class = "bg-white border-0 pb-0",
      style = "border-radius: 15px 15px 0 0;",
      div(
        class = "d-flex align-items-center",
        div(
          class = "me-3",
          style = paste0("background: ", HOSPITAL_COLORS$secondary, "; width: 50px; height: 50px; border-radius: 12px; display: flex; align-items: center; justify-content: center;"),
          icon("lightbulb", style = "color: white; font-size: 1.5rem;")
        ),
        div(
          h3(class = "card-title mb-1", "Forstå SPC"),
          p(class = "text-muted mb-0", "Værktøjet der transformerer data til handling")
        )
      )
    ),
    card_body(
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
        content = HTML("
          <ul class='list-unstyled'>
            <li><strong>🎯 Spot trends tidligt:</strong> Identificer problemer før de bliver kritiske</li>
            <li><strong>📊 Forstå variation:</strong> Normal udsving vs. særlige årsager</li>
            <li><strong>💡 Træf bedre beslutninger:</strong> Baseret på statistisk evidens</li>
            <li><strong>🚀 Forbedre kontinuerligt:</strong> Måle effekt af ændringer</li>
          </ul>
        ")
      ),

      # Healthcare Examples
      create_info_section(
        icon = "hospital",
        title = "Konkrete eksempler fra BFH",
        content = HTML("
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
  div(
    class = "step-item d-flex mb-4",
    # Step Number Circle
    div(
      class = "step-number me-3 flex-shrink-0",
      style = paste0("width: 40px; height: 40px; background: ", HOSPITAL_COLORS$primary, "; color: white; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-weight: bold; font-size: 1.1rem;"),
      number
    ),
    # Step Content
    div(
      class = "step-content flex-grow-1",
      div(
        class = "d-flex align-items-center mb-2",
        icon(icon, class = "text-primary me-2"),
        h5(class = "mb-0 fw-semibold", title)
      ),
      p(class = "text-muted mb-1", description),
      if (!is.null(example)) {
        p(
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
  div(
    class = "info-section mb-4",
    div(
      class = "d-flex align-items-start mb-2",
      div(
        class = "me-2 flex-shrink-0",
        icon(icon, class = "text-secondary", style = "font-size: 1.2rem; margin-top: 2px;")
      ),
      div(
        class = "flex-grow-1",
        h5(class = "fw-semibold mb-2", title),
        div(class = "text-muted", content)
      )
    )
  )
}
