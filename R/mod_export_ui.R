# mod_export_ui.R
# UI components for export module
# Provides user interface for exporting SPC charts in multiple formats

# Dependencies ----------------------------------------------------------------
# Helper functions loaded globally in global.R for better performance

# EXPORT MODULE UI ============================================================

#' Export Module UI
#'
#' Brugerinterface til eksport af SPC charts.
#' Understøtter PDF, PNG og PowerPoint formater med live preview.
#'
#' Layout:
#' - Venstre panel (40%): Format selector, metadata input fields
#' - Højre panel (60%): Live preview af chart
#'
#' @param id Character. Namespace ID for modulet
#' @return Shiny UI element
#' @family export_modules
#' @export
mod_export_ui <- function(id) {
  ns <- shiny::NS(id)

  # Hovedlayout: To-kolonne layout (40% / 60%)
  bslib::layout_columns(
    col_widths = c(4, 8), # 40% / 60% split
    height = "auto",
    min_height = "calc(100vh - 200px)",

    # VENSTRE PANEL: Format selector og metadata ----
    bslib::card(
      full_screen = FALSE,
      height = "100%",
      bslib::card_header(
        shiny::div(
          shiny::icon("file-export"),
          " Eksport Indstillinger"
        )
      ),
      bslib::card_body(
        # Format selector (radio buttons)
        shiny::div(
          style = "margin-bottom: 20px;",
          shiny::tags$label(
            "Eksport Format:",
            class = "control-label",
            style = "font-weight: 500; margin-bottom: 10px; display: block;"
          ),
          shiny::radioButtons(
            ns("export_format"),
            label = NULL,
            choices = EXPORT_FORMAT_OPTIONS,
            selected = "pdf",
            inline = FALSE
          )
        ),
        shiny::hr(),

        # Metadata fields (alle formater) ----
        shiny::div(
          style = "margin-bottom: 15px;",
          shiny::textInput(
            ns("export_title"),
            "Titel:",
            value = "",
            placeholder = "Angiv titel til eksport",
            width = "100%"
          ),
          shiny::tags$small(
            class = "text-muted",
            sprintf("Maksimalt %d karakterer", EXPORT_TITLE_MAX_LENGTH)
          )
        ),
        shiny::div(
          style = "margin-bottom: 15px;",
          shiny::textInput(
            ns("export_department"),
            "Afdeling/Afsnit:",
            value = "",
            placeholder = "F.eks. 'Medicinsk Afdeling'",
            width = "100%"
          ),
          shiny::tags$small(
            class = "text-muted",
            sprintf("Maksimalt %d karakterer", EXPORT_DEPARTMENT_MAX_LENGTH)
          )
        ),

        # Conditional panels for format-specific fields ----

        ## PDF-specific fields ----
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'pdf'", ns("export_format")),
          shiny::div(
            style = "margin-bottom: 15px;",
            shiny::textAreaInput(
              ns("pdf_description"),
              "Datadefinition:",
              value = "",
              placeholder = "Beskriv hvad indikatoren måler og hvordan data opgøres",
              width = "100%",
              rows = 4,
              resize = "vertical"
            ),
            shiny::tags$small(
              class = "text-muted",
              sprintf("Maksimalt %d karakterer", EXPORT_DESCRIPTION_MAX_LENGTH)
            )
          ),
          shiny::div(
            style = "margin-bottom: 15px;",
            shiny::textAreaInput(
              ns("pdf_improvement"),
              "Forbedringsmål:",
              value = "",
              placeholder = "Angiv mål for forbedring eller ønsket udvikling",
              width = "100%",
              rows = 4,
              resize = "vertical"
            ),
            shiny::tags$small(
              class = "text-muted",
              sprintf("Maksimalt %d karakterer", EXPORT_DESCRIPTION_MAX_LENGTH)
            )
          )
        ),

        ## PNG-specific fields ----
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'png'", ns("export_format")),
          shiny::div(
            style = "margin-bottom: 15px;",
            shiny::selectInput(
              ns("png_size_preset"),
              "Størrelse:",
              choices = c(
                EXPORT_SIZE_PRESETS$small$label,
                EXPORT_SIZE_PRESETS$medium$label,
                EXPORT_SIZE_PRESETS$large$label
              ),
              selected = EXPORT_SIZE_PRESETS$medium$label,
              width = "100%"
            )
          ),
          shiny::div(
            style = "margin-bottom: 15px;",
            shiny::selectInput(
              ns("png_dpi"),
              "DPI (Opløsning):",
              choices = EXPORT_DPI_OPTIONS,
              selected = 96,
              width = "100%"
            ),
            shiny::tags$small(
              class = "text-muted",
              "96 DPI: Skærm | 150 DPI: Medium print | 300 DPI: Høj kvalitet"
            )
          )
        ),

        ## PowerPoint-specific fields ----
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'pptx'", ns("export_format")),
          shiny::div(
            style = "margin-bottom: 15px;",
            shiny::tags$p(
              class = "text-muted",
              style = "font-size: 0.9rem; margin-bottom: 10px;",
              shiny::icon("info-circle"),
              " Chart eksporteres med optimal størrelse til PowerPoint slides."
            ),
            shiny::tags$p(
              class = "text-muted",
              style = "font-size: 0.85rem;",
              sprintf(
                "Standard: %g × %g %s ved %d DPI",
                EXPORT_POWERPOINT_CONFIG$width,
                EXPORT_POWERPOINT_CONFIG$height,
                EXPORT_POWERPOINT_CONFIG$unit,
                EXPORT_SIZE_PRESETS$powerpoint$dpi
              )
            )
          )
        ),
        shiny::hr(),

        # Download button ----
        shiny::div(
          style = "margin-top: 20px;",
          shiny::downloadButton(
            ns("download_export"),
            "Download",
            icon = shiny::icon("download"),
            class = "btn-primary w-100",
            style = "padding: 12px; font-weight: 500;"
          )
        )
      )
    ),

    # HØJRE PANEL: Live preview ----
    bslib::card(
      full_screen = TRUE,
      height = "100%",
      fillable = TRUE,
      bslib::card_header(
        shiny::div(
          shiny::icon("eye"),
          " Preview"
        )
      ),
      bslib::card_body(
        fill = TRUE,
        # Conditional panels for preview availability
        # Show warning when no plot available
        shiny::conditionalPanel(
          condition = "output.plot_available == false",
          ns = ns,
          shiny::div(
            class = "alert alert-warning",
            style = "margin: 20px; padding: 20px;",
            shiny::icon("exclamation-triangle"),
            " Ingen graf er genereret endnu. Gå til hovedsiden for at oprette en SPC-graf først."
          )
        ),
        # Show preview when plot available
        shiny::conditionalPanel(
          condition = "output.plot_available == true",
          ns = ns,
          shiny::div(
            style = "height: 100%; display: flex; align-items: center; justify-content: center;",
            shiny::plotOutput(
              ns("export_preview"),
              width = "100%",
              height = "600px"
            )
          )
        )
      )
    )
  )
}
