# server_file_upload.R
# Server logik til håndtering af fil uploads og import

# Dependencies ----------------------------------------------------------------
# Bruger readxl og readr til fil-import

# UPLOAD HÅNDTERING ===========================================================

## Setup fil upload funktionalitet
setup_file_upload <- function(input, output, session, values, waiter_file) {
  # File upload handler
  observeEvent(input$data_file, {
    req(input$data_file)

    # Show loading
    waiter_file$show()

    # Ensure loading is hidden no matter what happens
    on.exit({
      waiter_file$hide()
    })

    # Close upload modal automatically
    on.exit(
      {
        removeModal()
      },
      add = TRUE
    )

    values$updating_table <- TRUE
    on.exit(
      {
        values$updating_table <- FALSE
      },
      add = TRUE
    )

    file_path <- input$data_file$datapath
    file_ext <- tools::file_ext(input$data_file$name)

    tryCatch(
      {
        if (file_ext %in% c("xlsx", "xls")) {
          handle_excel_upload(file_path, session, values)
        } else {
          handle_csv_upload(file_path, values)
        }
      },
      error = function(e) {
        showNotification(
          paste("Fejl ved upload:", e$message),
          type = "error",
          duration = 5
        )
      }
    )
  })
}

## Håndter Excel fil upload
handle_excel_upload <- function(file_path, session, values) {
  excel_sheets <- readxl::excel_sheets(file_path)

  if ("Data" %in% excel_sheets && "Metadata" %in% excel_sheets) {
    # Read data from Data sheet
    data <- readxl::read_excel(file_path, sheet = "Data", col_names = TRUE)

    # Ensure standard columns are present and in correct order
    data <- ensure_standard_columns(data)

    # Read session info from Metadata sheet
    session_info_raw <- readxl::read_excel(
      file_path,
      sheet = "Metadata",
      col_names = FALSE
    )

    # Parse session info for configuration
    session_lines <- as.character(session_info_raw[[1]])

    # Extract metadata from session info
    metadata <- parse_session_metadata(session_lines, names(data))

    # Load data
    values$current_data <- as.data.frame(data)
    values$original_data <- as.data.frame(data)
    values$file_uploaded <- TRUE
    values$auto_detect_done <- TRUE # Skip auto-detect since we have session info
    values$hide_anhoej_rules <- FALSE # Re-enable Anhøj rules when real data is uploaded

    # Restore metadata with delay to ensure UI is ready
    invalidateLater(500)
    isolate({
      restore_metadata(session, metadata)
    })

    showNotification(
      paste("Komplet session importeret:", nrow(data), "rækker,", ncol(data), "kolonner + konfiguration"),
      type = "message",
      duration = 4
    )
  } else {
    # Standard Excel file
    data <- readxl::read_excel(file_path, col_names = TRUE)

    # Ensure standard columns are present and in correct order
    data <- ensure_standard_columns(data)

    values$current_data <- as.data.frame(data)
    values$original_data <- as.data.frame(data)
    values$file_uploaded <- TRUE
    values$auto_detect_done <- FALSE
    values$initial_auto_detect_completed <- FALSE # Reset for new data
    values$hide_anhoej_rules <- FALSE # Re-enable Anhøj rules when real data is uploaded

    showNotification(
      paste("Excel fil uploadet:", nrow(data), "rækker,", ncol(data), "kolonner"),
      type = "message",
      duration = 3
    )
  }
}

## Håndter CSV fil upload
handle_csv_upload <- function(file_path, values) {
  # CSV behandling med danske standarder
  data <- readr::read_csv2(
    file_path,
    locale = readr::locale(
      decimal_mark = ",",
      grouping_mark = ".",
      encoding = "ISO-8859-1"
    ),
    show_col_types = FALSE
  )

  # Ensure standard columns are present and in correct order
  data <- ensure_standard_columns(data)

  values$current_data <- as.data.frame(data)
  values$original_data <- as.data.frame(data)
  values$file_uploaded <- TRUE
  values$auto_detect_done <- FALSE
  values$hide_anhoej_rules <- FALSE # Re-enable Anhøj rules when real data is uploaded

  showNotification(
    paste("CSV fil uploadet:", nrow(data), "rækker,", ncol(data), "kolonner"),
    type = "message",
    duration = 3
  )
}

## Parse session metadata fra importerede filer
parse_session_metadata <- function(session_lines, data_cols) {
  metadata <- list()

  # Parse titel
  title_line <- session_lines[grepl("^• Titel:", session_lines)]
  if (length(title_line) > 0) {
    metadata$title <- gsub("^• Titel: ", "", title_line[1])
    metadata$title <- gsub(" Ikke angivet$", "", metadata$title)
  }

  # Parse enhed
  unit_line <- session_lines[grepl("^• Enhed:", session_lines)]
  if (length(unit_line) > 0) {
    unit_text <- gsub("^• Enhed: ", "", unit_line[1])
    if (unit_text != "Ikke angivet" && unit_text != "") {
      # Check if it's a standard unit
      standard_units <- list(
        "Medicinsk Afdeling" = "med",
        "Kirurgisk Afdeling" = "kir",
        "Intensiv Afdeling" = "icu",
        "Ambulatorie" = "amb",
        "Akutmodtagelse" = "akut",
        "Pædiatrisk Afdeling" = "paed",
        "Gynækologi/Obstetrik" = "gyn"
      )

      if (unit_text %in% names(standard_units)) {
        metadata$unit_type <- "select"
        metadata$unit_select <- standard_units[[unit_text]]
      } else {
        metadata$unit_type <- "custom"
        metadata$unit_custom <- unit_text
      }
    }
  }

  # Parse beskrivelse
  desc_line <- session_lines[grepl("^• Beskrivelse:", session_lines)]
  if (length(desc_line) > 0) {
    desc_text <- gsub("^• Beskrivelse: ", "", desc_line[1])
    if (desc_text != "Ikke angivet" && desc_text != "") {
      metadata$description <- desc_text
    }
  }

  # Parse graf konfiguration
  chart_line <- session_lines[grepl("^• Chart Type:", session_lines)]
  if (length(chart_line) > 0) {
    chart_text <- gsub("^• Chart Type: ", "", chart_line[1])
    if (chart_text %in% names(CHART_TYPES_DA)) {
      metadata$chart_type <- chart_text
    }
  }

  # Parse kolonne mapping
  x_line <- session_lines[grepl("^• X-akse:", session_lines)]
  if (length(x_line) > 0) {
    x_text <- gsub("^• X-akse: (.+) \\(.*\\)$", "\\1", x_line[1])
    if (x_text != "Ikke valgt" && x_text %in% data_cols) {
      metadata$x_column <- x_text
    }
  }

  y_line <- session_lines[grepl("^• Y-akse:", session_lines)]
  if (length(y_line) > 0) {
    y_text <- gsub("^• Y-akse: (.+) \\(.*\\)$", "\\1", y_line[1])
    if (y_text != "Ikke valgt" && y_text %in% data_cols) {
      metadata$y_column <- y_text
    }
  }

  n_line <- session_lines[grepl("^• Nævner:", session_lines)]
  if (length(n_line) > 0) {
    n_text <- gsub("^• Nævner: ", "", n_line[1])
    if (n_text %in% data_cols) {
      metadata$n_column <- n_text
    }
  }

  return(metadata)
}
# server_download.R
# Server logik for download handlers og eksport formater

# Dependencies ----------------------------------------------------------------

# DOWNLOAD SETUP ==============================================================

## Hovedfunktion for downloads
# Opsætter alle download handlers for forskellige eksport formater
setup_download_handlers <- function(input, output, session, values) {
  # Hent visualisering objekt til download handlers
  visualization <- setup_visualization(input, output, session, values)

  # Komplet Excel eksport
  output$download_complete_excel <- downloadHandler(
    filename = function() {
      title_clean <- gsub("[^A-Za-z0-9æøåÆØÅ ]", "", chart_title(input)())
      title_clean <- gsub(" ", "_", title_clean)
      if (nchar(title_clean) == 0) title_clean <- "SPC_Analyse"
      paste0("SPC_Session_", title_clean, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      create_complete_excel_export(file, input, values)
    }
  )

  # PNG download
  output$download_png <- downloadHandler(
    filename = function() {
      title_clean <- gsub("[^A-Za-z0-9æøåÆØÅ ]", "", chart_title(input)())
      title_clean <- gsub(" ", "_", title_clean)
      paste0(title_clean, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      if (!is.null(visualization$plot())) {
        ggsave(file, visualization$plot(), width = 12, height = 8, dpi = 300)

        showNotification(
          paste("PNG eksporteret:", chart_title(input)()),
          type = "message",
          duration = 3
        )
      }
    }
  )

  # PDF download
  output$download_pdf <- downloadHandler(
    filename = function() {
      title_clean <- gsub("[^A-Za-z0-9æøåÆØÅ ]", "", chart_title(input)())
      title_clean <- gsub(" ", "_", title_clean)
      paste0("rapport_", title_clean, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      showNotification("PDF rapport kommer i næste fase", type = "message")
    }
  )
}

## Opret komplet Excel eksport
# Opretter omfattende Excel eksport med data og metadata
create_complete_excel_export <- function(file, input, values) {
  # Hent aktive data til eksport
  active_data_for_export <- values$current_data

  # Filtrer tomme rækker fra
  if (!is.null(active_data_for_export)) {
    non_empty_rows <- apply(active_data_for_export, 1, function(row) any(!is.na(row)))
    if (any(non_empty_rows)) {
      active_data_for_export <- active_data_for_export[non_empty_rows, ]
    }
  }

  if (!is.null(active_data_for_export)) {
    tryCatch(
      {
        # Creating Excel complete export with 2 sheets

        # Create Excel workbook
        wb <- openxlsx::createWorkbook()

        # ============== SHEET 1: DATA ==============
        openxlsx::addWorksheet(wb, "Data")

        # Write data to sheet with professional formatting
        openxlsx::writeData(wb, "Data", active_data_for_export,
          startRow = 1, startCol = 1,
          headerStyle = openxlsx::createStyle(
            textDecoration = "bold",
            fgFill = HOSPITAL_COLORS$primary,
            fontColour = "white",
            border = "TopBottomLeftRight",
            fontSize = 12
          )
        )

        # Format data sheet
        openxlsx::addStyle(wb, "Data",
          style = openxlsx::createStyle(
            border = "TopBottomLeftRight",
            wrapText = TRUE
          ),
          rows = 2:(nrow(active_data_for_export) + 1),
          cols = 1:ncol(active_data_for_export),
          gridExpand = TRUE
        )

        # Auto-width columns and freeze header
        openxlsx::setColWidths(wb, "Data", cols = 1:ncol(active_data_for_export), widths = "auto")
        openxlsx::freezePane(wb, "Data", firstActiveRow = 2)

        # ============== SHEET 2: METADATA ==============
        openxlsx::addWorksheet(wb, "Metadata")

        # Create combined session information
        session_lines <- create_session_info_lines(input, active_data_for_export, values)

        # Remove NULL values
        session_lines <- session_lines[!is.null(session_lines)]

        # Write session info
        session_df <- data.frame(Information = session_lines, stringsAsFactors = FALSE)
        openxlsx::writeData(wb, "Metadata", session_df, startRow = 1, startCol = 1, colNames = FALSE)

        # Format metadata sheet
        openxlsx::setColWidths(wb, "Metadata", cols = 1, widths = 85)

        # Style for header and separators
        header_style <- openxlsx::createStyle(
          fontSize = 16,
          textDecoration = "bold",
          fgFill = HOSPITAL_COLORS$primary,
          fontColour = "white",
          halign = "center"
        )
        openxlsx::addStyle(wb, "Metadata", header_style, rows = 1, cols = 1)

        # Style for section headers
        section_rows <- which(grepl("^[A-ZÆØÅ ]+:$", session_lines))
        if (length(section_rows) > 0) {
          section_style <- openxlsx::createStyle(
            fontSize = 12,
            textDecoration = "bold",
            fgFill = HOSPITAL_COLORS$light
          )
          openxlsx::addStyle(wb, "Metadata", section_style, rows = section_rows, cols = 1)
        }

        # Save Excel file
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

        showNotification(
          paste("Komplet Excel session eksporteret:", basename(file)),
          type = "message",
          duration = 4
        )
      },
      error = function(e) {
        cat("ERROR during Excel export:", e$message, "\n")
        showNotification(
          paste("Fejl ved Excel eksport:", e$message),
          type = "error",
          duration = 5
        )
      }
    )
  } else {
    showNotification(
      "Ingen data at eksportere",
      type = "warning",
      duration = 3
    )
  }
}

## Opret session info linjer
# Opretter detaljerede session informations linjer til metadata
create_session_info_lines <- function(input, active_data_for_export, values) {
  c(
    paste(HOSPITAL_NAME, "- SPC ANALYSE"),
    "════════════════════════════════════════════════════════",
    "",
    "INDIKATOR INFORMATION:",
    paste("• Titel:", if (is.null(input$indicator_title) || input$indicator_title == "") "Ikke angivet" else input$indicator_title),
    paste("• Enhed:", current_unit(input)()),
    paste("• Beskrivelse:", if (is.null(input$indicator_description) || input$indicator_description == "") "Ikke angivet" else input$indicator_description),
    "",
    "GRAF KONFIGURATION:",
    paste("• Chart Type:", if (is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type),
    paste("• X-akse:", if (is.null(input$x_column) || input$x_column == "") "Ikke valgt" else input$x_column, "(tid/observation)"),
    paste("• Y-akse:", if (is.null(input$y_column) || input$y_column == "") "Ikke valgt" else input$y_column, "(værdier)"),
    if (!is.null(input$n_column) && input$n_column != "") paste("• Nævner:", input$n_column) else NULL,
    paste("• Målsætninger:", ifelse(if (is.null(input$show_targets)) FALSE else input$show_targets, "Vist", "Skjult")),
    paste("• Faser:", ifelse(if (is.null(input$show_phases)) FALSE else input$show_phases, "Vist", "Skjult")),
    "",
    "DATA INFORMATION:",
    paste("• Rækker:", nrow(active_data_for_export)),
    paste("• Kolonner:", ncol(active_data_for_export)),
    paste("• Kolonnenavne:", paste(names(active_data_for_export), collapse = ", ")),
    paste("• Data kilde:", if (values$file_uploaded) "File Upload" else "Manuel indtastning"),
    paste("• Eksporteret:", format(Sys.time(), "%d-%m-%Y %H:%M")),
    "",
    "TEKNISK INFORMATION:",
    paste("• App Version: BFH_SPC_v1.2"),
    paste("• Chart Type Code:", get_qic_chart_type(if (is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type)),
    "",
    "════════════════════════════════════════════════════════",
    "IMPORT INSTRUKTIONER:",
    "• For at re-importere: Rediger data i 'Data' sheet og gem filen",
    "• Bevar kolonnenavnene og strukturen",
    "• Slet eller tilføj rækker efter behov",
    "• Import filen i SPC appen som Excel fil",
    "",
    paste("Genereret af:", HOSPITAL_NAME, "SPC App")
  )
}
