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
    tryCatch({
      # Creating Excel complete export with 2 sheets
      
      # Create Excel workbook
      wb <- openxlsx::createWorkbook()
      
      # ============== SHEET 1: DATA ==============
      openxlsx::addWorksheet(wb, "Data")
      
      # Write data to sheet with professional formatting
      openxlsx::writeData(wb, "Data", active_data_for_export, startRow = 1, startCol = 1, 
                headerStyle = openxlsx::createStyle(
                  textDecoration = "bold",
                  fgFill = HOSPITAL_COLORS$primary,
                  fontColour = "white",
                  border = "TopBottomLeftRight",
                  fontSize = 12
                ))
      
      # Format data sheet
      openxlsx::addStyle(wb, "Data", 
               style = openxlsx::createStyle(
                 border = "TopBottomLeftRight", 
                 wrapText = TRUE
               ), 
               rows = 2:(nrow(active_data_for_export) + 1), 
               cols = 1:ncol(active_data_for_export), 
               gridExpand = TRUE)
      
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
      
      cat("DEBUG: Excel complete export created successfully\n")
      
      showNotification(
        paste("Komplet Excel session eksporteret:", basename(file)),
        type = "message",
        duration = 4
      )
      
    }, error = function(e) {
      cat("ERROR during Excel export:", e$message, "\n")
      showNotification(
        paste("Fejl ved Excel eksport:", e$message),
        type = "error",
        duration = 5
      )
    })
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
    paste("• Titel:", if(is.null(input$indicator_title) || input$indicator_title == "") "Ikke angivet" else input$indicator_title),
    paste("• Enhed:", current_unit(input)()),
    paste("• Beskrivelse:", if(is.null(input$indicator_description) || input$indicator_description == "") "Ikke angivet" else input$indicator_description),
    "",
    "GRAF KONFIGURATION:",
    paste("• Chart Type:", if(is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type),
    paste("• X-akse:", if(is.null(input$x_column) || input$x_column == "" || input$x_column == "BLANK") "Ikke valgt" else input$x_column, "(tid/observation)"),
    paste("• Y-akse:", if(is.null(input$y_column) || input$y_column == "" || input$y_column == "BLANK") "Ikke valgt" else input$y_column, "(værdier)"),
    if (!is.null(input$n_column) && input$n_column != "" && input$n_column != "BLANK") paste("• Nævner:", input$n_column) else NULL,
    paste("• Målsætninger:", ifelse(if(is.null(input$show_targets)) FALSE else input$show_targets, "Vist", "Skjult")),
    paste("• Faser:", ifelse(if(is.null(input$show_phases)) FALSE else input$show_phases, "Vist", "Skjult")),
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
    paste("• Chart Type Code:", get_qic_chart_type(if(is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type)),
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
