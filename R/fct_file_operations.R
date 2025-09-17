# server_file_upload.R
# Server logik til håndtering af fil uploads og import

# Dependencies ----------------------------------------------------------------
# Bruger readxl og readr til fil-import

# UPLOAD HÅNDTERING ===========================================================

## Setup fil upload funktionalitet
setup_file_upload <- function(input, output, session, values, waiter_file, app_state = NULL) {
  # Unified state: App state is always available
  log_debug("===========================================", "FILE_UPLOAD")
  log_debug("Setting up file upload handlers", "FILE_UPLOAD")

  # File upload handler
  observeEvent(input$data_file, {
    log_debug("File upload triggered", "FILE_UPLOAD")
    req(input$data_file)

    # Start workflow tracer for file upload
    upload_tracer <- debug_workflow_tracer("file_upload_workflow", app_state, session$token)
    upload_tracer$step("upload_initiated")

    log_debug("File info:", "FILE_UPLOAD")
    log_debug(paste("- Name:", input$data_file$name), "FILE_UPLOAD")
    log_debug(paste("- Size:", input$data_file$size, "bytes"), "FILE_UPLOAD")
    log_debug(paste("- Type:", input$data_file$type), "FILE_UPLOAD")
    log_debug(paste("- Path:", input$data_file$datapath), "FILE_UPLOAD")

    debug_log("File upload started", "FILE_UPLOAD_FLOW", level = "INFO",
              context = list(
                filename = input$data_file$name,
                size_bytes = input$data_file$size,
                file_type = input$data_file$type
              ),
              session_id = session$token)

    upload_tracer$step("file_validation")

    # ENHANCED FILE VALIDATION
    validation_result <- validate_uploaded_file(input$data_file, session$token)
    if (!validation_result$valid) {
      upload_tracer$complete("file_validation_failed")
      debug_log("File validation failed", "ERROR_HANDLING", level = "ERROR",
                context = list(
                  validation_errors = validation_result$errors,
                  filename = input$data_file$name
                ),
                session_id = session$token)

      showNotification(
        paste("File validation failed:", paste(validation_result$errors, collapse = "; ")),
        type = "error",
        duration = 8
      )
      return()
    }

    upload_tracer$step("file_validation_complete")

    # Show loading
    log_debug("Showing waiter...", "FILE_UPLOAD")
    waiter_file$show()
    log_debug("✅ Waiter displayed", "FILE_UPLOAD")

    # Ensure loading is hidden no matter what happens
    on.exit({
      log_debug("Hiding waiter on exit...", "FILE_UPLOAD")
      waiter_file$hide()
      log_debug("✅ Waiter hidden", "FILE_UPLOAD")
    })

    # Close upload modal automatically
    on.exit(
      {
        log_debug("Removing modal on exit...", "FILE_UPLOAD")
        removeModal()
        log_debug("✅ Modal removed", "FILE_UPLOAD")
      },
      add = TRUE
    )

    log_debug("Setting updating_table flag...", "FILE_UPLOAD")
    upload_tracer$step("state_management_setup")

    # PHASE 4B: Unified state assignment only - Set table updating flag
    app_state$data$updating_table <- TRUE

    debug_log("File upload state flags set", "FILE_UPLOAD_FLOW", level = "TRACE",
              context = list(updating_table = TRUE),
              session_id = session$token)
    on.exit(
      {
        log_debug("Clearing updating_table flag on exit...", "FILE_UPLOAD")
        # PHASE 4B: Unified state assignment only - Clear table updating flag
        app_state$data$updating_table <- FALSE
        log_debug("✅ updating_table flag cleared", "FILE_UPLOAD")
      },
      add = TRUE
    )

    file_path <- input$data_file$datapath
    file_ext <- tools::file_ext(input$data_file$name)

    log_debug("File processing:", "FILE_UPLOAD")
    log_debug(paste("- Extension:", file_ext), "FILE_UPLOAD")
    log_debug(paste("- File exists:", file.exists(file_path)), "FILE_UPLOAD")

    if (file.exists(file_path)) {
      file_info <- file.info(file_path)
      log_debug(paste("- Actual file size:", file_info$size, "bytes"), "FILE_UPLOAD")
    }

    tryCatch(
      {
        upload_tracer$step("file_processing_started")

        if (file_ext %in% c("xlsx", "xls")) {
          log_debug("📊 Processing Excel file...", "FILE_UPLOAD")
          debug_log("Starting Excel file processing", "FILE_UPLOAD_FLOW", level = "INFO", session_id = session$token)
          handle_excel_upload(file_path, session, values)
          log_debug("✅ Excel file processed successfully", "FILE_UPLOAD")
          upload_tracer$step("excel_processing_complete")
        } else {
          log_debug("📄 Processing CSV file...", "FILE_UPLOAD")
          debug_log("Starting CSV file processing", "FILE_UPLOAD_FLOW", level = "INFO", session_id = session$token)
          handle_csv_upload(file_path, values, app_state, session$token)
          log_debug("✅ CSV file processed successfully", "FILE_UPLOAD")
          upload_tracer$step("csv_processing_complete")
        }
        log_debug("File upload completed successfully", "FILE_UPLOAD")

        # Complete workflow tracing
        upload_tracer$complete("file_upload_workflow_complete")
        debug_log("File upload workflow completed successfully", "FILE_UPLOAD_FLOW", level = "INFO", session_id = session$token)
      },
      error = function(e) {
        log_debug(paste("❌ Error during file processing:", e$message), "FILE_UPLOAD")
        log_debug(paste("Error class:", class(e)), "FILE_UPLOAD")

        upload_tracer$complete("file_upload_workflow_failed")

        # ENHANCED ERROR HANDLING: Use comprehensive error recovery
        error_result <- handle_upload_error(e, input$data_file, session$token)
        log_debug(paste("Error categorized as:", error_result$error_type), "FILE_UPLOAD")
      }
    )

    log_debug("File upload handler completed", "FILE_UPLOAD")
    log_debug("===========================================", "FILE_UPLOAD")
  })

  # AUTO-DETECT TRIGGER OBSERVER ===============================================
  # Observer som reagerer på trigger_auto_detect flag og kører auto-detect
  observeEvent(values$trigger_auto_detect, {
    log_debug("===========================================", "AUTO_DETECT_TRIGGER")
    log_debug("Auto-detect trigger flag detected", "AUTO_DETECT_TRIGGER")

    # Check at der er data at arbejde med
    # Unified state: Use centralized state for current data
    current_data_check <- app_state$data$current_data

    req(current_data_check)
    req(values$trigger_auto_detect == TRUE)

    log_debug("Running auto-detect from trigger...", "AUTO_DETECT_TRIGGER")

    # Kør auto-detect funktionen
    auto_detect_result <- auto_detect_and_update_columns(
      input = input,
      session = session,
      values = values,
      app_state = app_state
    )

    if (!is.null(auto_detect_result)) {
      log_debug("✅ Auto-detect completed successfully from trigger", "AUTO_DETECT_TRIGGER")
      # Sæt auto_detect_done flag
      # PHASE 4B: Unified state assignment only - Set auto detect completed
      app_state$columns$auto_detect$completed <- TRUE
    } else {
      log_debug("⚠️ Auto-detect failed from trigger", "AUTO_DETECT_TRIGGER")
    }

    # PHASE 4B: Unified state assignment only - Clear trigger flag
    app_state$columns$auto_detect$trigger_needed <- FALSE

    log_debug("✅ Auto-detect trigger processing completed", "AUTO_DETECT_TRIGGER")
    log_debug("===========================================", "AUTO_DETECT_TRIGGER")
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
}

## Håndter Excel fil upload
handle_excel_upload <- function(file_path, session, values) {
  log_debug("========================================", "EXCEL_READ")
  log_debug("Starting Excel file processing", "EXCEL_READ")
  log_debug(paste("File path:", file_path), "EXCEL_READ")

  log_debug("Reading Excel sheets...", "EXCEL_READ")
  excel_sheets <- readxl::excel_sheets(file_path)
  log_debug(paste("Available sheets:", paste(excel_sheets, collapse = ", ")), "EXCEL_READ")

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
    # PHASE 4: Unified state assignment only - session restore
    data_frame <- as.data.frame(data)
    app_state$data$current_data <- data_frame
    app_state$data$original_data <- data_frame
    # PHASE 4B: Unified state assignment only - Set file uploaded flag
    app_state$session$file_uploaded <- TRUE
    # PHASE 4B: Unified state assignment only - Set auto detect completed (skip since we have session info)
    app_state$columns$auto_detect$completed <- TRUE
    # PHASE 4B: Unified state assignment only - Re-enable Anhøj rules when real data is uploaded
    app_state$ui$hide_anhoej_rules <- FALSE

    # NAVIGATION TRIGGER: Increment trigger to notify reactive navigation system
    if (!is.null(app_state$navigation_trigger)) {
      old_trigger <- app_state$navigation_trigger()
      app_state$navigation_trigger(old_trigger + 1)
      new_trigger <- app_state$navigation_trigger()
      log_debug(paste("SESSION_RESTORE: navigation_trigger incremented from", old_trigger, "to", new_trigger), "SESSION_RESTORE")
    }

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

    # PHASE 4: Unified state assignment only - Excel file loading
    data_frame <- as.data.frame(data)
    app_state$data$current_data <- data_frame
    app_state$data$original_data <- data_frame
    # PHASE 4B: Unified state assignment only - Set file uploaded flag
    app_state$session$file_uploaded <- TRUE
    # PHASE 4B: Unified state assignment only - Set auto detect flag
    app_state$columns$auto_detect$completed <- FALSE
    # PHASE 4B: Unified state assignment only - Re-enable Anhøj rules when real data is uploaded
    app_state$ui$hide_anhoej_rules <- FALSE

    # NAVIGATION TRIGGER: Increment trigger to notify reactive navigation system
    if (!is.null(app_state$navigation_trigger)) {
      old_trigger <- app_state$navigation_trigger()
      app_state$navigation_trigger(old_trigger + 1)
      new_trigger <- app_state$navigation_trigger()
      log_debug(paste("EXCEL_READ: navigation_trigger incremented from", old_trigger, "to", new_trigger), "EXCEL_READ")
    }

    showNotification(
      paste("Excel fil uploadet:", nrow(data), "rækker,", ncol(data), "kolonner"),
      type = "message",
      duration = 3
    )
  }
}

#' Håndter CSV fil upload med dansk formattering
#'
#' Indlæser og processer CSV filer med danske standarder inklusive
#' encoding, decimal separatorer og standard kolonner. Funktionen
#' håndterer fejl robust og opdaterer app state accordingly.
#'
#' @param file_path Character string med sti til CSV fil
#' @param values Reactive values list til opdatering af app state
#'
#' @details
#' CSV læsning konfiguration:
#' \itemize{
#'   \item Encoding: ISO-8859-1 (danske karakterer)
#'   \item Decimal mark: komma (,)
#'   \item Grouping mark: punktum (.)
#'   \item Separator: semikolon (;) - CSV2 format
#' }
#'
#' Behandling proces:
#' \enumerate{
#'   \item Læs CSV med readr::read_csv2 og dansk locale
#'   \item Tilføj standard SPC kolonner hvis manglende
#'   \item Opdater reactive values med ny data
#'   \item Sæt file_uploaded flag til TRUE
#'   \item Vis success notification til bruger
#' }
#'
#' @return NULL ved success, character string med fejlbesked ved fejl
#'
#' @examples
#' \dontrun{
#' # Upload CSV fil
#' result <- handle_csv_upload("data/spc_data.csv", values)
#' if (is.null(result)) {
#'   message("CSV uploaded successfully")
#' } else {
#'   message("Error:", result)
#' }
#' }
#'
#' @seealso \code{\link{handle_excel_upload}}, \code{\link{ensure_standard_columns}}
handle_csv_upload <- function(file_path, values, app_state = NULL, session_id = NULL) {
  log_debug("==========================================", "CSV_READ")
  log_debug("Starting CSV file processing", "CSV_READ")
  log_debug(paste("File path:", file_path), "CSV_READ")

  debug_log("CSV upload processing started", "FILE_UPLOAD_FLOW", level = "INFO",
            context = list(file_path = file_path),
            session_id = session_id)

  # CSV behandling med danske standarder
  log_debug("Reading CSV with Danish locale...", "CSV_READ")
  log_debug("- Decimal mark: ','", "CSV_READ")
  log_debug("- Grouping mark: '.'", "CSV_READ")
  log_debug("- Encoding: ISO-8859-1", "CSV_READ")

  data <- readr::read_csv2(
    file_path,
    locale = readr::locale(
      decimal_mark = ",",
      grouping_mark = ".",
      encoding = "ISO-8859-1"
    ),
    show_col_types = FALSE
  )

  log_debug(paste("CSV read successfully - dimensions:", nrow(data), "x", ncol(data)), "CSV_READ")
  log_debug(paste("Column names:", paste(names(data), collapse = ", ")), "CSV_READ")

  debug_log("CSV data loaded successfully", "FILE_UPLOAD_FLOW", level = "INFO",
            context = list(
              rows = nrow(data),
              columns = ncol(data),
              column_names = names(data)
            ),
            session_id = session_id)

  # ENHANCED DATA PREPROCESSING: Clean and validate data
  log_debug("Starting data preprocessing...", "CSV_READ")
  preprocessing_result <- preprocess_uploaded_data(
    data,
    list(name = basename(file_path), size = file.info(file_path)$size),
    session_id
  )
  data <- preprocessing_result$data
  log_debug(paste("Preprocessing completed - dimensions:", nrow(data), "x", ncol(data)), "CSV_READ")

  # Show user notification if significant cleaning occurred
  if (!is.null(preprocessing_result$cleaning_log)) {
    cleaning_messages <- character(0)
    if (!is.null(preprocessing_result$cleaning_log$empty_rows_removed)) {
      cleaning_messages <- c(cleaning_messages,
        paste(preprocessing_result$cleaning_log$empty_rows_removed, "empty rows removed"))
    }
    if (!is.null(preprocessing_result$cleaning_log$empty_columns_removed)) {
      cleaning_messages <- c(cleaning_messages,
        paste(preprocessing_result$cleaning_log$empty_columns_removed, "empty columns removed"))
    }
    if (!is.null(preprocessing_result$cleaning_log$column_names_cleaned)) {
      cleaning_messages <- c(cleaning_messages, "column names cleaned")
    }

    if (length(cleaning_messages) > 0) {
      showNotification(
        paste("Data cleaned:", paste(cleaning_messages, collapse = ", ")),
        type = "message",
        duration = 5
      )
    }
  }

  # Ensure standard columns are present and in correct order
  log_debug("Ensuring standard columns...", "CSV_READ")
  data <- ensure_standard_columns(data)
  log_debug(paste("Standard columns applied - dimensions:", nrow(data), "x", ncol(data)), "CSV_READ")

  log_debug("Setting reactive values...", "CSV_READ")

  # Take state snapshot before data assignment
  if (!is.null(app_state)) {
    debug_state_snapshot("before_csv_data_assignment", app_state, session_id = session_id)
  }

  # PHASE 4: Unified state assignment only - CSV file loading
  data_frame <- as.data.frame(data)
  app_state$data$current_data <- data_frame
  log_debug("✅ Set current_data to unified state", "CSV_READ")
  app_state$data$original_data <- data_frame
  log_debug("✅ Set original_data to unified state", "CSV_READ")
  # PHASE 4B: Unified state assignment only - Set file uploaded flag
  app_state$session$file_uploaded <- TRUE
  # PHASE 4B: Unified state assignment only - Set auto detect flag
  app_state$columns$auto_detect$completed <- FALSE
  # PHASE 4B: Unified state assignment only - Re-enable Anhøj rules when real data is uploaded
  app_state$ui$hide_anhoej_rules <- FALSE

  # NAVIGATION TRIGGER: Increment trigger to notify reactive navigation system
  if (!is.null(app_state$navigation_trigger)) {
    old_trigger <- app_state$navigation_trigger()
    app_state$navigation_trigger(old_trigger + 1)
    new_trigger <- app_state$navigation_trigger()
    log_debug(paste("CSV_READ: navigation_trigger incremented from", old_trigger, "to", new_trigger), "CSV_READ")
  }

  log_debug("✅ Reactive values set successfully", "CSV_READ")

  # ROBUST AUTO-DETECT: Enhanced auto-detection triggering with validation
  log_debug("Setting auto-detect trigger with validation...", "CSV_READ")

  # Validate data suitability for auto-detection
  auto_detect_suitable <- validate_data_for_auto_detect(data, session_id)
  if (auto_detect_suitable$suitable) {
    # PHASE 4B: Unified state assignment only - Set auto detect trigger
    app_state$columns$auto_detect$trigger_needed <- TRUE
    log_debug("✅ Data suitable for auto-detection - trigger set", "CSV_READ")

    debug_log("Auto-detection trigger set successfully", "FILE_UPLOAD_FLOW", level = "INFO",
              context = list(
                data_validation = auto_detect_suitable$validation_results,
                rows = nrow(data),
                columns = ncol(data)
              ),
              session_id = session_id)
  } else {
    log_debug("⚠️ Data not suitable for auto-detection", "CSV_READ")
    app_state$columns$auto_detect$trigger_needed <- FALSE

    debug_log("Auto-detection skipped due to data validation", "FILE_UPLOAD_FLOW", level = "WARNING",
              context = list(
                validation_issues = auto_detect_suitable$issues,
                rows = nrow(data),
                columns = ncol(data)
              ),
              session_id = session_id)

    # Show user notification about auto-detection skip
    showNotification(
      paste("Auto-detection skipped:", paste(auto_detect_suitable$issues, collapse = "; ")),
      type = "warning",
      duration = 8
    )
  }

  # Take state snapshot after all state is set
  if (!is.null(app_state)) {
    debug_state_snapshot("after_csv_upload_complete", app_state, session_id = session_id)
  }

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
  # Unified state: Use centralized state for current data
  active_data_for_export <- app_state$data$current_data

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
        log_error(paste("Excel export failed:", e$message), "FILE_EXPORT")
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
    # Unified state: Use centralized state for file_uploaded
    paste("• Data kilde:", if (app_state$session$file_uploaded) "File Upload" else "Manuel indtastning"),
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

# ENHANCED FILE VALIDATION ===================================================

## Enhanced file validation with comprehensive checks
validate_uploaded_file <- function(file_info, session_id = NULL) {
  errors <- character(0)

  # File existence check
  if (!file.exists(file_info$datapath)) {
    errors <- c(errors, "Uploaded file does not exist or is corrupted")
    return(list(valid = FALSE, errors = errors))
  }

  # File size validation (max 50MB)
  max_size_mb <- 50
  if (file_info$size > max_size_mb * 1024 * 1024) {
    errors <- c(errors, paste("File size exceeds maximum allowed size of", max_size_mb, "MB"))
  }

  # Empty file check
  if (file_info$size == 0) {
    errors <- c(errors, "Uploaded file is empty")
  }

  # File extension validation
  file_ext <- tools::file_ext(file_info$name)
  allowed_extensions <- c("csv", "xlsx", "xls")
  if (!tolower(file_ext) %in% allowed_extensions) {
    errors <- c(errors, paste("File type not supported. Allowed types:", paste(allowed_extensions, collapse = ", ")))
  }

  # Additional validation for specific file types
  if (tolower(file_ext) %in% c("xlsx", "xls")) {
    validation_excel <- validate_excel_file(file_info$datapath)
    if (!validation_excel$valid) {
      errors <- c(errors, validation_excel$errors)
    }
  } else if (tolower(file_ext) == "csv") {
    validation_csv <- validate_csv_file(file_info$datapath)
    if (!validation_csv$valid) {
      errors <- c(errors, validation_csv$errors)
    }
  }

  # Log validation results
  if (length(errors) > 0) {
    debug_log("File validation failed", "FILE_UPLOAD_FLOW", level = "WARNING",
              context = list(
                filename = file_info$name,
                file_size = file_info$size,
                validation_errors = errors
              ),
              session_id = session_id)
  } else {
    debug_log("File validation successful", "FILE_UPLOAD_FLOW", level = "INFO",
              context = list(
                filename = file_info$name,
                file_size = file_info$size,
                file_extension = file_ext
              ),
              session_id = session_id)
  }

  return(list(
    valid = length(errors) == 0,
    errors = errors
  ))
}

## Excel file specific validation
validate_excel_file <- function(file_path) {
  errors <- character(0)

  tryCatch({
    # Check if file can be read
    sheets <- readxl::excel_sheets(file_path)

    if (length(sheets) == 0) {
      errors <- c(errors, "Excel file contains no sheets")
    }

    # If this is a session restore file, check for required sheets
    if (all(c("Data", "Metadata") %in% sheets)) {
      # Validate Data sheet
      data_validation <- tryCatch({
        data <- readxl::read_excel(file_path, sheet = "Data", n_max = 1)
        if (ncol(data) == 0) {
          errors <- c(errors, "Data sheet is empty")
        }
        if (nrow(data) == 0) {
          errors <- c(errors, "Data sheet contains no data rows")
        }
        TRUE
      }, error = function(e) {
        errors <<- c(errors, paste("Cannot read Data sheet:", e$message))
        FALSE
      })

      # Validate Metadata sheet
      metadata_validation <- tryCatch({
        metadata <- readxl::read_excel(file_path, sheet = "Metadata", n_max = 1)
        TRUE
      }, error = function(e) {
        errors <<- c(errors, paste("Cannot read Metadata sheet:", e$message))
        FALSE
      })
    } else {
      # Regular Excel file - validate first sheet
      tryCatch({
        data <- readxl::read_excel(file_path, n_max = 1)
        if (ncol(data) == 0) {
          errors <- c(errors, "Excel file contains no columns")
        }
      }, error = function(e) {
        errors <<- c(errors, paste("Cannot read Excel file:", e$message))
      })
    }

  }, error = function(e) {
    errors <<- c(errors, paste("Excel file is corrupted or invalid:", e$message))
  })

  return(list(
    valid = length(errors) == 0,
    errors = errors
  ))
}

## CSV file specific validation
validate_csv_file <- function(file_path) {
  errors <- character(0)

  tryCatch({
    # Try to read first few lines to validate structure
    sample_data <- readr::read_csv2(
      file_path,
      locale = readr::locale(
        decimal_mark = ",",
        grouping_mark = ".",
        encoding = "ISO-8859-1"
      ),
      n_max = 5,
      show_col_types = FALSE
    )

    if (ncol(sample_data) == 0) {
      errors <- c(errors, "CSV file contains no columns")
    }

    if (nrow(sample_data) == 0) {
      errors <- c(errors, "CSV file contains no data rows")
    }

    # Check for proper column separation
    if (ncol(sample_data) == 1 && nrow(sample_data) > 0) {
      first_value <- as.character(sample_data[1, 1])
      if (grepl("[,;\\t]", first_value)) {
        errors <- c(errors, "CSV file may have incorrect delimiter. Expected semicolon (;) separated values")
      }
    }

  }, error = function(e) {
    if (grepl("invalid", tolower(e$message)) || grepl("encoding", tolower(e$message))) {
      errors <<- c(errors, "CSV file has encoding issues. Try saving as UTF-8 or ISO-8859-1")
    } else {
      errors <<- c(errors, paste("Cannot read CSV file:", e$message))
    }
  })

  return(list(
    valid = length(errors) == 0,
    errors = errors
  ))
}

# ENHANCED ERROR RECOVERY ====================================================

## Enhanced error handling with recovery suggestions
handle_upload_error <- function(error, file_info, session_id = NULL) {
  error_message <- as.character(error$message)
  error_type <- "unknown"
  user_message <- "An unexpected error occurred during file upload"
  suggestions <- character(0)

  # Categorize error types and provide specific guidance
  if (grepl("encoding|locale|character", error_message, ignore.case = TRUE)) {
    error_type <- "encoding"
    user_message <- "File encoding issue detected"
    suggestions <- c(
      "Try saving your file with UTF-8 or ISO-8859-1 encoding",
      "Ensure Danish characters (æ, ø, å) are properly encoded",
      "For Excel files: Save as 'Excel Workbook (.xlsx)' format"
    )
  } else if (grepl("permission|access|locked", error_message, ignore.case = TRUE)) {
    error_type <- "permission"
    user_message <- "File access permission issue"
    suggestions <- c(
      "Close the file in other applications (Excel, etc.)",
      "Check that the file is not read-only",
      "Try copying the file to a different location"
    )
  } else if (grepl("memory|size|allocation", error_message, ignore.case = TRUE)) {
    error_type <- "memory"
    user_message <- "File too large or memory issue"
    suggestions <- c(
      "Try uploading a smaller file",
      "Remove unnecessary columns or rows",
      "Split large datasets into smaller files"
    )
  } else if (grepl("column|header|sheet", error_message, ignore.case = TRUE)) {
    error_type <- "structure"
    user_message <- "File structure issue"
    suggestions <- c(
      "Ensure your file has proper column headers",
      "Check that data is properly organized in rows and columns",
      "For Excel files: Ensure data is in the first sheet or 'Data' sheet"
    )
  } else if (grepl("corrupt|invalid|damaged", error_message, ignore.case = TRUE)) {
    error_type <- "corruption"
    user_message <- "File appears to be corrupted"
    suggestions <- c(
      "Try re-saving the file from the original application",
      "Check if the file opens correctly in Excel or other applications",
      "Try exporting data to a new file"
    )
  }

  # Log detailed error information
  debug_log("Enhanced error handling triggered", "ERROR_HANDLING", level = "ERROR",
            context = list(
              error_type = error_type,
              error_message = error_message,
              filename = file_info$name,
              file_size = file_info$size,
              file_type = file_info$type,
              suggestions = suggestions
            ),
            session_id = session_id)

  # Create comprehensive user notification
  notification_html <- tags$div(
    tags$strong(user_message),
    tags$br(),
    tags$em(paste("Technical details:", error_message)),
    if (length(suggestions) > 0) {
      tags$div(
        tags$br(),
        tags$strong("Suggestions:"),
        tags$ul(
          lapply(suggestions, function(s) tags$li(s))
        )
      )
    }
  )

  showNotification(
    notification_html,
    type = "error",
    duration = 15
  )

  return(list(
    error_type = error_type,
    user_message = user_message,
    suggestions = suggestions
  ))
}

# DATA VALIDATION FOR AUTO-DETECTION ========================================

## Validate data suitability for auto-detection
validate_data_for_auto_detect <- function(data, session_id = NULL) {
  issues <- character(0)
  validation_results <- list()

  # Check data dimensions
  validation_results$rows <- nrow(data)
  validation_results$columns <- ncol(data)

  if (nrow(data) < 2) {
    issues <- c(issues, "Too few data rows (minimum 2 required)")
  }

  if (ncol(data) < 2) {
    issues <- c(issues, "Too few columns (minimum 2 required)")
  }

  # Check for reasonable column names
  col_names <- names(data)
  validation_results$column_names <- col_names

  # Count empty/missing column names
  empty_names <- sum(is.na(col_names) | col_names == "" | grepl("^\\.\\.\\.", col_names))
  validation_results$empty_column_names <- empty_names

  if (empty_names > 0) {
    issues <- c(issues, paste(empty_names, "columns have missing or invalid names"))
  }

  # Check for data content
  has_data_content <- sapply(data, function(col) {
    if (is.numeric(col)) {
      sum(!is.na(col)) > 0
    } else if (is.character(col)) {
      sum(nzchar(col, keepNA = FALSE)) > 0
    } else if (is.logical(col)) {
      sum(!is.na(col)) > 0
    } else {
      sum(!is.na(col)) > 0
    }
  })

  columns_with_data <- sum(has_data_content)
  validation_results$columns_with_data <- columns_with_data

  if (columns_with_data < 2) {
    issues <- c(issues, "Insufficient columns with meaningful data")
  }

  # Check for potential date columns (for X-axis)
  potential_date_columns <- sapply(col_names, function(name) {
    grepl("dato|date|tid|time", tolower(name)) ||
    grepl("^(x|uge|måned|år|dag)", tolower(name))
  })
  validation_results$potential_date_columns <- sum(potential_date_columns)

  # Check for potential numeric columns (for Y-axis)
  potential_numeric_columns <- sapply(data, function(col) {
    if (is.numeric(col)) return(TRUE)
    if (is.character(col)) {
      # Check if character data looks like it could be numeric
      non_empty <- col[nzchar(col, keepNA = FALSE)]
      if (length(non_empty) == 0) return(FALSE)
      # Try to parse some values as numbers (Danish format)
      sample_size <- min(10, length(non_empty))
      sample_values <- non_empty[1:sample_size]
      parsed <- suppressWarnings(parse_danish_number(sample_values))
      return(sum(!is.na(parsed)) > 0)
    }
    return(FALSE)
  })
  validation_results$potential_numeric_columns <- sum(potential_numeric_columns)

  if (sum(potential_numeric_columns) < 1) {
    issues <- c(issues, "No suitable numeric columns found for Y-axis")
  }

  # Overall suitability assessment
  suitable <- length(issues) == 0

  # Log validation results
  debug_log("Data validation for auto-detection completed", "FILE_UPLOAD_FLOW", level = "INFO",
            context = list(
              suitable = suitable,
              validation_results = validation_results,
              issues = if (length(issues) > 0) issues else "none"
            ),
            session_id = session_id)

  return(list(
    suitable = suitable,
    issues = issues,
    validation_results = validation_results
  ))
}

# EDGE CASE HANDLING =========================================================

## Enhanced data cleaning and preprocessing
preprocess_uploaded_data <- function(data, file_info, session_id = NULL) {
  log_debug("Starting data preprocessing...", "DATA_PREPROCESSING")

  original_dims <- c(nrow(data), ncol(data))
  cleaning_log <- list()

  # Handle completely empty rows
  if (nrow(data) > 0) {
    empty_rows <- apply(data, 1, function(row) all(is.na(row) | row == "" | row == " "))
    if (sum(empty_rows) > 0) {
      data <- data[!empty_rows, ]
      cleaning_log$empty_rows_removed <- sum(empty_rows)
      log_debug(paste("Removed", sum(empty_rows), "completely empty rows"), "DATA_PREPROCESSING")
    }
  }

  # Handle columns with only missing values
  if (ncol(data) > 0) {
    empty_cols <- sapply(data, function(col) all(is.na(col) | col == "" | col == " "))
    if (sum(empty_cols) > 0) {
      data <- data[, !empty_cols, drop = FALSE]
      cleaning_log$empty_columns_removed <- sum(empty_cols)
      log_debug(paste("Removed", sum(empty_cols), "completely empty columns"), "DATA_PREPROCESSING")
    }
  }

  # Clean column names
  if (ncol(data) > 0) {
    original_names <- names(data)
    cleaned_names <- make.names(original_names, unique = TRUE)

    # Replace problematic characters with readable alternatives
    cleaned_names <- gsub("\\.\\.+", "_", cleaned_names)  # Multiple dots to underscore
    cleaned_names <- gsub("^X", "Column_", cleaned_names)  # R's automatic X prefix
    cleaned_names <- gsub("\\.$", "", cleaned_names)  # Trailing dots

    if (!identical(original_names, cleaned_names)) {
      names(data) <- cleaned_names
      cleaning_log$column_names_cleaned <- TRUE
      log_debug("Cleaned column names for R compatibility", "DATA_PREPROCESSING")
    }
  }

  final_dims <- c(nrow(data), ncol(data))
  cleaning_log$dimension_change <- list(
    original = original_dims,
    final = final_dims
  )

  # Log preprocessing results
  debug_log("Data preprocessing completed", "FILE_UPLOAD_FLOW", level = "INFO",
            context = list(
              filename = file_info$name,
              cleaning_log = cleaning_log,
              original_dimensions = original_dims,
              final_dimensions = final_dims
            ),
            session_id = session_id)

  log_debug(paste("Data preprocessing completed - dimensions:", final_dims[1], "x", final_dims[2]), "DATA_PREPROCESSING")

  return(list(
    data = data,
    cleaning_log = cleaning_log
  ))
}
