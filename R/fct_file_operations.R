# server_file_upload.R
# Server logik til håndtering af fil uploads og import

# Dependencies ----------------------------------------------------------------
# Bruger readxl og readr til fil-import

# UPLOAD HÅNDTERING ===========================================================

## Setup fil upload funktionalitet
setup_file_upload <- function(input, output, session, app_state, emit, ui_service = NULL) {
  # Unified state: App state is always available

  # File upload handler
  shiny::observeEvent(input$data_file, {
    # Enhanced debug tracking for comprehensive testing
    debug_user_interaction("file_upload_initiated",
                          list(filename = input$data_file$name,
                               size = input$data_file$size,
                               type = input$data_file$type),
                          session$token)

    shiny::req(input$data_file)

    # Start workflow tracer for file upload
    upload_tracer <- debug_workflow_tracer("file_upload_workflow", app_state, session$token)
    upload_tracer$step("upload_initiated")


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

      shiny::showNotification(
        paste("File validation failed:", paste(validation_result$errors, collapse = "; ")),
        type = "error",
        duration = 8
      )
      return()
    }

    upload_tracer$step("file_validation_complete")

    # Show loading indicator (replaced waiter with simple logging)

    # Close upload modal automatically
    on.exit(
      {
        shiny::removeModal()
      },
      add = TRUE
    )

    upload_tracer$step("state_management_setup")

    # Unified state assignment only - Set table updating flag
    app_state$data$updating_table <- TRUE

    debug_log("File upload state flags set", "FILE_UPLOAD_FLOW", level = "TRACE",
              context = list(updating_table = TRUE),
              session_id = session$token)
    on.exit(
      {
        # Unified state assignment only - Clear table updating flag
        app_state$data$updating_table <- FALSE
      },
      add = TRUE
    )

    # Sikker path validation - forhindrer path traversal attacks
    file_path <- normalizePath(input$data_file$datapath, mustWork = TRUE)

    # Verificer at filen befinder sig i et sikkert directory (tempdir eller upload area)
    safe_dirs <- c(tempdir(), dirname(tempfile()))
    # Normaliser både fil-sti og safe directories for at håndtere macOS symlinks korrekt
    normalized_safe_dirs <- vapply(safe_dirs, function(dir) normalizePath(dir, mustWork = FALSE), character(1))
    safe_path <- any(vapply(normalized_safe_dirs, function(dir) startsWith(file_path, dir), logical(1)))

    if (!safe_path) {
      log_error(
        message = paste("Path traversal attempt detected:", file_path),
        component = "[SECURITY]"
      )
      stop("Sikkerhedsfejl: Ugyldig fil sti")
    }

    file_ext <- tools::file_ext(input$data_file$name)


    if (file.exists(file_path)) {
      file_info <- file.info(file_path)
    }

    safe_operation(
      operation_name = "Fil upload processing",
      code = {
        upload_tracer$step("file_processing_started")

        if (file_ext %in% c("xlsx", "xls")) {
          debug_log("Starting Excel file processing", "FILE_UPLOAD_FLOW", level = "INFO", session_id = session$token)
          handle_excel_upload(file_path, session, app_state, emit, ui_service)
          upload_tracer$step("excel_processing_complete")
        } else {
          debug_log("Starting CSV file processing", "FILE_UPLOAD_FLOW", level = "INFO", session_id = session$token)
          handle_csv_upload(file_path, app_state, session$token, emit)
          upload_tracer$step("csv_processing_complete")
        }

        # Complete workflow tracing
        upload_tracer$complete("file_upload_workflow_complete")
        debug_log("File upload workflow completed successfully", "FILE_UPLOAD_FLOW", level = "INFO", session_id = session$token)
      },
      error_type = "network",
      emit = emit,
      app_state = app_state,
      session = session,
      show_user = TRUE
    )

  })

  # UNIFIED EVENT SYSTEM: Auto-detection is now handled by data_loaded events
  # The event system automatically triggers auto-detection when new data is loaded
}

## Håndter Excel fil upload
handle_excel_upload <- function(file_path, session, app_state, emit, ui_service = NULL) {
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
    # Dual-state sync during migration - session restore
    data_frame <- as.data.frame(data)
    set_current_data(app_state, data_frame)
    app_state$data$original_data <- data_frame

    # Emit data_loaded event to trigger unified event system
    emit$data_loaded()

    # Unified state assignment only - Set file uploaded flag
    app_state$session$file_uploaded <- TRUE
    # Unified state assignment only - Set auto detect completed (skip since we have session info)
    app_state$columns$auto_detect$completed <- TRUE
    # Unified state assignment only - Re-enable Anhøj rules when real data is uploaded
    app_state$ui$hide_anhoej_rules <- FALSE

    # NAVIGATION TRIGGER: Emit navigation changed event to update reactive components
    emit$navigation_changed()

    # Restore metadata with delay to ensure UI is ready
    shiny::invalidateLater(500)
    shiny::isolate({
      if (!is.null(ui_service)) {
        restore_metadata(session, metadata, ui_service)
      } else {
      }
    })

    shiny::showNotification(
      paste("Komplet session importeret:", nrow(data), "rækker,", ncol(data), "kolonner + konfiguration"),
      type = "message",
      duration = 4
    )
  } else {
    # Standard Excel file
    data <- readxl::read_excel(file_path, col_names = TRUE)

    # Ensure standard columns are present and in correct order
    data <- ensure_standard_columns(data)

    # Dual-state sync during migration - Excel file loading
    data_frame <- as.data.frame(data)
    set_current_data(app_state, data_frame)
    app_state$data$original_data <- data_frame

    # Emit data_loaded event to trigger unified event system
    emit$data_loaded()

    # Unified state assignment only - Set file uploaded flag
    app_state$session$file_uploaded <- TRUE
    # Unified state assignment only - Set auto detect flag
    app_state$columns$auto_detect$completed <- FALSE
    # Unified state assignment only - Re-enable Anhøj rules when real data is uploaded
    app_state$ui$hide_anhoej_rules <- FALSE

    # NAVIGATION TRIGGER: Navigation events are now handled by the unified event system
    emit$navigation_changed()


    shiny::showNotification(
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
handle_csv_upload <- function(file_path, app_state, session_id = NULL, emit = NULL) {

  debug_log("CSV upload processing started", "FILE_UPLOAD_FLOW", level = "INFO",
            context = list(file_path = file_path),
            session_id = session_id)

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


  debug_log("CSV data loaded successfully", "FILE_UPLOAD_FLOW", level = "INFO",
            context = list(
              rows = nrow(data),
              columns = ncol(data),
              column_names = names(data)
            ),
            session_id = session_id)

  # ENHANCED DATA PREPROCESSING: Clean and validate data
  preprocessing_result <- preprocess_uploaded_data(
    data,
    list(name = basename(file_path), size = file.info(file_path)$size),
    session_id
  )
  data <- preprocessing_result$data

  # Show user notification if significant cleaning occurred
  if (!is.null(preprocessing_result$cleaning_log)) {
    cleaning_messages <- character(0)
    if (!is.null(preprocessing_result$cleaning_log$empty_rows_removed)) {
      cleaning_messages <- c(cleaning_messages,
        paste(preprocessing_result$cleaning_log$empty_rows_removed, "empty rows removed"))
    }
    # DISABLED: empty columns removal is no longer performed
    # if (!is.null(preprocessing_result$cleaning_log$empty_columns_removed)) {
    #   cleaning_messages <- c(cleaning_messages,
    #     paste(preprocessing_result$cleaning_log$empty_columns_removed, "empty columns removed"))
    # }
    if (!is.null(preprocessing_result$cleaning_log$column_names_cleaned)) {
      cleaning_messages <- c(cleaning_messages, "column names cleaned")
    }

    if (length(cleaning_messages) > 0) {
      shiny::showNotification(
        paste("Data cleaned:", paste(cleaning_messages, collapse = ", ")),
        type = "message",
        duration = 5
      )
    }
  }

  # Ensure standard columns are present and in correct order
  data <- ensure_standard_columns(data)


  # Take state snapshot before data assignment
  if (!is.null(app_state)) {
    debug_state_snapshot("before_csv_data_assignment", app_state, session_id = session_id)
  }

  # Unified state assignment only - CSV file loading
  data_frame <- as.data.frame(data)

  # Enhanced debugging: Data structure analysis before assignment


  # Enhanced state change tracking
  debug_state_change("CSV_UPLOAD", "app_state$data$current_data",
                    app_state$data$current_data, data_frame,
                    "file_upload_processing", session_id)

  set_current_data(app_state, data_frame)
  app_state$data$original_data <- data_frame

  # Verify state assignment success
  if (is.null(app_state$data$current_data)) {
    warning("State assignment failed: current_data is NULL")
  }

  # Emit data_loaded event to trigger unified event system
  emit$data_loaded()

  # Unified state assignment - Set file uploaded flag
  app_state$session$file_uploaded <- TRUE
  # Unified state assignment - Set auto detect flag
  app_state$columns$auto_detect$completed <- FALSE
  # Unified state assignment - Re-enable Anhøj rules when real data is uploaded
  app_state$ui$hide_anhoej_rules <- FALSE

  # NAVIGATION TRIGGER: Navigation events are now handled by the unified event system
  emit$navigation_changed()


  # ROBUST AUTO-DETECT: Enhanced auto-detection triggering with validation

  debug_log("Data loaded event emitted successfully", "FILE_UPLOAD_FLOW", level = "INFO",
            context = list(
              rows = nrow(data),
              columns = ncol(data),
              event_system = "unified_event_bus"
            ),
            session_id = session_id)

  # Take state snapshot after all state is set
  if (!is.null(app_state)) {
    debug_state_snapshot("after_csv_upload_complete", app_state, session_id = session_id)
  }

  shiny::showNotification(
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

# ENHANCED FILE VALIDATION ===================================================

## Enhanced file validation with comprehensive checks
validate_uploaded_file <- function(file_info, session_id = NULL) {
  errors <- character(0)

  # File existence check
  if (!file.exists(file_info$datapath)) {
    errors <- c(errors, "Uploaded file does not exist or is corrupted")
    return(list(valid = FALSE, errors = errors))
  }

  # Enhanced file size validation med DoS protection
  max_size_mb <- 50
  if (file_info$size > max_size_mb * 1024 * 1024) {
    # Log potential DoS attempt for large files
    if (file_info$size > 100 * 1024 * 1024) { # Over 100MB is suspicious
      log_warn(
        component = "[FILE_SECURITY]",
        message = "Extremely large file upload attempt - potential DoS",
        details = list(
          filename = file_info$name,
          size_mb = round(file_info$size / (1024 * 1024), 2),
          max_allowed_mb = max_size_mb
        )
      )
    }
    errors <- c(errors, paste("Filstørrelse overskrider maksimum på", max_size_mb, "MB"))
  }

  # Additional DoS protection: Row count limits for data files
  if (tolower(tools::file_ext(file_info$name)) == "csv") {
    # Quick row count check for CSV files uden at læse al data
    safe_operation(
      "Quick CSV row count validation",
      code = {
        # Count lines i file (approximation)
        con <- file(file_info$datapath, "r")
        line_count <- 0
        while (length(readLines(con, n = 1)) > 0) {
          line_count <- line_count + 1
          if (line_count > 100000) { # Stop counting at reasonable limit
            break
          }
        }
        close(con)

        # Maximum 50k rows for reasonable performance
        if (line_count > 50000) {
          log_warn(
            component = "[FILE_SECURITY]",
            message = "Large row count detected - performance risk",
            details = list(
              filename = file_info$name,
              estimated_rows = line_count
            )
          )
          errors <- c(errors, "CSV fil har for mange rækker (maksimum 50,000)")
        }
      },
      fallback = function(e) {
        # If we can't count, allow file but log warning
        log_warn(
          component = "[FILE_VALIDATION]",
          message = "Could not validate row count",
          details = list(filename = file_info$name, error = e$message)
        )
      }
    )
  }

  # Empty file check
  if (file_info$size == 0) {
    errors <- c(errors, "Uploaded file is empty")
  }

  # Enhanced file extension validation med security hardening
  file_ext <- tools::file_ext(file_info$name)

  # Brug centraliseret validation med sikkerhedshardening
  if (!validate_file_extension(file_ext)) {
    errors <- c(errors, "Ikke-tilladt filtype. Kun CSV, Excel tilladt")
  }

  # MIME type validation - sikr at file extension matcher content type
  if (length(errors) == 0) {
    # Basic MIME type check baseret på file header
    file_header <- readBin(file_info$datapath, what = "raw", n = 8)

    # Check for common malicious patterns eller uoverensstemmelser
    is_valid_mime <- switch(tolower(trimws(file_ext)),
      "csv" = {
        # CSV filer bør ikke have binary headers
        !any(file_header[1:4] == as.raw(c(0x50, 0x4B, 0x03, 0x04))) # ZIP header (Excel)
      },
      "xlsx" = {
        # Excel files should have ZIP/OLE header
        identical(file_header[1:4], as.raw(c(0x50, 0x4B, 0x03, 0x04))) # ZIP header
      },
      "xls" = {
        # Legacy Excel files have OLE header
        identical(file_header[1:8], as.raw(c(0xD0, 0xCF, 0x11, 0xE0, 0xA1, 0xB1, 0x1A, 0xE1))) ||
        identical(file_header[1:4], as.raw(c(0x09, 0x08, 0x10, 0x00))) ||
        identical(file_header[1:4], as.raw(c(0x50, 0x4B, 0x03, 0x04)))
      },
      FALSE  # Default - reject if unknown
    )

    if (!is_valid_mime) {
      log_warn(
        component = "[FILE_SECURITY]",
        message = "MIME type mismatch detected - potential file masquerading",
        details = list(
          filename = file_info$name,
          extension = file_ext,
          header_bytes = as.character(file_header[1:8])
        )
      )
      errors <- c(errors, "Filtype stemmer ikke overens med indhold")
    }
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

  safe_operation(
    "Validate Excel file structure",
    code = {
      # Check if file can be read
      sheets <- readxl::excel_sheets(file_path)

      if (length(sheets) == 0) {
        errors <- c(errors, "Excel file contains no sheets")
      }

      # If this is a session restore file, check for required sheets
      if (all(c("Data", "Metadata") %in% sheets)) {
        # Validate Data sheet
        data_validation <- safe_operation(
          "Validate Excel Data sheet",
          code = {
            data <- readxl::read_excel(file_path, sheet = "Data", n_max = 1)
            if (ncol(data) == 0) {
              errors <- c(errors, "Data sheet is empty")
            }
            if (nrow(data) == 0) {
              errors <- c(errors, "Data sheet contains no data rows")
            }
            TRUE
          },
          fallback = function(e) {
            errors <<- c(errors, paste("Cannot read Data sheet:", e$message))
            FALSE
          },
          error_type = "processing"
        )

        # Validate Metadata sheet
        metadata_validation <- safe_operation(
          "Validate Excel Metadata sheet",
          code = {
            metadata <- readxl::read_excel(file_path, sheet = "Metadata", n_max = 1)
            TRUE
          },
          fallback = function(e) {
            errors <<- c(errors, paste("Cannot read Metadata sheet:", e$message))
            FALSE
          },
          error_type = "processing"
        )
      } else {
        # Regular Excel file - validate first sheet
        safe_operation(
          "Validate regular Excel file",
          code = {
            data <- readxl::read_excel(file_path, n_max = 1)
            if (ncol(data) == 0) {
              errors <- c(errors, "Excel file contains no columns")
            }
          },
          fallback = function(e) {
            errors <<- c(errors, paste("Cannot read Excel file:", e$message))
          },
          error_type = "processing"
        )
      }
    },
    fallback = function(e) {
      errors <<- c(errors, paste("Excel file is corrupted or invalid:", e$message))
    },
    error_type = "processing"
  )

  return(list(
    valid = length(errors) == 0,
    errors = errors
  ))
}

## CSV file specific validation
validate_csv_file <- function(file_path) {
  errors <- character(0)

  safe_operation(
    "Validate CSV file structure",
    code = {
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
    },
    fallback = function(e) {
      if (grepl("invalid", tolower(e$message)) || grepl("encoding", tolower(e$message))) {
        errors <<- c(errors, "CSV file has encoding issues. Try saving as UTF-8 or ISO-8859-1")
      } else {
        errors <<- c(errors, paste("Cannot read CSV file:", e$message))
      }
    },
    error_type = "processing"
  )

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
  notification_html <- shiny::tags$div(
    shiny::tags$strong(user_message),
    shiny::tags$br(),
    shiny::tags$em(paste("Technical details:", error_message)),
    if (length(suggestions) > 0) {
      shiny::tags$div(
        shiny::tags$br(),
        shiny::tags$strong("Suggestions:"),
        shiny::tags$ul(
          purrr::map(suggestions, ~ shiny::tags$li(.x))
        )
      )
    }
  )

  shiny::showNotification(
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

  # Check for data content - optimized med vectorized base R operations
  has_data_content <- vapply(data, function(col) {
    if (is.numeric(col)) {
      sum(!is.na(col)) > 0
    } else if (is.character(col)) {
      sum(nzchar(col, keepNA = FALSE)) > 0
    } else if (is.logical(col)) {
      sum(!is.na(col)) > 0
    } else {
      sum(!is.na(col)) > 0
    }
  }, logical(1))

  columns_with_data <- sum(has_data_content)
  validation_results$columns_with_data <- columns_with_data

  if (columns_with_data < 2) {
    issues <- c(issues, "Insufficient columns with meaningful data")
  }

  # Check for potential date columns (for X-axis) - optimized med vectorized base R
  col_names_lower <- tolower(col_names)
  potential_date_columns <- grepl("dato|date|tid|time", col_names_lower) |
                           grepl("^(x|uge|måned|år|dag)", col_names_lower)
  validation_results$potential_date_columns <- sum(potential_date_columns)

  # Check for potential numeric columns (for Y-axis) - optimized med vectorized base R
  potential_numeric_columns <- vapply(data, function(col) {
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
  }, logical(1))
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

  original_dims <- c(nrow(data), ncol(data))
  cleaning_log <- list()

  # Data quality analysis before preprocessing - optimized med vectorized base R
  na_counts <- vapply(data, function(col) sum(is.na(col)), integer(1))
  # Fix: Only check character columns for empty strings to avoid NA-warnings
  empty_counts <- vapply(data, function(col) {
    if (is.character(col)) {
      sum(trimws(col) == "", na.rm = TRUE)
    } else {
      0L
    }
  }, integer(1))

  # Handle completely empty rows using tidyverse approach
  if (nrow(data) > 0) {
    # Count empty rows before filtering
    empty_rows_count <- data |>
      dplyr::filter(dplyr::if_all(dplyr::everything(), ~ {
        is.na(.x) | (is.character(.x) & stringr::str_trim(.x) == "")
      })) |>
      nrow()

    if (empty_rows_count > 0) {
      # Filter out empty rows
      data <- data |>
        dplyr::filter(!dplyr::if_all(dplyr::everything(), ~ {
          is.na(.x) | (is.character(.x) & stringr::str_trim(.x) == "")
        }))
      cleaning_log$empty_rows_removed <- empty_rows_count
    }
  }

  # DISABLED: Handle columns with only missing values
  # Previously automatically removed empty columns, but users want to preserve them
  # Tomme kolonner bevares nu som ønsket af brugerne

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
    }
  }

  final_dims <- c(nrow(data), ncol(data))
  cleaning_log$dimension_change <- list(
    original = original_dims,
    final = final_dims
  )

  # Enhanced final analysis and logging


  # Data quality check after preprocessing
  if (nrow(data) > 0 && ncol(data) > 0) {
    # Check for columns with all NA values - optimized med vectorized base R
    all_na_cols <- vapply(data, function(col) all(is.na(col)), logical(1))

    # Check for potential numeric columns - optimized med vectorized base R
    potential_numeric <- vapply(data, function(col) {
      if (is.character(col)) {
        numeric_values <- suppressWarnings(as.numeric(col))
        sum(!is.na(numeric_values)) > 0
      } else {
        is.numeric(col)
      }
    }, logical(1))
  }

  # Log preprocessing results
  debug_log("Data preprocessing completed", "FILE_UPLOAD_FLOW", level = "INFO",
            context = list(
              filename = file_info$name,
              cleaning_log = cleaning_log,
              original_dimensions = original_dims,
              final_dimensions = final_dims
            ),
            session_id = session_id)


  return(list(
    data = data,
    cleaning_log = cleaning_log
  ))
}
