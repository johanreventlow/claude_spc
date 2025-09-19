# server_column_management.R
# Server logik for kolonnehåndtering inklusive auto-detektion og validering

# Dependencies ----------------------------------------------------------------

# KOLONNEHÅNDTERING SETUP ====================================================

#' Opsæt kolonnehåndtering for SPC app
#'
#' Hovedfunktion der opsætter al server-side logik relateret til kolonne-management,
#' inklusive auto-detektion, validering og reactive observers for kolonnevalg.
#' Understøtter både legacy values-baseret og ny centraliseret state management.
#'
#' @param input Shiny input object med brugerinteraktioner
#' @param output Shiny output object for rendering
#' @param session Shiny session object for server kommunikation
#' @param values Reactive values list med app state (legacy system)
#' @param app_state List med centraliseret app state (Phase 4 system), optional
#'
#' @details
#' Funktionen opsætter følgende observers:
#' \itemize{
#'   \item Kolonneopdatering ved data ændringer
#'   \item Auto-detektion trigger ved file upload
#'   \item UI synkronisering efter auto-detektion
#'   \item Fejlhåndtering og user feedback
#' }
#'
#' PHASE 4 compatibility: Funktionen detekterer automatisk om centraliseret
#' state management er tilgængeligt og tilpasser sig entsprechend.
#'
#' @return NULL (side effects via observers)
#'
#' @examples
#' \dontrun{
#' # I Shiny server function:
#' setup_column_management(input, output, session, values, app_state)
#' }
#'
#' @seealso \code{\link{autodetect_engine}}, \code{\link{ensure_standard_columns}}
setup_column_management <- function(input, output, session, app_state, emit) {
  log_debug_block("COLUMN_MGMT", "Setting up column management")
  log_debug("Received app_state environment address:", capture.output(print(app_state)), .context = "COLUMN_MGMT")

  # PHASE 4: Centralized state is now always available
  log_debug("Centralized state available for column management", .context = "PHASE4")

  # LEGACY: Column choices update observer moved to unified event system
  # Now handled by emit$data_changed() -> update_column_choices_unified()
  # CONVERTED: Direct reactive observation removed in favor of event-driven pattern

  # LEGACY: Auto-detection trigger observer removed - now handled by unified event system
  # Manual auto-detection is triggered via emit$auto_detection_started() in the event system
  # The observeEvent(app_state$events$data_loaded) -> emit$auto_detection_started() chain
  # handles all auto-detection triggering automatically through the unified event architecture

  # TEST MODE: Now handled by unified event system in utils_event_system.R
  # Legacy test mode observer removed - replaced by emit$test_mode_ready() pattern

  # UNIFIED EVENT SYSTEM: File upload auto-detection triggers are now handled by data_loaded events
  # The emit$data_loaded() -> observeEvent(app_state$events$data_loaded) -> emit$auto_detection_started()
  # chain handles all auto-detection triggering automatically

  # UNIFIED EVENT SYSTEM: File upload triggers are now handled by data_loaded events
  # The event system automatically handles auto-detection when data is loaded

  # UNIFIED EVENT SYSTEM: Auto-detection is now handled by event listeners in utils_event_system.R
  # The observeEvent(app_state$events$auto_detection_started) handles all auto-detection logic

  # UNIFIED EVENT SYSTEM: Auto-detection and UI sync are now handled through events
  # emit$auto_detection_started() triggers auto-detection -> emit$ui_sync_needed() -> sync_ui_with_columns_unified()
  # The complete UI sync logic is implemented in utils_event_system.R
  log_debug("Auto-detection and UI sync handled by unified event system", .context = "AUTODETECT_SETUP")

  # Auto-detekterings knap handler - kører altid når bruger trykker
  observeEvent(input$auto_detect_columns, {
    # FASE 3: Use event-driven manual trigger for consistency
    safe_operation(
      "Manual auto-detection trigger",
      code = {
        emit$manual_autodetect_button()  # This triggers the event listener with frozen state bypass
      },
      fallback = NULL,
      session = session,
      show_user = TRUE,
      error_type = "processing",
      emit = emit,
      app_state = app_state
    )
  })

  # Kolonnevaliderings output
  output$column_validation_messages <- renderUI({
    # PHASE 4: Use unified state management
    current_data_check <- app_state$data$current_data
    req(current_data_check)

    if ((is.null(input$x_column) || input$x_column == "") ||
      (is.null(input$y_column) || input$y_column == "")) {
      return(NULL)
    }

    chart_type <- get_qic_chart_type(if (is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type)
    warnings <- character(0)

    # Tjek om Y-kolonne er numerisk
    if (!is.null(input$y_column) && input$y_column != "" && input$y_column %in% names(current_data_check)) {
      y_data <- current_data_check[[input$y_column]]
      if (!is.numeric(y_data)) {
        numeric_test <- parse_danish_number(y_data)
        if (sum(!is.na(numeric_test)) < length(y_data) * 0.8) {
          warnings <- c(warnings, paste("Y-kolonne '", input$y_column, "' er ikke numerisk"))
        }
      }
    }

    # Tjek P/U chart krav
    if (chart_type %in% c("p", "pp", "u", "up")) {
      if (is.null(input$n_column) || input$n_column == "") {
        warnings <- c(warnings, paste("Chart type", chart_type, "kræver en nævner-kolonne (N)"))
      } else if (input$n_column %in% names(current_data_check)) {
        n_data <- current_data_check[[input$n_column]]
        if (!is.numeric(n_data)) {
          numeric_test <- parse_danish_number(n_data)
          if (sum(!is.na(numeric_test)) < length(n_data) * 0.8) {
            warnings <- c(warnings, paste("Nævner-kolonne '", input$n_column, "' er ikke numerisk"))
          }
        }
      }
    }

    # Tjek om samme kolonne er valgt flere gange
    selected_cols <- c(input$x_column, input$y_column, input$n_column)
    selected_cols <- selected_cols[!is.null(selected_cols) & selected_cols != ""]

    if (length(selected_cols) != length(unique(selected_cols))) {
      warnings <- c(warnings, "Samme kolonne kan ikke bruges til flere formål")
    }

    # Vis resultater
    if (length(warnings) > 0) {
      div(
        class = "alert alert-warning",
        style = "font-size: 0.85rem; padding: 8px; margin: 5px 0;",
        icon("exclamation-triangle"),
        strong(" Kolonne advarsler:"),
        tags$ul(
          style = "margin: 5px 0; padding-left: 20px;",
          lapply(warnings, function(warn) tags$li(warn))
        )
      )
    } else if (length(selected_cols) >= 2) {
      div(
        class = "alert alert-success",
        style = "font-size: 0.85rem; padding: 8px; margin: 5px 0;",
        icon("check-circle"),
        strong(" Kolonner valideret! "),
        sprintf("Klar til %s chart", chart_type)
      )
    }
  })

  # Redigér kolonnenavne modal
  observeEvent(input$edit_column_names, {
    show_column_edit_modal(session, app_state)
  })

  # Bekræft kolonnenavn ændringer
  observeEvent(input$confirm_column_names, {
    handle_column_name_changes(input, session, app_state, emit)
  })

  # Tilføj kolonne
  observeEvent(input$add_column, {
    show_add_column_modal()
  })

  observeEvent(input$confirm_add_col, {
    handle_add_column(input, session, app_state, emit)
  })

  # UNIFIED EVENT SYSTEM: No longer returning autodetect_trigger
  # Auto-detection is handled through emit$auto_detection_started() events
  log_debug("Auto-detection now handled by unified event system", .context = "COLUMN_MGMT")
}

# AUTO-DETEKTION FUNKTIONER ==================================================

# NOTE: detect_columns_name_only function has been replaced by autodetect_engine
# with strategy = "name_only". All production code now uses the unified autodetect_engine.

## Auto-detekter og opdater kolonner
# Automatisk detektion af kolonnetyper baseret på data indhold
auto_detect_and_update_columns <- function(input, session, app_state = NULL, emit = NULL) {
  # Get session ID for debugging
  session_id <- if (!is.null(session)) session$token else NULL

  # Start auto-detect workflow tracing
  autodetect_tracer <- debug_workflow_tracer("auto_detect_columns", app_state, session_id)
  autodetect_tracer$step("auto_detect_initiated")

  log_debug_block("AUTO_DETECT_FUNC", "Starting auto_detect_and_update_columns")
  log_debug("app_state hash:", if(!is.null(app_state)) digest::digest(app_state$data$current_data) else "NULL", .context = "AUTO_DETECT_FUNC")

  debug_log("Auto-detect columns process started", "AUTO_DETECT_FLOW", level = "INFO",
            context = list(
              use_centralized_state = !is.null(app_state)
            ),
            session_id = session_id)

  # PHASE 4: Centralized state availability check
  use_centralized_state <- !is.null(app_state)
  if (use_centralized_state) {
    log_debug("Centralized state available for auto-detect function", .context = "PHASE4")
  }

  autodetect_tracer$step("data_source_validation")

  # PHASE 4: Use unified state management for current_data access
  current_data_check <- if (!is.null(app_state)) {
    app_state$data$current_data
  } else {
    NULL
  }
  req(current_data_check)

  data <- current_data_check

  debug_log("Data source validated for auto-detect", "AUTO_DETECT_FLOW", level = "TRACE",
            context = list(
              data_source = if (!is.null(app_state)) "app_state" else "values",
              data_available = !is.null(current_data_check)
            ),
            session_id = session_id)

  # DEBUGGING: Verificer hvilke data auto-detection faktisk læser
  log_debug("DATA SOURCE VERIFICATION:", .context = "AUTO_DETECT_FUNC")
  log_debug("- use_centralized_state:", use_centralized_state, .context = "AUTO_DETECT_FUNC")
  if (!is.null(app_state)) {
    log_debug("- Reading from app_state$data$current_data", .context = "AUTO_DETECT_FUNC")
    if (!is.null(app_state$data$current_data)) {
      log_debug_kv(
        data_dims = paste(dim(app_state$data$current_data), collapse = "x"),
        data_cols = paste(names(app_state$data$current_data), collapse = ", "),
        .context = "AUTO_DETECT_FUNC"
      )
    }
  } else {
    # Using unified state management only
    log_debug("- Using unified state management for data access", .context = "AUTO_DETECT_FUNC")
  }

  autodetect_tracer$step("data_inspection_started")

  # Tjek for tomme datasæt - skift til name-only mode
  if (is.null(data) || ncol(data) == 0) {
    log_debug("⚠️ Tomt datasæt (NULL eller ingen kolonner) - afbryder auto-detection", .context = "AUTO_DETECT_FUNC")
    debug_log("Auto-detect aborted - empty dataset", "AUTO_DETECT_FLOW", level = "WARN",
              context = list(reason = "null_or_no_columns"),
              session_id = session_id)
    autodetect_tracer$complete("auto_detect_aborted_empty_dataset")
    return(NULL)
  }

  # Name-only mode for datasæt med 0 rækker men med kolonneoverskrifter
  name_only_mode <- nrow(data) == 0
  if (name_only_mode) {
    log_debug("Tomt datasæt (0 rækker) - skifter til name-only detection", .context = "AUTO_DETECT_FUNC")
    debug_log("Switching to name-only detection mode", "AUTO_DETECT_FLOW", level = "INFO",
              context = list(rows = 0, columns = ncol(data)),
              session_id = session_id)
    autodetect_tracer$step("name_only_mode_activated")
  }

  col_names <- names(data)

  log_debug_kv(
    data_dimensions = paste(dim(data), collapse = "x"),
    column_names = paste(col_names, collapse = ", "),
    .context = "AUTO_DETECT_FUNC"
  )

  debug_log("Data analysis for auto-detect", "AUTO_DETECT_FLOW", level = "TRACE",
            context = list(
              rows = nrow(data),
              columns = ncol(data),
              column_names = col_names,
              name_only_mode = name_only_mode
            ),
            session_id = session_id)

  # NAME-ONLY DETECTION for tomme datasaet
  if (name_only_mode) {
    autodetect_tracer$step("executing_name_only_detection")
    # Use unified autodetect_engine instead of detect_columns_name_only
    # För empty dataset (name_only_mode) behöver vi skapa et dummy dataset med kolonnerne
    dummy_data <- data.frame(matrix(NA, nrow = 0, ncol = length(col_names)))
    names(dummy_data) <- col_names

    result <- autodetect_engine(
      data = dummy_data,
      trigger_type = "session_start",  # Name-only detection behandles som session start
      app_state = app_state,
      emit = emit
    )
    autodetect_tracer$complete("auto_detect_name_only_complete")
    return(result)
  }

  autodetect_tracer$step("full_content_analysis_started")

  # Forbedret detektering af potentielle dato-kolonner
  # Evaluer ALLE kolonner og find den bedste dato-kandidat

  log_debug("Starting date candidate analysis...", .context = "AUTO_DETECT_FUNC")
  date_candidates <- list()

  for (col_name in col_names) {
    log_debug("Analyzing column:", col_name, .context = "AUTO_DETECT_FUNC")
    col_data <- data[[col_name]]

    # Skip ikke-dato kolonner baseret på navn
    if (grepl("^(nr|id|count|antal|total|sum)$", col_name, ignore.case = TRUE)) {
      log_debug("Skipping", col_name, "- excluded by name pattern", .context = "AUTO_DETECT_FUNC")
      next
    }

    # Skip kolonner der kun indeholder NA værdier
    if (all(is.na(col_data)) || length(col_data) == 0) {
      log_debug("Skipping", col_name, "- all NA or empty", .context = "AUTO_DETECT_FUNC")
      next
    }

    candidate <- list(
      name = col_name,
      score = 0,
      type = "unknown",
      success_rate = 0,
      reason = ""
    )

    # HØJESTE PRIORITET: Allerede parsede dato-objekter
    if (inherits(col_data, c("Date", "POSIXct", "POSIXt"))) {
      log_debug("⭐", col_name, "- Already parsed date object (score: 100)", .context = "AUTO_DETECT_FUNC")
      candidate$score <- 100
      candidate$type <- "parsed_date"
      candidate$success_rate <- 1.0
      candidate$reason <- paste("Allerede", class(col_data)[1], "format")
      date_candidates[[col_name]] <- candidate
      next
    }

    # MELLEMPRIORITY: Navn-baseret detektion
    if (grepl("dato|date|tid|time|år|year|måned|month|uge|week|dag|day", col_name, ignore.case = TRUE)) {
      candidate$score <- candidate$score + 50
      candidate$reason <- paste(candidate$reason, "Navn-match")
    }

    # TEST: Parsing kvalitet for character/factor data
    if (is.character(col_data) || is.factor(col_data)) {
      char_data <- as.character(col_data)[!is.na(col_data)]

      if (length(char_data) > 0) {
        test_sample <- char_data[1:min(5, length(char_data))]

        # Test danske formater først med ny safe parsing
        danish_result <- safe_date_parse(test_sample,
          locale = "da_DK.UTF-8",
          operation_name = paste("dansk parsing for", col_name)
        )

        if (danish_result$success) {
          candidate$score <- candidate$score + (danish_result$success_rate * 40)
          candidate$success_rate <- danish_result$success_rate
          candidate$type <- "danish_date"
          candidate$reason <- paste(candidate$reason, "dansk dmy() format")
          date_candidates[[col_name]] <- candidate
          next
        }

        # Fallback til guess_formats med standardiseret fejlhåndtering
        guess_result <- safe_operation(
          operation_name = paste("guess_formats for", col_name),
          code = {
            guessed_formats <- suppressWarnings(
              lubridate::guess_formats(test_sample, c("ymd", "dmy", "mdy", "dby", "dmY", "Ymd", "mdY"))
            )

            if (!is.null(guessed_formats) && length(guessed_formats) > 0) {
              # Filtrer ugyldige formater
              valid_formats <- guessed_formats[!grepl("^n$|Unknown", guessed_formats)]

              if (length(valid_formats) > 0) {
                date_test <- suppressWarnings(
                  lubridate::parse_date_time(test_sample, orders = valid_formats, quiet = TRUE)
                )

                if (!is.null(date_test)) {
                  success_rate <- sum(!is.na(date_test)) / length(date_test)
                  if (success_rate >= 0.5) {
                    list(success_rate = success_rate, success = TRUE)
                  } else {
                    list(success_rate = success_rate, success = FALSE)
                  }
                } else {
                  list(success_rate = 0, success = FALSE)
                }
              } else {
                list(success_rate = 0, success = FALSE)
              }
            } else {
              list(success_rate = 0, success = FALSE)
            }
          },
          fallback = list(success_rate = 0, success = FALSE)
        )

        if (!is.null(guess_result) && guess_result$success) {
          candidate$score <- candidate$score + (guess_result$success_rate * 30)
          candidate$success_rate <- guess_result$success_rate
          candidate$type <- "guessed_date"
          candidate$reason <- paste(candidate$reason, "lubridate guess")
          date_candidates[[col_name]] <- candidate
        }
      }
    }

    # Gem kandidat hvis den har nogen score
    if (candidate$score > 0) {
      date_candidates[[col_name]] <- candidate
    }
  }

  # Vælg bedste kandidat baseret på score
  log_debug("📊 Date candidate selection results:", .context = "AUTO_DETECT_FUNC")
  x_col <- NULL
  best_score <- 0

  if (length(date_candidates) > 0) {
    log_debug("Found", length(date_candidates), "date candidates:", .context = "AUTO_DETECT_FUNC")
    # Log kandidater for debugging
    for (name in names(date_candidates)) {
      cand <- date_candidates[[name]]
      log_debug(sprintf(
        "- %s: score=%.1f (%s, success=%.2f) - %s",
        name, cand$score, cand$type, cand$success_rate, cand$reason
      ), .context = "AUTO_DETECT_FUNC")
    }
    # Find bedste kandidat
    for (name in names(date_candidates)) {
      cand <- date_candidates[[name]]
      if (cand$score > best_score) {
        best_score <- cand$score
        x_col <- name
      }
    }
    log_debug("🎯 Best date candidate:", x_col, "(score:", best_score, ")", .context = "AUTO_DETECT_FUNC")
  } else {
    log_debug("⚠️ No date candidates found", .context = "AUTO_DETECT_FUNC")
  }

  # Fallback til første kolonne hvis ingen dato-kandidater
  if (is.null(x_col) && length(col_names) > 0) {
    x_col <- col_names[1]
    log_debug("📝 Fallback X column:", x_col, .context = "AUTO_DETECT_FUNC")
  }

  # Tjek om vi har en gyldig X kolonne
  if (is.null(x_col)) {
    log_debug("⚠️ Ingen gyldig X kolonne fundet - afbryder auto-detection", .context = "AUTO_DETECT_FUNC")
    return(NULL)
  }

  # POST-PROCESSING: Konverter detekterede datokolonner til Date objekter (med reaktiv loop beskyttelse)
  # NOTE: app_state$data$original_data bevares uændret, kun current_data modificeres

  # Sikker dato-konvertering uden at trigger reaktive loops
  safe_operation(
    operation_name = "POST-PROCESSING: Dato-konvertering",
    code = {
      # Temporarily disable reactive observers during data modification
      isolate({
      for (candidate_name in names(date_candidates)) {
        candidate <- date_candidates[[candidate_name]]

        # Konverter character kolonner der blev detekteret som datoer
        if (candidate$type %in% c("danish_date", "guessed_date") && candidate$success_rate >= 0.7) {
          col_data <- current_data_check[[candidate_name]]

          if (is.character(col_data) || is.factor(col_data)) {

            # Perform safe date conversion
            converted_dates <- safe_operation(
              operation_name = paste("konvertering af", candidate_name, "til", candidate$type),
              code = {
                if (candidate$type == "danish_date") {
                  # Use safe_date_parse for Danish formats
                  result <- safe_date_parse(col_data,
                    locale = "da_DK.UTF-8",
                    operation_name = paste("dansk konvertering for", candidate_name)
                  )
                  as.POSIXct(result$data)
                } else if (candidate$type == "guessed_date") {
                  # Use parse_date_time for other formats
                  char_data <- as.character(col_data)
                  test_sample <- char_data[!is.na(char_data)][1:min(3, length(char_data[!is.na(char_data)]))]

                  guessed_formats <- suppressWarnings(
                    lubridate::guess_formats(test_sample, c("ymd", "dmy", "mdy", "dby", "dmY", "Ymd", "mdY"))
                  )

                  if (!is.null(guessed_formats) && length(guessed_formats) > 0) {
                    valid_formats <- guessed_formats[!grepl("^n$|Unknown", guessed_formats)]
                    if (length(valid_formats) > 0) {
                      result <- suppressWarnings(
                        lubridate::parse_date_time(col_data, orders = valid_formats, quiet = TRUE)
                      )
                      result
                    } else {
                      NULL
                    }
                  } else {
                    NULL
                  }
                } else {
                  NULL
                }
              },
              fallback = NULL
            )

            # Apply conversion if successful (inside isolate to prevent reactive triggers)
            if (!is.null(converted_dates) && sum(!is.na(converted_dates)) / length(converted_dates) >= 0.7) {
              # PHASE 4: Dimension safety check before assignment - unified state only
              current_rows <- nrow(app_state$data$current_data)
              converted_rows <- length(converted_dates)

              if (current_rows == converted_rows) {
                # Safe to assign - dimensions match - PHASE 4: Unified state only
                # Using unified state management
                app_state_rows <- nrow(app_state$data$current_data)
                if (app_state_rows == converted_rows) {
                  app_state$data$current_data[[candidate_name]] <- converted_dates
                } else {
                  log_warn(paste("Skipping app_state assignment - dimension mismatch:", app_state_rows, "vs", converted_rows), "POST_PROCESSING")
                }
              } else {
                log_warn(paste("Skipping date conversion assignment - dimension mismatch:", current_rows, "vs", converted_rows), "POST_PROCESSING")
              }
            }
          }
        }
      }
      })
    },
    error_type = "processing",
    emit = emit,
    app_state = app_state
  )

  # Detekter numeriske kolonner
  log_debug("🔢 Detecting numeric columns...", .context = "AUTO_DETECT_FUNC")
  numeric_cols <- character(0)
  for (col_name in col_names) {
    if (col_name != x_col) {
      col_data <- data[[col_name]]
      is_numeric <- is.numeric(col_data)
      danish_parse_success <- sum(!is.na(parse_danish_number(col_data))) > length(col_data) * 0.8

      if (is_numeric || danish_parse_success) {
        numeric_cols <- c(numeric_cols, col_name)
        log_debug("✅", col_name, "- numeric (native:", is_numeric, ", danish:", danish_parse_success, ")", .context = "AUTO_DETECT_FUNC")
      } else {
        log_debug("❌", col_name, "- not numeric", .context = "AUTO_DETECT_FUNC")
      }
    } else {
      log_debug("⏭️", col_name, "- skipped (X column)", .context = "AUTO_DETECT_FUNC")
    }
  }
  log_debug("Found", length(numeric_cols), "numeric columns:", paste(numeric_cols, collapse = ", "), .context = "AUTO_DETECT_FUNC")

  # Smart detektion af tæller/nævner
  log_debug("🎯 Smart detection for Y/N columns...", .context = "AUTO_DETECT_FUNC")
  col_names_lower <- tolower(col_names)
  taeller_col <- NULL
  naevner_col <- NULL

  taeller_idx <- which(grepl("t.ller|tael|num|count", col_names_lower, ignore.case = TRUE))
  naevner_idx <- which(grepl("n.vner|naev|denom|total", col_names_lower, ignore.case = TRUE))

  log_debug_kv(pattern_matches_taeller = length(taeller_idx), pattern_matches_naevner = length(naevner_idx), .context = "AUTO_DETECT_FUNC")

  if (length(taeller_idx) > 0 && length(naevner_idx) > 0) {
    taeller_col <- col_names[taeller_idx[1]]
    naevner_col <- col_names[naevner_idx[1]]
    log_debug("📝 Name-based assignment - Y:", taeller_col, ", N:", naevner_col, .context = "AUTO_DETECT_FUNC")
  } else if (length(numeric_cols) >= 2) {
    taeller_col <- numeric_cols[1]
    naevner_col <- numeric_cols[2]
    log_debug("📊 Fallback assignment (first two numerics) - Y:", taeller_col, ", N:", naevner_col, .context = "AUTO_DETECT_FUNC")
  } else if (length(numeric_cols) >= 1) {
    taeller_col <- numeric_cols[1]
    log_debug("📊 Single numeric assignment - Y:", taeller_col, ", N: NULL", .context = "AUTO_DETECT_FUNC")
  } else {
    log_debug("⚠️ No numeric columns found for Y/N assignment", .context = "AUTO_DETECT_FUNC")
  }

  # Detekter skift/fase kolonne (boolean eller tekst med skift-relaterede termer)
  skift_col <- NULL
  skift_idx <- which(grepl("skift|shift|fase|phase|change|periode", col_names_lower, ignore.case = TRUE))

  if (length(skift_idx) > 0) {
    skift_col <- col_names[skift_idx[1]]
  } else {
    # Søg efter boolean kolonner som kan repræsentere skift
    for (col_name in col_names) {
      col_data <- data[[col_name]]
      if (is.logical(col_data)) {
        skift_col <- col_name
        break
      }
    }
  }

  # Detekter frys/kontrol-frysning kolonne (boolean eller tekst med frys-relaterede termer)
  frys_col <- NULL
  frys_idx <- which(grepl("frys|freeze|kontrol|control|stop|pause", col_names_lower, ignore.case = TRUE))

  if (length(frys_idx) > 0) {
    frys_col <- col_names[frys_idx[1]]
  } else {
    # Søg efter boolean kolonner som kan repræsentere frysning (efter skift kolonne)
    for (col_name in col_names) {
      if (col_name != skift_col) { # Skip skift kolonne
        col_data <- data[[col_name]]
        if (is.logical(col_data)) {
          frys_col <- col_name
          break
        }
      }
    }
  }

  # Detekter kommentar kolonne (tekst-baserede kolonner)
  kommentar_col <- NULL
  kommentar_idx <- which(grepl("kommentar|comment|note|noter|bemærk|remark", col_names_lower, ignore.case = TRUE))

  if (length(kommentar_idx) > 0) {
    kommentar_col <- col_names[kommentar_idx[1]]
  } else {
    # Søg efter karakter kolonner som ikke allerede er tildelt og indeholder tekst
    for (col_name in col_names) {
      if (col_name != x_col && col_name != taeller_col && col_name != naevner_col && col_name != skift_col && col_name != frys_col) {
        col_data <- data[[col_name]]
        if (is.character(col_data) && any(nzchar(col_data, keepNA = FALSE), na.rm = TRUE)) {
          kommentar_col <- col_name
          break
        }
      }
    }
  }

  # Opdater dropdowns til at VISE de detekterede værdier
  log_debug("📋 Final column assignments:", .context = "AUTO_DETECT_FUNC")
  log_debug_kv(
    X_date_time = x_col %||% "NULL",
    Y_taeller = taeller_col %||% "NULL",
    N_naevner = naevner_col %||% "NULL",
    Skift = skift_col %||% "NULL",
    Frys = frys_col %||% "NULL",
    Kommentar = kommentar_col %||% "NULL",
    .context = "AUTO_DETECT_FUNC"
  )

  # Først sikr at choices er opdateret med nuværende kolonnenavne
  all_cols <- col_names
  col_choices <- setNames(
    c("", all_cols),
    c("Vælg kolonne...", all_cols)
  )

  # UI opdateringer håndteres nu af reaktiv observer - fjern duplikeret logik
  isolate({
    detected_msg <- paste0(
      "Auto-detekteret og opdateret dropdowns: ",
      "X=", if (is.null(x_col)) "ingen" else x_col, ", ",
      "Y=", if (is.null(taeller_col)) "ingen" else taeller_col,
      if (!is.null(naevner_col)) paste0(", N=", naevner_col) else ", N=ingen",
      if (!is.null(skift_col)) paste0(", Skift=", skift_col) else ", Skift=ingen",
      if (!is.null(frys_col)) paste0(", Frys=", frys_col) else ", Frys=ingen",
      if (!is.null(kommentar_col)) paste0(", Kommentar=", kommentar_col) else ", Kommentar=ingen"
    )

    showNotification(
      detected_msg,
      type = "message",
      duration = 4
    )

    # Using unified state management for auto-detected columns storage
    log_debug("Auto-detected columns handled by unified state management", .context = "PHASE4")
    # PHASE 4: Sync auto_detected_columns to centralized state - FIX: Use correct path that visualization expects
    app_state$columns$auto_detect$results <- list(
      x_col = x_col,
      y_col = taeller_col,
      n_col = naevner_col,
      skift_col = skift_col,
      frys_col = frys_col,
      kommentar_col = kommentar_col,
      timestamp = Sys.time()
    )
    log_debug("Synced auto_detected_columns to centralized state (FIXED PATH)", .context = "PHASE4")

    # Trigger UI sync for visual feedback on Kolonnematch tab using reactive trigger
    log_debug("🎯 CRITICAL: Setting ui_sync_needed with:", .context = "AUTO_DETECT_FUNC")
    log_debug_kv(
      x_col = x_col %||% "NULL",
      taeller_col = taeller_col %||% "NULL",
      naevner_col = naevner_col %||% "NULL",
      col_choices_length = length(col_choices),
      .context = "AUTO_DETECT_FUNC"
    )

    # Use reactive trigger with timestamp to force reactivity
    log_debug("🔄 Triggering UI sync - should update input fields!", .context = "AUTO_DETECT_FUNC")
    sync_data <- list(
      x_col = x_col,
      taeller_col = taeller_col,
      naevner_col = naevner_col,
      skift_col = skift_col,
      frys_col = frys_col,
      kommentar_col = kommentar_col,
      col_choices = col_choices,
      timestamp = as.numeric(Sys.time())  # Force reactivity trigger
    )

    # PHASE 4: Use unified state management
    app_state$columns$ui_sync$needed <- sync_data

    # CRITICAL: Set debounce timestamp BEFORE triggering UI sync to prevent race condition
    app_state$columns$ui_sync$last_sync_time <- Sys.time()
    log_debug("⏰ Debounce timestamp set BEFORE UI sync to prevent column mgmt override", .context = "UI_SYNC_TRIGGER")

    # UNIFIED EVENT SYSTEM: UI sync is now handled by events (CRITICAL for UI sync)
    # Store results in app_state for unified event system access
    app_state$columns$auto_detect$results <- sync_data
    log_debug("FULL MODE: Results stored in app_state for unified event system", .context = "UI_SYNC_UNIFIED")

    log_debug_block("AUTO_DETECT_FUNC", "✅ Auto-detect completed successfully", type = "stop")

    # Complete auto-detect workflow tracing
    autodetect_tracer$step("ui_sync_data_created")
    autodetect_tracer$complete("auto_detect_full_workflow_complete")

    # CRITICAL: Trigger navigation update efter autodetect completion
    # UNIFIED EVENTS: Trigger navigation change through event system
    log_debug("Emitting navigation_changed event", .context = "AUTO_DETECT")
    emit$navigation_changed()
    log_debug("AUTO_DETECT: navigation_changed event emitted", "AUTO_DETECT")

    debug_log("Auto-detect process completed successfully", "AUTO_DETECT_FLOW", level = "INFO",
              context = list(
                x_col = if(exists("x_col")) x_col else "unknown",
                y_col = if(exists("y_col")) y_col else "unknown",
                n_col = if(exists("n_col")) n_col else "unknown",
                ui_sync_triggered = TRUE,
                navigation_events_unified = TRUE
              ),
              session_id = session_id)

    # Take final state snapshot
    if (!is.null(app_state)) {
      debug_state_snapshot("after_auto_detect_complete", app_state, session_id = session_id)
    }
  })

}

# MODAL FUNKTIONER ============================================================

## Vis kolonne-redigeré modal
# Viser modal dialog for redigering af kolonnenavne
show_column_edit_modal <- function(session, app_state = NULL) {
  # PHASE 4: Use unified state management
  current_data_check <- app_state$data$current_data
  req(current_data_check)

  current_names <- names(current_data_check)

  name_inputs <- lapply(1:length(current_names), function(i) {
    textInput(
      paste0("col_name_", i),
      paste("Kolonne", i, ":"),
      value = current_names[i],
      placeholder = paste("Navn for kolonne", i)
    )
  })

  showModal(modalDialog(
    title = "Redigér kolonnenavne",
    size = "m",
    div(
      style = "margin-bottom: 15px;",
      h6("Nuværende kolonnenavne:", style = "font-weight: 500;"),
      p(paste(current_names, collapse = ", "), style = "color: #666; font-style: italic;")
    ),
    div(
      style = "max-height: 300px; overflow-y: auto;",
      name_inputs
    ),
    footer = tagList(
      modalButton("Annuller"),
      actionButton("confirm_column_names", "Gem ændringer", class = "btn-primary")
    )
  ))
}

## Håndtér kolonnenavn ændringer
# Behandler ændringer af kolonnenavne fra modal dialog
handle_column_name_changes <- function(input, session, app_state = NULL, emit = NULL) {
  # PHASE 4: Use unified state management
  current_data_check <- app_state$data$current_data
  req(current_data_check)

  current_names <- names(current_data_check)
  new_names <- character(length(current_names))

  for (i in 1:length(current_names)) {
    input_value <- input[[paste0("col_name_", i)]]
    if (!is.null(input_value) && input_value != "") {
      new_names[i] <- trimws(input_value)
    } else {
      new_names[i] <- current_names[i]
    }
  }

  if (any(duplicated(new_names))) {
    showNotification(
      "Kolonnenavne skal være unikke. Ret duplikater og prøv igen.",
      type = "error",
      duration = 5
    )
    return()
  }

  # PHASE 4: Unified state assignment only
  names(app_state$data$current_data) <- new_names

  # Emit event to trigger downstream effects
  if (!is.null(emit)) {
    emit$data_changed()
  }

  removeModal()

  if (!identical(current_names, new_names)) {
    changed_cols <- which(current_names != new_names)
    change_summary <- paste(
      paste0("'", current_names[changed_cols], "' -> '", new_names[changed_cols], "'"),
      collapse = ", "
    )

    showNotification(
      paste("Kolonnenavne opdateret:", change_summary),
      type = "message",
      duration = 4
    )
  } else {
    showNotification("Ingen ændringer i kolonnenavne", type = "message", duration = 2)
  }
}

## Vis tilføj kolonne modal
# Viser modal dialog for tilføjelse af nye kolonner
show_add_column_modal <- function() {
  showModal(modalDialog(
    title = "Tilføj ny kolonne",
    textInput("new_col_name", "Kolonnenavn:", value = "Ny_kolonne"),
    selectInput("new_col_type", "Type:",
      choices = list("Numerisk" = "numeric", "Tekst" = "text", "Dato" = "date")
    ),
    footer = tagList(
      modalButton("Annuller"),
      actionButton("confirm_add_col", "Tilføj", class = "btn-primary")
    )
  ))
}

## Håndtér tilføjelse af kolonne
# Behandler tilføjelse af nye kolonner til data
handle_add_column <- function(input, session, app_state = NULL, emit = NULL) {
  # PHASE 4: Use unified state management
  current_data_check <- app_state$data$current_data
  req(input$new_col_name, current_data_check)

  new_col_name <- input$new_col_name
  new_col_type <- input$new_col_type

  if (new_col_type == "numeric") {
    # PHASE 4: Unified state assignment only
    app_state$data$current_data[[new_col_name]] <- rep(NA_real_, nrow(current_data_check))
  } else if (new_col_type == "date") {
    # PHASE 4: Unified state assignment only
    app_state$data$current_data[[new_col_name]] <- rep(NA_character_, nrow(current_data_check))
  } else {
    # PHASE 4: Unified state assignment only
    app_state$data$current_data[[new_col_name]] <- rep(NA_character_, nrow(current_data_check))
  }

  # Emit event to trigger downstream effects
  if (!is.null(emit)) {
    emit$data_changed()
  }

  removeModal()
  showNotification(paste("Kolonne", new_col_name, "tilføjet"), type = "message")
}
# server_data_table.R
# Server logik for data tabel rendering og interaktion

# Dependencies ----------------------------------------------------------------

# DATATABEL SETUP =============================================================

## Hovedfunktion for datatabel
# Opsætter al server logik relateret til data-tabel håndtering
setup_data_table <- function(input, output, session, app_state, emit) {
  log_debug_block("DATA_TABLE", "Setting up data table with unified state")

  # UNIFIED EVENT SYSTEM: Direct access to app_state data instead of reactive pattern
  log_debug("Using unified event system for data table setup", .context = "DATA_TABLE")

  # Hovedtabel rendering med excelR
  output$main_data_table <- excelR::renderExcel({
    # UNIFIED EVENT SYSTEM: Direct access to current data
    current_data_check <- app_state$data$current_data
    req(current_data_check)

    log_debug("Rendering table with data dimensions:", paste(dim(current_data_check), collapse = "x"), .context = "DATA_TABLE")

    # Inkluder table_version for at tvinge re-render efter gendannelse
    # PHASE 4: Use unified state management
    version_trigger <- app_state$session$table_version

    data <- current_data_check

    # Behold logiske kolonner som logiske for excelR checkbox
    # excelR håndterer logiske værdier direkte for checkbox type

    excelR::excelTable(
      data = data,
      columns = data.frame(
        title = names(data),
        type = case_when(
          names(data) == "Skift" ~ "checkbox",
          names(data) == "Frys" ~ "radio",
          TRUE ~ "text"
        ),
        width = case_when(
          names(data) == "Skift" ~ 60,
          names(data) == "Frys" ~ 60,
          names(data) == "Dato" ~ 100,
          names(data) %in% c("Tæller", "Nævner") ~ 80,
          names(data) == "Kommentar" ~ 300,
          TRUE ~ 120
        ),
        stringsAsFactors = FALSE
      ),
      allowInsertRow = FALSE,
      allowInsertColumn = FALSE,
      allowDeleteRow = FALSE,
      allowDeleteColumn = FALSE,
      allowRenameColumn = FALSE,
      columnSorting = FALSE,
      rowDrag = FALSE,
      columnDrag = FALSE,
      autoFill = TRUE
    )
  })

  # Håndtér excelR tabel ændringer
  observeEvent(input$main_data_table,
    {
      # PHASE 4: Use unified state management
      updating_table_check <- app_state$data$updating_table

      # PHASE 4: Use unified state management
      restoring_session_check <- app_state$session$restoring_session

      if (updating_table_check || restoring_session_check) {
        return()
      }

      # PHASE 4: Use unified state management
      app_state$data$updating_table <- TRUE
      # PHASE 4: Use unified state management
      app_state$data$table_operation_in_progress <- TRUE

      on.exit(
        {
          # PHASE 4: Use unified state management
          app_state$data$updating_table <- FALSE
        },
        add = TRUE
      )

      # Trigger event-driven cleanup instead of timing-based
      # PHASE 4: Use unified state management
      app_state$data$table_operation_cleanup_needed <- TRUE

      safe_operation(
        operation_name = "ExcelR tabel data opdatering",
        code = {
          new_data <- input$main_data_table

          if (is.null(new_data) || length(new_data) == 0) {
            return()
          }


          # excelR sender data i new_data$data som liste af rækker
          if (!is.null(new_data$data) && length(new_data$data) > 0) {
            # Hent kolonnenavne fra colHeaders
            col_names <- unlist(new_data$colHeaders)

            # Konvertér liste af rækker til data frame
            row_list <- new_data$data

            # Opret tom data frame med korrekt struktur
            new_df <- data.frame(matrix(NA, nrow = length(row_list), ncol = length(col_names)))
            names(new_df) <- col_names

            # Fyld data frame række for række
            for (i in seq_along(row_list)) {
              row_data <- row_list[[i]]
              for (j in seq_along(row_data)) {
                if (j <= length(col_names)) {
                  new_df[i, j] <- row_data[[j]]
                }
              }
            }

            # Konvertér datatyper korrekt
            # Skift kolonne (logisk) - excelR sender checkbox som logisk allerede
            if ("Skift" %in% names(new_df)) {
              # Håndtér både logiske og streng repræsentationer
              skift_values <- new_df$Skift
              if (is.character(skift_values)) {
                new_df$Skift <- skift_values == "TRUE" | skift_values == "true" | skift_values == TRUE
              } else {
                new_df$Skift <- as.logical(skift_values)
              }
            }

            # Frys kolonne (logisk) - excelR sender radio som logisk
            if ("Frys" %in% names(new_df)) {
              # Håndtér både logiske og streng repræsentationer
              frys_values <- new_df$Frys
              if (is.character(frys_values)) {
                new_df$Frys <- frys_values == "TRUE" | frys_values == "true" | frys_values == TRUE
              } else {
                new_df$Frys <- as.logical(frys_values)
              }
            }

            # Numeriske kolonner
            numeric_cols <- c("Tæller", "Nævner")
            for (col in numeric_cols) {
              if (col %in% names(new_df)) {
                new_df[[col]] <- as.numeric(new_df[[col]])
              }
            }

            # Dato kolonne
            if ("Dato" %in% names(new_df)) {
              new_df$Dato <- as.character(new_df$Dato)
            }

            # Karakter kolonner
            if ("Kommentar" %in% names(new_df)) {
              new_df$Kommentar <- as.character(new_df$Kommentar)
            }
          } else {
            return()
          }

          # PHASE 4: Dual-state sync for compatibility during migration
          set_current_data(app_state, new_df)

          # Emit event to trigger downstream effects
          emit$data_changed()

          showNotification("Tabel opdateret", type = "message", duration = 2)
        },
        error_type = "processing",
        emit = emit,
        app_state = app_state,
        show_user = TRUE,
        session = session
      )
    },
    ignoreInit = TRUE
  )

  # Tilføj række
  observeEvent(input$add_row, {
    # UNIFIED EVENT SYSTEM: Direct access to current data
    current_data_check <- app_state$data$current_data
    req(current_data_check)

    # Sæt vedvarende flag for at forhindre auto-save interferens
    # PHASE 4: Use unified state management
    app_state$data$table_operation_in_progress <- TRUE

    new_row <- current_data_check[1, ]
    new_row[1, ] <- NA

    # PHASE 4: Dual-state sync for compatibility during migration
    current_data <- get_current_data(app_state)
    set_current_data(app_state, rbind(current_data, new_row))

    # Emit event to trigger downstream effects
    emit$data_changed()

    showNotification("Ny række tilføjet", type = "message")

    # Trigger event-driven cleanup instead of timing-based
    # PHASE 4: Use unified state management
    app_state$data$table_operation_cleanup_needed <- TRUE
  })

  # UNIFIED STATE: Table reset functionality moved to utils_server_management.R
  # Uses emit$session_reset() events and unified app_state management
}
