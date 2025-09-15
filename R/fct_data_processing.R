# server_column_management.R
# Server logik for kolonnehåndtering inklusive auto-detektion og validering

# Dependencies ----------------------------------------------------------------

# KOLONNEHÅNDTERING SETUP ====================================================

## Hovedfunktion for kolonnehåndtering
# Opsætter al server logik relateret til kolonne-management
setup_column_management <- function(input, output, session, values) {
  # Opdater kolonnevalg når data ændres
  observe({
    if (values$updating_table) {
      return()
    }

    # Skip hvis auto-detect er i gang for at undgå at overskrive auto-detect resultater
    if (values$auto_detect_in_progress) {
      return()
    }

    req(values$current_data)

    data <- values$current_data
    all_cols <- names(data)

    if (length(all_cols) > 0) {
      # Kun "Vælg kolonne..." som første option - selectizeInput kan rydde selv
      col_choices <- setNames(
        c("", all_cols),
        c("Vælg kolonne...", all_cols)
      )

      isolate({
        # Bevar nuværende valgte værdier når choices opdateres
        current_x <- input$x_column
        current_y <- input$y_column
        current_n <- input$n_column
        current_skift <- input$skift_column
        current_frys <- input$frys_column
        current_kommentar <- input$kommentar_column

        updateSelectizeInput(session, "x_column", choices = col_choices, selected = current_x)
        updateSelectizeInput(session, "y_column", choices = col_choices, selected = current_y)
        updateSelectizeInput(session, "n_column", choices = col_choices, selected = current_n)
        updateSelectizeInput(session, "skift_column", choices = col_choices, selected = current_skift)
        updateSelectizeInput(session, "frys_column", choices = col_choices, selected = current_frys)
        updateSelectizeInput(session, "kommentar_column", choices = col_choices, selected = current_kommentar)
      })
    }
  })

  # Auto-detekterings trigger flag - bruges kun til manuel triggering (ikke test mode)
  observeEvent(values$current_data,
    {
      # Skip automatisk auto-detect hvis vi allerede har været igennem det i test mode
      if (values$initial_auto_detect_completed %||% FALSE) {
        return()
      }

      if (!is.null(values$current_data) &&
        (is.null(input$x_column) || input$x_column == "") &&
        (is.null(input$y_column) || input$y_column == "")) {
        values$auto_detect_trigger <- Sys.time() # Brug timestamp for at sikre reaktivitet
      }
    },
    ignoreInit = TRUE
  )

  # Test mode auto-detect trigger (event-driven instead of later::later)
  observeEvent(values$test_mode_auto_detect_ready,
    {
      req(values$test_mode_auto_detect_ready)
      cat("TEST MODE: Event-driven auto-detect trigger fired!\n")
      values$auto_detect_trigger <- Sys.time()
    },
    ignoreInit = TRUE, ignoreNULL = TRUE
  )

  # Forsinket auto-detekterings udførelse
  observeEvent(values$auto_detect_trigger,
    {
      values$auto_detect_in_progress <- TRUE # Set flag før auto-detect starter
      auto_detect_and_update_columns(input, session, values)
      values$initial_auto_detect_completed <- TRUE # Marker som færdig efter første kørsel

      # Clear flag after auto-detect completion (event-driven instead of timing)
      values$auto_detect_in_progress <- FALSE
    },
    ignoreInit = TRUE
  )

  # Event-driven UI sync observer (replaces later::later timing)
  observeEvent(values$ui_sync_needed,
    {
      req(values$ui_sync_needed)

      sync_data <- values$ui_sync_needed

      # Re-trigger UI updates with choices parameter for visual sync
      if (!is.null(sync_data$x_col)) {
        updateSelectizeInput(session, "x_column", choices = sync_data$col_choices, selected = sync_data$x_col)
      }
      if (!is.null(sync_data$taeller_col)) {
        updateSelectizeInput(session, "y_column", choices = sync_data$col_choices, selected = sync_data$taeller_col)
      }
      if (!is.null(sync_data$naevner_col)) {
        updateSelectizeInput(session, "n_column", choices = sync_data$col_choices, selected = sync_data$naevner_col)
      }
      if (!is.null(sync_data$skift_col)) {
        updateSelectizeInput(session, "skift_column", choices = sync_data$col_choices, selected = sync_data$skift_col)
      }
      if (!is.null(sync_data$frys_col)) {
        updateSelectizeInput(session, "frys_column", choices = sync_data$col_choices, selected = sync_data$frys_col)
      }
      if (!is.null(sync_data$kommentar_col)) {
        updateSelectizeInput(session, "kommentar_column", choices = sync_data$col_choices, selected = sync_data$kommentar_col)
      }


      # Clear the sync request
      values$ui_sync_needed <- NULL
    },
    ignoreInit = TRUE, ignoreNULL = TRUE, priority = -10  # Lower priority = runs after other observers
  )

  # Auto-detekterings knap handler - kører altid når bruger trykker
  observeEvent(input$auto_detect_columns, {
    values$auto_detect_in_progress <- TRUE # Set flag før auto-detect starter
    auto_detect_and_update_columns(input, session, values)
    values$initial_auto_detect_completed <- TRUE # Marker som færdig
    values$auto_detect_in_progress <- FALSE # Clear flag efter auto-detect er færdig
  })

  # Kolonnevaliderings output
  output$column_validation_messages <- renderUI({
    req(values$current_data)

    if ((is.null(input$x_column) || input$x_column == "") ||
      (is.null(input$y_column) || input$y_column == "")) {
      return(NULL)
    }

    chart_type <- get_qic_chart_type(if (is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type)
    warnings <- character(0)

    # Tjek om Y-kolonne er numerisk
    if (!is.null(input$y_column) && input$y_column != "" && input$y_column %in% names(values$current_data)) {
      y_data <- values$current_data[[input$y_column]]
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
      } else if (input$n_column %in% names(values$current_data)) {
        n_data <- values$current_data[[input$n_column]]
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
    show_column_edit_modal(session, values)
  })

  # Bekræft kolonnenavn ændringer
  observeEvent(input$confirm_column_names, {
    handle_column_name_changes(input, session, values)
  })

  # Tilføj kolonne
  observeEvent(input$add_column, {
    show_add_column_modal()
  })

  observeEvent(input$confirm_add_col, {
    handle_add_column(input, session, values)
  })
}

# AUTO-DETEKTION FUNKTIONER ==================================================

## Auto-detekter og opdater kolonner
# Automatisk detektion af kolonnetyper baseret på data indhold
auto_detect_and_update_columns <- function(input, session, values) {
  req(values$current_data)

  data <- values$current_data
  col_names <- names(data)

  # Forbedret detektering af potentielle dato-kolonner
  # Evaluer ALLE kolonner og find den bedste dato-kandidat

  date_candidates <- list()

  for (col_name in col_names) {
    col_data <- data[[col_name]]

    # Skip ikke-dato kolonner baseret på navn
    if (grepl("^(nr|id|count|antal|total|sum)$", col_name, ignore.case = TRUE)) {
      next
    }

    # Skip kolonner der kun indeholder NA værdier
    if (all(is.na(col_data)) || length(col_data) == 0) {
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
  x_col <- NULL
  best_score <- 0

  if (length(date_candidates) > 0) {
    # Log kandidater for debugging
    for (name in names(date_candidates)) {
      cand <- date_candidates[[name]]
      cat(sprintf(
        "- %s: score=%.1f (%s, success=%.2f) - %s\n",
        name, cand$score, cand$type, cand$success_rate, cand$reason
      ))
    }

    # Find bedste kandidat
    for (name in names(date_candidates)) {
      cand <- date_candidates[[name]]
      if (cand$score > best_score) {
        best_score <- cand$score
        x_col <- name
      }
    }
  }

  # Fallback til første kolonne hvis ingen dato-kandidater
  if (is.null(x_col) && length(col_names) > 0) {
    x_col <- col_names[1]
  }

  # POST-PROCESSING: Konverter detekterede datokolonner til Date objekter
  # NOTE: values$original_data bevares uændret, kun values$current_data modificeres

  for (candidate_name in names(date_candidates)) {
    candidate <- date_candidates[[candidate_name]]

    # Konverter character kolonner der blev detekteret som datoer
    if (candidate$type %in% c("danish_date", "guessed_date") && candidate$success_rate >= 0.7) {
      col_data <- values$current_data[[candidate_name]] # FIX: Brug values$current_data

      if (is.character(col_data) || is.factor(col_data)) {
        # Brug safe date parsing til konvertering
        converted_dates <- safe_operation(
          operation_name = paste("konvertering af", candidate_name, "til", candidate$type),
          code = {
            if (candidate$type == "danish_date") {
              # Brug safe_date_parse for danske formater
              result <- safe_date_parse(col_data,
                locale = "da_DK.UTF-8",
                operation_name = paste("dansk konvertering for", candidate_name)
              )
              as.POSIXct(result$data)
            } else if (candidate$type == "guessed_date") {
              # Brug parse_date_time for andre formater
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
                  # Returner som POSIXct for konsistens med qicharts2
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

        # Opdater data hvis konvertering var succesrig
        if (!is.null(converted_dates) && sum(!is.na(converted_dates)) / length(converted_dates) >= 0.7) {
          values$current_data[[candidate_name]] <- converted_dates
        }
      }
    }
  }

  # Detekter numeriske kolonner
  numeric_cols <- character(0)
  for (col_name in col_names) {
    if (col_name != x_col) {
      col_data <- data[[col_name]]
      if (is.numeric(col_data) ||
        sum(!is.na(parse_danish_number(col_data))) > length(col_data) * 0.8) {
        numeric_cols <- c(numeric_cols, col_name)
      }
    }
  }

  # Smart detektion af tæller/nævner
  col_names_lower <- tolower(col_names)
  taeller_col <- NULL
  naevner_col <- NULL

  taeller_idx <- which(grepl("t.ller|tael|num|count", col_names_lower, ignore.case = TRUE))
  naevner_idx <- which(grepl("n.vner|naev|denom|total", col_names_lower, ignore.case = TRUE))

  if (length(taeller_idx) > 0 && length(naevner_idx) > 0) {
    taeller_col <- col_names[taeller_idx[1]]
    naevner_col <- col_names[naevner_idx[1]]
  } else if (length(numeric_cols) >= 2) {
    taeller_col <- numeric_cols[1]
    naevner_col <- numeric_cols[2]
  } else if (length(numeric_cols) >= 1) {
    taeller_col <- numeric_cols[1]
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
  # Først sikr at choices er opdateret med nuværende kolonnenavne
  all_cols <- col_names
  col_choices <- setNames(
    c("", all_cols),
    c("Vælg kolonne...", all_cols)
  )

  isolate({
    if (!is.null(x_col) && x_col %in% col_names) {
      updateSelectizeInput(session, "x_column", choices = col_choices, selected = x_col)
    } else {
      updateSelectizeInput(session, "x_column", choices = col_choices, selected = "")
    }

    if (!is.null(taeller_col) && taeller_col %in% col_names) {
      updateSelectizeInput(session, "y_column", choices = col_choices, selected = taeller_col)
    } else {
      updateSelectizeInput(session, "y_column", choices = col_choices, selected = "")
    }

    if (!is.null(naevner_col) && naevner_col %in% col_names) {
      updateSelectizeInput(session, "n_column", choices = col_choices, selected = naevner_col)
    } else {
      updateSelectizeInput(session, "n_column", choices = col_choices, selected = "")
    }

    if (!is.null(skift_col) && skift_col %in% col_names) {
      updateSelectizeInput(session, "skift_column", choices = col_choices, selected = skift_col)
    } else {
      updateSelectizeInput(session, "skift_column", choices = col_choices, selected = "")
    }

    if (!is.null(frys_col) && frys_col %in% col_names) {
      updateSelectizeInput(session, "frys_column", choices = col_choices, selected = frys_col)
    } else {
      updateSelectizeInput(session, "frys_column", choices = col_choices, selected = "")
    }

    if (!is.null(kommentar_col) && kommentar_col %in% col_names) {
      updateSelectizeInput(session, "kommentar_column", choices = col_choices, selected = kommentar_col)
    } else {
      updateSelectizeInput(session, "kommentar_column", choices = col_choices, selected = "")
    }

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

    # Set flag for UI sync instead of using timing
    values$ui_sync_needed <- list(
      x_col = x_col,
      taeller_col = taeller_col,
      naevner_col = naevner_col,
      skift_col = skift_col,
      frys_col = frys_col,
      kommentar_col = kommentar_col,
      col_choices = col_choices,
      timestamp = Sys.time()
    )
  })
}

# MODAL FUNKTIONER ============================================================

## Vis kolonne-redigeré modal
# Viser modal dialog for redigering af kolonnenavne
show_column_edit_modal <- function(session, values) {
  req(values$current_data)

  current_names <- names(values$current_data)

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
handle_column_name_changes <- function(input, session, values) {
  req(values$current_data)

  current_names <- names(values$current_data)
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

  names(values$current_data) <- new_names

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
handle_add_column <- function(input, session, values) {
  req(input$new_col_name, values$current_data)

  new_col_name <- input$new_col_name
  new_col_type <- input$new_col_type

  if (new_col_type == "numeric") {
    values$current_data[[new_col_name]] <- rep(NA_real_, nrow(values$current_data))
  } else if (new_col_type == "date") {
    values$current_data[[new_col_name]] <- rep(NA_character_, nrow(values$current_data))
  } else {
    values$current_data[[new_col_name]] <- rep(NA_character_, nrow(values$current_data))
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
setup_data_table <- function(input, output, session, values) {
  # Hovedtabel rendering med excelR
  output$main_data_table <- excelR::renderExcel({
    req(values$current_data)

    # Inkluder table_version for at tvinge re-render efter gendannelse
    version_trigger <- values$table_version

    data <- values$current_data

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
      if (values$updating_table || values$restoring_session) {
        return()
      }

      values$updating_table <- TRUE
      values$table_operation_in_progress <- TRUE

      on.exit(
        {
          values$updating_table <- FALSE
        },
        add = TRUE
      )

      # Clear persistent flag after delay
      later::later(function() {
        values$table_operation_in_progress <- FALSE
      }, delay = 2)

      tryCatch(
        {
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

          values$current_data <- new_df

          showNotification("Tabel opdateret", type = "message", duration = 2)
        },
        error = function(e) {
          cat("ERROR in excelR table change:\n", e$message, "\n")
          showNotification(
            paste("Fejl ved tabel-opdatering:", e$message),
            type = "error",
            duration = 3
          )
        }
      )
    },
    ignoreInit = TRUE
  )

  # Tilføj række
  observeEvent(input$add_row, {
    req(values$current_data)

    # Sæt vedvarende flag for at forhindre auto-save interferens
    values$table_operation_in_progress <- TRUE

    new_row <- values$current_data[1, ]
    new_row[1, ] <- NA

    values$current_data <- rbind(values$current_data, new_row)

    showNotification("Ny række tilføjet", type = "message")

    # Ryd vedvarende flag efter forsinkelse
    later::later(function() {
      values$table_operation_in_progress <- FALSE
    }, delay = 1)
  })

  # Nulstil tabel
  # observeEvent(input$reset_table, {
  #   values$updating_table <- TRUE
  #   values$table_operation_in_progress <- TRUE
  #
  #   values$current_data <- data.frame(
  #     Dato = rep(NA_character_, 5),
  #     Taeller = rep(NA_real_, 5),
  #     Naevner = rep(NA_real_, 5),
  #     stringsAsFactors = FALSE
  #   )
  #
  #   values$file_uploaded <- FALSE
  #   values$original_data <- NULL
  #   values$auto_detect_done <- FALSE
  #
  #   isolate({
  #     shinyjs::reset("data_file")
  #   })
  #
  #   values$updating_table <- FALSE
  #
  #   # Ryd vedvarende flag efter forsinkelse
  #   later::later(function() {
  #     values$table_operation_in_progress <- FALSE
  #   }, delay = 1)
  #
  #   showNotification(
  #     "Tabel og fil-upload tømt - indtast nye data eller upload ny fil. Titel og beskrivelse bevaret.",
  #     type = "message",
  #     duration = 4
  #   )
  # })
}
