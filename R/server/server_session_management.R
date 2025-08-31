# R/server/server_session_management.R
# Session management logic including auto-restore and manual save/clear

setup_session_management <- function(input, output, session, values, waiter_file) {
  
  # Auto-restore session data - TEMPORARILY DISABLED FOR TESTING
  # observeEvent(input$auto_restore_data, {
  #   req(input$auto_restore_data)
  #   
  #   tryCatch({
  #     cat("DEBUG: Auto-restoring saved session data\n")
  #     saved_state <- input$auto_restore_data
  #     
  #     if (!is.null(saved_state$data)) {
  #       cat("DEBUG: Found saved data for auto-restore\n")
  #       
  #       # Disable auto-save and set restore guard
  #       values$restoring_session <- TRUE
  #       values$updating_table <- TRUE
  #       values$auto_save_enabled <- FALSE
  #       
  #       # Cleanup function
  #       on.exit({
  #         cat("DEBUG: Auto-restore cleanup - re-enabling auto-save\n")
  #         values$updating_table <- FALSE
  #         values$restoring_session <- FALSE
  #         values$auto_save_enabled <- TRUE
  #       }, add = TRUE)
  #       
  #       # Reconstruct data.frame from saved structure
  #       saved_data <- saved_state$data
  #       
  #       if (!is.null(saved_data$values) && !is.null(saved_data$nrows) && !is.null(saved_data$ncols)) {
  #         cat("DEBUG: Reconstructing data.frame from structured format\n")
  #         
  #         # Reconstruct data.frame manually
  #         reconstructed_data <- data.frame(
  #           matrix(nrow = saved_data$nrows, ncol = saved_data$ncols),
  #           stringsAsFactors = FALSE
  #         )
  #         
  #         # Set column names first if available
  #         if (!is.null(saved_data$col_names)) {
  #           names(reconstructed_data) <- saved_data$col_names
  #         }
  #         
  #         # Populate columns one by one
  #         for (i in seq_along(saved_data$values)) {
  #           reconstructed_data[[i]] <- saved_data$values[[i]]
  #         }
  #         
  #         # Restore column classes if available
  #         if (!is.null(saved_data$class_info)) {
  #           for (col_name in names(saved_data$class_info)) {
  #             if (col_name %in% names(reconstructed_data)) {
  #               target_class <- saved_data$class_info[[col_name]]
  #               if (target_class == "numeric") {
  #                 reconstructed_data[[col_name]] <- as.numeric(reconstructed_data[[col_name]])
  #               } else if (target_class == "character") {
  #                 reconstructed_data[[col_name]] <- as.character(reconstructed_data[[col_name]])
  #               } else if (target_class == "logical") {
  #                 reconstructed_data[[col_name]] <- as.logical(reconstructed_data[[col_name]])
  #               }
  #             }
  #           }
  #         }
  #         
  #         values$current_data <- reconstructed_data
  #         values$original_data <- reconstructed_data
  #         
  #       } else {
  #         # Fallback for older save format
  #         values$current_data <- as.data.frame(saved_state$data)
  #         values$original_data <- as.data.frame(saved_state$data)
  #       }
  #       
  #       values$file_uploaded <- TRUE
  #       values$auto_detect_done <- TRUE
  #       
  #       # Restore metadata if available
  #       if (!is.null(saved_state$metadata)) {
  #         cat("DEBUG: Restoring metadata\n")
  #         restore_metadata(session, saved_state$metadata)
  #       }
  #       
  #       # Show notification about auto restore
  #       data_rows <- if (!is.null(saved_state$data$nrows)) {
  #         saved_state$data$nrows
  #       } else {
  #         nrow(saved_state$data)
  #       }
  #       
  #       showNotification(
  #         paste("Tidligere session automatisk genindlæst:", data_rows, "datapunkter fra", 
  #               format(as.POSIXct(saved_state$timestamp), "%d-%m-%Y %H:%M")),
  #         type = "message",
  #         duration = 5
  #       )
  #       
  #       cat("DEBUG: Auto-restore completed successfully\n")
  #       
  #       # Force reset updating_table after a delay
  #       invalidateLater(1000)
  #       isolate({
  #         values$updating_table <- FALSE
  #         values$restoring_session <- FALSE
  #         values$table_version <- values$table_version + 1
  #       })
  #       
  #     } else {
  #       cat("DEBUG: No saved data found in session state\n")
  #     }
  #     
  #   }, error = function(e) {
  #     cat("ERROR during auto-restore:", e$message, "\n")
  #     showNotification(paste("Fejl ved automatisk genindlæsning:", e$message), type = "error")
  #   })
  # }, once = TRUE)
  
  # Manual save handler
  observeEvent(input$manual_save, {
    req(values$current_data)
    
    metadata <- collect_metadata(input)
    
    saveDataLocally(session, values$current_data, metadata)
    values$last_save_time <- Sys.time()
    showNotification("Session gemt lokalt!", type = "message", duration = 2)
  })
  
  # Clear saved handler
  observeEvent(input$clear_saved, {
    handle_clear_saved_request(input, session, values)
  })
  
  # Upload modal handler
  observeEvent(input$show_upload_modal, {
    show_upload_modal()
  })
  
  # Confirm clear saved handler
  observeEvent(input$confirm_clear_saved, {
    handle_confirm_clear_saved(session, values)
  })
  
  # Track file selection for modal
  output$fileSelected <- reactive({
    !is.null(input$data_file) && !is.null(input$data_file$datapath)
  })
  outputOptions(output, "fileSelected", suspendWhenHidden = FALSE)
  
  # Confirm upload handler
  observeEvent(input$confirm_upload, {
    removeModal()
  })
  
  # Save status display
  output$save_status_display <- renderUI({
    if (!is.null(values$last_save_time)) {
      time_diff <- as.numeric(difftime(Sys.time(), values$last_save_time, units = "mins"))
      if (time_diff < 1) {
        span(icon("check"), " Gemt lige nu", style = "color: green;")
      } else if (time_diff < 60) {
        span(icon("clock"), paste(" Gemt for", round(time_diff), "min siden"))
      } else {
        span(icon("clock"), " Gemt for mere end 1 time siden")
      }
    }
  })
  
  # NOTE: output$dataLoaded is now handled in server_helpers.R with smart logic
}

# Helper functions for session management
restore_metadata <- function(session, metadata) {
  isolate({
    if (!is.null(metadata$title)) {
      updateTextInput(session, "indicator_title", value = metadata$title)
    }
    if (!is.null(metadata$unit_type)) {
      updateRadioButtons(session, "unit_type", selected = metadata$unit_type)
    }
    if (!is.null(metadata$unit_select)) {
      updateSelectInput(session, "unit_select", selected = metadata$unit_select)
    }
    if (!is.null(metadata$unit_custom)) {
      updateTextInput(session, "unit_custom", value = metadata$unit_custom)
    }
    if (!is.null(metadata$description)) {
      updateTextAreaInput(session, "indicator_description", value = metadata$description)
    }
    if (!is.null(metadata$chart_type)) {
      updateSelectInput(session, "chart_type", selected = metadata$chart_type)
    }
    if (!is.null(metadata$x_column)) {
      updateSelectInput(session, "x_column", selected = metadata$x_column)
    }
    if (!is.null(metadata$y_column)) {
      updateSelectInput(session, "y_column", selected = metadata$y_column)
    }
    if (!is.null(metadata$n_column)) {
      updateSelectInput(session, "n_column", selected = metadata$n_column)
    }
  })
}

collect_metadata <- function(input) {
  list(
    title = input$indicator_title,
    unit_type = input$unit_type,
    unit_select = input$unit_select,
    unit_custom = input$unit_custom,
    description = input$indicator_description,
    x_column = if(input$x_column == "BLANK") "" else input$x_column,
    y_column = if(input$y_column == "BLANK") "" else input$y_column,
    n_column = if(input$n_column == "BLANK") "" else input$n_column,
    skift_column = if(input$skift_column == "BLANK") "" else input$skift_column,
    kommentar_column = if(input$kommentar_column == "BLANK") "" else input$kommentar_column,
    chart_type = input$chart_type,
    target_value = input$target_value
  )
}

handle_clear_saved_request <- function(input, session, values) {
  # Check if there's data or settings to lose
  has_data <- !is.null(values$current_data) && 
               any(!is.na(values$current_data), na.rm = TRUE) &&
               nrow(values$current_data) > 0
  
  has_settings <- (!is.null(input$indicator_title) && input$indicator_title != "") ||
                  (!is.null(input$indicator_description) && input$indicator_description != "") ||
                  (!is.null(input$unit_select) && input$unit_select != "") ||
                  (!is.null(input$unit_custom) && input$unit_custom != "") ||
                  (!is.null(values$last_save_time))
  
  # If no data or settings, start new session directly
  if (!has_data && !has_settings) {
    reset_to_empty_session(session, values)
    showNotification("Ny session startet", type = "message", duration = 2)
    return()
  }
  
  # If there IS data or settings, show confirmation dialog
  show_clear_confirmation_modal(has_data, has_settings, values)
}

handle_confirm_clear_saved <- function(session, values) {
  reset_to_empty_session(session, values)
  removeModal()
  showNotification("Ny session startet - alt data og indstillinger nulstillet", type = "message", duration = 4)
}

reset_to_empty_session <- function(session, values) {
  clearDataLocally(session)
  values$last_save_time <- NULL
  
  values$updating_table <- TRUE
  
  # Force hide Anhøj rules until real data is loaded
  values$hide_anhoej_rules <- TRUE
  
  # Reset to standard column order
  values$current_data <- data.frame(
    Skift = rep(FALSE, 5),
    Dato = rep(NA_character_, 5),
    Tæller = rep(NA_real_, 5),
    Nævner = rep(NA_real_, 5),
    Kommentar = rep(NA_character_, 5),
    stringsAsFactors = FALSE
  )
  
  values$file_uploaded <- FALSE
  values$user_started_session <- TRUE  # NEW: Set flag that user has started
  values$original_data <- NULL
  values$auto_detect_done <- FALSE
  
  # Reset UI inputs
  isolate({
    updateTextInput(session, "indicator_title", value = "")
    updateRadioButtons(session, "unit_type", selected = "select")
    updateSelectInput(session, "unit_select", selected = "")
    updateTextInput(session, "unit_custom", value = "")
    updateTextAreaInput(session, "indicator_description", value = "")
    updateSelectInput(session, "chart_type", selected = "Seriediagram (Run Chart)")
    updateSelectInput(session, "x_column", selected = "")
    updateSelectInput(session, "y_column", selected = "")
    updateSelectInput(session, "n_column", selected = "")
    shinyjs::reset("data_file")
  })
  
  values$updating_table <- FALSE
}

show_upload_modal <- function() {
  showModal(modalDialog(
    title = div(
      icon("upload"),
      " Upload datafil",
      style = paste("color:", HOSPITAL_COLORS$primary)
    ),
    size = "m",
    
    div(
      style = "margin: 20px 0;",
      
      # File input i modal
      fileInput(
        "data_file",
        "Vælg datafil:",
        accept = c(".xlsx", ".xls", ".csv", ".CSV"),
        placeholder = "Ingen fil valgt...",
        width = "100%"
      ),
      
      hr(),
      
      div(
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; font-size: 0.9rem;",
        h6("Understøttede filformater:", style = "font-weight: 500; margin-bottom: 10px;"),
        tags$ul(
          style = "margin-bottom: 0;",
          tags$li(strong("Excel filer:"), " .xlsx, .xls - med automatisk kolonnedetekttion"),
          tags$li(strong("CSV filer:"), " .csv - danske indstillinger (semikolon, komma som decimal)")
        ),
        br(),
        div(
          style = "font-size: 0.8rem; color: #666;",
          icon("info-circle"),
          " Filer med 'Data' og 'Metadata' sheets genindlæser komplette sessioner automatisk."
        )
      )
    ),
    
    footer = tagList(
      modalButton("Annuller"),
      conditionalPanel(
        condition = "output.fileSelected == true",
        actionButton("confirm_upload", "Upload fil", class = "btn-primary", icon = icon("check"))
      )
    ),
    easyClose = TRUE
  ))
}

show_clear_confirmation_modal <- function(has_data, has_settings, values) {
  showModal(modalDialog(
    title = "Start ny session?",
    size = "m",
    
    div(
      icon("refresh"),
      " Er du sikker på at du vil starte en helt ny session?",
      br(), br(),
      p("Dette vil:"),
      tags$ul(
        if(has_data) tags$li("Slette eksisterende data i tabellen"),
        if(has_settings) tags$li("Nulstille titel, beskrivelse og andre indstillinger"),
        if(!is.null(values$last_save_time)) tags$li("Fjerne gemt session fra lokal storage"),
        tags$li("Oprette en tom standardtabel")
      ),
      br(),
      p("Denne handling kan ikke fortrydes.")
    ),
    
    footer = tagList(
      modalButton("Annuller"),
      actionButton("confirm_clear_saved", "Ja, start ny session", class = "btn-warning")
    ),
    easyClose = FALSE
  ))
}
