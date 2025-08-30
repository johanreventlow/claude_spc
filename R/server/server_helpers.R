# R/server/server_helpers.R
# Helper functions and utility observers

setup_helper_observers <- function(input, output, session, values) {
  
  # Don't auto-initialize empty table on startup - wait for user action
  # observe({
  #   if (is.null(values$current_data)) {
  #     empty_data <- data.frame(
  #       Skift = rep(FALSE, 5),
  #       Dato = rep(NA_character_, 5),
  #       Tæller = rep(NA_real_, 5), 
  #       Nævner = rep(NA_real_, 5),
  #       Kommentar = rep(NA_character_, 5),
  #       stringsAsFactors = FALSE
  #     )
  #     values$current_data <- empty_data
  #   }
  # })
  
  # Data loading status flags - following BFH UTH pattern
  output$dataLoaded <- renderText({
    result <- if (is.null(values$current_data)) {
      "FALSE"
    } else {
      # Check if data has meaningful content (not just empty template)
      # Also check if user has actively started working (file uploaded or started manually)
      meaningful_data <- any(sapply(values$current_data, function(x) {
        if (is.logical(x)) return(any(x, na.rm = TRUE))
        if (is.numeric(x)) return(any(!is.na(x)))
        if (is.character(x)) return(any(nzchar(x, keepNA = FALSE), na.rm = TRUE))
        return(FALSE)
      }))
      
      # Only consider data loaded if:
      # 1. There's meaningful data, OR
      # 2. User has uploaded a file, OR 
      # 3. User has explicitly started a new session
      user_has_started <- values$file_uploaded || values$user_started_session %||% FALSE
      
      if (meaningful_data || user_has_started) "TRUE" else "FALSE"
    }
    cat("DEBUG: output$dataLoaded returning:", result, "\n")
    result
  })
  outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)
  
  output$has_data <- renderText({
    if (is.null(values$current_data)) {
      "false"
    } else {
      # Check if data has meaningful content (not just empty template)
      meaningful_data <- any(sapply(values$current_data, function(x) {
        if (is.logical(x)) return(any(x, na.rm = TRUE))
        if (is.numeric(x)) return(any(!is.na(x)))
        if (is.character(x)) return(any(nzchar(x, keepNA = FALSE), na.rm = TRUE))
        return(FALSE)
      }))
      if (meaningful_data) "true" else "false"
    }
  })
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)
  
  output$plot_ready <- renderText({
    # Plot is ready when we have data and valid column mappings
    data_ready <- !is.null(values$current_data) && any(sapply(values$current_data, function(x) {
      if (is.logical(x)) return(any(x, na.rm = TRUE))
      if (is.numeric(x)) return(any(!is.na(x)))
      if (is.character(x)) return(any(nzchar(x, keepNA = FALSE), na.rm = TRUE))
      return(FALSE)
    }))
    
    columns_ready <- !is.null(input$x_column) && !is.null(input$y_column) &&
                    input$x_column != "" && input$y_column != ""
    
    if (data_ready && columns_ready) "true" else "false"
  })
  outputOptions(output, "plot_ready", suspendWhenHidden = FALSE)
  
  # Data status display
  output$data_status_display <- renderUI({
    if (is.null(values$current_data)) {
      div(
        span(class = "status-indicator status-warning"),
        "Ingen data",
        style = "font-size: 0.9rem;"
      )
    } else if (values$file_uploaded) {
      data_rows <- sum(!is.na(values$current_data[[1]]))
      div(
        span(class = "status-indicator status-ready"),
        paste("Fil uploadet -", data_rows, "datapunkter"),
        style = "font-size: 0.9rem;"
      )
    } else {
      data_rows <- sum(!is.na(values$current_data[[1]]))
      if (data_rows > 0) {
        div(
          span(class = "status-indicator status-processing"),
          paste("Manuel indtastning -", data_rows, "datapunkter"),
          style = "font-size: 0.9rem;"
        )
      } else {
        div(
          span(class = "status-indicator status-warning"),
          "Tom tabel - indtast data eller upload fil",
          style = "font-size: 0.9rem;"
        )
      }
    }
  })
  
  # Anti-stuck mechanism
  observeEvent(values$current_data, {
    invalidateLater(200)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  # Auto-save when data changes
  observeEvent(values$current_data, {
    # Guards to prevent auto-save under restore
    if (!values$auto_save_enabled || 
        values$updating_table || 
        values$restoring_session) {
      cat("DEBUG: Auto-save skipped - guards active\n")
      return()
    }
    
    if (!is.null(values$current_data) && 
        nrow(values$current_data) > 0 && 
        any(!is.na(values$current_data))) {
      
      cat("DEBUG: Triggering auto-save for data change\n")
      
      metadata <- collect_metadata(input)
      
      autoSaveAppState(session, values$current_data, metadata)
      values$last_save_time <- Sys.time()
    }
  }, ignoreInit = TRUE)
  
  # Auto-save when settings change
  observe({
    # Same guards as data auto-save
    if (!values$auto_save_enabled || 
        values$updating_table || 
        values$restoring_session) {
      return()
    }
    
    if (!is.null(values$current_data)) {
      cat("DEBUG: Settings changed - scheduling auto-save\n")
      
      metadata <- collect_metadata(input)
      
      # Debounce with 2 seconds delay
      invalidateLater(2000)
      autoSaveAppState(session, values$current_data, metadata)
      values$last_save_time <- Sys.time()
    }
  }) %>% 
    bindEvent({
      list(
        input$indicator_title,
        input$unit_type,
        input$unit_select,
        input$unit_custom,
        input$indicator_description,
        input$x_column,
        input$y_column,
        input$n_column,
        input$chart_type
      )
    }, ignoreInit = TRUE)
}

# Helper function to ensure standard columns
ensure_standard_columns <- function(data) {
  # This function ensures that uploaded data has our standard columns
  # in the right order while preserving existing data
  
  standard_cols <- c("Skift", "Dato", "Tæller", "Nævner", "Kommentar")
  current_cols <- names(data)
  
  # Add missing standard columns
  for (col in standard_cols) {
    if (!col %in% current_cols) {
      if (col == "Skift") {
        data[[col]] <- rep(FALSE, nrow(data))
      } else {
        data[[col]] <- rep(NA_character_, nrow(data))
      }
    }
  }
  
  # Reorder to put standard columns first
  other_cols <- setdiff(current_cols, standard_cols)
  final_order <- c(standard_cols, other_cols)
  
  data <- data[final_order]
  
  return(data)
}

# Reactive for current organizational unit
current_unit <- function(input) {
  reactive({
    if (input$unit_type == "select") {
      unit_names <- list(
        "med" = "Medicinsk Afdeling",
        "kir" = "Kirurgisk Afdeling", 
        "icu" = "Intensiv Afdeling",
        "amb" = "Ambulatorie",
        "akut" = "Akutmodtagelse",
        "paed" = "Pædiatrisk Afdeling",
        "gyn" = "Gynækologi/Obstetrik"
      )
      selected_unit <- if(is.null(input$unit_select)) "" else input$unit_select
      if (selected_unit != "" && selected_unit %in% names(unit_names)) {
        return(unit_names[[selected_unit]])
      } else {
        return("")
      }
    } else {
      return(if(is.null(input$unit_custom)) "" else input$unit_custom)
    }
  })
}

# Reactive for complete chart title
chart_title <- function(input) {
  reactive({
    base_title <- if(is.null(input$indicator_title) || input$indicator_title == "") "SPC Analyse" else input$indicator_title
    unit_name <- current_unit(input)()
    
    if (base_title != "SPC Analyse" && unit_name != "") {
      return(paste(base_title, "-", unit_name))
    } else if (base_title != "SPC Analyse") {
      return(base_title)
    } else if (unit_name != "") {
      return(paste("SPC Analyse -", unit_name))
    } else {
      return("SPC Analyse")
    }
  })
}
