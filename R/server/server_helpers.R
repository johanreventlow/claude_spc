# server_helpers.R
# Server hjælpefunktioner og utility observers

# Dependencies ----------------------------------------------------------------

# HJÆLPEFUNKTIONER SETUP ====================================================

## Hovedfunktion for hjælper
# Opsætter alle hjælper observers og status funktioner
setup_helper_observers <- function(input, output, session, values) {
  
  # Initialiser ikke automatisk tom tabel ved opstart - vent på bruger aktion
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
  
  # Data indlæsnings status flags - følger BFH UTH mønster
  output$dataLoaded <- renderText({
    result <- if (is.null(values$current_data)) {
      "FALSE"
    } else {
      # Tjek om data har meningsfuldt indhold (ikke bare tom skabelon)
      # Tjek også om bruger aktivt er startet på at arbejde (fil uploadet eller startet manuelt)
      meaningful_data <- any(sapply(values$current_data, function(x) {
        if (is.logical(x)) return(any(x, na.rm = TRUE))
        if (is.numeric(x)) return(any(!is.na(x)))
        if (is.character(x)) return(any(nzchar(x, keepNA = FALSE), na.rm = TRUE))
        return(FALSE)
      }))
      
      # Betragt kun data som indlæst hvis:
      # 1. Der er meningsfuldt data, ELLER
      # 2. Bruger har uploadet en fil, ELLER 
      # 3. Bruger har eksplicit startet en ny session
      user_has_started <- values$file_uploaded || values$user_started_session %||% FALSE
      
      if (meaningful_data || user_has_started) "TRUE" else "FALSE"
    }
    result
  })
  outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)
  
  output$has_data <- renderText({
    if (is.null(values$current_data)) {
      "false"
    } else {
      # Tjek om data har meningsfuldt indhold (ikke bare tom skabelon)
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
  
  
  # Data status visning
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
  
  
  # Auto-gem når data ændres (med guards for at forhindre uendelige løkker)
  observeEvent(values$current_data, {
    # Starkere guards for at forhindre auto-gem under tabel operationer
    if (!values$auto_save_enabled || 
        values$updating_table || 
        values$table_operation_in_progress ||
        values$restoring_session) {
      return()
    }
    
    if (!is.null(values$current_data) && 
        nrow(values$current_data) > 0 && 
        any(!is.na(values$current_data))) {
      
      metadata <- collect_metadata(input)
      autoSaveAppState(session, values$current_data, metadata)
      values$last_save_time <- Sys.time()
    }
  }, ignoreInit = TRUE)
  
  # Auto-gem når indstillinger ændres (med guards for at forhindre konflikter)
  observe({
    # Samme starkere guards som data auto-gem  
    if (!values$auto_save_enabled || 
        values$updating_table || 
        values$table_operation_in_progress ||
        values$restoring_session) {
      return()
    }
    
    if (!is.null(values$current_data)) {
      
      metadata <- collect_metadata(input)
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
        input$skift_column,
        input$kommentar_column,
        input$chart_type,
        input$target_value,
        input$x_axis_unit,
        input$y_axis_unit
      )
    }, ignoreInit = TRUE)
}

# HJÆLPEFUNKTIONER ============================================================

## Opret tom session data
# Opretter standarddatastruktur for nye sessioner
create_empty_session_data <- function() {
  data.frame(
    Skift = rep(FALSE, 20),
    Frys = rep(FALSE, 20),
    Dato = rep(NA_character_, 20),
    Tæller = rep(NA_real_, 20),
    Nævner = rep(NA_real_, 20),
    Kommentar = rep(NA_character_, 20),
    stringsAsFactors = FALSE
  )
}

## Sikr standard kolonner
# Denne funktion sikrer at uploadede data har vores standard kolonner
# i den rigtige rækkefølge mens eksisterende data bevares
ensure_standard_columns <- function(data) {
  
  # Standard kolonner med Skift og Frys altid først
  standard_cols <- c("Skift", "Frys", "Dato", "Tæller", "Nævner", "Kommentar")
  current_cols <- names(data)
  
  # Tilføj manglende standard kolonner
  for (col in standard_cols) {
    if (!col %in% current_cols) {
      if (col == "Skift" || col == "Frys") {
        data[[col]] <- rep(FALSE, nrow(data))
      } else {
        data[[col]] <- rep(NA_character_, nrow(data))
      }
    }
  }
  
  # Omorganiser for at sætte standard kolonner først
  other_cols <- setdiff(current_cols, standard_cols)
  final_order <- c(standard_cols, other_cols)
  
  data <- data[final_order]
  
  return(data)
}

## Aktuel organisatorisk enhed
# Reaktiv funktion for nuværende organisatoriske enhed
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

## Komplet chart titel
# Reaktiv funktion for komplet chart titel
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
