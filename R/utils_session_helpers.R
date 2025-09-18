# server_helpers.R
# Server hjælpefunktioner og utility observers

# Dependencies ----------------------------------------------------------------

# HJÆLPEFUNKTIONER SETUP ====================================================

## Hovedfunktion for hjælper
# Opsætter alle hjælper observers og status funktioner
setup_helper_observers <- function(input, output, session, obs_manager = NULL, app_state = NULL) {
  # PHASE 4: Centralized state is now always available
  # UNIFIED STATE: Empty table initialization now handled through session management events

  # UNIFIED NAVIGATION: Event-driven pattern using app_state navigation trigger
  # This creates a reactive that updates when navigation_changed events are fired
  app_data_reactive <- eventReactive(app_state$navigation$trigger, {
    current_data_value <- app_state$data$current_data

    cat("DEBUG: [NAVIGATION_UNIFIED] app_data_reactive triggered\n")
    cat("DEBUG: [NAVIGATION_UNIFIED] trigger_value:", app_state$navigation$trigger, "\n")
    cat("DEBUG: [NAVIGATION_UNIFIED] current_data is null:", is.null(current_data_value), "\n")

    # Return the actual data from unified state
    return(current_data_value)
  }, ignoreNULL = FALSE)

  # Data indlæsnings status flags - følger BFH UTH mønster
  output$dataLoaded <- renderText({
    # NAVIGATION TRIGGER: Use eventReactive pattern for app_state navigation
    # This now properly triggers when navigation events are emitted
    current_data_check <- app_data_reactive()

    cat("DEBUG: [NAVIGATION] Evaluating dataLoaded status\n")
    cat("DEBUG: [NAVIGATION] current_data_check is null:", is.null(current_data_check), "\n")

    result <- if (is.null(current_data_check)) {
      cat("DEBUG: [NAVIGATION] No current data - showing welcome screen\n")
      "FALSE"
    } else {
      # Tjek om data har meningsfuldt indhold (ikke bare tom skabelon)
      # Tjek også om bruger aktivt er startet på at arbejde (fil uploadet eller startet manuelt)
      meaningful_data <- any(sapply(current_data_check, function(x) {
        if (is.logical(x)) {
          return(any(x, na.rm = TRUE))
        }
        if (is.numeric(x)) {
          return(any(!is.na(x)))
        }
        if (is.character(x)) {
          return(any(nzchar(x, keepNA = FALSE), na.rm = TRUE))
        }
        return(FALSE)
      }))

      # Betragt kun data som indlæst hvis:
      # 1. Der er meningsfuldt data, ELLER
      # 2. Bruger har uploadet en fil, ELLER
      # 3. Bruger har eksplicit startet en ny session
      # PHASE 4: Use unified state management
      file_uploaded_check <- app_state$session$file_uploaded

      # PHASE 4: Use unified state management
      user_started_session_check <- app_state$session$user_started_session

      user_has_started <- file_uploaded_check || user_started_session_check %||% FALSE

      cat("DEBUG: [NAVIGATION] meaningful_data:", meaningful_data, "\n")
      cat("DEBUG: [NAVIGATION] file_uploaded_check:", file_uploaded_check, "\n")
      cat("DEBUG: [NAVIGATION] user_started_session_check:", user_started_session_check, "\n")
      cat("DEBUG: [NAVIGATION] user_has_started:", user_has_started, "\n")

      final_result <- if (meaningful_data || user_has_started) "TRUE" else "FALSE"
      cat("DEBUG: [NAVIGATION] Final dataLoaded result:", final_result, "\n")
      final_result
    }
    cat("DEBUG: [NAVIGATION] Returning dataLoaded:", result, "\n")
    result
  })
  outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)

  output$has_data <- renderText({
    # NAVIGATION TRIGGER: Use eventReactive pattern for consistent behavior
    current_data_check <- app_data_reactive()

    if (is.null(current_data_check)) {
      "false"
    } else {
      # Tjek om data har meningsfuldt indhold (ikke bare tom skabelon)
      meaningful_data <- any(sapply(current_data_check, function(x) {
        if (is.logical(x)) {
          return(any(x, na.rm = TRUE))
        }
        if (is.numeric(x)) {
          return(any(!is.na(x)))
        }
        if (is.character(x)) {
          return(any(nzchar(x, keepNA = FALSE), na.rm = TRUE))
        }
        return(FALSE)
      }))
      if (meaningful_data) "true" else "false"
    }
  })
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)


  # Data status visning
  output$data_status_display <- renderUI({
    # PHASE 4: Use unified state management
    file_uploaded_check <- app_state$session$file_uploaded

    # NAVIGATION TRIGGER: Use eventReactive pattern for consistent behavior
    current_data_check <- app_data_reactive()

    if (is.null(current_data_check)) {
      div(
        span(class = "status-indicator status-warning"),
        "Ingen data",
        style = "font-size: 0.9rem;"
      )
    } else if (file_uploaded_check) {
      data_rows <- sum(!is.na(current_data_check[[1]]))
      div(
        span(class = "status-indicator status-ready"),
        paste("Fil uploadet -", data_rows, "datapunkter"),
        style = "font-size: 0.9rem;"
      )
    } else {
      data_rows <- sum(!is.na(current_data_check[[1]]))
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


  # Reaktiv debounced auto-save - følger Shiny best practices
  auto_save_trigger <- debounce(reactive({
    # Guards for at forhindre auto-gem under tabel operationer
    # PHASE 4: Use unified state management
    updating_table_check <- app_state$data$updating_table

    # PHASE 4: Use unified state management
    auto_save_enabled_check <- app_state$session$auto_save_enabled

    # PHASE 4: Use unified state management
    restoring_session_check <- app_state$session$restoring_session

    # PHASE 4: Use unified state management
    table_operation_check <- app_state$data$table_operation_in_progress

    if (!auto_save_enabled_check ||
      updating_table_check ||
      table_operation_check ||
      restoring_session_check) {
      return(NULL)
    }

    # PHASE 4: Use unified state management
    current_data_check <- app_state$data$current_data

    if (!is.null(current_data_check) &&
      nrow(current_data_check) > 0 &&
      any(!is.na(current_data_check))) {
      list(
        data = current_data_check,
        metadata = collect_metadata(input),
        timestamp = Sys.time()
      )
    } else {
      NULL
    }
  }), millis = 2000)

  obs_data_save <- observe({
    save_data <- auto_save_trigger()
    req(save_data)  # Only proceed if we have valid save data

    autoSaveAppState(session, save_data$data, save_data$metadata)
    # PHASE 4: Use unified state management
    app_state$session$last_save_time <- save_data$timestamp
  })

  # Register observer with manager
  if (!is.null(obs_manager)) {
    obs_manager$add(obs_data_save, "data_auto_save")
  }

  # Reaktiv debounced settings save - følger Shiny best practices
  settings_save_trigger <- debounce(reactive({
    # Samme guards som data auto-gem
    # PHASE 4: Use unified state management
    updating_table_check <- app_state$data$updating_table

    # PHASE 4: Use unified state management
    auto_save_enabled_check <- app_state$session$auto_save_enabled

    # PHASE 4: Use unified state management
    restoring_session_check <- app_state$session$restoring_session

    # PHASE 4: Use unified state management
    table_operation_check_settings <- app_state$data$table_operation_in_progress

    if (!auto_save_enabled_check ||
      updating_table_check ||
      table_operation_check_settings ||
      restoring_session_check) {
      return(NULL)
    }

    # PHASE 4: Use unified state management
    current_data_check <- app_state$data$current_data

    if (!is.null(current_data_check)) {
      list(
        data = current_data_check,
        metadata = collect_metadata(input),
        timestamp = Sys.time()
      )
    } else {
      NULL
    }
  }), millis = 1000)  # Faster debounce for settings

  obs_settings_save <- observe({
    save_data <- settings_save_trigger()
    req(save_data)  # Only proceed if we have valid save data

    autoSaveAppState(session, save_data$data, save_data$metadata)
    # PHASE 4: Use unified state management
    app_state$session$last_save_time <- save_data$timestamp
  }) %>%
    bindEvent(
      {
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
          input$y_axis_unit
        )
      },
      ignoreInit = TRUE
    )

  # Event-driven table operation cleanup - replaces later::later() anti-pattern
  table_cleanup_trigger <- debounce(reactive({
    # PHASE 4: Use unified state management
    table_operation_cleanup_needed_check <- app_state$data$table_operation_cleanup_needed

    if (table_operation_cleanup_needed_check) {
      Sys.time()  # Return timestamp to trigger cleanup
    } else {
      NULL
    }
  }), millis = 2000)

  observe({
    cleanup_time <- table_cleanup_trigger()
    req(cleanup_time)  # Only proceed if cleanup is needed

    # Clear the table operation flag and reset cleanup request
    # PHASE 4: Use unified state management
    app_state$data$table_operation_in_progress <- FALSE
    app_state$data$table_operation_cleanup_needed <- FALSE
  })

  # Register observer with manager
  if (!is.null(obs_manager)) {
    obs_manager$add(obs_settings_save, "settings_auto_save")
  }

  # UNIFIED NAVIGATION: Return app_data_reactive for backward compatibility
  # Navigation is now managed through app_state$navigation$trigger and emit$navigation_changed()
  return(app_data_reactive)
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

# ensure_standard_columns funktionen er nu defineret i global.R

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
      selected_unit <- if (is.null(input$unit_select)) "" else input$unit_select
      if (selected_unit != "" && selected_unit %in% names(unit_names)) {
        return(unit_names[[selected_unit]])
      } else {
        return("")
      }
    } else {
      return(if (is.null(input$unit_custom)) "" else input$unit_custom)
    }
  })
}

## Komplet chart titel
# Reaktiv funktion for komplet chart titel
chart_title <- function(input) {
  reactive({
    base_title <- if (is.null(input$indicator_title) || input$indicator_title == "") "SPC Analyse" else input$indicator_title
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
