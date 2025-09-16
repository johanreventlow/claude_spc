# server_visualization.R
# Server logik for visualisering og data forberedelse

# Dependencies ----------------------------------------------------------------

# VISUALISERING SETUP =========================================================

## Hovedfunktion for visualisering
# Opsætter al server logik relateret til visualisering og data forberedelse
setup_visualization <- function(input, output, session, values) {
  log_debug("=======================================", "VISUALIZATION")
  log_debug("Setting up visualization system", "VISUALIZATION")

  # Data til visualiserings modul
  active_data <- reactive({
    log_debug("Active data reactive triggered", "PLOT_DATA")

    # Check both old and new state management for current_data
    current_data_check <- if (exists("use_centralized_state") && use_centralized_state && exists("app_state")) {
      app_state$data$current_data
    } else {
      values$current_data
    }

    req(current_data_check)
    log_debug("Current data available, processing...", "PLOT_DATA")

    data <- current_data_check
    log_debug(paste("Data dimensions:", nrow(data), "x", ncol(data)), "PLOT_DATA")
    log_debug(paste("Column names:", paste(names(data), collapse = ", ")), "PLOT_DATA")

    # Tilføj hide_anhoej_rules flag som attribut til data
    # PHASE 4: Check both old and new state management for hide_anhoej_rules
    hide_anhoej_rules_check <- if (exists("use_centralized_state") && use_centralized_state && exists("app_state")) {
      app_state$ui$hide_anhoej_rules
    } else {
      values$hide_anhoej_rules
    }

    attr(data, "hide_anhoej_rules") <- hide_anhoej_rules_check
    log_debug(paste("Hide Anhøj rules flag:", hide_anhoej_rules_check), "PLOT_DATA")

    # Tjek om dette er den tomme standard tabel fra session reset
    if (nrow(data) == 5 && all(c("Skift", "Dato", "Tæller", "Nævner", "Kommentar") %in% names(data))) {
      if (all(is.na(data$Dato)) && all(is.na(data$Tæller)) && all(is.na(data$Nævner))) {
        log_debug("⚠️ Detected empty standard table - returning with flag", "PLOT_DATA")
        # Dette er tom standard tabel men vi skal stadig videregive hide flag
        attr(data, "hide_anhoej_rules") <- hide_anhoej_rules_check
        return(data) # Retur nér data med flag i stedet for NULL
      }
    }

    non_empty_rows <- apply(data, 1, function(row) any(!is.na(row)))
    log_debug(paste("Non-empty rows:", sum(non_empty_rows), "out of", nrow(data)), "PLOT_DATA")

    if (any(non_empty_rows)) {
      filtered_data <- data[non_empty_rows, ]
      attr(filtered_data, "hide_anhoej_rules") <- hide_anhoej_rules_check
      log_debug(paste("✅ Returning filtered data:", nrow(filtered_data), "rows"), "PLOT_DATA")
      return(filtered_data)
    } else {
      log_debug("⚠️ No meaningful data found - returning original with flag", "PLOT_DATA")
      # Selv når ingen meningsfuld data, videregiv flaget
      attr(data, "hide_anhoej_rules") <- hide_anhoej_rules_check
      return(data)
    }
  })

  # Kolonne konfiguration til visualisering
  # Store last valid config to avoid NULL during input updates
  log_debug("Setting up column configuration reactives", "VISUALIZATION")
  last_valid_config <- reactiveVal(list(x_col = NULL, y_col = NULL, n_col = NULL, chart_type = "run"))

  # Separate reactives for auto-detected and manual column selection
  auto_detected_config <- reactive({
    # PHASE 4: Check both old and new state management for auto_detected_columns
    auto_columns <- if (exists("use_centralized_state") && use_centralized_state && exists("app_state")) {
      app_state$columns$auto_detect$results
    } else {
      values$auto_detected_columns
    }

    req(auto_columns)
    list(
      x_col = auto_columns$x_col,
      y_col = auto_columns$y_col,
      n_col = auto_columns$n_col,
      chart_type = get_qic_chart_type(if (is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type)
    )
  })

  manual_config <- reactive({
    x_col <- if (!is.null(input$x_column) && input$x_column != "") input$x_column else NULL
    y_col <- if (!is.null(input$y_column) && input$y_column != "") input$y_column else NULL
    n_col <- if (!is.null(input$n_column) && input$n_column != "") input$n_column else NULL

    list(
      x_col = x_col,
      y_col = y_col,
      n_col = n_col,
      chart_type = get_qic_chart_type(if (is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type)
    )
  })

  # Clear column config selection - prioritize manual input when available
  column_config <- reactive({
    # Prioritize manual config when user has made selections
    manual_config_check <- manual_config()
    if (!is.null(manual_config_check) && !is.null(manual_config_check$y_col)) {
      return(manual_config_check)
    }

    # Fall back to auto-detected config
    auto_config <- auto_detected_config()
    if (!is.null(auto_config) && !is.null(auto_config$y_col)) {
      return(auto_config)
    }

    # Final fallback - return NULL if neither available
    return(NULL)
  })

  # Observer to update last_valid_config (side effects outside reactives)
  observe({
    config <- column_config()
    if (!is.null(config$y_col)) {
      last_valid_config(config)
    }
  })


  # Initialiser visualiserings modul
  log_debug("Initializing visualization module server", "VISUALIZATION")
  visualization <- visualizationModuleServer(
    "visualization",
    data_reactive = active_data,
    column_config_reactive = column_config,
    chart_type_reactive = reactive({
      chart_selection <- if (is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type
      get_qic_chart_type(chart_selection)
    }),
    target_value_reactive = reactive({
      if (is.null(input$target_value) || input$target_value == "") {
        return(NULL)
      }

      # Hent y-akse data til smart konvertering
      data <- active_data()
      config <- column_config()

      if (!is.null(data) && !is.null(config) && !is.null(config$y_col) && config$y_col %in% names(data)) {
        y_data <- data[[config$y_col]]
        y_numeric <- parse_danish_number(y_data)
        return(parse_danish_target(input$target_value, y_numeric))
      } else {
        return(parse_danish_target(input$target_value, NULL))
      }
    }),
    centerline_value_reactive = reactive({
      if (is.null(input$centerline_value) || input$centerline_value == "") {
        return(NULL)
      }

      # Hent y-akse data til smart konvertering
      data <- active_data()
      config <- column_config()

      if (!is.null(data) && !is.null(config) && !is.null(config$y_col) && config$y_col %in% names(data)) {
        y_data <- data[[config$y_col]]
        y_numeric <- parse_danish_number(y_data)
        return(parse_danish_target(input$centerline_value, y_numeric))
      } else {
        return(parse_danish_target(input$centerline_value, NULL))
      }
    }),
    skift_config_reactive = reactive({
      # Bestem om vi skal vise faser baseret på Skift kolonne valg og data
      data <- active_data()
      config <- column_config()

      if (is.null(data) || is.null(config)) {
        return(list(show_phases = FALSE, skift_column = NULL))
      }

      # Tjek om bruger har valgt en Skift kolonne
      skift_col <- if (!is.null(input$skift_column) && input$skift_column != "") {
        input$skift_column
      } else {
        NULL
      }

      # Hvis ingen Skift kolonne valgt, ingen faser
      if (is.null(skift_col) || !skift_col %in% names(data)) {
        return(list(show_phases = FALSE, skift_column = NULL))
      }

      # Tjek om Skift kolonne har nogen TRUE værdier
      skift_data <- data[[skift_col]]
      has_phase_shifts <- any(skift_data == TRUE, na.rm = TRUE)

      return(list(
        show_phases = has_phase_shifts,
        skift_column = skift_col
      ))
    }),
    frys_config_reactive = reactive({
      # Bestem frys kolonne for baseline freeze
      data <- active_data()
      config <- column_config()

      if (is.null(data) || is.null(config)) {
        return(NULL)
      }

      # Tjek om bruger har valgt en Frys kolonne
      frys_col <- if (!is.null(input$frys_column) && input$frys_column != "") {
        input$frys_column
      } else {
        NULL
      }

      # Hvis ingen Frys kolonne valgt eller ikke i data, returner NULL
      if (is.null(frys_col) || !frys_col %in% names(data)) {
        return(NULL)
      }

      return(frys_col)
    }),
    chart_title_reactive = chart_title(input),
    y_axis_unit_reactive = reactive({
      if (is.null(input$y_axis_unit) || input$y_axis_unit == "") {
        return("count")
      } else {
        return(input$y_axis_unit)
      }
    }),
    kommentar_column_reactive = reactive({
      if (is.null(input$kommentar_column) || input$kommentar_column == "") {
        return(NULL)
      } else {
        return(input$kommentar_column)
      }
    })
  )

  # Plot klar tjek
  output$plot_ready <- reactive({
    result <- !is.null(visualization$plot_ready()) && visualization$plot_ready()
    return(if (result) "true" else "false")
  })
  outputOptions(output, "plot_ready", suspendWhenHidden = FALSE)

  # Retur nér visualiserings objekt til brug i download handlers
  return(visualization)
}
