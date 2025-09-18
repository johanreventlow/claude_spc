# server_visualization.R
# Server logik for visualisering og data forberedelse

# Dependencies ----------------------------------------------------------------

# VISUALISERING SETUP =========================================================

## Hovedfunktion for visualisering
# Opsætter al server logik relateret til visualisering og data forberedelse
setup_visualization <- function(input, output, session, app_state) {
  log_debug("=======================================", "VISUALIZATION")
  log_debug("Setting up visualization system", "VISUALIZATION")

  # UNIFIED EVENT SYSTEM: Direct access to app_state data instead of reactive dependencies
  # No need for app_data_reactive - visualization module uses its own event-driven data access
  log_debug("Visualization setup - using unified event system", "VISUALIZATION")

  # Kolonne konfiguration til visualisering
  # Store last valid config to avoid NULL during input updates
  log_debug("Setting up column configuration reactives", "VISUALIZATION")
  last_valid_config <- reactiveVal(list(x_col = NULL, y_col = NULL, n_col = NULL, chart_type = "run"))

  # Separate reactives for auto-detected and manual column selection
  auto_detected_config <- reactive({
    log_debug("======================================", "AUTO_DETECTED_CONFIG")
    log_debug("auto_detected_config reactive triggered", "AUTO_DETECTED_CONFIG")

    # PHASE 4: Use unified state management - CORRECTED PATH
    auto_columns <- app_state$columns$auto_detected_columns
    log_debug(paste("Auto columns state (auto_detected_columns):", if(is.null(auto_columns)) "NULL" else "PRESENT"), "AUTO_DETECTED_CONFIG")

    if (!is.null(auto_columns)) {
      log_debug(paste("Auto detected columns - X:", auto_columns$x_col, "Y:", auto_columns$y_col, "N:", auto_columns$n_col), "AUTO_DETECTED_CONFIG")
      if (!is.null(auto_columns$timestamp)) {
        log_debug(paste("Auto detection timestamp:", auto_columns$timestamp), "AUTO_DETECTED_CONFIG")
      }
    }

    req(auto_columns)

    config <- list(
      x_col = auto_columns$x_col,
      y_col = auto_columns$y_col,
      n_col = auto_columns$n_col,
      chart_type = get_qic_chart_type(if (is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type)
    )

    log_debug(paste("✅ auto_detected_config returning:", paste(names(config), config, sep="=", collapse=", ")), "AUTO_DETECTED_CONFIG")
    return(config)
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

  # Simplified column config - single source of truth
  column_config <- reactive({
    log_debug("======================================", "COLUMN_CONFIG")
    log_debug("column_config reactive triggered (simplified)", "COLUMN_CONFIG")

    # Always prioritize manual input when available (user selections)
    manual_config_check <- manual_config()
    log_debug(paste("Manual config Y column:", if(is.null(manual_config_check) || is.null(manual_config_check$y_col)) "NULL" else manual_config_check$y_col), "COLUMN_CONFIG")

    # If user has made explicit selections, use those
    if (!is.null(manual_config_check) && !is.null(manual_config_check$y_col)) {
      log_debug("✅ Using manual config (user selections take priority)", "COLUMN_CONFIG")
      return(manual_config_check)
    }

    # Otherwise, try auto-detected config as fallback only
    log_debug("Manual config not available, checking auto-detected config", "COLUMN_CONFIG")

    # Simplified auto-config access with better error handling
    auto_columns <- app_state$columns$auto_detected_columns
    if (!is.null(auto_columns) && !is.null(auto_columns$y_col)) {
      log_debug("✅ Using auto-detected config as fallback", "COLUMN_CONFIG")
      return(list(
        x_col = auto_columns$x_col,
        y_col = auto_columns$y_col,
        n_col = auto_columns$n_col,
        chart_type = get_qic_chart_type(if (is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type)
      ))
    }

    # No valid config available
    log_debug("⚠️ No valid config found - returning NULL", "COLUMN_CONFIG")
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
    data_reactive = NULL,  # Module uses its own event-driven data access
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
      data <- app_state$data$current_data
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
      data <- app_state$data$current_data
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
      data <- app_state$data$current_data
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
      data <- app_state$data$current_data
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
    }),
    app_state = app_state
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
