# server_visualization.R
# Server logik for visualisering og data forberedelse

# Dependencies ----------------------------------------------------------------

# VISUALISERING SETUP =========================================================

## Hovedfunktion for visualisering
# Opsætter al server logik relateret til visualisering og data forberedelse
setup_visualization <- function(input, output, session, app_state) {
  # Operation completed
  # Operation completed

  # INPUT SANITIZATION: Using centralized sanitize_selection from utils_ui_helpers.R

  # UNIFIED EVENT SYSTEM: Direct access to app_state data instead of reactive dependencies
  # No need for app_data_reactive - visualization module uses its own event-driven data access
  # Operation completed

  # Kolonne konfiguration til visualisering
  # Store last valid config to avoid NULL during input updates
  # Operation completed
  # Initialize last_valid_config in app_state if not already present
  # Use isolate() to safely check reactive value outside reactive context
  current_config <- isolate(app_state$visualization$last_valid_config)
  if (is.null(current_config)) {
    app_state$visualization$last_valid_config <- list(x_col = NULL, y_col = NULL, n_col = NULL, chart_type = "run")
  }

  # Separate reactives for auto-detected and manual column selection
  auto_detected_config <- shiny::reactive({
    # Operation completed
    # Operation completed

    # Use unified state management - CORRECTED PATH
    auto_columns <- app_state$columns$auto_detect$results
    # Operation completed

    if (!is.null(auto_columns)) {
      # Operation completed
      if (!is.null(auto_columns$timestamp)) {
        # Operation completed
      }
    }

    shiny::req(auto_columns)

    config <- list(
      x_col = auto_columns$x_col,
      y_col = auto_columns$y_col,
      n_col = auto_columns$n_col,
      chart_type = get_qic_chart_type(if (is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type)
    )

    # Operation completed
    return(config)
  })

  manual_config <- shiny::reactive({
    x_col <- sanitize_selection(input$x_column)
    y_col <- sanitize_selection(input$y_column)
    n_col <- sanitize_selection(input$n_column)

    list(
      x_col = x_col,
      y_col = y_col,
      n_col = n_col,
      chart_type = get_qic_chart_type(if (is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type)
    )
  })

  # Simplified column config - single source of truth without debouncing for valuebox stability
  column_config <- shiny::reactive({
    # Operation completed
    # Operation completed

    # Always prioritize manual input when available (user selections)
    manual_config_check <- manual_config()
    # Operation completed

    # If user has made explicit selections, use those
    if (!is.null(manual_config_check) && !is.null(manual_config_check$y_col)) {
      # Operation completed
      return(manual_config_check)
    }

    # Otherwise, try auto-detected config as fallback only
    # Operation completed

    # Simplified auto-config access with better error handling
    auto_columns <- app_state$columns$auto_detect$results
    if (!is.null(auto_columns) && !is.null(auto_columns$y_col)) {
      # Operation completed
      return(list(
        x_col = auto_columns$x_col,
        y_col = auto_columns$y_col,
        n_col = auto_columns$n_col,
        chart_type = get_qic_chart_type(if (is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type)
      ))
    }

    # No valid config available
    # Operation completed
    return(NULL)
  })

  # Observer to update last_valid_config (side effects outside reactives)
  shiny::observe({
    config <- column_config()
    if (!is.null(config$y_col)) {
      app_state$visualization$last_valid_config <- config
    }
  })

  # Initialiser visualiserings modul - no debouncing for valuebox stability
  # Operation completed
  visualization <- visualizationModuleServer(
    "visualization",
    data_reactive = NULL,  # Module uses its own event-driven data access
    column_config_reactive = column_config,
    chart_type_reactive = shiny::reactive({
      chart_selection <- if (is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type
      get_qic_chart_type(chart_selection)
    }),
    target_value_reactive = shiny::reactive({
      if (is.null(input$target_value) || input$target_value == "") {
        return(NULL)
      }

      # Hent y-akse data og enhed til smart konvertering
      data <- app_state$data$current_data
      config <- column_config()
      y_unit <- if (is.null(input$y_axis_unit) || input$y_axis_unit == "") NULL else input$y_axis_unit

      if (!is.null(data) && !is.null(config) && !is.null(config$y_col) && config$y_col %in% names(data)) {
        y_data <- data[[config$y_col]]
        y_numeric <- parse_danish_number(y_data)
        return(parse_danish_target(input$target_value, y_numeric, y_unit))
      } else {
        return(parse_danish_target(input$target_value, NULL, y_unit))
      }
    }),
    centerline_value_reactive = shiny::reactive({
      if (is.null(input$centerline_value) || input$centerline_value == "") {
        return(NULL)
      }

      # Hent y-akse data og enhed til smart konvertering
      data <- app_state$data$current_data
      config <- column_config()
      y_unit <- if (is.null(input$y_axis_unit) || input$y_axis_unit == "") NULL else input$y_axis_unit

      if (!is.null(data) && !is.null(config) && !is.null(config$y_col) && config$y_col %in% names(data)) {
        y_data <- data[[config$y_col]]
        y_numeric <- parse_danish_number(y_data)
        return(parse_danish_target(input$centerline_value, y_numeric, y_unit))
      } else {
        return(parse_danish_target(input$centerline_value, NULL, y_unit))
      }
    }),
    skift_config_reactive = shiny::reactive({
      # Bestem om vi skal vise faser baseret på Skift kolonne valg og data
      data <- app_state$data$current_data
      config <- column_config()

      if (is.null(data) || is.null(config)) {
        return(list(show_phases = FALSE, skift_column = NULL))
      }

      # Tjek om bruger har valgt en Skift kolonne - brug robust sanitization
      skift_col <- sanitize_selection(input$skift_column)

      # Hvis ingen Skift kolonne valgt, ingen faser
      if (is.null(skift_col) || !skift_col %in% names(data)) {
        return(list(show_phases = FALSE, skift_column = NULL))
      }

      # Tjek om Skift kolonne har nogen TRUE værdier
      skift_data <- data[[skift_col]]
      # Handle case where skift_data might be a list or non-logical type
      if (is.list(skift_data)) {
        skift_data <- unlist(skift_data)
      }
      # Convert to logical if needed
      if (!is.logical(skift_data)) {
        skift_data <- as.logical(skift_data)
      }
      has_phase_shifts <- any(skift_data == TRUE, na.rm = TRUE)

      return(list(
        show_phases = has_phase_shifts,
        skift_column = skift_col
      ))
    }),
    frys_config_reactive = shiny::reactive({
      # Bestem frys kolonne for baseline freeze
      data <- app_state$data$current_data
      config <- column_config()

      if (is.null(data) || is.null(config)) {
        return(NULL)
      }

      # Tjek om bruger har valgt en Frys kolonne - brug robust sanitization
      frys_col <- sanitize_selection(input$frys_column)

      # Hvis ingen Frys kolonne valgt eller ikke i data, returner NULL
      if (is.null(frys_col) || !frys_col %in% names(data)) {
        return(NULL)
      }

      return(frys_col)
    }),
    chart_title_reactive = chart_title(input),
    y_axis_unit_reactive = shiny::reactive({
      if (is.null(input$y_axis_unit) || input$y_axis_unit == "") {
        return("count")
      } else {
        return(input$y_axis_unit)
      }
    }),
    kommentar_column_reactive = shiny::reactive({
      return(sanitize_selection(input$kommentar_column))
    }),
    app_state = app_state
  )

  # Plot klar tjek
  output$plot_ready <- shiny::reactive({
    result <- !is.null(visualization$plot_ready()) && visualization$plot_ready()
    return(if (result) "true" else "false")
  })
  outputOptions(output, "plot_ready", suspendWhenHidden = FALSE)

  # Retur nér visualiserings objekt til brug i download handlers
  return(visualization)
}
