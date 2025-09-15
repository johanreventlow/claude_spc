# server_visualization.R
# Server logik for visualisering og data forberedelse

# Dependencies ----------------------------------------------------------------

# VISUALISERING SETUP =========================================================

## Hovedfunktion for visualisering
# Opsætter al server logik relateret til visualisering og data forberedelse
setup_visualization <- function(input, output, session, values) {
  # Data til visualiserings modul
  active_data <- reactive({
    cat("DEBUG: active_data reactive triggered\n")
    cat("DEBUG: values$current_data is null:", is.null(values$current_data), "\n")
    req(values$current_data)

    data <- values$current_data

    # Tilføj hide_anhoej_rules flag som attribut til data
    attr(data, "hide_anhoej_rules") <- values$hide_anhoej_rules

    # Tjek om dette er den tomme standard tabel fra session reset
    if (nrow(data) == 5 && all(c("Skift", "Dato", "Tæller", "Nævner", "Kommentar") %in% names(data))) {
      if (all(is.na(data$Dato)) && all(is.na(data$Tæller)) && all(is.na(data$Nævner))) {
        # Dette er tom standard tabel men vi skal stadig videregive hide flag
        attr(data, "hide_anhoej_rules") <- values$hide_anhoej_rules
        return(data) # Retur nér data med flag i stedet for NULL
      }
    }

    non_empty_rows <- apply(data, 1, function(row) any(!is.na(row)))

    if (any(non_empty_rows)) {
      filtered_data <- data[non_empty_rows, ]
      attr(filtered_data, "hide_anhoej_rules") <- values$hide_anhoej_rules
      return(filtered_data)
    } else {
      # Selv når ingen meningsfuld data, videregiv flaget
      attr(data, "hide_anhoej_rules") <- values$hide_anhoej_rules
      return(data)
    }
  })

  # Kolonne konfiguration til visualisering
  # Store last valid config to avoid NULL during input updates
  last_valid_config <- reactiveVal(list(x_col = NULL, y_col = NULL, n_col = NULL, chart_type = "run"))

  column_config <- reactive({
    cat("DEBUG: column_config reactive - input values:\n")
    cat("  x_column:", if(!is.null(input$x_column)) input$x_column else "NULL", "\n")
    cat("  y_column:", if(!is.null(input$y_column)) input$y_column else "NULL", "\n")
    cat("  n_column:", if(!is.null(input$n_column)) input$n_column else "NULL", "\n")

    x_col <- if (!is.null(input$x_column) && input$x_column != "") input$x_column else NULL
    y_col <- if (!is.null(input$y_column) && input$y_column != "") input$y_column else NULL
    n_col <- if (!is.null(input$n_column) && input$n_column != "") input$n_column else NULL

    config_result <- list(
      x_col = x_col,
      y_col = y_col,
      n_col = n_col,
      chart_type = get_qic_chart_type(if (is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type)
    )

    # If we have a valid y_col, store this config as the last valid one
    if (!is.null(config_result$y_col)) {
      last_valid_config(config_result)
      return(config_result)
    } else {
      # Return last valid config to avoid breaking the plot during input updates
      return(last_valid_config())
    }
  })


  # Initialiser visualiserings modul
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
