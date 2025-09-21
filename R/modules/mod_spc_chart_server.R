# mod_spc_chart_server.R
# Server logic for SPC chart module
# Extracted from mod_spc_chart.R for better maintainability

# Dependencies ----------------------------------------------------------------
# Helper functions now loaded globally in global.R for better performance


visualizationModuleServer <- function(id, data_reactive, column_config_reactive, chart_type_reactive, target_value_reactive, centerline_value_reactive, skift_config_reactive, frys_config_reactive, chart_title_reactive = NULL, y_axis_unit_reactive = NULL, kommentar_column_reactive = NULL, app_state = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    log_debug("==========================================", "MODULE")
    log_debug("Initializing visualization module server", "MODULE")
    log_debug(paste("Module ID:", id), "MODULE")
    ns <- session$ns
    log_debug("Namespace function created", "MODULE")

    # State Management --------------------------------------------------------
    # Use unified state management if available, fallback to local reactiveValues
    log_debug("Setting up state management", "MODULE")

    # MODULE-INTERNAL DATA REACTIVE ==========================================
    # Create both reactive and cached value for robust access
    log_debug("Creating module-internal data reactive with navigation trigger", "MODULE")

    # Cached data storage
    cached_data <- shiny::reactiveVal(NULL)

    # Create a reactive-safe data function
    get_module_data <- function() {
      log_debug("get_module_data() called - reactive-safe approach", "MODULE_DATA")

      # Use shiny::isolate() to safely access reactive values
      current_data_check <- shiny::isolate(app_state$data$current_data)
      if (is.null(current_data_check)) {
        log_debug("get_module_data: No current data available", "MODULE_DATA")
        return(NULL)
      }

      log_debug("get_module_data: Current data available", "MODULE_DATA")
      data <- current_data_check
      log_debug(paste("get_module_data: Data dimensions:", nrow(data), "x", ncol(data)), "MODULE_DATA")

      # Add hide_anhoej_rules flag as attribute
      hide_anhoej_rules_check <- shiny::isolate(app_state$ui$hide_anhoej_rules)
      attr(data, "hide_anhoej_rules") <- hide_anhoej_rules_check
      log_debug(paste("get_module_data: Hide Anhøj rules flag:", hide_anhoej_rules_check), "MODULE_DATA")

      # Filter non-empty rows
      non_empty_rows <- apply(data, 1, function(row) any(!is.na(row)))
      log_debug(paste("get_module_data: Non-empty rows:", sum(non_empty_rows), "out of", nrow(data)), "MODULE_DATA")

      if (any(non_empty_rows)) {
        filtered_data <- data[non_empty_rows, ]
        attr(filtered_data, "hide_anhoej_rules") <- hide_anhoej_rules_check
        log_debug(paste("✅ get_module_data: Returning filtered data:", nrow(filtered_data), "rows"), "MODULE_DATA")
        return(filtered_data)
      } else {
        log_debug("⚠️ get_module_data: No meaningful data found - returning original with flag", "MODULE_DATA")
        attr(data, "hide_anhoej_rules") <- hide_anhoej_rules_check
        return(data)
      }
    }

    # UNIFIED EVENT SYSTEM: Event-driven data reactive
    module_data_cache <- shiny::reactiveVal(NULL)

    module_data_reactive <- shiny::reactive({
      return(module_data_cache())
    })

    # UNIFIED EVENT SYSTEM: Update data cache when relevant events occur
    shiny::observeEvent(app_state$events$navigation_changed, ignoreInit = TRUE, priority = 1000, {
      log_debug("Module data update triggered by navigation_changed event", "MODULE_DATA")

      # Use the pure function to get data
      result_data <- get_module_data()

      # Update cache
      module_data_cache(result_data)
      cached_data(result_data)
      log_debug("✅ Data cached for renderPlot access via pure function", "MODULE_DATA")
    })

    # UNIFIED EVENT SYSTEM: Also update when data changes
    shiny::observeEvent(app_state$events$data_loaded, ignoreInit = TRUE, priority = 1000, {
      log_debug("Module data update triggered by data_loaded event", "MODULE_DATA")

      # Use the pure function to get data
      result_data <- get_module_data()

      # Update cache
      module_data_cache(result_data)
      cached_data(result_data)
      log_debug("✅ Data cached for data_loaded event", "MODULE_DATA")
    })

    # UNIFIED EVENT SYSTEM: Initialize data at startup if available
    if (!is.null(shiny::isolate(app_state$data$current_data))) {
      log_debug("Initializing module data at startup", "MODULE_DATA")
      initial_data <- get_module_data()
      module_data_cache(initial_data)
      cached_data(initial_data)
      log_debug("✅ Initial data cached", "MODULE_DATA")
    }

    # UNIFIED STATE: Always use app_state for visualization state management
    log_debug("Using unified app_state for visualization state", "MODULE")
    log_debug("Using app_state$visualization for plot state management", .context = "MODULE")
    log_debug("✅ State management initialized", "MODULE")

    # UNIFIED STATE: Helper functions for app_state visualization management
    set_plot_state <- function(key, value) {
      app_state$visualization[[key]] <- value
    }

    get_plot_state <- function(key) {
      return(app_state$visualization[[key]])
    }

    # Plot generation logging (replaced waiter)

    # Konfiguration og Validering ---------------------------------------------

    ## Chart Configuration
    # Reaktiv konfiguration for chart setup
    # Håndterer kolonne-validering og auto-detection
    chart_config <- shiny::reactive({
      # Enhanced shiny::req() guards - stop execution if dependencies not ready
      data <- module_data_reactive()
      shiny::req(data)
      shiny::req(is.data.frame(data))
      shiny::req(nrow(data) > 0)
      shiny::req(ncol(data) > 0)

      config <- column_config_reactive()
      shiny::req(config)
      shiny::req(is.list(config))

      chart_type <- chart_type_reactive() %||% "run"  # Use %||% for cleaner fallback
      shiny::req(chart_type)

      # Valider at kolonner eksisterer i data - hvis ikke, fallback til NULL
      if (!is.null(config$x_col) && !(config$x_col %in% names(data))) {
        config$x_col <- NULL
      }
      if (!is.null(config$y_col) && !(config$y_col %in% names(data))) {
        config$y_col <- NULL
      }
      if (!is.null(config$n_col) && !(config$n_col %in% names(data))) {
        config$n_col <- NULL
      }

      # INGEN AUTO-DETECTION her - dropdown values respekteres altid
      # Auto-detection sker kun ved data upload i server_column_management.R

      # FIXED: Replace blocking shiny::req() with safe validation
      # If y_col is not available, return NULL instead of hanging with shiny::req()
      if (is.null(config$y_col) || !(config$y_col %in% names(data))) {
        log_debug("chart_config: y_col not available, returning NULL to prevent hang", "CHART_CONFIG")
        return(NULL)
      }

      return(list(
        x_col = config$x_col,
        y_col = config$y_col,
        n_col = config$n_col,
        chart_type = chart_type
      ))
    })

    # Plot Generering ---------------------------------------------------------

    # Plot caching infrastructure
    plot_cache <- shiny::reactiveVal(NULL)
    plot_cache_key <- shiny::reactiveVal(NULL)

    ## Hoved SPC Plot Reactive
    # Hovedfunktion for SPC plot generering med caching
    # Håndterer data validering, plot oprettelse og Anhøj rules analyse
    spc_plot <- shiny::reactive({
      # FIXED: Safe chart_config validation without hanging shiny::req()
      config <- chart_config()
      if (is.null(config)) {
        log_debug("spc_plot: chart_config returned NULL, plot not ready", "SPC_PLOT")
        return(NULL)
      }

      data <- module_data_reactive()

      # Generate cache key based on data and config
      current_cache_key <- digest::digest(list(
        data = data,
        config = config,
        target = target_value_reactive(),
        centerline = centerline_value_reactive(),
        skift = skift_config_reactive(),
        frys = frys_config_reactive()
      ))

      # Check if we can use cached plot
      if (!is.null(plot_cache()) && !is.null(plot_cache_key()) &&
          plot_cache_key() == current_cache_key) {
        log_debug("Using cached plot", .context = "PLOT_CACHE")
        return(plot_cache())
      }

      log_debug("Cache miss - generating new plot", .context = "PLOT_CACHE")

      # Clean state management - only set computing when actually needed
      # Unified state assignment using helper functions
      set_plot_state("plot_ready", FALSE)
      set_plot_state("plot_warnings", character(0))
      set_plot_state("anhoej_results", NULL)

      log_debug("Starting plot generation...", "PLOT_GENERATION")

      on.exit(
        {
          log_debug("Plot generation completed", "PLOT_GENERATION")
          # Unified state assignment using helper function
          set_plot_state("is_computing", FALSE)
        },
        add = TRUE
      )

      # Use unified state assignment
      set_plot_state("is_computing", TRUE)

      # Get chart type from config (already validated)
      chart_type <- config$chart_type

      # Valider data
      validation <- validateDataForChart(data, config, chart_type)

      if (!validation$valid) {
        set_plot_state("plot_warnings", validation$warnings)
        set_plot_state("plot_ready", FALSE)
        set_plot_state("anhoej_results", NULL)
        return(NULL)
      }

      set_plot_state("plot_warnings", character(0))

      # Generer plot
      safe_operation(
        "Generate SPC plot",
        code = {
          # Hent fase konfiguration
          skift_config <- skift_config_reactive()

          # Hent freeze konfiguration
          frys_column <- frys_config_reactive()

          # Get axis units with fallbacks
          x_unit <- "observation"
          y_unit <- if (!is.null(y_axis_unit_reactive)) y_axis_unit_reactive() else "count"

          # Get kommentar column
          kommentar_col <- if (!is.null(kommentar_column_reactive)) kommentar_column_reactive() else NULL

          # DEBUG: Log all parameters before generateSPCPlot call
          log_debug(paste("target_value:", deparse(target_value_reactive())), .context = "PLOT_PARAMS")
          log_debug(paste("centerline_value:", deparse(centerline_value_reactive())), .context = "PLOT_PARAMS")
          log_debug(paste("skift_config:", deparse(skift_config)), .context = "PLOT_PARAMS")
          log_debug(paste("frys_column:", deparse(frys_column)), .context = "PLOT_PARAMS")
          log_debug(paste("chart_title_reactive result:", deparse(chart_title_reactive())), .context = "PLOT_PARAMS")
          log_debug(paste("kommentar_col:", deparse(kommentar_col)), .context = "PLOT_PARAMS")

          spc_result <- generateSPCPlot(
            data = data,
            config = config,
            chart_type = chart_type,
            target_value = target_value_reactive(),
            centerline_value = centerline_value_reactive(),
            show_phases = skift_config$show_phases,
            skift_column = skift_config$skift_column,
            frys_column = frys_column,
            chart_title_reactive = chart_title_reactive,
            y_axis_unit = y_unit,
            kommentar_column = kommentar_col
          )

          plot <- applyHospitalTheme(spc_result$plot)
          qic_data <- spc_result$qic_data

          set_plot_state("plot_object", plot)
          set_plot_state("plot_ready", TRUE)

          # Udtræk ALLE qic() resultater for ALLE chart typer - vigtig for konsistent beregning
          if (!is.null(qic_data)) {
            # Udtræk ALLE tilgængelige metrics fra qic() - lad value boxes bestemme visning
            qic_results <- list(
              # Standard SPC beregninger (altid tilgængelig fra qic)
              any_signal = any(qic_data$sigma.signal, na.rm = TRUE),
              out_of_control_count = sum(qic_data$sigma.signal, na.rm = TRUE),

              # Anhøj rules (meningsfulde for alle chart typer)
              runs_signal = if ("runs.signal" %in% names(qic_data)) any(qic_data$runs.signal, na.rm = TRUE) else FALSE,
              crossings_signal = if ("n.crossings" %in% names(qic_data) && "n.crossings.min" %in% names(qic_data)) {
                n_cross <- max(qic_data$n.crossings, na.rm = TRUE)
                n_cross_min <- max(qic_data$n.crossings.min, na.rm = TRUE)
                !is.na(n_cross) && !is.na(n_cross_min) && n_cross < n_cross_min
              } else {
                FALSE
              },
              longest_run = if ("longest.run" %in% names(qic_data)) max(qic_data$longest.run, na.rm = TRUE) else NA_real_,
              longest_run_max = if ("longest.run.max" %in% names(qic_data)) max(qic_data$longest.run.max, na.rm = TRUE) else NA_real_,
              n_crossings = if ("n.crossings" %in% names(qic_data)) max(qic_data$n.crossings, na.rm = TRUE) else NA_real_,
              n_crossings_min = if ("n.crossings.min" %in% names(qic_data)) max(qic_data$n.crossings.min, na.rm = TRUE) else NA_real_,

              # Chart-type specifik besked
              message = if (chart_type == "run") {
                if (any(qic_data$sigma.signal, na.rm = TRUE)) "Særlig årsag detekteret" else "Ingen særlige årsager fundet"
              } else {
                if (any(qic_data$sigma.signal, na.rm = TRUE)) "Punkter uden for kontrol detekteret" else "Alle punkter inden for kontrol"
              }
            )

            set_plot_state("anhoej_results", qic_results)
          } else {
            set_plot_state("anhoej_results", NULL)
          }

          # Cache the plot with current key
          plot_cache(plot)
          plot_cache_key(current_cache_key)
          log_debug("Plot cached with key:", substr(current_cache_key, 1, 8), .context = "PLOT_CACHE")

          return(plot)
        },
        fallback = function(e) {
          set_plot_state("plot_warnings", c("Graf-generering fejlede:", e$message))
          set_plot_state("plot_ready", FALSE)
          set_plot_state("anhoej_results", NULL)

          # Detailed error logging for diagnosis
          log_debug(paste("❌ PLOT ERROR:", e$message), .context = "PLOT_ERROR")
          log_debug(paste("Error class:", class(e)), .context = "PLOT_ERROR")
          if (exists("config")) log_debug(paste("Config at error:", deparse(config)), .context = "PLOT_ERROR")
          if (exists("data")) log_debug(paste("Data dimensions at error:", nrow(data), "x", ncol(data)), .context = "PLOT_ERROR")

          # Invalidate cache on error
          plot_cache(NULL)
          plot_cache_key(NULL)
          log_debug("Cache invalidated due to error", .context = "PLOT_CACHE")

          return(NULL)
        },
        error_type = "processing"
      )
    })

    # UI Output Funktioner ----------------------------------------------------



    ## Faktisk Plot Rendering
    # Separat renderPlot for det faktiske SPC plot
    # PRODUCTION VERSION with fixes applied
    output$spc_plot_actual <- shiny::renderPlot({
      log_debug("Starting SPC plot rendering", "PLOT_RENDER")

      # Use the fixed spc_plot() reactive which handles NULL chart_config gracefully
      plot_result <- spc_plot()

      if (!is.null(plot_result)) {
        log_debug("SPC plot generated successfully", "PLOT_RENDER")
        print(plot_result)
        return(plot_result)
      } else {
        log_debug("SPC plot returned NULL - configuration not ready", "PLOT_RENDER")
        # Return NULL for now - UI will show loading state
        return(NULL)
      }
    }, width = 800, height = 600)

    # Status og Information ---------------------------------------------------

    ## Plot Status
    # Plot klar status - exposed til parent
    output$plot_ready <- shiny::reactive({
      get_plot_state("plot_ready")
    })
    outputOptions(output, "plot_ready", suspendWhenHidden = FALSE)

    ## Plot Information
    # Plot info og advarsler - event-driven med reactive isolation
    output$plot_info <- shiny::renderUI({
      # Use reactive values for event-driven updates
      warnings <- get_plot_state("plot_warnings")
      plot_ready <- get_plot_state("plot_ready")

      if (length(warnings) > 0) {
        shiny::div(
          class = "alert alert-warning",
          shiny::icon("exclamation-triangle"),
          shiny::strong(" Graf-advarsler:"),
          shiny::tags$ul(
            lapply(warnings, function(warn) shiny::tags$li(warn))
          )
        )
      } else if (plot_ready) {
        # Only access external reactives when needed, with isolation
        data <- shiny::isolate(module_data_reactive())
        chart_type <- shiny::isolate(chart_type_reactive()) %||% "ukendt"

        shiny::div(
          class = "alert alert-success",
          style = "font-size: 0.9rem;",
          shiny::icon("check-circle"),
          shiny::strong(" Graf genereret succesfuldt! "),
          sprintf(
            "Chart type: %s | Datapunkter: %d",
            chart_type,
            nrow(data)
          )
        )
      }
    })

    # Value Boxes -------------------------------------------------------------

    ## Data Status Box
    # Data oversigt value box - event-driven med isolation
    output$plot_status_boxes <- shiny::renderUI({
      # Event-driven approach - react to plot_ready changes
      plot_ready <- get_plot_state("plot_ready")

      if (plot_ready) {
        # Only access external reactives when needed, with isolation
        data <- shiny::isolate(module_data_reactive())
        config <- shiny::isolate(chart_config())
        chart_type <- shiny::isolate(chart_type_reactive()) %||% "run"

        data_count <- nrow(data)
        chart_name <- switch(chart_type,
          "run" = "Run Chart",
          "p" = "P-kort",
          "u" = "U-kort",
          "i" = "I-kort",
          "mr" = "MR-kort",
          "Ukendt"
        )

        bslib::value_box(
          title = "Data Overblik",
          value = paste(data_count, "punkter"),
          showcase = shiny::icon("chart-line"),
          theme = if (data_count >= 15) "info" else "warning",
          shiny::p(class = "fs-7 text-muted mb-0", paste(chart_name, if (data_count < 15) "| Få datapunkter" else "| Tilstrækkelig data"))
        )
      } else {
        # Standard tilstand
        bslib::value_box(
          title = "Data Status",
          value = "Ingen data",
          showcase = shiny::icon("database"),
          theme = "secondary",
          shiny::p(class = "fs-7 text-muted mb-0", "Upload eller indtast data")
        )
      }
    })

    ## Data Summary Box
    # Data oversigt value box til fejlkontrol
    output$data_summary_box <- shiny::renderUI({
      data <- module_data_reactive()
      config <- chart_config()

      if (is.null(data) || nrow(data) == 0) {
        return(
          bslib::value_box(
            title = "Data Oversigt",
            value = "Ingen data",
            showcase = spc_out_of_control_icon,
            theme = "light",
            shiny::p(class = "fs-7 text-muted mb-0", "Vent på data load")
          )
        )
      }

      # Generer oversigt info
      total_rows <- nrow(data)
      total_cols <- ncol(data)

      # Check for konfigurerede kolonner
      summary_text <- ""
      if (!is.null(config)) {
        if (!is.null(config$y_col) && config$y_col %in% names(data)) {
          y_data <- data[[config$y_col]]
          valid_y <- sum(!is.na(y_data))
          summary_text <- paste0("Y: ", valid_y, "/", total_rows, " gyldige")

          if (!is.null(config$n_col) && config$n_col %in% names(data)) {
            n_data <- data[[config$n_col]]
            valid_n <- sum(!is.na(n_data))
            zeros_n <- sum(n_data == 0, na.rm = TRUE)
            summary_text <- paste0(summary_text, " | N: ", valid_n, "/", total_rows)
            if (zeros_n > 0) summary_text <- paste0(summary_text, " (", zeros_n, " nuller)")
          }
        } else {
          summary_text <- "Kolonner ikke konfigureret"
        }
      } else {
        summary_text <- "Konfiguration ikke klar"
      }

      bslib::value_box(
        title = "Data Oversigt",
        value = paste0(total_rows, "×", total_cols),
        showcase = spc_out_of_control_icon,
        showcase_layout = "top right",
        theme = "light",
        shiny::p(class = "fs-7 text-muted mb-0", summary_text)
      )
    })

    ## Anhøj Rules Value Boxes
    # Anhøj rules som value boxes - ALTID SYNLIGE
    # Viser serielængde og antal kryds for alle chart typer
    output$anhoej_rules_boxes <- shiny::renderUI({
      log_debug("============================= VALUE_BOXES", "VALUEBOX_RENDER")
      log_debug("anhoej_rules_boxes renderUI triggered", "VALUEBOX_RENDER")

      data <- module_data_reactive()
      log_debug(paste("Data available:", !is.null(data)), "VALUEBOX_RENDER")
      if (!is.null(data)) {
        log_debug(paste("Data dimensions:", nrow(data), "x", ncol(data)), "VALUEBOX_RENDER")
      }

      # Smart indhold baseret på nuværende status - ALTID vis boxes
      config <- chart_config()
      log_debug(paste("Config available:", !is.null(config)), "VALUEBOX_RENDER")
      chart_type <- chart_type_reactive() %||% "run"
      log_debug(paste("Chart type:", chart_type), "VALUEBOX_RENDER")
      anhoej <- get_plot_state("anhoej_results")
      log_debug(paste("Anhoej results available:", !is.null(anhoej)), "VALUEBOX_RENDER")

      # Simplificeret logik - hvis vi har data med meningsfuldt indhold, er vi klar
      has_meaningful_data <- !is.null(data) && nrow(data) > 0 &&
        any(sapply(data, function(x) {
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

      # Bestem nuværende status og passende indhold
      status_info <- if (!has_meaningful_data) {
        list(
          status = "no_data",
          message = "Upload data eller start ny session",
          theme = "secondary"
        )
      } else if (is.null(config) || is.null(config$y_col)) {
        list(
          status = "not_configured",
          message = "Vælg kolonner i indstillinger",
          theme = "warning"
        )
      } else {
        # Check om vi har nok meningsfuldt data
        meaningful_count <- if (!is.null(config$n_col) && config$n_col %in% names(data)) {
          sum(!is.na(data[[config$y_col]]) & !is.na(data[[config$n_col]]) & data[[config$n_col]] > 0)
        } else {
          sum(!is.na(data[[config$y_col]]))
        }

        if (meaningful_count < 10) {
          list(
            status = "insufficient_data",
            message = "Mindst 10 datapunkter nødvendigt for SPC analyse"
          )
        } else if (!(get_plot_state("plot_ready") %||% FALSE)) {
          list(
            status = "processing",
            message = "Behandler data og beregner...",
            theme = "info"
          )
        } else {
          list(
            status = "ready",
            message = "SPC analyse klar",
            theme = "success"
          )
        }
      }

      # Altid returner de tre hoved boxes med passende indhold
      shiny::tagList(
        ### Serielængde Box-----
        bslib::value_box(
          title = "Serielængde",
          style = if (status_info$status == "insufficient_data") {
            "flex: 1;  background-color: white !important; color: #999999;"
          } else {
            "flex: 1;"
          },
          value = if (status_info$status == "ready") {
            if (!is.null(anhoej$longest_run) && !is.na(anhoej$longest_run)) {
              bslib::layout_column_wrap(
                width = 1 / 2,
                shiny::div(anhoej$longest_run_max),
                shiny::div(anhoej$longest_run)
              )
            } else {
              "Beregner..."
            }
          } else {
            shiny::span(
              style = "font-size:1.5em; color: #999999 !important;",
              switch(status_info$status,
                "no_data" = "Ingen data",
                "not_started" = "Afventer start",
                "not_configured" = "Ikke konfigureret",
                "insufficient_data" = "For få data",
                "processing" = "Behandler...",
                "calculating" = "Beregner...",
                "Afventer data"
              )
            )
          },
          showcase = spc_run_chart_icon,
          theme = if (status_info$status == "ready" && chart_type == "run" && !is.null(anhoej$runs_signal) && (anhoej$runs_signal %||% FALSE)) {
            "dark"
          } else if (status_info$status == "ready") {
            "light"
          } else {
            status_info$theme
          },
          shiny::p(
            class = "fs-7 text-muted mb-0",
            if (status_info$status == "ready") {
              if (!is.null(anhoej$longest_run_max) && !is.na(anhoej$longest_run_max)) {
                bslib::layout_column_wrap(
                  width = 1 / 2,
                  shiny::div("Forventet (maksimum)"),
                  shiny::div("Faktisk")
                )
              } else {
                "Anhøj rules analyse - serielængde"
              }
            } else {
              shiny::span(
                style = "color: #999999;",
                status_info$message
              )
            }
          )
        ),

        ### Antal Kryds Box -----
        bslib::value_box(
          title = "Antal kryds",
          style = if (status_info$status == "insufficient_data") {
            "flex: 1;  background-color: white !important; color: #999999;"
          } else {
            "flex: 1;"
          },
          value = if (status_info$status == "ready") {
            if (!is.null(anhoej$n_crossings) && !is.na(anhoej$n_crossings)) {
              bslib::layout_column_wrap(
                width = 1 / 2,
                shiny::div(anhoej$n_crossings_min),
                shiny::div(anhoej$n_crossings)
              )
            } else {
              "Beregner..."
            }
          } else {
            shiny::span(
              style = "font-size:1.5em; color: #999999 !important;",
              switch(status_info$status,
                "no_data" = "Ingen data",
                "not_started" = "Afventer start",
                "not_configured" = "Ikke konfigureret",
                "insufficient_data" = "For få data",
                "processing" = "Behandler...",
                "calculating" = "Beregner...",
                "Afventer data"
              )
            )
          },
          showcase = spc_median_crossings_icon,
          theme = if (status_info$status == "ready" && !is.null(anhoej$crossings_signal) && (anhoej$crossings_signal %||% FALSE)) {
            "dark"
          } else if (status_info$status == "ready") {
            "light"
          } else {
            status_info$theme
          },
          shiny::p(
            class = "fs-7 text-muted mb-0",
            if (status_info$status == "ready") {
              if (!is.null(anhoej$n_crossings_min) && !is.na(anhoej$n_crossings_min)) {
                bslib::layout_column_wrap(
                  width = 1 / 2,
                  shiny::div("Forventet (minimum)"),
                  shiny::div("Faktisk")
                )
              } else {
                "Anhøj rules analyse - median krydsninger"
              }
            } else {
              shiny::span(
                style = "color: #999999;",
                status_info$message
              )
            }
          )
        ),

        ### Kontrolgrænser Box ----
        bslib::value_box(
          title = if (status_info$status == "ready" && chart_type == "run") {
            shiny::div("Uden for kontrolgrænser", style = "color: #999999 !important;")
          } else {
            "Uden for kontrolgrænser"
          },
          style = if (status_info$status == "ready" && chart_type == "run") {
            "flex: 1; background-color: white !important; color: #999999 !important;"
          } else if (status_info$status == "insufficient_data") {
            "flex: 1;  background-color: white !important; color: #999999;"
          } else {
            "flex: 1;"
          },
          value = if (status_info$status == "ready" && !chart_type == "run" && !is.null(anhoej$out_of_control_count)) {
            anhoej$out_of_control_count
          } else if (status_info$status == "ready" && chart_type == "run") {
            shiny::div(
              style = "font-size:1em; color: #999999 !important; padding-bottom: 1em;",
              class = "fs-7 mb-0",
              "Anvendes ikke ved analyse af seriediagrammer (run charts)"
            )
          } else {
            shiny::span(
              style = "font-size:1.5em; color: #999999 !important;",
              switch(status_info$status,
                "no_data" = "Ingen data",
                "not_started" = "Afventer start",
                "not_configured" = "Ikke konfigureret",
                "insufficient_data" = "For få data",
                "processing" = "Behandler...",
                "calculating" = "Beregner...",
                "Afventer data"
              )
            )
          },
          showcase = spc_out_of_control_icon,
          theme = if (status_info$status == "ready" && chart_type == "run") {
            NULL # No theme when we use custom styling
          } else if (status_info$status == "ready" && !is.null(anhoej$out_of_control_count) && (anhoej$out_of_control_count > 0)) {
            "dark"
          } else if (status_info$status == "ready") {
            "light"
          } else {
            status_info$theme
          },
          shiny::p(
            class = if (status_info$status == "ready" && chart_type == "run") "fs-7 mb-0" else "fs-7 text-muted mb-0",
            style = if (status_info$status == "ready" && chart_type == "run") "color: #999999 !important;" else NULL,
            if (status_info$status == "ready") {
              if (chart_type == "run") {
                ""
              } else {
                "Punkter uden for kontrolgrænser"
              }
            } else {
              shiny::span(
                style = "color: #999999;",
                status_info$message
              )
            }
          )
        )
      )
    })

    ## Placeholder Value Boxes
    # Reserveret til fremtidige funktioner

    ### Data Kvalitet Box
    output$data_quality_box <- shiny::renderUI({
      data <- module_data_reactive()
      if (is.null(data) || nrow(data) == 0) {
        return(shiny::div())
      }

      bslib::value_box(
        title = "Data Kvalitet",
        value = "God",
        showcase = shiny::icon("check-circle"),
        theme = "success",
        shiny::p(class = "fs-6 text-muted", "Automatisk kvalitetskontrol")
      )
    })

    ### Rapport Status Box
    output$report_status_box <- shiny::renderUI({
      data <- module_data_reactive()
      if (is.null(data) || nrow(data) == 0) {
        return(shiny::div())
      }

      bslib::value_box(
        title = "Rapport Status",
        value = "Klar",
        showcase = shiny::icon("file-text"),
        theme = "info",
        shiny::p(class = "fs-6 text-muted", "Eksport og deling tilgængelig")
      )
    })

    # Return Values -----------------------------------------------------------
    # Returner reactive values til parent scope
    # Giver adgang til plot objekt, status og Anhøj resultater
    return(
      list(
        plot = shiny::reactive(get_plot_state("plot_object")),
        plot_ready = shiny::reactive(get_plot_state("plot_ready")),
        anhoej_results = shiny::reactive(get_plot_state("anhoej_results")),
        chart_config = chart_config
      )
    )
  })
}
