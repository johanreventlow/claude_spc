# mod_spc_chart_server.R
# Server logic for SPC chart module
# Extracted from mod_spc_chart.R for better maintainability

# Dependencies ----------------------------------------------------------------
# Helper functions now loaded globally in global.R for better performance


visualizationModuleServer <- function(id, data_reactive, column_config_reactive, chart_type_reactive, target_value_reactive, centerline_value_reactive, skift_config_reactive, frys_config_reactive, chart_title_reactive = NULL, y_axis_unit_reactive = NULL, kommentar_column_reactive = NULL, app_state = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    # Module initialization
    ns <- session$ns

    # Helper function: Safe max that handles empty vectors and preserves actual values
    safe_max <- function(x, na.rm = TRUE) {
      if (length(x) == 0) {
        log_debug("safe_max: empty vector", "VISUALIZATION")
        return(NA_real_)
      }
      if (all(is.na(x))) {
        log_debug("safe_max: all NA values", "VISUALIZATION")
        return(NA_real_)
      }
      result <- max(x, na.rm = na.rm)
      if (is.infinite(result)) {
        log_debug(paste("safe_max: infinite result, returning NA. Input:", paste(x, collapse=", ")), "VISUALIZATION")
        return(NA_real_)
      }
      return(result)
    }

    # State Management --------------------------------------------------------
    # Use unified state management if available, fallback to local reactiveValues

    # MODULE-INTERNAL DATA REACTIVE ==========================================
    # Create both reactive and cached value for robust access

    # Cached data storage - use centralized app_state
    # Initialize module cache in app_state if not already present
    # Use isolate() to safely check reactive value outside reactive context
    current_cached_data <- isolate(app_state$visualization$module_cached_data)
    if (is.null(current_cached_data)) {
      app_state$visualization$module_cached_data <- NULL
    }

    # Create a reactive-safe data function
    get_module_data <- function() {

      # Use shiny::isolate() to safely access reactive values
      current_data_check <- shiny::isolate(app_state$data$current_data)
      if (is.null(current_data_check)) {
        return(NULL)
      }

      data <- current_data_check

      # Add hide_anhoej_rules flag as attribute
      hide_anhoej_rules_check <- shiny::isolate(app_state$ui$hide_anhoej_rules)
      attr(data, "hide_anhoej_rules") <- hide_anhoej_rules_check

      # Filter non-empty rows
      non_empty_rows <- apply(data, 1, function(row) any(!is.na(row)))

      if (any(non_empty_rows)) {
        filtered_data <- data[non_empty_rows, ]
        attr(filtered_data, "hide_anhoej_rules") <- hide_anhoej_rules_check
        return(filtered_data)
      } else {
        attr(data, "hide_anhoej_rules") <- hide_anhoej_rules_check
        return(data)
      }
    }

    # UNIFIED EVENT SYSTEM: Event-driven data reactive - use centralized app_state
    # Initialize module data cache in app_state if not already present
    # Use isolate() to safely check reactive value outside reactive context
    current_data_cache <- isolate(app_state$visualization$module_data_cache)
    if (is.null(current_data_cache)) {
      app_state$visualization$module_data_cache <- NULL
    }

    # Debounced data reactive to prevent multiple rapid invalidations
    module_data_reactive <- shiny::debounce(
      shiny::reactive({
        # Directly depend on the events that should trigger data updates
        app_state$events$data_loaded
        app_state$events$data_changed
        app_state$events$navigation_changed

        # Get fresh data every time any of these events fire
        result <- get_module_data()

        data_info <- if (!is.null(result)) {
          paste("rows:", nrow(result), "cols:", ncol(result))
        } else {
          "NULL"
        }
        log_debug(paste("module_data_reactive called (debounced), returning:", data_info), "VISUALIZATION")
        return(result)
      }),
      millis = 300  # Wait 300ms before processing to batch rapid events
    )

    # UNIFIED EVENT SYSTEM: Consolidated event handling following Race Condition Prevention strategy
    # Implements Event Consolidation (CLAUDE.md Section 3.1.1) for functionally related events

    # DISABLED: Create reactive expression that responds to multiple events
    # Now using event-driven module_data_reactive instead
    # consolidated_trigger <- shiny::reactive({
    #   list(
    #     data_loaded = app_state$events$data_loaded,
    #     data_changed = app_state$events$data_changed,
    #     navigation_changed = app_state$events$navigation_changed
    #   )
    # })
    if (FALSE) { # Disable consolidated event handler
    shiny::observeEvent(consolidated_trigger(), ignoreInit = TRUE, priority = OBSERVER_PRIORITIES$DATA_PROCESSING, {

      # Level 3: Guard condition - prevent concurrent operations (Overlap Prevention)
      if (shiny::isolate(app_state$visualization$cache_updating)) {
        log_debug("Skipping visualization cache update - already in progress", "VISUALIZATION")
        return()
      }

      # Level 2: Atomic state update with guard flag (State-Based Atomicity)
      safe_operation(
        operation_name = "Update visualization cache (consolidated)",
        code = {
          # Set guard flag atomically
          app_state$visualization$cache_updating <- TRUE

          on.exit({
            # Clear guard flag on function exit (success or error)
            app_state$visualization$cache_updating <- FALSE
          }, add = TRUE)

          # Use the pure function to get data
          result_data <- get_module_data()

          # Debug: Log data info
          data_info <- if (!is.null(result_data)) {
            paste("rows:", nrow(result_data), "cols:", ncol(result_data))
          } else {
            "NULL"
          }
          log_debug(paste("Cache update - result_data:", data_info), "VISUALIZATION")

          # Atomic cache update - both values updated together
          app_state$visualization$module_data_cache <- result_data
          app_state$visualization$module_cached_data <- result_data

          log_debug("Visualization cache updated successfully (consolidated)", "VISUALIZATION")
        },
        fallback = function(e) {
          log_error(paste("Visualization cache update failed:", e$message), "VISUALIZATION")
          # Guard flag cleared by on.exit() even in error case
        },
        error_type = "processing"
      )
    })
    } # End disable consolidated event handler

    # UNIFIED EVENT SYSTEM: Initialize data at startup if available
    if (!is.null(shiny::isolate(app_state$data$current_data))) {
      initial_data <- get_module_data()
      app_state$visualization$module_data_cache <- initial_data
      app_state$visualization$module_cached_data <- initial_data
    }

    # UNIFIED STATE: Always use app_state for visualization state management

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
    # Use centralized app_state for plot caching
    # Initialize plot cache in app_state if not already present
    # Use isolate() to safely check reactive values outside reactive context
    current_plot_cache <- isolate(app_state$visualization$plot_cache)
    if (is.null(current_plot_cache)) {
      app_state$visualization$plot_cache <- NULL
    }
    current_plot_cache_key <- isolate(app_state$visualization$plot_cache_key)
    if (is.null(current_plot_cache_key)) {
      app_state$visualization$plot_cache_key <- NULL
    }

    ## Hoved SPC Plot Reactive
    # Hovedfunktion for SPC plot generering med caching
    # Håndterer data validering, plot oprettelse og Anhøj rules analyse
    spc_plot <- shiny::reactive({
      # DEBUG: Remove detailed timing after fix verification
      # current_time <- Sys.time()
      # log_debug(paste("spc_plot reactive triggered at", format(current_time, "%H:%M:%S.%OS3")), "VISUALIZATION")

      # FIXED: Safe chart_config validation without hanging shiny::req()
      config <- chart_config()
      if (is.null(config)) {
        log_debug("spc_plot: config is NULL", "VISUALIZATION")
        return(NULL)
      }

      data <- module_data_reactive()
      data_info <- if (!is.null(data)) {
        paste("rows:", nrow(data), "cols:", ncol(data))
      } else {
        "NULL"
      }
      log_debug(paste("spc_plot: data from cache:", data_info), "VISUALIZATION")

      # Generate cache key based on data and config
      # Enhanced cache key to better detect actual changes
      current_cache_key <- digest::digest(list(
        data_hash = if (!is.null(data)) digest::digest(data) else NULL,
        data_nrow = if (!is.null(data)) nrow(data) else 0,
        data_ncol = if (!is.null(data)) ncol(data) else 0,
        config = config,
        target = target_value_reactive(),
        centerline = centerline_value_reactive(),
        skift = skift_config_reactive(),
        frys = frys_config_reactive(),
        chart_title = if (!is.null(chart_title_reactive)) chart_title_reactive() else NULL,
        y_axis_unit = if (!is.null(y_axis_unit_reactive)) y_axis_unit_reactive() else NULL
      ))

      # DEBUG: Reduce cache logging verbosity
      # log_debug(paste("Cache key generated:", substr(current_cache_key, 1, 8), "..."), "VISUALIZATION")

      # Check if we can use cached plot
      cached_key <- app_state$visualization$plot_cache_key
      if (!is.null(app_state$visualization$plot_cache) && !is.null(cached_key) &&
          cached_key == current_cache_key) {
        # DEBUG: Keep minimal cache logging for troubleshooting
        # log_debug(paste("CACHE HIT - using cached plot and results, key:", substr(current_cache_key, 1, 8), "..."), "VISUALIZATION")
        return(app_state$visualization$plot_cache)
      } else {
        # log_debug(paste("CACHE MISS - will generate new plot, key:", substr(current_cache_key, 1, 8),
        #               "vs cached:", if(is.null(cached_key)) "NULL" else substr(cached_key, 1, 8)), "VISUALIZATION")
      }

      # Cache miss - generating new plot
      log_debug("Cache miss - starting new plot generation", "VISUALIZATION")

      # Clean state management - preserve existing results during computation
      # Unified state assignment using helper functions
      set_plot_state("plot_ready", FALSE)
      set_plot_state("plot_warnings", character(0))
      set_plot_state("is_computing", TRUE)
      # FIX: Don't clear anhoej_results until new computation is complete

      # Starting plot generation

      on.exit(
        {
          # Unified state assignment using helper function
          set_plot_state("is_computing", FALSE)
        },
        add = TRUE
      )

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
          # Ensure skift_config is valid
          if (is.null(skift_config)) {
            skift_config <- list(show_phases = FALSE, skift_column = NULL)
          }

          # Hent freeze konfiguration
          frys_column <- frys_config_reactive()

          # Get axis units with fallbacks
          x_unit <- "observation"
          y_unit <- if (!is.null(y_axis_unit_reactive)) y_axis_unit_reactive() else "count"

          # Get kommentar column
          kommentar_col <- if (!is.null(kommentar_column_reactive)) kommentar_column_reactive() else NULL

          # Parameters validated before plot generation

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
                n_cross <- safe_max(qic_data$n.crossings)
                n_cross_min <- safe_max(qic_data$n.crossings.min)
                !is.na(n_cross) && !is.na(n_cross_min) && n_cross < n_cross_min
              } else {
                FALSE
              },
              longest_run = if ("longest.run" %in% names(qic_data)) safe_max(qic_data$longest.run) else NA_real_,
              longest_run_max = if ("longest.run.max" %in% names(qic_data)) safe_max(qic_data$longest.run.max) else NA_real_,
              n_crossings = if ("n.crossings" %in% names(qic_data)) safe_max(qic_data$n.crossings) else NA_real_,
              n_crossings_min = if ("n.crossings.min" %in% names(qic_data)) safe_max(qic_data$n.crossings.min) else NA_real_,

              # Chart-type specifik besked
              message = if (chart_type == "run") {
                if (any(qic_data$sigma.signal, na.rm = TRUE)) "Særlig årsag detekteret" else "Ingen særlige årsager fundet"
              } else {
                if (any(qic_data$sigma.signal, na.rm = TRUE)) "Punkter uden for kontrol detekteret" else "Alle punkter inden for kontrol"
              }
            )

            # Only update anhoej_results if we got valid non-NA values (NA-beskyttelse)
            if (!is.na(qic_results$longest_run) || !is.na(qic_results$n_crossings)) {
              log_debug(paste("Setting anhoej_results with valid values:",
                             "longest_run=", qic_results$longest_run,
                             "n_crossings=", qic_results$n_crossings), "VISUALIZATION")
              set_plot_state("anhoej_results", qic_results)
            } else {
              log_warn(paste("Skipping anhoej_results update - all NA values:",
                             "longest_run=", qic_results$longest_run,
                             "n_crossings=", qic_results$n_crossings,
                             "- preserving existing results"), "VISUALIZATION")
              # Don't overwrite existing valid results with NA values - this prevents "Beregner..." issue
            }
          } else {
            log_info("Setting anhoej_results to NULL - no qic_data available", "VISUALIZATION")
            set_plot_state("anhoej_results", NULL)
          }

          # Cache the plot with current key
          app_state$visualization$plot_cache <- plot
          app_state$visualization$plot_cache_key <- current_cache_key

          return(plot)
        },
        fallback = function(e) {
          set_plot_state("plot_warnings", c("Graf-generering fejlede:", e$message))
          set_plot_state("plot_ready", FALSE)
          set_plot_state("anhoej_results", NULL)

          # Invalidate cache on error
          app_state$visualization$plot_cache <- NULL
          app_state$visualization$plot_cache_key <- NULL

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

      # Use the fixed spc_plot() reactive which handles NULL chart_config gracefully
      plot_result <- spc_plot()

      if (!is.null(plot_result)) {
        print(plot_result)
        return(plot_result)
      } else {
        # Return NULL for now - UI will show loading state
        return(NULL)
      }
    })

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
            lapply(warnings, function(warn) {
              # HTML escape warning content for XSS protection
              shiny::tags$li(htmltools::htmlEscape(warn))
            })
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

      data <- module_data_reactive()

      # Smart indhold baseret på nuværende status - ALTID vis boxes
      config <- chart_config()
      chart_type <- chart_type_reactive() %||% "run"
      anhoej <- get_plot_state("anhoej_results")

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
      current_anhoej <- get_plot_state("anhoej_results")
      current_is_computing <- get_plot_state("is_computing")
      current_plot_ready <- get_plot_state("plot_ready")

      # DEBUG: Keep status determination at debug level
      # log_debug(paste(
      #   "Status determination:",
      #   "has_meaningful_data=", has_meaningful_data,
      #   "config_y_col=", !is.null(config) && !is.null(config$y_col),
      #   "anhoej_exists=", !is.null(current_anhoej),
      #   "is_computing=", current_is_computing %||% FALSE,
      #   "plot_ready=", current_plot_ready %||% FALSE
      # ), "VISUALIZATION")

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
        } else if (get_plot_state("is_computing") %||% FALSE) {
          list(
            status = "calculating",
            message = "Beregner nye værdier...",
            theme = "info"
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
              # DEBUG: Minimal valuebox logging
              # log_debug(paste("longest_run_box: showing values -", "longest_run=", anhoej$longest_run), "VISUALIZATION")
              bslib::layout_column_wrap(
                width = 1 / 2,
                shiny::div(anhoej$longest_run_max),
                shiny::div(anhoej$longest_run)
              )
            } else {
              # Only log if this still happens (should be rare now with NA protection)
              log_warn(paste("longest_run_box showing 'Beregner...' -",
                            "anhoej_exists=", !is.null(anhoej),
                            "longest_run=", if(is.null(anhoej)) "NULL" else anhoej$longest_run), "VISUALIZATION")
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
