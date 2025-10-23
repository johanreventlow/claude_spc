# mod_export_server.R
# Server logic for export module
# Handles SPC chart export in multiple formats with live preview

# Dependencies ----------------------------------------------------------------
# Helper functions loaded globally in global.R for better performance

# INTERNAL HELPERS ============================================================

#' Normalize Mapping Value
#'
#' Konverterer tomme strenge til NULL for at sikre konsistent behandling.
#' Issue #68: Tomme mappings skal normaliseres før brug.
#'
#' @param value Character eller NULL. Mapping værdi fra app_state
#' @return NULL hvis tom/NULL, ellers original værdi
#' @keywords internal
normalize_mapping <- function(value) {
  if (is.null(value) || (is.character(value) && !nzchar(trimws(value)))) {
    NULL
  } else {
    value
  }
}

#' Build Export Plot (Generic Helper)
#'
#' Genererer export plot for given context.
#' Issue #65: Fælles helper for at reducere code duplication.
#' Issue #67: Undebounced, så download handler får fresh plot.
#'
#' @param app_state Reactive values. Global app state
#' @param title_input Character. Export title input
#' @param dept_input Character. Export department input
#' @param plot_context Character. Plot context ("export_preview", "export_pdf")
#' @return Plot object eller NULL ved fejl
#' @keywords internal
build_export_plot <- function(app_state, title_input, dept_input,
                              plot_context = "export_pdf") {
  # Validate required data
  if (is.null(app_state$data$current_data)) {
    log_warn(
      component = "[EXPORT_MODULE]",
      message = "build_export_plot: No data available"
    )
    return(NULL)
  }

  if (is.null(app_state$columns$mappings$x_column) ||
    is.null(app_state$columns$mappings$y_column)) {
    log_warn(
      component = "[EXPORT_MODULE]",
      message = "build_export_plot: Missing required column mappings"
    )
    return(NULL)
  }

  # chart_type can be NULL at startup - use default "run" as fallback
  chart_type <- app_state$columns$mappings$chart_type %||% "run"

  # Normalize all mappings (Issue #68)
  mappings_target_value <- normalize_mapping(
    app_state$columns$mappings$target_value
  )
  mappings_target_text <- normalize_mapping(
    app_state$columns$mappings$target_text
  )
  mappings_centerline_value <- normalize_mapping(
    app_state$columns$mappings$centerline_value
  )
  mappings_skift_column <- normalize_mapping(
    app_state$columns$mappings$skift_column
  )
  mappings_frys_column <- normalize_mapping(
    app_state$columns$mappings$frys_column
  )
  mappings_y_axis_unit <- normalize_mapping(
    app_state$columns$mappings$y_axis_unit
  )
  mappings_kommentar_column <- normalize_mapping(
    app_state$columns$mappings$kommentar_column
  )
  mappings_n_column <- normalize_mapping(
    app_state$columns$mappings$n_column
  )

  # Construct chart title with export metadata
  title_parts <- c()

  if (!is.null(title_input) && nchar(title_input) > 0) {
    # Convert newlines to CommonMark line breaks (backslash + newline)
    title_processed <- gsub("\n", "\\\n", title_input, fixed = TRUE)
    title_parts <- c(title_parts, title_processed)
  }

  if (!is.null(dept_input) && nchar(trimws(dept_input)) > 0) {
    title_parts <- c(title_parts, paste0("(", trimws(dept_input), ")"))
  }

  # If no metadata, use default instructional title
  export_title <- if (length(title_parts) > 0) {
    paste(title_parts, collapse = " ")
  } else {
    # Default title when field is empty - instructs user what to write
    "Skriv en kort og sigende titel eller\n**konkluder hvad grafen viser**"
  }

  # Regenerate plot with context-specific dimensions
  # This ensures correct label placement for the target context
  safe_operation(
    operation_name = paste("Generate", plot_context, "plot"),
    code = {
      # Get dimensions for the specified context
      context_dims <- get_context_dimensions(plot_context)

      # Get chart configuration from app_state
      config <- list(
        x_col = app_state$columns$mappings$x_column,
        y_col = app_state$columns$mappings$y_column,
        n_col = mappings_n_column
      )

      # Regenerate plot with specified export context and dimensions
      spc_result <- generateSPCPlot(
        data = app_state$data$current_data,
        config = config,
        chart_type = chart_type,
        target_value = mappings_target_value,
        target_text = mappings_target_text,
        centerline_value = mappings_centerline_value,
        show_phases = !is.null(mappings_skift_column),
        skift_column = mappings_skift_column,
        frys_column = mappings_frys_column,
        chart_title_reactive = export_title,
        y_axis_unit = mappings_y_axis_unit %||% "count",
        kommentar_column = mappings_kommentar_column,
        base_size = 14,
        viewport_width = context_dims$width_px,
        viewport_height = context_dims$height_px,
        plot_context = plot_context
      )

      export_plot <- spc_result$plot

      log_debug(
        component = "[EXPORT_MODULE]",
        message = sprintf("Export plot generated for context: %s", plot_context),
        details = list(
          title = export_title,
          context = plot_context,
          width = context_dims$width_px,
          height = context_dims$height_px,
          dpi = context_dims$dpi,
          has_title = nchar(trimws(title_input %||% "")) > 0,
          has_dept = nchar(trimws(dept_input %||% "")) > 0
        )
      )

      return(export_plot)
    },
    fallback = function(e) {
      log_error(
        component = "[EXPORT_MODULE]",
        message = sprintf("Failed to generate %s plot", plot_context),
        details = list(error = e$message, context = plot_context)
      )
      return(NULL)
    },
    error_type = "processing"
  )
}

# EXPORT MODULE SERVER ========================================================

#' Export Module Server
#'
#' Server logik for eksport af SPC charts.
#' Håndterer live preview og download af charts i PDF, PNG og PowerPoint formater.
#'
#' @param id Module ID
#' @param app_state Reactive values. Global app state med data, columns og chart config.
#'   Tilgås read-only - ingen modificering af state.
#'
#' @return Liste med reactive values for module status
#' @family export_modules
#' @export
mod_export_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    # Module initialization
    ns <- session$ns

    # Log module initialization
    log_info(
      component = "[EXPORT_MODULE]",
      message = "Export module initialized"
    )

    # PREVIEW GENERATION ======================================================

    # Export plot reactive - regenerates plot with export-specific dimensions
    # Issue #61: Separate plot generation with context "export_preview" (800×450px)
    # Issue #62: Cache isolated from analysis context
    # Debounced to prevent excessive re-rendering when user types metadata
    export_plot <- shiny::reactive({
      # Log BEFORE req() checks to diagnose issues
      log_debug(
        component = "[EXPORT_MODULE]",
        message = "export_plot() reactive called - checking prerequisites",
        details = list(
          has_app_state = !is.null(app_state),
          has_data = !is.null(app_state$data$current_data),
          has_x_col = !is.null(app_state$columns$mappings$x_column),
          has_y_col = !is.null(app_state$columns$mappings$y_column),
          has_chart_type = !is.null(app_state$columns$mappings$chart_type),
          x_col_value = app_state$columns$mappings$x_column %||% "NULL",
          y_col_value = app_state$columns$mappings$y_column %||% "NULL",
          chart_type_value = app_state$columns$mappings$chart_type %||% "NULL"
        )
      )

      # Issue #66: Use idiomatic Shiny req() pattern for cleaner validation
      # chart_type can be NULL at startup - use default "run" as fallback
      chart_type <- app_state$columns$mappings$chart_type %||% "run"

      # Single req() call for all required dependencies
      shiny::req(
        app_state,
        app_state$data$current_data,
        app_state$columns$mappings$x_column,
        app_state$columns$mappings$y_column,
        chart_type,
        nchar(trimws(chart_type)) > 0
      )

      # NOTE: We do NOT require app_state$visualization$plot_object here because:
      # 1. export_plot() regenerates independently (doesn't clone Analyse-side plot)
      # 2. All required data is in app_state$columns$mappings (set by autodetection)
      # 3. Requiring plot_object would block PNG/PPTX preview when user navigates
      #    directly to Export-side before visiting Analyse-side

      log_debug(
        component = "[EXPORT_MODULE]",
        message = "export_plot() reactive - all req() checks passed, generating plot"
      )

      # Read export metadata inputs (triggers reactive dependency)
      # Note: Use %||% to ensure reactive dependency is tracked even if NULL
      title_input <- input$export_title %||% ""
      dept_input <- input$export_department %||% ""

      # Issue #65: Use shared helper to reduce code duplication
      build_export_plot(app_state, title_input, dept_input, "export_preview")
    }) %>% shiny::debounce(millis = 500) # Debounce metadata changes for performance

    # PDF EXPORT PLOT GENERATION ==============================================

    # PDF export reactive - regenerates plot with PDF-specific dimensions
    # This ensures correct label placement for high-res print output
    # (200×120mm @ 300 DPI = ~2362×1417px)
    # Issue #65: Use shared helper to reduce code duplication
    # Issue #67: Helper is undebounced; reactive debounces for preview performance
    pdf_export_plot <- shiny::reactive({
      # Log BEFORE req() checks to diagnose issues
      log_debug(
        component = "[EXPORT_MODULE]",
        message = "pdf_export_plot() reactive called - checking prerequisites"
      )

      # Issue #66: Use idiomatic Shiny req() pattern for cleaner validation
      shiny::req(
        app_state,
        app_state$data$current_data,
        app_state$columns$mappings$x_column,
        app_state$columns$mappings$y_column
      )

      # Read export metadata inputs (triggers reactive dependency)
      title_input <- input$export_title %||% ""
      dept_input <- input$export_department %||% ""

      # Issue #65: Use shared helper with "export_pdf" context
      build_export_plot(app_state, title_input, dept_input, "export_pdf")
    }) %>% shiny::debounce(millis = 1000) # Debounce for preview performance

    # EXPORT PREVIEW RENDERING ================================================

    # Export preview renderPlot - displays plot with export metadata
    # Note: BFHcharts already applies hospital theme automatically via BFHtheme::theme_bfh()
    output$export_preview <- shiny::renderPlot(
      {
        log_debug(
          component = "[EXPORT_MODULE]",
          message = "renderPlot for export_preview starting"
        )

        plot <- export_plot()

        log_debug(
          component = "[EXPORT_MODULE]",
          message = "export_plot() returned",
          details = list(is_null = is.null(plot), has_data = !is.null(plot$data))
        )

        if (is.null(plot)) {
          # Display placeholder using ggplot2 for consistency
          return(
            ggplot2::ggplot() +
              ggplot2::annotate(
                "text",
                x = 0.5,
                y = 0.5,
                label = "Ingen graf tilgængelig.\nGå til hovedsiden for at oprette en SPC-graf.",
                size = 6,
                color = "#858585"
              ) +
              ggplot2::theme_void()
          )
        }

        # BFHcharts already includes hospital theme - just return plot directly
        # Add zero margin for tight display (matches main chart rendering)
        plot + ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "mm"))
      },
      width = 800, # Fixed 16:9 aspect ratio
      height = 450,
      res = 96
    )

    # Plot availability reactive - for conditional UI
    output$plot_available <- shiny::reactive({
      !is.null(app_state$data$current_data) &&
        !is.null(app_state$columns$mappings$y_column)
    })
    outputOptions(output, "plot_available", suspendWhenHidden = FALSE)

    # CRITICAL: Disable suspension for export preview to ensure reactive dependencies
    # are tracked even when user is on Analysis tab. Without this, changes to
    # target_value, centerline, etc. on Analysis-side won't trigger export plot
    # regeneration because the reactive is suspended when output is hidden.
    outputOptions(output, "export_preview", suspendWhenHidden = FALSE)

    # PDF PREVIEW GENERATION ==================================================

    # PDF preview reactive - generates PNG preview of Typst PDF layout
    # Only active when format is "pdf"
    pdf_preview_image <- shiny::reactive({
      # Only generate for PDF format
      format <- input$export_format %||% "pdf"
      if (format != "pdf") {
        return(NULL)
      }

      # Defensive checks - require valid app_state and data
      shiny::req(app_state)
      shiny::req(app_state$data$current_data)
      shiny::req(app_state$columns$mappings$y_column)

      # Get plot regenerated with PDF export context (200×120mm @ 300 DPI)
      # This ensures correct label placement for high-res print output
      base_plot <- pdf_export_plot()
      shiny::req(base_plot)

      # Read export metadata inputs (triggers reactive dependency)
      # Note: title and department are already embedded in pdf_export_plot()
      title_input <- input$export_title
      dept_input <- input$export_department
      analysis_input <- input$pdf_improvement
      data_def_input <- input$pdf_description

      # Build metadata for PDF generation
      metadata <- list(
        hospital = get_hospital_name_for_export(),
        department = dept_input,
        title = title_input,
        analysis = analysis_input,
        data_definition = data_def_input,
        details = generate_details_string(app_state, format = "full"),
        author = Sys.getenv("USER"),
        date = Sys.Date()
      )

      # Extract SPC statistics
      spc_stats <- extract_spc_statistics(app_state)

      # NOTE: No need to update title or apply theme here
      # - pdf_export_plot() already has title and department embedded
      # - BFHcharts already applies theme via BFHtheme::theme_bfh()
      preview_plot <- base_plot

      # Generate PDF preview PNG
      safe_operation(
        operation_name = "Generate PDF preview PNG",
        code = {
          preview_path <- generate_pdf_preview(
            plot_object = preview_plot,
            metadata = metadata,
            spc_statistics = spc_stats,
            dpi = 150
          )

          log_debug(
            component = "[EXPORT_MODULE]",
            message = "PDF preview PNG generated",
            details = list(
              preview_path = preview_path,
              has_preview = !is.null(preview_path)
            )
          )

          return(preview_path)
        },
        fallback = function(e) {
          log_error(
            component = "[EXPORT_MODULE]",
            message = "Failed to generate PDF preview PNG",
            details = list(error = e$message)
          )
          return(NULL)
        },
        error_type = "processing"
      )
    }) %>% shiny::debounce(millis = 1000) # Debounce for performance (PDF generation is slow)

    # PDF preview renderImage - displays PNG preview of Typst PDF layout
    output$pdf_preview <- shiny::renderImage(
      {
        preview_path <- pdf_preview_image()

        if (is.null(preview_path) || !file.exists(preview_path)) {
          # Return placeholder image (1x1 transparent PNG)
          return(list(
            src = "",
            contentType = "image/png",
            width = "100%",
            height = "auto",
            alt = "PDF preview ikke tilgængelig"
          ))
        }

        # Return PNG preview
        return(list(
          src = preview_path,
          contentType = "image/png",
          width = "100%",
          height = "auto",
          alt = "PDF layout preview"
        ))
      },
      deleteFile = FALSE # Don't delete temp file (will be cleaned up by R session)
    )

    # CRITICAL: Disable suspension for PDF preview to ensure pdf_export_plot()
    # reactive tracks dependencies even when user is on Analysis tab
    outputOptions(output, "pdf_preview", suspendWhenHidden = FALSE)

    # PDF format flag - for conditional UI rendering
    output$is_pdf_format <- shiny::reactive({
      format <- input$export_format %||% "pdf"
      format == "pdf"
    })
    outputOptions(output, "is_pdf_format", suspendWhenHidden = FALSE)

    # DOWNLOAD HANDLER ========================================================

    # Download handler - generates export file based on format
    output$download_export <- shiny::downloadHandler(
      filename = function() {
        # Generate filename using utility function
        format <- input$export_format %||% "pdf"
        title <- input$export_title %||% ""
        department <- input$export_department %||% ""

        # Use centralized filename generation
        filename <- generate_export_filename(
          format = format,
          title = title,
          department = department
        )

        log_info(
          component = "[EXPORT_MODULE]",
          message = "Download initiated",
          details = list(
            format = format,
            filename = filename
          )
        )

        return(filename)
      },
      content = function(file) {
        # Defensive checks - validate app_state and data
        shiny::req(app_state)
        shiny::req(app_state$data$current_data)

        format <- input$export_format %||% "pdf"

        safe_operation(
          operation_name = paste("Export", toupper(format)),
          code = {
            # Export logic based on format
            if (format == "pdf") {
              # Issue #65: Use shared helper for consistent plot generation
              # Issue #67: Call undebounced helper directly for download (fresh data)
              # Preview uses debounced reactive for performance
              plot <- build_export_plot(
                app_state = app_state,
                title_input = input$export_title %||% "",
                dept_input = input$export_department %||% "",
                plot_context = "export_pdf"
              )

              # Validate plot exists
              if (is.null(plot)) {
                stop("Ingen plot tilgængeligt til eksport")
              }

              # NOTE: No need to apply theme - pdf_export_plot() already handles it
              # BFHcharts applies theme via BFHtheme::theme_bfh()

              # PDF export via Typst/Quarto
              log_debug(
                component = "[EXPORT_MODULE]",
                message = "PDF export via Typst/Quarto"
              )

              # Check Quarto availability
              if (!quarto_available()) {
                shiny::showNotification(
                  "PDF export kræver Quarto installation. Download fra https://quarto.org",
                  type = "warning",
                  duration = 10
                )
                stop("Quarto CLI ikke tilgængelig. Installér fra https://quarto.org")
              }

              # Udtræk metadata fra UI inputs
              metadata <- list(
                hospital = get_hospital_name_for_export(),
                department = input$export_department,
                title = input$export_title,
                analysis = input$pdf_improvement,
                data_definition = input$pdf_description,
                details = generate_details_string(app_state, format = "full"),
                author = Sys.getenv("USER"),
                date = Sys.Date()
              )

              # Udtræk SPC statistikker fra app_state
              spc_stats <- extract_spc_statistics(app_state)

              # Validate export inputs
              validate_export_inputs(
                format = "pdf",
                title = input$export_title,
                department = input$export_department
              )

              # Generate PDF using Typst export workflow
              result <- export_spc_to_typst_pdf(
                plot_object = plot,
                metadata = metadata,
                spc_statistics = spc_stats,
                output_path = file
              )

              # Verify successful generation
              if (is.null(result) || !file.exists(file)) {
                stop("PDF generation failed - file not created")
              }

              # Success notification
              shiny::showNotification(
                "PDF genereret og downloadet via Typst",
                type = "message",
                duration = 3
              )
            } else if (format == "png") {
              # PNG export - full implementation with size presets and DPI
              # M13: Regenerate plot with export_png context for correct label placement
              log_debug(
                component = "[EXPORT_MODULE]",
                message = "PNG export with configurable size/DPI and context-aware generation"
              )

              # Get DPI from input (default 96)
              dpi <- as.numeric(input$export_dpi %||% 96)

              # Get size preset or custom dimensions
              size_preset <- input$export_size_preset %||% "medium"

              if (size_preset == "custom") {
                # Use custom dimensions from input
                width_px <- as.numeric(input$export_custom_width %||% 1200)
                height_px <- as.numeric(input$export_custom_height %||% 900)

                # Convert pixels to inches for generate_png_export
                width_inches <- width_px / dpi
                height_inches <- height_px / dpi

                log_debug(
                  paste(
                    "Custom PNG dimensions:",
                    sprintf("%dx%d pixels @ %d DPI", width_px, height_px, dpi),
                    sprintf("(%.2f×%.2f inches)", width_inches, height_inches)
                  ),
                  .context = "EXPORT_MODULE"
                )
              } else {
                # Use preset dimensions
                preset <- get_size_from_preset(size_preset)

                # Convert pixels to inches if preset uses pixels
                if (preset$unit == "px") {
                  width_inches <- preset$width / preset$dpi
                  height_inches <- preset$height / preset$dpi
                } else {
                  # Preset already in inches (e.g., powerpoint)
                  width_inches <- preset$width
                  height_inches <- preset$height
                }

                log_debug(
                  paste(
                    "Using size preset:",
                    size_preset,
                    sprintf("(%.2f×%.2f inches @ %d DPI)", width_inches, height_inches, dpi)
                  ),
                  .context = "EXPORT_MODULE"
                )
              }

              # Validate export inputs before generating
              validate_export_inputs(
                format = "png",
                title = input$export_title,
                department = input$export_department,
                width = round(width_inches * dpi),
                height = round(width_inches * dpi)
              )

              # M13: Regenerate plot with export_png context and actual export dimensions
              # This ensures labels are placed correctly for the final export size

              # Validate required mappings exist
              if (is.null(app_state$columns$mappings$chart_type) ||
                length(app_state$columns$mappings$chart_type) == 0) {
                stop("Chart type er ikke defineret. Gå til Analyse-siden og vælg charttype.")
              }

              # Issue #68: Normalize empty mappings to NULL
              config <- list(
                x_col = app_state$columns$mappings$x_column,
                y_col = app_state$columns$mappings$y_column,
                n_col = normalize_mapping(app_state$columns$mappings$n_column)
              )

              # Build export title from inputs
              title_parts <- c()
              if (!is.null(input$export_title) && nchar(input$export_title) > 0) {
                title_processed <- gsub("\n", "\\\n", input$export_title, fixed = TRUE)
                title_parts <- c(title_parts, title_processed)
              }
              if (!is.null(input$export_department) && nchar(trimws(input$export_department)) > 0) {
                title_parts <- c(title_parts, paste0("(", trimws(input$export_department), ")"))
              }
              export_title <- if (length(title_parts) > 0) paste(title_parts, collapse = " ") else NULL

              # Issue #68: Normalize mappings before use
              norm_skift <- normalize_mapping(
                app_state$columns$mappings$skift_column
              )
              norm_frys <- normalize_mapping(
                app_state$columns$mappings$frys_column
              )
              norm_kommentar <- normalize_mapping(
                app_state$columns$mappings$kommentar_column
              )
              norm_target_value <- normalize_mapping(
                app_state$columns$mappings$target_value
              )
              norm_target_text <- normalize_mapping(
                app_state$columns$mappings$target_text
              )
              norm_centerline_value <- normalize_mapping(
                app_state$columns$mappings$centerline_value
              )
              norm_y_axis_unit <- normalize_mapping(
                app_state$columns$mappings$y_axis_unit
              )

              # Issue #64: Pass override_dpi to ensure correct dimension conversion
              png_plot_result <- generateSPCPlot(
                data = app_state$data$current_data,
                config = config,
                chart_type = app_state$columns$mappings$chart_type,
                target_value = norm_target_value,
                target_text = norm_target_text,
                centerline_value = norm_centerline_value,
                show_phases = !is.null(norm_skift),
                skift_column = norm_skift,
                frys_column = norm_frys,
                chart_title_reactive = export_title,
                y_axis_unit = norm_y_axis_unit %||% "count",
                kommentar_column = norm_kommentar,
                base_size = 14,
                viewport_width = round(width_inches * dpi), # PNG export width in pixels
                viewport_height = round(height_inches * dpi), # PNG export height in pixels
                plot_context = "export_png", # M13: PNG export context
                override_dpi = dpi # Issue #64: User-selected DPI
              )

              # Generate PNG using dedicated export function
              result <- generate_png_export(
                plot_object = png_plot_result$plot,
                width_inches = width_inches,
                height_inches = height_inches,
                dpi = dpi,
                output_path = file
              )

              # Verify successful generation
              if (is.null(result) || !file.exists(file)) {
                stop("PNG generation failed - file not created")
              }

              # Success notification
              shiny::showNotification(
                "PNG genereret og downloadet",
                type = "message",
                duration = 3
              )
            } else if (format == "pptx") {
              # PowerPoint export - full implementation using officer
              # M13: Regenerate plot with export_pptx context for correct label placement
              log_debug(
                component = "[EXPORT_MODULE]",
                message = "PowerPoint export with officer package and context-aware generation"
              )

              # Validate export inputs before generating
              validate_export_inputs(
                format = "pptx",
                title = input$export_title,
                department = input$export_department
              )

              # Check if custom template exists
              template_path <- system.file(
                "templates",
                "hospital_presentation.pptx",
                package = "SPCify"
              )

              # If template doesn't exist, use NULL (creates default)
              if (!file.exists(template_path) || nchar(template_path) == 0) {
                log_debug(
                  "No hospital template found, using default PowerPoint template",
                  .context = "EXPORT_MODULE"
                )
                template_path <- NULL
              } else {
                log_debug(
                  paste("Using hospital template:", template_path),
                  .context = "EXPORT_MODULE"
                )
              }

              # Build title with department if provided
              export_title <- input$export_title %||% ""
              if (!is.null(input$export_department) && nchar(input$export_department) > 0) {
                export_title <- paste0(
                  export_title,
                  " (",
                  input$export_department,
                  ")"
                )
              }

              # M13: Regenerate plot with export_pptx context (9×6.5 inches @ 96 DPI)

              # Validate required mappings exist
              if (is.null(app_state$columns$mappings$chart_type) ||
                length(app_state$columns$mappings$chart_type) == 0) {
                stop("Chart type er ikke defineret. Gå til Analyse-siden og vælg charttype.")
              }

              pptx_dims <- get_context_dimensions("export_pptx")

              # Issue #68: Normalize empty mappings to NULL
              config <- list(
                x_col = app_state$columns$mappings$x_column,
                y_col = app_state$columns$mappings$y_column,
                n_col = normalize_mapping(app_state$columns$mappings$n_column)
              )

              # Issue #68: Normalize mappings before use
              norm_skift <- normalize_mapping(
                app_state$columns$mappings$skift_column
              )
              norm_frys <- normalize_mapping(
                app_state$columns$mappings$frys_column
              )
              norm_kommentar <- normalize_mapping(
                app_state$columns$mappings$kommentar_column
              )
              norm_target_value <- normalize_mapping(
                app_state$columns$mappings$target_value
              )
              norm_target_text <- normalize_mapping(
                app_state$columns$mappings$target_text
              )
              norm_centerline_value <- normalize_mapping(
                app_state$columns$mappings$centerline_value
              )
              norm_y_axis_unit <- normalize_mapping(
                app_state$columns$mappings$y_axis_unit
              )

              pptx_plot_result <- generateSPCPlot(
                data = app_state$data$current_data,
                config = config,
                chart_type = app_state$columns$mappings$chart_type,
                target_value = norm_target_value,
                target_text = norm_target_text,
                centerline_value = norm_centerline_value,
                show_phases = !is.null(norm_skift),
                skift_column = norm_skift,
                frys_column = norm_frys,
                chart_title_reactive = export_title,
                y_axis_unit = norm_y_axis_unit %||% "count",
                kommentar_column = norm_kommentar,
                base_size = 14,
                viewport_width = pptx_dims$width_px, # PowerPoint dimensions
                viewport_height = pptx_dims$height_px,
                plot_context = "export_pptx" # M13: PowerPoint export context
              )

              # Generate PowerPoint using dedicated export function
              result <- generate_powerpoint_export(
                plot_object = pptx_plot_result$plot,
                title = export_title,
                template_path = template_path,
                output_path = file
              )

              # Verify successful generation
              if (is.null(result) || !file.exists(file)) {
                stop("PowerPoint generation failed - file not created")
              }

              # Success notification
              shiny::showNotification(
                "PowerPoint genereret og downloadet",
                type = "message",
                duration = 3
              )
            } else {
              stop(paste("Ukendt format:", format))
            }

            log_info(
              component = "[EXPORT_MODULE]",
              message = "Export completed successfully",
              details = list(format = format, file = basename(file))
            )
          },
          fallback = function(e) {
            log_error(
              component = "[EXPORT_MODULE]",
              message = "Export failed",
              details = list(
                format = format,
                error = e$message
              )
            )
            # Show error notification to user
            shiny::showNotification(
              paste("Eksport fejlede:", e$message),
              type = "error",
              duration = 10
            )
          },
          error_type = "processing"
        )
      }
    )

    # Return values -----------------------------------------------------------
    # Return module status for parent scope
    return(
      list(
        # Export preview ready indicator
        preview_ready = shiny::reactive({
          !is.null(export_plot())
        })
      )
    )
  })
}
