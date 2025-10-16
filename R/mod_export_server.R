# mod_export_server.R
# Server logic for export module
# Handles SPC chart export in multiple formats with live preview

# Dependencies ----------------------------------------------------------------
# Helper functions loaded globally in global.R for better performance

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

    # Export plot reactive - generates plot with export metadata applied
    # Debounced to prevent excessive re-rendering when user types metadata
    export_plot <- shiny::reactive({
      # Defensive checks - require valid app_state and data
      shiny::req(app_state)
      shiny::req(app_state$data$current_data)
      shiny::req(app_state$columns$mappings$y_column)

      # Get existing plot from visualization state
      base_plot <- app_state$visualization$plot_object
      shiny::req(base_plot)

      # Read export metadata inputs (triggers reactive dependency)
      title_input <- input$export_title
      dept_input <- input$export_department

      # Construct chart title with export metadata
      title_parts <- c()

      if (!is.null(title_input) && nchar(trimws(title_input)) > 0) {
        title_parts <- c(title_parts, trimws(title_input))
      }

      if (!is.null(dept_input) && nchar(trimws(dept_input)) > 0) {
        title_parts <- c(title_parts, paste0("(", trimws(dept_input), ")"))
      }

      # If no metadata, use existing plot title or empty
      export_title <- if (length(title_parts) > 0) {
        paste(title_parts, collapse = " ")
      } else {
        "" # Empty title if no metadata
      }

      # Clone plot and update title
      safe_operation(
        operation_name = "Generate export preview plot",
        code = {
          # Clone plot to avoid modifying original
          preview_plot <- base_plot

          # Update plot title with export metadata
          preview_plot <- preview_plot + ggplot2::labs(title = export_title)

          log_debug(
            component = "[EXPORT_MODULE]",
            message = "Export preview plot generated with metadata",
            details = list(
              title = export_title,
              has_title = nchar(trimws(title_input %||% "")) > 0,
              has_dept = nchar(trimws(dept_input %||% "")) > 0
            )
          )

          return(preview_plot)
        },
        fallback = function(e) {
          log_error(
            component = "[EXPORT_MODULE]",
            message = "Failed to generate export preview plot",
            details = list(error = e$message)
          )
          return(NULL)
        },
        error_type = "processing"
      )
    }) %>% shiny::debounce(millis = 500) # Debounce metadata changes for performance

    # Export preview renderPlot - displays plot with export metadata
    output$export_preview <- shiny::renderPlot(
      {
        plot <- export_plot()

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

        # Apply hospital theme to preview
        safe_operation(
          operation_name = "Render export preview with hospital theme",
          code = {
            # Apply hospital theme (matches main app styling)
            if (exists("applyHospitalTheme") && is.function(applyHospitalTheme)) {
              themed_plot <- applyHospitalTheme(plot, base_size = 14)
              return(themed_plot)
            } else {
              # Fallback: return plot without theme if function not available
              log_warn(
                component = "[EXPORT_MODULE]",
                message = "applyHospitalTheme function not found, using plot without theme"
              )
              return(plot)
            }
          },
          fallback = function(e) {
            log_error(
              component = "[EXPORT_MODULE]",
              message = "Failed to apply hospital theme to preview plot",
              details = list(error = e$message)
            )
            # Fallback: show error message in ggplot
            ggplot2::ggplot() +
              ggplot2::annotate(
                "text",
                x = 0.5,
                y = 0.5,
                label = paste("Fejl ved preview:\n", e$message),
                size = 5,
                color = "#dc3545"
              ) +
              ggplot2::theme_void()
          },
          error_type = "processing"
        )
      },
      res = 96 # Standard screen resolution for preview
    )

    # Plot availability reactive - for conditional UI
    output$plot_available <- shiny::reactive({
      !is.null(app_state$data$current_data) &&
        !is.null(app_state$columns$mappings$y_column)
    })
    outputOptions(output, "plot_available", suspendWhenHidden = FALSE)

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
        shiny::req(app_state$visualization$plot_object)

        format <- input$export_format %||% "pdf"

        safe_operation(
          operation_name = paste("Export", toupper(format)),
          code = {
            # Get plot with export metadata applied
            plot <- export_plot()

            # Validate plot exists
            if (is.null(plot)) {
              stop("Ingen plot tilgængeligt til eksport")
            }

            # Apply hospital theme for consistent export styling
            if (exists("applyHospitalTheme") && is.function(applyHospitalTheme)) {
              plot <- applyHospitalTheme(plot, base_size = 14)
            }

            # Export logic based on format
            if (format == "pdf") {
              # PDF export - placeholder for actual implementation
              log_debug(
                component = "[EXPORT_MODULE]",
                message = "PDF export - placeholder implementation"
              )
              # TODO: Implement PDF export with metadata
              # For now, save as simple PDF
              grDevices::pdf(
                file = file,
                width = 10,
                height = 7.5,
                paper = EXPORT_PDF_CONFIG$paper
              )
              print(plot)
              grDevices::dev.off()
            } else if (format == "png") {
              # PNG export - full implementation with size presets and DPI
              log_debug(
                component = "[EXPORT_MODULE]",
                message = "PNG export with configurable size/DPI"
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
                height = round(height_inches * dpi)
              )

              # Generate PNG using dedicated export function
              result <- generate_png_export(
                plot_object = plot,
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
              # PowerPoint export - placeholder for actual implementation
              log_debug(
                component = "[EXPORT_MODULE]",
                message = "PowerPoint export - placeholder implementation"
              )
              # TODO: Implement PowerPoint export using officer package
              # For now, save as PNG and notify user
              grDevices::png(
                filename = file,
                width = EXPORT_SIZE_PRESETS$powerpoint$width,
                height = EXPORT_SIZE_PRESETS$powerpoint$height,
                units = "in",
                res = EXPORT_SIZE_PRESETS$powerpoint$dpi,
                type = EXPORT_PNG_CONFIG$type,
                bg = EXPORT_PNG_CONFIG$bg
              )
              print(plot)
              grDevices::dev.off()
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
