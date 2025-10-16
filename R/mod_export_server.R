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

    # Preview reactive - generates plot for live preview
    # Returns current SPC plot from app_state visualization
    preview_plot <- shiny::reactive({
      # Defensive checks - require valid app_state and data
      shiny::req(app_state)
      shiny::req(app_state$data$current_data)

      # Check if visualization plot is ready
      plot_ready <- app_state$visualization$plot_ready %||% FALSE
      shiny::req(plot_ready)

      # Get plot object from visualization state
      plot_obj <- app_state$visualization$plot_object

      # Validate plot object exists
      shiny::req(plot_obj)

      log_debug(
        component = "[EXPORT_MODULE]",
        message = "Preview plot generated successfully"
      )

      return(plot_obj)
    })

    # Preview plot output
    output$preview_plot <- shiny::renderPlot(
      {
        plot <- preview_plot()

        if (is.null(plot)) {
          # Display placeholder when no plot available
          graphics::plot.new()
          graphics::text(
            0.5, 0.5,
            "Ingen plot tilgængeligt\n\nGenerer først et SPC chart i hovedvinduet",
            cex = 1.1,
            col = "#6c757d"
          )
          return(invisible(NULL))
        }

        # Render plot
        safe_operation(
          operation_name = "Render export preview plot",
          code = {
            print(plot)
            invisible(plot)
          },
          fallback = function(e) {
            log_error(
              component = "[EXPORT_MODULE]",
              message = "Failed to render preview plot",
              details = list(error = e$message)
            )
            # Show error message in plot area
            graphics::plot.new()
            graphics::text(
              0.5, 0.5,
              paste("Fejl ved preview:\n", e$message),
              cex = 1.0,
              col = "#dc3545"
            )
            invisible(NULL)
          },
          error_type = "processing"
        )
      },
      res = 96 # Standard screen resolution
    )

    # DOWNLOAD HANDLER ========================================================

    # Download handler - generates export file based on format
    output$download_export <- shiny::downloadHandler(
      filename = function() {
        # Generate filename based on format and title
        format <- input$export_format %||% "pdf"
        title <- input$export_title %||% "SPC_Chart"

        # Sanitize title for filename (replace spaces and special chars)
        safe_title <- gsub("[^a-zA-Z0-9_-]", "_", title)
        safe_title <- gsub("_{2,}", "_", safe_title) # Replace multiple underscores
        safe_title <- substr(safe_title, 1, 50) # Limit length

        # Generate timestamp
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

        # Construct filename
        filename <- paste0(
          EXPORT_FILENAME_PREFIX,
          EXPORT_FILENAME_SEPARATOR,
          safe_title,
          EXPORT_FILENAME_SEPARATOR,
          timestamp,
          ".",
          format
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
            # Get plot from visualization state
            plot <- app_state$visualization$plot_object

            # Validate plot exists
            if (is.null(plot)) {
              stop("Ingen plot tilgængeligt til eksport")
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
              # PNG export - placeholder for actual implementation
              log_debug(
                component = "[EXPORT_MODULE]",
                message = "PNG export - placeholder implementation"
              )
              # Get size preset
              dpi <- as.numeric(input$png_dpi %||% 96)
              # TODO: Parse size preset and apply custom dimensions
              # For now, use medium preset defaults
              grDevices::png(
                filename = file,
                width = EXPORT_SIZE_PRESETS$medium$width,
                height = EXPORT_SIZE_PRESETS$medium$height,
                res = dpi,
                type = EXPORT_PNG_CONFIG$type,
                bg = EXPORT_PNG_CONFIG$bg
              )
              print(plot)
              grDevices::dev.off()
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
        # Preview ready indicator
        preview_ready = shiny::reactive({
          !is.null(preview_plot())
        })
      )
    )
  })
}
