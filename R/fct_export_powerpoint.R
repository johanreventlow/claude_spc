# ==============================================================================
# FCT_EXPORT_POWERPOINT.R
# ==============================================================================
# FORMÅL: PowerPoint export funktionalitet med template-baseret slide generation.
#         Indsætter PNG chart og titel i PowerPoint præsentation.
#
# FUNKTIONER:
#   - generate_powerpoint_export() - PowerPoint generation wrapper
#   - detect_pptx_placeholders() - Placeholder detection helper
#
# ANVENDES AF:
#   - Export module server (mod_export_server.R)
#   - Download handlers for PowerPoint format
#
# TEKNISKE DETALJER:
#   - Uses officer package for PowerPoint manipulation
#   - Loads user template from inst/templates/ eller creates default
#   - Generates PNG at PowerPoint-optimal size (10×7.5 inches @ 96 DPI)
#   - Inserts title into slide 1 title placeholder
#   - Inserts chart PNG into slide 1 body area
#   - Fallback positioning if standard placeholders not found
#   - Temporary PNG cleanup via on.exit()
#   - Robust error handling via safe_operation()
#
# DEPENDENCIES:
#   - officer for PowerPoint manipulation
#   - generate_png_export() for chart rendering
#   - safe_operation() from utils_error_handling.R
#   - Logging functions from utils_logging.R
# ==============================================================================

#' Generate PowerPoint Export
#'
#' Genererer PowerPoint export af SPC chart ved at indsætte PNG chart og titel
#' i en PowerPoint template. Bruger officer package til manipulation af .pptx filer.
#'
#' @param plot_object ggplot object (fra generateSPCPlot eller applyHospitalTheme)
#' @param title Character titel til slide (optional, default "")
#' @param template_path Path til PowerPoint template (optional, default NULL)
#' @param output_path Path til output fil (optional, NULL = create tempfile)
#'
#' @return Character path til genereret PowerPoint fil, eller NULL ved fejl
#'
#' @details
#' PowerPoint generation workflow:
#' 1. Validate plot object (must be ggplot)
#' 2. Validate template exists (eller use default)
#' 3. Generate temporary PNG at PowerPoint-optimal size (10×7.5 inches @ 96 DPI)
#' 4. Load PowerPoint template med officer::read_pptx()
#' 5. Detect title og body placeholders på slide 1
#' 6. Insert title med officer::ph_with()
#' 7. Insert PNG chart med officer::external_img()
#' 8. Save PowerPoint med print()
#' 9. Cleanup temporary PNG via on.exit()
#'
#' Template handling:
#' - Hvis template_path specificeret: Load fra path
#' - Hvis template_path NULL: Create default blank presentation
#' - Hvis template ikke findes: Return NULL med log error
#'
#' Placeholder detection:
#' - Detect standard title og body placeholders via officer::ph_location_type()
#' - Fallback til default positioning hvis placeholders ikke findes
#'
#' Chart dimensions:
#' - PowerPoint-optimal: 10 inches × 7.5 inches @ 96 DPI
#' - Resulterer i 960×720 pixel PNG for embedding
#'
#' Error handling:
#' - NULL plot → return NULL med log error
#' - Invalid plot (not ggplot) → return NULL med log error
#' - Missing template → return NULL med log error
#' - PNG generation failure → return NULL med log error
#' - PowerPoint generation failure → return NULL med log error
#' - All errors logged via log_error()
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point()
#' generate_powerpoint_export(
#'   plot_object = plot,
#'   title = "Min SPC Graf",
#'   output_path = "output.pptx"
#' )
#'
#' # With custom template
#' generate_powerpoint_export(
#'   plot_object = plot,
#'   title = "SPC Oversigt - Kardiologi",
#'   template_path = "inst/templates/hospital_template.pptx",
#'   output_path = "export.pptx"
#' )
#'
#' # Auto tempfile (for download handlers)
#' pptx_path <- generate_powerpoint_export(plot, "Titel")
#' }
#'
#' @export
generate_powerpoint_export <- function(plot_object,
                                       title = "",
                                       template_path = NULL,
                                       output_path = NULL) {
  safe_operation(
    operation_name = "PowerPoint export generation",
    code = {
      # Validate plot object
      if (is.null(plot_object)) {
        stop("Plot object cannot be NULL", call. = FALSE)
      }

      if (!inherits(plot_object, "ggplot")) {
        stop("Invalid plot object - must be a ggplot object", call. = FALSE)
      }

      # Handle template_path
      if (is.null(template_path)) {
        # Create default blank presentation
        log_debug(
          "No template provided, creating default presentation",
          .context = "EXPORT_PPTX"
        )
        pptx_doc <- officer::read_pptx()
        pptx_doc <- officer::add_slide(pptx_doc, layout = "Title and Content")
      } else {
        # Validate template exists
        if (!file.exists(template_path)) {
          stop(
            paste("PowerPoint template not found:", template_path),
            call. = FALSE
          )
        }

        log_debug(
          paste("Loading PowerPoint template:", template_path),
          .context = "EXPORT_PPTX"
        )
        pptx_doc <- officer::read_pptx(template_path)
      }

      # Use slide 2 (content slide) instead of slide 1 (title slide)
      # Slide 1 in hospital templates is typically a title slide without body placeholder
      # Slide 2 has the actual content layout

      # Check if slide 2 exists, if not add a slide
      if (length(pptx_doc) < 2) {
        log_debug(
          "Template has only 1 slide, adding content slide",
          .context = "EXPORT_PPTX"
        )
        # Add a slide with "Title and Content" layout (or first available layout)
        available_layouts <- officer::layout_summary(pptx_doc)
        layout_to_use <- available_layouts$layout[min(2, nrow(available_layouts))]
        pptx_doc <- officer::add_slide(pptx_doc, layout = layout_to_use)
      }

      slide_index <- 2

      log_debug(
        paste("Using slide", slide_index, "for chart insertion"),
        .context = "EXPORT_PPTX"
      )

      # Use conservative chart dimensions that work well in most templates
      # PowerPoint standard content area is typically around 9×6.5 inches
      chart_width <- 9
      chart_height <- 6.5

      # Generate PNG chart at template-appropriate size
      temp_png <- tempfile(fileext = ".png")
      on.exit(unlink(temp_png), add = TRUE)

      log_debug(
        sprintf(
          "Generating PNG chart for PowerPoint embedding (%.1f×%.1f inches @ 96 DPI)",
          chart_width, chart_height
        ),
        .context = "EXPORT_PPTX"
      )

      png_result <- generate_png_export(
        plot_object = plot_object,
        width_inches = chart_width,
        height_inches = chart_height,
        dpi = 96,
        output_path = temp_png
      )

      # Verify PNG was created successfully
      if (is.null(png_result) || !file.exists(temp_png)) {
        stop("PNG generation failed - cannot embed chart in PowerPoint", call. = FALSE)
      }

      # Detect placeholders
      log_debug(
        "Detecting PowerPoint placeholders",
        .context = "EXPORT_PPTX"
      )
      placeholders <- detect_pptx_placeholders(pptx_doc)

      # Navigate to slide 2 for content insertion (slide 1 is title slide)
      pptx_doc <- officer::on_slide(pptx_doc, index = slide_index)

      # Add title to slide 2
      log_debug(
        paste("Inserting title on slide", slide_index, ":", title),
        .context = "EXPORT_PPTX"
      )

      title_inserted <- tryCatch(
        {
          pptx_doc <- officer::ph_with(
            pptx_doc,
            value = title,
            location = officer::ph_location_type(type = "title")
          )
          log_info(
            paste("Title inserted successfully on slide", slide_index),
            .context = "EXPORT_PPTX"
          )
          TRUE
        },
        error = function(e) {
          log_warn(
            paste("Could not insert title:", e$message),
            .context = "EXPORT_PPTX"
          )
          FALSE
        }
      )

      # Insert PNG chart on slide 2
      log_debug(
        paste("Inserting chart image on slide", slide_index, "- PNG path:", temp_png),
        .context = "EXPORT_PPTX"
      )

      chart_inserted <- FALSE
      tryCatch(
        {
          pptx_doc <- officer::ph_with(
            pptx_doc,
            value = officer::external_img(temp_png),
            location = officer::ph_location_type(type = "body")
          )
          log_info(
            "Chart image inserted successfully using 'body' placeholder",
            .context = "EXPORT_PPTX"
          )
          chart_inserted <- TRUE
        },
        error = function(e) {
          # If body location fails, try pic location (more compatible)
          log_warn(
            paste("Body placeholder failed:", e$message, "- trying 'pic' placeholder"),
            .context = "EXPORT_PPTX"
          )
          tryCatch(
            {
              pptx_doc <- officer::ph_with(
                pptx_doc,
                value = officer::external_img(temp_png),
                location = officer::ph_location_type(type = "pic")
              )
              log_info(
                "Chart image inserted successfully using 'pic' placeholder",
                .context = "EXPORT_PPTX"
              )
              chart_inserted <<- TRUE
            },
            error = function(e2) {
              log_error(
                paste("Chart insertion failed on both 'body' and 'pic' placeholders:", e2$message),
                .context = "EXPORT_PPTX",
                details = list(
                  png_path = temp_png,
                  png_exists = file.exists(temp_png),
                  slide_index = slide_index,
                  error_body = e$message,
                  error_pic = e2$message
                )
              )
              stop("Failed to insert chart into PowerPoint", call. = FALSE)
            }
          )
        }
      )

      if (!chart_inserted) {
        log_error(
          "Chart insertion did not complete successfully",
          .context = "EXPORT_PPTX"
        )
      }

      # Save PowerPoint
      if (is.null(output_path)) {
        output_path <- tempfile(fileext = ".pptx")
      }

      log_debug(
        paste("Saving PowerPoint to:", output_path),
        .context = "EXPORT_PPTX"
      )

      print(pptx_doc, target = output_path)

      # Verify file was created
      if (!file.exists(output_path)) {
        stop("PowerPoint file was not created successfully", call. = FALSE)
      }

      log_info(
        paste(
          "PowerPoint exported successfully:",
          basename(output_path)
        ),
        .context = "EXPORT_PPTX"
      )

      return(output_path)
    },
    fallback = function(e) {
      log_error(
        paste("PowerPoint export fejlede:", e$message),
        .context = "EXPORT_PPTX",
        details = list(
          title = title,
          template_path = template_path %||% "default",
          output_path = output_path %||% "tempfile",
          error_class = class(e)[1]
        )
      )
      return(NULL)
    },
    error_type = "processing"
  )
}

#' Detect PowerPoint Placeholders
#'
#' Detekterer title og body placeholders på aktiv slide i PowerPoint dokument.
#' Returnerer officer location objekter til brug med ph_with().
#'
#' @param pptx_doc officer rpptx object fra read_pptx()
#'
#' @return Named list med title_location og body_location
#'   - title_location: officer::ph_location object eller NULL
#'   - body_location: officer::ph_location object eller NULL
#'
#' @details
#' Placeholder detection strategy:
#' 1. Get layout summary via officer::layout_summary()
#' 2. Try standard title placeholder (type = "title")
#' 3. Try standard body placeholder (type = "body")
#' 4. Return NULL for missing placeholders (fallback positioning)
#'
#' Note: Actual placeholder detection depends on template structure.
#' Different templates may have different layout types.
#'
#' @examples
#' \dontrun{
#' pptx_doc <- officer::read_pptx("template.pptx")
#' placeholders <- detect_pptx_placeholders(pptx_doc)
#'
#' if (!is.null(placeholders$title_location)) {
#'   pptx_doc <- officer::ph_with(pptx_doc, "Titel", placeholders$title_location)
#' }
#' }
#'
#' @export
detect_pptx_placeholders <- function(pptx_doc) {
  # Initialize result
  result <- list(
    title_location = NULL,
    body_location = NULL
  )

  # Try to detect standard placeholders
  # Note: Dette er en simplified implementation
  # Mere avanceret detection kunne inspicere layout_summary()
  # og finde specifikke placeholder typer

  # Title placeholder
  result$title_location <- tryCatch(
    {
      officer::ph_location_type(type = "title")
    },
    error = function(e) {
      log_debug(
        "Could not detect title placeholder",
        .context = "EXPORT_PPTX"
      )
      NULL
    }
  )

  # Body placeholder
  result$body_location <- tryCatch(
    {
      officer::ph_location_type(type = "body")
    },
    error = function(e) {
      log_debug(
        "Could not detect body placeholder",
        .context = "EXPORT_PPTX"
      )
      NULL
    }
  )

  return(result)
}

# HELPER: NULL coalescing operator ============================================

#' NULL Coalescing Operator
#'
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
