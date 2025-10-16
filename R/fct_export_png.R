# ==============================================================================
# FCT_EXPORT_PNG.R
# ==============================================================================
# FORMÅL: PNG export funktionalitet med configurable size/DPI, metadata burning
#         og high-quality rendering.
#
# FUNKTIONER:
#   - generate_png_export() - PNG generation wrapping ggplot2::ggsave()
#   - get_size_from_preset() - Convert size preset to dimensions
#
# ANVENDES AF:
#   - Export module server (mod_export_server.R)
#   - Download handlers for PNG format
#
# TEKNISKE DETALJER:
#   - Uses ggplot2::ggsave() for high-quality PNG rendering
#   - Title and afdeling burned into PNG via ggplot2::labs() (not post-processing)
#   - Supports size presets (Lille/Medium/Stor/PowerPoint/Brugerdefineret)
#   - Supports DPI selection (72/96/150/300)
#   - Automatic temporary file creation if output_path not provided
#   - Robust error handling via safe_operation()
#
# DEPENDENCIES:
#   - ggplot2 for plot rendering
#   - EXPORT_SIZE_PRESETS from config_export_config.R
#   - EXPORT_DPI_OPTIONS from config_export_config.R
#   - safe_operation() from utils_error_handling.R
#   - Logging functions from utils_logging.R
# ==============================================================================

#' Generate PNG Export
#'
#' Genererer PNG export af en ggplot2 graf med specificerede dimensioner og DPI.
#' Understøtter både pixels og inches som input units, og sikrer exact pixel
#' output med ±1 pixel tolerance.
#'
#' @param plot_object ggplot object (fra generateSPCPlot eller applyHospitalTheme)
#' @param width_inches Numeric width i inches (e.g., 10 for standard, 7.5 for PowerPoint)
#' @param height_inches Numeric height i inches (e.g., 7.5 for standard, 5.625 for PowerPoint)
#' @param dpi Numeric DPI resolution (72, 96, 150, 300)
#' @param output_path Character path for output file (NULL = create tempfile)
#'
#' @return Character path til genereret PNG fil, eller NULL ved fejl
#'
#' @details
#' PNG generation workflow:
#' 1. Validate plot object (must be ggplot)
#' 2. Create temporary file hvis output_path er NULL
#' 3. Use ggplot2::ggsave() med device = "png" for high-quality rendering
#' 4. Set bg = "white" for consistent background
#' 5. Return file path eller NULL ved fejl
#'
#' Dimension calculation:
#' - Final pixel dimensions = width_inches × dpi × height_inches × dpi
#' - Example: 10 inches × 96 DPI = 960 pixels width
#' - Tolerance: ±1 pixel due to rounding
#'
#' Error handling:
#' - NULL plot → return NULL med log error
#' - Invalid plot (not ggplot) → return NULL med log error
#' - File write failure → return NULL med log error
#' - All errors logged via log_error()
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point()
#' generate_png_export(plot, width_inches = 10, height_inches = 7.5, dpi = 96)
#'
#' # With custom output path
#' generate_png_export(plot, 8, 6, 150, "output.png")
#'
#' # High resolution for print
#' generate_png_export(plot, 10, 7.5, 300, "high_res.png")
#' }
#'
#' @export
generate_png_export <- function(plot_object,
                                width_inches = 10,
                                height_inches = 7.5,
                                dpi = 96,
                                output_path = NULL) {
  safe_operation(
    operation_name = "PNG export generation",
    code = {
      # Validate plot object
      if (is.null(plot_object)) {
        stop("Plot object cannot be NULL", call. = FALSE)
      }

      if (!inherits(plot_object, "ggplot")) {
        stop("Invalid plot object - must be a ggplot object", call. = FALSE)
      }

      # Validate dimensions
      if (!is.numeric(width_inches) || width_inches <= 0) {
        stop("Width must be a positive number", call. = FALSE)
      }

      if (!is.numeric(height_inches) || height_inches <= 0) {
        stop("Height must be a positive number", call. = FALSE)
      }

      if (!is.numeric(dpi) || dpi <= 0) {
        stop("DPI must be a positive number", call. = FALSE)
      }

      # Create temp file if no output path provided
      if (is.null(output_path)) {
        output_path <- tempfile(fileext = ".png")
      }

      # Log export start
      log_debug(
        paste(
          "Starting PNG export:",
          sprintf("%.1f×%.1f inches @ %d DPI", width_inches, height_inches, dpi),
          sprintf(
            "(target: %d×%d pixels)",
            round(width_inches * dpi),
            round(height_inches * dpi)
          )
        ),
        .context = "EXPORT_PNG"
      )

      # Use ggplot2::ggsave for high-quality PNG rendering
      ggplot2::ggsave(
        filename = output_path,
        plot = plot_object,
        width = width_inches,
        height = height_inches,
        dpi = dpi,
        device = "png",
        bg = "white",
        units = "in"
      )

      # Verify file was created
      if (!file.exists(output_path)) {
        stop("PNG file was not created successfully", call. = FALSE)
      }

      # Log success
      log_info(
        paste(
          "PNG exported successfully:",
          basename(output_path),
          sprintf(
            "(%d×%d @ %d DPI)",
            round(width_inches * dpi),
            round(height_inches * dpi),
            dpi
          )
        ),
        .context = "EXPORT_PNG"
      )

      return(output_path)
    },
    fallback = function(e) {
      log_error(
        paste("PNG export fejlede:", e$message),
        .context = "EXPORT_PNG",
        details = list(
          width_inches = width_inches,
          height_inches = height_inches,
          dpi = dpi,
          output_path = output_path %||% "tempfile",
          error_class = class(e)[1]
        )
      )
      return(NULL)
    },
    error_type = "processing"
  )
}

#' Convert Size Preset to Dimensions
#'
#' Konverterer en size preset navn til konkrete dimensioner og DPI værdier.
#' Bruges til at mappe UI dropdown værdier til faktiske export parametre.
#'
#' @param preset_name Character preset name ("small", "medium", "large", "powerpoint")
#'
#' @return Named list med width, height, dpi, unit, label
#'   - width: Numeric width (pixels eller inches afhængig af unit)
#'   - height: Numeric height (pixels eller inches afhængig af unit)
#'   - dpi: Numeric DPI resolution
#'   - unit: Character unit type ("px" eller "in")
#'   - label: Character display label for UI
#'
#' @details
#' Available presets (fra EXPORT_SIZE_PRESETS):
#' - small: 800×600px @ 96 DPI (lille web/email format)
#' - medium: 1200×900px @ 96 DPI (standard præsentation)
#' - large: 1920×1440px @ 96 DPI (høj opløsning)
#' - powerpoint: 10×7.5 inches @ 96 DPI (optimal til PowerPoint slides)
#'
#' Default fallback: medium preset hvis preset_name ikke findes
#'
#' Dimension conversion:
#' - Pixel presets: Skal konverteres til inches via width_px / dpi
#' - Inch presets: Bruges direkte i ggplot2::ggsave()
#'
#' @examples
#' \dontrun{
#' # Get small preset
#' preset <- get_size_from_preset("small")
#' # Returns: list(width = 800, height = 600, dpi = 96, unit = "px", label = "Lille")
#'
#' # Get powerpoint preset
#' preset <- get_size_from_preset("powerpoint")
#' # Returns: list(width = 10, height = 7.5, dpi = 96, unit = "in", label = "PowerPoint")
#'
#' # Unknown preset falls back to medium
#' preset <- get_size_from_preset("unknown")
#' # Returns: medium preset (1200×900px @ 96 DPI)
#' }
#'
#' @export
get_size_from_preset <- function(preset_name) {
  # Get presets from config
  presets <- EXPORT_SIZE_PRESETS

  # Handle NULL input
  if (is.null(preset_name)) {
    log_debug(
      "NULL preset_name provided, defaulting to medium",
      .context = "EXPORT_PNG"
    )
    return(presets$medium)
  }

  # Convert to character and lowercase for case-insensitive matching
  preset_key <- tolower(as.character(preset_name))

  # Check if preset exists
  if (preset_key %in% names(presets)) {
    log_debug(
      paste("Size preset found:", preset_key),
      .context = "EXPORT_PNG"
    )
    return(presets[[preset_key]])
  } else {
    # Default to medium if unknown preset
    log_warn(
      paste("Unknown size preset:", preset_name, "- defaulting to medium"),
      .context = "EXPORT_PNG"
    )
    return(presets$medium)
  }
}

#' Convert Pixel Dimensions to Inches
#'
#' Helper function til at konvertere pixel dimensioner til inches baseret på DPI.
#' Bruges når size preset er i pixels (small, medium, large) og skal konverteres
#' til inches for ggplot2::ggsave().
#'
#' @param width_px Numeric width i pixels
#' @param height_px Numeric height i pixels
#' @param dpi Numeric DPI resolution
#'
#' @return Named list med width_inches og height_inches
#'
#' @details
#' Conversion formula: inches = pixels / DPI
#' Example: 960 pixels / 96 DPI = 10 inches
#'
#' @keywords internal
#' @noRd
convert_pixels_to_inches <- function(width_px, height_px, dpi) {
  list(
    width_inches = width_px / dpi,
    height_inches = height_px / dpi
  )
}
