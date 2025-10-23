# ==============================================================================
# CONFIG_PLOT_CONTEXTS.R
# ==============================================================================
# FORMÅL: Definerer plot-kontekster og deres dimensioner for context-aware
#         plot generation og caching. Sikrer at labels placeres korrekt og
#         cache isolation mellem forskellige plot contexts.
#
# ANVENDES AF:
#   - Plot generation (generateSPCPlot med context parameter)
#   - Cache keys (spc_results reactive)
#   - Export functions (PDF, PNG, PowerPoint)
#   - Label placement (fct_add_spc_labels.R)
#
# RELATERET:
#   - Issue #61: Context-aware label placement
#   - Issue #62: Context-aware plot caching
#   - docs/PLOT_CONTEXTS.md (future documentation)
# ==============================================================================

#' Plot Context Identifiers
#'
#' Definerer de forskellige kontekster hvor SPC plots genereres.
#' Hver kontekst har potentielt forskellige dimensioner, DPI og label krav.
#'
#' @format Named list med context identifiers (character strings)
#' @details
#' Kontekster:
#' - **analysis:** Analyse-side interactive plot (responsive viewport)
#' - **export_preview:** Export-side preview (fast 800×450px @ 96 DPI)
#' - **export_pdf:** PDF export via Typst (200×120mm @ 300 DPI)
#' - **export_png:** PNG export (variable dimensions, configurable DPI)
#' - **export_pptx:** PowerPoint export (9×6.5 inches @ 96 DPI)
#'
#' @export
PLOT_CONTEXTS <- list(
  ANALYSIS = "analysis",
  EXPORT_PREVIEW = "export_preview",
  EXPORT_PDF = "export_pdf",
  EXPORT_PNG = "export_png",
  EXPORT_PPTX = "export_pptx"
)

#' Plot Context Dimensions
#'
#' Standard dimensioner for hver plot context.
#' Bruges som fallback når eksplicitte dimensioner ikke er tilgængelige.
#'
#' @format Named list med dimension specifications per context
#' @details
#' Hver context har:
#' - width_px/width_inches: Bredde (afhængig af unit)
#' - height_px/height_inches: Højde (afhængig af unit)
#' - dpi: Opløsning
#' - unit: "px" (pixels) eller "in" (inches)
#' - description: Kort beskrivelse af use case
#'
#' **Vigtig note om export contexts:**
#' For export contexts (PDF, PNG, PPTX) er disse dimensioner **defaults**.
#' Faktiske dimensioner kommer fra:
#' - PDF: Typst template settings (200×120mm @ 300 DPI)
#' - PNG: User-valgte presets eller custom dimensions
#' - PPTX: PowerPoint slide dimensions (9×6.5 inches @ 96 DPI)
#'
#' @export
PLOT_CONTEXT_DIMENSIONS <- list(
  analysis = list(
    width_px = 800,
    height_px = 600,
    dpi = 96,
    unit = "px",
    description = "Interactive display on Analyse-side (responsive viewport)",
    responsive = TRUE # Actual dimensions from clientData
  ),
  export_preview = list(
    width_px = 800,
    height_px = 450,
    dpi = 96,
    unit = "px",
    description = "Live preview on Export-side (16:9 aspect ratio)",
    responsive = FALSE # Fixed dimensions
  ),
  export_pdf = list(
    width_mm = 200,
    height_mm = 120,
    dpi = 300,
    unit = "mm",
    description = "PDF export via Typst (A4 landscape optimized, high-res)",
    responsive = FALSE # Fixed dimensions from Typst template
  ),
  export_png = list(
    width_px = 1200,
    height_px = 900,
    dpi = 96,
    unit = "px",
    description = "PNG export default (medium preset, user-configurable)",
    responsive = FALSE # User-selectable dimensions
  ),
  export_pptx = list(
    width_inches = 9,
    height_inches = 6.5,
    dpi = 96,
    unit = "in",
    description = "PowerPoint export (slide-optimized dimensions)",
    responsive = FALSE # Fixed dimensions for slide embedding
  )
)

#' Get Dimensions for Plot Context
#'
#' Henter standard dimensioner for en given plot context.
#' Returnerer dimensions i pixels (konverteret fra andre units hvis nødvendigt).
#'
#' @param context Character. Context identifier (brug PLOT_CONTEXTS konstanter)
#' @param override_width Numeric. Override width (optional, i pixels)
#' @param override_height Numeric. Override height (optional, i pixels)
#' @param override_dpi Numeric. Override DPI (optional)
#'
#' @return Named list med width_px, height_px, dpi
#'
#' @details
#' Denne funktion:
#' 1. Validerer at context eksisterer
#' 2. Henter standard dimensioner fra PLOT_CONTEXT_DIMENSIONS
#' 3. Konverterer til pixels hvis unit er "mm" eller "in"
#' 4. Anvender overrides hvis specificeret
#' 5. Returnerer standardiseret format (pixels)
#'
#' @examples
#' \dontrun{
#' # Get analysis context dimensions
#' dims <- get_context_dimensions("analysis")
#' # Returns: list(width_px = 800, height_px = 600, dpi = 96)
#'
#' # Get PDF context with custom DPI
#' dims <- get_context_dimensions("export_pdf", override_dpi = 150)
#'
#' # Get PNG context with custom size
#' dims <- get_context_dimensions("export_png",
#'   override_width = 1920,
#'   override_height = 1080
#' )
#' }
#'
#' @export
get_context_dimensions <- function(context,
                                   override_width = NULL,
                                   override_height = NULL,
                                   override_dpi = NULL) {
  # Validate context
  if (!context %in% names(PLOT_CONTEXT_DIMENSIONS)) {
    stop(
      sprintf(
        "Unknown plot context: '%s'. Valid contexts: %s",
        context,
        paste(names(PLOT_CONTEXT_DIMENSIONS), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Get context config
  config <- PLOT_CONTEXT_DIMENSIONS[[context]]

  # Extract dimensions based on unit
  if (config$unit == "px") {
    width_px <- config$width_px
    height_px <- config$height_px
  } else if (config$unit == "mm") {
    # Convert mm to pixels: mm / 25.4 * dpi
    width_px <- (config$width_mm / 25.4) * config$dpi
    height_px <- (config$height_mm / 25.4) * config$dpi
  } else if (config$unit == "in") {
    # Convert inches to pixels: inches * dpi
    width_px <- config$width_inches * config$dpi
    height_px <- config$height_inches * config$dpi
  } else {
    stop(
      sprintf("Unknown unit type: %s", config$unit),
      call. = FALSE
    )
  }

  # Apply overrides
  if (!is.null(override_width)) {
    width_px <- override_width
  }

  if (!is.null(override_height)) {
    height_px <- override_height
  }

  dpi <- if (!is.null(override_dpi)) override_dpi else config$dpi

  # Return standardized format
  return(list(
    width_px = round(width_px),
    height_px = round(height_px),
    dpi = dpi,
    context = context,
    responsive = config$responsive %||% FALSE
  ))
}

#' Validate Plot Context
#'
#' Validerer at en context identifier er gyldig.
#'
#' @param context Character. Context identifier to validate
#' @param stop_on_invalid Logical. Stop execution if invalid (default TRUE)
#'
#' @return Logical. TRUE hvis valid, FALSE hvis invalid (når stop_on_invalid = FALSE)
#'
#' @examples
#' \dontrun{
#' validate_plot_context("analysis") # Returns TRUE
#' validate_plot_context("invalid") # Stops with error
#' validate_plot_context("invalid", stop_on_invalid = FALSE) # Returns FALSE
#' }
#'
#' @export
validate_plot_context <- function(context, stop_on_invalid = TRUE) {
  is_valid <- context %in% unlist(PLOT_CONTEXTS, use.names = FALSE)

  if (!is_valid && stop_on_invalid) {
    stop(
      sprintf(
        "Invalid plot context: '%s'. Valid contexts: %s",
        context,
        paste(unlist(PLOT_CONTEXTS, use.names = FALSE), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  return(is_valid)
}

# NULL coalescing operator (hvis ikke allerede defineret)
if (!exists("%||%")) {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}
