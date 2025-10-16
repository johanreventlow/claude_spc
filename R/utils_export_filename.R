# ==============================================================================
# UTILS_EXPORT_FILENAME.R
# ==============================================================================
# FORMÅL: Filename generation og sanitization for export funktioner.
#         Sikrer cross-platform kompatibilitet og korrekt håndtering af
#         danske karakterer.
#
# FUNKTIONER:
#   - generate_export_filename() - Generer komplet export filnavn
#   - sanitize_filename() - Rens filnavn komponenter
#
# ANVENDES AF:
#   - Export module server (mod_export_server.R)
#   - PDF export logic
#   - PNG export logic
#   - PowerPoint export logic
#
# KONVENTIONER:
#   - Prefix: "SPC" (fra EXPORT_FILENAME_PREFIX)
#   - Separator: "_" (fra EXPORT_FILENAME_SEPARATOR)
#   - Format: SPC_Department_Title.extension
#   - Danske karakterer bevares (æøåÆØÅ)
#   - Special characters fjernes for cross-platform kompatibilitet
# ==============================================================================

#' Generate Export Filename
#'
#' Genererer sanitized filnavn til export baseret på format, titel og afdeling.
#' Inkluderer korrekt file extension og håndterer danske karakterer.
#'
#' @param format Export format ("pdf", "png", "powerpoint" eller "pptx")
#' @param title Chart titel (optional)
#' @param department Afdeling/afsnit navn (optional)
#'
#' @return Complete filename med extension (e.g., "SPC_Kardiologi_Graf.pdf")
#'
#' @details
#' Filename structure:
#' - Prefix (SPC) + separator (_) + Department + separator (_) + Title + extension
#' - Tomme komponenter udelades
#' - Alle komponenter saniteres via sanitize_filename()
#'
#' Supported formats:
#' - "pdf" → ".pdf"
#' - "png" → ".png"
#' - "powerpoint" eller "pptx" → ".pptx"
#' - Unknown → ".pdf" (default)
#'
#' @examples
#' \dontrun{
#' generate_export_filename("pdf", "Min Graf", "Kardiologi")
#' # "SPC_Kardiologi_Min_Graf.pdf"
#'
#' generate_export_filename("png", "SPC Oversigt")
#' # "SPC_SPC_Oversigt.png"
#'
#' generate_export_filename("pptx", "", "Hæmatologi Ø")
#' # "SPC_Hæmatologi_Ø.pptx"
#' }
#'
#' @export
generate_export_filename <- function(format, title = "", department = "") {
  # Initialize with prefix
  parts <- c(EXPORT_FILENAME_PREFIX)

  # Convert NULL to empty string
  title <- title %||% ""
  department <- department %||% ""

  # Add department if provided
  if (nchar(department) > 0) {
    sanitized_dept <- sanitize_filename(department)
    if (nchar(sanitized_dept) > 0) {
      parts <- c(parts, sanitized_dept)
    }
  }

  # Add title if provided
  if (nchar(title) > 0) {
    sanitized_title <- sanitize_filename(title)
    if (nchar(sanitized_title) > 0) {
      parts <- c(parts, sanitized_title)
    }
  }

  # Combine parts with separator
  base_name <- paste(parts, collapse = EXPORT_FILENAME_SEPARATOR)

  # Determine extension based on format
  extension <- switch(tolower(format),
    "pdf" = ".pdf",
    "png" = ".png",
    "powerpoint" = ".pptx",
    "pptx" = ".pptx",
    ".pdf" # default fallback
  )

  # Combine base name and extension
  filename <- paste0(base_name, extension)

  return(filename)
}

#' Sanitize Filename
#'
#' Renser filnavn komponent for special characters og sikrer cross-platform
#' kompatibilitet. Bevarer danske karakterer (æøåÆØÅ).
#'
#' @param text Input text til sanitization
#'
#' @return Sanitized filename komponent
#'
#' @details
#' Sanitization proces:
#' 1. Trim leading/trailing whitespace
#' 2. Replace spaces med underscores
#' 3. Remove special characters (keep alphanumeric, Danish chars, underscore, hyphen)
#' 4. Collapse multiple underscores til single underscore
#' 5. Remove leading/trailing underscores
#'
#' Allowed characters:
#' - A-Z, a-z (Latin letters)
#' - 0-9 (Numbers)
#' - æøåÆØÅ (Danish characters)
#' - _ (Underscore)
#' - - (Hyphen)
#'
#' Removed characters:
#' - Path separators (/ \)
#' - Special characters (@#$%^&*()!+=[]{}|;:'",<>?)
#' - Unicode characters outside allowed set
#'
#' @examples
#' \dontrun{
#' sanitize_filename("København Sygehus")
#' # "København_Sygehus"
#'
#' sanitize_filename("Test@#$Name")
#' # "TestName"
#'
#' sanitize_filename("  Multiple   Spaces  ")
#' # "Multiple_Spaces"
#' }
#'
#' @export
sanitize_filename <- function(text) {
  # Handle NULL input
  if (is.null(text)) {
    return("")
  }

  # Convert to character if necessary
  if (!is.character(text)) {
    text <- as.character(text)
  }

  # Trim leading/trailing whitespace
  text <- trimws(text)

  # Replace spaces with underscores
  text <- gsub(" ", "_", text, fixed = TRUE)

  # Remove special characters (keep alphanumeric, Danish chars, underscore, hyphen)
  # Pattern: keep A-Z, a-z, 0-9, æøåÆØÅ, underscore, hyphen
  text <- gsub("[^A-Za-z0-9_æøåÆØÅ-]", "", text)

  # Collapse multiple underscores to single underscore
  text <- gsub("_{2,}", "_", text)

  # Remove leading underscores
  text <- gsub("^_+", "", text)

  # Remove trailing underscores
  text <- gsub("_+$", "", text)

  return(text)
}

# HELPER: NULL coalescing operator ============================================

#' NULL Coalescing Operator
#'
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
