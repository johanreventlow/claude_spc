# ==============================================================================
# UTILS_EXPORT_VALIDATION.R
# ==============================================================================
# FORMÅL: Input validation og sanitization for export funktioner.
#         Sikrer XSS protection, character limits og dimension validation.
#
# FUNKTIONER:
#   - validate_export_inputs() - Komplet input validation
#   - sanitize_user_input() - XSS protection og character filtering
#   - validate_aspect_ratio() - Aspect ratio validation med warnings
#
# ANVENDES AF:
#   - Export module server (mod_export_server.R)
#   - PDF export logic
#   - PNG export logic
#   - PowerPoint export logic
#
# SIKKERHED:
#   - XSS protection gennem HTML escaping
#   - Character filtering til cross-platform kompatibilitet
#   - Input validation forhindrer buffer overflow og injection
# ==============================================================================

#' Validate Export Inputs
#'
#' Validerer alle export inputs før generering af PDF/PNG/PowerPoint.
#' Tjekker character limits, dimension ranges og aspect ratios.
#'
#' @param format Export format ("pdf", "png", "pptx")
#' @param title Chart titel (max 200 tegn)
#' @param department Afdeling/afsnit (max 100 tegn)
#' @param description Indikator beskrivelse (max 2000 tegn)
#' @param width Custom bredde i pixels (kun PNG)
#' @param height Custom højde i pixels (kun PNG)
#'
#' @return TRUE hvis alle validations passerer
#' @throws Error hvis validation fejler med beskrivende fejlbesked
#'
#' @examples
#' \dontrun{
#' validate_export_inputs(
#'   format = "pdf",
#'   title = "Min SPC Graf",
#'   department = "Kardiologi"
#' )
#'
#' validate_export_inputs(
#'   format = "png",
#'   title = "Graf",
#'   width = 1200,
#'   height = 900
#' )
#' }
#'
#' @export
validate_export_inputs <- function(format,
                                   title = "",
                                   department = "",
                                   description = "",
                                   width = NULL,
                                   height = NULL) {
  errors <- character(0)

  # Convert NULL to empty string
  title <- title %||% ""
  department <- department %||% ""
  description <- description %||% ""

  # Character limit validation
  if (nchar(title) > EXPORT_TITLE_MAX_LENGTH) {
    errors <- c(errors, sprintf(
      "Titel må max være %d tegn (nuværende: %d)",
      EXPORT_TITLE_MAX_LENGTH,
      nchar(title)
    ))
  }

  if (nchar(description) > EXPORT_DESCRIPTION_MAX_LENGTH) {
    errors <- c(errors, sprintf(
      "Beskrivelse må max være %d tegn (nuværende: %d)",
      EXPORT_DESCRIPTION_MAX_LENGTH,
      nchar(description)
    ))
  }

  if (nchar(department) > EXPORT_DEPARTMENT_MAX_LENGTH) {
    errors <- c(errors, sprintf(
      "Afdeling må max være %d tegn (nuværende: %d)",
      EXPORT_DEPARTMENT_MAX_LENGTH,
      nchar(department)
    ))
  }

  # PNG-specific dimension validation
  if (tolower(format) == "png" && !is.null(width) && !is.null(height)) {
    # Width validation
    if (width < EXPORT_VALIDATION_RULES$min_width_px) {
      errors <- c(errors, sprintf(
        "Bredde skal være mellem %d og %d pixels (nuværende: %d)",
        EXPORT_VALIDATION_RULES$min_width_px,
        EXPORT_VALIDATION_RULES$max_width_px,
        width
      ))
    }

    if (width > EXPORT_VALIDATION_RULES$max_width_px) {
      errors <- c(errors, sprintf(
        "Bredde skal være mellem %d og %d pixels (nuværende: %d)",
        EXPORT_VALIDATION_RULES$min_width_px,
        EXPORT_VALIDATION_RULES$max_width_px,
        width
      ))
    }

    # Height validation
    if (height < EXPORT_VALIDATION_RULES$min_height_px) {
      errors <- c(errors, sprintf(
        "Højde skal være mellem %d og %d pixels (nuværende: %d)",
        EXPORT_VALIDATION_RULES$min_height_px,
        EXPORT_VALIDATION_RULES$max_height_px,
        height
      ))
    }

    if (height > EXPORT_VALIDATION_RULES$max_height_px) {
      errors <- c(errors, sprintf(
        "Højde skal være mellem %d og %d pixels (nuværende: %d)",
        EXPORT_VALIDATION_RULES$min_height_px,
        EXPORT_VALIDATION_RULES$max_height_px,
        height
      ))
    }

    # Aspect ratio validation
    if (width > 0 && height > 0) {
      aspect_ratio <- width / height

      if (aspect_ratio < EXPORT_ASPECT_RATIO_MIN ||
        aspect_ratio > EXPORT_ASPECT_RATIO_MAX) {
        errors <- c(errors, sprintf(
          "⚠️ Ekstrem aspekt-ratio (%.2f) kan resultere i forvrænget graf (forventet: %.1f-%.1f)",
          aspect_ratio,
          EXPORT_ASPECT_RATIO_MIN,
          EXPORT_ASPECT_RATIO_MAX
        ))
      }
    }
  }

  # Throw error hvis validation fejlede
  if (length(errors) > 0) {
    stop(paste(errors, collapse = "\n"), call. = FALSE)
  }

  return(TRUE)
}

#' Sanitize User Input for XSS Protection
#'
#' Renser bruger input for potentielle XSS angreb og fjerner ugyldige karakterer.
#' Understøtter danske karakterer (æøåÆØÅ) og basis tegnsætning.
#'
#' @param input_value Input string til sanitization
#' @param max_length Maximum tilladt længde (NULL for ingen grænse)
#' @param allowed_chars Regex pattern for tilladte karakterer
#' @param html_escape Escape HTML special characters (default TRUE)
#'
#' @return Sanitized string
#'
#' @details
#' Sanitization proces:
#' 1. Konverter NULL til tom string
#' 2. Konverter til character hvis nødvendigt
#' 3. HTML escape hvis aktiveret
#' 4. Fjern ugyldige karakterer via regex
#' 5. Trim whitespace
#' 6. Truncate til max_length hvis specificeret
#'
#' @examples
#' \dontrun{
#' sanitize_user_input("<script>alert('XSS')</script>")
#' # "&lt;script&gt;alert('XSS')&lt;/script&gt;"
#'
#' sanitize_user_input("København Sygehus @#$")
#' # "København Sygehus "
#' }
#'
#' @export
sanitize_user_input <- function(input_value,
                                max_length = NULL,
                                allowed_chars = "A-Za-z0-9_æøåÆØÅ .,-:!?*_",
                                html_escape = TRUE) {
  # Handle NULL input
  if (is.null(input_value)) {
    return("")
  }

  # Convert to character if necessary
  if (!is.character(input_value)) {
    input_value <- as.character(input_value)
  }

  # HTML escape for XSS protection
  if (html_escape) {
    input_value <- gsub("<", "&lt;", input_value, fixed = TRUE)
    input_value <- gsub(">", "&gt;", input_value, fixed = TRUE)
    input_value <- gsub("&", "&amp;", input_value, fixed = TRUE)
    input_value <- gsub("\"", "&quot;", input_value, fixed = TRUE)
    input_value <- gsub("'", "&#39;", input_value, fixed = TRUE)

    # Remove JavaScript protocols
    input_value <- gsub("javascript:", "", input_value, ignore.case = TRUE)
    input_value <- gsub("vbscript:", "", input_value, ignore.case = TRUE)
  }

  # Split by newlines to preserve them during sanitization
  # Process each line separately, then rejoin
  lines <- strsplit(input_value, "\n", fixed = TRUE)[[1]]

  # Remove characters not in allowed set from each line (newlines already separated)
  pattern <- sprintf("[^%s]", allowed_chars)
  lines <- gsub(pattern, "", lines)

  # Trim leading/trailing whitespace per line
  lines <- trimws(lines)

  # Remove empty lines but preserve the structure with newlines
  input_value <- paste(lines, collapse = "\n")

  # Truncate to max length if specified
  if (!is.null(max_length) && nchar(input_value) > max_length) {
    input_value <- substr(input_value, 1, max_length)
  }

  return(input_value)
}

#' Validate Aspect Ratio
#'
#' Validerer aspect ratio (bredde/højde) for export dimensioner.
#' Advarer eller fejler ved ekstreme ratios udenfor acceptable grænser.
#'
#' @param width Bredde i pixels eller inches
#' @param height Højde i pixels eller inches
#' @param warn_only Emit warning i stedet for error (default TRUE)
#'
#' @return TRUE hvis aspect ratio er acceptable eller warn_only = TRUE
#' @throws Warning hvis aspect ratio er ekstrem og warn_only = TRUE
#' @throws Error hvis aspect ratio er ekstrem og warn_only = FALSE
#'
#' @details
#' Acceptable aspect ratios: 0.5 - 2.0
#' - < 0.5: For smalt (højt og snævert)
#' - > 2.0: For bredt (lavt og bredt)
#'
#' @examples
#' \dontrun{
#' validate_aspect_ratio(1200, 900) # OK (1.33)
#' validate_aspect_ratio(400, 1000) # Warning (0.4 - too narrow)
#' validate_aspect_ratio(2000, 800, warn_only = FALSE) # Error (2.5 - too wide)
#' }
#'
#' @export
validate_aspect_ratio <- function(width, height, warn_only = TRUE) {
  # Handle invalid inputs
  if (is.null(width) || is.null(height) ||
    !is.numeric(width) || !is.numeric(height)) {
    stop("Width and height must be numeric values", call. = FALSE)
  }

  if (height <= 0) {
    stop("Height must be greater than zero", call. = FALSE)
  }

  if (width <= 0) {
    stop("Width must be greater than zero", call. = FALSE)
  }

  # Calculate aspect ratio
  aspect_ratio <- width / height

  # Check if within acceptable range
  if (aspect_ratio < EXPORT_ASPECT_RATIO_MIN ||
    aspect_ratio > EXPORT_ASPECT_RATIO_MAX) {
    message <- sprintf(
      "Aspect ratio %.2f is extreme (expected: %.1f-%.1f)",
      aspect_ratio,
      EXPORT_ASPECT_RATIO_MIN,
      EXPORT_ASPECT_RATIO_MAX
    )

    if (warn_only) {
      warning(message, call. = FALSE)
      return(TRUE)
    } else {
      stop(message, call. = FALSE)
    }
  }

  return(TRUE)
}

# HELPER: NULL coalescing operator ============================================

#' NULL Coalescing Operator
#'
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
