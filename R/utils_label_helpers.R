# utils_label_helpers.R
# Marquee label helper functions for advanced label placement
#
# Extracted from bfh_layout_reference_dev.R POC
# Provides caching, sanitization, and responsive label formatting

# ============================================================================
# PERFORMANCE: Marquee style cache
# ============================================================================
# Cache for marquee style objects keyed by lineheight
# Eliminerer redundant style creation når samme lineheight genbruges
.marquee_style_cache <- new.env(parent = emptyenv())

#' Get cached right-aligned marquee style
#'
#' PERFORMANCE: Returnerer cached style object hvis tilgængelig.
#' Style creation er relativt dyrt (~1-2ms), og styles er immutable baseret
#' på lineheight parameter, så caching er sikkert.
#'
#' @param lineheight numeric lineheight værdi (default 0.9)
#' @return marquee style object
#' @keywords internal
#' @export
get_right_aligned_marquee_style <- function(lineheight = 0.9) {
  cache_key <- paste0("right_aligned_", lineheight)

  if (!exists(cache_key, envir = .marquee_style_cache)) {
    # CACHE MISS: Opret ny style
    style <- marquee::modify_style(
      marquee::classic_style(),
      "p",
      margin = marquee::trbl(0),
      align = "right",
      lineheight = lineheight
    )
    .marquee_style_cache[[cache_key]] <- style
  }

  # CACHE HIT: Returnér cached style
  .marquee_style_cache[[cache_key]]
}

#' Clear marquee style cache (for testing or memory management)
#'
#' @keywords internal
#' @export
clear_marquee_style_cache <- function() {
  rm(list = ls(envir = .marquee_style_cache), envir = .marquee_style_cache)
  invisible(NULL)
}

# ============================================================================
# INPUT SANITIZATION
# ============================================================================

#' Sanitize text for marquee markup
#'
#' Escaper specialtegn der har betydning i marquee markup
#' for at forhindre injection attacks
#'
#' @param text Character string to sanitize
#' @return Sanitized character string
#' @keywords internal
#' @export
sanitize_marquee_text <- function(text) {
  if (is.null(text) || length(text) == 0) {
    return("")
  }

  if (!is.character(text)) {
    warning("sanitize_marquee_text: Konverterer ikke-character input til character")
    text <- as.character(text)
  }

  if (length(text) > 1) {
    warning("sanitize_marquee_text: Flere værdier modtaget, bruger kun første")
    text <- text[1]
  }

  # Escape marquee special characters
  # Note: Marquee bruger {} til markup, ** bevar vi da det skal være bold i output
  text <- gsub("\\{", "&#123;", text) # Escape {
  text <- gsub("\\}", "&#125;", text) # Escape }

  # Fjern kontroltegn (men bevar \n for linjeskift)
  text <- gsub("[[:cntrl:]&&[^\n]]", "", text)

  # Begræns længde for at forhindre memory exhaustion
  max_length <- 200
  if (nchar(text) > max_length) {
    warning(sprintf("Text afkortet fra %d til %d tegn", nchar(text), max_length))
    text <- substr(text, 1, max_length)
  }

  text
}

# ============================================================================
# RESPONSIVE LABEL FORMATTING
# ============================================================================

#' Generer responsive marquee label med skalerede font-størrelser
#'
#' @param header character header text
#' @param value character value text
#' @param label_size numeric label size parameter (default 6, som i legacy code)
#' @param header_pt numeric header font size ved label_size=6 (default 10)
#' @param value_pt numeric value font size ved label_size=6 (default 30)
#' @return character marquee-formateret label string
#'
#' @details
#' Funktionen bruger `label_size` semantik (baseline = 6) frem for `base_size` (baseline = 14)
#' for at matche legacy SPC plot sizing konventioner.
#'
#' @examples
#' \dontrun{
#' create_responsive_label("MÅL", ">= 90%", label_size = 6)
#' }
#'
#' @export
create_responsive_label <- function(header, value, label_size = 6, header_pt = 10, value_pt = 30) {
  # Input validation
  if (!is.numeric(label_size) || length(label_size) != 1 || label_size <= 0) {
    stop("label_size skal være et positivt tal, modtog: ", label_size)
  }

  if (label_size < 1 || label_size > 24) {
    warning("label_size uden for normalt interval (1-24), modtog: ", label_size)
  }

  if (!is.numeric(header_pt) || !is.numeric(value_pt)) {
    stop("header_pt og value_pt skal være numeriske")
  }

  if (header_pt <= 0 || value_pt <= 0) {
    stop("header_pt og value_pt skal være positive")
  }

  # Sanitize inputs
  header <- sanitize_marquee_text(header)
  value <- sanitize_marquee_text(value)

  # Compute scaled sizes (baseline: label_size = 6)
  scale_factor <- label_size / 6
  header_size <- round(header_pt * scale_factor)
  value_size <- round(value_pt * scale_factor)

  # Sanity check sizes
  if (header_size < 1 || header_size > 100) {
    stop(sprintf("Beregnet header_size uden for gyldigt interval: %d", header_size))
  }
  if (value_size < 1 || value_size > 100) {
    stop(sprintf("Beregnet value_size uden for gyldigt interval: %d", value_size))
  }

  sprintf(
    "{.%d **%s**}  \n{.%d **%s**}",
    header_size,
    header,
    value_size,
    value
  )
}
