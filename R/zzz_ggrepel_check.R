#' Check marquee geom availability (strict marquee::geom_marquee)
#'
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  # Strict requirement: marquee::geom_marquee
  has_marquee <- tryCatch(requireNamespace("marquee", quietly = TRUE) &&
    "geom_marquee" %in% getNamespaceExports("marquee"), error = function(e) FALSE)

  if (!has_marquee) {
    packageStartupMessage(
      "\n",
      "╔══════════════════════════════════════════════════════════════════╗\n",
      "║ WARNING: Marquee geom mangler                                   ║\n",
      "║                                                                  ║\n",
      "║ Pakken 'marquee' med geom_marquee() er ikke installeret.         ║\n",
      "║                                                                  ║\n",
      "║ Løsning:                                                         ║\n",
      "║  • Installer 'marquee'                                           ║\n",
      "║  • eller kør renv::restore()                                     ║\n",
      "╚══════════════════════════════════════════════════════════════════╝\n"
    )
  } else {
    # Silent success
  }
}

#' Ensure a marquee geom is available before plotting
#'
#' @return TRUE if available, stops with informative error otherwise
#' @keywords internal
check_ggrepel_marquee <- function() {
  has_marquee <- tryCatch(requireNamespace("marquee", quietly = TRUE) &&
    "geom_marquee" %in% getNamespaceExports("marquee"), error = function(e) FALSE)

  if (!has_marquee) {
    stop(
      "\n",
      "marquee::geom_marquee er ikke tilgængelig.\n\n",
      "Installer 'marquee' pakken.\n\n",
      "Eller brug renv:\n",
      "  renv::restore()\n\n",
      "Genstart derefter R session.",
      call. = FALSE
    )
  }
  TRUE
}
