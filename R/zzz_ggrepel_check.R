#' Check ggrepel version and geom_marquee_repel availability
#'
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  # Check if we have the correct ggrepel fork with marquee support
  has_marquee <- "geom_marquee_repel" %in% getNamespaceExports("ggrepel")
  ggrepel_version <- utils::packageVersion("ggrepel")

  if (!has_marquee) {
    packageStartupMessage(
      "\n",
      "╔══════════════════════════════════════════════════════════════════╗\n",
      "║ WARNING: SPCify kræver en custom version af ggrepel             ║\n",
      "║                                                                  ║\n",
      "║ Du har ggrepel ", format(ggrepel_version, width = 10), " installeret,                   ║\n",
      "║ men geom_marquee_repel() funktionen mangler.                    ║\n",
      "║                                                                  ║\n",
      "║ For at installere den korrekte version:                         ║\n",
      "║   remotes::install_github('teunbrand/ggrepel@marquee_repel')    ║\n",
      "║                                                                  ║\n",
      "║ Eller brug renv til automatisk installation:                    ║\n",
      "║   renv::restore()                                                ║\n",
      "╚══════════════════════════════════════════════════════════════════╝\n"
    )
  } else {
    # Silent success - only show warning if something is wrong
    if (ggrepel_version < "0.9.6.9999") {
      packageStartupMessage(
        "Note: SPCify bruger custom ggrepel fork (", ggrepel_version, ") med geom_marquee_repel support"
      )
    }
  }
}

#' Ensure geom_marquee_repel is available before plotting
#'
#' @return TRUE if available, stops with informative error otherwise
#' @keywords internal
check_ggrepel_marquee <- function() {
  if (!"geom_marquee_repel" %in% getNamespaceExports("ggrepel")) {
    stop(
      "\n",
      "geom_marquee_repel() er ikke tilgængelig i din ggrepel installation.\n\n",
      "SPCify kræver en custom fork af ggrepel med marquee support.\n\n",
      "Installér den korrekte version:\n",
      "  remotes::install_github('teunbrand/ggrepel@marquee_repel')\n\n",
      "Eller brug renv:\n",
      "  renv::restore()\n\n",
      "Genstart derefter R session.",
      call. = FALSE
    )
  }
  TRUE
}
