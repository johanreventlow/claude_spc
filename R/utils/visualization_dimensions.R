# R/utils/visualization_dimensions.R
# Hjælpefunktioner til beregning af plotdimensioner baseret på klientdata

#' Beregn dynamisk plot-højde
#'
#' Beregner passende plot-højde for SPC visualiseringer baseret på tilgængelige
#' klientdimensioner og en fallback-aspekt-ratio. Sikrer minimumshøjde for at
#' undgå ggplot "margins too large" fejl når containeren er meget lille.
#'
#' @param width Numerisk. Aktuel bredde fra Shiny clientData (pixels)
#' @param client_height Numerisk. Registreret højde fra Shiny clientData (pixels)
#' @param aspect_ratio Numerisk. Forhold mellem højde og bredde (standard 0.75)
#' @param min_height Numerisk. Minimum højde for plot (default 420)
#' @param default_height Numerisk. Fallback når ingen dimensioner findes (default 600)
#'
#' @return Numerisk højde i pixels
#' @keywords internal
compute_spc_plot_height <- function(width,
                                    client_height,
                                    aspect_ratio = 0.75,
                                    min_height = 420,
                                    default_height = 600) {
  # Brug klientens højde når den er valide og over minimum
  if (is.numeric(client_height) && length(client_height) == 1 && !is.na(client_height)) {
    if (client_height >= min_height) {
      return(client_height)
    }
  }

  # Fallback: Brug bredde og aspekt ratio når tilgængelig
  if (is.numeric(width) && length(width) == 1 && !is.na(width) && width > 0) {
    computed_height <- round(width * aspect_ratio)
    return(max(min_height, computed_height))
  }

  # Endelig fallback til default højde
  default_height
}
