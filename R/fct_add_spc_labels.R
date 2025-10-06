# fct_add_spc_labels.R
# Shiny app wrapper for advanced label placement system
#
# Forenklet interface til add_right_labels_marquee() med Shiny-specifikke defaults

#' Add SPC labels to plot using advanced placement system
#'
#' Wrapper funktion der tilføjer CL og Target labels til SPC plot
#' ved hjælp af NPC-baseret collision avoidance system.
#'
#' @param plot ggplot object (SPC plot uden labels)
#' @param qic_data data.frame fra qicharts2::qic() med columns: cl, target, part
#' @param y_axis_unit character unit for y-akse ("count", "percent", eller andet)
#' @param label_size numeric base font size for responsive sizing (default 6)
#' @param verbose logical print placement warnings (default FALSE)
#' @param debug_mode logical add visual debug annotations (default FALSE)
#' @return ggplot object med tilføjede labels
#'
#' @details
#' Funktionen:
#' 1. Ekstraherer CL værdi fra seneste part i qic_data
#' 2. Ekstraherer Target værdi fra qic_data
#' 3. Formaterer værdier baseret på y_axis_unit
#' 4. Opretter responsive marquee-formaterede labels
#' 5. Kalder add_right_labels_marquee() med Shiny-specifikke defaults
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' plot <- qic(x, y, data = df, chart = "i")
#' plot_with_labels <- add_spc_labels(plot, plot$data, y_axis_unit = "count")
#'
#' # With debug mode
#' plot_with_labels <- add_spc_labels(
#'   plot, plot$data,
#'   y_axis_unit = "percent",
#'   verbose = TRUE,
#'   debug_mode = TRUE
#' )
#' }
#'
#' @export
add_spc_labels <- function(
  plot,
  qic_data,
  y_axis_unit = "count",
  label_size = 6,
  verbose = FALSE,
  debug_mode = FALSE
) {

  # Input validation ----
  if (!inherits(plot, "gg")) {
    stop("plot skal være et ggplot object")
  }

  if (!is.data.frame(qic_data)) {
    stop("qic_data skal være en data.frame")
  }

  if (!y_axis_unit %in% c("count", "percent")) {
    warning("y_axis_unit skal være 'count' eller 'percent'. Bruger 'count' som fallback.")
    y_axis_unit <- "count"
  }

  # Hjælpefunktion til formatering af værdier PRÆCIS som y-aksen ----
  # TODO: Denne funktion bør ekstraheres til utils_label_formatting.R
  # så den kan genbruges i add_plot_enhancements()
  format_y_value <- function(val, y_unit) {
    if (is.na(val)) return(NA_character_)

    if (y_unit == "percent") {
      # Percent formatting - matcher scale_y_continuous(labels = scales::label_percent())
      scales::label_percent()(val)
    } else if (y_unit == "count") {
      # Count formatting med K/M notation - matcher y-akse logik præcist
      if (abs(val) >= 1e9) {
        scaled <- val / 1e9
        if (scaled == round(scaled)) {
          paste0(round(scaled), " mia.")
        } else {
          paste0(format(scaled, decimal.mark = ",", nsmall = 1), " mia.")
        }
      } else if (abs(val) >= 1e6) {
        scaled <- val / 1e6
        if (scaled == round(scaled)) {
          paste0(round(scaled), "M")
        } else {
          paste0(format(scaled, decimal.mark = ",", nsmall = 1), "M")
        }
      } else if (abs(val) >= 1e3) {
        scaled <- val / 1e3
        if (scaled == round(scaled)) {
          paste0(round(scaled), "K")
        } else {
          paste0(format(scaled, decimal.mark = ",", nsmall = 1), "K")
        }
      } else {
        # For values < 1000: show decimals only if present
        if (val == round(val)) {
          format(round(val), decimal.mark = ",", big.mark = ".")
        } else {
          format(val, decimal.mark = ",", nsmall = 1, big.mark = ".")
        }
      }
    } else {
      # Default: formatér som tal med dansk decimal notation
      format(val, decimal.mark = ",", nsmall = 1, big.mark = ".")
    }
  }

  # Ekstrahér CL værdi fra seneste part ----
  cl_value <- NA_real_
  if (!is.null(qic_data$cl) && any(!is.na(qic_data$cl))) {
    # Find seneste part
    if ("part" %in% names(qic_data)) {
      latest_part <- max(qic_data$part, na.rm = TRUE)
      part_data <- qic_data[qic_data$part == latest_part & !is.na(qic_data$part), ]

      if (nrow(part_data) > 0) {
        # Brug sidste punkt i seneste part
        last_row <- part_data[nrow(part_data), ]
        cl_value <- last_row$cl
      }
    } else {
      # Ingen parts - brug sidste CL værdi
      cl_value <- tail(stats::na.omit(qic_data$cl), 1)
    }
  }

  # Ekstrahér Target værdi ----
  target_value <- NA_real_
  if (!is.null(qic_data$target) && any(!is.na(qic_data$target))) {
    target_value <- qic_data$target[!is.na(qic_data$target)][1]
  }

  # Valider at vi har mindst én værdi ----
  if (is.na(cl_value) && is.na(target_value)) {
    warning("Ingen CL eller Target værdier fundet i qic_data. Returnerer plot uændret.")
    return(plot)
  }

  # Formatér labels ----
  label_cl <- NULL
  if (!is.na(cl_value)) {
    formatted_cl <- format_y_value(cl_value, y_axis_unit)
    label_cl <- create_responsive_label(
      header = "NUV. NIVEAU",
      value = formatted_cl,
      base_size = label_size
    )
  }

  label_target <- NULL
  if (!is.na(target_value)) {
    formatted_target <- format_y_value(target_value, y_axis_unit)
    # Tilføj større-end tegn for target
    størreend <- "\U2265"
    label_target <- create_responsive_label(
      header = "MÅL",
      value = paste0(størreend, formatted_target),
      base_size = label_size
    )
  }

  # Håndter edge case: kun én label ----
  if (is.null(label_cl) || is.na(cl_value)) {
    # Kun target label
    if (verbose) {
      message("Kun Target label tilgængelig")
    }
    # Sæt cl til NA og brug target som primær
    yA <- target_value
    yB <- NA_real_
    textA <- label_target
    textB <- ""
  } else if (is.null(label_target) || is.na(target_value)) {
    # Kun CL label
    if (verbose) {
      message("Kun CL label tilgængelig")
    }
    yA <- cl_value
    yB <- NA_real_
    textA <- label_cl
    textB <- ""
  } else {
    # Begge labels tilgængelige
    yA <- cl_value
    yB <- target_value
    textA <- label_cl
    textB <- label_target
  }

  # Placer labels med advanced placement system ----
  plot_with_labels <- add_right_labels_marquee(
    p = plot,
    yA = yA,
    yB = yB,
    textA = textA,
    textB = textB,
    params = list(
      # label_height_npc - AUTO-BEREGNES fra font sizes
      # gap_line - AUTO: fra config (relative_gap_line)
      # gap_labels - AUTO: fra config (relative_gap_labels)
      pad_top = 0.01,               # Top padding
      pad_bot = 0.01,               # Bottom padding
      pref_pos = c("under", "under"), # Default: placer under linjer
      priority = "A"                # Beskyt CL label ved konflikter
    ),
    gpA = grid::gpar(col = "#009CE8"),  # CL label farve (lyseblå)
    gpB = grid::gpar(col = "#565656"),  # Target label farve (grå)
    base_size = label_size,           # Responsive sizing
    verbose = verbose,                # Print placement warnings
    debug_mode = debug_mode           # Visual debugging
  )

  return(plot_with_labels)
}
