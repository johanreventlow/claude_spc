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
#' @param y_axis_unit character unit for y-akse ("count", "percent", "rate", "time", eller andet)
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

  # Validate y_axis_unit
  valid_units <- c("count", "percent", "rate", "time")
  if (!y_axis_unit %in% valid_units && verbose) {
    message("y_axis_unit '", y_axis_unit, "' ikke standard. Understøttede: ",
            paste(valid_units, collapse = ", "))
  }

  # Beregn y_range for time formatting context
  y_range <- if (y_axis_unit == "time" && !is.null(qic_data$y)) {
    range(qic_data$y, na.rm = TRUE)
  } else {
    NULL
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

  # Formatér labels med delt formatter ----
  label_cl <- NULL
  if (!is.na(cl_value)) {
    formatted_cl <- format_y_value(cl_value, y_axis_unit, y_range)
    label_cl <- create_responsive_label(
      header = "NUV. NIVEAU",
      value = formatted_cl,
      base_size = label_size
    )
  }

  label_target <- NULL
  if (!is.na(target_value)) {
    formatted_target <- format_y_value(target_value, y_axis_unit, y_range)
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
