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
#' @param viewport_width numeric viewport width in pixels (optional, from clientData)
#' @param viewport_height numeric viewport height in pixels (optional, from clientData)
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
    viewport_width = NULL,
    viewport_height = NULL,
    verbose = FALSE,
    debug_mode = FALSE) {
  # Entry logging (conditional)
  if (verbose || getOption("spc.debug.label_placement", FALSE)) {
    message("[ADD_SPC_LABELS] Function called")
    message(sprintf("[ADD_SPC_LABELS] y_axis_unit: %s, label_size: %.1f", y_axis_unit, label_size))

    if (!is.null(viewport_width) && !is.null(viewport_height)) {
      message(sprintf(
        "[ADD_SPC_LABELS] Viewport dimensions provided: %.0f × %.0f pixels",
        viewport_width, viewport_height
      ))
    } else {
      message("[ADD_SPC_LABELS] No viewport dimensions provided - will use device detection")
    }
  }

  # Convert viewport dimensions from pixels to inches (renderPlot uses res=96)
  viewport_width_inches <- if (!is.null(viewport_width)) viewport_width / 96 else NULL
  viewport_height_inches <- if (!is.null(viewport_height)) viewport_height / 96 else NULL

  if (verbose || getOption("spc.debug.label_placement", FALSE)) {
    if (!is.null(viewport_width_inches) && !is.null(viewport_height_inches)) {
      message(sprintf(
        "[ADD_SPC_LABELS] Viewport dimensions in inches: %.2f × %.2f",
        viewport_width_inches, viewport_height_inches
      ))
    }
  }

  # Input validation ----
  if (!inherits(plot, "gg")) {
    message("[ADD_SPC_LABELS] EARLY EXIT: plot is not ggplot object")
    stop("plot skal være et ggplot object")
  }

  if (!is.data.frame(qic_data)) {
    message("[ADD_SPC_LABELS] EARLY EXIT: qic_data is not data.frame")
    stop("qic_data skal være en data.frame")
  }

  if (verbose || getOption("spc.debug.label_placement", FALSE)) {
    message("[ADD_SPC_LABELS] Input validation passed")
  }

  # Validate y_axis_unit
  valid_units <- c("count", "percent", "rate", "time")
  if (!y_axis_unit %in% valid_units && verbose) {
    message(
      "y_axis_unit '", y_axis_unit, "' ikke standard. Understøttede: ",
      paste(valid_units, collapse = ", ")
    )
  }

  # DEVICE INFO LOGGING (Non-blocking) ----
  # FIX: ggplot objects CAN be built without an active graphics device.
  # Device is only needed when renderPlot() executes, not during plot construction.
  # The downstream functions (add_right_labels_marquee, measure_panel_height_from_gtable)
  # already have fallback logic for missing device (defaults to 7×7 inches).
  #
  # Therefore: NO blocking checks here - just log device status for debugging.

  # Log device status (non-blocking)
  device_info <- tryCatch(
    {
      dev_cur <- grDevices::dev.cur()
      dev_open <- dev_cur > 1

      if (dev_open) {
        dev_size <- grDevices::dev.size("in")
        list(
          open = TRUE,
          dev_num = dev_cur,
          width = dev_size[1],
          height = dev_size[2]
        )
      } else {
        list(
          open = FALSE,
          dev_num = dev_cur,
          width = NA_real_,
          height = NA_real_
        )
      }
    },
    error = function(e) {
      list(
        open = FALSE,
        dev_num = 1,
        width = NA_real_,
        height = NA_real_,
        error = e$message
      )
    }
  )

  # Log device info (conditional on verbose mode)
  if (verbose || getOption("spc.debug.label_placement", FALSE)) {
    message(sprintf(
      "[DEVICE_INFO] Device: %s (dev.cur = %d)",
      if (device_info$open) "OPEN" else "NOT OPEN",
      device_info$dev_num
    ))

    if (device_info$open) {
      message(sprintf(
        "[DEVICE_INFO] Size: %.1f × %.1f inches",
        device_info$width,
        device_info$height
      ))
    } else {
      message("[DEVICE_INFO] No device open - downstream functions will use fallback sizes (7×7 inches)")
    }
  }

  # FIX: Auto-scale label_size baseret på device height (hvis tilgængelig)
  # Dette sikrer at labels skalerer proportionelt med plot størrelse
  # Baseline: label_size = 6 for ~7.8" device height (small plot reference)
  device_height_baseline <- 7.8 # inches (reference: 751px @ 96dpi)

  if (device_info$open && !is.na(device_info$height)) {
    # Device aktiv med valid height - skalér label_size
    dev_height <- device_info$height

    # Skalér label_size proportionelt med device height
    # VIGTIGT: Kun skalér opad (scale_factor >= 1.0) for at undgå under-skalering
    # på små plots hvor labels ville blive ulæseligt små
    # Small plot (7.8"): label_size = 6.0 (ikke skaleret ned)
    # Large plot (18.2"): label_size = 6.0 * (18.2/7.8) ≈ 14.0 (skaleret op)
    scale_factor <- pmax(1.0, dev_height / device_height_baseline)
    label_size_scaled <- label_size * scale_factor

    if (verbose) {
      message(sprintf(
        "Auto-scaled label_size: %.1f → %.1f (device height: %.1f\", scale: %.2f)",
        label_size, label_size_scaled, dev_height, scale_factor
      ))
    }

    label_size <- label_size_scaled
  } else {
    # Ingen device eller ingen valid height - brug original label_size
    if (verbose) {
      message(sprintf(
        "No device height available - using fixed label_size: %.1f",
        label_size
      ))
    }
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
    if (verbose || getOption("spc.debug.label_placement", FALSE)) {
      message("[CL_TARGET_CHECK] BLOCKING: Ingen CL eller Target værdier fundet i qic_data")
    }
    warning("Ingen CL eller Target værdier fundet i qic_data. Returnerer plot uændret.")
    return(plot)
  }

  # DEBUG: Log extracted values
  if (verbose || getOption("spc.debug.label_placement", FALSE)) {
    message(sprintf(
      "[CL_TARGET_CHECK] ✓ Values extracted - CL: %s, Target: %s",
      ifelse(is.na(cl_value), "NA", sprintf("%.2f", cl_value)),
      ifelse(is.na(target_value), "NA", sprintf("%.2f", target_value))
    ))
  }

  # Formatér labels med delt formatter ----
  label_cl <- NULL
  if (!is.na(cl_value)) {
    formatted_cl <- format_y_value(cl_value, y_axis_unit, y_range)
    label_cl <- create_responsive_label(
      header = "NUV. NIVEAU",
      value = formatted_cl,
      label_size = label_size
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
      label_size = label_size
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
  if (verbose || getOption("spc.debug.label_placement", FALSE)) {
    message(sprintf(
      "[LABEL_PLACEMENT] Calling add_right_labels_marquee - yA=%.3f, yB=%s, textA_length=%d, textB_length=%d",
      yA,
      ifelse(is.na(yB), "NA", sprintf("%.3f", yB)),
      nchar(textA),
      nchar(textB)
    ))
  }

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
      pad_top = 0.01, # Top padding
      pad_bot = 0.01, # Bottom padding
      pref_pos = c("under", "under"), # Default: placer under linjer
      priority = "A" # Beskyt CL label ved konflikter
    ),
    gpA = grid::gpar(col = "#009CE8"), # CL label farve (lyseblå)
    gpB = grid::gpar(col = "#565656"), # Target label farve (grå)
    label_size = label_size, # Label sizing (baseline = 6)
    viewport_width = viewport_width_inches, # Viewport width in inches (from clientData)
    viewport_height = viewport_height_inches, # Viewport height in inches (from clientData)
    verbose = verbose, # Print placement warnings
    debug_mode = debug_mode # Visual debugging
  )

  if (verbose || getOption("spc.debug.label_placement", FALSE)) {
    message("[LABEL_PLACEMENT] add_right_labels_marquee returned successfully")
  }

  return(plot_with_labels)
}
