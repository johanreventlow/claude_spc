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
    debug_mode = FALSE) {
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
    message(
      "y_axis_unit '", y_axis_unit, "' ikke standard. Understøttede: ",
      paste(valid_units, collapse = ", ")
    )
  }

  # DEVICE READY CHECK (Phase 2 Fix) ----
  # Graceful degradation: Returner plot uden labels hvis device ikke er klar.
  # Dette forhindrer forkert label placement ved første render hvor viewport
  # muligvis ikke er fuldt initialiseret (trods viewport guard i server).
  #
  # LEVEL 1: Check om device er åbent
  device_open <- tryCatch(
    {
      grDevices::dev.cur() > 1
    },
    error = function(e) FALSE
  )

  # DEBUG: Always log device status
  if (verbose || getOption("spc.debug.label_placement", FALSE)) {
    message(sprintf(
      "[DEVICE_CHECK] Device open: %s (dev.cur = %d)",
      device_open,
      grDevices::dev.cur()
    ))
  }

  if (!device_open) {
    if (verbose || getOption("spc.debug.label_placement", FALSE)) {
      message("[DEVICE_CHECK] BLOCKING: No graphics device open - deferring label placement")
    }
    return(plot) # Graceful degradation: plot uden labels
  }

  # LEVEL 2: Check om device size er realistisk (ikke NULL eller ekstreme værdier)
  device_size <- tryCatch(
    {
      dev_size <- grDevices::dev.size("in")
      list(width = dev_size[1], height = dev_size[2])
    },
    error = function(e) NULL
  )

  # ADJUSTED THRESHOLD: Accepter device sizes ned til 2 inches (permissive)
  # Dette tillader conservative defaults (800×600 @ 96dpi ≈ 8.3×6.25 inches)
  # men afviser ekstreme værdier (< 2" eller > 50")
  device_ready <- !is.null(device_size) &&
    device_size$height >= 2 && # Permissive minimum (accepter small plots)
    device_size$height <= 50 && # Maximum realistic height
    device_size$width >= 2 # Permissive minimum

  # DEBUG: Always log device size check
  if (verbose || getOption("spc.debug.label_placement", FALSE)) {
    message(sprintf(
      "[DEVICE_SIZE] Width: %.1f\" (>= 2: %s), Height: %.1f\" (>= 2: %s, <= 50: %s)",
      device_size$width %||% 0,
      !is.null(device_size) && device_size$width >= 2,
      device_size$height %||% 0,
      !is.null(device_size) && device_size$height >= 2,
      !is.null(device_size) && device_size$height <= 50
    ))
    message(sprintf(
      "[DEVICE_SIZE] Ready: %s",
      device_ready
    ))
  }

  if (!device_ready) {
    if (verbose || getOption("spc.debug.label_placement", FALSE)) {
      message(sprintf(
        "[DEVICE_SIZE] BLOCKING: Device not ready (%.1f×%.1f inches) - deferring label placement",
        device_size$width %||% 0,
        device_size$height %||% 0
      ))
    }
    return(plot) # Graceful degradation: plot uden labels
  }

  # DEVICE ER KLAR: Fortsæt med normal label placement
  if (verbose || getOption("spc.debug.label_placement", FALSE)) {
    message(sprintf(
      "[DEVICE_SIZE] ✓ Device ready: %.1f×%.1f inches - proceeding with label placement",
      device_size$width,
      device_size$height
    ))
  }

  # FIX: Auto-scale label_size baseret på device height
  # Dette sikrer at labels skalerer proportionelt med plot størrelse
  # Baseline: label_size = 6 for ~7.8" device height (small plot reference)
  device_height_baseline <- 7.8 # inches (reference: 751px @ 96dpi)

  tryCatch(
    {
      if (grDevices::dev.cur() > 1) { # Device aktiv
        dev_height <- grDevices::dev.size("in")[2]

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
      }
    },
    error = function(e) {
      # Fallback: brug original label_size hvis device detection fejler
      if (verbose) {
        message("Device height detection fejlede - bruger fast label_size: ", label_size)
      }
    }
  )

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
    verbose = verbose, # Print placement warnings
    debug_mode = debug_mode # Visual debugging
  )

  return(plot_with_labels)
}
