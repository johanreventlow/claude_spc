# utils_add_right_labels_marquee.R
# Core label placement function using NPC-based collision avoidance
#
# Extracted from bfh_layout_reference_dev.R POC

#' Add right-aligned marquee labels med NPC-baseret placering
#'
#' Anvender marquee::geom_marquee for at placere to-linje labels ved højre kant
#' med intelligent collision avoidance.
#'
#' @param p ggplot object
#' @param yA numeric(1) y-værdi (data units) for label A (CL)
#' @param yB numeric(1) y-værdi (data units) for label B (Target)
#' @param textA,textB character marquee markup strings
#' @param params list of placement parameters
#' @param gpA,gpB grid::gpar styling
#' @param label_size numeric label size for responsive sizing (default 6, legacy baseline)
#' @param verbose logical print placement warnings
#' @param debug_mode logical add visual debug annotations
#' @return ggplot object med marquee labels
#'
#' @export
add_right_labels_marquee <- function(
    p,
    yA,
    yB,
    textA,
    textB,
    params = list(
      label_height_npc = NULL,  # Auto-beregnes
      gap_line = NULL,          # Auto-beregnes
      gap_labels = NULL,        # Auto-beregnes
      pad_top = 0.01,
      pad_bot = 0.01,
      pref_pos = c("under", "under"),
      priority = "A"
    ),
    gpA = grid::gpar(col = "#009CE8"),
    gpB = grid::gpar(col = "#565656"),
    label_size = 6,
    verbose = TRUE,
    debug_mode = FALSE
) {

  # Beregn responsive størrelser baseret på label_size (baseline = 6)
  scale_factor <- label_size / 6

  # PERFORMANCE: Load config ÉN gang i starten
  placement_cfg <- list()
  config_available <- FALSE

  if (exists("get_label_placement_config", mode = "function")) {
    placement_cfg <- get_label_placement_config()
    config_available <- TRUE
  } else if (exists("get_label_placement_param", mode = "function")) {
    placement_cfg <- list(
      marquee_lineheight = get_label_placement_param("marquee_lineheight"),
      marquee_size_factor = get_label_placement_param("marquee_size_factor"),
      pad_top = get_label_placement_param("pad_top"),
      pad_bot = get_label_placement_param("pad_bot")
    )
    config_available <- TRUE
  }

  # Hent marquee_lineheight fra config
  marquee_lineheight <- if (config_available && !is.null(placement_cfg$marquee_lineheight)) {
    placement_cfg$marquee_lineheight
  } else {
    0.9
  }

  # PERFORMANCE: Hent cached right-aligned style
  right_aligned_style <- get_right_aligned_marquee_style(lineheight = marquee_lineheight)

  # PERFORMANCE: Build plot én gang
  built_plot <- ggplot2::ggplot_build(p)
  gtable <- ggplot2::ggplot_gtable(built_plot)

  # Detektér aktiv device størrelse for korrekt panel height measurement
  # FIX: Brug faktisk device size i stedet for hardcoded 7×7 inches
  device_size <- tryCatch({
    if (grDevices::dev.cur() > 1) {  # Device aktiv (ikke null device)
      dev_inches <- grDevices::dev.size("in")
      list(width = dev_inches[1], height = dev_inches[2])
    } else {
      # Ingen aktiv device - brug defaults
      list(width = 7, height = 7)
    }
  }, error = function(e) {
    # Fallback hvis dev.size() fejler
    list(width = 7, height = 7)
  })

  if (verbose) {
    message(sprintf("Device size: %.2f × %.2f inches",
                    device_size$width, device_size$height))
  }

  # Mål panel højde med faktisk device størrelse
  panel_height_inches <- tryCatch({
    measure_panel_height_from_gtable(
      gtable,
      device_width = device_size$width,
      device_height = device_size$height
    )
  }, error = function(e) {
    if (verbose) {
      message("Kunne ikke måle panel højde: ", e$message, " - bruger viewport fallback")
    }
    NULL
  })

  if (verbose && !is.null(panel_height_inches)) {
    message("Målt panel højde: ", round(panel_height_inches, 3), " inches")
  }

  # Auto-beregn label_height
  if (is.null(params$label_height_npc)) {
    heights <- estimate_label_heights_npc(
      texts = c(textA, textB),
      style = right_aligned_style,
      panel_height_inches = panel_height_inches,
      return_details = TRUE
    )
    height_A <- heights[[1]]
    height_B <- heights[[2]]

    if (height_A$npc > height_B$npc) {
      params$label_height_npc <- height_A
    } else {
      params$label_height_npc <- height_B
    }

    if (verbose) {
      message("Auto-beregnet label_height_npc: ", round(params$label_height_npc$npc, 4),
              " (A: ", round(height_A$npc, 4), ", B: ", round(height_B$npc, 4), ")",
              " [", round(params$label_height_npc$inches, 4), " inches]")
    }
  }

  # Default parameters
  if (is.null(params$pad_top)) {
    if (config_available && !is.null(placement_cfg$pad_top)) {
      params$pad_top <- placement_cfg$pad_top
    } else {
      params$pad_top <- 0.01
    }
  }

  if (is.null(params$pad_bot)) {
    if (config_available && !is.null(placement_cfg$pad_bot)) {
      params$pad_bot <- placement_cfg$pad_bot
    } else {
      params$pad_bot <- 0.01
    }
  }

  if (is.null(params$pref_pos)) params$pref_pos <- c("under", "under")
  if (is.null(params$priority)) params$priority <- "A"

  # Build mapper
  mapper <- npc_mapper_from_built(built_plot, original_plot = p)

  # Konverter y-værdier til NPC
  yA_npc <- if (!is.na(yA)) mapper$y_to_npc(yA) else NA_real_
  yB_npc <- if (!is.na(yB)) mapper$y_to_npc(yB) else NA_real_

  # Compute placement
  placement <- place_two_labels_npc(
    yA_npc = yA_npc,
    yB_npc = yB_npc,
    label_height_npc = params$label_height_npc,
    gap_line = params$gap_line,
    gap_labels = params$gap_labels,
    pad_top = params$pad_top,
    pad_bot = params$pad_bot,
    priority = params$priority,
    pref_pos = params$pref_pos,
    debug = debug_mode
  )

  # Print warnings
  if (verbose && length(placement$warnings) > 0) {
    message("Label placement warnings:")
    for (w in placement$warnings) {
      message("  - ", w)
    }
    message("Placement quality: ", placement$placement_quality)
  }

  # Konverter NPC positions til data coordinates
  if (!is.na(placement$yA)) {
    yA_data <- mapper$npc_to_y(placement$yA)
  } else {
    yA_data <- NA_real_
  }

  if (!is.na(placement$yB)) {
    yB_data <- mapper$npc_to_y(placement$yB)
  } else {
    yB_data <- NA_real_
  }

  # Hent x-koordinater
  x_range <- built_plot$layout$panel_params[[1]]$x.range
  x_max <- as.POSIXct(x_range[2], origin = "1970-01-01", tz = "UTC")

  # Opret label data
  label_data <- tibble::tibble(
    x = as.POSIXct(character()),
    y = numeric(),
    label = character(),
    color = character()
  )

  color_A <- if (!is.null(gpA$col)) gpA$col else "#009CE8"
  color_B <- if (!is.null(gpB$col)) gpB$col else "#565656"

  if (!is.na(yA_data)) {
    label_data <- label_data %>%
      dplyr::bind_rows(tibble::tibble(
        x = x_max,
        y = yA_data,
        label = textA,
        color = color_A,
        vjust = 0.5
      ))
  }

  if (!is.na(yB_data)) {
    label_data <- label_data %>%
      dplyr::bind_rows(tibble::tibble(
        x = x_max,
        y = yB_data,
        label = textB,
        color = color_B,
        vjust = 0.5
      ))
  }

  # Skalerede størrelser
  marquee_size_factor <- if (config_available && !is.null(placement_cfg$marquee_size_factor)) {
    placement_cfg$marquee_size_factor
  } else {
    6
  }
  marquee_size <- marquee_size_factor * scale_factor

  # Tilføj labels
  result <- p
  if (nrow(label_data) > 0) {
    result <- result +
      marquee::geom_marquee(
        data = label_data,
        ggplot2::aes(x = x, y = y, label = label, color = color, vjust = vjust),
        hjust = 1,
        style = right_aligned_style,
        size = marquee_size,
        lineheight = marquee_lineheight,
        family = "Roboto Medium",
        inherit.aes = FALSE
      ) +
      ggplot2::scale_color_identity()
  }

  # Attach metadata
  attr(result, "placement_info") <- placement
  attr(result, "mapper_info") <- list(
    limits = mapper$limits,
    trans_name = mapper$trans_name
  )

  result
}
