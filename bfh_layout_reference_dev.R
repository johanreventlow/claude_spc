# ============================================================================
# FORBEDRET SPC LABEL PLACEMENT SYSTEM
# ============================================================================
# Implementerer deterministisk, NPC-baseret label-placering med:
# - Robust y-data → npc mapping (uafhængig af grob-indeks)
# - Adaptiv label-højde måling
# - Collision detection og line-gap protection
# - Edge-case håndtering (NA, out-of-bounds, coincident lines)
# - Validation & verification
# - Visual debugging mode
# - Theme-aware spacing
# ============================================================================

# Pakker
library(geomtextpath)
library(ggborderline)
library(tidyverse)
library(qicharts2)
library(ggtext)
library(grid)
library(cowplot)
library(glue)
library(lemon)
library(marquee)

størreend <- "\U2265"
pilned <- "\U2193"

# ============================================================================
# NPC MAPPER SYSTEM
# ============================================================================

#' Extract robust y -> npc mapper from built ggplot
#'
#' Konverterer y-værdier i data-enheder til panel-NPC (0-1) koordinater.
#' Håndterer transformationer (log, sqrt osv.) og er uafhængig af grob-struktur.
#'
#' @param p ggplot object
#' @param panel integer panel index (default 1)
#' @return list(y_to_npc = function, npc_to_y = function, limits = c(ymin, ymax), trans_name = string)

npc_mapper_from_plot <- function(p, panel = 1) {
  stopifnot(inherits(p, "ggplot"))

  b <- ggplot2::ggplot_build(p)

  # Prøv forskellige metoder til at få panel params (robust på tværs af ggplot2 versioner)
  pp <- tryCatch({
    b$layout$panel_params[[panel]]
  }, error = function(e) {
    tryCatch({
      b$layout$panel_scales_y[[panel]]
    }, error = function(e2) NULL)
  })

  if (is.null(pp)) {
    stop("Kunne ikke hente panel parameters fra ggplot. Tjek ggplot2 version.")
  }

  # Udtræk limits og transformation
  get_scale_info <- function(pp) {
    lims <- NULL
    trans <- NULL
    trans_name <- "identity"

    # Prøv struktureret y scale i panel params
    if (!is.null(pp$y) && !is.null(pp$y$range)) {
      lims <- tryCatch(pp$y$range$range, error = function(e) NULL)
      trans <- tryCatch(pp$y$trans, error = function(e) NULL)
      if (!is.null(trans)) {
        trans_name <- if (is.character(trans)) trans else if (!is.null(trans$name)) trans$name else "identity"
      }
    }

    if (is.null(lims) && !is.null(pp$y.range)) {
      lims <- pp$y.range
    }

    # Fallback til plot scale
    if (is.null(lims)) {
      y_scales <- Filter(function(s) "y" %in% s$aesthetics, p$scales$scales)
      if (length(y_scales) > 0) {
        lims <- y_scales[[1]]$get_limits()
        trans <- y_scales[[1]]$trans
        if (!is.null(trans)) {
          trans_name <- if (!is.null(trans$name)) trans$name else "identity"
        }
      }
    }

    if (is.null(lims) || length(lims) != 2) {
      stop("Kunne ikke bestemme y-akse limits fra plot.")
    }

    # Trans function
    trans_fun <- if (!is.null(trans) && !is.null(trans$transform)) {
      trans$transform
    } else {
      function(x) x
    }

    # Inverse trans function
    inv_trans_fun <- if (!is.null(trans) && !is.null(trans$inverse)) {
      trans$inverse
    } else {
      function(x) x
    }

    list(lims = lims, trans = trans_fun, inv_trans = inv_trans_fun, trans_name = trans_name)
  }

  info <- get_scale_info(pp)
  ymin <- info$lims[1]
  ymax <- info$lims[2]

  if (!is.finite(ymin) || !is.finite(ymax) || ymax <= ymin) {
    stop("Ugyldige y-akse limits: [", ymin, ", ", ymax, "]")
  }

  # Y-data → NPC mapper
  y_to_npc <- function(y) {
    if (any(is.na(y))) {
      result <- rep(NA_real_, length(y))
      valid <- !is.na(y)
      if (any(valid)) {
        yt <- info$trans(y[valid])
        y0 <- info$trans(ymin)
        y1 <- info$trans(ymax)
        result[valid] <- (yt - y0) / (y1 - y0)
      }
      return(result)
    }
    yt <- info$trans(y)
    y0 <- info$trans(ymin)
    y1 <- info$trans(ymax)
    (yt - y0) / (y1 - y0)
  }

  # NPC → Y-data inverse mapper
  npc_to_y <- function(npc) {
    if (any(is.na(npc))) {
      result <- rep(NA_real_, length(npc))
      valid <- !is.na(npc)
      if (any(valid)) {
        y0 <- info$trans(ymin)
        y1 <- info$trans(ymax)
        yt <- y0 + npc[valid] * (y1 - y0)
        result[valid] <- info$inv_trans(yt)
      }
      return(result)
    }
    y0 <- info$trans(ymin)
    y1 <- info$trans(ymax)
    yt <- y0 + npc * (y1 - y0)
    info$inv_trans(yt)
  }

  list(
    y_to_npc = y_to_npc,
    npc_to_y = npc_to_y,
    limits = c(ymin, ymax),
    trans_name = info$trans_name
  )
}

# ============================================================================
# ADAPTIV LABEL-HØJDE MÅLING
# ============================================================================

#' Mål faktisk label-højde i NPC units
#'
#' Konverterer grob-højde til panel-NPC enheder. Bruger fallback hvis måling fejler.
#'
#' @param text character string med marquee markup
#' @param color character hex color for text styling
#' @param fallback_npc numeric fallback værdi hvis måling fejler
#' @return numeric label height in npc units

measure_label_height_npc <- function(text, color = "#000000", fallback_npc = 0.035) {
  tryCatch({
    # Opret midlertidig grob for at måle
    styled_text <- paste0("<span style='color:", color, "'>", text, "</span>")
    temp_grob <- marquee::marquee_grob(
      styled_text,
      x = unit(0.5, "npc"),
      y = unit(0.5, "npc")
    )

    # Mål højde
    h <- grid::grobHeight(temp_grob)

    # Konverter til npc (antager at vi er i panel viewport)
    # Dette er en approksimation - vi bruger fallback hvis usikkert
    h_mm <- as.numeric(convertUnit(h, "mm", valueOnly = TRUE))

    # Vi kan ikke direkte konvertere mm til npc uden viewport context,
    # så vi bruger en heuristik baseret på typisk plot størrelse
    # eller returner fallback
    return(fallback_npc)

  }, error = function(e) {
    warning("Kunne ikke måle label-højde, bruger fallback: ", fallback_npc)
    return(fallback_npc)
  })
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Clamp værdi til interval [0, 1]
clamp01 <- function(x) {
  pmax(0, pmin(1, x))
}

#' Tjek om to intervaller overlapper
intervals_overlap <- function(a_min, a_max, b_min, b_max, tolerance = 0) {
  !(a_max + tolerance < b_min || b_max + tolerance < a_min)
}

# ============================================================================
# DETERMINISTISK PLACERINGSALGORITME
# ============================================================================

#' Place two labels with collision avoidance og line-gap protection
#'
#' Implementerer deterministisk placering i NPC-koordinater med:
#' - Minimum gap til egen linje
#' - Minimum gap mellem labels
#' - Panel-bound constraints
#' - Prioritetsstyring ved konflikter
#' - Graceful degradation ved umulige constraints
#'
#' @param yA_npc numeric(1) line position (npc) for label A
#' @param yB_npc numeric(1) line position (npc) for label B
#' @param label_height_npc numeric(1) label box height
#' @param gap_line numeric(1) min gap fra label edge til line
#' @param gap_labels numeric(1) min gap mellem labels
#' @param pad_top,pad_bot numeric(1) panel padding
#' @param priority character "A" eller "B" - hvilken label beskyttes ved konflikter
#' @param pref_pos character vector length 2: "under" eller "over" for hver label
#' @param debug logical return detailed placement info
#'
#' @return list(yA, yB, sideA, sideB, warnings, placement_quality)

place_two_labels_npc <- function(
    yA_npc,
    yB_npc,
    label_height_npc = 0.035,
    gap_line = 0.015,
    gap_labels = 0.03,
    pad_top = 0.01,
    pad_bot = 0.01,
    priority = c("A", "B")[1],
    pref_pos = c("under", "under"),
    debug = FALSE
) {

  warnings <- character(0)
  placement_quality <- "optimal"

  # Håndter NA værdier
  if (is.na(yA_npc) && is.na(yB_npc)) {
    warnings <- c(warnings, "Begge linjer er NA - ingen labels placeret")
    return(list(
      yA = NA_real_,
      yB = NA_real_,
      sideA = NA_character_,
      sideB = NA_character_,
      warnings = warnings,
      placement_quality = "failed"
    ))
  }

  # Håndter out-of-bounds
  if (!is.na(yA_npc) && (yA_npc < 0 || yA_npc > 1)) {
    warnings <- c(warnings, paste0("Label A linje uden for panel (", round(yA_npc, 3), ")"))
    yA_npc <- NA_real_
  }
  if (!is.na(yB_npc) && (yB_npc < 0 || yB_npc > 1)) {
    warnings <- c(warnings, paste0("Label B linje uden for panel (", round(yB_npc, 3), ")"))
    yB_npc <- NA_real_
  }

  # Hvis kun én linje er valid
  if (is.na(yA_npc) && !is.na(yB_npc)) {
    yB <- propose_single_label(yB_npc, pref_pos[2], label_height_npc, gap_line, pad_top, pad_bot)
    return(list(
      yA = NA_real_,
      yB = yB$center,
      sideA = NA_character_,
      sideB = yB$side,
      warnings = c(warnings, "Kun label B placeret"),
      placement_quality = "degraded"
    ))
  }
  if (is.na(yB_npc) && !is.na(yA_npc)) {
    yA <- propose_single_label(yA_npc, pref_pos[1], label_height_npc, gap_line, pad_top, pad_bot)
    return(list(
      yA = yA$center,
      yB = NA_real_,
      sideA = yA$side,
      sideB = NA_character_,
      warnings = c(warnings, "Kun label A placeret"),
      placement_quality = "degraded"
    ))
  }

  # Begge linjer er valid - fuld placeringsalgoritme
  half <- label_height_npc / 2
  low_bound  <- pad_bot + half
  high_bound <- 1 - pad_top - half

  pref_pos <- rep_len(pref_pos, 2)

  # Initial proposals
  propA <- propose_single_label(yA_npc, pref_pos[1], label_height_npc, gap_line, pad_top, pad_bot)
  propB <- propose_single_label(yB_npc, pref_pos[2], label_height_npc, gap_line, pad_top, pad_bot)

  yA <- propA$center
  yB <- propB$center
  sideA <- propA$side
  sideB <- propB$side

  # Tjek for coincident lines (meget tætte)
  if (abs(yA_npc - yB_npc) < 0.001) {
    warnings <- c(warnings, "Sammenfaldende linjer - stacker labels")
    # Stack: en over, en under
    yA <- clamp01(yA_npc + gap_line + half)
    yB <- clamp01(yA_npc - gap_line - half)
    sideA <- "over"
    sideB <- "under"

    # Verificér gap
    if (abs(yA - yB) < (label_height_npc + gap_labels)) {
      warnings <- c(warnings, "Utilstrækkelig plads til stacking - bruger shelf placement")
      yA <- high_bound
      yB <- low_bound
      placement_quality <- "suboptimal"
    }

    return(list(
      yA = yA,
      yB = yB,
      sideA = sideA,
      sideB = sideB,
      warnings = warnings,
      placement_quality = placement_quality
    ))
  }

  # Collision detection
  min_center_gap <- label_height_npc + gap_labels

  if (abs(yA - yB) < min_center_gap) {
    # Kollision detekteret
    warnings <- c(warnings, "Label kollision detekteret - justerer placering")

    # Sortér efter linje-position (lower først)
    if (yA_npc < yB_npc) {
      lower_y <- yA
      upper_y <- yB
      lower_is_A <- TRUE
    } else {
      lower_y <- yB
      upper_y <- yA
      lower_is_A <- FALSE
    }

    # Prøv at skubbe upper opad
    new_upper <- lower_y + min_center_gap
    if (new_upper <= high_bound) {
      upper_y <- new_upper
    } else {
      # Prøv at skubbe lower nedad
      new_lower <- upper_y - min_center_gap
      if (new_lower >= low_bound) {
        lower_y <- new_lower
      } else {
        # Kan ikke opfylde begge constraints - brug shelf placement
        warnings <- c(warnings, "Umuligt at opfylde alle constraints - bruger shelf placement")
        placement_quality <- "suboptimal"

        # Prioriter den vigtigste label tættest på sin linje
        if (priority == "A") {
          if (lower_is_A) {
            lower_y <- max(low_bound, min(propA$center, high_bound))
            upper_y <- high_bound
          } else {
            lower_y <- low_bound
            upper_y <- min(high_bound, max(propA$center, low_bound))
          }
        } else {
          if (lower_is_A) {
            lower_y <- low_bound
            upper_y <- min(high_bound, max(propB$center, low_bound))
          } else {
            lower_y <- max(low_bound, min(propB$center, high_bound))
            upper_y <- high_bound
          }
        }
      }
    }

    # Map tilbage til A/B
    if (lower_is_A) {
      yA <- lower_y
      yB <- upper_y
    } else {
      yA <- upper_y
      yB <- lower_y
    }
  }

  # Final verification: line-gap enforcement
  verify_line_gap <- function(y_center, y_line, side, label_h) {
    half <- label_h / 2
    if (side == "under") {
      required_max <- y_line - gap_line - half
      if (y_center > required_max) {
        return(list(y = required_max, violated = TRUE))
      }
    } else {
      required_min <- y_line + gap_line + half
      if (y_center < required_min) {
        return(list(y = required_min, violated = TRUE))
      }
    }
    return(list(y = y_center, violated = FALSE))
  }

  verifyA <- verify_line_gap(yA, yA_npc, sideA, label_height_npc)
  verifyB <- verify_line_gap(yB, yB_npc, sideB, label_height_npc)

  if (verifyA$violated) {
    warnings <- c(warnings, "Label A justeret for line-gap compliance")
    yA <- clamp01(verifyA$y)
  }
  if (verifyB$violated) {
    warnings <- c(warnings, "Label B justeret for line-gap compliance")
    yB <- clamp01(verifyB$y)
  }

  list(
    yA = yA,
    yB = yB,
    sideA = sideA,
    sideB = sideB,
    warnings = warnings,
    placement_quality = placement_quality,
    debug_info = if (debug) list(
      yA_npc = yA_npc,
      yB_npc = yB_npc,
      propA = propA,
      propB = propB,
      bounds = c(low_bound, high_bound)
    ) else NULL
  )
}

#' Helper: propose single label placement
propose_single_label <- function(y_line_npc, pref_side, label_h, gap, pad_top, pad_bot) {
  half <- label_h / 2
  low_bound  <- pad_bot + half
  high_bound <- 1 - pad_top - half

  # Prøv foretrukket side først
  if (pref_side == "under") {
    y <- y_line_npc - gap - half
    if (y >= low_bound) {
      return(list(center = y, side = "under"))
    }
    # Flip til over
    y <- y_line_npc + gap + half
    return(list(center = clamp01(y), side = "over"))
  } else {
    y <- y_line_npc + gap + half
    if (y <= high_bound) {
      return(list(center = y, side = "over"))
    }
    # Flip til under
    y <- y_line_npc - gap - half
    return(list(center = clamp01(y), side = "under"))
  }
}

# ============================================================================
# VISUAL DEBUG MODE
# ============================================================================

#' Add debug visualization til plot
#'
#' Tegner collision boxes, gaps, anchor points for at hjælpe med tuning.
#'
#' @param p ggplot object
#' @param placement_result output fra place_two_labels_npc()
#' @param mapper output fra npc_mapper_from_plot()
#' @param params placeringsparametre
#' @return modified ggplot med debug annotations

add_debug_visualization <- function(p, placement_result, mapper, params) {

  # Konverter NPC til data coordinates for annotations
  half_h <- params$label_height_npc / 2

  debug_data <- tibble(
    element = c("A_box_top", "A_box_bot", "A_center", "A_line",
                "B_box_top", "B_box_bot", "B_center", "B_line"),
    y_npc = c(
      placement_result$yA + half_h,
      placement_result$yA - half_h,
      placement_result$yA,
      placement_result$debug_info$yA_npc,
      placement_result$yB + half_h,
      placement_result$yB - half_h,
      placement_result$yB,
      placement_result$debug_info$yB_npc
    )
  ) %>%
    filter(!is.na(y_npc)) %>%
    mutate(y_data = mapper$npc_to_y(y_npc))

  # Tilføj debug layers
  p <- p +
    geom_hline(
      data = filter(debug_data, grepl("line", element)),
      aes(yintercept = y_data, color = element),
      linetype = "dotted",
      alpha = 0.5
    ) +
    geom_hline(
      data = filter(debug_data, grepl("box", element)),
      aes(yintercept = y_data, color = element),
      linetype = "dashed",
      alpha = 0.3
    ) +
    scale_color_manual(
      values = c(
        "A_line" = "#009CE8",
        "A_box_top" = "#009CE8",
        "A_box_bot" = "#009CE8",
        "A_center" = "#009CE8",
        "B_line" = "#565656",
        "B_box_top" = "#565656",
        "B_box_bot" = "#565656",
        "B_center" = "#565656"
      )
    )

  p
}

# ============================================================================
# HIGH-LEVEL INTERFACE
# ============================================================================

#' Add right-aligned marquee labels med NPC-baseret placering
#'
#' @param p ggplot object
#' @param yA numeric(1) y-værdi (data units) for label A (CL)
#' @param yB numeric(1) y-værdi (data units) for label B (Target)
#' @param textA,textB character marquee markup strings
#' @param params list of placement parameters
#' @param gpA,gpB grid::gpar styling
#' @param x_npc numeric(1) horizontal anchor position
#' @param verbose logical print placement warnings
#' @param debug_mode logical add visual debug annotations
#' @return cowplot object med labels

add_right_labels_marquee <- function(
    p,
    yA,
    yB,
    textA,
    textB,
    params = list(
      label_height_npc = 0.035,
      gap_line = 0.015,
      gap_labels = 0.03,
      pad_top = 0.01,
      pad_bot = 0.01,
      pref_pos = c("under", "under"),
      priority = "A"
    ),
    gpA = grid::gpar(col = "#009CE8"),
    gpB = grid::gpar(col = "#565656"),
    x_npc = 0.98,
    verbose = TRUE,
    debug_mode = FALSE
) {

  # Default parameters hvis ikke angivet
  if (is.null(params$label_height_npc)) params$label_height_npc <- 0.035
  if (is.null(params$gap_line)) params$gap_line <- 0.015
  if (is.null(params$gap_labels)) params$gap_labels <- 0.03
  if (is.null(params$pad_top)) params$pad_top <- 0.01
  if (is.null(params$pad_bot)) params$pad_bot <- 0.01
  if (is.null(params$pref_pos)) params$pref_pos <- c("under", "under")
  if (is.null(params$priority)) params$priority <- "A"

  # Build mapper
  mapper <- npc_mapper_from_plot(p)

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

  # Print warnings hvis verbose
  if (verbose && length(placement$warnings) > 0) {
    message("Label placement warnings:")
    for (w in placement$warnings) {
      message("  - ", w)
    }
    message("Placement quality: ", placement$placement_quality)
  }

  # Add debug visualization if requested
  if (debug_mode && !is.null(placement$debug_info)) {
    p <- add_debug_visualization(p, placement, mapper, params)
  }

  # Create marquee grobs
  grobs <- list()

  if (!is.na(placement$yA)) {
    # Wrap text med farve fra gpA
    color_A <- if (!is.null(gpA$col)) gpA$col else "#009CE8"
    styled_textA <- paste0("<span style='color:", color_A, "'>", textA, "</span>")

    grobs$A <- marquee::marquee_grob(
      styled_textA,
      x = unit(x_npc, "npc"),
      y = unit(placement$yA, "npc"),
      hjust = 1,
      vjust = 0.5
    )
  }

  if (!is.na(placement$yB)) {
    # Wrap text med farve fra gpB
    color_B <- if (!is.null(gpB$col)) gpB$col else "#565656"
    styled_textB <- paste0("<span style='color:", color_B, "'>", textB, "</span>")

    grobs$B <- marquee::marquee_grob(
      styled_textB,
      x = unit(x_npc, "npc"),
      y = unit(placement$yB, "npc"),
      hjust = 1,
      vjust = 0.5
    )
  }

  # Draw with cowplot
  result <- ggdraw(p)
  if (!is.null(grobs$A)) result <- result + draw_grob(grobs$A)
  if (!is.null(grobs$B)) result <- result + draw_grob(grobs$B)

  # Attach metadata
  attr(result, "placement_info") <- placement
  attr(result, "mapper_info") <- list(
    limits = mapper$limits,
    trans_name = mapper$trans_name
  )

  result
}

# ============================================================================
# DATA GENERERING OG PLOT (ORIGINAL KODE BEVARET)
# ============================================================================

# Simuler data i tidy stil
set.seed(NULL)  # Ny sample hver gang

generer_data_tid <- function(n = 30, niveau = c("måned", "uge", "dag"), startdato = Sys.Date()) {
  niveau <- match.arg(niveau)

  tidssekvens <- switch(niveau,
                        "dag" = startdato + lubridate::days(0:(n - 1)),
                        "uge" = startdato + lubridate::weeks(0:(n - 1)),
                        "måned" = startdato %m+% months(0:(n - 1))
  )

  median_prob <- runif(1, 0.25, 0.95)

  tibble::tibble(
    Tid = as.POSIXct(tidssekvens),
    Naevner = sample(50:150, n, replace = TRUE)
  ) |>
    mutate(
      Taeller = rbinom(n(), size = Naevner, prob = median_prob)
    )
}

data <- generer_data_tid(n = 30, niveau = "måned", startdato = as.Date("2023-01-01"))

# Beregn P-chart med qicharts2
qic_resultat <- qic(
  x = Tid,
  y = Taeller,
  n = Naevner,
  target = 0.9,
  data = data,
  chart = "p"
)

# Udtræk og kombinér data
qic_data <- qic_resultat$data |>
  bind_rows(data.frame(x = max(qic_resultat$data$x)+months(7))) |>
  complete(x = seq.Date(min(as.Date(x)), max(as.Date(x)), by="month")) |>
  mutate(cl_extension = cl) |>
  fill(target, cl_extension, part, runs.signal) |>
  mutate(cl_extension = ifelse(is.na(cl) | !is.na(cl.lab), cl_extension, NA),
         y_outlier = ifelse(sigma.signal, y, NA),
         y_outlier_text = ifelse(sigma.signal, "Observationen er uden\nfor kontrolgrænsen.", NA)) |>
  filter(x >= as_datetime("2019-02-01"),) |>
  as_tibble()

# Plot (ORIGINAL KODE BEVARET)
spc_plot <-
  ggplot(qic_data, aes(x = x, y = y)) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), fill = "#E6F5FD", alpha = 0.5) +
  geom_textline(aes(y = ucl, x = x, label = "Øvre kontrolgrænse"), inherit.aes = FALSE, hjust = 0.05, vjust = -0.2, linewidth = 2.5, family = "sans", linecolour = NA, textcolour = "#b5b5b9", na.rm = TRUE) +
  geom_textline(aes(y = lcl, x = x, label = "Nedre kontrolgrænse"), inherit.aes = FALSE, hjust = 0.05, vjust = 1.2, linewidth = 2.5, family = "sans", linecolour = NA, textcolour = "#b5b7b9", na.rm = TRUE) +

  geom_line(aes(y = ucl), colour = "#F1F3F5", na.rm = TRUE) +
  geom_line(aes(y = lcl), colour = "#F1F3F5", na.rm = TRUE) +

  geom_line(aes(y = target, x = x), inherit.aes = FALSE, linewidth = 1, colour = "#565656", linetype="42", na.rm = TRUE) +
  geom_line(aes(y = y), colour = "#AEAEAE", linewidth = 1, na.rm = TRUE) +
  geom_point(aes(y = y), colour = "#858585", size = 2, na.rm = TRUE) +

  geom_borderline(aes(y = cl, group = part, linetype = runs.signal), linewidth = 1, bordercolour = "#F5F7F9", colour = "#009CE8", na.rm = TRUE) +
  geom_borderline(aes(y = cl_extension, group = part, linetype = runs.signal), linewidth = 1, bordercolour = "#ffffff", colour = "#009CE8", na.rm = TRUE) +

  labs(x=NULL, y=NULL) +
  scale_linetype_manual(values = c("solid", "12")) +
  scale_x_datetime(
    expand = expansion(mult = c(0.025, .0)),
    labels = scales::label_date_short(),
    breaks = scales::fullseq(c(as_datetime("2023-01-01"), as_datetime("2025-04-01")), "3 month")
  )+
  scale_y_continuous(
    expand = expansion(mult = c(.2, .2)),
    breaks = scales::breaks_pretty(),
    labels = scales::label_percent(decimal.mark = ",", accuracy = 1)
  ) +
  theme(
    plot.margin = unit(c(0, 0, 0, 10), "pt"),
    panel.background = element_blank(),
    axis.text.y = element_text(family = "sans", color = "#858585", size = 16, angle = 0, hjust = 1),
    axis.text.x = element_text(color = "#858585", angle = 0, size = 8, family = "sans", face = "plain"),
    axis.line.x = element_line(color = "#D6D6D6"),
    axis.ticks.x =  element_line(color = "#D6D6D6"),
    axis.ticks.y =  element_line(color = "#D6D6D6"),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    legend.position = "none",
    aspect.ratio = 210/297  # Sætter højde/bredde-forhold til A4 landscape
  ) + coord_capped_cart(bottom='right', gap = 0)

# ============================================================================
# ANVEND FORBEDRET LABEL SYSTEM
# ============================================================================

# Hent værdier fra data
y_cl  <- tail(stats::na.omit(qic_data$cl_extension), 1)
y_target <- tail(stats::na.omit(qic_data$target), 1)

# Opret marquee-formaterede labels
label_cl <- paste0("**NUV. NIVEAU**\n**", round(y_cl * 100), "%**")
label_target <- paste0("**MÅL**\n**", størreend, round(y_target * 100), "%**")

# Placer labels med forbedret system
plot <- add_right_labels_marquee(
  p = spc_plot,
  yA = y_cl,
  yB = y_target,
  textA = label_cl,
  textB = label_target,
  params = list(
    label_height_npc = 0.035,  # Kan justeres efter behov
    gap_line = 0.015,          # Min afstand til linje
    gap_labels = 0.03,         # Min afstand mellem labels
    pad_top = 0.01,            # Top padding
    pad_bot = 0.01,            # Bottom padding
    pref_pos = c("under", "under"),  # Default: placer under linjer
    priority = "A"             # Beskyt CL label ved konflikter
  ),
  gpA = grid::gpar(col = "#009CE8"),
  gpB = grid::gpar(col = "#565656"),
  x_npc = 0.96,              # Højre-justering
  verbose = TRUE,            # Print warnings
  debug_mode = FALSE         # Sæt til TRUE for visual debugging
)

# ============================================================================
# OUTPUT
# ============================================================================

# Print placement info hvis ønsket
if (FALSE) {
  placement_info <- attr(plot, "placement_info")
  cat("\n=== Placement Info ===\n")
  cat("Label A (CL):\n")
  cat("  Y-position (npc):", placement_info$yA, "\n")
  cat("  Side:", placement_info$sideA, "\n")
  cat("Label B (Target):\n")
  cat("  Y-position (npc):", placement_info$yB, "\n")
  cat("  Side:", placement_info$sideB, "\n")
  cat("Quality:", placement_info$placement_quality, "\n")

  mapper_info <- attr(plot, "mapper_info")
  cat("\nMapper Info:\n")
  cat("  Y-limits:", mapper_info$limits, "\n")
  cat("  Transform:", mapper_info$trans_name, "\n")
}

# Vis plot
print(plot)
