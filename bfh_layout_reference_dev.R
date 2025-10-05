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

#' Estimer label-højde i NPC units fra font-størrelser
#'
#' Beregner omtrentlig label-højde baseret på marquee font sizes.
#' Bruger heuristik: total_pt * 0.0004 (empirisk afledt).
#'
#' @param text character string med marquee markup (fx "{.8 text}\n{.24 text}")
#' @param base_size numeric base font size in pt (default 11)
#' @param fallback_npc numeric fallback værdi hvis parsing fejler
#' @return numeric label height in npc units
#'
#' @details
#' Udtrækker font-størrelser fra marquee {.size Xpt} tags og summerer.
#' Konvertering: pt → mm → npc (approx for standard plot dimensions)

estimate_label_height_npc <- function(text, base_size = 11, fallback_npc = 0.055) {
  tryCatch({
    # Udtræk font sizes fra marquee syntax: {.8 ...} eller {.size 8pt ...}
    # Match både {.8 ...} og {.size 8pt ...}
    sizes <- stringr::str_extract_all(text, "\\{\\.(?:size\\s+)?(\\d+)(?:pt)?")[[1]]

    if (length(sizes) == 0) {
      # Ingen sizes fundet, brug fallback
      return(fallback_npc)
    }

    # Udtræk numeriske værdier
    size_vals <- as.numeric(stringr::str_extract(sizes, "\\d+"))

    # Beregn total højde: sum af font sizes + line spacing
    # Heuristik: hver pt ≈ 0.3528mm, panel typisk ~140-200mm
    # 1 NPC ≈ 150mm (typisk), så pt_to_npc ≈ 0.3528/150 ≈ 0.0024
    # Men vi skal også have line spacing, så vi bruger empirisk factor
    total_pt <- sum(size_vals)

    # Empirisk afledt konvertering (konservativ for at undgå overlap):
    # - 8pt + 24pt = 32pt total → ~0.13 NPC (med line spacing + margin)
    # - Factor: 0.13/32 ≈ 0.004
    # Øget fra 0.0025 til 0.004 for bedre margin
    pt_to_npc_factor <- 0.0033

    estimated_height <- total_pt * pt_to_npc_factor

    # Tilføj linjeskift overhead hvis \n findes
    if (grepl("\\n", text)) {
      line_count <- length(gregexpr("\\n", text)[[1]]) + 1
      line_spacing_npc <- 0.015 * (line_count - 1)  # 1.5% per ekstra linje
      estimated_height <- estimated_height + line_spacing_npc
    }

    # Tilføj sikkerheds-margin for marquee rendering
    estimated_height <- estimated_height * 1.1  # 10% safety margin

    # Sikr rimelig værdi
    estimated_height <- max(0.02, min(0.3, estimated_height))

    return(estimated_height)

  }, error = function(e) {
    warning("Kunne ikke estimere label-højde fra font sizes, bruger fallback: ", fallback_npc)
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

  # Hvis linjer er meget tætte, flip strategy: en over, en under
  line_gap_npc <- abs(yA_npc - yB_npc)
  min_center_gap <- label_height_npc + gap_labels

  if (line_gap_npc < min_center_gap * 0.5) {
    # Linjer er for tætte til begge at være på samme side
    warnings <- c(warnings, paste0("Linjer meget tætte (gap=", round(line_gap_npc, 3), ") - bruger over/under strategi"))

    # Placer den øverste linje's label OVER, den nederste UNDER
    if (yA_npc > yB_npc) {
      # A er højere
      pref_pos[1] <- "over"   # A over
      pref_pos[2] <- "under"  # B under
    } else {
      # B er højere
      pref_pos[1] <- "under"  # A under
      pref_pos[2] <- "over"   # B over
    }
  }

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
  # MEN: Prioriter collision avoidance over line-gap hvis konflikt
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

  # Tjek om line-gap enforcement vil skabe ny collision
  if (verifyA$violated || verifyB$violated) {
    proposed_yA <- if (verifyA$violated) verifyA$y else yA
    proposed_yB <- if (verifyB$violated) verifyB$y else yB

    # Vil dette skabe collision?
    if (abs(proposed_yA - proposed_yB) < min_center_gap) {
      warnings <- c(warnings, "Line-gap enforcement ville skabe collision - forsøger multi-level fallback")

      # === NIVEAU 1: Reducer gap_labels for at give mere plads ===
      reduced_gap_successful <- FALSE
      for (reduction_factor in c(0.5, 0.3, 0.15)) {
        reduced_min_gap <- label_height_npc + gap_labels * reduction_factor

        if (abs(proposed_yA - proposed_yB) >= reduced_min_gap) {
          # Success! Vi kan bruge line-gap enforcement med reduceret label-gap
          warnings <- c(warnings, paste0("NIVEAU 1: Reduceret label gap til ",
                                         round(reduction_factor * 100), "% - line-gaps overholdt"))
          yA <- clamp01(proposed_yA)
          yB <- clamp01(proposed_yB)
          reduced_gap_successful <- TRUE
          placement_quality <- "acceptable"
          break
        }
      }

      # === NIVEAU 2: Flip label til modsatte side ===
      if (!reduced_gap_successful) {
        warnings <- c(warnings, "NIVEAU 1 fejlede - forsøger NIVEAU 2: flip labels til modsatte side")

        # Prøv begge flip-strategier i prioritetsrækkefølge
        # Strategi 1: Flip A (CL) til modsatte side, hold B (Target) fast
        new_side_A <- if (sideA == "under") "over" else "under"
        propA_flipped <- propose_single_label(yA_npc, new_side_A, label_height_npc, gap_line, pad_top, pad_bot)
        verifyA_flipped <- verify_line_gap(propA_flipped$center, yA_npc, propA_flipped$side, label_height_npc)
        test_yA <- if (verifyA_flipped$violated) verifyA_flipped$y else propA_flipped$center

        # Check om flip A løser problemet
        if (abs(test_yA - proposed_yB) >= label_height_npc) {  # Minimum: labels må ikke overlappe
          yA <- clamp01(test_yA)
          yB <- clamp01(proposed_yB)
          sideA <- propA_flipped$side
          warnings <- c(warnings, "NIVEAU 2a: Flippet label A til modsatte side - konflikt løst")
          placement_quality <- "acceptable"
          reduced_gap_successful <- TRUE
        } else {
          # Strategi 2: Hold A fast, flip B til modsatte side
          new_side_B <- if (sideB == "under") "over" else "under"
          propB_flipped <- propose_single_label(yB_npc, new_side_B, label_height_npc, gap_line, pad_top, pad_bot)
          verifyB_flipped <- verify_line_gap(propB_flipped$center, yB_npc, propB_flipped$side, label_height_npc)
          test_yB <- if (verifyB_flipped$violated) verifyB_flipped$y else propB_flipped$center

          if (abs(proposed_yA - test_yB) >= label_height_npc) {
            yA <- clamp01(proposed_yA)
            yB <- clamp01(test_yB)
            sideB <- propB_flipped$side
            warnings <- c(warnings, "NIVEAU 2b: Flippet label B til modsatte side - konflikt løst")
            placement_quality <- "acceptable"
            reduced_gap_successful <- TRUE
          } else {
            # Strategi 3: Flip BEGGE labels til modsatte side
            if (abs(test_yA - test_yB) >= label_height_npc) {
              yA <- clamp01(test_yA)
              yB <- clamp01(test_yB)
              sideA <- propA_flipped$side
              sideB <- propB_flipped$side
              warnings <- c(warnings, "NIVEAU 2c: Flippet BEGGE labels til modsatte side - konflikt løst")
              placement_quality <- "suboptimal"
              reduced_gap_successful <- TRUE
            }
          }
        }
      }

      # === NIVEAU 3: Shelf placement som sidste udvej ===
      if (!reduced_gap_successful) {
        warnings <- c(warnings, "NIVEAU 2 fejlede - bruger NIVEAU 3: shelf placement")

        # Prioriter den vigtigste label tættest på sin linje
        if (priority == "A") {
          yA <- clamp01(proposed_yA)
          yB <- if (yA < 0.5) high_bound else low_bound  # Modsatte shelf
        } else {
          yB <- clamp01(proposed_yB)
          yA <- if (yB < 0.5) high_bound else low_bound
        }
        placement_quality <- "degraded"
      }

    } else {
      # Sikkert at enforce line-gaps uden collision
      if (verifyA$violated) {
        warnings <- c(warnings, "Label A justeret for line-gap compliance")
        yA <- clamp01(verifyA$y)
      }
      if (verifyB$violated) {
        warnings <- c(warnings, "Label B justeret for line-gap compliance")
        yB <- clamp01(verifyB$y)
      }
    }
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
#' Anvender marquee::geom_marquee for at placere to-linje labels ved højre kant.
#'
#' @param p ggplot object
#' @param yA numeric(1) y-værdi (data units) for label A (CL)
#' @param yB numeric(1) y-værdi (data units) for label B (Target)
#' @param textA,textB character marquee markup strings med format:
#'   "{.size 12pt **Header**}\n{.size 36pt **Value**}"
#' @param params list of placement parameters
#' @param gpA,gpB grid::gpar styling (bruges til farve)
#' @param x_npc numeric(1) horizontal anchor position (ikke brugt med geom_marquee)
#' @param base_size numeric base font size for responsive sizing (default 14)
#' @param verbose logical print placement warnings
#' @param debug_mode logical add visual debug annotations
#' @return ggplot object med marquee labels
#'
#' @details
#' Marquee syntax eksempler:
#' - {.8 **text**} - 8pt bold text (kort notation)
#' - {.size 24pt text} - 24pt normal text (lang notation)
#' - {.align right} - Højrejuster tekst (left/center/right/justify)
#' - {.b text} - Fed tekst (shorthand for bold)
#' - <span style='color:#009CE8'>...</span> - Farvet text
#' - \n - Linjeskift
#'
#' Responsive sizing med base_size:
#' - `base_size = 14` er reference (standard)
#' - `marquee_size = 6 × (base_size/14)` - Skaleret label størrelse
#' - Label font sizes skal også skaleres i textA/textB markup
#' - Følger samme pattern som fct_spc_plot_generation.R
#'
#' Auto-beregning af parametre:
#' - `label_height_npc`: Beregnes fra font sizes hvis ikke angivet
#'   (fx {.8 ...}\n{.24 ...} → 0.13 NPC ved base_size=14)
#' - `gap_line`: Default 8% af label_height_npc (sikrer ingen overlap med linjer)
#' - `gap_labels`: Default 30% af label_height_npc
#'
#' Overskrivning:
#' Alle params kan manuelt overstyres ved at angive i params list.
#' Eks: params = list(label_height_npc = 0.12, gap_line = 0.02)
#'
#' @examples
#' # Med base_size = 14 (standard):
#' header_size <- 8; value_size <- 24
#' label <- sprintf("{.%d **Header**}\n{.%d **Value**}", header_size, value_size)
#'
#' # Med base_size = 20 (større plot):
#' scale <- 20/14
#' header_size <- round(8 * scale); value_size <- round(24 * scale)
#' # → {.11 **Header**}\n{.34 **Value**}

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
    x_npc = 0.98,
    base_size = 14,
    verbose = TRUE,
    debug_mode = FALSE
) {

  # Beregn responsive størrelser baseret på base_size
  # Reference: base_size 14 giver original størrelser
  scale_factor <- base_size / 14

  # Auto-beregn label_height hvis ikke angivet
  if (is.null(params$label_height_npc)) {
    # Estimer fra begge labels og tag max
    # NOTE: textA/textB er allerede skaleret med base_size via create_responsive_label()
    # så estimate_label_height_npc() parser de korrekte størrelser direkte
    height_A <- estimate_label_height_npc(textA)
    height_B <- estimate_label_height_npc(textB)
    params$label_height_npc <- max(height_A, height_B)

    if (verbose) {
      message("Auto-beregnet label_height_npc: ", round(params$label_height_npc, 4),
              " (A: ", round(height_A, 4), ", B: ", round(height_B, 4), ")")
    }
  }

  # Default parameters hvis ikke angivet
  # gap_line sættes til 8% for sikker placering ved linjer uden overlap
  # Balancerer mellem "tæt placering" og "ingen overlap med linjer"
  if (is.null(params$gap_line)) params$gap_line <- params$label_height_npc * 0.08  # 8% af label height
  if (is.null(params$gap_labels)) params$gap_labels <- params$label_height_npc * 0.3  # 30% af label height
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

  # Konverter NPC y-positions til data coordinates
  yA_data <- if (!is.na(placement$yA)) mapper$npc_to_y(placement$yA) else NA_real_
  yB_data <- if (!is.na(placement$yB)) mapper$npc_to_y(placement$yB) else NA_real_

  # Hent x-koordinater fra plottet
  # Find den maksimale x-værdi fra den underliggende data
  x_range <- ggplot_build(p)$layout$panel_params[[1]]$x.range
  x_max <- as.POSIXct(x_range[2], origin = "1970-01-01", tz = "UTC")

  # Opret label data frame med marquee-formateret text
  label_data <- tibble::tibble(
    x = as.POSIXct(character()),
    y = numeric(),
    label = character(),
    color = character()
  )

  color_A <- if (!is.null(gpA$col)) gpA$col else "#009CE8"
  color_B <- if (!is.null(gpB$col)) gpB$col else "#565656"

  if (!is.na(yA_data)) {
    # Marquee label uden color wrapper (farve sættes via color aesthetic)
    label_data <- label_data %>%
      bind_rows(tibble::tibble(
        x = x_max,
        y = yA_data,
        label = textA,
        color = color_A
      ))
  }

  if (!is.na(yB_data)) {
    label_data <- label_data %>%
      bind_rows(tibble::tibble(
        x = x_max,
        y = yB_data,
        label = textB,
        color = color_B
      ))
  }
  
  right_aligned_style <- marquee::modify_style(
    marquee::classic_style(),#
    "p",
    margin = marquee::trbl(0),
    align = "right"
  )

  # Skalerede størrelser for marquee labels
  marquee_size <- 6 * scale_factor

  # Tilføj labels med marquee::geom_marquee
  result <- p
  if (nrow(label_data) > 0) {
    result <- result +
      marquee::geom_marquee(
        data = label_data,
        aes(x = x, y = y, label = label, color = color),
        hjust = 1,
        vjust = 0.5,
        style = right_aligned_style,
        size = marquee_size,
        lineheight = 0.9,
        family = "Roboto Medium",
        inherit.aes = FALSE
      ) +
      scale_color_identity()  # Brug faktiske farveværdier fra color column
  }

  # Attach metadata
  attr(result, "placement_info") <- placement
  attr(result, "mapper_info") <- list(
    limits = mapper$limits,
    trans_name = mapper$trans_name
  )

  result
}

# ============================================================================
# HELPER FUNCTIONS FOR RESPONSIVE LABELS
# ============================================================================

#' Generer responsive marquee label med skalerede font-størrelser
#'
#' @param header character header text
#' @param value character value text
#' @param base_size numeric base font size (default 14)
#' @param header_pt numeric header font size ved base_size=14 (default 8)
#' @param value_pt numeric value font size ved base_size=14 (default 24)
#' @return character marquee-formateret label string
#'
#' @examples
#' create_responsive_label("MÅL", "≥90%", base_size = 14)
#' # → "{.8 **MÅL**}\n{.24 **≥90%**}"
#'
#' create_responsive_label("MÅL", "≥90%", base_size = 20)
#' # → "{.11 **MÅL**}\n{.34 **≥90%**}"

create_responsive_label <- function(header, value, base_size = 14, header_pt = 8, value_pt = 24) {
  scale_factor <- base_size / 14
  header_size <- round(header_pt * scale_factor)
  value_size <- round(value_pt * scale_factor)

  sprintf(
    "{.%d **%s**}  \n{.%d **%s**}",
    header_size,
    header,
    value_size,
    value
  )
}

# ============================================================================
# DATA GENERERING OG PLOT (ORIGINAL KODE BEVARET)
# ============================================================================

# Simuler data i tidy stil
set.seed(123)

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
  target = 0.4,
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
    # expand = expansion(mult = c(0.025, .0)),
    labels = scales::label_date_short(),
    breaks = scales::fullseq(c(as_datetime("2023-01-01"), as_datetime("2025-04-01")), "3 month")
  )+
  scale_y_continuous(
    expand = expansion(mult = c(.2, .2)),
    breaks = scales::breaks_pretty(),
    labels = scales::label_percent(decimal.mark = ",", accuracy = 1)
  ) +
  theme(
    # plot.margin = unit(c(0, 0, 0, 10), "pt"),
    panel.background = element_blank(),
    axis.text.y = element_text(family = "sans", color = "#858585", size = 16, angle = 0, hjust = 1),
    axis.text.x = element_text(color = "#858585", angle = 0, size = 8, family = "sans", face = "plain"),
    axis.line.x = element_line(color = "#D6D6D6"),
    axis.ticks.x =  element_line(color = "#D6D6D6"),
    axis.ticks.y =  element_line(color = "#D6D6D6"),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    legend.position = "none"
    # aspect.ratio = 210/297  # Sætter højde/bredde-forhold til A4 landscape
  ) + coord_capped_cart(bottom='right', gap = 0)

# ============================================================================
# ANVEND FORBEDRET LABEL SYSTEM
# ============================================================================

# Hent værdier fra data
y_cl  <- tail(stats::na.omit(qic_data$cl_extension), 1)
y_target <- tail(stats::na.omit(qic_data$target), 1)

# Base size for responsive scaling
base_size_plot <- 14  # Standard base size (matches theme_minimal default)

# Opret responsive marquee-formaterede labels
label_cl <- create_responsive_label(
  header = "NUV. NIVEAU",
  value = paste0(round(y_cl * 100), "%"),
  base_size = base_size_plot
)

label_target <- create_responsive_label(
  header = "MÅL",
  value = paste0(størreend, round(y_target * 100), "%"),
  base_size = base_size_plot
)

# Placer labels med forbedret system
plot <- add_right_labels_marquee(
  p = spc_plot,
  yA = y_cl,
  yB = y_target,
  textA = label_cl,
  textB = label_target,
  params = list(
    # label_height_npc - AUTO-BEREGNES fra font sizes i textA/textB
    # gap_line - AUTO: 15% af label_height_npc
    # gap_labels - AUTO: 30% af label_height_npc
    pad_top = 0.01,            # Top padding
    pad_bot = 0.01,            # Bottom padding
    pref_pos = c("under", "under"),  # Default: placer under linjer
    priority = "A"             # Beskyt CL label ved konflikter
  ),
  gpA = grid::gpar(col = "#009CE8"),
  gpB = grid::gpar(col = "#565656"),
  x_npc = 0.99,              # Ikke brugt med geom_marquee
  base_size = base_size_plot,  # Responsive sizing (matches plot theme)
  verbose = TRUE,            # Print warnings inkl. auto-beregninger
  debug_mode = FALSE         # Sæt til TRUE for visual debugging
)

# Gem mapper for debugging
mapper <- npc_mapper_from_plot(spc_plot)

# ============================================================================
# OUTPUT
# ============================================================================

# Print placement info for debugging
if (FALSE) {
  placement_info <- attr(plot, "placement_info")
  cat("\n=== Placement Info ===\n")
  cat("Label A (CL):\n")
  cat("  Y-position (npc):", placement_info$yA, "\n")
  cat("  Y-position (data):", mapper$npc_to_y(placement_info$yA), "\n")
  cat("  Side:", placement_info$sideA, "\n")
  cat("Label B (Target):\n")
  cat("  Y-position (npc):", placement_info$yB, "\n")
  cat("  Y-position (data):", mapper$npc_to_y(placement_info$yB), "\n")
  cat("  Side:", placement_info$sideB, "\n")
  cat("Quality:", placement_info$placement_quality, "\n")

  cat("\nLine positions:\n")
  cat("  CL line (data):", y_cl, "\n")
  cat("  CL line (npc):", mapper$y_to_npc(y_cl), "\n")
  cat("  Target line (data):", y_target, "\n")
  cat("  Target line (npc):", mapper$y_to_npc(y_target), "\n")
  cat("  Gap between lines (npc):", abs(mapper$y_to_npc(y_cl) - mapper$y_to_npc(y_target)), "\n")
  cat("  Gap between labels (npc):", abs(placement_info$yA - placement_info$yB), "\n")

  mapper_info <- attr(plot, "mapper_info")
  cat("\nMapper Info:\n")
  cat("  Y-limits:", mapper_info$limits, "\n")
  cat("  Transform:", mapper_info$trans_name, "\n")
}

# Vis plot
print(plot)
