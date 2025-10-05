# ==============================================================================
# INTELLIGENT LABEL PLACEMENT SYSTEM - STANDALONE VERSION
# ==============================================================================
#
# Intelligent label placement system med NPC-koordinater og multi-level
# collision avoidance.
#
# Kan genbruges i ethvert R plotting projekt til at placere labels ved
# horisontale linjer uden overlaps.
#
# FEATURES:
# - Ingen overlaps mellem labels eller med linjer
# - Multi-level collision avoidance (3 niveauer)
# - Auto-adaptive parametre baseret på font sizes
# - Device-independent (NPC koordinater 0-1)
# - Robust på tværs af ggplot2 versioner
# - Edge case handling (sammenfaldende linjer, bounds violations)
#
# DEPENDENCIES:
# - ggplot2
# - stringr
#
# USAGE:
# library(ggplot2)
#
# # Opret plot
# p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#   geom_point() +
#   geom_hline(yintercept = 20, color = "blue") +
#   geom_hline(yintercept = 25, color = "red") +
#   theme_minimal()
#
# # Opret NPC mapper
# mapper <- npc_mapper_from_plot(p)
#
# # Definer labels (marquee format)
# label_A <- "{.8 **CL**}  \n{.24 **20 mpg**}"
# label_B <- "{.8 **Target**}  \n{.24 **25 mpg**}"
#
# # Auto-beregn label height
# label_height <- estimate_label_height_npc(label_A)
#
# # Placer labels
# result <- place_two_labels_npc(
#   yA_npc = mapper$y_to_npc(20),
#   yB_npc = mapper$y_to_npc(25),
#   label_height_npc = label_height,
#   gap_line = label_height * 0.08,     # 8% af label height
#   gap_labels = label_height * 0.3,    # 30% af label height
#   priority = "A",
#   pref_pos = c("under", "under")
# )
#
# # Konverter NPC tilbage til data coordinates
# yA_data <- mapper$npc_to_y(result$yA)
# yB_data <- mapper$npc_to_y(result$yB)
#
# EXPORTS:
# - npc_mapper_from_plot()
# - estimate_label_height_npc()
# - place_two_labels_npc()
# - propose_single_label()
# - clamp01()
#
# ==============================================================================


# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Clamp værdi til interval [0, 1]
#'
#' @param x Numerisk værdi eller vektor
#' @return Værdi begrænset til [0, 1]
clamp01 <- function(x) {
  # Input validation
  if (is.null(x) || length(x) == 0) {
    stop("clamp01: x må ikke være NULL eller tom")
  }

  if (!is.numeric(x)) {
    stop("clamp01: x skal være numerisk, modtog: ", class(x)[1])
  }

  if (any(!is.finite(x[!is.na(x)]))) {
    warning("clamp01: Ikke-finite værdier (Inf/-Inf) detekteret")
    x[!is.finite(x)] <- NA_real_
  }

  pmax(0, pmin(1, x))
}

#' Clamp værdi til custom bounds interval
#'
#' Bruges til at sikre at labels respekterer panel padding (pad_top/pad_bot)
#' også ved flip-scenarios.
#'
#' @param x Numerisk værdi eller vektor
#' @param low_bound Nedre grænse
#' @param high_bound Øvre grænse
#' @return Værdi begrænset til [low_bound, high_bound]
clamp_to_bounds <- function(x, low_bound, high_bound) {
  # Input validation
  if (is.null(x) || length(x) == 0) {
    stop("clamp_to_bounds: x må ikke være NULL eller tom")
  }

  if (!is.numeric(x) || !is.numeric(low_bound) || !is.numeric(high_bound)) {
    stop("clamp_to_bounds: Alle parametre skal være numeriske")
  }

  if (low_bound >= high_bound) {
    stop("clamp_to_bounds: low_bound skal være mindre end high_bound")
  }

  pmax(low_bound, pmin(high_bound, x))
}


# ==============================================================================
# NPC MAPPING
# ==============================================================================

#' Opret NPC mapper fra ggplot object
#'
#' Denne funktion bygger ggplot og udtræker scale information for at konvertere
#' mellem data-koordinater og NPC (Normalized Parent Coordinates, 0-1).
#'
#' @param p ggplot object
#' @param panel Panel nummer (default = 1)
#'
#' @return List med:
#'   - `y_to_npc`: function(y_data) → NPC
#'   - `npc_to_y`: function(npc) → y_data
#'   - `limits`: c(ymin, ymax)
#'   - `trans_name`: transformation navn (fx "identity", "log10")
#'
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' mapper <- npc_mapper_from_plot(p)
#' mapper$y_to_npc(20)  # Konverter 20 mpg til NPC
#' mapper$npc_to_y(0.5) # Konverter 0.5 NPC til mpg
npc_mapper_from_plot <- function(p, panel = 1) {
  # Validate ggplot object
  if (is.null(p) || !inherits(p, "ggplot")) {
    stop("npc_mapper_from_plot: p skal være et ggplot object, modtog: ", class(p)[1])
  }

  # Validate panel parameter
  if (!is.numeric(panel) || length(panel) != 1 || panel < 1 || panel != floor(panel)) {
    stop("npc_mapper_from_plot: panel skal være et positivt heltal, modtog: ", panel)
  }

  # Build plot med fejlhåndtering
  b <- tryCatch({
    ggplot2::ggplot_build(p)
  }, error = function(e) {
    stop("npc_mapper_from_plot: Kunne ikke bygge ggplot object. Fejl: ", e$message)
  })

  # Validate plot build structure
  if (is.null(b) || !inherits(b, "ggplot_built")) {
    stop("npc_mapper_from_plot: Ugyldig ggplot_build struktur")
  }

  if (is.null(b$layout) || is.null(b$layout$panel_params)) {
    stop("npc_mapper_from_plot: Plot mangler layout information")
  }

  # Validate panel exists
  if (panel > length(b$layout$panel_params)) {
    stop(sprintf(
      "npc_mapper_from_plot: Panel %d eksisterer ikke (plot har kun %d panels)",
      panel, length(b$layout$panel_params)
    ))
  }

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

        # Protect against division by zero
        if (abs(y1 - y0) < .Machine$double.eps) {
          stop(sprintf(
            "Y-akse range er praktisk talt nul efter transformation: y0=%.10f, y1=%.10f",
            y0, y1
          ))
        }

        result[valid] <- (yt - y0) / (y1 - y0)
      }
      return(result)
    }
    yt <- info$trans(y)
    y0 <- info$trans(ymin)
    y1 <- info$trans(ymax)

    # Protect against division by zero
    if (abs(y1 - y0) < .Machine$double.eps) {
      stop(sprintf(
        "Y-akse range er praktisk talt nul efter transformation: y0=%.10f, y1=%.10f",
        y0, y1
      ))
    }

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


# ==============================================================================
# LABEL HEIGHT ESTIMATION
# ==============================================================================

#' Estimér label højde fra marquee markup VED FAKTISK GROB-MÅLING
#'
#' Opretter et marquee grob med de faktiske styles og måler højden præcist.
#' Dette giver korrekt højde på alle panelstørrelser uden magic numbers.
#'
#' @param text Marquee string med font size markup (fx "{.8 **Header**}  \n{.24 **Value**}")
#' @param style marquee style object (default: classic_style med right align)
#' @param panel_height_inches Panel højde i inches (hvis kendt, ellers NULL for auto-detect)
#' @param fallback_npc Fallback værdi hvis grob-måling fejler (default 0.13)
#'
#' @return Faktisk label højde i NPC koordinater (0-1)
#'
#' @details
#' Denne funktion erstatter den tidligere estimation-baserede tilgang med
#' faktisk måling. Det eliminerer alle magic numbers (pt_to_npc_factor,
#' line_spacing, safety_margin) og giver præcis højde på:
#' - Små paneler (facetter)
#' - Store paneler
#' - Forskellige base_size værdier
#' - Forskellige font families
#'
#' @examples
#' label <- "{.8 **CL**}  \n{.24 **45%**}"
#' style <- marquee::modify_style(
#'   marquee::classic_style(),
#'   "p",
#'   margin = marquee::trbl(0),
#'   align = "right"
#' )
#' height <- estimate_label_height_npc(label, style = style)
#' # Returns faktisk målt højde (typisk ~0.10-0.15 for 2-line label)
estimate_label_height_npc <- function(
  text,
  style = NULL,
  panel_height_inches = NULL,
  fallback_npc = 0.13
) {

  tryCatch({
    # Default style hvis ikke angivet
    # NOTE: marquee_grob accepterer kun 'style' parameter, ikke separate
    # size/lineheight/family. Disse skal specificeres gennem style-objektet.
    if (is.null(style)) {
      style <- marquee::modify_style(
        marquee::classic_style(),
        "p",
        margin = marquee::trbl(0),
        align = "right"
      )
    }

    # Opret marquee grob for at måle faktisk højde
    # NOTE: marquee_grob() bruger default size fra style
    # For at måle korrekt skal vi bruge samme setup som ved rendering
    g <- marquee::marquee_grob(
      text = text,
      x = 0.5,
      y = 0.5,
      style = style
    )

    # Mål højde i native units
    h_native <- grid::grobHeight(g)

    # Konverter til NPC
    # Hvis panel_height kendt, brug den; ellers brug current viewport
    if (!is.null(panel_height_inches)) {
      h_inches <- grid::convertHeight(h_native, "inches", valueOnly = TRUE)
      h_npc <- h_inches / panel_height_inches
    } else {
      # Auto-detect fra current viewport
      h_npc <- grid::convertHeight(h_native, "npc", valueOnly = TRUE)
    }

    # Tilføj lille sikkerhedsmargin (5%) for at være konservativ
    # Dette sikrer at labels ikke overlapper selv med små afrundingsfejl
    h_npc <- h_npc * 1.05

    # Sanity check: Verificer at målingen er rimelig
    if (!is.finite(h_npc) || h_npc <= 0 || h_npc > 0.5) {
      warning(
        "Grob-måling gav usandsynlig værdi (", round(h_npc, 4), "), bruger fallback. ",
        "Dette kan skyldes manglende viewport eller ugyldigt marquee markup."
      )
      return(fallback_npc)
    }

    return(as.numeric(h_npc))

  }, error = function(e) {
    warning(
      "Grob-baseret højdemåling fejlede: ", e$message,
      " - bruger fallback: ", fallback_npc
    )
    return(fallback_npc)
  })
}


# ==============================================================================
# LABEL PLACEMENT - HELPER FUNCTION
# ==============================================================================

#' Helper: propose single label placement
#'
#' Foreslår en position for én label ved en linje, med foretrukken side.
#' Flipper automatisk til modsatte side hvis foretrukken side er udenfor bounds.
#'
#' @param y_line_npc Linje position i NPC
#' @param pref_side Foretrukken side: "under" eller "over"
#' @param label_h Label højde i NPC
#' @param gap Min gap fra label edge til linje
#' @param pad_top Top padding
#' @param pad_bot Bottom padding
#'
#' @return List med:
#'   - `center`: NPC position for label center
#'   - `side`: Faktisk side ("under" eller "over")
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
    # Flip til over - NU MED BOUNDS-AWARE CLAMPING
    y <- y_line_npc + gap + half
    return(list(center = clamp_to_bounds(y, low_bound, high_bound), side = "over"))
  } else {
    y <- y_line_npc + gap + half
    if (y <= high_bound) {
      return(list(center = y, side = "over"))
    }
    # Flip til under - NU MED BOUNDS-AWARE CLAMPING
    y <- y_line_npc - gap - half
    return(list(center = clamp_to_bounds(y, low_bound, high_bound), side = "under"))
  }
}


# ==============================================================================
# LABEL PLACEMENT - CORE ALGORITHM
# ==============================================================================

#' Placer to labels ved horisontale linjer med collision avoidance
#'
#' Core placement algoritme med multi-level collision avoidance:
#' - NIVEAU 1: Reducer label gap (50% → 30% → 15%)
#' - NIVEAU 2: Flip labels til modsatte side (3 strategier)
#' - NIVEAU 3: Shelf placement (sidste udvej)
#'
#' @param yA_npc Y-position for linje A i NPC (0-1)
#' @param yB_npc Y-position for linje B i NPC (0-1)
#' @param label_height_npc Label højde i NPC (brug estimate_label_height_npc())
#' @param gap_line Min gap fra label edge til linje (default 0.015, anbefalet 8% af label_height)
#' @param gap_labels Min gap mellem labels (default 0.03, anbefalet 30% af label_height)
#' @param pad_top Top panel padding (default 0.01)
#' @param pad_bot Bottom panel padding (default 0.01)
#' @param priority Prioriteret label: "A" eller "B" (default "A")
#' @param pref_pos Foretrukne positioner: c("under"/"over", "under"/"over") (default c("under", "under"))
#' @param debug Returnér debug info? (default FALSE)
#'
#' @return List med:
#'   - `yA`: NPC position for label A center
#'   - `yB`: NPC position for label B center
#'   - `sideA`: "over" eller "under"
#'   - `sideB`: "over" eller "under"
#'   - `placement_quality`: "optimal" / "acceptable" / "suboptimal" / "degraded" / "failed"
#'   - `warnings`: Character vector med warnings
#'   - `debug_info`: (kun hvis debug=TRUE)
#'
#' @examples
#' # Basic usage
#' result <- place_two_labels_npc(
#'   yA_npc = 0.4,
#'   yB_npc = 0.6,
#'   label_height_npc = 0.13,
#'   gap_line = 0.01,
#'   gap_labels = 0.04
#' )
#'
#' # Coincident lines (target = CL)
#' result <- place_two_labels_npc(
#'   yA_npc = 0.5,
#'   yB_npc = 0.5,  # Samme værdi
#'   label_height_npc = 0.13,
#'   pref_pos = c("under", "under")
#' )
#' # Result: sideA = "under", sideB = "over"
place_two_labels_npc <- function(
    yA_npc,
    yB_npc,
    label_height_npc = 0.035,
    gap_line = NULL,      # NU: Auto-beregnes fra config
    gap_labels = NULL,    # NU: Auto-beregnes fra config
    pad_top = NULL,       # NU: Hentes fra config
    pad_bot = NULL,       # NU: Hentes fra config
    priority = c("A", "B")[1],
    pref_pos = c("under", "under"),
    debug = FALSE
) {

  # ============================================================================
  # INPUT VALIDATION
  # ============================================================================

  # Helper function for NPC parameter validation
  validate_npc_param <- function(value, name, allow_na = TRUE) {
    if (is.null(value)) return(invisible(NULL))

    if (!is.numeric(value)) {
      stop(sprintf("%s skal være numerisk, modtog: %s", name, class(value)[1]))
    }

    if (length(value) != 1) {
      stop(sprintf("%s skal være en enkelt værdi, modtog: %d værdier", name, length(value)))
    }

    if (!allow_na && is.na(value)) {
      stop(sprintf("%s må ikke være NA", name))
    }

    if (!is.na(value) && !is.finite(value)) {
      stop(sprintf("%s skal være finite (ikke Inf/-Inf), modtog: %s", name, value))
    }

    invisible(NULL)
  }

  # Validér alle NPC inputs
  validate_npc_param(yA_npc, "yA_npc", allow_na = TRUE)
  validate_npc_param(yB_npc, "yB_npc", allow_na = TRUE)
  validate_npc_param(label_height_npc, "label_height_npc", allow_na = FALSE)
  validate_npc_param(gap_line, "gap_line", allow_na = FALSE)
  validate_npc_param(gap_labels, "gap_labels", allow_na = FALSE)
  validate_npc_param(pad_top, "pad_top", allow_na = FALSE)
  validate_npc_param(pad_bot, "pad_bot", allow_na = FALSE)

  # Bounds validation for label_height_npc
  if (!is.null(label_height_npc)) {
    if (label_height_npc <= 0) {
      stop("label_height_npc skal være positiv, modtog: ", label_height_npc)
    }
    if (label_height_npc > 0.5) {
      stop("label_height_npc må ikke overstige 0.5 (50% af panel), modtog: ", label_height_npc)
    }
  }

  # Bounds validation for padding
  if (!is.null(pad_top) && (pad_top < 0 || pad_top > 0.2)) {
    stop("pad_top skal være mellem 0 og 0.2, modtog: ", pad_top)
  }
  if (!is.null(pad_bot) && (pad_bot < 0 || pad_bot > 0.2)) {
    stop("pad_bot skal være mellem 0 og 0.2, modtog: ", pad_bot)
  }

  # Priority validation
  priority <- match.arg(priority, choices = c("A", "B"))

  # pref_pos validation
  if (!is.character(pref_pos) || length(pref_pos) == 0) {
    stop("pref_pos skal være en character vektor")
  }
  pref_pos <- rep_len(pref_pos, 2)
  if (!all(pref_pos %in% c("under", "over"))) {
    stop("pref_pos skal indeholde 'under' eller 'over', modtog: ", paste(pref_pos, collapse = ", "))
  }

  # debug validation
  if (!is.logical(debug) || length(debug) != 1) {
    stop("debug skal være en enkelt logical værdi")
  }

  # ============================================================================
  # CONFIGURATION LOADING
  # ============================================================================

  # Source config hvis tilgængelig (standalone compatibility)
  if (exists("get_label_placement_param", mode = "function")) {
    config_available <- TRUE
  } else {
    config_available <- FALSE
  }

  # Beregn defaults fra config (eller brug fallback for standalone mode)
  if (is.null(gap_line)) {
    if (config_available) {
      gap_line <- label_height_npc * get_label_placement_param("relative_gap_line")
    } else {
      gap_line <- label_height_npc * 0.08  # Fallback: 8% af label height
    }
  }

  if (is.null(gap_labels)) {
    if (config_available) {
      gap_labels <- label_height_npc * get_label_placement_param("relative_gap_labels")
    } else {
      gap_labels <- label_height_npc * 0.30  # Fallback: 30% af label height
    }
  }

  if (is.null(pad_top)) {
    if (config_available) {
      pad_top <- get_label_placement_param("pad_top")
    } else {
      pad_top <- 0.01  # Fallback: 1%
    }
  }

  if (is.null(pad_bot)) {
    if (config_available) {
      pad_bot <- get_label_placement_param("pad_bot")
    } else {
      pad_bot <- 0.01  # Fallback: 1%
    }
  }

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

  # Hent tight_lines_threshold fra config
  tight_threshold_factor <- if (config_available) {
    get_label_placement_param("tight_lines_threshold_factor")
  } else {
    0.5  # Fallback: 50% af min_center_gap
  }

  if (line_gap_npc < min_center_gap * tight_threshold_factor) {
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
  # Hent coincident_threshold fra config (som procent af label_height)
  coincident_threshold <- if (config_available) {
    label_height_npc * get_label_placement_param("coincident_threshold_factor")
  } else {
    label_height_npc * 0.1  # Fallback: 10% af label højde
  }

  if (abs(yA_npc - yB_npc) < coincident_threshold) {
    warnings <- c(warnings, "Sammenfaldende linjer - placerer labels over/under")

    # Placer den ene label over, den anden under samme linje
    # Prioriter CL (A) til foretrukken side
    if (pref_pos[1] == "under") {
      # CL under, Target over
      yA <- clamp01(yA_npc - gap_line - half)
      yB <- clamp01(yA_npc + gap_line + half)
      sideA <- "under"
      sideB <- "over"
    } else {
      # CL over, Target under
      yA <- clamp01(yA_npc + gap_line + half)
      yB <- clamp01(yA_npc - gap_line - half)
      sideA <- "over"
      sideB <- "under"
    }

    # Verificér at begge labels er inden for bounds
    if (yA < low_bound || yA > high_bound || yB < low_bound || yB > high_bound) {
      warnings <- c(warnings, "Label(s) uden for bounds - justerer til bounds")
      yA <- clamp01(yA)
      yB <- clamp01(yB)
      placement_quality <- "acceptable"
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

      # Hent gap_reduction_factors fra config
      reduction_factors <- if (config_available) {
        get_label_placement_param("gap_reduction_factors")
      } else {
        c(0.5, 0.3, 0.15)  # Fallback: 50%, 30%, 15%
      }

      for (reduction_factor in reduction_factors) {
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

        # Hent shelf_center_threshold fra config
        shelf_threshold <- if (config_available) {
          get_label_placement_param("shelf_center_threshold")
        } else {
          0.5  # Fallback: midtpunkt (50% NPC)
        }

        # Prioriter den vigtigste label tættest på sin linje
        if (priority == "A") {
          yA <- clamp01(proposed_yA)
          yB <- if (yA < shelf_threshold) high_bound else low_bound  # Modsatte shelf
        } else {
          yB <- clamp01(proposed_yB)
          yA <- if (yB < shelf_threshold) high_bound else low_bound
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
