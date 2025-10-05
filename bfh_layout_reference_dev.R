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
# LOAD STANDALONE LABEL PLACEMENT FUNCTIONS
# ============================================================================
# Core funktioner til NPC-baseret label placement:
# - npc_mapper_from_plot()
# - estimate_label_height_npc()
# - place_two_labels_npc()
# - propose_single_label()
# - clamp01()
#
# Disse funktioner kan også bruges i andre projekter ved at inkludere
# utils_standalone_label_placement.R
# ============================================================================

source("utils_standalone_label_placement.R")

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
    base_size = 14,
    verbose = TRUE,
    debug_mode = FALSE
) {

  # Beregn responsive størrelser baseret på base_size
  # Reference: base_size 14 giver original størrelser
  scale_factor <- base_size / 14

  # Opret right-aligned style FØRST (bruges til højdemåling)
  right_aligned_style <- marquee::modify_style(
    marquee::classic_style(),
    "p",
    margin = marquee::trbl(0),
    align = "right"
  )

  # DEFENSIVE: Mål faktisk panel højde for korrekt NPC-normalisering
  # Dette sikrer at labels normaliseres mod panel (ikke hele device inkl. margener)
  panel_height_inches <- tryCatch({
    measure_panel_height_inches(p)
  }, error = function(e) {
    if (verbose) {
      message("Kunne ikke måle panel højde: ", e$message, " - bruger viewport fallback")
    }
    NULL  # Fallback til viewport-baseret måling
  })

  if (verbose && !is.null(panel_height_inches)) {
    message("Målt panel højde: ", round(panel_height_inches, 3), " inches")
  }

  # Auto-beregn label_height hvis ikke angivet - NU MED GROB-BASERET MÅLING
  if (is.null(params$label_height_npc)) {
    # Mål faktisk højde fra marquee grobs
    # NOTE: textA/textB er allerede skaleret med base_size via create_responsive_label()
    # Style-objektet indeholder allerede alle nødvendige formatting-info
    # DEFENSIVE: Send panel_height_inches videre for korrekt normalisering
    height_A <- estimate_label_height_npc(
      text = textA,
      style = right_aligned_style,
      panel_height_inches = panel_height_inches
    )
    height_B <- estimate_label_height_npc(
      text = textB,
      style = right_aligned_style,
      panel_height_inches = panel_height_inches
    )
    params$label_height_npc <- max(height_A, height_B)

    if (verbose) {
      message("Auto-beregnet label_height_npc via grob-måling: ", round(params$label_height_npc, 4),
              " (A: ", round(height_A, 4), ", B: ", round(height_B, 4), ")")
    }
  }

  # Default parameters hvis ikke angivet - HENT FRA CONFIG
  # NOTE: place_two_labels_npc() vil også bruge config, men vi sætter dem her
  # for klarhed og for at kunne override ved behov

  # Check om config er tilgængelig
  config_available <- exists("get_label_placement_param", mode = "function")

  if (is.null(params$gap_line)) {
    if (config_available) {
      params$gap_line <- params$label_height_npc * get_label_placement_param("relative_gap_line")
    } else {
      params$gap_line <- params$label_height_npc * 0.08  # Fallback: 8% af label height
    }
  }

  if (is.null(params$gap_labels)) {
    if (config_available) {
      params$gap_labels <- params$label_height_npc * get_label_placement_param("relative_gap_labels")
    } else {
      params$gap_labels <- params$label_height_npc * 0.3  # Fallback: 30% af label height
    }
  }

  if (is.null(params$pad_top)) {
    if (config_available) {
      params$pad_top <- get_label_placement_param("pad_top")
    } else {
      params$pad_top <- 0.01  # Fallback
    }
  }

  if (is.null(params$pad_bot)) {
    if (config_available) {
      params$pad_bot <- get_label_placement_param("pad_bot")
    } else {
      params$pad_bot <- 0.01  # Fallback
    }
  }

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

  # Skalerede størrelser for marquee labels
  # NOTE: right_aligned_style er allerede oprettet tidligere (bruges til højdemåling)
  # Hent marquee_size_factor fra config
  marquee_size_factor <- if (config_available) {
    get_label_placement_param("marquee_size_factor")
  } else {
    6  # Fallback
  }
  marquee_size <- marquee_size_factor * scale_factor

  # Hent lineheight fra config
  marquee_lineheight <- if (config_available) {
    get_label_placement_param("marquee_lineheight")
  } else {
    0.9  # Fallback
  }

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
        lineheight = marquee_lineheight,
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

#' Sanitize text for marquee markup
#'
#' Escaper specialtegn der har betydning i marquee markup
#' for at forhindre injection attacks
sanitize_marquee_text <- function(text) {
  if (is.null(text) || length(text) == 0) {
    return("")
  }

  if (!is.character(text)) {
    warning("sanitize_marquee_text: Konverterer ikke-character input til character")
    text <- as.character(text)
  }

  if (length(text) > 1) {
    warning("sanitize_marquee_text: Flere værdier modtaget, bruger kun første")
    text <- text[1]
  }

  # Escape marquee special characters
  # Note: Marquee bruger {} til markup, ** bevar vi da det skal være bold i output
  text <- gsub("\\{", "&#123;", text)    # Escape {
  text <- gsub("\\}", "&#125;", text)    # Escape }

  # Fjern kontroltegn (men bevar \n for linjeskift)
  text <- gsub("[[:cntrl:]&&[^\n]]", "", text)

  # Begræns længde for at forhindre memory exhaustion
  max_length <- 200
  if (nchar(text) > max_length) {
    warning(sprintf("Text afkortet fra %d til %d tegn", nchar(text), max_length))
    text <- substr(text, 1, max_length)
  }

  text
}

create_responsive_label <- function(header, value, base_size = 14, header_pt = 8, value_pt = 24) {
  # Input validation
  if (!is.numeric(base_size) || length(base_size) != 1 || base_size <= 0) {
    stop("base_size skal være et positivt tal, modtog: ", base_size)
  }

  if (base_size < 8 || base_size > 48) {
    warning("base_size uden for normalt interval (8-48), modtog: ", base_size)
  }

  if (!is.numeric(header_pt) || !is.numeric(value_pt)) {
    stop("header_pt og value_pt skal være numeriske")
  }

  if (header_pt <= 0 || value_pt <= 0) {
    stop("header_pt og value_pt skal være positive")
  }

  # Sanitize inputs
  header <- sanitize_marquee_text(header)
  value <- sanitize_marquee_text(value)

  # Compute scaled sizes
  scale_factor <- base_size / 14
  header_size <- round(header_pt * scale_factor)
  value_size <- round(value_pt * scale_factor)

  # Sanity check sizes
  if (header_size < 1 || header_size > 100) {
    stop(sprintf("Beregnet header_size uden for gyldigt interval: %d", header_size))
  }
  if (value_size < 1 || value_size > 100) {
    stop(sprintf("Beregnet value_size uden for gyldigt interval: %d", value_size))
  }

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
set.seed(NULL)

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
    labels = scales::label_date_short(),
    breaks = scales::fullseq(c(as_datetime("2023-01-01"), as_datetime("2025-04-01")), "3 month")
  )+
  scale_y_continuous(
    expand = expansion(mult = c(.2, .2)),
    breaks = scales::breaks_pretty(),
    labels = scales::label_percent(decimal.mark = ",", accuracy = 1)
  ) +
  theme(
    panel.background = element_blank(),
    axis.text.y = element_text(family = "sans", color = "#858585", size = 16, angle = 0, hjust = 1),
    axis.text.x = element_text(color = "#858585", angle = 0, size = 8, family = "sans", face = "plain"),
    axis.line.x = element_line(color = "#D6D6D6"),
    axis.ticks.x =  element_line(color = "#D6D6D6"),
    axis.ticks.y =  element_line(color = "#D6D6D6"),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    legend.position = "none"
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
    # gap_line - AUTO: 8% af label_height_npc (fra config)
    # gap_labels - AUTO: 30% af label_height_npc (fra config)
    pad_top = 0.01,            # Top padding (fra config)
    pad_bot = 0.01,            # Bottom padding (fra config)
    pref_pos = c("under", "under"),  # Default: placer under linjer
    priority = "A"             # Beskyt CL label ved konflikter
  ),
  gpA = grid::gpar(col = "#009CE8"),
  gpB = grid::gpar(col = "#565656"),
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
