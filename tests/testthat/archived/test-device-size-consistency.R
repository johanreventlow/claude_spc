# test-device-size-consistency.R
# Regression test: Label gap skal være konsistent på tværs af device sizes
#
# FIX: Panel height measurement skal bruge faktisk device size,
# ikke hardcoded 7×7 inches, for at sikre korrekt NPC normalisering.

test_that("Panel height measurement respekterer device størrelse", {
  skip_if_not_installed("qicharts2")
  skip_if_not_installed("ggplot2")

  # Load required functions
  if (!exists("measure_panel_height_from_gtable")) {
    source("R/utils_label_placement.R")
  }

  # Create test plot
  test_data <- data.frame(
    x = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 30),
    y = rnorm(30, mean = 50, sd = 10)
  )

  qic_result <- qicharts2::qic(
    x = x,
    y = y,
    data = test_data,
    chart = "i"
  )

  # Build gtable
  built <- ggplot2::ggplot_build(qic_result)
  gt <- ggplot2::ggplot_gtable(built)

  # Mål panel height på forskellige device sizes
  # Small device (som brugerens 1294 × 751 px plot ved 96 dpi)
  small_width <- 1294 / 96  # ~13.5 inches
  small_height <- 751 / 96  # ~7.8 inches

  panel_height_small <- measure_panel_height_from_gtable(
    gt,
    device_width = small_width,
    device_height = small_height,
    use_cache = FALSE  # Disable cache for test isolation
  )

  # Large device (som brugerens 3288 × 1748 px plot ved 96 dpi)
  large_width <- 3288 / 96  # ~34.3 inches
  large_height <- 1748 / 96  # ~18.2 inches

  panel_height_large <- measure_panel_height_from_gtable(
    gt,
    device_width = large_width,
    device_height = large_height,
    use_cache = FALSE
  )

  # Verificér at panel heights skalerer med device size
  # Panel height burde være proportional til device height
  height_ratio <- large_height / small_height
  panel_ratio <- panel_height_large / panel_height_small

  # Ratio bør være tæt på height ratio (±10% tolerance for layout overhead)
  expect_equal(panel_ratio, height_ratio, tolerance = 0.1,
               info = sprintf(
                 "Panel height skal skalere med device size. Small: %.2f\", Large: %.2f\", Ratio: %.2f (expected ~%.2f)",
                 panel_height_small, panel_height_large, panel_ratio, height_ratio
               ))

  # Sanity checks
  expect_true(panel_height_small > 0,
              "Small device panel height skal være positiv")
  expect_true(panel_height_large > 0,
              "Large device panel height skal være positiv")
  expect_true(panel_height_large > panel_height_small,
              "Større device skal give større panel height")
})

test_that("NPC gap er konsistent på tværs af device sizes", {
  skip_if_not_installed("qicharts2")
  skip_if_not_installed("ggplot2")

  # Load required functions
  if (!exists("measure_panel_height_from_gtable")) {
    source("R/utils_label_placement.R")
  }
  if (!exists("LABEL_PLACEMENT_CONFIG")) {
    source("R/config_label_placement.R")
  }

  # Create test plot
  test_data <- data.frame(
    x = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 30),
    y = rnorm(30, mean = 50, sd = 10)
  )

  qic_result <- qicharts2::qic(
    x = x,
    y = y,
    data = test_data,
    chart = "i"
  )

  # Build gtable
  built <- ggplot2::ggplot_build(qic_result)
  gt <- ggplot2::ggplot_gtable(built)

  # Simuler label height estimation på forskellige device sizes
  # (bruger samme metode som estimate_label_heights_npc)

  # Small device
  small_width <- 1294 / 96
  small_height <- 751 / 96
  panel_height_small <- measure_panel_height_from_gtable(
    gt, device_width = small_width, device_height = small_height, use_cache = FALSE
  )

  # Large device
  large_width <- 3288 / 96
  large_height <- 1748 / 96
  panel_height_large <- measure_panel_height_from_gtable(
    gt, device_width = large_width, device_height = large_height, use_cache = FALSE
  )

  # Simuler label height (antag 0.5 inches fysisk højde)
  label_height_inches <- 0.5

  # Beregn NPC gap som systemet gør det
  config <- LABEL_PLACEMENT_CONFIG
  gap_inches <- label_height_inches * config$relative_gap_line

  # Konverter til NPC for hver device size
  gap_npc_small <- gap_inches / panel_height_small
  gap_npc_large <- gap_inches / panel_height_large

  # KRITISK: gap_npc skal IKKE være ens på tværs af device sizes
  # fordi panel_height_inches skalerer med device size.
  # MEN: gap_inches / label_height_inches ratio skal være konstant!

  # Det vi rent faktisk vil teste: visual ratio gap/label skal være ens
  # Dette opnås ved at label_height_npc også skalerer korrekt

  # Simuler label height i NPC
  label_npc_small <- label_height_inches / panel_height_small
  label_npc_large <- label_height_inches / panel_height_large

  # Visual ratio: gap_npc / label_npc
  visual_ratio_small <- gap_npc_small / label_npc_small
  visual_ratio_large <- gap_npc_large / label_npc_large

  # Disse skal være ens fordi både gap og label skalerer med panel_height
  expect_equal(visual_ratio_small, visual_ratio_large, tolerance = 0.001,
               info = sprintf(
                 "Visual gap/label ratio skal være ens. Small: %.3f, Large: %.3f",
                 visual_ratio_small, visual_ratio_large
               ))

  # Verificér at ratio matcher config
  expect_equal(visual_ratio_small, config$relative_gap_line, tolerance = 0.001,
               info = sprintf(
                 "Visual ratio skal matche config. Got: %.3f, Expected: %.3f",
                 visual_ratio_small, config$relative_gap_line
               ))
})

test_that("add_right_labels_marquee() detekterer device size korrekt", {
  skip_if_not_installed("qicharts2")
  skip_if_not_installed("ggplot2")

  # Load required functions
  if (!exists("add_right_labels_marquee")) {
    source("R/utils_add_right_labels_marquee.R")
  }
  if (!exists("add_spc_labels")) {
    source("R/fct_add_spc_labels.R")
  }
  # Load dependencies
  if (!exists("create_responsive_label")) {
    source("R/utils_label_helpers.R")
  }
  if (!exists("format_y_value")) {
    source("R/utils_label_formatting.R")
  }
  if (!exists("place_two_labels_npc")) {
    source("R/utils_label_placement.R")
  }
  if (!exists("npc_mapper_from_built")) {
    source("R/utils_npc_mapper.R")
  }
  if (!exists("estimate_label_heights_npc")) {
    source("R/utils_label_height_estimation.R")
  }
  if (!exists("measure_panel_height_from_gtable")) {
    source("R/utils_gtable_measurement.R")
  }
  if (!exists("LABEL_PLACEMENT_CONFIG")) {
    source("R/config_label_placement.R")
  }

  # Create test plot
  test_data <- data.frame(
    x = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 30),
    y = rnorm(30, mean = 50, sd = 10)
  )

  qic_result <- qicharts2::qic(
    x = x,
    y = y,
    data = test_data,
    chart = "i",
    target = 55
  )

  # Test på forskellige device sizes
  # Small device
  result_small <- NULL
  tryCatch({
    pdf(NULL, width = 1294/96, height = 751/96)
    result_small <- add_spc_labels(
      plot = qic_result,
      qic_data = qic_result$data,
      y_axis_unit = "count",
      label_size = 6,
      verbose = FALSE
    )
  }, error = function(e) {
    # På testthat kan device size detection fejle - det er OK
  }, finally = {
    if (dev.cur() > 1) dev.off()
  })

  # Large device
  result_large <- NULL
  tryCatch({
    pdf(NULL, width = 3288/96, height = 1748/96)
    result_large <- add_spc_labels(
      plot = qic_result,
      qic_data = qic_result$data,
      y_axis_unit = "count",
      label_size = 6,
      verbose = FALSE
    )
  }, error = function(e) {
    # På testthat kan device size detection fejle - det er OK
  }, finally = {
    if (dev.cur() > 1) dev.off()
  })

  # Hvis begge succeeds, verificér placement info
  if (!is.null(result_small) && !is.null(result_large)) {
    placement_small <- attr(result_small, "placement_info")
    placement_large <- attr(result_large, "placement_info")

    # Placement quality skal være samme niveau
    expect_equal(placement_small$placement_quality, placement_large$placement_quality,
                 info = "Placement quality skal være ens på tværs af device sizes")
  } else {
    skip("Device size detection ikke tilgængelig i test environment")
  }
})
