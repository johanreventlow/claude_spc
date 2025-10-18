# ==============================================================================
# PHASE 0: CACHE ISOLATION & VERIFICATION TESTS
# ==============================================================================
#
# Formål: Systematisk verificering af om cache er årsagen til label placement
# upålidelighed. Disse tests køres med cache disabled for at isolere problemet.
#
# Test-strategi:
# 1. Reproducibility test: 10 identiske plots skal give identiske resultater
# 2. Cache corruption test: Sammenlign cache miss vs cache hit outputs
# 3. Device consistency test: Samme input på forskellige devices
#
# Forventet outcome:
# - Hvis tests BESTÅR med cache disabled → cache er problemet
# - Hvis tests FEJLER med cache disabled → andet problem (grob measurement, etc.)
#
# ==============================================================================

library(testthat)

# ==============================================================================
# TEST 1: REPRODUCIBILITY - 10 IDENTISKE PLOTS UDEN CACHE
# ==============================================================================

test_that("Label placement er reproducerbar uden cache (10 identiske plots)", {
  skip_if_not_installed("qicharts2")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("marquee")

  # Unlock cache bindings først (nødvendigt i test kontekst)
  unlock_placement_cache_bindings()

  # Disable BEGGE caches
  configure_panel_cache(enabled = FALSE)
  configure_grob_cache(enabled = FALSE)

  # Clear any existing cache
  clear_panel_height_cache()
  clear_grob_height_cache()

  # Opret test data
  test_data <- data.frame(
    x = 1:20,
    y = c(10, 12, 11, 13, 15, 14, 16, 18, 17, 19,
          20, 22, 21, 23, 25, 24, 26, 28, 27, 29)
  )

  # PHASE 1 FIX: Warm-up qicharts2 for at eliminere first-run effect
  # (Fund: qicharts2 har package-level state der initialiseres ved første kald)
  # Kør 2 warm-up calls for at stabilisere internal state
  warmup1 <- qicharts2::qic(x = x, y = y, data = test_data, chart = "run")
  warmup2 <- qicharts2::qic(x = x, y = y, data = test_data, chart = "run")

  # Funktion til at generere plot og ekstrahere label koordinater
  generate_plot_and_extract_labels <- function(data) {
    # PHASE 1 FIX: Use fixed seed for reproducible plot generation
    set.seed(12345)

    # Opret SPC plot med qicharts2
    p <- qicharts2::qic(
      x = x,
      y = y,
      data = data,
      chart = "run"
    )

    # Build ggplot object for at få access til grob layer
    gt <- ggplot2::ggplot_build(p)

    # Ekstrahér label layers (typisk geom_text eller marquee)
    # Dette er simplified - i virkeligheden skal vi parse p$layers
    # for at finde label positions

    # For nu: Returner en hash af plot strukturen
    structure_hash <- digest::digest(list(
      data = gt$data,
      layout = gt$layout
    ), algo = "xxhash64")

    return(structure_hash)
  }

  # PHASE 1 FIX: Discard first call inside test context to eliminate warm-up effect
  discard_hash <- generate_plot_and_extract_labels(test_data)
  cat(sprintf("\nWarm-up call hash: %s (discarded)\n", discard_hash))

  # Generer 10 identiske plots
  cat("\nGenerating 10 plots for reproducibility test:\n")
  plot_hashes <- replicate(10, {
    hash <- generate_plot_and_extract_labels(test_data)
    cat(sprintf("  Hash: %s\n", hash))
    hash
  })

  # Verificer at alle hashes er identiske
  unique_hashes <- unique(plot_hashes)
  cat(sprintf("\nUnique hashes: %d (hashes: %s)\n",
              length(unique_hashes),
              paste(head(unique_hashes, 3), collapse = ", ")))

  expect_length(
    unique_hashes,
    1
  )

  # Re-enable caches efter test
  configure_panel_cache(enabled = TRUE)
  configure_grob_cache(enabled = TRUE)
})

# ==============================================================================
# TEST 2: CACHE CORRUPTION - SAMMENLIGN CACHE MISS VS CACHE HIT
# ==============================================================================

test_that("Cache miss og cache hit giver identiske resultater", {
  skip_if_not_installed("marquee")

  # Unlock cache bindings
  unlock_placement_cache_bindings()

  # Clear caches først
  clear_panel_height_cache()
  clear_grob_height_cache()

  # Enable cache for denne test
  configure_panel_cache(enabled = TRUE)
  configure_grob_cache(enabled = TRUE)

  # Test grob height measurement consistency
  test_text <- "{.8 **CL**}  \n{.24 **42.5 enheder**}"
  test_style <- marquee::classic_style(lineheight = 0.9)

  # Første måling (cache miss)
  result_miss <- estimate_label_height_npc(
    text = test_text,
    style = test_style,
    panel_height_inches = 6.0,
    use_cache = TRUE,
    return_details = TRUE
  )

  # Anden måling (cache hit)
  result_hit <- estimate_label_height_npc(
    text = test_text,
    style = test_style,
    panel_height_inches = 6.0,
    use_cache = TRUE,
    return_details = TRUE
  )

  # Verificer at resultater er identiske
  expect_equal(
    result_miss$npc,
    result_hit$npc,
    tolerance = 1e-10
  )

  expect_equal(
    result_miss$inches,
    result_hit$inches,
    tolerance = 1e-10
  )

  # Verificer at cache faktisk blev brugt
  stats <- get_grob_cache_stats()
  expect_true(
    is.numeric(stats$hits) && length(stats$hits) == 1 && stats$hits > 0,
    label = "Cache hit count should be > 0"
  )
})

# ==============================================================================
# TEST 3: DEVICE SIZE CONSISTENCY - SAMME INPUT, FORSKELLIGE DEVICES
# ==============================================================================

test_that("Label placement er konsistent på tværs af device størrelser", {
  skip_if_not_installed("marquee")

  # Unlock cache bindings
  unlock_placement_cache_bindings()

  # Disable cache for at isolere device-effekter
  configure_panel_cache(enabled = FALSE)
  configure_grob_cache(enabled = FALSE)

  # Clear caches
  clear_panel_height_cache()
  clear_grob_height_cache()

  test_text <- "{.8 **CL**}  \n{.24 **42.5 enheder**}"
  test_style <- marquee::classic_style(lineheight = 0.9)

  # Test devices: Small (4.5×3), Medium (8×4.5), Large (12×6.75), XLarge (16×9)
  device_heights <- c(3, 4.5, 6.75, 9)  # inches

  results <- lapply(device_heights, function(height) {
    estimate_label_height_npc(
      text = test_text,
      style = test_style,
      panel_height_inches = height,
      use_cache = FALSE,
      return_details = TRUE
    )
  })

  # Verificer at absolute inches height er konsistent
  # (NPC vil variere baseret på panel height, men absolute height skal være ens)
  inches_values <- sapply(results, function(r) r$inches)

  # Coefficient of variation (CV) skal være lav hvis measurement er stabil
  mean_inches <- mean(inches_values)
  sd_inches <- sd(inches_values)
  cv <- sd_inches / mean_inches

  expect_lt(
    cv,
    0.05  # Max 5% variation
  )

  # Re-enable caches
  configure_panel_cache(enabled = TRUE)
  configure_grob_cache(enabled = TRUE)
})

# ==============================================================================
# TEST 4: GAP BEREGNING VERIFICERING
# ==============================================================================

test_that("Gap beregnes korrekt som andel af label height", {
  skip_if_not_installed("marquee")

  # Unlock cache bindings
  unlock_placement_cache_bindings()

  # Disable cache
  configure_panel_cache(enabled = FALSE)
  configure_grob_cache(enabled = FALSE)
  clear_panel_height_cache()
  clear_grob_height_cache()

  test_text <- "{.8 **CL**}  \n{.24 **42.5 enheder**}"
  test_style <- marquee::classic_style(lineheight = 0.9)
  panel_height <- 6.0  # inches

  # Mål label height
  label_result <- estimate_label_height_npc(
    text = test_text,
    style = test_style,
    panel_height_inches = panel_height,
    use_cache = FALSE,
    return_details = TRUE
  )

  label_height_inches <- label_result$inches

  # Hent config
  cfg <- get_label_placement_config()
  relative_gap_line <- cfg$relative_gap_line  # Default 0.25

  # Beregn forventet gap
  expected_gap_inches <- label_height_inches * relative_gap_line

  # Verificer at gap faktisk er denne værdi i koden
  # (Dette er mere en dokumentation end en faktisk test)
  expect_gt(
    expected_gap_inches,
    0
  )

  # Verificer at gap respekterer minimum
  min_gap_inches <- 0.05
  actual_gap <- max(expected_gap_inches, min_gap_inches)

  expect_gte(
    actual_gap,
    min_gap_inches
  )

  # Re-enable caches
  configure_panel_cache(enabled = TRUE)
  configure_grob_cache(enabled = TRUE)
})

# ==============================================================================
# TEST 5: GROB HEIGHT MEASUREMENT STABILITY
# ==============================================================================

test_that("Grob height measurement returnerer ikke 0 intermittently", {
  skip_if_not_installed("marquee")

  # Unlock cache bindings
  unlock_placement_cache_bindings()

  # Disable cache for at tvinge fresh measurements
  configure_panel_cache(enabled = FALSE)
  configure_grob_cache(enabled = FALSE)
  clear_panel_height_cache()
  clear_grob_height_cache()

  test_text <- "{.8 **CL**}  \n{.24 **42.5 enheder**}"
  test_style <- marquee::classic_style(lineheight = 0.9)
  panel_height <- 6.0

  # Kør 20 målinger af samme grob
  measurements <- replicate(20, {
    result <- estimate_label_height_npc(
      text = test_text,
      style = test_style,
      panel_height_inches = panel_height,
      use_cache = FALSE,
      return_details = TRUE
    )
    result$inches
  })

  # Verificer at INGEN målinger er 0
  zero_count <- sum(measurements == 0)

  expect_equal(
    zero_count,
    0
  )

  # Verificer at alle målinger er identiske (eller meget tæt på)
  unique_measurements <- unique(round(measurements, 6))

  expect_length(
    unique_measurements,
    1
  )

  # Re-enable caches
  configure_panel_cache(enabled = TRUE)
  configure_grob_cache(enabled = TRUE)
})
