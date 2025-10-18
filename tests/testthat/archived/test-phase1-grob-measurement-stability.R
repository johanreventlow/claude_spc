# ==============================================================================
# PHASE 1: GROB MEASUREMENT STABILITY - DIAGNOSTIC DEEP-DIVE
# ==============================================================================
#
# Formål: Systematisk undersøgelse af non-deterministisk adfærd i grob height
# measurements. Phase 0 viste at label placement ikke er reproducerbar selv
# uden cache - dette tyder på fundamental instabilitet i measurement pipeline.
#
# Test-strategi:
# 1. Pure grob measurement test (ingen qicharts2, bare marquee)
# 2. Device context stability
# 3. Viewport state consistency
# 4. convertHeight() reliability
#
# ==============================================================================

library(testthat)

# ==============================================================================
# TEST 1: PURE GROB MEASUREMENT - 20 IDENTISKE MÅLINGER
# ==============================================================================

test_that("Grob height measurement er deterministisk (20 målinger)", {
  skip_if_not_installed("marquee")

  # Unlock og disable cache
  unlock_placement_cache_bindings()
  configure_grob_cache(enabled = FALSE)
  clear_grob_height_cache()

  # Test setup
  test_text <- "{.8 **CL**}  \n{.24 **42.5 enheder**}"
  test_style <- marquee::classic_style(lineheight = 0.9)
  panel_height <- 6.0  # inches

  # Kør 20 målinger
  cat("\n=== STARTING 20 GROB MEASUREMENTS ===\n")
  measurements <- replicate(20, {
    result <- estimate_label_height_npc(
      text = test_text,
      style = test_style,
      panel_height_inches = panel_height,
      use_cache = FALSE,
      return_details = TRUE
    )

    # Print summary for each measurement
    cat(sprintf("Measurement: npc=%.6f inches=%.6f panel=%.4f\n",
                result$npc, result$inches, result$panel_height_inches))

    result$inches
  })
  cat("=== MEASUREMENTS COMPLETE ===\n\n")

  # Analyze results
  cat(sprintf("Mean: %.6f\n", mean(measurements)))
  cat(sprintf("SD: %.8f\n", sd(measurements)))
  cat(sprintf("Min: %.6f\n", min(measurements)))
  cat(sprintf("Max: %.6f\n", max(measurements)))
  cat(sprintf("Range: %.6f\n", max(measurements) - min(measurements)))
  cat(sprintf("Unique values: %d\n", length(unique(round(measurements, 10)))))

  # Test 1: No zeros
  zero_count <- sum(measurements == 0)
  expect_equal(zero_count, 0, label = "No measurements should be zero")

  # Test 2: All finite
  expect_true(all(is.finite(measurements)), label = "All measurements should be finite")

  # Test 3: Reasonable range (label should be between 0.1 and 2 inches)
  expect_true(all(measurements > 0.1 & measurements < 2.0),
              label = "All measurements should be in reasonable range (0.1-2.0 inches)")

  # Test 4: Low variability (coefficient of variation < 1%)
  cv <- sd(measurements) / mean(measurements)
  cat(sprintf("\nCoefficient of Variation: %.4f%% (target: <1%%)\n", cv * 100))

  expect_lt(cv, 0.01, label = "CV should be < 1% for deterministic measurements")

  # Re-enable cache
  configure_grob_cache(enabled = TRUE)
})

# ==============================================================================
# TEST 2: DEVICE CONTEXT STABILITY
# ==============================================================================

test_that("Device context forbliver stabil under målinger", {
  skip_if_not_installed("marquee")

  unlock_placement_cache_bindings()
  configure_grob_cache(enabled = FALSE)
  clear_grob_height_cache()

  test_text <- "{.8 **CL**}  \n{.24 **42.5 enheder**}"
  test_style <- marquee::classic_style(lineheight = 0.9)

  # Capture initial device state
  initial_dev <- grDevices::dev.cur()
  initial_size <- grDevices::dev.size()

  # Kør 10 målinger og tjek device state
  for (i in 1:10) {
    result <- estimate_label_height_npc(
      text = test_text,
      style = test_style,
      panel_height_inches = 6.0,
      use_cache = FALSE
    )

    # Verify device hasn't changed
    current_dev <- grDevices::dev.cur()
    current_size <- grDevices::dev.size()

    expect_equal(current_dev, initial_dev,
                 label = sprintf("Device should remain same (iteration %d)", i))

    expect_equal(current_size, initial_size,
                 label = sprintf("Device size should remain same (iteration %d)", i))
  }

  configure_grob_cache(enabled = TRUE)
})

# ==============================================================================
# TEST 3: VIEWPORT STATE CONSISTENCY
# ==============================================================================

test_that("Viewport state er konsistent før og efter målinger", {
  skip_if_not_installed("marquee")

  unlock_placement_cache_bindings()
  configure_grob_cache(enabled = FALSE)
  clear_grob_height_cache()

  test_text <- "{.8 **CL**}  \n{.24 **42.5 enheder**}"
  test_style <- marquee::classic_style(lineheight = 0.9)

  # Check viewport depth before measurements
  vp_depth_before <- length(grid::current.vpTree())

  # Run measurements
  for (i in 1:5) {
    result <- estimate_label_height_npc(
      text = test_text,
      style = test_style,
      panel_height_inches = 6.0,
      use_cache = FALSE
    )

    # Check viewport depth after each measurement
    vp_depth_after <- length(grid::current.vpTree())

    expect_equal(vp_depth_after, vp_depth_before,
                 label = sprintf("Viewport stack depth should remain constant (iteration %d)", i))
  }

  configure_grob_cache(enabled = TRUE)
})

# ==============================================================================
# TEST 4: convertHeight() CONSISTENCY
# ==============================================================================

test_that("grid::convertHeight() er deterministisk", {
  skip_if_not_installed("marquee")

  # Create a simple text grob (not marquee)
  g <- grid::textGrob("Test", gp = grid::gpar(fontsize = 12))

  # Measure height 20 times
  heights_npc <- replicate(20, {
    grid::convertHeight(grid::grobHeight(g), "npc", valueOnly = TRUE)
  })

  heights_inches <- replicate(20, {
    grid::convertHeight(grid::grobHeight(g), "inches", valueOnly = TRUE)
  })

  # All measurements should be identical
  unique_npc <- unique(heights_npc)
  unique_inches <- unique(heights_inches)

  expect_length(unique_npc, 1,
                label = "All NPC measurements should be identical")

  expect_length(unique_inches, 1,
                label = "All inches measurements should be identical")

  cat(sprintf("\nconvertHeight() consistency: NPC=%.6f (n=%d), Inches=%.6f (n=%d)\n",
              unique_npc[1], length(heights_npc),
              unique_inches[1], length(heights_inches)))
})

# ==============================================================================
# TEST 5: MARQUEE-SPECIFIC MEASUREMENT STABILITY
# ==============================================================================

test_that("Marquee grobs har stabil height measurement", {
  skip_if_not_installed("marquee")

  # Test forskellige marquee text styles
  test_cases <- list(
    simple = "{.8 **Test**}",
    multiline = "{.8 **Line1**}  \n{.8 **Line2**}",
    mixed_sizes = "{.8 **Header**}  \n{.24 **Value**}",
    complex = "{.8 **CL**}  \n{.24 **42.5 enheder**}"
  )

  for (name in names(test_cases)) {
    text <- test_cases[[name]]
    style <- marquee::classic_style(lineheight = 0.9)

    cat(sprintf("\nTesting: %s ('%s')\n", name, substring(text, 1, 30)))

    # Measure 10 times
    measurements <- replicate(10, {
      g <- marquee::marquee_grob(text = text, x = 0.5, y = 0.5, style = style)
      g_rendered <- tryCatch({
        grid::makeContent(g)
      }, error = function(e) {
        g
      })
      h <- grid::grobHeight(g_rendered)
      as.numeric(grid::convertHeight(h, "inches", valueOnly = TRUE))
    })

    # Check consistency
    unique_measurements <- unique(round(measurements, 10))
    cv <- sd(measurements) / mean(measurements)

    cat(sprintf("  Mean: %.6f, SD: %.8f, CV: %.4f%%, Unique: %d\n",
                mean(measurements), sd(measurements), cv * 100, length(unique_measurements)))

    expect_lte(length(unique_measurements), 2,
               label = sprintf("%s: Should have <= 2 unique values (allowing minor float variance)", name))

    expect_lt(cv, 0.001,
              label = sprintf("%s: CV should be < 0.1%% for stable measurements", name))
  }
})
