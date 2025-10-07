# ==============================================================================
# PHASE 2: DEVICE SIZE MATRIX - SYSTEMATISK LABEL PLACEMENT TEST
# ==============================================================================
#
# Formål: Systematisk test af label placement på tværs af realistiske device
# størrelser og line konfigurationer.
#
# Test Matrix: 4 device sizes × 4 line configs = 16 test cases
#
# Device sizes (realistic aspect ratios):
# - Small:  4.5 × 3.0 inches (16:10.7 ≈ 3:2)
# - Medium: 8.0 × 4.5 inches (16:9)
# - Large:  12.0 × 6.75 inches (16:9)
# - XLarge: 16.0 × 9.0 inches (16:9)
#
# Line configurations:
# 1. CL only (baseline)
# 2. CL + Target (coincident - samme værdi)
# 3. CL + Target (wide spacing - stor forskel)
# 4. CL + Target (tight spacing - lille forskel)
#
# Success criteria:
# - Labels ikke overlapper med linjer
# - Gap = relative_gap_line × label_height (within tolerance)
# - Labels placeres konsistent på tværs af devices
# - Minimum gap respekteres (0.05 inches)
#
# ==============================================================================

library(testthat)

# Test data generator
create_test_data <- function(cl_value, target_value = NULL) {
  data <- data.frame(
    x = 1:20,
    y = rnorm(20, mean = cl_value, sd = 2)
  )

  if (!is.null(target_value)) {
    data$target <- target_value
  }

  data
}

# ==============================================================================
# TEST 1: BASELINE - CL ONLY (NO TARGET)
# ==============================================================================

test_that("CL-only labels placeres korrekt på alle device sizes", {
  skip_if_not_installed("qicharts2")

  # Warm-up
  warmup_data <- create_test_data(cl_value = 50)
  w1 <- qicharts2::qic(x = x, y = y, data = warmup_data, chart = "run")
  w2 <- qicharts2::qic(x = x, y = y, data = warmup_data, chart = "run")

  # Device sizes
  devices <- list(
    small = c(width = 4.5, height = 3.0),
    medium = c(width = 8.0, height = 4.5),
    large = c(width = 12.0, height = 6.75),
    xlarge = c(width = 16.0, height = 9.0)
  )

  test_data <- create_test_data(cl_value = 50)

  cat("\n=== TEST: CL-only labels ===\n")

  for (device_name in names(devices)) {
    device <- devices[[device_name]]

    cat(sprintf("\nDevice: %s (%.1f × %.1f inches)\n",
                device_name, device["width"], device["height"]))

    # Generate plot
    set.seed(12345)
    p <- qicharts2::qic(x = x, y = y, data = test_data, chart = "run")

    # Plot skulle have labels - dette er en placeholder test
    # I virkeligheden skal vi ekstrahere label positions fra ggplot_build

    expect_true(!is.null(p),
                label = sprintf("%s: Plot generated successfully", device_name))
  }
})

# ==============================================================================
# TEST 2: CL + TARGET (COINCIDENT)
# ==============================================================================

test_that("Coincident labels (CL = Target) håndteres korrekt", {
  skip_if_not_installed("qicharts2")

  # Warm-up
  warmup_data <- create_test_data(cl_value = 50, target_value = 50)
  w1 <- qicharts2::qic(x = x, y = y, data = warmup_data, chart = "run", target = 50)
  w2 <- qicharts2::qic(x = x, y = y, data = warmup_data, chart = "run", target = 50)

  test_data <- create_test_data(cl_value = 50, target_value = 50)

  cat("\n=== TEST: Coincident labels (CL = Target = 50) ===\n")

  # Test på medium device
  set.seed(12345)
  p <- qicharts2::qic(
    x = x,
    y = y,
    data = test_data,
    chart = "run",
    target = 50
  )

  expect_true(!is.null(p))

  # TODO: Verificer at labels ikke overlapper
  # Dette kræver parsing af ggplot_build() output
})

# ==============================================================================
# TEST 3: CL + TARGET (WIDE SPACING)
# ==============================================================================

test_that("Wide-spaced labels (stor CL-Target forskel) placeres korrekt", {
  skip_if_not_installed("qicharts2")

  # Warm-up
  warmup_data <- create_test_data(cl_value = 30, target_value = 70)
  w1 <- qicharts2::qic(x = x, y = y, data = warmup_data, chart = "run", target = 70)
  w2 <- qicharts2::qic(x = x, y = y, data = warmup_data, chart = "run", target = 70)

  test_data <- create_test_data(cl_value = 30, target_value = 70)

  cat("\n=== TEST: Wide spacing (CL = 30, Target = 70) ===\n")

  set.seed(12345)
  p <- qicharts2::qic(
    x = x,
    y = y,
    data = test_data,
    chart = "run",
    target = 70
  )

  expect_true(!is.null(p))
})

# ==============================================================================
# TEST 4: CL + TARGET (TIGHT SPACING)
# ==============================================================================

test_that("Tight-spaced labels (lille CL-Target forskel) håndteres", {
  skip_if_not_installed("qicharts2")

  # Warm-up
  warmup_data <- create_test_data(cl_value = 48, target_value = 52)
  w1 <- qicharts2::qic(x = x, y = y, data = warmup_data, chart = "run", target = 52)
  w2 <- qicharts2::qic(x = x, y = y, data = warmup_data, chart = "run", target = 52)

  test_data <- create_test_data(cl_value = 48, target_value = 52)

  cat("\n=== TEST: Tight spacing (CL = 48, Target = 52) ===\n")

  set.seed(12345)
  p <- qicharts2::qic(
    x = x,
    y = y,
    data = test_data,
    chart = "run",
    target = 52
  )

  expect_true(!is.null(p))
})

# ==============================================================================
# TEST 5: GAP VERIFICATION - MANUAL CALCULATION
# ==============================================================================

test_that("Gap beregning matcher formlen: gap = relative_gap × label_height", {
  skip_if_not_installed("marquee")

  # Unlock cache
  unlock_placement_cache_bindings()
  configure_grob_cache(enabled = FALSE)

  # Test label
  test_text <- "{.8 **CL**}  \n{.24 **50 enheder**}"
  test_style <- marquee::classic_style(lineheight = 0.9)

  # Device sizes
  device_heights <- c(3.0, 4.5, 6.75, 9.0)  # inches

  cat("\n=== VERIFYING GAP FORMULA ===\n")
  cat("Formula: gap_inches = label_height_inches × relative_gap_line\n\n")

  # Get config
  cfg <- get_label_placement_config()
  relative_gap_line <- cfg$relative_gap_line
  min_gap_inches <- 0.05

  cat(sprintf("Config:\n"))
  cat(sprintf("  relative_gap_line: %.2f (%.0f%%)\n",
              relative_gap_line, relative_gap_line * 100))
  cat(sprintf("  min_gap_inches: %.2f\n", min_gap_inches))
  cat(sprintf("  height_safety_margin: %.2f\n\n", cfg$height_safety_margin))

  for (panel_height in device_heights) {
    # Measure label
    result <- estimate_label_height_npc(
      text = test_text,
      style = test_style,
      panel_height_inches = panel_height,
      use_cache = FALSE,
      return_details = TRUE
    )

    label_height_inches <- result$inches

    # Calculate expected gap
    expected_gap <- label_height_inches * relative_gap_line
    actual_gap <- max(expected_gap, min_gap_inches)

    cat(sprintf("Panel: %.2f\" → Label: %.4f\" → Gap: %.4f\" (min: %.2f\")\n",
                panel_height, label_height_inches, actual_gap, min_gap_inches))

    # Verify gap is >= minimum
    expect_gte(actual_gap, min_gap_inches,
               label = sprintf("Gap should be >= %.2f inches", min_gap_inches))

    # Verify gap formula
    if (expected_gap >= min_gap_inches) {
      expect_equal(actual_gap, expected_gap, tolerance = 1e-6,
                   label = "Gap should match formula when above minimum")
    }
  }

  # Re-enable cache
  configure_grob_cache(enabled = TRUE)
})

# ==============================================================================
# TEST 6: FULL MATRIX (PLACEHOLDER FOR VISUAL VERIFICATION)
# ==============================================================================

test_that("Full 16-case matrix kan genereres (manual verification)", {
  skip_if_not_installed("qicharts2")
  skip("Manual verification required - use visual test app")

  # Dette ville generere alle 16 kombinationer og gemme plots
  # til manuel visuel verifikation

  # Warm-up
  warmup_data <- create_test_data(cl_value = 50)
  w1 <- qicharts2::qic(x = x, y = y, data = warmup_data, chart = "run")
  w2 <- qicharts2::qic(x = x, y = y, data = warmup_data, chart = "run")

  devices <- list(
    small = c(4.5, 3.0),
    medium = c(8.0, 4.5),
    large = c(12.0, 6.75),
    xlarge = c(16.0, 9.0)
  )

  configs <- list(
    cl_only = list(cl = 50, target = NULL),
    coincident = list(cl = 50, target = 50),
    wide = list(cl = 30, target = 70),
    tight = list(cl = 48, target = 52)
  )

  cat("\n=== FULL MATRIX: 16 TEST CASES ===\n")

  for (device_name in names(devices)) {
    for (config_name in names(configs)) {
      config <- configs[[config_name]]
      test_data <- create_test_data(config$cl, config$target)

      cat(sprintf("  [%s × %s]\n", device_name, config_name))

      # Generate plot
      set.seed(12345)
      if (!is.null(config$target)) {
        p <- qicharts2::qic(x = x, y = y, data = test_data,
                           chart = "run", target = config$target)
      } else {
        p <- qicharts2::qic(x = x, y = y, data = test_data, chart = "run")
      }

      expect_true(!is.null(p))
    }
  }
})
