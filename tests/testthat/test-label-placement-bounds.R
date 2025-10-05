# ==============================================================================
# TEST: Label Placement Bounds Respekt
# ==============================================================================
# Verificerer at propose_single_label() respekterer panel padding (pad_top/pad_bot)
# også ved flip-scenarios, ved at bruge clamp_to_bounds() i stedet for clamp01().
#
# Task 1.1: Fix propose_single_label() bounds problem
# ==============================================================================

# Source the standalone functions
if (!exists("propose_single_label")) {
  source("utils_standalone_label_placement.R")
}

test_that("propose_single_label respekterer bounds ved flip fra under til over", {
  # Test scenario: Stor label (30% af panel), meget lav linje
  # Foretrækker "under" men må flippe til "over"
  # Ved flip skal den respektere pad_top, ikke bare clampe til 1.0

  result <- propose_single_label(
    y_line_npc = 0.05,      # Meget lav linje (5% fra bunden)
    pref_side = "under",    # Foretrækker under
    label_h = 0.3,          # Stor label (30%)
    gap = 0.01,             # 1% gap til linje
    pad_top = 0.01,         # 1% top padding
    pad_bot = 0.01          # 1% bottom padding
  )

  # Skal flippe til over (fordi under ikke passer)
  expect_equal(result$side, "over")

  # Skal respektere bounds:
  # low_bound = pad_bot + half = 0.01 + 0.15 = 0.16
  # high_bound = 1 - pad_top - half = 1 - 0.01 - 0.15 = 0.84
  low_bound <- 0.01 + 0.15
  high_bound <- 1 - 0.01 - 0.15

  expect_gte(result$center, low_bound)
  expect_lte(result$center, high_bound)
})

test_that("propose_single_label respekterer bounds ved flip fra over til under", {
  # Test scenario: Stor label, meget høj linje
  # Foretrækker "over" men må flippe til "under"

  result <- propose_single_label(
    y_line_npc = 0.95,      # Meget høj linje (95% fra bunden)
    pref_side = "over",     # Foretrækker over
    label_h = 0.3,          # Stor label (30%)
    gap = 0.01,             # 1% gap til linje
    pad_top = 0.01,         # 1% top padding
    pad_bot = 0.01          # 1% bottom padding
  )

  # Skal flippe til under
  expect_equal(result$side, "under")

  # Skal respektere bounds
  low_bound <- 0.01 + 0.15
  high_bound <- 1 - 0.01 - 0.15

  expect_gte(result$center, low_bound)
  expect_lte(result$center, high_bound)
})

test_that("propose_single_label placerer korrekt når foretrukken side passer", {
  # Test scenario: Normal label, central linje, "under" passer fint

  result <- propose_single_label(
    y_line_npc = 0.5,       # Midt i panel
    pref_side = "under",    # Foretrækker under
    label_h = 0.13,         # Normal label højde
    gap = 0.01,             # 1% gap
    pad_top = 0.01,         # 1% top padding
    pad_bot = 0.01          # 1% bottom padding
  )

  # Skal bruge foretrukken side
  expect_equal(result$side, "under")

  # Skal placeres korrekt: linje - gap - half
  expected_center <- 0.5 - 0.01 - 0.065
  expect_equal(result$center, expected_center, tolerance = 0.001)
})

test_that("clamp_to_bounds fungerer korrekt", {
  # Test den nye helper function

  # Værdi inden for bounds
  expect_equal(clamp_to_bounds(0.5, 0.2, 0.8), 0.5)

  # Værdi under lower bound
  expect_equal(clamp_to_bounds(0.1, 0.2, 0.8), 0.2)

  # Værdi over upper bound
  expect_equal(clamp_to_bounds(0.9, 0.2, 0.8), 0.8)

  # Vektor input
  result <- clamp_to_bounds(c(0.1, 0.5, 0.9), 0.2, 0.8)
  expect_equal(result, c(0.2, 0.5, 0.8))
})

test_that("propose_single_label håndterer edge case: meget lille padding", {
  # Test med minimal padding (0.5%)

  result <- propose_single_label(
    y_line_npc = 0.03,      # Meget lav linje
    pref_side = "under",    # Foretrækker under
    label_h = 0.2,          # Medium-stor label
    gap = 0.005,            # Lille gap
    pad_top = 0.005,        # Minimal top padding
    pad_bot = 0.005         # Minimal bottom padding
  )

  # Skal respektere selv minimal padding
  low_bound <- 0.005 + 0.1  # 0.105
  high_bound <- 1 - 0.005 - 0.1  # 0.895

  expect_gte(result$center, low_bound)
  expect_lte(result$center, high_bound)
})

test_that("propose_single_label håndterer edge case: meget stor label", {
  # Test med ekstremt stor label (50% af panel)

  result <- propose_single_label(
    y_line_npc = 0.1,       # Lav linje
    pref_side = "under",    # Foretrækker under
    label_h = 0.5,          # Ekstremt stor label (50%)
    gap = 0.02,             # 2% gap
    pad_top = 0.01,         # 1% top padding
    pad_bot = 0.01          # 1% bottom padding
  )

  # Skal respektere bounds selv med stor label
  low_bound <- 0.01 + 0.25  # 0.26
  high_bound <- 1 - 0.01 - 0.25  # 0.74

  expect_gte(result$center, low_bound)
  expect_lte(result$center, high_bound)

  # Forvent at den må flippe til over
  expect_equal(result$side, "over")
})
