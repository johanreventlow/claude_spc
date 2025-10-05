# ==============================================================================
# TEST: Label Height Estimation via Grob Måling
# ==============================================================================
# Verificerer at estimate_label_height_npc() måler faktisk højde præcist
# ved at oprette marquee grobs og måle dem, i stedet for at estimere med
# magic numbers.
#
# Task 1.2: Implementer præcis estimate_label_height_npc() med grob-måling
# ==============================================================================

# Source the standalone functions
if (!exists("estimate_label_height_npc")) {
  source("utils_standalone_label_placement.R")
}

test_that("estimate_label_height_npc returnerer positiv værdi", {
  label <- "{.8 **HEADER**}  \n{.24 **VALUE**}"

  style <- marquee::modify_style(
    marquee::classic_style(),
    "p",
    align = "right",
    margin = marquee::trbl(0)
  )

  h <- estimate_label_height_npc(
    text = label,
    style = style,
    base_size = 14
  )

  # Højden skal være positiv
  expect_gt(h, 0)

  # Højden skal være rimelig (mellem 2% og 30% af panel)
  expect_gte(h, 0.02)
  expect_lte(h, 0.3)
})

test_that("estimate_label_height_npc skalerer med base_size", {
  label <- "{.8 **HEADER**}  \n{.24 **VALUE**}"

  style <- marquee::modify_style(
    marquee::classic_style(),
    "p",
    align = "right",
    margin = marquee::trbl(0)
  )

  h1 <- estimate_label_height_npc(text = label, style = style, base_size = 14)
  h2 <- estimate_label_height_npc(text = label, style = style, base_size = 20)

  # Større base_size skal give større højde
  expect_gt(h2, h1)

  # Ratio skal være ca. 20/14 (med tolerance for non-lineær scaling + safety margin)
  expect_gt(h2/h1, 1.2)  # Mindst 20% større
  expect_lt(h2/h1, 2.0)  # Men ikke dobbelt så stor
})

test_that("estimate_label_height_npc håndterer fejl gracefully", {
  # Ugyldig markup - skal bruge fallback
  h <- estimate_label_height_npc(
    text = "invalid markup without marquee syntax",
    fallback_npc = 0.1
  )

  # Skal returnere en værdi (enten målt eller fallback)
  expect_true(is.numeric(h))
  expect_gt(h, 0)
})

test_that("estimate_label_height_npc returnerer konsistent værdi", {
  label <- "{.12 **MÅL**}  \n{.36 **≥90%**}"

  style <- marquee::modify_style(
    marquee::classic_style(),
    "p",
    align = "right",
    margin = marquee::trbl(0)
  )

  # Kald funktionen 3 gange
  h1 <- estimate_label_height_npc(text = label, style = style, base_size = 14)
  h2 <- estimate_label_height_npc(text = label, style = style, base_size = 14)
  h3 <- estimate_label_height_npc(text = label, style = style, base_size = 14)

  # Samme input skal give samme output
  expect_equal(h1, h2)
  expect_equal(h2, h3)
})

test_that("estimate_label_height_npc måler forskellige label størrelser", {
  style <- marquee::modify_style(
    marquee::classic_style(),
    "p",
    align = "right",
    margin = marquee::trbl(0)
  )

  # Lille label (1 linje, lille font)
  small_label <- "{.10 **Small**}"
  h_small <- estimate_label_height_npc(text = small_label, style = style, base_size = 14)

  # Medium label (2 linjer, standard font)
  medium_label <- "{.8 **HEADER**}  \n{.24 **VALUE**}"
  h_medium <- estimate_label_height_npc(text = medium_label, style = style, base_size = 14)

  # Stor label (2 linjer, store fonts)
  large_label <- "{.16 **BIG HEADER**}  \n{.48 **BIG VALUE**}"
  h_large <- estimate_label_height_npc(text = large_label, style = style, base_size = 14)

  # Forvent progression: small < medium < large
  expect_lt(h_small, h_medium)
  expect_lt(h_medium, h_large)

  # Forvent at medium er større end small (mindst 50% større pga. ekstra linje + større font)
  expect_gt(h_medium / h_small, 1.5)
})

test_that("estimate_label_height_npc respekterer safety margin", {
  label <- "{.8 **TEST**}  \n{.24 **VALUE**}"

  style <- marquee::modify_style(
    marquee::classic_style(),
    "p",
    align = "right",
    margin = marquee::trbl(0)
  )

  h <- estimate_label_height_npc(text = label, style = style, base_size = 14)

  # Med 5% safety margin skal højden være mindst rå-højden
  # Vi kan ikke teste direkte uden at kende rå-højden, men vi kan verificere
  # at den er rimelig og ikke for lille
  expect_gte(h, 0.05)  # Mindst 5% af panel for 2-line label
})

test_that("estimate_label_height_npc fungerer med default parametre", {
  label <- "{.8 **DEFAULT**}  \n{.24 **TEST**}"

  # Kald med minimale argumenter (bruger defaults)
  h <- estimate_label_height_npc(text = label)

  # Skal stadig returnere rimelig værdi
  expect_true(is.numeric(h))
  expect_gt(h, 0)
  expect_lt(h, 0.5)
})
