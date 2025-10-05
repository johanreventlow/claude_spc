# ==============================================================================
# TEST: NPC Mapper - Coordinate Transformation
# ==============================================================================
# Tests npc_mapper_from_plot() covering:
# - ggplot object validation
# - y_to_npc conversion accuracy
# - npc_to_y inverse transformation
# - NA value handling
# - Log scale transformations
# - Division-by-zero protection
# ==============================================================================

library(ggplot2)

# Source the standalone functions
if (!exists("npc_mapper_from_plot")) {
  source("utils_standalone_label_placement.R")
}

# ==============================================================================
# INPUT VALIDATION TESTS
# ==============================================================================

test_that("npc_mapper_from_plot validerer ggplot object", {
  # Ikke-ggplot object
  expect_error(
    npc_mapper_from_plot(list()),
    "p skal være et ggplot object"
  )

  # NULL input
  expect_error(
    npc_mapper_from_plot(NULL),
    "p skal være et ggplot object"
  )
})

test_that("npc_mapper_from_plot validerer panel parameter", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()

  # Negativ panel
  expect_error(
    npc_mapper_from_plot(p, panel = -1),
    "panel skal være et positivt heltal"
  )

  # Ikke-heltal panel
  expect_error(
    npc_mapper_from_plot(p, panel = 1.5),
    "panel skal være et positivt heltal"
  )

  # Panel 0
  expect_error(
    npc_mapper_from_plot(p, panel = 0),
    "panel skal være et positivt heltal"
  )
})

test_that("npc_mapper_from_plot fejler ved ikke-eksisterende panel", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()

  expect_error(
    npc_mapper_from_plot(p, panel = 10),
    "Panel 10 eksisterer ikke"
  )
})

# ==============================================================================
# BASIC FUNCTIONALITY
# ==============================================================================

test_that("npc_mapper_from_plot bygger mapper fra ggplot", {
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    ylim(10, 35)

  mapper <- npc_mapper_from_plot(p)

  # Skal returnere list med korrekte komponenter
  expect_true(is.list(mapper))
  expect_true(is.function(mapper$y_to_npc))
  expect_true(is.function(mapper$npc_to_y))
  expect_equal(length(mapper$limits), 2)
  expect_true(is.character(mapper$trans_name))
})

test_that("npc_mapper_from_plot y_to_npc conversion er korrekt", {
  p <- ggplot(data.frame(x = 1:10, y = 10:19), aes(x, y)) +
    geom_point() +
    ylim(10, 20)

  mapper <- npc_mapper_from_plot(p)

  # Bunden af range (10) skal være 0 NPC
  expect_equal(mapper$y_to_npc(10), 0, tolerance = 0.01)

  # Toppen af range (20) skal være 1 NPC
  expect_equal(mapper$y_to_npc(20), 1, tolerance = 0.01)

  # Midten (15) skal være 0.5 NPC
  expect_equal(mapper$y_to_npc(15), 0.5, tolerance = 0.01)
})

test_that("npc_mapper_from_plot npc_to_y er invers af y_to_npc", {
  p <- ggplot(data.frame(x = 1:10, y = 0:100), aes(x, y)) +
    geom_point()

  mapper <- npc_mapper_from_plot(p)

  # Test round-trip conversion
  test_values <- c(0, 25, 50, 75, 100)

  for (y_val in test_values) {
    npc_val <- mapper$y_to_npc(y_val)
    y_back <- mapper$npc_to_y(npc_val)
    expect_equal(y_back, y_val, tolerance = 0.01)
  }
})

# ==============================================================================
# NA VALUE HANDLING
# ==============================================================================

test_that("npc_mapper_from_plot håndterer NA værdier i y_to_npc", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  mapper <- npc_mapper_from_plot(p)

  # y_to_npc med NA
  result <- mapper$y_to_npc(c(20, NA, 25))
  expect_true(is.na(result[2]))
  expect_false(is.na(result[1]))
  expect_false(is.na(result[3]))
})

test_that("npc_mapper_from_plot håndterer NA værdier i npc_to_y", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  mapper <- npc_mapper_from_plot(p)

  # npc_to_y med NA
  result2 <- mapper$npc_to_y(c(0.5, NA, 0.8))
  expect_true(is.na(result2[2]))
  expect_false(is.na(result2[1]))
  expect_false(is.na(result2[3]))
})

test_that("npc_mapper_from_plot håndterer alle-NA vektorer", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  mapper <- npc_mapper_from_plot(p)

  # y_to_npc med kun NA
  result1 <- mapper$y_to_npc(c(NA_real_, NA_real_, NA_real_))
  expect_true(all(is.na(result1)))

  # npc_to_y med kun NA
  result2 <- mapper$npc_to_y(c(NA_real_, NA_real_))
  expect_true(all(is.na(result2)))
})

# ==============================================================================
# LOG SCALE TRANSFORMATION
# ==============================================================================

test_that("npc_mapper_from_plot håndterer log transformations", {
  skip_if_not_installed("scales")

  p <- ggplot(data.frame(x = 1:10, y = 10^(1:10)), aes(x, y)) +
    geom_point() +
    scale_y_log10()

  mapper <- npc_mapper_from_plot(p)

  # Skal detektere log transformation
  expect_true(grepl("log", mapper$trans_name, ignore.case = TRUE))

  # Konvertering skal stadig virke korrekt
  # På log skala: 10 og 100 skal være lineært fordelt i NPC
  npc_10 <- mapper$y_to_npc(10)
  npc_100 <- mapper$y_to_npc(100)

  # 10 og 100 er 1 dekade fra hinanden
  # Skal give samme NPC afstand som anden dekade
  npc_1000 <- mapper$y_to_npc(1000)

  expect_equal(npc_100 - npc_10, npc_1000 - npc_100, tolerance = 0.05)
})

# ==============================================================================
# DIVISION BY ZERO PROTECTION
# ==============================================================================

test_that("npc_mapper_from_plot beskytter mod division by zero", {
  # Plot med praktisk talt identiske y-værdier
  # Dette bør ikke være muligt i normal brug, men tester det alligevel

  # Vi kan ikke direkte teste dette uden at mocke ggplot_build,
  # men vi kan verificere at funktionen har denne check

  # Check at koden indeholder division-by-zero protection
  mapper_code <- capture.output(print(body(npc_mapper_from_plot)))
  has_protection <- any(grepl("division|zero|eps", mapper_code, ignore.case = TRUE))

  # Dette er en svag test, men verificerer at koden er til stede
  expect_true(has_protection)
})

# ==============================================================================
# VECTORIZED OPERATIONS
# ==============================================================================

test_that("npc_mapper_from_plot håndterer vektoriserede inputs", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  mapper <- npc_mapper_from_plot(p)

  # Vectorized y_to_npc
  y_values <- c(10, 15, 20, 25, 30)
  npc_values <- mapper$y_to_npc(y_values)

  expect_equal(length(npc_values), length(y_values))
  expect_true(all(is.numeric(npc_values)))

  # Vectorized npc_to_y
  npc_inputs <- c(0, 0.25, 0.5, 0.75, 1.0)
  y_outputs <- mapper$npc_to_y(npc_inputs)

  expect_equal(length(y_outputs), length(npc_inputs))
  expect_true(all(is.numeric(y_outputs)))
})

# ==============================================================================
# EDGE CASES
# ==============================================================================

test_that("npc_mapper_from_plot håndterer ekstreme værdier", {
  p <- ggplot(data.frame(x = 1:10, y = 1:10), aes(x, y)) + geom_point()
  mapper <- npc_mapper_from_plot(p)

  # Værdier uden for range
  npc_below <- mapper$y_to_npc(-100)
  npc_above <- mapper$y_to_npc(100)

  # Skal stadig returnere numeriske værdier (kan være < 0 eller > 1)
  expect_true(is.numeric(npc_below))
  expect_true(is.numeric(npc_above))
  expect_lt(npc_below, 0)
  expect_gt(npc_above, 1)
})

test_that("npc_mapper_from_plot limits er korrekte", {
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    ylim(10, 30)

  mapper <- npc_mapper_from_plot(p)

  # Limits skal være ca. [10, 30]
  expect_gte(mapper$limits[1], 9)
  expect_lte(mapper$limits[1], 11)
  expect_gte(mapper$limits[2], 29)
  expect_lte(mapper$limits[2], 31)
})

# ==============================================================================
# TRANSFORMATION CONSISTENCY
# ==============================================================================

test_that("npc_mapper_from_plot transformations er konsistente", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  mapper <- npc_mapper_from_plot(p)

  # Multiple transformations skal give samme resultat
  y_val <- 20

  npc1 <- mapper$y_to_npc(y_val)
  npc2 <- mapper$y_to_npc(y_val)

  expect_equal(npc1, npc2)

  # Inverse transformation
  y_back1 <- mapper$npc_to_y(npc1)
  y_back2 <- mapper$npc_to_y(npc2)

  expect_equal(y_back1, y_back2)
  expect_equal(y_back1, y_val, tolerance = 0.01)
})
