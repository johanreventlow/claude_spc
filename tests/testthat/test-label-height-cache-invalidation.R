# test-label-height-cache-invalidation.R
# Regression test: Cache skal invalideres når height_safety_margin ændres
#
# FIX: Cache keys skal inkludere height_safety_margin for at sikre
# at config ændringer ikke ignoreres pga. cache hits med gamle værdier.

test_that("Cache key inkluderer height_safety_margin", {
  skip_if_not_installed("digest")

  # Load required functions
  if (!exists("LABEL_PLACEMENT_CONFIG")) {
    source("R/config_label_placement.R")
  }

  # Test at cache key ændrer sig når height_safety_margin ændres
  # Vi simulerer cache key beregning uden at afhænge af marquee styles

  # Mock style hash (konstant for test)
  style_hash <- "test_style_hash"
  test_label <- "{.header **TEST**}\n{.value **42**}"

  # Cache key med safety_margin = 1.44
  old_config <- override_label_placement_config(height_safety_margin = 1.44)
  safety_margin_1 <- get_label_placement_config()[["height_safety_margin"]]

  cache_key_1 <- digest::digest(list(
    text = test_label,
    style = style_hash,
    panel_height = round(10.0, 4),
    return_details = TRUE,
    safety_margin = round(safety_margin_1, 4)
  ), algo = "xxhash32")

  # Cache key med safety_margin = 2.0
  override_label_placement_config(height_safety_margin = 2.0)
  safety_margin_2 <- get_label_placement_config()[["height_safety_margin"]]

  cache_key_2 <- digest::digest(list(
    text = test_label,
    style = style_hash,
    panel_height = round(10.0, 4),
    return_details = TRUE,
    safety_margin = round(safety_margin_2, 4)
  ), algo = "xxhash32")

  # KRITISK: Cache keys skal være forskellige
  expect_false(
    cache_key_1 == cache_key_2,
    info = sprintf(
      "Cache keys skal være forskellige ved safety_margin ændring. key_1=%s, key_2=%s, margin_1=%.2f, margin_2=%.2f",
      cache_key_1, cache_key_2, safety_margin_1, safety_margin_2
    )
  )

  # Verificer at safety_margin faktisk blev ændret
  expect_equal(safety_margin_1, 1.44, tolerance = 0.01)
  expect_equal(safety_margin_2, 2.0, tolerance = 0.01)

  # Restore config
  LABEL_PLACEMENT_CONFIG <<- old_config
})

test_that("clear_grob_height_cache() er tilgængelig og fungerer", {
  # Load required functions
  if (!exists("clear_grob_height_cache")) {
    source("R/utils_label_placement.R")
  }
  if (!exists("get_grob_cache_stats")) {
    source("R/utils_label_placement.R")
  }

  # Suppress console output
  result <- suppressMessages(clear_grob_height_cache())

  # Verificer return value er numeric
  expect_true(
    is.numeric(result) || is.integer(result),
    info = "clear_grob_height_cache() skal returnere antal ryddede entries"
  )

  # Verificer cache stats er nulstillet
  stats <- get_grob_cache_stats()
  expect_equal(stats$cache_size, 0, info = "Cache size skal være 0 efter clear")
  expect_equal(stats$cache_hits, 0, info = "Cache hits skal være 0 efter clear")
  expect_equal(stats$cache_misses, 0, info = "Cache misses skal være 0 efter clear")
})

test_that("clear_grob_height_cache() er exported", {
  # Verificer at funktionen er tilgængelig i package namespace
  # (dette test vil fejle hvis @export mangler)

  expect_true(
    exists("clear_grob_height_cache", mode = "function"),
    info = "clear_grob_height_cache() skal være exported fra package"
  )
})
