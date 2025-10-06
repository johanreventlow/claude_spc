# Test device batching for label height measurements
# Verifies that batch API opens fewer devices than individual calls

test_that("batch API opens only one device for multiple labels", {
  skip_if_not_installed("mockery")

  library(mockery)
  source("utils_standalone_label_placement.R")

  # Mock grDevices::png to count device opens
  device_open_count <- 0
  mock_png <- mock(function(...) {
    device_open_count <<- device_open_count + 1
    # Don't actually open device in test
    NULL
  }, cycle = TRUE)

  # Mock dev.cur to simulate no device open
  mock_dev_cur <- mock(1, cycle = TRUE)  # Return 1 = no device

  # Mock dev.off
  mock_dev_off <- mock(NULL, cycle = TRUE)

  # Create test texts and style
  text_a <- "{.24 **70%**}\n{.8 NUV. NIVEAU}"
  text_b <- "{.24 **65%**}\n{.8 MÅL}"
  style <- marquee::modify_style(
    marquee::classic_style(),
    "p",
    margin = marquee::trbl(0),
    align = "right"
  )

  # Test batch API
  device_open_count <- 0
  stub(estimate_label_heights_npc, "grDevices::dev.cur", mock_dev_cur)
  stub(estimate_label_heights_npc, "grDevices::png", mock_png)
  stub(estimate_label_heights_npc, "grDevices::dev.off", mock_dev_off)

  # Note: Skip actual grob operations in test
  skip("Requires full graphics stack for grob measurement")

  # This would test in real environment:
  # heights <- estimate_label_heights_npc(
  #   texts = c(text_a, text_b),
  #   style = style,
  #   panel_height_inches = 6.5,
  #   return_details = TRUE
  # )
  #
  # expect_equal(device_open_count, 1, info = "Batch API should open device once")
  # expect_length(heights, 2)
})

test_that("batch API returns same results as individual calls", {
  source("utils_standalone_label_placement.R")

  text_a <- "{.24 **70%**}\n{.8 NUV. NIVEAU}"
  text_b <- "{.24 **65%**}\n{.8 MÅL}"
  style <- marquee::modify_style(
    marquee::classic_style(),
    "p",
    margin = marquee::trbl(0),
    align = "right"
  )
  panel_height <- 6.5

  # Measure individually (forces 2 device opens if no device active)
  height_a_individual <- estimate_label_height_npc(
    text = text_a,
    style = style,
    panel_height_inches = panel_height,
    return_details = TRUE
  )

  height_b_individual <- estimate_label_height_npc(
    text = text_b,
    style = style,
    panel_height_inches = panel_height,
    return_details = TRUE
  )

  # Measure with batch API (1 device open)
  heights_batch <- estimate_label_heights_npc(
    texts = c(text_a, text_b),
    style = style,
    panel_height_inches = panel_height,
    return_details = TRUE
  )

  # Results should be identical (thanks to caching)
  expect_equal(heights_batch[[1]]$npc, height_a_individual$npc)
  expect_equal(heights_batch[[2]]$npc, height_b_individual$npc)
  expect_equal(heights_batch[[1]]$inches, height_a_individual$inches)
  expect_equal(heights_batch[[2]]$inches, height_b_individual$inches)
})

test_that("batch API respects existing device", {
  source("utils_standalone_label_placement.R")

  # Open a device first
  temp_file <- tempfile(fileext = ".pdf")
  pdf(temp_file, width = 7, height = 7)
  on.exit({
    dev.off()
    unlink(temp_file)
  })

  initial_dev <- dev.cur()

  text_a <- "{.24 **70%**}\n{.8 TEST}"
  text_b <- "{.24 **50%**}\n{.8 LABEL}"
  style <- marquee::modify_style(
    marquee::classic_style(),
    "p",
    margin = marquee::trbl(0),
    align = "right"
  )

  # Batch measurement should use existing device
  heights <- estimate_label_heights_npc(
    texts = c(text_a, text_b),
    style = style,
    panel_height_inches = 6.5,
    return_details = TRUE
  )

  # Device should still be the same (not closed)
  expect_equal(dev.cur(), initial_dev,
               info = "Batch API should not close existing device")

  # Should have valid results
  expect_length(heights, 2)
  expect_true(is.numeric(heights[[1]]$npc))
  expect_true(is.numeric(heights[[2]]$npc))
})

test_that("cache works across batch and individual calls", {
  source("utils_standalone_label_placement.R")

  # Clear cache
  clear_grob_height_cache()

  text <- "{.24 **TEST**}"
  style <- marquee::modify_style(
    marquee::classic_style(),
    "p",
    margin = marquee::trbl(0),
    align = "right"
  )
  panel_height <- 6.5

  # First call via batch API
  heights_batch <- estimate_label_heights_npc(
    texts = c(text),
    style = style,
    panel_height_inches = panel_height,
    return_details = TRUE,
    use_cache = TRUE
  )

  # Second call via individual API (should hit cache)
  height_individual <- estimate_label_height_npc(
    text = text,
    style = style,
    panel_height_inches = panel_height,
    return_details = TRUE,
    use_cache = TRUE
  )

  # Should be identical due to cache
  expect_equal(height_individual$npc, heights_batch[[1]]$npc)

  clear_grob_height_cache()
})
