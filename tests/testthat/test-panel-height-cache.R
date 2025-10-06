# Test panel height caching functionality
# Tests cache hits, misses, bypass, and statistics

test_that("panel height cache works correctly", {
  # Setup
  library(ggplot2)

  # Clear cache before test
  clear_panel_height_cache()

  # Create a simple plot
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    theme_minimal()

  # Build gtable
  built <- ggplot2::ggplot_build(p)
  gt <- ggplot2::ggplot_gtable(built)

  # Get initial cache stats
  stats_before <- get_panel_height_cache_stats()
  expect_equal(stats_before$cache_size, 0)
  expect_equal(stats_before$cache_hits, 0)
  expect_equal(stats_before$cache_misses, 0)
  expect_equal(stats_before$hit_rate, 0)

  # First call - should be cache miss
  height1 <- measure_panel_height_from_gtable(gt, use_cache = TRUE)
  stats_after_first <- get_panel_height_cache_stats()

  expect_equal(stats_after_first$cache_misses, 1)
  expect_equal(stats_after_first$cache_hits, 0)
  expect_equal(stats_after_first$cache_size, 1)
  expect_true(!is.null(height1))
  expect_true(is.numeric(height1))
  expect_true(height1 > 0)

  # Second call with same gtable - should be cache hit
  height2 <- measure_panel_height_from_gtable(gt, use_cache = TRUE)
  stats_after_second <- get_panel_height_cache_stats()

  expect_equal(stats_after_second$cache_misses, 1)  # Still 1
  expect_equal(stats_after_second$cache_hits, 1)    # Now 1
  expect_equal(stats_after_second$cache_size, 1)    # Still 1 entry

  # Heights should be identical
  expect_equal(height1, height2)

  # Hit rate should be 50%
  expect_equal(stats_after_second$hit_rate, 0.5)

  # Third call - another hit
  height3 <- measure_panel_height_from_gtable(gt, use_cache = TRUE)
  stats_after_third <- get_panel_height_cache_stats()

  expect_equal(stats_after_third$cache_hits, 2)
  expect_equal(stats_after_third$hit_rate, 2/3)

  # Clear cache
  clear_panel_height_cache()
  stats_cleared <- get_panel_height_cache_stats()
  expect_equal(stats_cleared$cache_size, 0)
  expect_equal(stats_cleared$cache_hits, 0)
  expect_equal(stats_cleared$cache_misses, 0)
})

test_that("cache bypass works correctly", {
  library(ggplot2)

  clear_panel_height_cache()

  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  built <- ggplot2::ggplot_build(p)
  gt <- ggplot2::ggplot_gtable(built)

  # First call with cache
  height1 <- measure_panel_height_from_gtable(gt, use_cache = TRUE)
  stats1 <- get_panel_height_cache_stats()
  expect_equal(stats1$cache_misses, 1)

  # Second call bypassing cache
  height2 <- measure_panel_height_from_gtable(gt, use_cache = FALSE)
  stats2 <- get_panel_height_cache_stats()

  # Stats should not change when use_cache = FALSE
  expect_equal(stats2$cache_hits, 0)
  expect_equal(stats2$cache_misses, 1)

  # But height should still be correct
  expect_equal(height2, height1)

  clear_panel_height_cache()
})

test_that("cache works through wrapper functions", {
  library(ggplot2)

  clear_panel_height_cache()

  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()

  # Test through measure_panel_height_inches
  height1 <- measure_panel_height_inches(p, use_cache = TRUE)
  stats1 <- get_panel_height_cache_stats()
  expect_equal(stats1$cache_misses, 1)
  expect_equal(stats1$cache_hits, 0)

  # Second call should hit cache
  height2 <- measure_panel_height_inches(p, use_cache = TRUE)
  stats2 <- get_panel_height_cache_stats()
  expect_equal(stats2$cache_hits, 1)
  expect_equal(height1, height2)

  # Test through measure_panel_height_from_built
  clear_panel_height_cache()

  built <- ggplot2::ggplot_build(p)
  height3 <- measure_panel_height_from_built(built, use_cache = TRUE)
  stats3 <- get_panel_height_cache_stats()
  expect_equal(stats3$cache_misses, 1)

  height4 <- measure_panel_height_from_built(built, use_cache = TRUE)
  stats4 <- get_panel_height_cache_stats()
  expect_equal(stats4$cache_hits, 1)
  expect_equal(height3, height4)

  clear_panel_height_cache()
})

test_that("cache handles different plots correctly", {
  library(ggplot2)

  clear_panel_height_cache()

  # Create two different plots
  p1 <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  p2 <- ggplot(mtcars, aes(hp, mpg)) + geom_line()

  built1 <- ggplot2::ggplot_build(p1)
  built2 <- ggplot2::ggplot_build(p2)
  gt1 <- ggplot2::ggplot_gtable(built1)
  gt2 <- ggplot2::ggplot_gtable(built2)

  # First plot
  height1a <- measure_panel_height_from_gtable(gt1, use_cache = TRUE)
  stats1 <- get_panel_height_cache_stats()
  expect_equal(stats1$cache_size, 1)
  expect_equal(stats1$cache_misses, 1)

  # Second plot - different layout, should be cache miss
  height2a <- measure_panel_height_from_gtable(gt2, use_cache = TRUE)
  stats2 <- get_panel_height_cache_stats()
  expect_equal(stats2$cache_size, 2)  # Two different entries
  expect_equal(stats2$cache_misses, 2)

  # Call first plot again - should hit cache
  height1b <- measure_panel_height_from_gtable(gt1, use_cache = TRUE)
  stats3 <- get_panel_height_cache_stats()
  expect_equal(stats3$cache_hits, 1)
  expect_equal(height1a, height1b)

  clear_panel_height_cache()
})
