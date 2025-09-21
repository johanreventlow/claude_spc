# test-visualization-dimensions.R
# Tests for responsive plot dimension calculations

library(testthat)

test_that("compute_spc_plot_height uses client height when valid", {
  expect_equal(
    compute_spc_plot_height(width = 900, client_height = 520),
    520
  )
})

test_that("compute_spc_plot_height falls back to aspect ratio when client height missing", {
  result <- compute_spc_plot_height(width = 1000, client_height = NULL)
  expect_true(result >= 700)
  expect_true(result <= 800)
})

test_that("compute_spc_plot_height respects minimum height", {
  result <- compute_spc_plot_height(width = 500, client_height = 50, min_height = 420)
  expect_equal(result, 420)
})

test_that("compute_spc_plot_height returns default when width unknown", {
  expect_equal(
    compute_spc_plot_height(width = NULL, client_height = NULL, default_height = 600),
    600
  )
})
