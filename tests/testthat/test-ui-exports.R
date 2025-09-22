# Tests for exported UI builder funktioner

library(testthat)

project_root <- here::here()
local_lib <- file.path(project_root, ".Rlibs")
if (dir.exists(local_lib)) {
  .libPaths(c(local_lib, .libPaths()))
}

library(claudespc)

test_that("UI builder funktioner er eksporteret", {
  exported <- getNamespaceExports("claudespc")

  expect_true("create_ui_header" %in% exported)
  expect_true("create_ui_main_content" %in% exported)
  expect_true("create_ui_sidebar" %in% exported)
  expect_true("create_welcome_page" %in% exported)
})
