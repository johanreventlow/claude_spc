# Sikrer at branding constants er tilgængelige for legacy kode

library(testthat)

project_root <- here::here()
local_lib <- file.path(project_root, ".Rlibs")
if (dir.exists(local_lib)) {
  .libPaths(c(local_lib, .libPaths()))
}

library(claudespc)

test_that("HOSPITAL_COLORS er tilgængelig globalt", {
  expect_true(exists("HOSPITAL_COLORS", envir = globalenv()))
  expect_true(is.list(HOSPITAL_COLORS))
  expect_true(all(c("primary", "success", "warning", "danger") %in% names(HOSPITAL_COLORS)))
})
