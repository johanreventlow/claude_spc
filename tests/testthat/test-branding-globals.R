# Sikrer at branding constants er tilgængelige for legacy kode

library(testthat)

project_root <- here::here()
local_lib <- file.path(project_root, ".Rlibs")
if (dir.exists(local_lib)) {
  .libPaths(c(local_lib, .libPaths()))
}

# NOTE: Cannot load claudespc package in its own tests due to circular dependency
# Load components directly from source instead
if (file.exists(file.path(project_root, "R/branding_globals.R"))) {
  source(file.path(project_root, "R/branding_globals.R"), local = FALSE)
}

test_that("HOSPITAL_COLORS er tilgængelig globalt", {
  expect_true(exists("HOSPITAL_COLORS", envir = globalenv()))
  expect_true(is.list(HOSPITAL_COLORS))
  expect_true(all(c("primary", "success", "warning", "danger") %in% names(HOSPITAL_COLORS)))
})
