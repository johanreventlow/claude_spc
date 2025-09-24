# Test af pakke namespace og eksporter
# Erstatter test-ui-exports.R med meningsfuld pakke-validering

library(testthat)

test_that("Core functions can be loaded via pkgload", {
  # Skip hvis pkgload ikke er tilgængelig
  skip_if_not_installed("pkgload")

  # Load package i development mode
  expect_no_error({
    pkgload::load_all(".", quiet = TRUE)
  })

  # TEST: Kritiske funktioner er tilgængelige via namespace
  core_functions <- c(
    "create_app_state",
    "initialize_app",
    "run_app",
    "safe_operation"
  )

  for (func_name in core_functions) {
    expect_true(
      exists(func_name, mode = "function"),
      info = paste("Function", func_name, "should exist after pkgload::load_all()")
    )
  }
})

test_that("Package DESCRIPTION and NAMESPACE are consistent", {
  # Read DESCRIPTION
  desc_path <- file.path(here::here(), "DESCRIPTION")
  skip_if_not(file.exists(desc_path), "DESCRIPTION file not found")

  desc <- read.dcf(desc_path)

  # TEST: Package name er korrekt
  expect_equal(desc[1, "Package"], "claudespc")

  # TEST: Version følger semantic versioning
  version <- desc[1, "Version"]
  expect_match(version, "^\\d+\\.\\d+\\.\\d+", info = "Version should follow semantic versioning")

  # Read NAMESPACE
  ns_path <- file.path(here::here(), "NAMESPACE")
  skip_if_not(file.exists(ns_path), "NAMESPACE file not found")

  namespace_lines <- readLines(ns_path)

  # TEST: NAMESPACE indeholder export statements
  exports <- grep("^export\\(", namespace_lines, value = TRUE)
  expect_gt(length(exports), 0, info = "NAMESPACE should contain export statements")

  # TEST: Key exports er til stede
  key_exports <- c("run_app", "initialize_app")
  for (export in key_exports) {
    pattern <- paste0("^export\\(", export, "\\)")
    expect_true(
      any(grepl(pattern, namespace_lines)),
      info = paste("Function", export, "should be exported in NAMESPACE")
    )
  }
})

test_that("Package dependencies can be loaded", {
  # Read dependencies from DESCRIPTION
  desc_path <- file.path(here::here(), "DESCRIPTION")
  skip_if_not(file.exists(desc_path), "DESCRIPTION file not found")

  desc <- read.dcf(desc_path)

  # Parse Imports field
  imports_field <- desc[1, "Imports"]
  if (!is.na(imports_field)) {
    # Extract package names (remove version constraints)
    imports <- strsplit(imports_field, ",\\s*")[[1]]
    imports <- gsub("\\s*\\([^)]*\\)", "", imports)  # Remove version constraints
    imports <- trimws(imports)

    # TEST: Core dependencies kan loades
    core_deps <- c("shiny", "DT", "qicharts2", "dplyr")
    available_deps <- intersect(core_deps, imports)

    for (dep in available_deps) {
      expect_true(
        requireNamespace(dep, quietly = TRUE),
        info = paste("Core dependency", dep, "should be available")
      )
    }
  }
})