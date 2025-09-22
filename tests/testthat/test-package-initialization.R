# test-package-initialization.R
# Tests for package-based initialization without filesystem dependencies

test_that("initialize_app() works with package loading", {

  # Test that initialize_app can run without requiring source() calls
  result <- initialize_app()

  # Should return a proper initialization result
  expect_type(result, "list")
  expect_true("package_verification" %in% names(result))
  expect_true("config" %in% names(result))
  expect_true("branding" %in% names(result))
  expect_true("verification" %in% names(result))
})

test_that("Package functions are available", {

  # Test that essential functions are loaded via package
  essential_functions <- c(
    "create_app_state",
    "create_emit_api",
    "app_ui",
    "app_server",
    "run_app",
    "autodetect_engine"
  )

  for (func_name in essential_functions) {
    expect_true(exists(func_name, mode = "function"),
                info = paste("Function", func_name, "should be available"))
  }
})

test_that("Branding configuration is available", {

  # Test that branding globals are set by package .onLoad
  expect_true(exists("HOSPITAL_NAME"))
  expect_true(exists("my_theme"))
  expect_true(exists("HOSPITAL_LOGO_PATH"))

  # Test branding values
  expect_type(HOSPITAL_NAME, "character")
  expect_length(HOSPITAL_NAME, 1)
  expect_true(nzchar(HOSPITAL_NAME))

  expect_s3_class(my_theme, "bs_theme")

  expect_type(HOSPITAL_LOGO_PATH, "character")
  expect_length(HOSPITAL_LOGO_PATH, 1)
})

test_that("initialize_app() returns valid configuration", {

  result <- initialize_app()

  # Test config structure
  config <- result$config
  expect_type(config, "list")
  expect_true("logging" %in% names(config))
  expect_true("testing" %in% names(config))

  # Test branding verification
  branding <- result$branding
  expect_type(branding, "list")
  expect_true("complete" %in% names(branding))
  expect_type(branding$complete, "logical")

  # Test package verification
  package_check <- result$package_verification
  expect_type(package_check, "list")
  expect_true("available_functions" %in% names(package_check))
  expect_true("missing_functions" %in% names(package_check))
})

test_that("Initialization verification works correctly", {

  result <- initialize_app()
  verification <- result$verification

  expect_type(verification, "list")
  expect_true("complete" %in% names(verification))
  expect_true("missing_functions" %in% names(verification))
  expect_true("missing_globals" %in% names(verification))

  # For a properly packaged app, verification should pass
  expect_type(verification$complete, "logical")
  expect_type(verification$missing_functions, "character")
  expect_type(verification$missing_globals, "character")
})

test_that("get_initialization_status_report() works", {

  result <- initialize_app()
  status_report <- get_initialization_status_report(result)

  expect_s3_class(status_report, "data.frame")
  expect_true("component" %in% names(status_report))
  expect_true("status" %in% names(status_report))
  expect_true("details" %in% names(status_report))

  expect_true(nrow(status_report) > 0)

  # Test with NULL input
  null_report <- get_initialization_status_report(NULL)
  expect_s3_class(null_report, "data.frame")
  expect_equal(null_report$status, "not_started")
})

test_that("No filesystem dependencies in initialization", {

  # Test that initialization doesn't try to source files
  # This is a regression test for the old source() chain

  # Mock file.exists to always return FALSE to ensure no file sourcing
  old_file_exists <- file.exists

  # Temporarily override file.exists to return FALSE for R/ paths
  unlockBinding("file.exists", baseenv())
  assign("file.exists", function(path) {
    if (any(grepl("^R/", path) | grepl("/R/", path))) {
      return(FALSE)  # Simulate missing source files
    }
    old_file_exists(path)
  }, envir = baseenv())

  # Test should still work because we're using package loading
  expect_no_error({
    result <- initialize_app()
    expect_type(result, "list")
  })

  # Restore original function
  assign("file.exists", old_file_exists, envir = baseenv())
  lockBinding("file.exists", baseenv())
})

test_that("Package-based loading is robust", {

  # Test with missing config override
  result1 <- initialize_app(config_override = NULL)
  expect_type(result1, "list")

  # Test with custom config override
  custom_config <- list(
    logging = list(debug_mode_enabled = TRUE),
    testing = list(auto_load_enabled = TRUE)
  )

  result2 <- initialize_app(config_override = custom_config)
  expect_type(result2, "list")
  expect_identical(result2$config, custom_config)
})

test_that("Performance optimizations work without file dependencies", {

  config <- list(
    testing = list(auto_load_enabled = TRUE),
    development = list(auto_restore_enabled = FALSE)
  )

  optimizations <- setup_performance_optimizations(config)

  expect_type(optimizations, "list")
  expect_true(optimizations$testing_config_set %||% FALSE)
  expect_true(optimizations$development_config_set %||% FALSE)

  # Check that global variables were set
  expect_true(exists("TEST_MODE_AUTO_LOAD"))
  expect_true(exists("AUTO_RESTORE_ENABLED"))
  expect_equal(TEST_MODE_AUTO_LOAD, TRUE)
  expect_equal(AUTO_RESTORE_ENABLED, FALSE)
})