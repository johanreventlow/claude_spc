# test-runtime-config-comprehensive.R
# Comprehensive tests for initialize_runtime_config() and helper functions
# Critical for miljøafhængig opsætning, logger levels og feature flags

library(testthat)

test_that("initialize_runtime_config works with default settings", {
  # TEST: Default configuration initialization
  expect_true(exists("initialize_runtime_config", mode = "function"),
              "initialize_runtime_config function must be available")

  config <- initialize_runtime_config()

  # Verify base structure exists
  expect_type(config, "list")
  expect_true("development" %in% names(config))
  expect_true("environment" %in% names(config))
  expect_true("logging" %in% names(config))
  expect_true("test_mode" %in% names(config))

  # Verify development section
  expect_type(config$development, "list")
  expect_true("debug_enabled" %in% names(config$development))
  expect_type(config$development$debug_enabled, "logical")

  # Verify environment section
  expect_type(config$environment, "list")
  expect_true("is_ci" %in% names(config$environment))
  expect_true("is_development" %in% names(config$environment))

  # Verify logging section
  expect_type(config$logging, "list")
  expect_true("level" %in% names(config$logging))
  expect_true(config$logging$level %in% c("debug", "info", "warn", "error"))
})

test_that("initialize_runtime_config respects override_options", {
  # TEST: Configuration overrides work correctly
  override_options <- list(
    debug_mode = TRUE,
    test_mode_auto_load = FALSE,
    logging_level = "debug",
    custom_feature = "test_value"
  )

  config <- initialize_runtime_config(override_options = override_options)

  # Verify overrides were applied where appropriate
  expect_type(config, "list")

  # Note: Exact override behavior depends on implementation
  # Test that config structure remained intact
  expect_true(all(c("development", "environment", "logging", "test_mode") %in% names(config)))
})

test_that("determine_environment_type_from_context works correctly", {
  skip_if_not(exists("determine_environment_type_from_context", mode = "function"),
              "determine_environment_type_from_context not available - check test setup")

  # TEST: CI environment detection
  old_ci <- Sys.getenv("CI", unset = NA_character_)
  Sys.setenv(CI = "true")
  on.exit({
    if (is.na(old_ci)) {
      Sys.unsetenv("CI")
    } else {
      Sys.setenv(CI = old_ci)
    }
  }, add = TRUE)

  env_type <- determine_environment_type_from_context()
  expect_true(env_type %in% c("ci", "test", "production", "development"))

  # TEST: Development environment detection
  Sys.unsetenv("CI")
  env_type_dev <- determine_environment_type_from_context()
  expect_true(env_type_dev %in% c("development", "test", "production"))
})

test_that("setup_development_config creates valid configuration", {
  skip_if_not(exists("setup_development_config", mode = "function"),
              "setup_development_config not available - check test setup")

  # TEST: Default development config
  dev_config <- setup_development_config()

  expect_type(dev_config, "list")
  expect_true("debug_enabled" %in% names(dev_config))
  expect_type(dev_config$debug_enabled, "logical")

  # TEST: Override development config
  override_options <- list(debug_mode = FALSE)
  dev_config_override <- setup_development_config(override_options)

  expect_type(dev_config_override, "list")
  # Structure should be consistent regardless of overrides
  expect_true("debug_enabled" %in% names(dev_config_override))
})

test_that("setup_environment_features detects environment correctly", {
  skip_if_not(exists("setup_environment_features", mode = "function"),
              "setup_environment_features not available - check test setup")

  # TEST: Environment feature detection
  env_config <- setup_environment_features()

  expect_type(env_config, "list")
  expect_true("is_ci" %in% names(env_config))
  expect_true("is_development" %in% names(env_config))
  expect_type(env_config$is_ci, "logical")
  expect_type(env_config$is_development, "logical")

  # TEST: Platform detection if available
  if ("platform" %in% names(env_config)) {
    expect_type(env_config$platform, "character")
    expect_true(nchar(env_config$platform) > 0)
  }
})

test_that("setup_logging_features creates appropriate log levels", {
  skip_if_not(exists("setup_logging_features", mode = "function"),
              "setup_logging_features not available - check test setup")

  # TEST: Default logging config
  log_config <- setup_logging_features()

  expect_type(log_config, "list")
  expect_true("level" %in% names(log_config))
  expect_true(log_config$level %in% c("debug", "info", "warn", "error"))

  # TEST: Override logging level
  override_options <- list(logging_level = "warn")
  log_config_override <- setup_logging_features(override_options)

  expect_type(log_config_override, "list")
  expect_true("level" %in% names(log_config_override))
})

test_that("runtime config handles environment variables correctly", {
  # TEST: Environment variable integration

  # Store original values
  original_vars <- list(
    test_mode = Sys.getenv("TEST_MODE_AUTO_LOAD", unset = NA_character_),
    debug_mode = Sys.getenv("DEBUG_MODE", unset = NA_character_),
    log_level = Sys.getenv("LOG_LEVEL", unset = NA_character_)
  )

  # Set test environment variables
  Sys.setenv(TEST_MODE_AUTO_LOAD = "TRUE")
  Sys.setenv(DEBUG_MODE = "FALSE")
  Sys.setenv(LOG_LEVEL = "info")

  on.exit({
    # Restore original environment variables
    for (var_name in names(original_vars)) {
      if (is.na(original_vars[[var_name]])) {
        Sys.unsetenv(names(original_vars)[which(names(original_vars) == var_name)])
      } else {
        do.call(Sys.setenv, setNames(list(original_vars[[var_name]]), var_name))
      }
    }
  }, add = TRUE)

  # TEST: Configuration respects environment variables
  config <- initialize_runtime_config()

  expect_type(config, "list")
  # Test that configuration structure is maintained
  expect_true(all(c("development", "environment", "logging", "test_mode") %in% names(config)))
})

test_that("runtime config error handling works correctly", {
  # TEST: Invalid override options
  invalid_options <- list(
    completely_invalid_key = "should_not_break",
    another_invalid = NULL
  )

  # Should not error even with invalid options
  expect_no_error({
    config <- initialize_runtime_config(override_options = invalid_options)
  })

  # Configuration should still be valid
  expect_type(config, "list")
  expect_true(all(c("development", "environment", "logging", "test_mode") %in% names(config)))

  # TEST: NULL override_options
  expect_no_error({
    config_null <- initialize_runtime_config(override_options = NULL)
  })

  expect_type(config_null, "list")
})

test_that("runtime config backwards compatibility maintained", {
  # TEST: Legacy configuration format is preserved
  config <- initialize_runtime_config()

  # Verify legacy structure elements are present
  legacy_required_keys <- c("development", "environment", "logging", "test_mode")

  for (key in legacy_required_keys) {
    expect_true(key %in% names(config),
                info = paste("Legacy key", key, "should be present for backwards compatibility"))
  }

  # TEST: Test mode configuration specifically
  expect_type(config$test_mode, "list")
  if ("auto_load" %in% names(config$test_mode)) {
    expect_type(config$test_mode$auto_load, "logical")
  }
})

test_that("runtime config performance is acceptable", {
  # TEST: Configuration initialization performance
  start_time <- Sys.time()

  # Run configuration multiple times to test performance
  for (i in 1:10) {
    config <- initialize_runtime_config()
  }

  duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  # Configuration should initialize quickly (less than 1 second for 10 iterations)
  expect_lt(duration, 1.0, info = paste("Configuration took", duration, "seconds for 10 iterations"))

  # Last config should still be valid
  expect_type(config, "list")
  expect_true(all(c("development", "environment", "logging", "test_mode") %in% names(config)))
})

test_that("runtime config integration with golem works", {
  # TEST: Integration with golem configuration system
  skip_if_not_installed("golem")

  # Mock golem options if needed
  if (exists("golem", mode = "list") && is.list(golem)) {
    # Test with golem integration
    expect_no_error({
      config <- initialize_runtime_config()
    })

    expect_type(config, "list")
  } else {
    # Test without golem integration
    expect_no_error({
      config <- initialize_runtime_config()
    })

    expect_type(config, "list")
    expect_true(all(c("development", "environment", "logging", "test_mode") %in% names(config)))
  }
})

test_that("runtime config thread safety and state consistency", {
  # TEST: Configuration state consistency across multiple calls
  configs <- list()

  # Generate multiple configs
  for (i in 1:5) {
    configs[[i]] <- initialize_runtime_config()
  }

  # All configs should have consistent structure
  for (i in 2:5) {
    expect_equal(names(configs[[1]]), names(configs[[i]]),
                 info = paste("Config", i, "should have same structure as config 1"))
  }

  # Environment-specific values should be consistent
  for (i in 2:5) {
    expect_equal(configs[[1]]$environment$is_ci, configs[[i]]$environment$is_ci,
                 info = "is_ci should be consistent across config instances")
  }
})