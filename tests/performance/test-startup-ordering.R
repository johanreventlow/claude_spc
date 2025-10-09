# test-startup-ordering.R
# Tests for proper startup configuration ordering

test_that("Environment configuration happens before logging configuration", {
  # Store original environment
  original_golem_config <- Sys.getenv("GOLEM_CONFIG_ACTIVE", "")
  original_log_level <- Sys.getenv("SPC_LOG_LEVEL", "")

  on.exit({
    # Restore original environment
    if (original_golem_config == "") {
      Sys.unsetenv("GOLEM_CONFIG_ACTIVE")
    } else {
      Sys.setenv(GOLEM_CONFIG_ACTIVE = original_golem_config)
    }
    if (original_log_level == "") {
      Sys.unsetenv("SPC_LOG_LEVEL")
    } else {
      Sys.setenv(SPC_LOG_LEVEL = original_log_level)
    }
  })

  # Clear environment variables to simulate fresh start
  Sys.unsetenv("GOLEM_CONFIG_ACTIVE")
  Sys.unsetenv("SPC_LOG_LEVEL")

  # Test the sequence: app environment THEN logging

  # Step 1: Configure app environment (should set GOLEM_CONFIG_ACTIVE)
  safe_operation(
    "Configure app environment for testing",
    code = {
      configure_app_environment(enable_test_mode = FALSE)

      # Verify GOLEM_CONFIG_ACTIVE is now set
      config_active <- Sys.getenv("GOLEM_CONFIG_ACTIVE", "")
      expect_true(config_active != "", "GOLEM_CONFIG_ACTIVE should be set after configure_app_environment")
      expect_equal(config_active, "production", "Should set production config for non-test mode")
    }
  )

  # Step 2: Configure logging (should use the correct environment)
  safe_operation(
    "Configure logging with proper environment context",
    code = {
      configure_logging_from_yaml()

      # Verify logging uses the correct environment context
      log_level <- Sys.getenv("SPC_LOG_LEVEL", "")
      expect_true(log_level != "", "SPC_LOG_LEVEL should be set after configure_logging_from_yaml")
    }
  )
})

test_that("Development environment gets correct log level", {
  original_golem_config <- Sys.getenv("GOLEM_CONFIG_ACTIVE", "")
  original_log_level <- Sys.getenv("SPC_LOG_LEVEL", "")

  on.exit({
    if (original_golem_config == "") {
      Sys.unsetenv("GOLEM_CONFIG_ACTIVE")
    } else {
      Sys.setenv(GOLEM_CONFIG_ACTIVE = original_golem_config)
    }
    if (original_log_level == "") {
      Sys.unsetenv("SPC_LOG_LEVEL")
    } else {
      Sys.setenv(SPC_LOG_LEVEL = original_log_level)
    }
  })

  # Clear and test development environment
  Sys.unsetenv("GOLEM_CONFIG_ACTIVE")
  Sys.unsetenv("SPC_LOG_LEVEL")

  # Configure for development mode
  configure_app_environment(enable_test_mode = TRUE)
  expect_equal(Sys.getenv("GOLEM_CONFIG_ACTIVE"), "development")

  # Configure logging - should get development log level
  configure_logging_from_yaml()

  # Development should default to DEBUG if YAML config not available
  log_level <- Sys.getenv("SPC_LOG_LEVEL", "")
  expect_true(log_level %in% c("DEBUG", "INFO"),
              "Development environment should use DEBUG or INFO log level")
})

test_that("Production environment gets correct log level", {
  original_golem_config <- Sys.getenv("GOLEM_CONFIG_ACTIVE", "")
  original_log_level <- Sys.getenv("SPC_LOG_LEVEL", "")

  on.exit({
    if (original_golem_config == "") {
      Sys.unsetenv("GOLEM_CONFIG_ACTIVE")
    } else {
      Sys.setenv(GOLEM_CONFIG_ACTIVE = original_golem_config)
    }
    if (original_log_level == "") {
      Sys.unsetenv("SPC_LOG_LEVEL")
    } else {
      Sys.setenv(SPC_LOG_LEVEL = original_log_level)
    }
  })

  # Clear and test production environment
  Sys.unsetenv("GOLEM_CONFIG_ACTIVE")
  Sys.unsetenv("SPC_LOG_LEVEL")

  # Configure for production mode
  configure_app_environment(enable_test_mode = FALSE)
  expect_equal(Sys.getenv("GOLEM_CONFIG_ACTIVE"), "production")

  # Configure logging - should get production log level
  configure_logging_from_yaml()

  # Production should default to WARN if YAML config not available
  log_level <- Sys.getenv("SPC_LOG_LEVEL", "")
  expect_equal(log_level, "WARN", "Production environment should use WARN log level")
})

test_that("Explicit log level override works regardless of environment", {
  original_golem_config <- Sys.getenv("GOLEM_CONFIG_ACTIVE", "")
  original_log_level <- Sys.getenv("SPC_LOG_LEVEL", "")

  on.exit({
    if (original_golem_config == "") {
      Sys.unsetenv("GOLEM_CONFIG_ACTIVE")
    } else {
      Sys.setenv(GOLEM_CONFIG_ACTIVE = original_golem_config)
    }
    if (original_log_level == "") {
      Sys.unsetenv("SPC_LOG_LEVEL")
    } else {
      Sys.setenv(SPC_LOG_LEVEL = original_log_level)
    }
  })

  # Test explicit override in production environment
  Sys.unsetenv("GOLEM_CONFIG_ACTIVE")
  Sys.unsetenv("SPC_LOG_LEVEL")

  configure_app_environment(enable_test_mode = FALSE)  # Sets production
  configure_logging_from_yaml(log_level = "INFO")     # Override to INFO

  expect_equal(Sys.getenv("SPC_LOG_LEVEL"), "INFO",
               "Explicit log level should override environment defaults")
})

test_that("Invalid log level falls back to YAML/environment defaults", {
  original_golem_config <- Sys.getenv("GOLEM_CONFIG_ACTIVE", "")
  original_log_level <- Sys.getenv("SPC_LOG_LEVEL", "")

  on.exit({
    if (original_golem_config == "") {
      Sys.unsetenv("GOLEM_CONFIG_ACTIVE")
    } else {
      Sys.setenv(GOLEM_CONFIG_ACTIVE = original_golem_config)
    }
    if (original_log_level == "") {
      Sys.unsetenv("SPC_LOG_LEVEL")
    } else {
      Sys.setenv(SPC_LOG_LEVEL = original_log_level)
    }
  })

  # Test invalid log level in development environment
  Sys.unsetenv("GOLEM_CONFIG_ACTIVE")
  Sys.unsetenv("SPC_LOG_LEVEL")

  configure_app_environment(enable_test_mode = TRUE)     # Sets development
  configure_logging_from_yaml(log_level = "INVALID")    # Invalid level

  # Should fall back to development default (DEBUG)
  log_level <- Sys.getenv("SPC_LOG_LEVEL", "")
  expect_equal(log_level, "DEBUG",
               "Invalid log level should fall back to environment default")
})