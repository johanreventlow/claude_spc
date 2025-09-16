# test-logging-system.R
# Tests af det nye konfigurerede logging system implementeret i A) Code Quality & Debug Cleanup

test_that("logging system grundlæggende funktionalitet", {
  # Test at logging funktioner eksisterer
  expect_true(exists("log_debug"))
  expect_true(exists("log_info"))
  expect_true(exists("log_warn"))
  expect_true(exists("log_error"))
  expect_true(exists("log_msg"))
  expect_true(exists("get_log_level"))

  # Test LOG_LEVELS konstanter
  expect_true(exists("LOG_LEVELS"))
  expect_type(LOG_LEVELS, "list")
  expect_equal(LOG_LEVELS$DEBUG, 1)
  expect_equal(LOG_LEVELS$INFO, 2)
  expect_equal(LOG_LEVELS$WARN, 3)
  expect_equal(LOG_LEVELS$ERROR, 4)
})

test_that("get_log_level håndterer environment variabler korrekt", {
  # Gem original log level
  original_level <- Sys.getenv("SPC_LOG_LEVEL", "")

  # Test default level (INFO)
  Sys.unsetenv("SPC_LOG_LEVEL")
  expect_equal(get_log_level(), LOG_LEVELS$INFO)

  # Test gyldige log levels
  Sys.setenv(SPC_LOG_LEVEL = "DEBUG")
  expect_equal(get_log_level(), LOG_LEVELS$DEBUG)

  Sys.setenv(SPC_LOG_LEVEL = "INFO")
  expect_equal(get_log_level(), LOG_LEVELS$INFO)

  Sys.setenv(SPC_LOG_LEVEL = "WARN")
  expect_equal(get_log_level(), LOG_LEVELS$WARN)

  Sys.setenv(SPC_LOG_LEVEL = "ERROR")
  expect_equal(get_log_level(), LOG_LEVELS$ERROR)

  # Test case insensitive
  Sys.setenv(SPC_LOG_LEVEL = "debug")
  expect_equal(get_log_level(), LOG_LEVELS$DEBUG)

  Sys.setenv(SPC_LOG_LEVEL = "Info")
  expect_equal(get_log_level(), LOG_LEVELS$INFO)

  # Test invalid level fallback til INFO
  Sys.setenv(SPC_LOG_LEVEL = "INVALID")
  expect_equal(get_log_level(), LOG_LEVELS$INFO)

  Sys.setenv(SPC_LOG_LEVEL = "")
  expect_equal(get_log_level(), LOG_LEVELS$INFO)

  # Gendan original level
  if (original_level == "") {
    Sys.unsetenv("SPC_LOG_LEVEL")
  } else {
    Sys.setenv(SPC_LOG_LEVEL = original_level)
  }
})

test_that("log level filtering virker korrekt", {
  # Gem original log level
  original_level <- Sys.getenv("SPC_LOG_LEVEL", "")

  # Test ERROR level - kun ERROR messages vises
  Sys.setenv(SPC_LOG_LEVEL = "ERROR")
  expect_output(log_error("Error message", "TEST"), "ERROR.*TEST.*Error message")
  expect_silent(log_warn("Warning message", "TEST"))
  expect_silent(log_info("Info message", "TEST"))
  expect_silent(log_debug("Debug message", "TEST"))

  # Test WARN level - WARN og ERROR vises
  Sys.setenv(SPC_LOG_LEVEL = "WARN")
  expect_output(log_error("Error message", "TEST"), "ERROR.*TEST.*Error message")
  expect_output(log_warn("Warning message", "TEST"), "WARN.*TEST.*Warning message")
  expect_silent(log_info("Info message", "TEST"))
  expect_silent(log_debug("Debug message", "TEST"))

  # Test INFO level - INFO, WARN og ERROR vises
  Sys.setenv(SPC_LOG_LEVEL = "INFO")
  expect_output(log_error("Error message", "TEST"), "ERROR.*TEST.*Error message")
  expect_output(log_warn("Warning message", "TEST"), "WARN.*TEST.*Warning message")
  expect_output(log_info("Info message", "TEST"), "INFO.*TEST.*Info message")
  expect_silent(log_debug("Debug message", "TEST"))

  # Test DEBUG level - alle messages vises
  Sys.setenv(SPC_LOG_LEVEL = "DEBUG")
  expect_output(log_error("Error message", "TEST"), "ERROR.*TEST.*Error message")
  expect_output(log_warn("Warning message", "TEST"), "WARN.*TEST.*Warning message")
  expect_output(log_info("Info message", "TEST"), "INFO.*TEST.*Info message")
  expect_output(log_debug("Debug message", "TEST"), "DEBUG.*TEST.*Debug message")

  # Gendan original level
  if (original_level == "") {
    Sys.unsetenv("SPC_LOG_LEVEL")
  } else {
    Sys.setenv(SPC_LOG_LEVEL = original_level)
  }
})

test_that("komponens-baseret tagging fungerer", {
  original_level <- Sys.getenv("SPC_LOG_LEVEL", "")
  Sys.setenv(SPC_LOG_LEVEL = "DEBUG")

  # Test med forskellige komponenter
  expect_output(log_debug("Test message", "DATA_PROC"), "DEBUG.*\\[DATA_PROC\\].*Test message")
  expect_output(log_info("Test message", "AUTO_DETECT"), "INFO.*\\[AUTO_DETECT\\].*Test message")
  expect_output(log_warn("Test message", "FILE_UPLOAD"), "WARN.*\\[FILE_UPLOAD\\].*Test message")
  expect_output(log_error("Test message", "ERROR_HANDLING"), "ERROR.*\\[ERROR_HANDLING\\].*Test message")

  # Test uden komponent
  expect_output(log_info("Test without component"), "INFO:.*Test without component")

  # Test tom komponent string
  expect_output(log_info("Test message", ""), "INFO:.*Test message")

  # Test NULL komponent
  expect_output(log_info("Test message", NULL), "INFO:.*Test message")

  # Gendan original level
  if (original_level == "") {
    Sys.unsetenv("SPC_LOG_LEVEL")
  } else {
    Sys.setenv(SPC_LOG_LEVEL = original_level)
  }
})

test_that("log_msg funktionalitet med forskellige levels", {
  original_level <- Sys.getenv("SPC_LOG_LEVEL", "")
  Sys.setenv(SPC_LOG_LEVEL = "DEBUG")

  # Test alle levels via log_msg
  expect_output(log_msg("Debug message", "DEBUG", "TEST"), "DEBUG.*\\[TEST\\].*Debug message")
  expect_output(log_msg("Info message", "INFO", "TEST"), "INFO.*\\[TEST\\].*Info message")
  expect_output(log_msg("Warn message", "WARN", "TEST"), "WARN.*\\[TEST\\].*Warn message")
  expect_output(log_msg("Error message", "ERROR", "TEST"), "ERROR.*\\[TEST\\].*Error message")

  # Test case insensitive levels
  expect_output(log_msg("Debug message", "debug", "TEST"), "DEBUG.*\\[TEST\\].*Debug message")
  expect_output(log_msg("Info message", "info", "TEST"), "INFO.*\\[TEST\\].*Info message")

  # Test invalid level behandles gracefully
  expect_silent(log_msg("Invalid message", "INVALID", "TEST"))

  # Test timestamp format (bare en grundlæggende check for HH:MM:SS pattern)
  output <- capture.output(log_msg("Test timestamp", "INFO", "TEST"))
  expect_match(output, "\\[\\d{2}:\\d{2}:\\d{2}\\] INFO: \\[TEST\\] Test timestamp")

  # Gendan original level
  if (original_level == "") {
    Sys.unsetenv("SPC_LOG_LEVEL")
  } else {
    Sys.setenv(SPC_LOG_LEVEL = original_level)
  }
})

test_that("logging system edge cases", {
  original_level <- Sys.getenv("SPC_LOG_LEVEL", "")
  Sys.setenv(SPC_LOG_LEVEL = "DEBUG")

  # Test med lange beskeder
  long_message <- paste(rep("A", 1000), collapse = "")
  expect_output(log_info(long_message, "TEST"), "INFO.*\\[TEST\\].*AAAA")

  # Test med special characters
  special_message <- "Æøå!@#$%^&*()[]{}|\\:;\"'<>,.?/~`"
  expect_output(log_info(special_message, "TEST"), "INFO.*\\[TEST\\].*Æøå")

  # Test med newlines i message
  newline_message <- "Line 1\nLine 2\nLine 3"
  expect_output(log_info(newline_message, "TEST"), "INFO.*\\[TEST\\].*Line 1")

  # Test med tomme beskeder
  expect_output(log_info("", "TEST"), "INFO.*\\[TEST\\].*")
  expect_output(log_info("   ", "TEST"), "INFO.*\\[TEST\\].*   ")

  # Test med NULL message - skal ikke crashe
  expect_silent(log_info(NULL, "TEST"))

  # Gendan original level
  if (original_level == "") {
    Sys.unsetenv("SPC_LOG_LEVEL")
  } else {
    Sys.setenv(SPC_LOG_LEVEL = original_level)
  }
})