# test-critical-fixes.R
# Tests for kritiske fixes implementeret i fix/critical-observer-logging-sanitization-issues
# Fokus på observer priorities, logging API og input sanitization

# Setup ----------------------------------------------------------------
source(file.path("..", "..", "global.R"))

test_that("OBSERVER_PRIORITIES er korrekt konfigureret og tilgængelig", {
  # Test at OBSERVER_PRIORITIES eksisterer og har forventede nøgler
  expect_true(exists("OBSERVER_PRIORITIES"),
              info = "OBSERVER_PRIORITIES skal være defineret")

  # Test primære named priorities
  required_priorities <- c("STATE_MANAGEMENT", "AUTO_DETECT", "DATA_PROCESSING",
                          "UI_SYNC", "PLOT_GENERATION", "STATUS_UPDATES",
                          "CLEANUP", "LOGGING")

  for (priority in required_priorities) {
    expect_true(priority %in% names(OBSERVER_PRIORITIES),
                info = paste("OBSERVER_PRIORITIES skal indeholde", priority))
    expect_true(is.numeric(OBSERVER_PRIORITIES[[priority]]),
                info = paste(priority, "skal have numerisk værdi"))
    expect_true(OBSERVER_PRIORITIES[[priority]] > 0,
                info = paste(priority, "skal have positiv værdi"))
  }

  # Test compatibility aliases
  compatibility_aliases <- c("HIGH", "MEDIUM", "LOW", "LOWEST")
  for (alias in compatibility_aliases) {
    expect_true(alias %in% names(OBSERVER_PRIORITIES),
                info = paste("OBSERVER_PRIORITIES skal indeholde alias", alias))
    expect_true(is.numeric(OBSERVER_PRIORITIES[[alias]]),
                info = paste("Alias", alias, "skal have numerisk værdi"))
  }

  # Test legacy aliases (deprecated but supported)
  legacy_aliases <- c("highest", "high", "medium", "low", "lowest")
  for (legacy in legacy_aliases) {
    expect_true(legacy %in% names(OBSERVER_PRIORITIES),
                info = paste("OBSERVER_PRIORITIES skal indeholde legacy alias", legacy))
  }
})

test_that("OBSERVER_PRIORITIES har logisk hierarki", {
  # Test at prioritetshierarkiet giver mening (højere tal = højere prioritet)
  expect_true(OBSERVER_PRIORITIES$STATE_MANAGEMENT > OBSERVER_PRIORITIES$AUTO_DETECT,
              info = "STATE_MANAGEMENT skal have højere prioritet end AUTO_DETECT")

  expect_true(OBSERVER_PRIORITIES$AUTO_DETECT > OBSERVER_PRIORITIES$UI_SYNC,
              info = "AUTO_DETECT skal have højere prioritet end UI_SYNC")

  expect_true(OBSERVER_PRIORITIES$UI_SYNC > OBSERVER_PRIORITIES$CLEANUP,
              info = "UI_SYNC skal have højere prioritet end CLEANUP")

  # Test alias mapping
  expect_equal(OBSERVER_PRIORITIES$HIGH, OBSERVER_PRIORITIES$STATE_MANAGEMENT,
               info = "HIGH alias skal mappe til STATE_MANAGEMENT")

  expect_equal(OBSERVER_PRIORITIES$LOW, OBSERVER_PRIORITIES$UI_SYNC,
               info = "LOW alias skal mappe til UI_SYNC")

  expect_equal(OBSERVER_PRIORITIES$LOWEST, OBSERVER_PRIORITIES$CLEANUP,
               info = "LOWEST alias skal mappe til CLEANUP")
})

test_that("Logging API understøtter component/message/details pattern", {
  # Test ny logging API med details parameter

  # Capture output for log_warn med details
  expect_no_error({
    log_warn(
      message = "Test warning med details",
      .context = "[TEST_COMPONENT]",
      details = list(test_key = "test_value", numeric_val = 42)
    )
  }, info = "log_warn skal acceptere component, message og details parametre")

  # Test log_info med details
  expect_no_error({
    log_info(
      message = "Test info med details",
      .context = "[TEST_COMPONENT]",
      details = list(status = "success", count = 10)
    )
  }, info = "log_info skal acceptere component, message og details parametre")

  # Test log_error med details
  expect_no_error({
    log_error(
      message = "Test error med details",
      .context = "[TEST_COMPONENT]",
      details = list(error_code = 500, operation = "test_operation")
    )
  }, info = "log_error skal acceptere component, message og details parametre")

  # Test backward compatibility - gamle log calls skal stadig virke
  expect_no_error({
    log_warn("Simple warning", .context = "TEST")
    log_info("Simple info", .context = "TEST")
    log_error("Simple error", .context = "TEST")
  }, info = "Logging API skal være backward compatible")
})

test_that("Input sanitization regex fungerer korrekt", {
  # Test at allowed_chars ikke længere har dobbelte kantede parenteser

  # Test normale inputs
  result1 <- sanitize_user_input("Test123_æøå.-", max_length = 100, html_escape = FALSE)
  expect_equal(result1, "Test123_æøå.-",
               info = "Gyldige karakterer skal bevares")

  # Test fjernelse af ikke-tilladte karakterer
  result2 <- sanitize_user_input("Test@#$%123", max_length = 100, html_escape = FALSE)
  expect_equal(result2, "Test123",
               info = "Ikke-tilladte karakterer skal fjernes")

  # Test danske karakterer bevares
  result3 <- sanitize_user_input("Størrelse_økonomi-År.2023", max_length = 100, html_escape = FALSE)
  expect_equal(result3, "Størrelse_økonomi-År.2023",
               info = "Danske karakterer (æ,ø,å) skal bevares")

  # Test at specialtegn fjernes korrekt
  result4 <- sanitize_user_input("Hello[World]&lt;script&gt;", max_length = 100, html_escape = FALSE)
  expect_equal(result4, "HelloWorldscript",
               info = "HTML/script tags skal fjernes")
})

test_that("Column name sanitization fungerer med dansk indhold", {
  # Test sanitize_column_name specifikt

  # Test normale kolonnenavne
  result1 <- sanitize_column_name("Y_værdi")
  expect_true(nchar(result1) > 0, info = "Kolonnenavn skal ikke være tomt")
  expect_true(grepl("Y", result1), info = "Gyldige karakterer skal bevares")

  # Test danske specialtegn
  result2 <- sanitize_column_name("Måling_før_intervention")
  expect_true(grepl("Måling", result2), info = "Danske tegn skal bevares i kolonnenavne")
  expect_true(grepl("før", result2), info = "ø skal bevares")

  # Test fjernelse af problematiske tegn
  result3 <- sanitize_column_name("Col&amp;Name<script>")
  expect_false(grepl("&", result3), info = "& tegn skal fjernes")
  expect_false(grepl("<", result3), info = "< tegn skal fjernes")
  expect_false(grepl(">", result3), info = "> tegn skal fjernes")
})

test_that("Logging API formaterer details korrekt", {
  # Test details formatting i praksis

  # Mock cat for at fange output
  output_lines <- character(0)

  # Test formatering af forskellige data typer i details
  test_details <- list(
    string_val = "test_string",
    numeric_val = 42.5,
    integer_val = 100L,
    logical_val = TRUE
  )

  expect_no_error({
    log_info(
      message = "Test med komplekse details",
      .context = "[TEST_FORMAT]",
      details = test_details
    )
  }, info = "Komplekse details skal formateres uden fejl")

  # Test empty details
  expect_no_error({
    log_warn(
      message = "Test med tomme details",
      .context = "[TEST_FORMAT]",
      details = list()
    )
  }, info = "Tomme details skal håndteres gracefully")

  # Test NULL details (backward compatibility)
  expect_no_error({
    log_error(
      message = "Test uden details",
      .context = "[TEST_FORMAT]",
      details = NULL
    )
  }, info = "NULL details skal være tilladt")
})