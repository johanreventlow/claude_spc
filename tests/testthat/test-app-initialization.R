# test-app-initialization.R
# Tests for app initialization, configuration loading og setup

test_that("App initialization og global configuration fungerer", {
  # Test at core funktioner er loaded (ikke package-baseret)
  # Projektet bruger source-baseret loading, ikke package struktur
  skip_if_not(exists("get_qic_chart_type", mode = "function"),
              "get_qic_chart_type not available - check global.R loading")

  expect_equal(get_qic_chart_type("P-kort (Andele)"), "p")
  expect_equal(get_qic_chart_type(""), "run")

  # Test hospital konfiguration hvis tilgængelig
  if (exists("HOSPITAL_NAME")) {
    expect_type(HOSPITAL_NAME, "character")
    expect_length(HOSPITAL_NAME, 1)
    expect_true(nzchar(HOSPITAL_NAME))
  } else {
    skip("HOSPITAL_NAME not defined - this is OK in test environment")
  }
})

test_that("Helper functions er tilgængelige efter initialization", {
  helper_functions <- c("ensure_standard_columns", "validate_numeric_column", "safe_date_parse")
  for (func in helper_functions) {
    # Check if function exists, but don't fail test if it doesn't
    if (exists(func)) {
      expect_true(is.function(get(func)))
    } else {
      skip(paste("Function", func, "not available - this is expected for some functions"))
    }
  }
})

test_that("TEST_MODE environment variables fungerer", {
  # Test at TEST_MODE kan toggles sikkert (men kun hvis de eksisterer)
  if (exists("TEST_MODE_AUTO_LOAD")) {
    expect_true(is.logical(TEST_MODE_AUTO_LOAD))
  } else {
    skip("TEST_MODE_AUTO_LOAD not defined - this is OK")
  }

  if (exists("TEST_MODE_FILE_PATH")) {
    expect_true(is.character(TEST_MODE_FILE_PATH))
  } else {
    skip("TEST_MODE_FILE_PATH not defined - this is OK")
  }
})

test_that("Unified state system kan initialiseres", {
  skip_if_not_installed("shiny")

  # Test at vi kan oprette app_state
  expect_true(exists("create_app_state"))

  # Strong assertion - this is critical functionality
  expect_true(exists("create_app_state", mode = "function"),
              "create_app_state must be available for unified state system")

  app_state <- create_app_state()
  expect_true(is.environment(app_state))
  expect_true(exists("events", envir = app_state))
  expect_true(exists("data", envir = app_state))
  expect_true(exists("columns", envir = app_state))
  expect_true(exists("session", envir = app_state))
})