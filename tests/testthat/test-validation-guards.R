# test-validation-guards.R
# Test suite for utils_validation_guards.R
# Comprehensive coverage for NULL check consolidation patterns

test_that("validate_data_or_return håndterer NULL data korrekt", {
  result <- validate_data_or_return(NULL, fallback = data.frame())
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})

test_that("validate_data_or_return validerer minimum rows", {
  test_data <- data.frame(x = 1:5, y = 6:10)

  # Skal returnere data når min_rows er opfyldt
  result <- validate_data_or_return(test_data, min_rows = 3, fallback = NULL)
  expect_equal(result, test_data)

  # Skal returnere fallback når min_rows ikke er opfyldt
  result <- validate_data_or_return(test_data, min_rows = 10, fallback = NULL)
  expect_null(result)
})

test_that("validate_data_or_return validerer minimum columns", {
  test_data <- data.frame(x = 1:5, y = 6:10, z = 11:15)

  # Skal returnere data når min_cols er opfyldt
  result <- validate_data_or_return(test_data, min_cols = 2, fallback = NULL)
  expect_equal(result, test_data)

  # Skal returnere fallback når min_cols ikke er opfyldt
  result <- validate_data_or_return(test_data, min_cols = 5, fallback = NULL)
  expect_null(result)
})

test_that("validate_data_or_return håndterer tibbles korrekt", {
  test_tbl <- tibble::tibble(x = 1:5, y = 6:10)
  result <- validate_data_or_return(test_tbl, fallback = NULL)
  expect_equal(result, test_tbl)
})

test_that("value_or_default håndterer NULL korrekt", {
  result <- value_or_default(NULL, default = "fallback")
  expect_equal(result, "fallback")
})

test_that("value_or_default håndterer empty strings", {
  result <- value_or_default("", default = "fallback")
  expect_equal(result, "fallback")

  result <- value_or_default("   ", default = "fallback")
  expect_equal(result, "fallback")
})

test_that("value_or_default håndterer empty vectors", {
  result <- value_or_default(character(0), default = "fallback")
  expect_equal(result, "fallback")

  result <- value_or_default(numeric(0), default = 0)
  expect_equal(result, 0)
})

test_that("value_or_default returnerer gyldige værdier", {
  result <- value_or_default("valid_value", default = "fallback")
  expect_equal(result, "valid_value")

  result <- value_or_default(42, default = 0)
  expect_equal(result, 42)
})

test_that("value_or_default validerer types", {
  # Skal returnere værdi ved korrekt type
  result <- value_or_default(42, default = 0, allowed_types = "numeric")
  expect_equal(result, 42)

  # Skal returnere fallback ved forkert type
  result <- value_or_default("text", default = 0, allowed_types = "numeric")
  expect_equal(result, 0)
})

test_that("validate_column_exists returnerer FALSE for NULL data", {
  result <- validate_column_exists(NULL, "column_name")
  expect_false(result)
})

test_that("validate_column_exists returnerer FALSE for NULL column_name", {
  test_data <- data.frame(x = 1:5)
  result <- validate_column_exists(test_data, NULL)
  expect_false(result)
})

test_that("validate_column_exists returnerer FALSE for empty string column", {
  test_data <- data.frame(x = 1:5)
  result <- validate_column_exists(test_data, "")
  expect_false(result)
})

test_that("validate_column_exists finder eksisterende kolonner", {
  test_data <- data.frame(Dato = 1:5, Værdi = 6:10)

  expect_true(validate_column_exists(test_data, "Dato"))
  expect_true(validate_column_exists(test_data, "Værdi"))
  expect_false(validate_column_exists(test_data, "NonExistent"))
})

test_that("validate_column_exists returnerer column data", {
  test_data <- data.frame(Dato = 1:5, Værdi = 6:10)

  result <- validate_column_exists(test_data, "Dato", return_column = TRUE)
  expect_equal(result, 1:5)

  result <- validate_column_exists(test_data, "NonExistent", return_column = TRUE, fallback = numeric(0))
  expect_equal(result, numeric(0))
})

test_that("validate_function_exists detekterer functions korrekt", {
  # Test med eksisterende function
  test_func <- function() {}
  assign("test_func", test_func, envir = .GlobalEnv)

  expect_true(validate_function_exists("test_func"))

  # Cleanup
  if (exists("test_func", envir = .GlobalEnv)) {
    rm("test_func", envir = .GlobalEnv)
  }
})

test_that("validate_function_exists returnerer FALSE for non-existent functions", {
  expect_false(validate_function_exists("non_existent_function_xyz"))
})

test_that("validate_function_exists håndterer NULL og empty strings", {
  expect_false(validate_function_exists(NULL))
  expect_false(validate_function_exists(""))
  expect_false(validate_function_exists("   "))
})

test_that("validate_config_value ekstraherer gyldige config values", {
  config <- list(
    x_col = "Dato",
    y_col = "Værdi",
    empty_field = "",
    null_field = NULL
  )

  expect_equal(validate_config_value(config, "x_col", default = "fallback"), "Dato")
  expect_equal(validate_config_value(config, "y_col", default = "fallback"), "Værdi")
})

test_that("validate_config_value returnerer default for NULL fields", {
  config <- list(x_col = "Dato", null_field = NULL)

  result <- validate_config_value(config, "null_field", default = "default_value")
  expect_equal(result, "default_value")
})

test_that("validate_config_value returnerer default for empty strings", {
  config <- list(empty_field = "")

  result <- validate_config_value(config, "empty_field", default = "default_value", allow_empty = FALSE)
  expect_equal(result, "default_value")

  result <- validate_config_value(config, "empty_field", default = "default_value", allow_empty = TRUE)
  expect_equal(result, "")
})

test_that("validate_config_value returnerer default for non-existent fields", {
  config <- list(x_col = "Dato")

  result <- validate_config_value(config, "non_existent", default = "default_value")
  expect_equal(result, "default_value")
})

test_that("validate_config_value håndterer environment objects", {
  config_env <- new.env()
  config_env$x_col <- "Dato"
  config_env$y_col <- "Værdi"

  expect_equal(validate_config_value(config_env, "x_col", default = "fallback"), "Dato")
  expect_equal(validate_config_value(config_env, "y_col", default = "fallback"), "Værdi")
})

test_that("validate_reactive_value håndterer simple reactive values", {
  skip_if_not_installed("shiny")

  # Mock reactive value
  test_reactive <- shiny::reactiveVal(42)

  result <- validate_reactive_value(test_reactive, default = 0, use_isolate = TRUE)
  expect_equal(result, 42)
})

test_that("validate_reactive_value returnerer default ved NULL", {
  skip_if_not_installed("shiny")

  test_reactive <- shiny::reactiveVal(NULL)

  result <- validate_reactive_value(test_reactive, default = 0)
  expect_equal(result, 0)
})

test_that("validate_reactive_value håndterer nested fields", {
  skip_if_not_installed("shiny")

  test_values <- shiny::reactiveValues(
    section = list(field = "value")
  )

  result <- validate_reactive_value(test_values$section, field = "field", default = "fallback")
  expect_equal(result, "value")
})

test_that("validate_state_transition validerer multiple checks", {
  skip_if_not_installed("shiny")

  # Mock app_state
  app_state <- list(
    data = shiny::reactiveValues(
      updating_table = FALSE,
      current_data = data.frame(x = 1:5)
    ),
    columns = shiny::reactiveValues(
      auto_detect = shiny::reactiveValues(in_progress = FALSE)
    )
  )

  # Alle checks skal bestå
  validation <- validate_state_transition(
    app_state,
    checks = list(
      not_updating = !app_state$data$updating_table,
      has_data = !is.null(app_state$data$current_data),
      auto_detect_ready = !app_state$columns$auto_detect$in_progress
    ),
    operation_name = "Test operation"
  )

  expect_true(validation$valid)
  expect_equal(length(validation$failed_checks), 0)
})

test_that("validate_state_transition detekterer failing checks", {
  skip_if_not_installed("shiny")

  # Mock app_state med fejlende conditions
  app_state <- list(
    data = shiny::reactiveValues(
      updating_table = TRUE,
      current_data = NULL
    )
  )

  validation <- validate_state_transition(
    app_state,
    checks = list(
      not_updating = !app_state$data$updating_table,
      has_data = !is.null(app_state$data$current_data)
    ),
    operation_name = "Test operation"
  )

  expect_false(validation$valid)
  expect_equal(length(validation$failed_checks), 2)
  expect_true("not_updating" %in% validation$failed_checks)
  expect_true("has_data" %in% validation$failed_checks)
})

test_that("validate_state_transition respekterer allow_proceed flag", {
  skip_if_not_installed("shiny")

  app_state <- list(
    data = shiny::reactiveValues(updating_table = TRUE)
  )

  # Med allow_proceed = TRUE skal valid være TRUE selv med failing checks
  validation <- validate_state_transition(
    app_state,
    checks = list(not_updating = !app_state$data$updating_table),
    operation_name = "Test operation",
    allow_proceed = TRUE
  )

  expect_true(validation$valid)
  expect_equal(length(validation$failed_checks), 1)
})

test_that("Integration: Alle guard functions håndterer edge cases gracefully", {
  # NULL inputs
  expect_null(validate_data_or_return(NULL))
  expect_equal(value_or_default(NULL, default = "x"), "x")
  expect_false(validate_column_exists(NULL, "col"))
  expect_false(validate_function_exists(NULL))
  expect_null(validate_config_value(NULL, "field"))

  # Empty strings
  expect_equal(value_or_default("", default = "x"), "x")
  expect_false(validate_column_exists(data.frame(), ""))
  expect_false(validate_function_exists(""))

  # Empty vectors
  expect_equal(value_or_default(character(0), default = "x"), "x")
})
