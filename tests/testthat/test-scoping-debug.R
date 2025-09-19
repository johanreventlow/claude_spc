# test-scoping-debug.R
# Test for at forstå R scoping problem

context("R Scoping Debug")

test_that("R list modification scoping test", {
  # TEST: Verificer hvordan R håndterer list modification

  # Function der modificerer list parameter
  modify_list <- function(input_list) {
    input_list$new_value <- "added"
    if (exists("log_debug", mode = "function")) {
      log_debug("Inside function, input_list$new_value =", input_list$new_value, .context = "TEST_SCOPING")
    } else {
      message("[TEST_SCOPING] Inside function, input_list$new_value =", input_list$new_value)
    }
    return(input_list)
  }

  # Mock list
  my_list <- list(existing = "value")

  # Call function
  result <- modify_list(my_list)

  # TEST: Original list er ikke modificeret
  expect_null(my_list$new_value)

  # TEST: Return value har modification
  expect_equal(result$new_value, "added")
})

test_that("detect_columns_name_only return value test", {
  # TEST: Test om detect_columns_name_only returnerer complete information

  # Source required functions
  source("../../R/fct_data_processing.R")

  standard_cols <- c("Dato", "Tæller")
  mock_session <- list(token = "test_session")
  mock_values <- list()

  result <- detect_columns_name_only(standard_cols, NULL, mock_session, mock_values, NULL)

  # TEST: Return value struktur
  expect_true(is.list(result))
  expect_equal(result$x_col, "Dato")
  expect_equal(result$taeller_col, "Tæller")

  # TEST: Original mock_values er ikke modificeret (expected behavior i R)
  expect_null(mock_values$auto_detected_columns)
  expect_null(mock_values$ui_sync_needed)
})
