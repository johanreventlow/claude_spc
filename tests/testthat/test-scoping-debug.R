# test-scoping-debug.R
# Test for at forstå R scoping problem

# R Scoping Debug Tests

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
