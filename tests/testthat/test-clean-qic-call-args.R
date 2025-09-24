# test-clean-qic-call-args.R
# Focused unit tests for clean_qic_call_args utility

test_that("clean_qic_call_args adjusts freeze position after removing incomplete rows", {
  skip_if_not_installed("purrr")

  call_args <- list(
    x = 1:6,
    y = c(NA, 2, 3, 4, 5, 6),
    freeze = 5
  )

  cleaned_args <- clean_qic_call_args(call_args)

  expect_equal(cleaned_args$freeze, 4)
  expect_length(cleaned_args$x, 5)
  expect_length(cleaned_args$y, 5)
})

test_that("clean_qic_call_args drops invalid freeze positions", {
  skip_if_not_installed("purrr")

  call_args <- list(
    x = 1:4,
    y = c(NA, NA, 3, 4),
    freeze = 2
  )

  cleaned_args <- clean_qic_call_args(call_args)

  expect_false("freeze" %in% names(cleaned_args))
  expect_length(cleaned_args$x, 2)
  expect_length(cleaned_args$y, 2)
})
