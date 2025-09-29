# test-anhoej-results-update.R
# Sikrer at clearing af baseline (centerline) opdaterer anhoej_results korrekt

test_that("update_anhoej_results: updates with valid metrics", {
  previous <- list(
    longest_run = 10,
    n_crossings = 5,
    has_valid_data = TRUE
  )

  qic_results <- list(
    longest_run = 8,
    n_crossings = 6
  )

  updated <- update_anhoej_results(previous, qic_results, centerline_changed = FALSE)
  expect_equal(updated$longest_run, 8)
  expect_equal(updated$n_crossings, 6)
  expect_true(updated$has_valid_data)
})

test_that("update_anhoej_results: clears stale metrics when centerline is cleared", {
  # Tidligere gyldige værdier
  previous <- list(
    longest_run = 12,
    n_crossings = 4,
    has_valid_data = TRUE
  )

  # Nye beregninger giver NA (fx midlertidig eller ikke-run chart)
  qic_results <- list(
    longest_run = NA_real_,
    n_crossings = NA_real_,
    message = NULL
  )

  updated <- update_anhoej_results(previous, qic_results, centerline_changed = TRUE)

  # Vi skal ikke bevare gamle tal – UI skal afspejle den nye tilstand
  expect_true(is.na(updated$longest_run))
  expect_true(is.na(updated$n_crossings))
  expect_false(isTRUE(updated$has_valid_data))
  expect_type(updated$message, "character")
})

test_that("update_anhoej_results: preserves previous when no change and NA metrics", {
  previous <- list(
    longest_run = 9,
    n_crossings = 3,
    has_valid_data = TRUE
  )

  qic_results <- list(
    longest_run = NA_real_,
    n_crossings = NA_real_
  )

  updated <- update_anhoej_results(previous, qic_results, centerline_changed = FALSE)
  expect_equal(updated$longest_run, 9)
  expect_equal(updated$n_crossings, 3)
  expect_true(updated$has_valid_data)
})

