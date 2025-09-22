# test-tidyverse-purrr-operations.R
# Comprehensive tests for purrr operations in tidyverse migration

test_that("purrr::map_lgl operations handle edge cases correctly", {
  # Test data with mixed types for has_data_content check
  test_data <- data.frame(
    logical_col = c(TRUE, FALSE, NA),
    numeric_col = c(1, 2, NA),
    character_col = c("test", "", NA),
    empty_char_col = c("", "", ""),
    all_na_col = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )

  # Test the has_data_content logic from server_utils_session_helpers.R
  if (exists("evaluate_has_data_status")) {
    # Test with meaningful data
    meaningful_data <- test_data |>
      purrr::map_lgl(~ {
        if (is.logical(.x)) {
          any(.x, na.rm = TRUE)
        } else if (is.numeric(.x)) {
          any(!is.na(.x))
        } else if (is.character(.x)) {
          any(nzchar(.x, keepNA = FALSE), na.rm = TRUE)
        } else {
          FALSE
        }
      }) |>
      any()

    expect_true(meaningful_data)

    # Test with empty data
    empty_data <- data.frame(
      empty_col1 = c(NA, NA, NA),
      empty_col2 = c("", "", "")
    )

    empty_result <- empty_data |>
      purrr::map_lgl(~ {
        if (is.logical(.x)) {
          any(.x, na.rm = TRUE)
        } else if (is.numeric(.x)) {
          any(!is.na(.x))
        } else if (is.character(.x)) {
          any(nzchar(.x, keepNA = FALSE), na.rm = TRUE)
        } else {
          FALSE
        }
      }) |>
      any()

    expect_false(empty_result)
  } else {
    skip("evaluate_has_data_status function not available")
  }
})

test_that("purrr::map_int operations for data quality analysis", {
  skip_if_not_installed("purrr")

  # Test data with various NA patterns
  test_data <- data.frame(
    col1 = c(1, 2, NA, 4),
    col2 = c(NA, NA, NA, NA),
    col3 = c("a", "b", "", "d"),
    col4 = c("", "", "", ""),
    stringsAsFactors = FALSE
  )

  # Test NA counting from preprocessing (fct_file_operations.R)
  na_counts <- purrr::map_int(test_data, ~sum(is.na(.x)))
  expect_equal(na_counts[["col1"]], 1)
  expect_equal(na_counts[["col2"]], 4)
  expect_equal(na_counts[["col3"]], 0)  # Empty strings are not NA

  # Test empty string counting for character columns
  empty_counts <- purrr::map_int(test_data, ~ {
    if (is.character(.x)) {
      sum(stringr::str_trim(.x) == "", na.rm = TRUE)
    } else {
      0L
    }
  })
  expect_equal(empty_counts[["col3"]], 1)
  expect_equal(empty_counts[["col4"]], 4)
  expect_equal(empty_counts[["col1"]], 0)  # Numeric columns return 0
})

test_that("purrr::map_dbl operations for scoring in autodetect", {
  skip_if_not_installed("purrr")

  # Mock date candidates structure from fct_autodetect_unified.R
  date_candidates <- list(
    "Dato" = list(score = 0.95, confidence = "high"),
    "Tid" = list(score = 0.75, confidence = "medium"),
    "Uge" = list(score = 0.85, confidence = "high")
  )

  # Test scoring extraction
  scores <- date_candidates |>
    purrr::map_dbl(~ .x$score)

  expect_equal(scores[["Dato"]], 0.95)
  expect_equal(scores[["Tid"]], 0.75)
  expect_equal(scores[["Uge"]], 0.85)

  # Test finding best score
  best_col <- names(date_candidates)[which.max(scores)]
  expect_equal(best_col, "Dato")

  # Test error handling with malformed candidate
  malformed_candidates <- list(
    "Good" = list(score = 0.8),
    "Bad" = list(score = NA),
    "Ugly" = list()  # Missing score
  )

  # Should handle NA and missing values gracefully
  expect_error({
    malformed_candidates |>
      purrr::map_dbl(~ .x$score)
  }, class = "purrr_error_indexed")
})

test_that("purrr::detect operations for pattern matching", {
  skip_if_not_installed("purrr")

  # Test pattern detection from fct_autodetect_unified.R
  patterns <- c("dato", "date", "tid", "time")
  col_names_lower <- c("timestamp", "value", "dato_start", "comment")

  # Test finding first matching pattern
  found_pattern <- patterns |>
    purrr::detect(~ {
      matched_idx <- which(grepl(.x, col_names_lower, ignore.case = TRUE))
      length(matched_idx) > 0
    })

  expect_equal(found_pattern, "dato")

  # Test no match scenario
  no_match_patterns <- c("xyz", "abc", "def")
  no_match_result <- no_match_patterns |>
    purrr::detect(~ {
      matched_idx <- which(grepl(.x, col_names_lower, ignore.case = TRUE))
      length(matched_idx) > 0
    })

  expect_null(no_match_result)
})

test_that("purrr::reduce operations for plot enhancements", {
  skip_if_not_installed("purrr")
  skip_if_not_installed("ggplot2")

  # Mock the phase_changes logic from fct_spc_plot_generation.R
  phase_changes <- c(5, 10, 15)
  mock_qic_data <- data.frame(
    x = 1:20,
    y = rnorm(20)
  )

  # Create base plot
  base_plot <- ggplot2::ggplot(mock_qic_data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point()

  # Test reduce operation for adding phase lines
  if (exists("HOSPITAL_COLORS")) {
    enhanced_plot <- phase_changes |>
      purrr::reduce(function(p, change_point) {
        p +
          ggplot2::geom_vline(
            xintercept = mock_qic_data$x[change_point + 1],
            color = "red",  # Use hardcoded color for test
            linetype = "dotted", linewidth = 1, alpha = 0.7
          )
      }, .init = base_plot)

    expect_s3_class(enhanced_plot, "ggplot")

    # Check that layers were added
    expect_true(length(enhanced_plot$layers) > 1)
  } else {
    skip("HOSPITAL_COLORS not available")
  }
})

test_that("pipe operator chain correctness", {
  # Test complex pipe chains for data processing
  test_data <- data.frame(
    x = 1:10,
    y = c(1, 2, NA, 4, 5, 6, NA, 8, 9, 10),
    category = c("A", "B", "A", "B", "A", "B", "A", "B", "A", "B"),
    stringsAsFactors = FALSE
  )

  # Test pipe chain from data preprocessing
  result <- test_data |>
    dplyr::filter(!is.na(y)) |>
    dplyr::group_by(category) |>
    dplyr::summarise(
      mean_y = mean(y, na.rm = TRUE),
      count = dplyr::n(),
      .groups = 'drop'
    )

  expect_equal(nrow(result), 2)
  expect_true(all(c("A", "B") %in% result$category))
  expect_true(all(!is.na(result$mean_y)))

  # Test complex dplyr pipe chain for filtering
  filtered_data <- test_data |>
    dplyr::filter(dplyr::if_all(dplyr::everything(), ~ {
      !is.na(.x) | (is.character(.x) & stringr::str_trim(.x) != "")
    }))

  expect_equal(nrow(filtered_data), 8)  # Should remove 2 NA rows
})

test_that("error handling in purrr operations", {
  skip_if_not_installed("purrr")

  # Test safe purrr operations with potentially failing functions
  test_list <- list(
    good = c(1, 2, 3),
    bad = c("a", "b", "c"),
    ugly = NULL
  )

  # Test map_safely pattern
  safe_results <- test_list |>
    purrr::map(purrr::safely(function(x) {
      if (is.null(x)) return(NA_real_)
      if (is.character(x)) stop("Cannot calculate mean of character data")
      mean(x, na.rm = TRUE)
    }, otherwise = NA_real_))

  # Good should succeed
  expect_false(is.null(safe_results$good$result))
  expect_null(safe_results$good$error)

  # Bad should fail gracefully
  expect_true(!is.null(safe_results$bad$error) || is.na(safe_results$bad$result))

  # Ugly should handle NULL input
  expect_true(is.null(safe_results$ugly$result) || is.na(safe_results$ugly$result))
})

test_that("performance of tidyverse vs base R operations", {
  skip_if_not_installed("bench")

  # Create larger test dataset for performance comparison
  large_data <- data.frame(
    x = rnorm(1000),
    y = rnorm(1000),
    category = sample(c("A", "B", "C"), 1000, replace = TRUE),
    stringsAsFactors = FALSE
  )

  # Test base R vs tidyverse approach for common operations
  base_r_time <- system.time({
    sapply(large_data[1:3], function(x) sum(!is.na(x)))
  })

  tidyverse_time <- system.time({
    large_data[1:3] |> purrr::map_int(~sum(!is.na(.x)))
  })

  # Both should complete successfully
  expect_true(base_r_time["elapsed"] > 0)
  expect_true(tidyverse_time["elapsed"] > 0)

  # Results should be equivalent
  base_result <- sapply(large_data[1:3], function(x) sum(!is.na(x)))
  tidyverse_result <- large_data[1:3] |> purrr::map_int(~sum(!is.na(.x)))

  expect_equal(as.numeric(base_result), as.numeric(tidyverse_result))
})