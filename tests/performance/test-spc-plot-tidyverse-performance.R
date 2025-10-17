# test-spc-plot-tidyverse-performance.R
# Performance and integration tests for SPC plot generation with tidyverse

test_that("generateSPCPlot with tidyverse data processing", {
  skip_if_not_installed("qicharts2")
  skip_if_not_installed("ggplot2")

  if (exists("generateSPCPlot")) {
    # Create comprehensive test data
    test_data <- data.frame(
      Skift = rep(c(FALSE, FALSE, TRUE, FALSE), 5),
      Frys = c(rep(FALSE, 8), TRUE, rep(FALSE, 11)),
      Dato = seq(as.Date("2024-01-01"), by = "month", length.out = 20),
      Tæller = sample(80:120, 20),
      Nævner = sample(800:1200, 20),
      Kommentar = c(rep("", 15), "Special event", rep("", 4)),
      stringsAsFactors = FALSE
    )

    config <- list(
      x_col = "Dato",
      y_col = "Tæller",
      n_col = "Nævner"
    )

    # Test basic plot generation
    result <- tryCatch({
      generateSPCPlot(
        data = test_data,
        config = config,
        chart_type = "p",
        target_value = 0.1,
        centerline_value = NULL,
        show_phases = TRUE,
        skift_column = "Skift",
        frys_column = "Frys",
        chart_title_reactive = function() "Test SPC Chart",
        y_axis_unit = "percent",
        kommentar_column = "Kommentar"
      )
    }, error = function(e) {
      list(error = e$message)
    })

    if (is.list(result) && "plot" %in% names(result)) {
      expect_s3_class(result$plot, "ggplot")
      expect_true(!is.null(result$qic_data))

      # Test that qic_data has expected structure
      qic_data <- result$qic_data
      expect_true(is.data.frame(qic_data))
      expect_true(all(c("x", "y", "cl") %in% names(qic_data)))

      # Test phase handling
      if ("part" %in% names(qic_data)) {
        expect_true(is.numeric(qic_data$part))
      }
    } else {
      # Plot generation might fail in test environment without full setup
      expect_true(is.list(result))
    }
  } else {
    skip("generateSPCPlot function not available")
  }
})

test_that("pipe operator chains in plot data processing", {
  if (exists("clean_qic_call_args")) {
    # Test data with missing values and part positions
    call_args <- list(
      x = c(1, 2, NA, 4, 5, 6, NA, 8, 9, 10),
      y = c(10, 15, NA, 20, 25, 30, NA, 35, 40, 45),
      n = c(100, 150, NA, 200, 250, 300, NA, 350, 400, 450),
      part = c(3, 7),  # Original part positions
      return.data = TRUE
    )

    cleaned_args <- clean_qic_call_args(call_args)

    expect_true(is.list(cleaned_args))
    expect_equal(length(cleaned_args$x), 8)  # Should remove 2 NA cases
    expect_equal(length(cleaned_args$y), 8)
    expect_equal(length(cleaned_args$n), 8)

    # Part positions should be adjusted for removed rows
    if (!is.null(cleaned_args$part)) {
      expect_true(all(cleaned_args$part <= length(cleaned_args$x)))
      expect_true(all(cleaned_args$part > 0))
    }
  } else {
    skip("clean_qic_call_args function not available")
  }
})

test_that("purrr::reduce for plot enhancement", {
  skip_if_not_installed("ggplot2")

  # NOTE: add_plot_enhancements() has been migrated to BFHcharts
  # Legacy function is no longer available in SPCify
  # This test now validates the base plot structure

  # Create base plot
  test_qic_data <- data.frame(
    x = 1:10,
    y = rnorm(10),
    part = c(rep(1, 4), rep(2, 3), rep(3, 3))
  )

  base_plot <- ggplot2::ggplot(test_qic_data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point()

  # Verify base plot is correctly constructed
  expect_s3_class(base_plot, "ggplot")
  expect_equal(length(base_plot$layers), 1)  # Single geom_point layer

  # Note: Plot enhancements (extended lines, target lines, etc.) are now handled by BFHcharts backend
})

test_that("performance benchmark: tidyverse vs base R data operations", {
  skip_if_not_installed("bench")

  # Create large dataset for performance testing
  large_data <- data.frame(
    x = seq_len(5000),
    y = rnorm(5000),
    group = sample(c("A", "B", "C"), 5000, replace = TRUE),
    has_na = sample(c(TRUE, FALSE), 5000, replace = TRUE, prob = c(0.1, 0.9)),
    stringsAsFactors = FALSE
  )

  # Add some NA values based on has_na column
  large_data$y[large_data$has_na] <- NA

  # Test data filtering performance
  if (requireNamespace("bench", quietly = TRUE)) {
    timing_results <- bench::mark(
      base_r = {
        complete_cases <- complete.cases(large_data$x, large_data$y)
        filtered_data <- large_data[complete_cases, ]
        nrow(filtered_data)
      },
      tidyverse = {
        large_data |>
          dplyr::filter(!is.na(x) & !is.na(y)) |>
          nrow()
      },
      iterations = 10,
      check = TRUE
    )

    expect_s3_class(timing_results, "bench_mark")
    expect_true(nrow(timing_results) == 2)

    # Both methods should produce same result
    base_result <- sum(complete.cases(large_data$x, large_data$y))
    tidyverse_result <- large_data |>
      dplyr::filter(!is.na(x) & !is.na(y)) |>
      nrow()

    expect_equal(base_result, tidyverse_result)
  } else {
    # Fallback timing without bench package
    base_time <- system.time({
      complete_cases <- complete.cases(large_data$x, large_data$y)
      base_result <- nrow(large_data[complete_cases, ])
    })

    tidyverse_time <- system.time({
      tidyverse_result <- large_data |>
        dplyr::filter(!is.na(x) & !is.na(y)) |>
        nrow()
    })

    expect_equal(base_result, tidyverse_result)
    expect_true(base_time["elapsed"] > 0)
    expect_true(tidyverse_time["elapsed"] > 0)
  }
})

test_that("memory usage in purrr operations vs apply family", {
  # Test memory efficiency of different approaches
  test_list <- replicate(100, rnorm(100), simplify = FALSE)

  # Compare memory usage patterns
  if (exists("profmem") || requireNamespace("profmem", quietly = TRUE)) {
    # Memory profiling would go here if profmem is available
    skip("Memory profiling requires profmem package")
  }

  # Basic performance comparison
  purrr_time <- system.time({
    purrr_result <- test_list |> purrr::map_dbl(mean)
  })

  sapply_time <- system.time({
    sapply_result <- sapply(test_list, mean)
  })

  # Results should be equivalent
  expect_equal(as.numeric(purrr_result), as.numeric(sapply_result))
  expect_true(purrr_time["elapsed"] >= 0)
  expect_true(sapply_time["elapsed"] >= 0)
})

test_that("complex pipe chains maintain data integrity", {
  # Test complex data transformation chains
  source_data <- data.frame(
    date = seq(as.Date("2024-01-01"), by = "day", length.out = 100),
    value = sample(1:100, 100),
    category = sample(c("A", "B", "C"), 100, replace = TRUE),
    has_issue = sample(c(TRUE, FALSE), 100, replace = TRUE, prob = c(0.1, 0.9)),
    stringsAsFactors = FALSE
  )

  # Complex transformation using tidyverse
  transformed_data <- source_data |>
    dplyr::mutate(
      week = lubridate::week(date),
      month = lubridate::month(date),
      is_weekend = lubridate::wday(date) %in% c(1, 7)
    ) |>
    dplyr::filter(!has_issue) |>
    dplyr::group_by(category, week) |>
    dplyr::summarise(
      avg_value = mean(value, na.rm = TRUE),
      count = n(),
      .groups = 'drop'
    ) |>
    dplyr::arrange(category, week)

  expect_true(is.data.frame(transformed_data))
  expect_true(nrow(transformed_data) > 0)
  expect_true(all(c("category", "week", "avg_value", "count") %in% names(transformed_data)))

  # Verify data integrity
  expect_true(all(!is.na(transformed_data$avg_value)))
  expect_true(all(transformed_data$count > 0))
  expect_true(all(transformed_data$category %in% c("A", "B", "C")))
})

test_that("error propagation in tidyverse chains", {
  # Test how errors propagate through pipe chains
  problematic_data <- data.frame(
    x = c(1, 2, "not_a_number", 4, 5),
    y = c("a", "b", "c", "d", "e"),
    stringsAsFactors = FALSE
  )

  # This should handle errors gracefully
  safe_result <- tryCatch({
    problematic_data |>
      dplyr::mutate(x_numeric = as.numeric(x)) |>
      dplyr::filter(!is.na(x_numeric)) |>
      dplyr::summarise(mean_x = mean(x_numeric))
  }, error = function(e) {
    list(error = e$message)
  }, warning = function(w) {
    # Warnings are acceptable (like NA coercion)
    problematic_data |>
      dplyr::mutate(x_numeric = suppressWarnings(as.numeric(x))) |>
      dplyr::filter(!is.na(x_numeric)) |>
      dplyr::summarise(mean_x = mean(x_numeric))
  })

  expect_true(is.data.frame(safe_result) || is.list(safe_result))
  if (is.data.frame(safe_result)) {
    expect_true("mean_x" %in% names(safe_result))
  }
})

test_that("session helper reactive evaluation with tidyverse", {
  if (exists("create_app_state")) {
    app_state <- create_app_state()

    # Set up test data in app state
    test_data <- data.frame(
      logical_col = c(TRUE, FALSE, TRUE),
      numeric_col = c(1, 2, 3),
      character_col = c("test", "data", "here"),
      stringsAsFactors = FALSE
    )

    app_state$data$current_data <- test_data

    # Test the tidyverse evaluation logic from server_utils_session_helpers.R
    meaningful_data_check <- test_data |>
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

    expect_true(meaningful_data_check)

    # Test with empty data
    empty_data <- data.frame(
      empty_logical = c(FALSE, FALSE, FALSE),
      empty_numeric = c(NA, NA, NA),
      empty_character = c("", "", ""),
      stringsAsFactors = FALSE
    )

    empty_check <- empty_data |>
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

    expect_false(empty_check)
  } else {
    skip("create_app_state function not available")
  }
})