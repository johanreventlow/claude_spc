# Test for ExcelR data reconstruction with tidyverse patterns
# Ensures name-based column matching and robust type conversion

test_that("ExcelR data reconstruction uses name-based matching", {
  # Mock excelR data structure
  mock_excel_data <- list(
    data = list(
      list("2024-01-01", 25, 100, TRUE, FALSE, "Test comment 1"),
      list("2024-01-02", 30, 120, FALSE, TRUE, "Test comment 2"),
      list("2024-01-03", 20, 80, TRUE, FALSE, "Test comment 3")
    ),
    colHeaders = list("Dato", "Tæller", "Nævner", "Skift", "Frys", "Kommentar")
  )

  # Test that purrr::map_dfr creates correct structure
  col_names <- unlist(mock_excel_data$colHeaders)
  row_list <- mock_excel_data$data

  result_df <- purrr::map_dfr(row_list, function(row_data) {
    if (length(row_data) < length(col_names)) {
      row_data <- c(row_data, rep(NA, length(col_names) - length(row_data)))
    }
    named_row <- stats::setNames(row_data[seq_along(col_names)], col_names)
    tibble::as_tibble_row(named_row)
  })

  # Verify structure
  expect_equal(nrow(result_df), 3)
  expect_equal(ncol(result_df), 6)
  expect_equal(names(result_df), c("Dato", "Tæller", "Nævner", "Skift", "Frys", "Kommentar"))

  # Test type conversion with dplyr::across
  converted_df <- result_df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c("Skift", "Frys")),
        ~ dplyr::case_when(
          is.character(.x) ~ .x %in% c("TRUE", "true"),
          is.logical(.x) ~ .x,
          .default = as.logical(.x)
        )
      ),
      dplyr::across(
        dplyr::any_of(c("Tæller", "Nævner")),
        ~ as.numeric(.x)
      ),
      dplyr::across(
        dplyr::any_of(c("Dato", "Kommentar")),
        ~ as.character(.x)
      )
    )

  # Verify types
  expect_type(converted_df$Skift, "logical")
  expect_type(converted_df$Frys, "logical")
  expect_type(converted_df$Tæller, "double")
  expect_type(converted_df$Nævner, "double")
  expect_type(converted_df$Dato, "character")
  expect_type(converted_df$Kommentar, "character")

  # Verify values
  expect_equal(converted_df$Skift, c(TRUE, FALSE, TRUE))
  expect_equal(converted_df$Frys, c(FALSE, TRUE, FALSE))
  expect_equal(converted_df$Tæller, c(25, 30, 20))
  expect_equal(converted_df$Nævner, c(100, 120, 80))
})

test_that("ExcelR data reconstruction handles edge cases", {
  # Test with missing columns
  sparse_data <- list(
    data = list(
      list("2024-01-01", 25), # Missing later columns
      list("2024-01-02", 30, 120, TRUE), # Missing last columns
      list("2024-01-03", 20, 80, FALSE, TRUE, "Complete")
    ),
    colHeaders = list("Dato", "Tæller", "Nævner", "Skift", "Frys", "Kommentar")
  )

  col_names <- unlist(sparse_data$colHeaders)
  row_list <- sparse_data$data

  result_df <- purrr::map_dfr(row_list, function(row_data) {
    if (length(row_data) < length(col_names)) {
      row_data <- c(row_data, rep(NA, length(col_names) - length(row_data)))
    }
    named_row <- stats::setNames(row_data[seq_along(col_names)], col_names)
    tibble::as_tibble_row(named_row)
  })

  # Should handle NA values correctly
  expect_equal(nrow(result_df), 3)
  expect_true(is.na(result_df$Nævner[1])) # Missing from first row
  expect_true(is.na(result_df$Kommentar[2])) # Missing from second row
  expect_false(is.na(result_df$Kommentar[3])) # Present in third row
})

test_that("ExcelR type conversion handles string boolean representations", {
  # Test mixed boolean representations
  mixed_bool_data <- list(
    data = list(
      list("2024-01-01", 25, 100, "TRUE", "false"),
      list("2024-01-02", 30, 120, "true", "FALSE"),
      list("2024-01-03", 20, 80, TRUE, FALSE),
      list("2024-01-04", 35, 140, FALSE, TRUE)
    ),
    colHeaders = list("Dato", "Tæller", "Nævner", "Skift", "Frys")
  )

  col_names <- unlist(mixed_bool_data$colHeaders)
  row_list <- mixed_bool_data$data

  result_df <- purrr::map_dfr(row_list, function(row_data) {
    # Ensure all elements are character first to avoid type conflicts
    row_data_char <- as.character(row_data[seq_along(col_names)])
    named_row <- stats::setNames(row_data_char, col_names)
    tibble::as_tibble_row(named_row)
  })

  # Apply type conversion
  converted_df <- result_df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c("Skift", "Frys")),
        ~ .x %in% c("TRUE", "true")
      )
    )

  # Verify boolean conversion
  expect_equal(converted_df$Skift, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(converted_df$Frys, c(FALSE, FALSE, FALSE, TRUE))
  expect_type(converted_df$Skift, "logical")
  expect_type(converted_df$Frys, "logical")
})