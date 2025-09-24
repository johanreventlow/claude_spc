# test-comment-row-mapping.R
# Tests for stable row-id comment mapping fix

test_that("extract_comment_data uses stable row-id mapping", {
  # TEST: Comment mapping resilient to qic_data row reordering

  # Create test data with comments
  original_data <- data.frame(
    Dato = c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04"),
    Vaerdi = c(10, 15, 12, 18),
    Kommentar = c("First", "Second", "", "Fourth"),
    stringsAsFactors = FALSE
  )

  # Simulate qic_data that has reordered/filtered rows (common with phases)
  qic_data_reordered <- data.frame(
    x = as.Date(c("2023-01-04", "2023-01-01", "2023-01-03")),  # Reordered: 4,1,3
    y = c(18, 10, 12),  # Corresponding values
    .original_row_id = c(4, 1, 3),  # Row IDs from original data
    stringsAsFactors = FALSE
  )

  # Extract comments using new robust mapping
  comment_data <- extract_comment_data(original_data, "Kommentar", qic_data_reordered)

  # TEST: Should have 2 comments (row 1 and 4, row 3 has empty comment)
  expect_equal(nrow(comment_data), 2)

  # TEST: Comments should be correctly mapped to their original rows
  expect_true("Fourth" %in% comment_data$comment)  # From row 4
  expect_true("First" %in% comment_data$comment)   # From row 1
  expect_false("Second" %in% comment_data$comment) # Row 2 not in qic_data
  expect_false("" %in% comment_data$comment)       # Empty comment filtered out

  # TEST: x,y coordinates should match qic_data
  fourth_comment_row <- comment_data[comment_data$comment == "Fourth", ]
  expect_equal(fourth_comment_row$x, as.Date("2023-01-04"))
  expect_equal(fourth_comment_row$y, 18)

  first_comment_row <- comment_data[comment_data$comment == "First", ]
  expect_equal(first_comment_row$x, as.Date("2023-01-01"))
  expect_equal(first_comment_row$y, 10)
})

test_that("extract_comment_data handles missing row-id gracefully", {
  # TEST: Fallback to positional mapping when .original_row_id missing

  original_data <- data.frame(
    Dato = c("2023-01-01", "2023-01-02"),
    Vaerdi = c(10, 15),
    Kommentar = c("First", "Second"),
    stringsAsFactors = FALSE
  )

  # qic_data without .original_row_id column (legacy scenario)
  qic_data_no_id <- data.frame(
    x = as.Date(c("2023-01-01", "2023-01-02")),
    y = c(10, 15),
    stringsAsFactors = FALSE
  )

  # Should fall back to positional mapping and log warning
  comment_data <- extract_comment_data(original_data, "Kommentar", qic_data_no_id)

  # TEST: Should still work with positional mapping
  expect_equal(nrow(comment_data), 2)
  expect_true("First" %in% comment_data$comment)
  expect_true("Second" %in% comment_data$comment)
})

test_that("build_qic_arguments adds stable row-id", {
  # TEST: Row-id is added before sending to qicharts2

  test_data <- data.frame(
    Dato = c("2023-01-01", "2023-01-02", "2023-01-03"),
    Vaerdi = c(10, 15, 12),
    stringsAsFactors = FALSE
  )

  qic_args <- build_qic_arguments(
    data = test_data,
    x_col_for_qic = "Dato",
    y_col_name = "Vaerdi",
    n_col_name = NULL,
    chart_type = "run",
    freeze_position = NULL,
    part_positions = NULL,
    centerline_value = NULL
  )

  # TEST: Data should have .original_row_id column
  expect_true(".original_row_id" %in% names(qic_args$data))
  expect_equal(qic_args$data$.original_row_id, 1:3)

  # TEST: Original columns should be preserved
  expect_true("Dato" %in% names(qic_args$data))
  expect_true("Vaerdi" %in% names(qic_args$data))
  expect_equal(nrow(qic_args$data), 3)
})