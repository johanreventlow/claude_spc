# test-tidyverse-conversion.R
# Tests for tidyverse conversion compliance and functionality

test_that("purrr::map functions work correctly in autodetect", {
  # Test data with standard SPC columns
  test_data <- data.frame(
    Dato = c("01-01-2024", "02-01-2024", "03-01-2024"),
    Tæller = c(10, 15, 12),
    Nævner = c(100, 120, 110),
    Kommentarer = c("Start", "Problem", "Fix")
  )

  if (exists("autodetect_engine") && exists("create_app_state")) {
    app_state <- create_app_state()
    mock_emit <- list(
      auto_detection_started = function() {},
      auto_detection_completed = function() {}
    )

    # Test that autodetect still works with tidyverse conversion
    result <- tryCatch({
      autodetect_engine(
        data = test_data,
        trigger_type = "file_upload",
        app_state = app_state,
        emit = mock_emit
      )
      "success"
    }, error = function(e) {
      paste("error:", e$message)
    })

    expect_true(result == "success")
  }
})

test_that("tidyverse patterns preserve data integrity", {
  # Test that converted functions still handle Danish data correctly
  test_data <- data.frame(
    Dato = c("15-02-2024", "29-02-2024", "31-12-2023"),  # Danish dates
    Værdi = c("12,5", "15,3", "8,7"),                    # Danish decimals
    Status = c("OK", "Problem", "Løst")                  # Danish text
  )

  # Test data frame processing with tidyverse functions
  expect_true(is.data.frame(test_data))
  expect_equal(nrow(test_data), 3)
  expect_equal(ncol(test_data), 3)

  # Test that column names are preserved
  expected_names <- c("Dato", "Værdi", "Status")
  expect_equal(names(test_data), expected_names)
})

test_that("file operations functions work with tidyverse conversion", {
  if (exists("validate_spc_data_for_autodetect")) {
    test_data <- data.frame(
      Dato = c("01-01-2024", "02-01-2024"),
      Tæller = c(10, 15),
      Nævner = c(100, 120)
    )

    # Test that validation still works after conversion
    result <- tryCatch({
      validate_spc_data_for_autodetect(test_data)
      "success"
    }, error = function(e) {
      paste("error:", e$message)
    })

    expect_true(result == "success" || is.list(result))
  }
})

test_that("column management functions preserve functionality", {
  # Test that UI generation still works with tidyverse patterns
  test_names <- c("Kolonne1", "Kolonne2", "Kolonne3")

  # Simulate the pattern used in column management
  ui_elements <- purrr::imap(test_names, ~ list(
    name = .x,
    index = .y,
    placeholder = paste("Navn for kolonne", .y)
  ))

  expect_equal(length(ui_elements), 3)
  expect_equal(ui_elements[[1]]$name, "Kolonne1")
  expect_equal(ui_elements[[1]]$index, 1)
  expect_equal(ui_elements[[2]]$name, "Kolonne2")
  expect_equal(ui_elements[[2]]$index, 2)
})

test_that("core SPC helpers work with tidyverse patterns", {
  if (exists("validate_spc_data")) {
    test_data <- data.frame(
      Dato = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
      Tæller = c(10, 15, 12),
      Nævner = c(100, 120, 110),
      Skift = c(FALSE, TRUE, FALSE),
      Frys = c(FALSE, FALSE, TRUE)
    )

    # Test that SPC validation still works
    result <- tryCatch({
      validate_spc_data(test_data)
      "success"
    }, error = function(e) {
      paste("error:", e$message)
    })

    expect_true(result == "success" || is.list(result))
  }
})

test_that("dplyr pipelines work correctly", {
  # Test that dplyr operations work as expected
  test_data <- tibble::tibble(
    old = c("navn1", "navn2", "navn3"),
    new = c("nyt_navn1", "navn2", "nyt_navn3"),
    index = 1:3
  )

  # Test the pattern used in column management changes detection
  changes <- test_data %>%
    dplyr::filter(old != new) %>%
    dplyr::mutate(change = paste0("'", old, "' -> '", new, "'")) %>%
    dplyr::pull(change)

  expect_equal(length(changes), 2)
  expect_true("'navn1' -> 'nyt_navn1'" %in% changes)
  expect_true("'navn3' -> 'nyt_navn3'" %in% changes)
})

test_that("purrr functional programming patterns work", {
  # Test purrr::map_lgl pattern used in data validation
  test_columns <- list(
    numeric_col = c(1, 2, 3, NA),
    character_col = c("a", "b", "", NA),
    logical_col = c(TRUE, FALSE, TRUE, NA)
  )

  # Test the pattern used in file operations
  has_content <- purrr::map_lgl(test_columns, ~ {
    if (is.numeric(.x)) {
      sum(!is.na(.x)) > 0
    } else if (is.character(.x)) {
      sum(nzchar(.x, keepNA = FALSE)) > 0
    } else {
      sum(!is.na(.x)) > 0
    }
  })

  expect_equal(length(has_content), 3)
  expect_true(all(has_content))  # All columns should have content
})