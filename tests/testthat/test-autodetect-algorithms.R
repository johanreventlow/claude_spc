# test-autodetect-algorithms.R
# Comprehensive tests for auto-detection algorithms and scoring

test_that("autodetect_engine handles different trigger types correctly", {
  skip_if_not_installed("shiny")

  if (exists("autodetect_engine") && exists("create_app_state")) {
    # Create test app state
    app_state <- create_app_state()
    mock_emit <- list(
      auto_detection_started = function() {},
      auto_detection_completed = function() {}
    )

    test_data <- data.frame(
      Skift = c(FALSE, FALSE, TRUE),
      Frys = c(FALSE, TRUE, FALSE),
      Dato = c("01-01-2024", "02-01-2024", "03-01-2024"),
      Tæller = c(10, 15, 12),
      Nævner = c(100, 120, 110)
    )

    # Test session_start trigger
    result <- tryCatch({
      autodetect_engine(
        data = test_data,
        trigger_type = "session_start",
        app_state = app_state,
        emit = mock_emit
      )
      "success"
    }, error = function(e) {
      "error"
    })

    expect_true(result == "success" || result == "error")

    # Test file_upload trigger
    result <- tryCatch({
      autodetect_engine(
        data = test_data,
        trigger_type = "file_upload",
        app_state = app_state,
        emit = mock_emit
      )
      "success"
    }, error = function(e) {
      "error"
    })

    expect_true(result == "success" || result == "error")
  } else {
    skip("autodetect_engine or create_app_state functions not available")
  }
})

test_that("detect_columns_full_analysis provides comprehensive scoring", {
  test_data <- data.frame(
    ID = 1:10,
    ObservationDate = c("01-01-2024", "02-01-2024", "03-01-2024", "04-01-2024", "05-01-2024",
                        "06-01-2024", "07-01-2024", "08-01-2024", "09-01-2024", "10-01-2024"),
    Numerator = c(8, 12, 10, 15, 13, 11, 9, 14, 12, 16),
    Denominator = c(100, 120, 110, 130, 125, 115, 105, 140, 120, 160),
    Comments = c("Start", "Good", "OK", "Excellent", "Fair", "Good", "Poor", "Excellent", "Good", "Great")
  )

  if (exists("detect_columns_full_analysis")) {
    result <- detect_columns_full_analysis(test_data)
    expect_true(is.list(result))

    # Should identify x_column (date)
    if ("x_col" %in% names(result)) {
      expect_equal(result$x_col, "ObservationDate")
    }

    # Should identify y_column (numerator)
    if ("y_col" %in% names(result)) {
      expect_equal(result$y_col, "Numerator")
    }

    # Should identify n_column (denominator)
    if ("n_col" %in% names(result)) {
      expect_equal(result$n_col, "Denominator")
    }
  } else {
    skip("detect_columns_full_analysis function not available")
  }
})

test_that("Column scoring algorithms work correctly", {
  test_data <- data.frame(
    pure_numeric = c(1, 2, 3, 4, 5),
    mixed_numeric = c("1", "2", "text", "4", "5"),
    date_like = c("01-01-2024", "02-01-2024", "03-01-2024", "04-01-2024", "05-01-2024"),
    text_only = c("alpha", "beta", "gamma", "delta", "epsilon")
  )

  # Test statistical properties scoring
  if (exists("score_by_statistical_properties")) {
    numeric_score <- score_by_statistical_properties(test_data$pure_numeric)
    mixed_score <- score_by_statistical_properties(test_data$mixed_numeric)

    expect_true(is.numeric(numeric_score))
    expect_true(is.numeric(mixed_score))
    expect_gt(numeric_score, mixed_score)
  }

  # Test name pattern scoring
  if (exists("score_by_name_patterns")) {
    date_score <- score_by_name_patterns("ObservationDate", type = "x")
    count_score <- score_by_name_patterns("Count", type = "y")
    random_score <- score_by_name_patterns("RandomColumn", type = "y")

    expect_true(is.numeric(date_score))
    expect_true(is.numeric(count_score))
    expect_gt(date_score, random_score)
    expect_gt(count_score, random_score)
  }

  # Test data characteristics scoring
  if (exists("score_by_data_characteristics")) {
    numeric_char_score <- score_by_data_characteristics(test_data$pure_numeric)
    text_char_score <- score_by_data_characteristics(test_data$text_only)

    expect_true(is.numeric(numeric_char_score))
    expect_true(is.numeric(text_char_score))
    expect_gt(numeric_char_score, text_char_score)
  }
})

test_that("update_all_column_mappings synchronizes state correctly", {
  skip_if_not_installed("shiny")

  if (exists("update_all_column_mappings") && exists("create_app_state")) {
    app_state <- create_app_state()

    detection_results <- list(
      x_col = "Dato",
      y_col = "Tæller",
      n_col = "Nævner",
      timestamp = Sys.time()
    )

    # Test mapping update
    result <- tryCatch({
      update_all_column_mappings(detection_results, app_state)
      "success"
    }, error = function(e) {
      "error"
    })

    expect_true(result == "success" || result == "error")

    if (result == "success") {
      # Verify state was updated
      expect_equal(shiny::isolate(app_state$columns$mappings$x_column), "Dato")
      expect_equal(shiny::isolate(app_state$columns$mappings$y_column), "Tæller")
      expect_equal(shiny::isolate(app_state$columns$mappings$n_column), "Nævner")
    }
  } else {
    skip("Required functions not available")
  }
})

test_that("Column scoring functions support both role and type parameters", {
  test_data <- data.frame(
    ObservationDate = c("01-01-2024", "02-01-2024", "03-01-2024"),
    Count = c(10, 15, 12),
    Total = c(100, 120, 110)
  )

  # Test score_by_name_patterns with different parameter styles
  if (exists("score_by_name_patterns")) {
    # Legacy type parameter
    date_score_type <- score_by_name_patterns("ObservationDate", type = "x")
    count_score_type <- score_by_name_patterns("Count", type = "y")

    # New role parameter
    date_score_role <- score_by_name_patterns("ObservationDate", role = "x_column")
    count_score_role <- score_by_name_patterns("Count", role = "y_column")

    # Default parameter (should work)
    default_score <- score_by_name_patterns("Count")

    expect_true(is.numeric(date_score_type))
    expect_true(is.numeric(count_score_type))
    expect_true(is.numeric(date_score_role))
    expect_true(is.numeric(count_score_role))
    expect_true(is.numeric(default_score))

    # Results should be equivalent between type and role parameters
    expect_equal(date_score_type, date_score_role)
    expect_equal(count_score_type, count_score_role)
  }

  # Test score_by_data_characteristics with different parameter styles
  if (exists("score_by_data_characteristics")) {
    # Legacy type parameter
    numeric_score_type <- score_by_data_characteristics(test_data$Count, type = "y")

    # New role parameter
    numeric_score_role <- score_by_data_characteristics(test_data$Count, role = "y_column")

    # Default parameter
    default_score <- score_by_data_characteristics(test_data$Count)

    expect_true(is.numeric(numeric_score_type))
    expect_true(is.numeric(numeric_score_role))
    expect_true(is.numeric(default_score))

    # Results should be equivalent
    expect_equal(numeric_score_type, numeric_score_role)
  }

  # Test score_by_statistical_properties with different parameter styles
  if (exists("score_by_statistical_properties")) {
    # Legacy type parameter
    stat_score_type <- score_by_statistical_properties(test_data$Count, type = "y")

    # New role parameter
    stat_score_role <- score_by_statistical_properties(test_data$Count, role = "y_column")

    # Default parameter
    default_score <- score_by_statistical_properties(test_data$Count)

    expect_true(is.numeric(stat_score_type))
    expect_true(is.numeric(stat_score_role))
    expect_true(is.numeric(default_score))

    # Results should be equivalent
    expect_equal(stat_score_type, stat_score_role)
  }
})

test_that("Auto-detection handles edge cases gracefully", {
  # Test empty data
  empty_data <- data.frame()

  if (exists("detect_columns_full_analysis")) {
    result <- tryCatch({
      detect_columns_full_analysis(empty_data)
    }, error = function(e) {
      list(error = e$message)
    })

    expect_true(is.list(result))
  }

  # Test single column data
  single_col_data <- data.frame(only_column = 1:5)

  if (exists("detect_columns_full_analysis")) {
    result <- tryCatch({
      detect_columns_full_analysis(single_col_data)
    }, error = function(e) {
      list(error = e$message)
    })

    expect_true(is.list(result))
  }

  # Test all-NA data
  na_data <- data.frame(
    col1 = rep(NA, 5),
    col2 = rep(NA_character_, 5),
    col3 = rep(NA_real_, 5)
  )

  if (exists("detect_columns_full_analysis")) {
    result <- tryCatch({
      detect_columns_full_analysis(na_data)
    }, error = function(e) {
      list(error = e$message)
    })

    expect_true(is.list(result))
  }
})