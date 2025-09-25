# test-autodetect-tidyverse-integration.R
# Integration tests for autodetect engine with tidyverse patterns

test_that("autodetect_engine with tidyverse column detection", {
  skip_if_not_installed("purrr")

  if (exists("autodetect_engine") && exists("create_app_state")) {
    # Create comprehensive test data
    test_data <- data.frame(
      Dato = c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01"),
      Tæller = c(10, 15, 12, 18),
      Nævner = c(100, 120, 110, 130),
      Skift = c(FALSE, FALSE, TRUE, FALSE),
      Frys = c(FALSE, TRUE, FALSE, FALSE),
      Kommentar = c("", "Note", "", "Issue"),
      Uge_Tekst = c("Uge 1", "Uge 2", "Uge 3", "Uge 4"),
      stringsAsFactors = FALSE
    )

    app_state <- create_app_state()
    mock_emit <- list(
      auto_detection_started = function() {},
      auto_detection_completed = function() {}
    )

    # Test file_upload trigger with full data analysis
    result <- autodetect_engine(
      data = test_data,
      trigger_type = "file_upload",
      app_state = app_state,
      emit = mock_emit
    )

    expect_true(is.list(result))

    # Verify hierarchical state structure was updated
    expect_true(!is.null(shiny::isolate(app_state$columns$mappings$x_column)))
    expect_true(!is.null(shiny::isolate(app_state$columns$mappings$y_column)))
    expect_true(!is.null(shiny::isolate(app_state$columns$auto_detect$results)))
    expect_true(shiny::isolate(app_state$columns$auto_detect$completed))

    # Test that date column was detected (either Dato or Uge_Tekst)
    detected_x <- shiny::isolate(app_state$columns$mappings$x_column)
    expect_true(detected_x %in% c("Dato", "Uge_Tekst"))

    # Test that numeric columns were detected
    expect_equal(shiny::isolate(app_state$columns$mappings$y_column), "Tæller")
    expect_equal(shiny::isolate(app_state$columns$mappings$n_column), "Nævner")

    # Test special columns
    expect_equal(shiny::isolate(app_state$columns$mappings$skift_column), "Skift")
    expect_equal(shiny::isolate(app_state$columns$mappings$frys_column), "Frys")
    expect_equal(shiny::isolate(app_state$columns$mappings$kommentar_column), "Kommentar")
  } else {
    skip("autodetect_engine or create_app_state not available")
  }
})

test_that("detect_columns_name_based with purrr::detect patterns", {
  if (exists("detect_columns_name_based")) {
    # Test Danish column names
    danish_columns <- c("dato_start", "antal_patienter", "total_antal", "skift_dag", "kommentar_tekst")

    result <- detect_columns_name_based(danish_columns)

    expect_true(is.list(result))
    expect_equal(result$x_col, "dato_start")
    expect_equal(result$y_col, "antal_patienter")
    expect_equal(result$n_col, "total_antal")
    expect_equal(result$skift_col, "skift_dag")
    expect_equal(result$kommentar_col, "kommentar_tekst")

    # Test English column names
    english_columns <- c("date", "count", "denominator", "weekday", "notes")

    result_en <- detect_columns_name_based(english_columns)
    expect_equal(result_en$x_col, "date")
    expect_equal(result_en$y_col, "count")
    expect_equal(result_en$n_col, "denominator")
    expect_equal(result_en$skift_col, "weekday")
    expect_equal(result_en$kommentar_col, "notes")

    # Test no matches
    no_match_columns <- c("column1", "column2", "column3")
    result_none <- detect_columns_name_based(no_match_columns)
    expect_null(result_none$x_col)
    expect_null(result_none$y_col)
    expect_null(result_none$n_col)

    # Test empty input
    result_empty <- detect_columns_name_based(character(0))
    expect_true(all(sapply(result_empty, is.null)))
  } else {
    skip("detect_columns_name_based function not available")
  }
})

test_that("detect_columns_full_analysis with tidyverse data operations", {
  if (exists("detect_columns_full_analysis")) {
    # Create data with various column types for comprehensive testing
    analysis_data <- data.frame(
      timestamp = as.POSIXct(c("2024-01-01 10:00:00", "2024-01-02 11:00:00", "2024-01-03 12:00:00")),
      dato_tekst = c("01-01-2024", "02-01-2024", "03-01-2024"),
      measurement = c(95.5, 87.2, 92.1),
      count_events = c(5, 8, 6),
      total_cases = c(100, 120, 110),
      text_data = c("Normal", "High", "Normal"),
      mixed_numeric = c("1.5", "2.0", "3.5"),
      shift_indicator = c(FALSE, TRUE, FALSE),
      stringsAsFactors = FALSE
    )

    result <- detect_columns_full_analysis(analysis_data)

    expect_true(is.list(result))

    # Should detect timestamp or dato_tekst as x_col
    expect_true(result$x_col %in% c("timestamp", "dato_tekst"))

    # Should detect numeric columns
    expect_true(result$y_col %in% c("measurement", "count_events"))
    expect_true(result$n_col %in% c("total_cases"))

    # Test with edge case data
    edge_case_data <- data.frame(
      all_na = c(NA, NA, NA),
      all_empty = c("", "", ""),
      one_value = c(1, NA, NA),
      stringsAsFactors = FALSE
    )

    result_edge <- detect_columns_full_analysis(edge_case_data)
    # Should handle gracefully without throwing errors
    expect_true(is.list(result_edge))
  } else {
    skip("detect_columns_full_analysis function not available")
  }
})

test_that("update_all_column_mappings with hierarchical state", {
  if (exists("update_all_column_mappings") && exists("create_app_state")) {
    app_state <- create_app_state()

    # Mock detection results
    mock_results <- list(
      x_col = "Dato",
      y_col = "Tæller",
      n_col = "Nævner",
      skift_col = "Skift",
      frys_col = "Frys",
      kommentar_col = "Kommentar"
    )

    # Test direct app_state update
    updated_columns <- update_all_column_mappings(
      mock_results,
      existing_columns = NULL,
      app_state = app_state
    )

    # Verify hierarchical structure was updated correctly
    expect_equal(shiny::isolate(app_state$columns$mappings$x_column), "Dato")
    expect_equal(shiny::isolate(app_state$columns$mappings$y_column), "Tæller")
    expect_equal(shiny::isolate(app_state$columns$mappings$n_column), "Nævner")
    expect_equal(shiny::isolate(app_state$columns$mappings$skift_column), "Skift")
    expect_equal(shiny::isolate(app_state$columns$mappings$frys_column), "Frys")
    expect_equal(shiny::isolate(app_state$columns$mappings$kommentar_column), "Kommentar")

    # Verify auto_detect state
    expect_true(shiny::isolate(app_state$columns$auto_detect$completed))
    expect_false(shiny::isolate(app_state$columns$auto_detect$in_progress))
    expect_equal(shiny::isolate(app_state$columns$auto_detect$results), mock_results)

    # Test legacy compatibility (return value)
    expect_true(is.list(updated_columns))
    expect_equal(updated_columns$x_column, "Dato")
    expect_equal(updated_columns$y_column, "Tæller")
  } else {
    skip("update_all_column_mappings or create_app_state not available")
  }
})

test_that("frozen state management in autodetect engine", {
  if (exists("autodetect_engine") && exists("create_app_state")) {
    app_state <- create_app_state()
    mock_emit <- list(
      auto_detection_started = function() {},
      auto_detection_completed = function() {}
    )

    test_data <- data.frame(
      Dato = c("2024-01-01", "2024-02-01"),
      Tæller = c(10, 15),
      stringsAsFactors = FALSE
    )

    # First run should work
    result1 <- autodetect_engine(
      data = test_data,
      trigger_type = "file_upload",
      app_state = app_state,
      emit = mock_emit
    )

    expect_true(!is.null(result1))
    expect_true(shiny::isolate(app_state$columns$auto_detect$frozen_until_next_trigger))

    # Second run with same trigger should be blocked
    result2 <- autodetect_engine(
      data = test_data,
      trigger_type = "file_upload",
      app_state = app_state,
      emit = mock_emit
    )

    expect_null(result2)

    # Manual trigger should override frozen state
    result3 <- autodetect_engine(
      data = test_data,
      trigger_type = "manual",
      app_state = app_state,
      emit = mock_emit
    )

    expect_true(!is.null(result3))
  } else {
    skip("autodetect_engine not available")
  }
})

test_that("robust date detection with lubridate integration", {
  if (exists("detect_date_columns_robust")) {
    # Test data with various date formats
    date_test_data <- data.frame(
      danish_dates = c("01-01-2024", "02-01-2024", "03-01-2024"),
      iso_dates = c("2024-01-01", "2024-01-02", "2024-01-03"),
      text_dates = c("Jan 2024", "Feb 2024", "Mar 2024"),
      numbers = c(1, 2, 3),
      text = c("a", "b", "c"),
      mixed = c("2024-01-01", "not a date", "2024-01-03"),
      stringsAsFactors = FALSE
    )

    candidates <- detect_date_columns_robust(date_test_data)

    expect_true(is.list(candidates))
    expect_true(length(candidates) >= 1)

    # Should detect at least danish_dates and iso_dates
    expect_true("danish_dates" %in% names(candidates) || "iso_dates" %in% names(candidates))

    # Each candidate should have a score
    for (candidate in candidates) {
      expect_true("score" %in% names(candidate))
      expect_true(is.numeric(candidate$score))
      expect_true(candidate$score >= 0 && candidate$score <= 1)
    }
  } else {
    skip("detect_date_columns_robust function not available")
  }
})

test_that("numeric column scoring with tidyverse patterns", {
  if (exists("score_column_candidates")) {
    # Test data with various numeric characteristics
    scoring_data <- data.frame(
      perfect_counts = c(1, 2, 3, 4, 5),
      high_variance = c(1, 100, 2, 200, 3),
      low_values = c(0.1, 0.2, 0.3, 0.4, 0.5),
      mixed_na = c(1, NA, 3, NA, 5),
      all_same = c(10, 10, 10, 10, 10),
      text_numbers = c("1", "2", "3", "4", "5"),
      stringsAsFactors = FALSE
    )

    numeric_candidates <- c("perfect_counts", "high_variance", "low_values", "mixed_na", "all_same")

    # Test Y-column scoring
    y_scores <- score_column_candidates(scoring_data, numeric_candidates, role = "y_column")
    expect_true(is.numeric(y_scores))
    expect_true(length(y_scores) == length(numeric_candidates))
    expect_true(all(names(y_scores) %in% numeric_candidates))

    # Perfect counts should score highly
    expect_true(y_scores["perfect_counts"] > 0.5)

    # Test N-column scoring
    n_scores <- score_column_candidates(scoring_data, numeric_candidates, role = "n_column")
    expect_true(is.numeric(n_scores))
    expect_true(all(!is.na(n_scores)))
  } else {
    skip("score_column_candidates function not available")
  }
})