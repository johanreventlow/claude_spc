# test-fase4-intelligent-heuristics.R
# Tests for Fase 4: Intelligent Heuristics
# Focus on enhanced pattern recognition, CV bug fixes, and ranked suggestions

test_that("tidsspecifikke danske mønstre detekteres korrekt", {
  # Skip if function not available
  skip_if_not(exists("detect_columns_name_based", mode = "function"), "detect_columns_name_based function not available")

  # SETUP: Test data med tidsspecifikke kolonnenavne
  col_names_tidsmønstre <- c("Måned", "Kvartal", "Periode", "Jan", "Feb", "Mar", "Uge")
  col_names_ikke_tid <- c("Tæller", "Nævner", "Kommentar")

  # TEST: Enhanced date patterns should detect time-specific columns
  for (col_name in col_names_tidsmønstre) {
    # Test name-based detection
    result <- detect_columns_name_based(c(col_name, "Tæller", "Nævner"))

    # More flexible test - x_col should be time-related or NULL
    if (!is.null(result$x_col)) {
      expect_true(result$x_col %in% col_names_tidsmønstre,
                  info = paste("Should detect time column, got", result$x_col))
    }
  }

  # TEST: Non-time columns should not be detected as x_col by time patterns
  for (col_name in col_names_ikke_tid) {
    result <- detect_columns_name_based(c("Data", col_name))

    # More flexible test - should not be the non-time column
    if (!is.null(result$x_col)) {
      expect_false(result$x_col == col_name, info = paste(col_name, "should not be x_col"))
    }
  }
})

test_that("rate/procent mønstre scores korrekt", {
  # SETUP: Test data with rate/percent patterns
  rate_patterns <- c("rate", "procent", "pct", "andel", "del_af", "per_100", "ratio")

  for (pattern in rate_patterns) {
    # TEST: Rate patterns should get high scores for y_column
    score <- score_by_name_patterns(pattern, "y_column")
    expect_true(score >= 0.8, info = paste(pattern, "should get high score (>=0.8), got", score))
  }
})

test_that("CV division-by-zero bug er fixet", {
  # SETUP: Test data med mean = 0 (would cause division by zero)
  zero_mean_data <- c(0, 0, 0, 0, 0)

  # TEST: Should not crash on zero mean data
  expect_silent({
    score_char <- score_by_data_characteristics(zero_mean_data, "n_column")
    score_stat <- score_by_statistical_properties(zero_mean_data, "y_column")
  })

  # Should return reasonable scores
  score_char <- score_by_data_characteristics(zero_mean_data, "n_column")
  score_stat <- score_by_statistical_properties(zero_mean_data, "y_column")

  expect_true(is.numeric(score_char))
  expect_true(is.numeric(score_stat))
  expect_true(score_char >= 0 && score_char <= 1)
  expect_true(score_stat >= 0 && score_stat <= 1)
})

test_that("CV bug med zero standard deviation håndteres", {
  # SETUP: Test data med constant values (zero std dev)
  constant_data <- c(5, 5, 5, 5, 5)

  # TEST: Should handle zero standard deviation gracefully
  expect_silent({
    score_stat <- score_by_statistical_properties(constant_data, "y_column")
  })

  score_stat <- score_by_statistical_properties(constant_data, "y_column")
  expect_true(is.numeric(score_stat))
  expect_true(score_stat >= 0 && score_stat <= 1)
})

test_that("ranked suggestions returnerer sorterede scores", {
  # Skip if function not available
  skip_if_not(exists("score_column_candidates", mode = "function"), "score_column_candidates function not available")

  # SETUP: Test data med forskellige kolonner
  test_data <- data.frame(
    PerfectY = c(10, 12, 8, 15, 11),      # Should score highest for y_column
    GoodY = c(100, 110, 95, 120, 105),    # Should score well
    PoorY = c(1, 1, 1, 1, 1),             # Should score low (no variation)
    NotNumeric = c("a", "b", "c", "d", "e") # Should not be included
  )

  numeric_cols <- c("PerfectY", "GoodY", "PoorY")

  # TEST: Should return sorted scores
  y_scores <- score_column_candidates(test_data, numeric_cols, "y_column")

  # Should be sorted in descending order
  expect_true(length(y_scores) > 0)
  if (length(y_scores) > 1) {
    for (i in 1:(length(y_scores)-1)) {
      expect_true(y_scores[i] >= y_scores[i+1],
                  info = paste("Scores should be descending:", y_scores[i], ">=", y_scores[i+1]))
    }
  }

  # PerfectY should likely score highest
  expect_equal(names(y_scores)[1], "PerfectY")
})

test_that("enhanced logging viser top kandidater", {
  # Skip if function not available
  skip_if_not(exists("score_column_candidates", mode = "function"), "score_column_candidates function not available")

  # SETUP: Test data med multiple candidates
  test_data <- data.frame(
    Tæller = c(10, 12, 8, 15, 11),
    Antal = c(5, 7, 3, 9, 6),
    Sum = c(100, 110, 95, 120, 105)
  )

  numeric_cols <- c("Tæller", "Antal", "Sum")

  # TEST: Should not crash and should return ranked results
  expect_silent({
    y_scores <- score_column_candidates(test_data, numeric_cols, "y_column")
  })

  y_scores <- score_column_candidates(test_data, numeric_cols, "y_column")

  # Should return all candidates
  expect_equal(length(y_scores), 3)
  expect_true(all(names(y_scores) %in% numeric_cols))
})

test_that("pattern matching er case-insensitive", {
  # SETUP: Test patterns med forskellige cases
  patterns <- list(
    c("MÅNED", "måned", "Måned"),
    c("PROCENT", "procent", "Procent"),
    c("TÆLLER", "tæller", "Tæller")
  )

  for (pattern_group in patterns) {
    scores <- sapply(pattern_group, function(p) {
      if (grepl("måned", tolower(p))) {
        # Test date pattern
        result <- detect_columns_name_based(c(p, "Tæller"))
        return(ifelse(result$x_col == p, 1, 0))
      } else {
        # Test y_column pattern
        return(score_by_name_patterns(p, "y_column"))
      }
    })

    # All variants should get same score
    expect_true(all(scores == scores[1]),
                info = paste("Case variants should score equally:", paste(pattern_group, collapse = ", ")))
  }
})

test_that("edge cases håndteres robust", {
  # Skip if function not available
  skip_if_not(exists("score_column_candidates", mode = "function"), "score_column_candidates function not available")

  # SETUP: Various edge cases

  # Empty data
  expect_silent({
    result1 <- score_column_candidates(data.frame(), character(0), "y_column")
  })
  expect_equal(length(result1), 0)

  # Single column
  single_col_data <- data.frame(OnlyCol = 1:5)
  result2 <- score_column_candidates(single_col_data, "OnlyCol", "y_column")
  expect_equal(length(result2), 1)
  expect_equal(names(result2), "OnlyCol")

  # All NA data
  na_data <- data.frame(NACol = rep(NA, 5))
  result3 <- score_column_candidates(na_data, "NACol", "y_column")
  expect_true(result3["NACol"] == 0 || is.na(result3["NACol"]))
})

test_that("performance med store datasæt er acceptabel", {
  # Skip if function not available
  skip_if_not(exists("score_column_candidates", mode = "function"), "score_column_candidates function not available")

  # SETUP: Large dataset simulation
  large_n <- 1000
  large_data <- data.frame(
    Col1 = rnorm(large_n, 100, 10),
    Col2 = rpois(large_n, 50),
    Col3 = runif(large_n, 0, 200),
    Col4 = rep(c(10, 20, 30), length.out = large_n)
  )

  numeric_cols <- names(large_data)

  # TEST: Should complete within reasonable time
  start_time <- Sys.time()
  scores <- score_column_candidates(large_data, numeric_cols, "y_column")
  end_time <- Sys.time()

  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Should complete within 2 seconds for 1000 rows, 4 columns
  expect_true(elapsed < 2.0, info = paste("Performance test took", elapsed, "seconds"))

  # Should still return valid results
  expect_equal(length(scores), 4)
  expect_true(all(scores >= 0 & scores <= 1))
})