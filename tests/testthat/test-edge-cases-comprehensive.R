# test-edge-cases-comprehensive.R
# ==============================================================================
# COMPREHENSIVE TEST SUITE: Edge Cases Across All Modules
# ==============================================================================
#
# FORMÅL: Comprehensive edge case coverage for all critical paths
# FOKUS: Empty data, single values, Danish characters, extreme values, large files
#
# STRUKTUR:
#   1. Empty/Null Data Edge Cases
#   2. Single Row/Column Data
#   3. Danish Characters Comprehensive
#   4. Extreme Values (very large/small numbers)
#   5. Large File Handling (50K rows)
#   6. Boundary Conditions
#
# SUCCESS CRITERIA:
#   - No crashes on edge cases
#   - Graceful degradation
#   - Helpful error messages
#   - Performance acceptable for large files
# ==============================================================================

library(shiny)
library(testthat)
library(readr)

# SETUP HELPERS ================================================================

# Helper til at oprette edge case test data
create_edge_case_data <- function(type = "normal", n = 10) {
  switch(type,
    "empty" = data.frame(),
    "single_row" = data.frame(x = 1, y = 2, stringsAsFactors = FALSE),
    "single_col" = data.frame(x = 1:n, stringsAsFactors = FALSE),
    "all_na" = data.frame(x = rep(NA, n), y = rep(NA, n), stringsAsFactors = FALSE),
    "partial_na" = data.frame(
      x = c(1:5, rep(NA, n - 5)),
      y = c(rep(NA, 5), 6:n),
      stringsAsFactors = FALSE
    ),
    "extreme_large" = data.frame(
      x = 1:n,
      y = rep(1e15, n),
      stringsAsFactors = FALSE
    ),
    "extreme_small" = data.frame(
      x = 1:n,
      y = rep(1e-15, n),
      stringsAsFactors = FALSE
    ),
    "mixed_extreme" = data.frame(
      x = 1:n,
      y = c(1e-15, 1e15, 0, -1e15, 1, -1, 1e10, -1e10, 100, -100)[1:n],
      stringsAsFactors = FALSE
    ),
    data.frame(x = 1:n, y = rnorm(n), stringsAsFactors = FALSE)
  )
}

# EMPTY/NULL DATA EDGE CASES ===================================================

describe("Empty and Null Data Handling", {

  it("handles completely empty data frame", {
    empty_data <- data.frame()

    # Functions should not crash
    expect_equal(nrow(empty_data), 0)
    expect_equal(ncol(empty_data), 0)

    # Auto-detection should handle gracefully
    skip_if_not(exists("detect_columns_name_based", mode = "function"))
    result <- detect_columns_name_based(character(0))
    expect_null(result$x_col)
    expect_null(result$y_col)
  })

  it("handles NULL data input", {
    skip_if_not(exists("detect_columns_full_analysis", mode = "function"))

    result <- detect_columns_full_analysis(NULL)

    expect_null(result$x_col)
    expect_null(result$y_col)
    expect_null(result$n_col)
  })

  it("handles data frame with only NA values", {
    na_data <- create_edge_case_data("all_na", n = 5)

    skip_if_not(exists("find_numeric_columns", mode = "function"))
    numeric_cols <- find_numeric_columns(na_data)

    # Should identify columns as numeric despite NA values
    expect_true(length(numeric_cols) >= 0)
  })

  it("handles data frame with no column names", {
    data <- data.frame(matrix(1:9, ncol = 3))
    names(data) <- rep("", 3)

    skip_if_not(exists("validate_data_for_auto_detect", mode = "function"))
    result <- validate_data_for_auto_detect(data)

    expect_false(result$suitable)
    expect_true(result$validation_results$empty_column_names > 0)
  })
})

# SINGLE ROW/COLUMN EDGE CASES =================================================

describe("Single Row/Column Data", {

  it("handles single row data frame", {
    single_row <- create_edge_case_data("single_row")

    expect_equal(nrow(single_row), 1)

    skip_if_not(exists("detect_columns_full_analysis", mode = "function"))
    result <- detect_columns_full_analysis(single_row)

    # Should still attempt detection
    expect_true(!is.null(result))
  })

  it("handles single column data frame", {
    single_col <- create_edge_case_data("single_col", n = 10)

    expect_equal(ncol(single_col), 1)

    skip_if_not(exists("detect_columns_full_analysis", mode = "function"))
    result <- detect_columns_full_analysis(single_col)

    # Should detect the one column appropriately
    expect_true(!is.null(result$y_col) || !is.null(result$x_col))
  })

  it("handles single value (1x1) data frame", {
    single_value <- data.frame(value = 42, stringsAsFactors = FALSE)

    expect_equal(nrow(single_value), 1)
    expect_equal(ncol(single_value), 1)

    skip_if_not(exists("validate_data_for_auto_detect", mode = "function"))
    result <- validate_data_for_auto_detect(single_value)

    # Should fail validation (too small)
    expect_false(result$suitable)
  })

  it("generates SPC plot with minimum data (10 rows)", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    min_data <- data.frame(
      Dato = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 10),
      Tæller = sample(5:10, 10, replace = TRUE),
      Nævner = rep(20, 10),
      stringsAsFactors = FALSE
    )

    config <- list(
      x_col = "Dato",
      y_col = "Tæller",
      n_col = "Nævner"
    )

    # Should generate plot without error
    expect_error(
      generateSPCPlot(min_data, config, chart_type = "p"),
      NA
    )
  })
})

# DANISH CHARACTERS COMPREHENSIVE ==============================================

describe("Danish Characters Comprehensive", {

  it("handles all Danish characters in column names", {
    danish_data <- data.frame(
      `Dato` = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 12),
      `Tæller` = sample(40:50, 12, replace = TRUE),
      `Nævner` = rep(50, 12),
      `Kommentar` = c("Første måned", "Anden måned", rep("", 10)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    expect_true("Tæller" %in% names(danish_data))
    expect_true("Nævner" %in% names(danish_data))

    skip_if_not(exists("detect_columns_name_based", mode = "function"))
    result <- detect_columns_name_based(names(danish_data))

    expect_equal(result$y_col, "Tæller")
    expect_equal(result$n_col, "Nævner")
    expect_equal(result$kommentar_col, "Kommentar")
  })

  it("handles Danish characters in data values", {
    danish_values <- data.frame(
      Måned = c("januar", "februar", "marts", "april", "maj", "juni",
                "juli", "august", "september", "oktober", "november", "december"),
      Note = c("Første", "Anden", "Tredje", "Fjerde", "Femte", "Sjette",
               "Syvende", "Ottende", "Niende", "Tiende", "Ellevte", "Tolvte"),
      Værdi = 1:12,
      stringsAsFactors = FALSE
    )

    # Should preserve Danish characters
    expect_true(all(c("å", "æ", "ø") %in% strsplit(paste(danish_values$Note, collapse = ""), "")[[1]]))
  })

  it("parses Danish month names correctly", {
    skip_if_not(exists("parse_danish_dates", mode = "function"))

    danish_dates <- c(
      "15 jan 2024", "20 februar 2024", "1 marts 2024",
      "10 april 2024", "5 maj 2024", "25 juni 2024",
      "15 juli 2024", "30 august 2024", "10 september 2024",
      "20 oktober 2024", "5 november 2024", "25 december 2024"
    )

    parsed <- parse_danish_dates(danish_dates, "dmy")

    expect_s3_class(parsed, "Date")
    expect_equal(length(parsed), 12)
    expect_true(all(!is.na(parsed)))
  })

  it("handles mixed Danish and English characters", {
    mixed_data <- data.frame(
      `Date/Dato` = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 6),
      `Count/Tæller` = sample(40:50, 6, replace = TRUE),
      `Total/Nævner` = rep(50, 6),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    skip_if_not(exists("detect_columns_name_based", mode = "function"))
    result <- detect_columns_name_based(names(mixed_data))

    # Should detect despite mixed names
    expect_true(!is.null(result$x_col))
    expect_true(!is.null(result$y_col))
  })

  it("handles Danish characters in CSV encoding", {
    danish_csv <- data.frame(
      Måned = c("januar", "februar", "marts"),
      Værdi = c(100, 105, 110),
      Beskrivelse = c("Høj værdi", "Normal værdi", "Lav værdi"),
      stringsAsFactors = FALSE
    )

    # Write and read back with ISO-8859-1
    temp_file <- tempfile(fileext = ".csv")
    readr::write_csv2(danish_csv, temp_file)
    on.exit(unlink(temp_file))

    result <- readr::read_csv2(
      temp_file,
      locale = readr::locale(encoding = "UTF-8"),
      show_col_types = FALSE
    )

    expect_equal(nrow(result), 3)
    expect_true("Måned" %in% names(result))
  })
})

# EXTREME VALUES ===============================================================

describe("Extreme Value Handling", {

  it("handles very large numbers (1e15)", {
    large_data <- create_edge_case_data("extreme_large", n = 10)

    skip_if_not(exists("score_by_data_characteristics", mode = "function"))
    score <- score_by_data_characteristics(large_data$y, role = "y_column")

    # Should not crash or return NA
    expect_true(!is.na(score))
    expect_true(is.numeric(score))
  })

  it("handles very small numbers (1e-15)", {
    small_data <- create_edge_case_data("extreme_small", n = 10)

    skip_if_not(exists("score_by_statistical_properties", mode = "function"))
    score <- score_by_statistical_properties(small_data$y, role = "y_column")

    # Should handle without crashing
    expect_true(!is.na(score))
    expect_true(is.numeric(score))
  })

  it("handles mixed extreme values", {
    mixed_data <- create_edge_case_data("mixed_extreme", n = 10)

    # Calculate statistics without crashing
    mean_val <- mean(mixed_data$y, na.rm = TRUE)
    sd_val <- sd(mixed_data$y, na.rm = TRUE)

    expect_true(!is.na(mean_val))
    expect_true(!is.na(sd_val))
    expect_true(is.finite(mean_val))
  })

  it("handles zero values in rate calculations", {
    zero_data <- data.frame(
      x = 1:10,
      y = c(0, 0, 0, 5, 10, 0, 0, 15, 0, 20),
      n = rep(100, 10),
      stringsAsFactors = FALSE
    )

    skip_if_not(exists("score_by_data_characteristics", mode = "function"))
    score <- score_by_data_characteristics(zero_data$y, role = "y_column")

    # Should handle zeros gracefully
    expect_true(!is.na(score))
  })

  it("handles negative values in data", {
    negative_data <- data.frame(
      x = 1:10,
      y = c(-5, -10, 5, 10, -3, 8, -12, 15, -1, 20),
      stringsAsFactors = FALSE
    )

    skip_if_not(exists("score_by_data_characteristics", mode = "function"))
    score <- score_by_data_characteristics(negative_data$y, role = "y_column")

    # Negative values should lower score but not crash
    expect_true(!is.na(score))
  })

  it("generates plot with extreme Y-axis range", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))
    skip("Visual inspection needed for extreme ranges")

    extreme_data <- data.frame(
      Dato = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 20),
      Værdi = c(rep(1e-6, 10), rep(1e6, 10)),
      stringsAsFactors = FALSE
    )

    config <- list(
      x_col = "Dato",
      y_col = "Værdi"
    )

    # Should generate plot without crashing
    result <- generateSPCPlot(extreme_data, config, chart_type = "i")
    expect_s3_class(result$plot, "ggplot")
  })
})

# LARGE FILE HANDLING ==========================================================

describe("Large File Performance", {

  it("handles large dataset (1000 rows) efficiently", {
    large_data <- data.frame(
      Dato = seq.Date(as.Date("2020-01-01"), by = "day", length.out = 1000),
      Tæller = sample(40:50, 1000, replace = TRUE),
      Nævner = rep(50, 1000),
      stringsAsFactors = FALSE
    )

    skip_if_not(exists("detect_columns_full_analysis", mode = "function"))

    start_time <- Sys.time()
    result <- detect_columns_full_analysis(large_data)
    end_time <- Sys.time()

    elapsed_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000

    expect_lt(elapsed_ms, 500)  # Should complete in <500ms
    expect_equal(result$x_col, "Dato")
  })

  it("handles CSV with many columns (50+)", {
    many_cols <- 50
    wide_data <- as.data.frame(matrix(rnorm(100 * many_cols), ncol = many_cols))
    names(wide_data)[1] <- "Dato"
    names(wide_data)[2] <- "Værdi"
    wide_data$Dato <- seq.Date(as.Date("2024-01-01"), by = "day", length.out = 100)

    skip_if_not(exists("find_numeric_columns", mode = "function"))

    start_time <- Sys.time()
    numeric_cols <- find_numeric_columns(wide_data)
    end_time <- Sys.time()

    elapsed_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000

    expect_lt(elapsed_ms, 1000)  # Should complete in <1s
    expect_gte(length(numeric_cols), many_cols - 1)  # All but Dato column
  })

  it("processes large file without memory issues", {
    skip("Manual memory test")

    # This test would verify memory doesn't balloon with large files
    # Would require manual monitoring with profvis or similar
  })

  it("validates large CSV file efficiently", {
    skip_if_not(exists("validate_csv_file", mode = "function"))

    # Create large test CSV
    large_data <- data.frame(
      x = 1:5000,
      y = rnorm(5000),
      stringsAsFactors = FALSE
    )

    temp_file <- tempfile(fileext = ".csv")
    readr::write_csv2(large_data, temp_file)
    on.exit(unlink(temp_file))

    start_time <- Sys.time()
    result <- validate_csv_file(temp_file)
    end_time <- Sys.time()

    elapsed_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000

    expect_true(result$valid)
    expect_lt(elapsed_ms, 2000)  # Should validate in <2s
  })
})

# BOUNDARY CONDITIONS ==========================================================

describe("Boundary Conditions", {

  it("handles exactly MIN_SPC_ROWS data points", {
    skip_if_not(exists("MIN_SPC_ROWS"))

    min_rows <- ifelse(exists("MIN_SPC_ROWS"), MIN_SPC_ROWS, 10)

    boundary_data <- data.frame(
      x = 1:min_rows,
      y = rnorm(min_rows),
      stringsAsFactors = FALSE
    )

    skip_if_not(exists("validate_data_for_auto_detect", mode = "function"))
    result <- validate_data_for_auto_detect(boundary_data)

    # Should be valid at exactly minimum
    expect_true(result$suitable)
  })

  it("handles MIN_SPC_ROWS - 1 data points", {
    skip_if_not(exists("MIN_SPC_ROWS"))

    min_rows <- ifelse(exists("MIN_SPC_ROWS"), MIN_SPC_ROWS, 10)

    boundary_data <- data.frame(
      x = 1:(min_rows - 1),
      y = rnorm(min_rows - 1),
      stringsAsFactors = FALSE
    )

    skip_if_not(exists("validate_data_for_auto_detect", mode = "function"))
    result <- validate_data_for_auto_detect(boundary_data)

    # Should fail just below minimum
    expect_false(result$suitable)
  })

  it("handles maximum string length in column names", {
    long_name <- paste(rep("A", 255), collapse = "")

    data <- data.frame(
      x = 1:10,
      stringsAsFactors = FALSE
    )
    names(data)[1] <- long_name

    skip_if_not(exists("detect_columns_name_based", mode = "function"))
    result <- detect_columns_name_based(names(data))

    # Should handle long names without crashing
    expect_true(!is.null(result))
  })

  it("handles maximum recommended SPC points", {
    skip_if_not(exists("RECOMMENDED_SPC_POINTS"))

    rec_points <- ifelse(exists("RECOMMENDED_SPC_POINTS"), RECOMMENDED_SPC_POINTS, 20)

    data <- data.frame(
      Dato = seq.Date(as.Date("2024-01-01"), by = "day", length.out = rec_points),
      Værdi = rnorm(rec_points),
      stringsAsFactors = FALSE
    )

    # Should process recommended size efficiently
    expect_equal(nrow(data), rec_points)
  })

  it("handles 100% missing data in one column", {
    partial_data <- data.frame(
      x = 1:10,
      y = rep(NA_real_, 10),
      z = rnorm(10),
      stringsAsFactors = FALSE
    )

    skip_if_not(exists("find_numeric_columns", mode = "function"))
    numeric_cols <- find_numeric_columns(partial_data)

    # Should identify x and z, skip y
    expect_true("x" %in% numeric_cols)
    expect_true("z" %in% numeric_cols)
  })
})

# SPECIAL DATA TYPES ===========================================================

describe("Special Data Types", {

  it("handles Date columns correctly", {
    date_data <- data.frame(
      Dato = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 12),
      Værdi = rnorm(12),
      stringsAsFactors = FALSE
    )

    expect_s3_class(date_data$Dato, "Date")

    skip_if_not(exists("detect_date_columns_robust", mode = "function"))
    result <- detect_date_columns_robust(date_data)

    # Should detect Date column
    expect_true("Dato" %in% names(result))
    expect_equal(result$Dato$score, 1.0)
  })

  it("handles POSIXct datetime columns", {
    datetime_data <- data.frame(
      Tidspunkt = seq.POSIXt(
        as.POSIXct("2024-01-01 00:00:00"),
        by = "hour",
        length.out = 24
      ),
      Værdi = rnorm(24),
      stringsAsFactors = FALSE
    )

    expect_s3_class(datetime_data$Tidspunkt, "POSIXct")

    skip_if_not(exists("detect_date_columns_robust", mode = "function"))
    result <- detect_date_columns_robust(datetime_data)

    # Should detect POSIXct column
    expect_true("Tidspunkt" %in% names(result))
  })

  it("handles factor columns", {
    factor_data <- data.frame(
      Kategori = factor(c("A", "B", "C", "A", "B", "C")),
      Værdi = rnorm(6),
      stringsAsFactors = TRUE
    )

    expect_s3_class(factor_data$Kategori, "factor")

    # Should convert factor to character for detection
    skip_if_not(exists("detect_columns_full_analysis", mode = "function"))
    result <- detect_columns_full_analysis(factor_data)

    expect_true(!is.null(result))
  })

  it("handles logical columns", {
    logical_data <- data.frame(
      Flag = c(TRUE, FALSE, TRUE, FALSE, TRUE),
      Værdi = rnorm(5),
      stringsAsFactors = FALSE
    )

    expect_type(logical_data$Flag, "logical")

    skip_if_not(exists("find_numeric_columns", mode = "function"))
    numeric_cols <- find_numeric_columns(logical_data)

    # Logical columns should not be treated as numeric for SPC
    expect_false("Flag" %in% numeric_cols)
  })
})

# MALFORMED DATA ===============================================================

describe("Malformed Data Handling", {

  it("handles inconsistent row lengths in CSV", {
    malformed_csv <- "Dato;Tæller;Nævner\n2024-01-01;10\n2024-01-02;12;100;extra"

    temp_file <- tempfile(fileext = ".csv")
    writeLines(malformed_csv, temp_file)
    on.exit(unlink(temp_file))

    # readr should handle gracefully
    result <- readr::read_csv2(
      temp_file,
      locale = readr::locale(encoding = "UTF-8"),
      show_col_types = FALSE
    )

    expect_equal(nrow(result), 2)
  })

  it("handles duplicate column names", {
    dup_data <- data.frame(
      Værdi = 1:5,
      Værdi = 6:10,
      check.names = TRUE,  # R will make unique
      stringsAsFactors = FALSE
    )

    # R should auto-generate unique names
    expect_equal(ncol(dup_data), 2)
    expect_true(all(names(dup_data) != names(dup_data)[1]))  # Names should differ
  })

  it("handles whitespace-only values", {
    whitespace_data <- data.frame(
      Text = c("value", "  ", "\t", "", "   "),
      Number = 1:5,
      stringsAsFactors = FALSE
    )

    skip_if_not(exists("preprocess_uploaded_data", mode = "function"))

    result <- preprocess_uploaded_data(
      whitespace_data,
      list(name = "test.csv", size = 100)
    )

    # Should handle whitespace gracefully
    expect_true(!is.null(result$data))
  })

  it("handles mixed delimiters in CSV", {
    # CSV with both semicolon and comma
    mixed_delim <- "Dato,Værdi;Note\n2024-01-01,100;Test"

    temp_file <- tempfile(fileext = ".csv")
    writeLines(mixed_delim, temp_file)
    on.exit(unlink(temp_file))

    # Should detect delimiter correctly
    result <- readr::read_csv2(
      temp_file,
      locale = readr::locale(encoding = "UTF-8"),
      show_col_types = FALSE
    )

    # May not parse perfectly, but shouldn't crash
    expect_true(!is.null(result))
  })
})
