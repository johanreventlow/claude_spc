# test-autodetect-engine.R
# ==============================================================================
# COMPREHENSIVE TEST SUITE: Auto-Detection Engine (fct_autodetect_unified.R)
# ==============================================================================
#
# FORMÅL: >70% coverage af autodetect system
# FOKUS: Column detection heuristics, Danish locale, frozen state, ambiguous data
#
# STRUKTUR:
#   1. Column Detection Heuristics (name-based + data-driven)
#   2. Danish Locale Handling (month names, date formats)
#   3. Frozen State Management (trigger types, unfreeze logic)
#   4. Ambiguous Data Handling (edge cases, conflicts)
#   5. Performance & Caching
#
# SUCCESS CRITERIA:
#   - Detects standard Danish column names correctly
#   - Handles Danish date formats and month names
#   - Respects frozen_until_next_trigger state
#   - Resolves ambiguous data intelligently
#   - Performance <100ms for typical datasets
# ==============================================================================

library(shiny)
library(testthat)

# SETUP HELPERS ================================================================

# Helper til at oprette minimal app_state for autodetect testing
create_autodetect_app_state <- function() {
  app_state <- new.env(parent = emptyenv())

  app_state$columns <- reactiveValues(
    auto_detect = reactiveValues(
      in_progress = FALSE,
      completed = FALSE,
      results = NULL,
      frozen_until_next_trigger = FALSE,
      last_run = NULL,
      last_trigger_type = NULL
    ),
    mappings = reactiveValues(
      x_column = NULL,
      y_column = NULL,
      n_column = NULL,
      skift_column = NULL,
      frys_column = NULL,
      kommentar_column = NULL
    )
  )

  return(app_state)
}

# Helper til at oprette mock emit API for autodetect
create_autodetect_emit <- function(app_state) {
  list(
    auto_detection_completed = function() {
      app_state$auto_detection_completed_called <- TRUE
    },
    visualization_update_needed = function() {
      app_state$visualization_update_needed_called <- TRUE
    }
  )
}

# COLUMN DETECTION HEURISTICS ==================================================

describe("Name-Based Column Detection", {

  it("detects standard Danish column names", {
    col_names <- c("Dato", "Tæller", "Nævner")
    app_state <- create_autodetect_app_state()

    results <- detect_columns_name_based(col_names, app_state)

    expect_equal(results$x_col, "Dato")
    expect_equal(results$y_col, "Tæller")
    expect_equal(results$n_col, "Nævner")
  })

  it("detects English column names", {
    col_names <- c("Date", "Count", "Total")
    app_state <- create_autodetect_app_state()

    results <- detect_columns_name_based(col_names, app_state)

    expect_equal(results$x_col, "Date")
    expect_equal(results$y_col, "Count")
    expect_equal(results$n_col, "Total")
  })

  it("detects case-insensitive column names", {
    col_names <- c("DATO", "tæller", "NæVnEr")
    app_state <- create_autodetect_app_state()

    results <- detect_columns_name_based(col_names, app_state)

    expect_equal(results$x_col, "DATO")
    expect_equal(results$y_col, "tæller")
    expect_equal(results$n_col, "NæVnEr")
  })

  it("handles missing column types gracefully", {
    col_names <- c("Dato", "Værdi")  # No denominator
    app_state <- create_autodetect_app_state()

    results <- detect_columns_name_based(col_names, app_state)

    expect_equal(results$x_col, "Dato")
    expect_equal(results$y_col, "Værdi")
    expect_null(results$n_col)
  })

  it("detects special columns (skift, frys, kommentar)", {
    col_names <- c("Dato", "Værdi", "Skift", "Frys", "Kommentar")
    app_state <- create_autodetect_app_state()

    results <- detect_columns_name_based(col_names, app_state)

    expect_equal(results$skift_col, "Skift")
    expect_equal(results$frys_col, "Frys")
    expect_equal(results$kommentar_col, "Kommentar")
  })

  it("handles empty column names", {
    col_names <- character(0)
    app_state <- create_autodetect_app_state()

    results <- detect_columns_name_based(col_names, app_state)

    expect_null(results$x_col)
    expect_null(results$y_col)
    expect_null(results$n_col)
  })
})

describe("Full Data Analysis Detection", {

  it("detects columns with data content", {
    data <- data.frame(
      Måned = c("jan 2024", "feb 2024", "mar 2024"),
      Antal = c(100, 105, 110),
      Total = c(200, 200, 200),
      stringsAsFactors = FALSE
    )
    app_state <- create_autodetect_app_state()

    results <- detect_columns_full_analysis(data, app_state)

    expect_equal(results$x_col, "Måned")
    expect_equal(results$y_col, "Antal")
    expect_equal(results$n_col, "Total")
  })

  it("handles empty data frame", {
    data <- data.frame()
    app_state <- create_autodetect_app_state()

    results <- detect_columns_full_analysis(data, app_state)

    expect_null(results$x_col)
    expect_null(results$y_col)
    expect_null(results$n_col)
  })

  it("handles single row data", {
    data <- data.frame(
      Dato = as.Date("2024-01-01"),
      Værdi = 100,
      stringsAsFactors = FALSE
    )
    app_state <- create_autodetect_app_state()

    results <- detect_columns_full_analysis(data, app_state)

    expect_equal(results$x_col, "Dato")
    expect_equal(results$y_col, "Værdi")
  })

  it("prefers data-driven detection over name-based", {
    # Column names suggest wrong assignments, but data content should override
    data <- data.frame(
      Text = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
      Value1 = c(10, 12, 11),
      Value2 = c(100, 100, 100),
      stringsAsFactors = FALSE
    )
    app_state <- create_autodetect_app_state()

    results <- detect_columns_full_analysis(data, app_state)

    # Should detect Date column based on data type, not name
    expect_equal(results$x_col, "Text")
  })
})

# DANISH LOCALE HANDLING =======================================================

describe("Danish Date Detection", {

  it("detects Danish month abbreviations", {
    data <- data.frame(
      Måned = c("jan 2024", "feb 2024", "mar 2024"),
      Værdi = c(100, 105, 110),
      stringsAsFactors = FALSE
    )

    date_candidates <- detect_date_columns_robust(data)

    expect_true("Måned" %in% names(date_candidates))
    expect_gte(date_candidates$Måned$score, 0.8)
  })

  it("detects Danish full month names", {
    data <- data.frame(
      Periode = c("1 januar 2024", "1 februar 2024", "1 marts 2024"),
      Værdi = c(100, 105, 110),
      stringsAsFactors = FALSE
    )

    date_candidates <- detect_date_columns_robust(data)

    expect_true("Periode" %in% names(date_candidates))
    expect_gte(date_candidates$Periode$score, 0.8)
  })

  it("handles Danish date formats (d-m-Y, d.m.Y, d/m/Y)", {
    data <- data.frame(
      Dato1 = c("01-01-2024", "02-01-2024", "03-01-2024"),
      Dato2 = c("01.01.2024", "02.01.2024", "03.01.2024"),
      Dato3 = c("01/01/2024", "02/01/2024", "03/01/2024"),
      Værdi = c(100, 105, 110),
      stringsAsFactors = FALSE
    )

    date_candidates <- detect_date_columns_robust(data)

    expect_true("Dato1" %in% names(date_candidates))
    expect_true("Dato2" %in% names(date_candidates))
    expect_true("Dato3" %in% names(date_candidates))
  })

  it("parses Danish dates correctly with parse_danish_dates", {
    date_strings <- c("15 jan 2024", "20 februar 2024", "1 marts 2024")

    parsed <- parse_danish_dates(date_strings, "dmy")

    expect_s3_class(parsed, "Date")
    expect_equal(length(parsed), 3)
    expect_false(any(is.na(parsed)))
  })

  it("handles mixed case Danish month names", {
    date_strings <- c("15 JAN 2024", "20 Februar 2024", "1 MARTS 2024")

    parsed <- parse_danish_dates(date_strings, "dmy")

    expect_s3_class(parsed, "Date")
    expect_false(any(is.na(parsed)))
  })
})

describe("Numeric Column Scoring", {

  it("scores columns by name patterns", {
    score_taeller <- score_by_name_patterns("Tæller", role = "y_column")
    score_naevner <- score_by_name_patterns("Nævner", role = "n_column")
    score_generic <- score_by_name_patterns("Column1", role = "y_column")

    expect_gte(score_taeller, 0.9)
    expect_gte(score_naevner, 0.9)
    expect_lte(score_generic, 0.1)
  })

  it("scores columns by data characteristics", {
    good_count_data <- c(10, 12, 15, 14, 11, 13)
    good_denom_data <- c(100, 100, 100, 100, 100, 100)

    count_score <- score_by_data_characteristics(good_count_data, role = "y_column")
    denom_score <- score_by_data_characteristics(good_denom_data, role = "n_column")

    expect_gte(count_score, 0.5)
    expect_gte(denom_score, 0.5)
  })

  it("scores columns by statistical properties", {
    variable_data <- c(10, 20, 15, 25, 18, 22)
    stable_data <- c(100, 101, 99, 100, 100, 101)

    var_score <- score_by_statistical_properties(variable_data, role = "y_column")
    stable_score <- score_by_statistical_properties(stable_data, role = "n_column")

    expect_gte(var_score, 0.3)
    expect_gte(stable_score, 0.3)
  })

  it("finds numeric columns including convertible character columns", {
    data <- data.frame(
      Text = c("a", "b", "c"),
      NumericChar = c("100", "105", "110"),
      Numeric = c(10, 12, 11),
      Date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
      stringsAsFactors = FALSE
    )

    numeric_cols <- find_numeric_columns(data)

    expect_true("Numeric" %in% numeric_cols)
    expect_true("NumericChar" %in% numeric_cols)
    expect_false("Text" %in% numeric_cols)
    expect_false("Date" %in% numeric_cols)
  })
})

# FROZEN STATE MANAGEMENT ======================================================

describe("Frozen State Logic", {

  it("respects frozen_until_next_trigger for same trigger type", {
    app_state <- create_autodetect_app_state()
    emit <- create_autodetect_emit(app_state)

    data <- data.frame(
      Dato = as.Date(c("2024-01-01", "2024-01-02")),
      Værdi = c(100, 105),
      stringsAsFactors = FALSE
    )

    # First call - should succeed
    result1 <- autodetect_engine(data, "file_upload", app_state, emit)

    # Verify frozen state set
    expect_true(isolate(app_state$columns$auto_detect$frozen_until_next_trigger))

    # Second call with same trigger - should be skipped
    result2 <- autodetect_engine(data, "file_upload", app_state, emit)

    expect_null(result2)
  })

  it("allows manual trigger to override frozen state", {
    app_state <- create_autodetect_app_state()
    emit <- create_autodetect_emit(app_state)

    data <- data.frame(
      Dato = as.Date(c("2024-01-01", "2024-01-02")),
      Værdi = c(100, 105),
      stringsAsFactors = FALSE
    )

    # First call
    autodetect_engine(data, "file_upload", app_state, emit)

    # Verify frozen
    expect_true(isolate(app_state$columns$auto_detect$frozen_until_next_trigger))

    # Manual trigger should override frozen state
    result_manual <- autodetect_engine(data, "manual", app_state, emit)

    expect_false(is.null(result_manual))
  })

  it("unfreezes automatically when new data arrives", {
    app_state <- create_autodetect_app_state()
    emit <- create_autodetect_emit(app_state)

    # Set frozen state manually
    isolate(app_state$columns$auto_detect$frozen_until_next_trigger <- TRUE)

    data <- data.frame(
      Dato = as.Date(c("2024-01-01", "2024-01-02")),
      Værdi = c(100, 105),
      stringsAsFactors = FALSE
    )

    # file_upload with data should unfreeze
    result <- autodetect_engine(data, "file_upload", app_state, emit)

    expect_false(is.null(result))
    expect_true(isolate(app_state$columns$auto_detect$frozen_until_next_trigger))  # Re-frozen after run
  })

  it("tracks last_run metadata correctly", {
    app_state <- create_autodetect_app_state()
    emit <- create_autodetect_emit(app_state)

    data <- data.frame(
      Dato = as.Date(c("2024-01-01", "2024-01-02")),
      Værdi = c(100, 105),
      stringsAsFactors = FALSE
    )

    autodetect_engine(data, "file_upload", app_state, emit)

    last_run <- isolate(app_state$columns$auto_detect$last_run)

    expect_false(is.null(last_run))
    expect_equal(last_run$trigger, "file_upload")
    expect_equal(last_run$data_rows, 2)
    expect_equal(last_run$data_cols, 2)
    expect_true(!is.null(last_run$timestamp))
  })
})

# AMBIGUOUS DATA HANDLING ======================================================

describe("Ambiguous Data Scenarios", {

  it("handles multiple date-like columns", {
    data <- data.frame(
      Dato = as.Date(c("2024-01-01", "2024-01-02")),
      Måned = c("jan 2024", "feb 2024"),
      Værdi = c(100, 105),
      stringsAsFactors = FALSE
    )

    date_candidates <- detect_date_columns_robust(data)

    # Should detect both, but prefer native Date class
    expect_true("Dato" %in% names(date_candidates))
    expect_equal(date_candidates$Dato$score, 1.0)
  })

  it("handles multiple numeric columns", {
    data <- data.frame(
      Dato = as.Date(c("2024-01-01", "2024-01-02")),
      Værdi1 = c(100, 105),
      Værdi2 = c(10, 12),
      Total = c(200, 200),
      stringsAsFactors = FALSE
    )
    app_state <- create_autodetect_app_state()

    results <- detect_columns_full_analysis(data, app_state)

    # Should pick best candidates
    expect_true(!is.null(results$y_col))
    expect_true(!is.null(results$n_col))
  })

  it("handles all-NA columns gracefully", {
    data <- data.frame(
      Dato = as.Date(c("2024-01-01", "2024-01-02")),
      NA_Col = c(NA, NA),
      Værdi = c(100, 105),
      stringsAsFactors = FALSE
    )
    app_state <- create_autodetect_app_state()

    results <- detect_columns_full_analysis(data, app_state)

    # Should ignore NA column and detect others
    expect_equal(results$x_col, "Dato")
    expect_equal(results$y_col, "Værdi")
  })

  it("handles character columns with partial numeric content", {
    data <- data.frame(
      Dato = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
      Mixed = c("100", "105", "not_a_number"),
      Værdi = c(10, 12, 11),
      stringsAsFactors = FALSE
    )

    numeric_cols <- find_numeric_columns(data)

    # Mixed column should not be detected as numeric (only 67% convertible)
    expect_false("Mixed" %in% numeric_cols)
    expect_true("Værdi" %in% numeric_cols)
  })

  it("resolves conflict when column name suggests one role but data suggests another", {
    # Column named "Dato" but contains numeric data
    data <- data.frame(
      Dato = c(1, 2, 3, 4, 5),
      Værdi = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-04", "2024-01-05")),
      Antal = c(100, 105, 110, 108, 112),
      stringsAsFactors = FALSE
    )
    app_state <- create_autodetect_app_state()

    results <- detect_columns_full_analysis(data, app_state)

    # Should prefer data-driven detection (Værdi is actual date)
    expect_equal(results$x_col, "Værdi")
  })
})

# STATE UPDATE & INTEGRATION ===================================================

describe("State Update Logic", {

  it("updates app_state column mappings correctly", {
    app_state <- create_autodetect_app_state()

    results <- list(
      x_col = "Dato",
      y_col = "Tæller",
      n_col = "Nævner",
      skift_col = NULL,
      frys_col = NULL,
      kommentar_col = "Kommentar"
    )

    update_all_column_mappings(results, app_state$columns, app_state)

    expect_equal(isolate(app_state$columns$mappings$x_column), "Dato")
    expect_equal(isolate(app_state$columns$mappings$y_column), "Tæller")
    expect_equal(isolate(app_state$columns$mappings$n_column), "Nævner")
    expect_equal(isolate(app_state$columns$mappings$kommentar_column), "Kommentar")
    expect_true(isolate(app_state$columns$auto_detect$completed))
  })

  it("emits auto_detection_completed event", {
    app_state <- create_autodetect_app_state()
    emit <- create_autodetect_emit(app_state)

    data <- data.frame(
      Dato = as.Date(c("2024-01-01", "2024-01-02")),
      Værdi = c(100, 105),
      stringsAsFactors = FALSE
    )

    autodetect_engine(data, "file_upload", app_state, emit)

    expect_true(app_state$auto_detection_completed_called)
  })

  it("handles NULL data for session_start scenario", {
    app_state <- create_autodetect_app_state()
    emit <- create_autodetect_emit(app_state)

    # Session start with no data
    result <- autodetect_engine(NULL, "session_start", app_state, emit)

    # Should complete without error
    expect_true(app_state$auto_detection_completed_called)
  })
})

# PERFORMANCE & CACHING ========================================================

describe("Performance & Caching", {

  it("completes in reasonable time for typical dataset", {
    app_state <- create_autodetect_app_state()
    emit <- create_autodetect_emit(app_state)

    # Typical SPC dataset: 12 months of data
    data <- data.frame(
      Dato = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 12),
      Tæller = sample(40:50, 12, replace = TRUE),
      Nævner = rep(50, 12),
      stringsAsFactors = FALSE
    )

    start_time <- Sys.time()
    autodetect_engine(data, "file_upload", app_state, emit)
    end_time <- Sys.time()

    elapsed_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000

    expect_lt(elapsed_ms, 100)  # Should complete in <100ms
  })

  it("handles large dataset efficiently", {
    skip("Performance test - run manually")

    app_state <- create_autodetect_app_state()
    emit <- create_autodetect_emit(app_state)

    # Large dataset: 1000 rows
    data <- data.frame(
      Dato = seq.Date(as.Date("2020-01-01"), by = "day", length.out = 1000),
      Tæller = sample(40:50, 1000, replace = TRUE),
      Nævner = rep(50, 1000),
      stringsAsFactors = FALSE
    )

    start_time <- Sys.time()
    autodetect_engine(data, "file_upload", app_state, emit)
    end_time <- Sys.time()

    elapsed_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000

    expect_lt(elapsed_ms, 500)  # Should complete in <500ms even for large data
  })
})

# EDGE CASES ===================================================================

describe("Edge Cases", {

  it("handles single-column data frame", {
    data <- data.frame(
      Værdi = c(100, 105, 110)
    )
    app_state <- create_autodetect_app_state()

    results <- detect_columns_full_analysis(data, app_state)

    expect_equal(results$y_col, "Værdi")
    expect_null(results$x_col)
    expect_null(results$n_col)
  })

  it("handles data with extreme values", {
    data <- data.frame(
      Dato = as.Date(c("2024-01-01", "2024-01-02")),
      Værdi = c(1e10, 1e-10),
      stringsAsFactors = FALSE
    )
    app_state <- create_autodetect_app_state()

    results <- detect_columns_full_analysis(data, app_state)

    # Should still detect columns despite extreme values
    expect_equal(results$x_col, "Dato")
    expect_equal(results$y_col, "Værdi")
  })

  it("handles column names with special characters", {
    data <- data.frame(
      `Dato (år-måned)` = as.Date(c("2024-01-01", "2024-01-02")),
      `Tæller/Nævner (%)` = c(50, 55),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    app_state <- create_autodetect_app_state()

    results <- detect_columns_full_analysis(data, app_state)

    # Should handle special characters in names
    expect_equal(results$x_col, "Dato (år-måned)")
    expect_equal(results$y_col, "Tæller/Nævner (%)")
  })

  it("handles duplicate column names", {
    # R automatically makes duplicate names unique with suffixes
    data <- data.frame(
      Værdi = c(100, 105, 110),
      Værdi = c(10, 12, 11),
      check.names = TRUE,
      stringsAsFactors = FALSE
    )
    app_state <- create_autodetect_app_state()

    # Should work with auto-generated unique names
    results <- detect_columns_full_analysis(data, app_state)

    expect_true(!is.null(results$y_col))
  })
})
