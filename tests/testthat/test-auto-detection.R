# test-auto-detection.R
# Test af auto-detection logik og data processing
# Fokuserer på testable utility functions og data transformations

test_that("Auto-detection kan identificere dato kolonner", {
  # SETUP: Test data med forskellige dato formater
  test_data <- data.frame(
    ID = 1:5,
    Dato = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01")),
    DanskDato = c("01-01-2024", "01-02-2024", "01-03-2024", "01-04-2024", "01-05-2024"),
    Tæller = c(90, 85, 92, 88, 94),
    Nævner = c(100, 95, 100, 98, 102),
    stringsAsFactors = FALSE
  )

  # TEST: Identificer kolonner som kan være datoer
  col_names <- names(test_data)
  date_candidates <- character(0)

  for (col_name in col_names) {
    col_data <- test_data[[col_name]]

    # Test for existing Date objects
    if (inherits(col_data, c("Date", "POSIXct", "POSIXt"))) {
      date_candidates <- c(date_candidates, col_name)
    }

    # Test for date-like names
    if (grepl("dato|date|tid|time", col_name, ignore.case = TRUE)) {
      date_candidates <- c(date_candidates, col_name)
    }
  }

  # FORVENT: Begge dato-kolonner blev identificeret
  expect_true("Dato" %in% date_candidates)
  expect_true("DanskDato" %in% date_candidates)
  expect_false("ID" %in% date_candidates)
  expect_false("Tæller" %in% date_candidates)
})

test_that("Auto-detection kan identificere numeriske kolonner", {
  # SETUP: Test data med forskellig numeriske typer
  test_data <- data.frame(
    Navn = c("A", "B", "C"),
    Tæller = c(90, 85, 92),
    Nævner = c(100, 95, 100),
    Rate = c(0.9, 0.89, 0.92),
    DanskTal = c("90,5", "85,2", "92,1"), # Danske decimal komma
    stringsAsFactors = FALSE
  )

  # TEST: Identificer numeriske kolonner
  numeric_cols <- character(0)
  for (col_name in names(test_data)) {
    col_data <- test_data[[col_name]]
    if (is.numeric(col_data)) {
      numeric_cols <- c(numeric_cols, col_name)
    }
  }

  # FORVENT: Numeriske kolonner blev fundet
  expected_numeric <- c("Tæller", "Nævner", "Rate")
  for (col in expected_numeric) {
    expect_true(col %in% numeric_cols,
                info = paste("Expected", col, "to be identified as numeric"))
  }
  expect_false("Navn" %in% numeric_cols)
  expect_false("DanskTal" %in% numeric_cols) # String med komma tal
})

test_that("Kolonne detektion baseret på navne fungerer", {
  # TEST: Navn-baseret detektion logik

  # Helper function to detect column purpose based on name
  detect_column_purpose <- function(col_name) {
    col_lower <- tolower(col_name)

    if (grepl("dato|date|tid|time", col_lower)) return("date")
    if (grepl("t.ller|tael|num|count", col_lower)) return("numerator")
    if (grepl("n.vner|naev|denom|total", col_lower)) return("denominator")
    if (grepl("kommentar|comment|note", col_lower)) return("comment")
    if (grepl("skift|phase|change", col_lower)) return("phase")
    if (grepl("frys|freeze", col_lower)) return("freeze")

    return("unknown")
  }

  # TEST: Danske kolonne navne
  test_cases <- list(
    list(name = "Dato", expected = "date"),
    list(name = "dato_start", expected = "date"),
    list(name = "Tæller", expected = "numerator"),
    list(name = "taeller_col", expected = "numerator"),
    list(name = "Nævner", expected = "denominator"),
    list(name = "naevner_total", expected = "denominator"),
    list(name = "Kommentar", expected = "comment"),
    list(name = "comment_text", expected = "comment"),
    list(name = "Skift", expected = "phase"),
    list(name = "phase_change", expected = "phase"),
    list(name = "Frys", expected = "freeze"),
    list(name = "freeze_flag", expected = "freeze"),
    list(name = "ID", expected = "unknown"),
    list(name = "random_col", expected = "unknown")
  )

  for (test_case in test_cases) {
    result <- detect_column_purpose(test_case$name)
    expect_equal(result, test_case$expected,
                info = paste("Column", test_case$name, "should be detected as", test_case$expected))
  }
})

test_that("Data validering og tom kolonne detektion", {
  # SETUP: Test data med tomme og NA værdier
  test_data <- data.frame(
    ValidCol = c(1, 2, 3, 4, 5),
    EmptyCol = rep(NA, 5),
    PartialCol = c(1, NA, 3, NA, 5),
    ZeroCol = rep(0, 5),
    TextCol = c("A", "B", "", "D", "E"),
    stringsAsFactors = FALSE
  )

  # Helper function to check if column is effectively empty
  is_effectively_empty <- function(col_data) {
    all(is.na(col_data)) || length(col_data) == 0
  }

  # Helper function to check if column has sufficient data
  has_sufficient_data <- function(col_data, min_ratio = 0.5) {
    non_na_count <- sum(!is.na(col_data))
    total_count <- length(col_data)
    (non_na_count / total_count) >= min_ratio
  }

  # TEST: Tom kolonne detektion
  expect_false(is_effectively_empty(test_data$ValidCol))
  expect_true(is_effectively_empty(test_data$EmptyCol))
  expect_false(is_effectively_empty(test_data$PartialCol))
  expect_false(is_effectively_empty(test_data$ZeroCol)) # 0 er ikke NA
  expect_false(is_effectively_empty(test_data$TextCol))

  # TEST: Tilstrækkelig data detektion
  expect_true(has_sufficient_data(test_data$ValidCol))
  expect_false(has_sufficient_data(test_data$EmptyCol))
  expect_true(has_sufficient_data(test_data$PartialCol, min_ratio = 0.5))
  expect_false(has_sufficient_data(test_data$PartialCol, min_ratio = 0.8))
  expect_true(has_sufficient_data(test_data$ZeroCol))
})

test_that("UI sync data structure generation", {
  # TEST: Oprettelse af korrekt UI sync data struktur

  # Mock auto-detected columns
  detected_cols <- list(
    x_col = "Dato",
    taeller_col = "Tæller",
    naevner_col = "Nævner",
    skift_col = NULL,
    frys_col = NULL,
    kommentar_col = "Kommentar"
  )

  # Mock column choices
  all_cols <- c("Dato", "Tæller", "Nævner", "Kommentar", "Extra")
  col_choices <- setNames(c("", all_cols), c("Vælg kolonne...", all_cols))

  # Function to create UI sync data (from auto_detect_and_update_columns)
  create_ui_sync_data <- function(detected_cols, col_choices) {
    list(
      x_col = detected_cols$x_col,
      taeller_col = detected_cols$taeller_col,
      naevner_col = detected_cols$naevner_col,
      skift_col = detected_cols$skift_col,
      frys_col = detected_cols$frys_col,
      kommentar_col = detected_cols$kommentar_col,
      col_choices = col_choices,
      timestamp = Sys.time()
    )
  }

  sync_data <- create_ui_sync_data(detected_cols, col_choices)

  # TEST: Structure is correct
  expect_equal(sync_data$x_col, "Dato")
  expect_equal(sync_data$taeller_col, "Tæller")
  expect_equal(sync_data$naevner_col, "Nævner")
  expect_null(sync_data$skift_col)
  expect_null(sync_data$frys_col)
  expect_equal(sync_data$kommentar_col, "Kommentar")

  # TEST: Choices structure
  expect_equal(length(sync_data$col_choices), 6) # "" + 5 columns
  expect_equal(sync_data$col_choices[[1]], "")
  expect_equal(names(sync_data$col_choices)[1], "Vælg kolonne...")

  # TEST: Timestamp is recent
  expect_true(difftime(Sys.time(), sync_data$timestamp, units = "secs") < 1)
})

test_that("Auto-detection prioriterer korrekt", {
  # TEST: Auto-detection prioritering logik

  # SETUP: Multiple potential date columns
  test_data <- data.frame(
    ID = 1:3,
    parsed_date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
    date_string = c("2024-01-01", "2024-02-01", "2024-03-01"),
    named_dato = c("Jan 2024", "Feb 2024", "Mar 2024"),
    regular_col = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )

  # Function to score date columns (from auto_detect_and_update_columns logic)
  score_date_column <- function(col_name, col_data) {
    score <- 0

    # HØJESTE PRIORITET: Allerede parsede dato-objekter
    if (inherits(col_data, c("Date", "POSIXct", "POSIXt"))) {
      score <- score + 100
    }

    # MELLEMPRIORITY: Navn-baseret detektion
    if (grepl("dato|date|tid|time", col_name, ignore.case = TRUE)) {
      score <- score + 50
    }

    return(score)
  }

  # TEST: Score all columns
  scores <- list()
  for (col_name in names(test_data)) {
    scores[[col_name]] <- score_date_column(col_name, test_data[[col_name]])
  }

  # FORVENT: Korrekt prioritering
  expect_equal(scores$parsed_date, 150)  # Highest: Date object (100) + name match (50)
  expect_equal(scores$date_string, 50)   # Name match ("date" in name), not Date object
  expect_equal(scores$named_dato, 50)    # Name match but not Date object
  expect_equal(scores$regular_col, 0)    # Neither
  expect_equal(scores$ID, 0)             # Neither

  # Find highest scoring column
  best_col <- names(scores)[which.max(unlist(scores))]
  expect_equal(best_col, "parsed_date")
})

test_that("Fallback logik fungerer korrekt", {
  # TEST: Fallback til første kolonne når ingen dato findes

  # SETUP: Data uden tydelige dato kolonner
  test_data <- data.frame(
    Col1 = c("A", "B", "C"),
    Col2 = c(1, 2, 3),
    Col3 = c(10, 20, 30),
    stringsAsFactors = FALSE
  )

  # Function to find best x column with fallback
  find_x_column <- function(data, date_candidates = NULL) {
    if (length(date_candidates) > 0) {
      return(date_candidates[1]) # Best date candidate
    }

    # Fallback til første kolonne
    col_names <- names(data)
    if (length(col_names) > 0) {
      return(col_names[1])
    }

    return(NULL)
  }

  # TEST: No date candidates - should fallback
  result <- find_x_column(test_data, date_candidates = NULL)
  expect_equal(result, "Col1")

  # TEST: Empty date candidates - should fallback
  result <- find_x_column(test_data, date_candidates = character(0))
  expect_equal(result, "Col1")

  # TEST: With date candidates - should use candidate
  result <- find_x_column(test_data, date_candidates = c("Col2", "Col3"))
  expect_equal(result, "Col2")

  # TEST: Empty data - should return NULL
  empty_data <- data.frame()
  result <- find_x_column(empty_data, date_candidates = NULL)
  expect_null(result)
})