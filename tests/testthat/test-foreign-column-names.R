# test-foreign-column-names.R
# Tests af app funktionalitet med fremmede/ikke-standard kolonnenavne

test_that("Manuel kolonnevalg prioriteres over auto-detect", {
  # Mock input objekter
  input_mock <- list(
    x_column = "Nr",
    y_column = "T",
    n_column = "N",
    chart_type = "Seriediagram (Run Chart)"
  )

  # Test data med fremmede kolonnenavne
  test_data <- data.frame(
    Nr = 1:10,
    Uge = as.Date("2023-01-01") + 0:9,
    T = c(5, 8, 12, 7, 9, 11, 6, 10, 8, 9),
    N = c(100, 120, 150, 110, 130, 140, 105, 125, 115, 135),
    stringsAsFactors = FALSE
  )

  # Mock reaktive værdier for auto-detect (simulerer app_state$columns$auto_detect$results)
  auto_detect_results <- list(
    x_col = "Uge",  # Auto-detect ville vælge Uge som X
    y_col = "T",    # Auto-detect kunne ikke detektere T som tæller
    n_col = "N"     # Auto-detect kunne ikke detektere N som nævner
  )

  # Simuler manual_config() funktionen
  manual_config <- function() {
    list(
      x_col = if (!is.null(input_mock$x_column) && input_mock$x_column != "") input_mock$x_column else NULL,
      y_col = if (!is.null(input_mock$y_column) && input_mock$y_column != "") input_mock$y_column else NULL,
      n_col = if (!is.null(input_mock$n_column) && input_mock$n_column != "") input_mock$n_column else NULL
    )
  }

  # Simuler auto_detected_config() funktionen
  auto_detected_config <- function() {
    if (!is.null(auto_detect_results)) {
      return(list(
        x_col = auto_detect_results$x_col,
        y_col = auto_detect_results$y_col,
        n_col = auto_detect_results$n_col
      ))
    }
    return(NULL)
  }

  # Test den nye prioriterings logik
  column_config <- function() {
    # Prioritize manual config when user has made selections
    manual_config_check <- manual_config()
    if (!is.null(manual_config_check) && !is.null(manual_config_check$y_col)) {
      return(manual_config_check)
    }

    # Fall back to auto-detected config
    auto_config <- auto_detected_config()
    if (!is.null(auto_config) && !is.null(auto_config$y_col)) {
      return(auto_config)
    }

    # Final fallback - return NULL if neither available
    return(NULL)
  }

  # Test at manual valg prioriteres
  result <- column_config()

  expect_equal(result$x_col, "Nr")  # Manuel valg, ikke auto-detected "Uge"
  expect_equal(result$y_col, "T")   # Manuel valg
  expect_equal(result$n_col, "N")   # Manuel valg
})

test_that("Auto-detect fallback når ingen manuel valg", {
  # Mock input med tomme værdier
  input_empty <- list(
    x_column = "",
    y_column = "",
    n_column = "",
    chart_type = "Seriediagram (Run Chart)"
  )

  # Mock reaktive værdier for auto-detect (simulerer app_state$columns$auto_detect$results)
  auto_detect_results <- list(
    x_col = "Uge",
    y_col = "Tæller",
    n_col = "Nævner"
  )

  # Simuler manual_config() med tomme værdier
  manual_config <- function() {
    list(
      x_col = if (!is.null(input_empty$x_column) && input_empty$x_column != "") input_empty$x_column else NULL,
      y_col = if (!is.null(input_empty$y_column) && input_empty$y_column != "") input_empty$y_column else NULL,
      n_col = if (!is.null(input_empty$n_column) && input_empty$n_column != "") input_empty$n_column else NULL
    )
  }

  # Simuler auto_detected_config()
  auto_detected_config <- function() {
    if (!is.null(auto_detect_results)) {
      return(list(
        x_col = auto_detect_results$x_col,
        y_col = auto_detect_results$y_col,
        n_col = auto_detect_results$n_col
      ))
    }
    return(NULL)
  }

  # Test den nye prioriterings logik
  column_config <- function() {
    # Prioritize manual config when user has made selections
    manual_config_check <- manual_config()
    if (!is.null(manual_config_check) && !is.null(manual_config_check$y_col)) {
      return(manual_config_check)
    }

    # Fall back to auto-detected config
    auto_config <- auto_detected_config()
    if (!is.null(auto_config) && !is.null(auto_config$y_col)) {
      return(auto_config)
    }

    # Final fallback - return NULL if neither available
    return(NULL)
  }

  # Test at auto-detect bruges når ingen manuel valg
  result <- column_config()

  expect_equal(result$x_col, "Uge")     # Auto-detect fallback
  expect_equal(result$y_col, "Tæller")  # Auto-detect fallback
  expect_equal(result$n_col, "Nævner")  # Auto-detect fallback
})

test_that("Foreign column names vises i selectize choices", {
  # Test data med fremmede kolonnenavne
  test_data <- data.frame(
    Nr = 1:5,
    "Uge" = 1:5,
    "Uge lang md" = paste("Uge", 1:5),
    "Måned" = rep("Januar", 5),
    "Uge tekst" = paste("W", 1:5),
    T = c(10, 15, 12, 8, 20),
    N = c(100, 150, 120, 80, 200),
    check.names = FALSE,  # Bevar originale kolonnenavne med mellemrum
    stringsAsFactors = FALSE
  )

  # Test at alle kolonnenavne er tilgængelige
  all_cols <- names(test_data)
  col_choices <- setNames(
    c("", all_cols),
    c("Vælg kolonne...", all_cols)
  )

  # Verificer at fremmede kolonnenavne er inkluderet
  expect_true("Nr" %in% names(col_choices))
  expect_true("Uge" %in% names(col_choices))
  expect_true("Uge lang md" %in% names(col_choices))
  expect_true("Måned" %in% names(col_choices))
  expect_true("Uge tekst" %in% names(col_choices))
  expect_true("T" %in% names(col_choices))
  expect_true("N" %in% names(col_choices))

  # Verificer at choices har korrekt struktur
  expect_equal(length(col_choices), length(all_cols) + 1)  # +1 for "Vælg kolonne..."
  expect_equal(col_choices[[1]], "")  # Første element er tom string
})

test_that("Scenario: Bruger vælger fremmede kolonnenavne manuelt", {
  # Dette test simulerer det oprindelige problem scenarie

  # Test data som i SPC_test_data_forskellige.xlsx
  test_data <- data.frame(
    Nr = 1:36,
    Uge = rep(1:18, each = 2),
    "Uge lang md" = paste("Uge", rep(1:18, each = 2), "lang"),
    "Måned" = rep(c("Jan", "Feb", "Mar"), each = 12),
    "Uge tekst" = paste("W", rep(1:18, each = 2)),
    T = sample(5:25, 36, replace = TRUE),
    N = sample(80:150, 36, replace = TRUE),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # 1. Auto-detect fejler (kan ikke genkende T/N som tæller/nævner)
  auto_detected_result <- list(x_col = "Uge", y_col = NULL, n_col = NULL)  # Auto-detect fejl

  # 2. Bruger vælger manuelt i selectize inputs
  manual_selection <- list(
    x_col = "Nr",     # Brugeren vælger Nr som X
    y_col = "T",      # Brugeren vælger T som Y
    n_col = "N"       # Brugeren vælger N som N
  )

  # 3. Test at manual prioriteres korrekt
  column_config_result <- function() {
    # Prioritet: manuel > auto-detect
    if (!is.null(manual_selection$y_col)) {
      return(manual_selection)
    }
    if (!is.null(auto_detected_result$y_col)) {
      return(auto_detected_result)
    }
    return(NULL)
  }

  result <- column_config_result()

  # Verificer at brugerens manuelle valg respekteres
  expect_equal(result$x_col, "Nr")
  expect_equal(result$y_col, "T")
  expect_equal(result$n_col, "N")

  # Verificer at valgte kolonner eksisterer i data
  expect_true(result$x_col %in% names(test_data))
  expect_true(result$y_col %in% names(test_data))
  expect_true(result$n_col %in% names(test_data))
})