# test-data-validation.R
# Tests af data validering og hjælpefunktioner

test_that("ensure_standard_columns virker korrekt", {
  # Test data uden standard kolonner
  test_data <- data.frame(
    Dato = c("2024-01-01", "2024-02-01"),
    Tæller = c(10, 15),
    Nævner = c(100, 120)
  )
  
  # Kør funktionen
  result <- ensure_standard_columns(test_data)
  
  # Test at standard kolonner er tilføjet
  expect_true("Skift" %in% names(result))
  expect_true("Frys" %in% names(result))
  
  # Test at standard kolonner kommer først
  expect_equal(names(result)[1:2], c("Skift", "Frys"))
  
  # Test at original data bevares
  expect_true("Dato" %in% names(result))
  expect_true("Tæller" %in% names(result))
  expect_true("Nævner" %in% names(result))
})

test_that("validate_numeric_column fungerer", {
  test_data <- data.frame(
    numerisk = c(1, 2, 3),
    tekst = c("a", "b", "c")
  )
  
  # Test valid numerisk kolonne
  expect_null(validate_numeric_column(test_data, "numerisk"))
  
  # Test invalid (ikke-numerisk) kolonne
  result <- validate_numeric_column(test_data, "tekst")
  expect_true(grepl("skal være numerisk", result))
  
  # Test ikke-eksisterende kolonne
  result <- validate_numeric_column(test_data, "findes_ikke")
  expect_true(grepl("ikke fundet", result))
})

test_that("validate_date_column fungerer", {
  test_data <- data.frame(
    dato_valid = as.Date(c("2024-01-01", "2024-02-01")),
    dato_tekst = c("2024-01-01", "2024-02-01"),
    ikke_dato = c(1, 2)
  )
  
  # Test valid dato kolonne
  expect_null(validate_date_column(test_data, "dato_valid"))
  
  # Test konverterbar tekst dato
  expect_null(validate_date_column(test_data, "dato_tekst"))
  
  # Test ikke-eksisterende kolonne
  result <- validate_date_column(test_data, "findes_ikke")
  expect_true(grepl("ikke fundet", result))
})

test_that("safe_date_parse fungerer robust", {
  # Test valid danske datoer
  danske_datoer <- c("01-01-2024", "15-02-2024", "31-12-2023")
  result <- safe_date_parse(danske_datoer)
  
  expect_true(result$success)
  expect_gt(result$success_rate, 0.5)
  expect_equal(result$total_count, 3)
  
  # Test invalid datoer
  invalid_datoer <- c("ikke-en-dato", "abc", "32-13-2024")
  result <- safe_date_parse(invalid_datoer)
  
  expect_false(result$success)
  expect_equal(result$parsed_count, 0)
})

test_that("chart type mapping fungerer", {
  # Test danske navne til engelske koder
  expect_equal(get_qic_chart_type("Seriediagram med SPC (Run Chart)"), "run")
  expect_equal(get_qic_chart_type("P-kort (Andele)"), "p")
  expect_equal(get_qic_chart_type("I-kort (Individuelle værdier)"), "i")
  
  # Test allerede engelske koder
  expect_equal(get_qic_chart_type("run"), "run")
  expect_equal(get_qic_chart_type("p"), "p")
  
  # Test fallback
  expect_equal(get_qic_chart_type("ukendt_type"), "run")
  expect_equal(get_qic_chart_type(""), "run")
  expect_equal(get_qic_chart_type(NULL), "run")
})