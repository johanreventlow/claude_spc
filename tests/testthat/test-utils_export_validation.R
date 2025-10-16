# ==============================================================================
# TEST: UTILS_EXPORT_VALIDATION.R
# ==============================================================================
# FORMÅL: Test input validation og sanitization for export funktioner.
#         Sikrer XSS protection, character limits og aspect ratio validation.
#
# TEST COVERAGE:
#   - validate_export_inputs() - Complete input validation
#   - sanitize_user_input() - XSS protection og character filtering
#   - validate_aspect_ratio() - Aspect ratio warnings/errors
#
# EDGE CASES:
#   - NULL values, empty strings, very long strings
#   - Extreme aspect ratios (< 0.5, > 2.0)
#   - Invalid PNG dimensions (< 400, > 5000)
#   - XSS attack vectors (HTML tags, JavaScript)
# ==============================================================================

library(testthat)

# SETUP ========================================================================

# Mock constants hvis de ikke er tilgængelige
if (!exists("EXPORT_TITLE_MAX_LENGTH")) {
  EXPORT_TITLE_MAX_LENGTH <- 200
}
if (!exists("EXPORT_DESCRIPTION_MAX_LENGTH")) {
  EXPORT_DESCRIPTION_MAX_LENGTH <- 2000
}
if (!exists("EXPORT_DEPARTMENT_MAX_LENGTH")) {
  EXPORT_DEPARTMENT_MAX_LENGTH <- 100
}
if (!exists("EXPORT_ASPECT_RATIO_MIN")) {
  EXPORT_ASPECT_RATIO_MIN <- 0.5
}
if (!exists("EXPORT_ASPECT_RATIO_MAX")) {
  EXPORT_ASPECT_RATIO_MAX <- 2.0
}

# TEST: sanitize_user_input() ==================================================

test_that("sanitize_user_input removes XSS attack vectors", {
  # HTML script tags skal fjernes eller escapes
  xss_input <- "<script>alert('XSS')</script>Test"
  result <- sanitize_user_input(xss_input, html_escape = TRUE)

  expect_false(grepl("<script>", result, fixed = TRUE))
  expect_false(grepl("</script>", result, fixed = TRUE))
})

test_that("sanitize_user_input handles JavaScript protocols", {
  xss_input <- "javascript:void(0)"
  result <- sanitize_user_input(xss_input, html_escape = TRUE)

  expect_false(grepl("javascript:", result, fixed = TRUE))
})

test_that("sanitize_user_input preserves Danish characters", {
  danish_input <- "København Sygehus Øst - Afdeling Æ"
  result <- sanitize_user_input(danish_input)

  expect_true(grepl("ø", result))
  expect_true(grepl("Ø", result))
  expect_true(grepl("Æ", result))
})

test_that("sanitize_user_input removes special characters", {
  special_input <- "Test@#$%^&*()!+="
  result <- sanitize_user_input(special_input)

  # Special characters should be removed
  expect_false(grepl("@", result, fixed = TRUE))
  expect_false(grepl("#", result, fixed = TRUE))
  expect_false(grepl("$", result, fixed = TRUE))
  expect_true(grepl("Test", result))
})

test_that("sanitize_user_input allows safe characters", {
  safe_input <- "SPC Graf 2024: Resultat - Test 1.5"
  result <- sanitize_user_input(safe_input)

  expect_true(grepl("SPC", result))
  expect_true(grepl("Graf", result))
  expect_true(grepl("2024", result))
  # Tjek at basale tegn er bevaret
  expect_match(result, "Resultat")
})

test_that("sanitize_user_input respects max_length parameter", {
  long_input <- paste(rep("A", 250), collapse = "")
  result <- sanitize_user_input(long_input, max_length = 200)

  expect_equal(nchar(result), 200)
})

test_that("sanitize_user_input handles NULL input", {
  result <- sanitize_user_input(NULL)

  expect_equal(result, "")
})

test_that("sanitize_user_input handles empty string", {
  result <- sanitize_user_input("")

  expect_equal(result, "")
})

test_that("sanitize_user_input trims whitespace", {
  input_with_whitespace <- "  Test String  "
  result <- sanitize_user_input(input_with_whitespace)

  expect_false(grepl("^\\s", result))
  expect_false(grepl("\\s$", result))
})

# TEST: validate_aspect_ratio() ================================================

test_that("validate_aspect_ratio accepts normal ratios", {
  # Normal aspect ratios (0.5 - 2.0)
  expect_true(validate_aspect_ratio(1200, 900))  # 1.33
  expect_true(validate_aspect_ratio(1920, 1080)) # 1.78
  expect_true(validate_aspect_ratio(800, 800))   # 1.0
})

test_that("validate_aspect_ratio warns on extreme ratios", {
  # Too narrow (< 0.5)
  expect_warning(
    validate_aspect_ratio(400, 1000, warn_only = TRUE),
    "Aspect ratio"
  )

  # Too wide (> 2.0)
  expect_warning(
    validate_aspect_ratio(2000, 800, warn_only = TRUE),
    "Aspect ratio"
  )
})

test_that("validate_aspect_ratio can error on extreme ratios", {
  # Error mode instead of warning
  expect_error(
    validate_aspect_ratio(400, 1000, warn_only = FALSE),
    "Aspect ratio"
  )

  expect_error(
    validate_aspect_ratio(2000, 800, warn_only = FALSE),
    "Aspect ratio"
  )
})

test_that("validate_aspect_ratio accepts boundary values", {
  # Exactly at boundaries should pass without warning
  expect_silent(validate_aspect_ratio(1000, 2000)) # 0.5
  expect_silent(validate_aspect_ratio(2000, 1000)) # 2.0
})

# TEST: validate_export_inputs() ===============================================

test_that("validate_export_inputs accepts valid inputs", {
  expect_true(
    validate_export_inputs(
      format = "pdf",
      title = "Valid Title",
      department = "Valid Department"
    )
  )
})

test_that("validate_export_inputs rejects overly long title", {
  long_title <- paste(rep("A", 201), collapse = "")

  expect_error(
    validate_export_inputs(format = "pdf", title = long_title),
    "Titel må max være"
  )
})

test_that("validate_export_inputs rejects overly long description", {
  long_description <- paste(rep("A", 2001), collapse = "")

  expect_error(
    validate_export_inputs(
      format = "pdf",
      title = "Valid",
      description = long_description
    ),
    "Beskrivelse må max være"
  )
})

test_that("validate_export_inputs rejects overly long department", {
  long_dept <- paste(rep("A", 101), collapse = "")

  expect_error(
    validate_export_inputs(format = "pdf", department = long_dept),
    "Afdeling må max være"
  )
})

test_that("validate_export_inputs validates PNG dimensions", {
  # Width too small
  expect_error(
    validate_export_inputs(format = "png", width = 300, height = 600),
    "Bredde skal være mellem"
  )

  # Width too large
  expect_error(
    validate_export_inputs(format = "png", width = 6000, height = 600),
    "Bredde skal være mellem"
  )

  # Height too small
  expect_error(
    validate_export_inputs(format = "png", width = 800, height = 200),
    "Højde skal være mellem"
  )

  # Height too large
  expect_error(
    validate_export_inputs(format = "png", width = 800, height = 6000),
    "Højde skal være mellem"
  )
})

test_that("validate_export_inputs accepts valid PNG dimensions", {
  expect_true(
    validate_export_inputs(
      format = "png",
      width = 1200,
      height = 900,
      title = "Valid"
    )
  )
})

test_that("validate_export_inputs warns on extreme aspect ratios for PNG", {
  # Extreme aspect ratio should trigger warning/error
  expect_error(
    validate_export_inputs(format = "png", width = 2000, height = 600),
    "aspekt-ratio"
  )
})

test_that("validate_export_inputs handles NULL dimensions gracefully", {
  # NULL dimensions should be acceptable for PDF
  expect_true(
    validate_export_inputs(
      format = "pdf",
      title = "Valid",
      width = NULL,
      height = NULL
    )
  )
})

test_that("validate_export_inputs handles empty strings", {
  # Empty strings should be acceptable (optional fields)
  expect_true(
    validate_export_inputs(
      format = "pdf",
      title = "",
      department = ""
    )
  )
})

test_that("validate_export_inputs combines multiple errors", {
  long_title <- paste(rep("A", 201), collapse = "")
  long_dept <- paste(rep("B", 101), collapse = "")

  expect_error(
    validate_export_inputs(
      format = "pdf",
      title = long_title,
      department = long_dept
    ),
    "Titel.*Afdeling"
  )
})

test_that("validate_export_inputs does not validate dimensions for PDF", {
  # PDF format should not check dimensions
  expect_true(
    validate_export_inputs(
      format = "pdf",
      title = "Valid",
      width = 100,  # Would be invalid for PNG
      height = 10000
    )
  )
})

# EDGE CASES ===================================================================

test_that("sanitize_user_input handles very long XSS vectors", {
  xss_long <- paste(rep("<script>alert('X')</script>", 50), collapse = "")
  result <- sanitize_user_input(xss_long, max_length = 100)

  expect_lte(nchar(result), 100)
  expect_false(grepl("<script>", result, fixed = TRUE))
})

test_that("validate_aspect_ratio handles zero height", {
  # Should error because division by zero
  expect_error(
    validate_aspect_ratio(1000, 0)
  )
})

test_that("validate_aspect_ratio handles negative dimensions", {
  # Should handle gracefully or error
  expect_error(
    validate_aspect_ratio(-1000, 800)
  )
})

test_that("sanitize_user_input handles numeric input", {
  # Should convert to character
  result <- sanitize_user_input(12345)

  expect_type(result, "character")
  expect_equal(result, "12345")
})

test_that("validate_export_inputs handles case-insensitive format", {
  expect_true(
    validate_export_inputs(
      format = "PDF",  # Uppercase
      title = "Valid"
    )
  )

  expect_true(
    validate_export_inputs(
      format = "PnG",  # Mixed case
      width = 800,
      height = 600
    )
  )
})
