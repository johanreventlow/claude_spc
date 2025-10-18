# ==============================================================================
# TEST: UTILS_EXPORT_FILENAME.R
# ==============================================================================
# FORMÅL: Test filename generation og sanitization for export funktioner.
#         Sikrer korrekt håndtering af danske karakterer og cross-platform
#         kompatibilitet.
#
# TEST COVERAGE:
#   - generate_export_filename() - Complete filename generation
#   - sanitize_filename() - Character filtering og spacing
#
# EDGE CASES:
#   - Danish characters (æøåÆØÅ) preservation
#   - Special characters removal (@#$%^&*()!)
#   - Multiple spaces → single underscore
#   - Leading/trailing underscores removed
#   - Very long strings
#   - Cross-platform path separators
# ==============================================================================

library(testthat)

# SETUP ========================================================================

# Mock constants hvis de ikke er tilgængelige
if (!exists("EXPORT_FILENAME_PREFIX")) {
  EXPORT_FILENAME_PREFIX <- "SPC"
}
if (!exists("EXPORT_FILENAME_SEPARATOR")) {
  EXPORT_FILENAME_SEPARATOR <- "_"
}

# TEST: sanitize_filename() ====================================================

test_that("sanitize_filename preserves Danish characters", {
  input <- "København Sygehus Øst"
  result <- sanitize_filename(input)

  expect_true(grepl("ø", result))
  expect_true(grepl("Ø", result))
  expect_true(grepl("København", result))
})

test_that("sanitize_filename handles all Danish characters", {
  input <- "æøåÆØÅ"
  result <- sanitize_filename(input)

  expect_equal(result, "æøåÆØÅ")
})

test_that("sanitize_filename removes special characters", {
  input <- "Test@#$%^&*()!+="
  result <- sanitize_filename(input)

  # Special characters should be removed
  expect_false(grepl("@", result, fixed = TRUE))
  expect_false(grepl("#", result, fixed = TRUE))
  expect_false(grepl("$", result, fixed = TRUE))
  expect_false(grepl("!", result, fixed = TRUE))
  expect_equal(result, "Test")
})

test_that("sanitize_filename replaces spaces with underscores", {
  input <- "Test File Name"
  result <- sanitize_filename(input)

  expect_equal(result, "Test_File_Name")
})

test_that("sanitize_filename collapses multiple spaces", {
  input <- "Test    Multiple    Spaces"
  result <- sanitize_filename(input)

  # Multiple spaces should become single underscore
  expect_false(grepl("__", result, fixed = TRUE))
  expect_equal(result, "Test_Multiple_Spaces")
})

test_that("sanitize_filename removes leading underscores", {
  input <- "___Leading"
  result <- sanitize_filename(input)

  expect_false(grepl("^_", result))
  expect_equal(result, "Leading")
})

test_that("sanitize_filename removes trailing underscores", {
  input <- "Trailing___"
  result <- sanitize_filename(input)

  expect_false(grepl("_$", result))
  expect_equal(result, "Trailing")
})

test_that("sanitize_filename removes leading and trailing whitespace", {
  input <- "   Trimmed   "
  result <- sanitize_filename(input)

  expect_equal(result, "Trimmed")
})

test_that("sanitize_filename handles empty string", {
  result <- sanitize_filename("")

  expect_equal(result, "")
})

test_that("sanitize_filename handles NULL input", {
  # Should handle gracefully
  expect_error(sanitize_filename(NULL), NA)
})

test_that("sanitize_filename preserves alphanumeric characters", {
  input <- "Test123ABC"
  result <- sanitize_filename(input)

  expect_equal(result, "Test123ABC")
})

test_that("sanitize_filename preserves hyphens", {
  input <- "Test-Name-With-Hyphens"
  result <- sanitize_filename(input)

  expect_true(grepl("-", result, fixed = TRUE))
  expect_equal(result, "Test-Name-With-Hyphens")
})

test_that("sanitize_filename handles mixed valid and invalid characters", {
  input <- "Valid-Name_123 æøå @#$%"
  result <- sanitize_filename(input)

  expect_true(grepl("Valid-Name_123", result))
  expect_true(grepl("æøå", result))
  expect_false(grepl("@", result, fixed = TRUE))
})

test_that("sanitize_filename collapses multiple underscores", {
  input <- "Test___Multiple___Underscores"
  result <- sanitize_filename(input)

  expect_false(grepl("__", result, fixed = TRUE))
  expect_equal(result, "Test_Multiple_Underscores")
})

# TEST: generate_export_filename() =============================================

test_that("generate_export_filename creates PDF filename", {
  result <- generate_export_filename(
    format = "pdf",
    title = "Test Chart",
    department = "Cardiology"
  )

  expect_match(result, "\\.pdf$")
  expect_match(result, "^SPC_")
  expect_match(result, "Cardiology")
  expect_match(result, "Test_Chart")
})

test_that("generate_export_filename creates PNG filename", {
  result <- generate_export_filename(
    format = "png",
    title = "Test Chart"
  )

  expect_match(result, "\\.png$")
  expect_match(result, "^SPC_")
})

test_that("generate_export_filename creates PowerPoint filename", {
  result <- generate_export_filename(
    format = "powerpoint",
    title = "Test Chart"
  )

  expect_match(result, "\\.pptx$")
  expect_match(result, "^SPC_")
})

test_that("generate_export_filename handles pptx format variant", {
  result <- generate_export_filename(
    format = "pptx",
    title = "Test Chart"
  )

  expect_match(result, "\\.pptx$")
})

test_that("generate_export_filename uses prefix constant", {
  result <- generate_export_filename(
    format = "pdf",
    title = "Test"
  )

  expect_match(result, paste0("^", EXPORT_FILENAME_PREFIX, "_"))
})

test_that("generate_export_filename uses separator constant", {
  result <- generate_export_filename(
    format = "pdf",
    title = "Test",
    department = "Dept"
  )

  # Should use separator between components
  expect_match(result, paste0(EXPORT_FILENAME_PREFIX,
                              EXPORT_FILENAME_SEPARATOR,
                              "Dept",
                              EXPORT_FILENAME_SEPARATOR,
                              "Test"))
})

test_that("generate_export_filename sanitizes title", {
  result <- generate_export_filename(
    format = "pdf",
    title = "Test@#$Chart"
  )

  # Special characters should be removed via sanitize_filename
  expect_false(grepl("@", result, fixed = TRUE))
  expect_false(grepl("#", result, fixed = TRUE))
  expect_match(result, "TestChart")
})

test_that("generate_export_filename sanitizes department", {
  result <- generate_export_filename(
    format = "pdf",
    department = "Dept @#$ Name"
  )

  expect_false(grepl("@", result, fixed = TRUE))
  expect_match(result, "Dept_Name")
})

test_that("generate_export_filename handles empty title", {
  result <- generate_export_filename(
    format = "pdf",
    title = "",
    department = "Dept"
  )

  # Should only include prefix and department
  expect_match(result, "^SPC_Dept\\.pdf$")
})

test_that("generate_export_filename handles empty department", {
  result <- generate_export_filename(
    format = "pdf",
    title = "Title",
    department = ""
  )

  # Should only include prefix and title
  expect_match(result, "^SPC_Title\\.pdf$")
})

test_that("generate_export_filename handles both empty", {
  result <- generate_export_filename(
    format = "pdf",
    title = "",
    department = ""
  )

  # Should only include prefix
  expect_equal(result, "SPC.pdf")
})

test_that("generate_export_filename preserves Danish characters", {
  result <- generate_export_filename(
    format = "pdf",
    title = "Grafisk Oversigt",
    department = "Hæmatologi Afdeling Ø"
  )

  expect_match(result, "Hæmatologi")
  expect_match(result, "Ø")
})

test_that("generate_export_filename handles case-insensitive format", {
  result1 <- generate_export_filename(format = "PDF", title = "Test")
  result2 <- generate_export_filename(format = "pdf", title = "Test")

  expect_match(result1, "\\.pdf$")
  expect_match(result2, "\\.pdf$")
})

test_that("generate_export_filename defaults to PDF for unknown format", {
  result <- generate_export_filename(
    format = "unknown",
    title = "Test"
  )

  expect_match(result, "\\.pdf$")
})

test_that("generate_export_filename orders components correctly", {
  result <- generate_export_filename(
    format = "pdf",
    title = "MyTitle",
    department = "MyDept"
  )

  # Order should be: Prefix _ Department _ Title . Extension
  expect_equal(result, "SPC_MyDept_MyTitle.pdf")
})

# EDGE CASES ===================================================================

test_that("sanitize_filename handles very long strings", {
  long_input <- paste(rep("A", 500), collapse = "")
  result <- sanitize_filename(long_input)

  # Should handle without error
  expect_type(result, "character")
  expect_equal(nchar(result), 500)
})

test_that("sanitize_filename handles only special characters", {
  input <- "@#$%^&*()"
  result <- sanitize_filename(input)

  # Should result in empty string
  expect_equal(result, "")
})

test_that("sanitize_filename handles only spaces", {
  input <- "     "
  result <- sanitize_filename(input)

  # Should result in empty string after trimming and underscore removal
  expect_equal(result, "")
})

test_that("sanitize_filename handles numeric strings", {
  input <- "12345"
  result <- sanitize_filename(input)

  expect_equal(result, "12345")
})

test_that("generate_export_filename handles very long title", {
  long_title <- paste(rep("A", 300), collapse = "")
  result <- generate_export_filename(
    format = "pdf",
    title = long_title
  )

  # Should include long title without truncation
  expect_match(result, paste(rep("A", 300), collapse = ""))
})

test_that("sanitize_filename handles Unicode characters", {
  input <- "Test 中文 العربية"
  result <- sanitize_filename(input)

  # Non-allowed Unicode should be removed
  # Only Latin chars, Danish chars, numbers allowed
  expect_match(result, "Test")
  expect_false(grepl("中", result))
})

test_that("sanitize_filename handles path separators", {
  input <- "Test/Path\\Separators"
  result <- sanitize_filename(input)

  # Path separators should be removed
  expect_false(grepl("/", result, fixed = TRUE))
  expect_false(grepl("\\\\", result, fixed = TRUE))
})

test_that("generate_export_filename handles NULL title and department", {
  # Should handle NULL gracefully (treat as empty)
  expect_error(
    generate_export_filename(format = "pdf", title = NULL, department = NULL),
    NA  # No error expected
  )
})

test_that("sanitize_filename preserves underscore and hyphen mix", {
  input <- "Test_Name-With_Mixed-Separators"
  result <- sanitize_filename(input)

  expect_equal(result, "Test_Name-With_Mixed-Separators")
})
