# test-config_export.R
# Comprehensive tests for export configuration constants
# Critical for ensuring export functionality has correct defaults and validation rules

library(testthat)

# EXPORT SIZE PRESETS TESTS ====================================================

test_that("EXPORT_SIZE_PRESETS has all required presets", {
  # TEST: All expected presets exist
  expect_true(exists("EXPORT_SIZE_PRESETS"),
              "EXPORT_SIZE_PRESETS constant must be defined")

  expect_type(EXPORT_SIZE_PRESETS, "list")

  # Verify all required presets are present
  required_presets <- c("small", "medium", "large", "powerpoint")
  for (preset in required_presets) {
    expect_true(preset %in% names(EXPORT_SIZE_PRESETS),
                info = paste("Preset", preset, "must be defined"))
  }
})

test_that("EXPORT_SIZE_PRESETS small preset has correct structure", {
  # TEST: Small preset configuration
  preset <- EXPORT_SIZE_PRESETS$small

  expect_type(preset, "list")
  expect_true("width" %in% names(preset))
  expect_true("height" %in% names(preset))
  expect_true("dpi" %in% names(preset))
  expect_true("unit" %in% names(preset))
  expect_true("label" %in% names(preset))

  # Verify values match specification
  expect_equal(preset$width, 800)
  expect_equal(preset$height, 600)
  expect_equal(preset$dpi, 96)
  expect_equal(preset$unit, "px")
  expect_equal(preset$label, "Lille (800 × 600 px)")
})

test_that("EXPORT_SIZE_PRESETS medium preset has correct structure", {
  # TEST: Medium preset configuration
  preset <- EXPORT_SIZE_PRESETS$medium

  expect_type(preset, "list")
  expect_equal(preset$width, 1200)
  expect_equal(preset$height, 900)
  expect_equal(preset$dpi, 96)
  expect_equal(preset$unit, "px")
  expect_equal(preset$label, "Medium (1200 × 900 px)")
})

test_that("EXPORT_SIZE_PRESETS large preset has correct structure", {
  # TEST: Large preset configuration
  preset <- EXPORT_SIZE_PRESETS$large

  expect_type(preset, "list")
  expect_equal(preset$width, 1920)
  expect_equal(preset$height, 1440)
  expect_equal(preset$dpi, 96)
  expect_equal(preset$unit, "px")
  expect_equal(preset$label, "Stor (1920 × 1440 px)")
})

test_that("EXPORT_SIZE_PRESETS powerpoint preset has correct structure", {
  # TEST: PowerPoint preset configuration
  preset <- EXPORT_SIZE_PRESETS$powerpoint

  expect_type(preset, "list")
  expect_equal(preset$width, 10)
  expect_equal(preset$height, 7.5)
  expect_equal(preset$dpi, 96)
  expect_equal(preset$unit, "in")
  expect_equal(preset$label, "Optimal til PowerPoint (10 × 7.5 in)")
})

test_that("EXPORT_SIZE_PRESETS all presets have valid aspect ratios", {
  # TEST: Aspect ratios are reasonable for all presets
  for (preset_name in names(EXPORT_SIZE_PRESETS)) {
    preset <- EXPORT_SIZE_PRESETS[[preset_name]]

    aspect_ratio <- preset$width / preset$height
    expect_true(aspect_ratio >= 0.5 && aspect_ratio <= 2.0,
                info = paste("Preset", preset_name, "has invalid aspect ratio:", aspect_ratio))
  }
})

# EXPORT DPI OPTIONS TESTS =====================================================

test_that("EXPORT_DPI_OPTIONS has correct values", {
  # TEST: DPI options are defined correctly
  expect_true(exists("EXPORT_DPI_OPTIONS"),
              "EXPORT_DPI_OPTIONS constant must be defined")

  expect_type(EXPORT_DPI_OPTIONS, "double")
  expect_equal(length(EXPORT_DPI_OPTIONS), 4)

  # Verify exact DPI values
  expect_equal(EXPORT_DPI_OPTIONS, c(72, 96, 150, 300))

  # Verify DPI values are in ascending order
  expect_true(all(diff(EXPORT_DPI_OPTIONS) > 0),
              "DPI options should be in ascending order")
})

test_that("EXPORT_DPI_OPTIONS are all positive integers", {
  # TEST: DPI values are valid positive integers
  for (dpi in EXPORT_DPI_OPTIONS) {
    expect_true(dpi > 0, info = paste("DPI", dpi, "must be positive"))
    expect_true(dpi == round(dpi), info = paste("DPI", dpi, "must be integer"))
  }
})

# ASPECT RATIO CONSTRAINTS TESTS ===============================================

test_that("EXPORT_ASPECT_RATIO_MIN has correct value", {
  # TEST: Minimum aspect ratio constraint
  expect_true(exists("EXPORT_ASPECT_RATIO_MIN"),
              "EXPORT_ASPECT_RATIO_MIN constant must be defined")

  expect_type(EXPORT_ASPECT_RATIO_MIN, "double")
  expect_equal(EXPORT_ASPECT_RATIO_MIN, 0.5)
  expect_true(EXPORT_ASPECT_RATIO_MIN > 0 && EXPORT_ASPECT_RATIO_MIN < 1,
              "Minimum aspect ratio should be between 0 and 1")
})

test_that("EXPORT_ASPECT_RATIO_MAX has correct value", {
  # TEST: Maximum aspect ratio constraint
  expect_true(exists("EXPORT_ASPECT_RATIO_MAX"),
              "EXPORT_ASPECT_RATIO_MAX constant must be defined")

  expect_type(EXPORT_ASPECT_RATIO_MAX, "double")
  expect_equal(EXPORT_ASPECT_RATIO_MAX, 2.0)
  expect_true(EXPORT_ASPECT_RATIO_MAX > 1,
              "Maximum aspect ratio should be greater than 1")
})

test_that("Aspect ratio min and max are logically consistent", {
  # TEST: Min is less than max
  expect_lt(EXPORT_ASPECT_RATIO_MIN, EXPORT_ASPECT_RATIO_MAX,
            "Minimum aspect ratio must be less than maximum")

  # TEST: Range covers standard aspect ratios (4:3 = 1.33, 16:9 = 1.78)
  expect_true(4/3 >= EXPORT_ASPECT_RATIO_MIN && 4/3 <= EXPORT_ASPECT_RATIO_MAX,
              "Standard 4:3 aspect ratio should be within allowed range")
  expect_true(16/9 >= EXPORT_ASPECT_RATIO_MIN && 16/9 <= EXPORT_ASPECT_RATIO_MAX,
              "Standard 16:9 aspect ratio should be within allowed range")
})

# METADATA CHARACTER LIMITS TESTS ==============================================

test_that("EXPORT_TITLE_MAX_LENGTH has correct value", {
  # TEST: Title length limit
  expect_true(exists("EXPORT_TITLE_MAX_LENGTH"),
              "EXPORT_TITLE_MAX_LENGTH constant must be defined")

  expect_type(EXPORT_TITLE_MAX_LENGTH, "double")
  expect_equal(EXPORT_TITLE_MAX_LENGTH, 200)
  expect_true(EXPORT_TITLE_MAX_LENGTH > 0,
              "Title max length must be positive")
})

test_that("EXPORT_DESCRIPTION_MAX_LENGTH has correct value", {
  # TEST: Description length limit
  expect_true(exists("EXPORT_DESCRIPTION_MAX_LENGTH"),
              "EXPORT_DESCRIPTION_MAX_LENGTH constant must be defined")

  expect_type(EXPORT_DESCRIPTION_MAX_LENGTH, "double")
  expect_equal(EXPORT_DESCRIPTION_MAX_LENGTH, 2000)
  expect_true(EXPORT_DESCRIPTION_MAX_LENGTH > EXPORT_TITLE_MAX_LENGTH,
              "Description max length should be greater than title max length")
})

test_that("EXPORT_DEPARTMENT_MAX_LENGTH has correct value", {
  # TEST: Department name length limit
  expect_true(exists("EXPORT_DEPARTMENT_MAX_LENGTH"),
              "EXPORT_DEPARTMENT_MAX_LENGTH constant must be defined")

  expect_type(EXPORT_DEPARTMENT_MAX_LENGTH, "double")
  expect_equal(EXPORT_DEPARTMENT_MAX_LENGTH, 100)
  expect_true(EXPORT_DEPARTMENT_MAX_LENGTH > 0 && EXPORT_DEPARTMENT_MAX_LENGTH <= EXPORT_TITLE_MAX_LENGTH,
              "Department max length should be reasonable compared to title")
})

# FILENAME GENERATION CONSTANTS TESTS ==========================================

test_that("EXPORT_FILENAME_PREFIX has correct value", {
  # TEST: Filename prefix constant
  expect_true(exists("EXPORT_FILENAME_PREFIX"),
              "EXPORT_FILENAME_PREFIX constant must be defined")

  expect_type(EXPORT_FILENAME_PREFIX, "character")
  expect_equal(EXPORT_FILENAME_PREFIX, "SPC")
  expect_true(nchar(EXPORT_FILENAME_PREFIX) > 0,
              "Filename prefix must not be empty")
})

test_that("EXPORT_FILENAME_SEPARATOR has correct value", {
  # TEST: Filename separator constant
  expect_true(exists("EXPORT_FILENAME_SEPARATOR"),
              "EXPORT_FILENAME_SEPARATOR constant must be defined")

  expect_type(EXPORT_FILENAME_SEPARATOR, "character")
  expect_equal(EXPORT_FILENAME_SEPARATOR, "_")
  expect_true(nchar(EXPORT_FILENAME_SEPARATOR) == 1,
              "Filename separator should be single character")
})

# EXPORT FORMAT OPTIONS TESTS ==================================================

test_that("EXPORT_FORMAT_OPTIONS has all required formats", {
  # TEST: Format options are defined
  expect_true(exists("EXPORT_FORMAT_OPTIONS"),
              "EXPORT_FORMAT_OPTIONS constant must be defined")

  expect_type(EXPORT_FORMAT_OPTIONS, "character")

  # Verify all required formats
  expect_true("PDF" %in% names(EXPORT_FORMAT_OPTIONS))
  expect_true("PNG" %in% names(EXPORT_FORMAT_OPTIONS))
  expect_true("PowerPoint" %in% names(EXPORT_FORMAT_OPTIONS))

  # Verify format codes
  expect_equal(EXPORT_FORMAT_OPTIONS[["PDF"]], "pdf")
  expect_equal(EXPORT_FORMAT_OPTIONS[["PNG"]], "png")
  expect_equal(EXPORT_FORMAT_OPTIONS[["PowerPoint"]], "pptx")
})

# PDF CONFIGURATION TESTS ======================================================

test_that("EXPORT_PDF_CONFIG has correct structure", {
  # TEST: PDF configuration
  expect_true(exists("EXPORT_PDF_CONFIG"),
              "EXPORT_PDF_CONFIG constant must be defined")

  expect_type(EXPORT_PDF_CONFIG, "list")

  # Verify required fields
  required_fields <- c("paper", "orientation", "margin_top", "margin_bottom",
                      "margin_left", "margin_right", "encoding", "version")
  for (field in required_fields) {
    expect_true(field %in% names(EXPORT_PDF_CONFIG),
                info = paste("PDF config missing field:", field))
  }

  # Verify specific values
  expect_equal(EXPORT_PDF_CONFIG$paper, "a4")
  expect_equal(EXPORT_PDF_CONFIG$orientation, "landscape")
  expect_equal(EXPORT_PDF_CONFIG$encoding, "UTF-8")
  expect_equal(EXPORT_PDF_CONFIG$version, "1.4")
})

test_that("EXPORT_PDF_CONFIG margins are valid", {
  # TEST: PDF margin values are reasonable
  expect_true(EXPORT_PDF_CONFIG$margin_top > 0 && EXPORT_PDF_CONFIG$margin_top < 5)
  expect_true(EXPORT_PDF_CONFIG$margin_bottom > 0 && EXPORT_PDF_CONFIG$margin_bottom < 5)
  expect_true(EXPORT_PDF_CONFIG$margin_left > 0 && EXPORT_PDF_CONFIG$margin_left < 5)
  expect_true(EXPORT_PDF_CONFIG$margin_right > 0 && EXPORT_PDF_CONFIG$margin_right < 5)
})

# PNG CONFIGURATION TESTS ======================================================

test_that("EXPORT_PNG_CONFIG has correct structure", {
  # TEST: PNG configuration
  expect_true(exists("EXPORT_PNG_CONFIG"),
              "EXPORT_PNG_CONFIG constant must be defined")

  expect_type(EXPORT_PNG_CONFIG, "list")

  # Verify required fields
  required_fields <- c("bg", "type", "compression")
  for (field in required_fields) {
    expect_true(field %in% names(EXPORT_PNG_CONFIG),
                info = paste("PNG config missing field:", field))
  }

  # Verify specific values
  expect_equal(EXPORT_PNG_CONFIG$bg, "white")
  expect_equal(EXPORT_PNG_CONFIG$type, "cairo")
  expect_true(EXPORT_PNG_CONFIG$compression >= 0 && EXPORT_PNG_CONFIG$compression <= 9,
              "PNG compression should be between 0 and 9")
})

# POWERPOINT CONFIGURATION TESTS ===============================================

test_that("EXPORT_POWERPOINT_CONFIG has correct structure", {
  # TEST: PowerPoint configuration
  expect_true(exists("EXPORT_POWERPOINT_CONFIG"),
              "EXPORT_POWERPOINT_CONFIG constant must be defined")

  expect_type(EXPORT_POWERPOINT_CONFIG, "list")

  # Verify required fields
  required_fields <- c("layout", "width", "height", "unit", "left_margin",
                      "top_margin", "title_font_size", "subtitle_font_size",
                      "body_font_size")
  for (field in required_fields) {
    expect_true(field %in% names(EXPORT_POWERPOINT_CONFIG),
                info = paste("PowerPoint config missing field:", field))
  }

  # Verify specific values
  expect_equal(EXPORT_POWERPOINT_CONFIG$layout, "Title and Content")
  expect_equal(EXPORT_POWERPOINT_CONFIG$width, 10)
  expect_equal(EXPORT_POWERPOINT_CONFIG$height, 7.5)
  expect_equal(EXPORT_POWERPOINT_CONFIG$unit, "in")
})

test_that("EXPORT_POWERPOINT_CONFIG font sizes are reasonable", {
  # TEST: Font sizes are in reasonable range
  expect_true(EXPORT_POWERPOINT_CONFIG$title_font_size >= 16 &&
              EXPORT_POWERPOINT_CONFIG$title_font_size <= 48)
  expect_true(EXPORT_POWERPOINT_CONFIG$subtitle_font_size >= 12 &&
              EXPORT_POWERPOINT_CONFIG$subtitle_font_size <= 32)
  expect_true(EXPORT_POWERPOINT_CONFIG$body_font_size >= 10 &&
              EXPORT_POWERPOINT_CONFIG$body_font_size <= 24)

  # Title should be larger than subtitle, subtitle larger than body
  expect_gt(EXPORT_POWERPOINT_CONFIG$title_font_size,
            EXPORT_POWERPOINT_CONFIG$subtitle_font_size)
  expect_gt(EXPORT_POWERPOINT_CONFIG$subtitle_font_size,
            EXPORT_POWERPOINT_CONFIG$body_font_size)
})

# EXPORT VALIDATION RULES TESTS ================================================

test_that("EXPORT_VALIDATION_RULES has correct structure", {
  # TEST: Validation rules configuration
  expect_true(exists("EXPORT_VALIDATION_RULES"),
              "EXPORT_VALIDATION_RULES constant must be defined")

  expect_type(EXPORT_VALIDATION_RULES, "list")

  # Verify required fields
  required_fields <- c("min_width_px", "max_width_px", "min_height_px",
                      "max_height_px", "min_dpi", "max_dpi",
                      "pdf_required_fields", "pdf_optional_fields")
  for (field in required_fields) {
    expect_true(field %in% names(EXPORT_VALIDATION_RULES),
                info = paste("Validation rules missing field:", field))
  }
})

test_that("EXPORT_VALIDATION_RULES dimension constraints are valid", {
  # TEST: Dimension validation rules are logical
  expect_lt(EXPORT_VALIDATION_RULES$min_width_px,
            EXPORT_VALIDATION_RULES$max_width_px,
            "Min width must be less than max width")
  expect_lt(EXPORT_VALIDATION_RULES$min_height_px,
            EXPORT_VALIDATION_RULES$max_height_px,
            "Min height must be less than max height")

  # Verify specific values
  expect_equal(EXPORT_VALIDATION_RULES$min_width_px, 400)
  expect_equal(EXPORT_VALIDATION_RULES$max_width_px, 4000)
  expect_equal(EXPORT_VALIDATION_RULES$min_height_px, 300)
  expect_equal(EXPORT_VALIDATION_RULES$max_height_px, 3000)
})

test_that("EXPORT_VALIDATION_RULES DPI constraints are valid", {
  # TEST: DPI validation rules are logical
  expect_lt(EXPORT_VALIDATION_RULES$min_dpi,
            EXPORT_VALIDATION_RULES$max_dpi,
            "Min DPI must be less than max DPI")

  # Verify specific values
  expect_equal(EXPORT_VALIDATION_RULES$min_dpi, 72)
  expect_equal(EXPORT_VALIDATION_RULES$max_dpi, 600)

  # Verify all standard DPI options are within valid range
  for (dpi in EXPORT_DPI_OPTIONS) {
    expect_true(dpi >= EXPORT_VALIDATION_RULES$min_dpi &&
                dpi <= EXPORT_VALIDATION_RULES$max_dpi,
                info = paste("DPI option", dpi, "should be within validation range"))
  }
})

test_that("EXPORT_VALIDATION_RULES PDF field requirements are valid", {
  # TEST: PDF field validation rules
  expect_type(EXPORT_VALIDATION_RULES$pdf_required_fields, "character")
  expect_type(EXPORT_VALIDATION_RULES$pdf_optional_fields, "character")

  # Verify required fields
  expect_true("title" %in% EXPORT_VALIDATION_RULES$pdf_required_fields)

  # Verify optional fields
  expected_optional <- c("department", "indicator_description", "improvement_potential")
  for (field in expected_optional) {
    expect_true(field %in% EXPORT_VALIDATION_RULES$pdf_optional_fields,
                info = paste("Expected optional field:", field))
  }

  # Ensure no overlap between required and optional
  overlap <- intersect(EXPORT_VALIDATION_RULES$pdf_required_fields,
                      EXPORT_VALIDATION_RULES$pdf_optional_fields)
  expect_equal(length(overlap), 0,
               info = "Required and optional fields should not overlap")
})

# INTEGRATION TESTS ============================================================

test_that("Size presets are compatible with validation rules", {
  # TEST: All size presets should be within validation constraints
  for (preset_name in names(EXPORT_SIZE_PRESETS)) {
    preset <- EXPORT_SIZE_PRESETS[[preset_name]]

    # Skip non-pixel units (like PowerPoint inches)
    if (preset$unit == "px") {
      expect_true(preset$width >= EXPORT_VALIDATION_RULES$min_width_px &&
                  preset$width <= EXPORT_VALIDATION_RULES$max_width_px,
                  info = paste("Preset", preset_name, "width should be within validation range"))
      expect_true(preset$height >= EXPORT_VALIDATION_RULES$min_height_px &&
                  preset$height <= EXPORT_VALIDATION_RULES$max_height_px,
                  info = paste("Preset", preset_name, "height should be within validation range"))
    }

    # DPI should be within validation range
    expect_true(preset$dpi >= EXPORT_VALIDATION_RULES$min_dpi &&
                preset$dpi <= EXPORT_VALIDATION_RULES$max_dpi,
                info = paste("Preset", preset_name, "DPI should be within validation range"))
  }
})

test_that("All configuration constants are exported", {
  # TEST: All constants are available in package namespace
  exported_constants <- c(
    "EXPORT_SIZE_PRESETS",
    "EXPORT_DPI_OPTIONS",
    "EXPORT_ASPECT_RATIO_MIN",
    "EXPORT_ASPECT_RATIO_MAX",
    "EXPORT_TITLE_MAX_LENGTH",
    "EXPORT_DESCRIPTION_MAX_LENGTH",
    "EXPORT_DEPARTMENT_MAX_LENGTH",
    "EXPORT_FILENAME_PREFIX",
    "EXPORT_FILENAME_SEPARATOR",
    "EXPORT_FORMAT_OPTIONS",
    "EXPORT_PDF_CONFIG",
    "EXPORT_PNG_CONFIG",
    "EXPORT_POWERPOINT_CONFIG",
    "EXPORT_VALIDATION_RULES"
  )

  for (constant in exported_constants) {
    expect_true(exists(constant),
                info = paste("Constant", constant, "should be defined"))
  }
})

test_that("Configuration constants are immutable types", {
  # TEST: Constants should not be functions or environments
  # (This ensures they are data, not code)

  expect_false(is.function(EXPORT_SIZE_PRESETS))
  expect_false(is.function(EXPORT_DPI_OPTIONS))
  expect_false(is.function(EXPORT_PDF_CONFIG))
  expect_false(is.function(EXPORT_PNG_CONFIG))
  expect_false(is.function(EXPORT_POWERPOINT_CONFIG))
  expect_false(is.function(EXPORT_VALIDATION_RULES))
})
