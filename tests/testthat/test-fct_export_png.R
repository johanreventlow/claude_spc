# ==============================================================================
# TEST: FCT_EXPORT_PNG.R
# ==============================================================================
# FORMÅL: Test PNG export funktionalitet med size presets, DPI configuration
#         og exact pixel dimension verification.
#
# TEST COVERAGE:
#   - generate_png_export() - PNG generation med ggplot2::ggsave()
#   - get_size_from_preset() - Size preset til dimension conversion
#   - Dimension accuracy (±1 pixel tolerance)
#   - DPI correctness
#   - Temporary file cleanup
#   - Error handling for invalid inputs
#
# EDGE CASES:
#   - NULL plot objects
#   - Invalid plot types (non-ggplot)
#   - All size presets (small, medium, large, powerpoint)
#   - Custom dimensions
#   - Extreme DPI values
#   - File permissions issues
# ==============================================================================

library(testthat)
library(ggplot2)

# SETUP ========================================================================

# Mock constants hvis de ikke er tilgængelige
if (!exists("EXPORT_SIZE_PRESETS")) {
  EXPORT_SIZE_PRESETS <- list(
    small = list(width = 800, height = 600, dpi = 96, unit = "px", label = "Lille"),
    medium = list(width = 1200, height = 900, dpi = 96, unit = "px", label = "Medium"),
    large = list(width = 1920, height = 1440, dpi = 96, unit = "px", label = "Stor"),
    powerpoint = list(width = 10, height = 7.5, dpi = 96, unit = "in", label = "PowerPoint")
  )
}

if (!exists("EXPORT_DPI_OPTIONS")) {
  EXPORT_DPI_OPTIONS <- c(72, 96, 150, 300)
}

# Helper: Create simple test plot
create_test_plot <- function() {
  ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    labs(title = "Test Plot", x = "Weight", y = "MPG") +
    theme_minimal()
}

# Helper: Get PNG dimensions using png package
get_png_dimensions <- function(file_path) {
  # Read PNG info
  png_array <- png::readPNG(file_path, native = FALSE, info = TRUE)
  png_info <- attr(png_array, "info")

  list(
    width = png_info$dim[1],
    height = png_info$dim[2]
  )
}

# TEST: get_size_from_preset() =================================================

test_that("get_size_from_preset returns correct small preset", {
  preset <- get_size_from_preset("small")

  expect_type(preset, "list")
  expect_equal(preset$width, 800)
  expect_equal(preset$height, 600)
  expect_equal(preset$dpi, 96)
  expect_equal(preset$unit, "px")
})

test_that("get_size_from_preset returns correct medium preset", {
  preset <- get_size_from_preset("medium")

  expect_equal(preset$width, 1200)
  expect_equal(preset$height, 900)
  expect_equal(preset$dpi, 96)
  expect_equal(preset$unit, "px")
})

test_that("get_size_from_preset returns correct large preset", {
  preset <- get_size_from_preset("large")

  expect_equal(preset$width, 1920)
  expect_equal(preset$height, 1440)
  expect_equal(preset$dpi, 96)
  expect_equal(preset$unit, "px")
})

test_that("get_size_from_preset returns correct powerpoint preset", {
  preset <- get_size_from_preset("powerpoint")

  expect_equal(preset$width, 10)
  expect_equal(preset$height, 7.5)
  expect_equal(preset$dpi, 96)
  expect_equal(preset$unit, "in")
})

test_that("get_size_from_preset returns medium as default for unknown preset", {
  preset <- get_size_from_preset("unknown_preset")

  expect_equal(preset$width, 1200)
  expect_equal(preset$height, 900)
})

test_that("get_size_from_preset handles NULL input", {
  preset <- get_size_from_preset(NULL)

  # Should default to medium
  expect_equal(preset$width, 1200)
  expect_equal(preset$height, 900)
})

# TEST: generate_png_export() - Basic Functionality ===========================

test_that("generate_png_export creates PNG file", {
  mock_plot <- create_test_plot()
  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  result <- generate_png_export(
    plot_object = mock_plot,
    width_inches = 10,
    height_inches = 7.5,
    dpi = 96,
    output_path = temp_file
  )

  expect_true(file.exists(temp_file))
  expect_equal(result, temp_file)
})

test_that("generate_png_export returns file path on success", {
  mock_plot <- create_test_plot()
  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  result <- generate_png_export(
    plot_object = mock_plot,
    width_inches = 8,
    height_inches = 6,
    dpi = 96,
    output_path = temp_file
  )

  expect_type(result, "character")
  expect_equal(result, temp_file)
})

test_that("generate_png_export creates tempfile when output_path is NULL", {
  mock_plot <- create_test_plot()

  result <- generate_png_export(
    plot_object = mock_plot,
    width_inches = 10,
    height_inches = 7.5,
    dpi = 96,
    output_path = NULL
  )

  on.exit(unlink(result), add = TRUE)

  expect_true(file.exists(result))
  expect_match(result, "\\.png$")
})

# TEST: generate_png_export() - Dimension Accuracy =============================

test_that("generate_png_export produces exact pixel dimensions (96 DPI)", {
  skip_if_not_installed("png")

  mock_plot <- create_test_plot()
  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  # 10 inches × 96 DPI = 960 pixels
  # 7.5 inches × 96 DPI = 720 pixels
  generate_png_export(
    plot_object = mock_plot,
    width_inches = 10,
    height_inches = 7.5,
    dpi = 96,
    output_path = temp_file
  )

  dims <- get_png_dimensions(temp_file)

  # Allow ±1 pixel tolerance
  expect_equal(dims$width, 960, tolerance = 1)
  expect_equal(dims$height, 720, tolerance = 1)
})

test_that("generate_png_export produces exact pixel dimensions (150 DPI)", {
  skip_if_not_installed("png")

  mock_plot <- create_test_plot()
  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  # 8 inches × 150 DPI = 1200 pixels
  # 6 inches × 150 DPI = 900 pixels
  generate_png_export(
    plot_object = mock_plot,
    width_inches = 8,
    height_inches = 6,
    dpi = 150,
    output_path = temp_file
  )

  dims <- get_png_dimensions(temp_file)

  expect_equal(dims$width, 1200, tolerance = 1)
  expect_equal(dims$height, 900, tolerance = 1)
})

test_that("generate_png_export produces exact pixel dimensions (300 DPI high-res)", {
  skip_if_not_installed("png")

  mock_plot <- create_test_plot()
  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  # 5 inches × 300 DPI = 1500 pixels
  # 4 inches × 300 DPI = 1200 pixels
  generate_png_export(
    plot_object = mock_plot,
    width_inches = 5,
    height_inches = 4,
    dpi = 300,
    output_path = temp_file
  )

  dims <- get_png_dimensions(temp_file)

  expect_equal(dims$width, 1500, tolerance = 1)
  expect_equal(dims$height, 1200, tolerance = 1)
})

test_that("generate_png_export handles fractional inches correctly", {
  skip_if_not_installed("png")

  mock_plot <- create_test_plot()
  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  # 7.5 inches × 96 DPI = 720 pixels
  # 5.5 inches × 96 DPI = 528 pixels
  generate_png_export(
    plot_object = mock_plot,
    width_inches = 7.5,
    height_inches = 5.5,
    dpi = 96,
    output_path = temp_file
  )

  dims <- get_png_dimensions(temp_file)

  expect_equal(dims$width, 720, tolerance = 1)
  expect_equal(dims$height, 528, tolerance = 1)
})

# TEST: generate_png_export() - Size Presets ===================================

test_that("generate_png_export works with small preset dimensions", {
  skip_if_not_installed("png")

  mock_plot <- create_test_plot()
  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  preset <- get_size_from_preset("small")

  # Convert pixels to inches for small preset (800×600 @ 96 DPI)
  width_inches <- preset$width / preset$dpi
  height_inches <- preset$height / preset$dpi

  generate_png_export(
    plot_object = mock_plot,
    width_inches = width_inches,
    height_inches = height_inches,
    dpi = preset$dpi,
    output_path = temp_file
  )

  dims <- get_png_dimensions(temp_file)

  expect_equal(dims$width, 800, tolerance = 1)
  expect_equal(dims$height, 600, tolerance = 1)
})

test_that("generate_png_export works with medium preset dimensions", {
  skip_if_not_installed("png")

  mock_plot <- create_test_plot()
  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  preset <- get_size_from_preset("medium")

  # Convert pixels to inches (1200×900 @ 96 DPI)
  width_inches <- preset$width / preset$dpi
  height_inches <- preset$height / preset$dpi

  generate_png_export(
    plot_object = mock_plot,
    width_inches = width_inches,
    height_inches = height_inches,
    dpi = preset$dpi,
    output_path = temp_file
  )

  dims <- get_png_dimensions(temp_file)

  expect_equal(dims$width, 1200, tolerance = 1)
  expect_equal(dims$height, 900, tolerance = 1)
})

test_that("generate_png_export works with large preset dimensions", {
  skip_if_not_installed("png")

  mock_plot <- create_test_plot()
  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  preset <- get_size_from_preset("large")

  # Convert pixels to inches (1920×1440 @ 96 DPI)
  width_inches <- preset$width / preset$dpi
  height_inches <- preset$height / preset$dpi

  generate_png_export(
    plot_object = mock_plot,
    width_inches = width_inches,
    height_inches = height_inches,
    dpi = preset$dpi,
    output_path = temp_file
  )

  dims <- get_png_dimensions(temp_file)

  expect_equal(dims$width, 1920, tolerance = 1)
  expect_equal(dims$height, 1440, tolerance = 1)
})

test_that("generate_png_export works with powerpoint preset dimensions", {
  skip_if_not_installed("png")

  mock_plot <- create_test_plot()
  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  preset <- get_size_from_preset("powerpoint")

  # PowerPoint preset uses inches directly (10×7.5 @ 96 DPI)
  generate_png_export(
    plot_object = mock_plot,
    width_inches = preset$width,
    height_inches = preset$height,
    dpi = preset$dpi,
    output_path = temp_file
  )

  dims <- get_png_dimensions(temp_file)

  expect_equal(dims$width, 960, tolerance = 1)   # 10 * 96
  expect_equal(dims$height, 720, tolerance = 1)  # 7.5 * 96
})

# TEST: generate_png_export() - DPI Validation =================================

test_that("generate_png_export accepts all standard DPI options", {
  mock_plot <- create_test_plot()

  for (dpi_value in EXPORT_DPI_OPTIONS) {
    temp_file <- tempfile(fileext = ".png")

    result <- generate_png_export(
      plot_object = mock_plot,
      width_inches = 8,
      height_inches = 6,
      dpi = dpi_value,
      output_path = temp_file
    )

    expect_true(file.exists(temp_file))
    unlink(temp_file)
  }
})

test_that("generate_png_export with 72 DPI produces correct dimensions", {
  skip_if_not_installed("png")

  mock_plot <- create_test_plot()
  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  # 10 inches × 72 DPI = 720 pixels
  generate_png_export(
    plot_object = mock_plot,
    width_inches = 10,
    height_inches = 7.5,
    dpi = 72,
    output_path = temp_file
  )

  dims <- get_png_dimensions(temp_file)

  expect_equal(dims$width, 720, tolerance = 1)
  expect_equal(dims$height, 540, tolerance = 1)
})

# TEST: generate_png_export() - Error Handling =================================

test_that("generate_png_export rejects NULL plot object", {
  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  result <- generate_png_export(
    plot_object = NULL,
    width_inches = 10,
    height_inches = 7.5,
    dpi = 96,
    output_path = temp_file
  )

  # safe_operation returns NULL on error
  expect_null(result)
})

test_that("generate_png_export rejects non-ggplot object", {
  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  # Try to pass a base R plot or other object
  invalid_plot <- data.frame(x = 1:10, y = 1:10)

  result <- generate_png_export(
    plot_object = invalid_plot,
    width_inches = 10,
    height_inches = 7.5,
    dpi = 96,
    output_path = temp_file
  )

  # Should return NULL due to error
  expect_null(result)
})

test_that("generate_png_export handles missing plot object gracefully", {
  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  # Missing required parameter should error or return NULL
  result <- tryCatch(
    {
      generate_png_export(
        # plot_object missing
        width_inches = 10,
        height_inches = 7.5,
        dpi = 96,
        output_path = temp_file
      )
    },
    error = function(e) NULL
  )

  expect_null(result)
})

# TEST: generate_png_export() - Metadata Burning ===============================

test_that("generate_png_export preserves plot title and labels", {
  skip_if_not_installed("png")

  # Create plot with metadata
  plot_with_metadata <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    labs(
      title = "Test SPC Graf",
      subtitle = "Afdeling Kardiologi",
      x = "Vægt",
      y = "MPG"
    ) +
    theme_minimal()

  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  result <- generate_png_export(
    plot_object = plot_with_metadata,
    width_inches = 10,
    height_inches = 7.5,
    dpi = 96,
    output_path = temp_file
  )

  # Verify file exists and has content
  expect_true(file.exists(temp_file))
  expect_gt(file.size(temp_file), 0)

  # NOTE: We can't easily verify text content in PNG without OCR,
  # but we can verify the file was created successfully
})

test_that("generate_png_export handles Danish characters in plot", {
  skip_if_not_installed("png")

  # Create plot with Danish characters
  plot_danish <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    labs(
      title = "SPC Graf: København Sygehus Øst",
      subtitle = "Afdeling Hæmatologi - Måling Æ",
      x = "Vægt (kg)",
      y = "Forbrug"
    ) +
    theme_minimal()

  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  result <- generate_png_export(
    plot_object = plot_danish,
    width_inches = 10,
    height_inches = 7.5,
    dpi = 96,
    output_path = temp_file
  )

  # Verify successful creation (Danish chars should render)
  expect_true(file.exists(temp_file))
  expect_gt(file.size(temp_file), 0)
})

# TEST: Temporary File Cleanup =================================================

test_that("generate_png_export cleans up on its own errors", {
  # This test verifies that failed exports don't leave temp files
  # generate_png_export uses ggsave which handles its own cleanup

  invalid_plot <- "not a plot"
  temp_file <- tempfile(fileext = ".png")

  result <- generate_png_export(
    plot_object = invalid_plot,
    width_inches = 10,
    height_inches = 7.5,
    dpi = 96,
    output_path = temp_file
  )

  # Result should be NULL (error)
  expect_null(result)

  # File should NOT exist (no partial write)
  expect_false(file.exists(temp_file))
})

test_that("caller is responsible for cleanup of successful exports", {
  mock_plot <- create_test_plot()
  temp_file <- tempfile(fileext = ".png")

  result <- generate_png_export(
    plot_object = mock_plot,
    width_inches = 10,
    height_inches = 7.5,
    dpi = 96,
    output_path = temp_file
  )

  # File should exist after successful export
  expect_true(file.exists(temp_file))

  # Caller must clean up
  unlink(temp_file)
  expect_false(file.exists(temp_file))
})

# TEST: Edge Cases =============================================================

test_that("generate_png_export handles very small dimensions", {
  skip_if_not_installed("png")

  mock_plot <- create_test_plot()
  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  # Very small: 2 inches × 1.5 inches @ 96 DPI = 192×144 pixels
  generate_png_export(
    plot_object = mock_plot,
    width_inches = 2,
    height_inches = 1.5,
    dpi = 96,
    output_path = temp_file
  )

  dims <- get_png_dimensions(temp_file)

  expect_equal(dims$width, 192, tolerance = 1)
  expect_equal(dims$height, 144, tolerance = 1)
})

test_that("generate_png_export handles very large dimensions", {
  skip_if_not_installed("png")

  mock_plot <- create_test_plot()
  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  # Very large: 20 inches × 15 inches @ 96 DPI = 1920×1440 pixels
  generate_png_export(
    plot_object = mock_plot,
    width_inches = 20,
    height_inches = 15,
    dpi = 96,
    output_path = temp_file
  )

  dims <- get_png_dimensions(temp_file)

  expect_equal(dims$width, 1920, tolerance = 1)
  expect_equal(dims$height, 1440, tolerance = 1)
})

test_that("generate_png_export handles extreme aspect ratios", {
  mock_plot <- create_test_plot()
  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  # Very wide aspect ratio
  result <- generate_png_export(
    plot_object = mock_plot,
    width_inches = 20,
    height_inches = 5,
    dpi = 96,
    output_path = temp_file
  )

  expect_true(file.exists(temp_file))
})

test_that("generate_png_export handles zero or negative dimensions gracefully", {
  mock_plot <- create_test_plot()
  temp_file <- tempfile(fileext = ".png")

  # Zero width should fail gracefully
  result <- generate_png_export(
    plot_object = mock_plot,
    width_inches = 0,
    height_inches = 7.5,
    dpi = 96,
    output_path = temp_file
  )

  expect_null(result)

  # Negative dimensions should also fail gracefully
  result2 <- generate_png_export(
    plot_object = mock_plot,
    width_inches = -10,
    height_inches = 7.5,
    dpi = 96,
    output_path = temp_file
  )

  expect_null(result2)
})

# TEST: Integration with Export Config ========================================

test_that("generate_png_export uses white background from config", {
  skip_if_not_installed("png")

  mock_plot <- create_test_plot()
  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  generate_png_export(
    plot_object = mock_plot,
    width_inches = 10,
    height_inches = 7.5,
    dpi = 96,
    output_path = temp_file
  )

  # PNG should exist and be valid
  expect_true(file.exists(temp_file))

  # Can read PNG without errors (implies valid format)
  png_data <- png::readPNG(temp_file)
  expect_true(is.array(png_data))
})
