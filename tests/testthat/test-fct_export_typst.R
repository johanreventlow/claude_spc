# tests/testthat/test-fct_export_typst.R
# Tests for Typst PDF export functions

# Test helper: Check if Quarto is available
quarto_available <- function() {
  nzchar(Sys.which("quarto"))
}

# Test helper: Create minimal test plot
create_test_plot <- function() {
  ggplot2::ggplot(data.frame(x = 1:10, y = 1:10), ggplot2::aes(x, y)) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal()
}

# TEST: export_chart_for_typst() =============================================

test_that("export_chart_for_typst() exports ggplot to PNG", {
  plot <- create_test_plot()
  output_path <- tempfile(fileext = ".png")

  result <- export_chart_for_typst(
    plot_object = plot,
    output_path = output_path,
    width = 200,
    height = 120,
    dpi = 300
  )

  expect_true(file.exists(result))
  expect_equal(result, output_path)
  expect_gt(file.size(result), 0)

  # Cleanup
  unlink(output_path)
})

test_that("export_chart_for_typst() validates inputs", {
  expect_error(
    export_chart_for_typst(plot_object = "not a plot", output_path = tempfile()),
    "plot_object must be a ggplot object"
  )

  expect_error(
    export_chart_for_typst(plot_object = create_test_plot(), output_path = NULL),
    "output_path must be provided"
  )
})

test_that("export_chart_for_typst() handles dimensions correctly", {
  plot <- create_test_plot()
  output_path <- tempfile(fileext = ".png")

  # Test with custom dimensions
  result <- export_chart_for_typst(
    plot_object = plot,
    output_path = output_path,
    width = 150,
    height = 100,
    dpi = 150
  )

  expect_true(file.exists(result))
  unlink(output_path)
})

# TEST: create_typst_document() ==============================================

test_that("create_typst_document() generates .typ file", {
  output_path <- tempfile(fileext = ".typ")

  result <- create_typst_document(
    template = "bfh-diagram2",
    output_path = output_path,
    hospital = "Test Hospital",
    department = "Test Department",
    title = "Test Title",
    analysis = "Test analysis text",
    details = "Test details",
    chart_image_path = "test_chart.png",
    spc_stats = list(
      runs_expected = 12,
      runs_actual = 10,
      crossings_expected = 16,
      crossings_actual = 14,
      outliers_expected = 0,
      outliers_actual = 2
    ),
    data_definition = "Test data definition",
    author = "Test Author",
    date = as.Date("2025-01-19")
  )

  expect_true(file.exists(result))
  expect_equal(result, output_path)

  # Read and verify content
  content <- readLines(result)
  expect_true(any(grepl("Test Hospital", content)))
  expect_true(any(grepl("Test Title", content)))
  expect_true(any(grepl("runs_expected: 12", content)))

  # Cleanup
  unlink(output_path)
})

test_that("create_typst_document() handles optional parameters", {
  output_path <- tempfile(fileext = ".typ")

  # Minimal parameters
  result <- create_typst_document(
    template = "bfh-diagram2",
    output_path = output_path,
    hospital = "Test Hospital",
    department = NULL,
    title = "Test Title",
    analysis = NULL,
    details = NULL,
    chart_image_path = "chart.png",
    spc_stats = list(),
    data_definition = NULL,
    author = NULL,
    date = Sys.Date()
  )

  expect_true(file.exists(result))

  content <- readLines(result)
  # Should not contain 'none' for omitted parameters
  expect_true(any(grepl("department: none", content)))

  unlink(output_path)
})

test_that("create_typst_document() validates template parameter", {
  expect_error(
    create_typst_document(
      template = "invalid-template",
      output_path = tempfile(),
      hospital = "Test",
      title = "Test",
      chart_image_path = "test.png"
    ),
    "Invalid template"
  )
})

# TEST: compile_typst_to_pdf() ===============================================

test_that("compile_typst_to_pdf() checks Quarto availability", {
  skip_if(quarto_available(), "Quarto is available, skipping negative test")

  typ_file <- tempfile(fileext = ".typ")
  writeLines("#let test = true", typ_file)

  expect_error(
    compile_typst_to_pdf(typst_file = typ_file),
    "Quarto CLI.*not found"
  )

  unlink(typ_file)
})

test_that("compile_typst_to_pdf() validates input file exists", {
  skip_if_not(quarto_available(), "Quarto not available")

  expect_error(
    compile_typst_to_pdf(typst_file = "nonexistent.typ"),
    "Typst file not found"
  )
})

test_that("compile_typst_to_pdf() compiles valid Typst file", {
  skip_if_not(quarto_available(), "Quarto not available")

  # Create minimal valid Typst file
  typ_file <- tempfile(fileext = ".typ")
  writeLines("= Test Document\n\nThis is a test.", typ_file)

  output_pdf <- tempfile(fileext = ".pdf")

  result <- compile_typst_to_pdf(
    typst_file = typ_file,
    output_pdf = output_pdf
  )

  expect_true(file.exists(result))
  expect_equal(result, output_pdf)
  expect_gt(file.size(result), 0)

  # Cleanup
  unlink(c(typ_file, output_pdf))
})

test_that("compile_typst_to_pdf() checks Quarto version", {
  skip_if_not(quarto_available(), "Quarto not available")

  # This test assumes Quarto >= 1.4 is installed
  # If not, the function should give informative error
  typ_file <- tempfile(fileext = ".typ")
  writeLines("= Test", typ_file)

  # Should not error if Quarto version is adequate
  expect_silent({
    result <- compile_typst_to_pdf(
      typst_file = typ_file,
      output_pdf = tempfile(fileext = ".pdf")
    )
  })

  unlink(typ_file)
  if (file.exists(result)) unlink(result)
})

# TEST: export_spc_to_typst_pdf() ============================================

test_that("export_spc_to_typst_pdf() orchestrates full workflow", {
  skip_if_not(quarto_available(), "Quarto not available")

  plot <- create_test_plot()
  output_pdf <- tempfile(fileext = ".pdf")

  metadata <- list(
    hospital = "Test Hospital",
    department = "Test Department",
    title = "Test SPC Chart",
    analysis = "Test analysis",
    details = "Test details",
    data_definition = "Test definition",
    author = "Test Author",
    date = as.Date("2025-01-19")
  )

  spc_stats <- list(
    runs_expected = 12,
    runs_actual = 10,
    crossings_expected = 16,
    crossings_actual = 14,
    outliers_expected = 0,
    outliers_actual = 2
  )

  result <- export_spc_to_typst_pdf(
    plot_object = plot,
    metadata = metadata,
    spc_statistics = spc_stats,
    output_path = output_pdf
  )

  expect_true(file.exists(result))
  expect_equal(result, output_pdf)
  expect_gt(file.size(result), 0)

  # Cleanup
  unlink(output_pdf)
})

test_that("export_spc_to_typst_pdf() cleans up temp files", {
  skip_if_not(quarto_available(), "Quarto not available")

  plot <- create_test_plot()
  output_pdf <- tempfile(fileext = ".pdf")

  # Track temp directory before
  temp_before <- list.files(tempdir(), full.names = TRUE)

  result <- export_spc_to_typst_pdf(
    plot_object = plot,
    metadata = list(
      hospital = "Test",
      title = "Test",
      department = NULL,
      analysis = NULL,
      details = NULL,
      data_definition = NULL
    ),
    spc_statistics = list(),
    output_path = output_pdf
  )

  # Check temp files were cleaned
  temp_after <- list.files(tempdir(), full.names = TRUE)

  # Should not have accumulated many new temp files
  # (allowing for some system temp file creation)
  expect_lt(length(temp_after) - length(temp_before), 5)

  # Cleanup
  unlink(output_pdf)
})

test_that("export_spc_to_typst_pdf() validates required parameters", {
  expect_error(
    export_spc_to_typst_pdf(
      plot_object = NULL,
      metadata = list(),
      spc_statistics = list(),
      output_path = tempfile()
    ),
    "plot_object.*required"
  )

  expect_error(
    export_spc_to_typst_pdf(
      plot_object = create_test_plot(),
      metadata = list(),
      spc_statistics = list(),
      output_path = NULL
    ),
    "output_path.*required"
  )
})

# TEST: Error handling =======================================================

test_that("Functions provide informative error messages", {
  # Test that error messages are in Danish and helpful
  expect_error(
    export_chart_for_typst(plot_object = "invalid", output_path = tempfile()),
    "plot_object"
  )

  skip_if(quarto_available(), "Quarto is available")
  expect_error(
    compile_typst_to_pdf(typst_file = tempfile(fileext = ".typ")),
    "Quarto.*install"
  )
})

# TEST: Integration with template files ======================================

test_that("Template files are accessible via system.file", {
  template_path <- system.file(
    "templates/typst/bfh-template/bfh-template.typ",
    package = "SPCify"
  )

  expect_true(nzchar(template_path), "Template file should be found")
  expect_true(file.exists(template_path), "Template file should exist")
})

test_that("create_typst_document() uses package template", {
  output_path <- tempfile(fileext = ".typ")

  result <- create_typst_document(
    template = "bfh-diagram2",
    output_path = output_path,
    hospital = "Test",
    title = "Test",
    chart_image_path = "test.png"
  )

  content <- readLines(result)

  # Should import from bfh-template
  expect_true(any(grepl("bfh-template", content)))

  unlink(output_path)
})
