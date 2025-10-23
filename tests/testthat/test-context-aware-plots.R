# tests/testthat/test-context-aware-plots.R
# Tests for context-aware plot generation and cache isolation
# Related: Issue #61 (label placement), Issue #62 (caching)

# SETUP ========================================================================

test_that("config_plot_contexts.R is loaded", {
  expect_true(exists("PLOT_CONTEXTS"))
  expect_true(exists("PLOT_CONTEXT_DIMENSIONS"))
  expect_true(exists("get_context_dimensions"))
  expect_true(exists("validate_plot_context"))
})

# PLOT CONTEXTS CONFIGURATION ==================================================

test_that("PLOT_CONTEXTS contains all required contexts", {
  expect_type(PLOT_CONTEXTS, "list")
  expect_true("ANALYSIS" %in% names(PLOT_CONTEXTS))
  expect_true("EXPORT_PREVIEW" %in% names(PLOT_CONTEXTS))
  expect_true("EXPORT_PDF" %in% names(PLOT_CONTEXTS))
  expect_true("EXPORT_PNG" %in% names(PLOT_CONTEXTS))
  expect_true("EXPORT_PPTX" %in% names(PLOT_CONTEXTS))

  # Verify context values
  expect_equal(PLOT_CONTEXTS$ANALYSIS, "analysis")
  expect_equal(PLOT_CONTEXTS$EXPORT_PREVIEW, "export_preview")
  expect_equal(PLOT_CONTEXTS$EXPORT_PDF, "export_pdf")
  expect_equal(PLOT_CONTEXTS$EXPORT_PNG, "export_png")
  expect_equal(PLOT_CONTEXTS$EXPORT_PPTX, "export_pptx")
})

test_that("PLOT_CONTEXT_DIMENSIONS contains all required contexts", {
  expect_type(PLOT_CONTEXT_DIMENSIONS, "list")

  # All contexts should have dimension configs
  contexts <- c("analysis", "export_preview", "export_pdf", "export_png", "export_pptx")
  for (ctx in contexts) {
    expect_true(ctx %in% names(PLOT_CONTEXT_DIMENSIONS),
      info = sprintf("Context '%s' should exist in PLOT_CONTEXT_DIMENSIONS", ctx)
    )
  }
})

test_that("PLOT_CONTEXT_DIMENSIONS have correct structure", {
  # Analysis context
  analysis <- PLOT_CONTEXT_DIMENSIONS$analysis
  expect_true("width_px" %in% names(analysis))
  expect_true("height_px" %in% names(analysis))
  expect_true("dpi" %in% names(analysis))
  expect_true("unit" %in% names(analysis))
  expect_true("responsive" %in% names(analysis))
  expect_equal(analysis$unit, "px")
  expect_true(analysis$responsive)

  # Export preview context
  preview <- PLOT_CONTEXT_DIMENSIONS$export_preview
  expect_equal(preview$width_px, 800)
  expect_equal(preview$height_px, 450)
  expect_equal(preview$dpi, 96)
  expect_false(preview$responsive)

  # PDF context (mm units)
  pdf <- PLOT_CONTEXT_DIMENSIONS$export_pdf
  expect_true("width_mm" %in% names(pdf))
  expect_true("height_mm" %in% names(pdf))
  expect_equal(pdf$unit, "mm")
  expect_equal(pdf$dpi, 300)

  # PowerPoint context (inches)
  pptx <- PLOT_CONTEXT_DIMENSIONS$export_pptx
  expect_true("width_inches" %in% names(pptx))
  expect_true("height_inches" %in% names(pptx))
  expect_equal(pptx$unit, "in")
  expect_equal(pptx$dpi, 96)
})

# GET_CONTEXT_DIMENSIONS =======================================================

test_that("get_context_dimensions returns correct structure", {
  dims <- get_context_dimensions("analysis")

  expect_type(dims, "list")
  expect_true("width_px" %in% names(dims))
  expect_true("height_px" %in% names(dims))
  expect_true("dpi" %in% names(dims))
  expect_true("context" %in% names(dims))
  expect_true("responsive" %in% names(dims))

  expect_equal(dims$context, "analysis")
})

test_that("get_context_dimensions works for all contexts", {
  contexts <- c("analysis", "export_preview", "export_pdf", "export_png", "export_pptx")

  for (ctx in contexts) {
    dims <- get_context_dimensions(ctx)
    expect_type(dims, "list")
    expect_true(dims$width_px > 0)
    expect_true(dims$height_px > 0)
    expect_equal(dims$context, ctx)
  }
})

test_that("get_context_dimensions converts mm to pixels correctly", {
  # PDF context uses mm, should convert to pixels
  pdf_dims <- get_context_dimensions("export_pdf")

  # 200mm / 25.4 * 300 DPI = ~2362 pixels
  expected_width <- round((200 / 25.4) * 300)
  expected_height <- round((120 / 25.4) * 300)

  expect_equal(pdf_dims$width_px, expected_width)
  expect_equal(pdf_dims$height_px, expected_height)
  expect_equal(pdf_dims$dpi, 300)
})

test_that("get_context_dimensions converts inches to pixels correctly", {
  # PowerPoint context uses inches
  pptx_dims <- get_context_dimensions("export_pptx")

  # 9 inches * 96 DPI = 864 pixels
  # 6.5 inches * 96 DPI = 624 pixels
  expected_width <- round(9 * 96)
  expected_height <- round(6.5 * 96)

  expect_equal(pptx_dims$width_px, expected_width)
  expect_equal(pptx_dims$height_px, expected_height)
  expect_equal(pptx_dims$dpi, 96)
})

test_that("get_context_dimensions accepts overrides", {
  dims <- get_context_dimensions(
    "export_png",
    override_width = 1920,
    override_height = 1080,
    override_dpi = 150
  )

  expect_equal(dims$width_px, 1920)
  expect_equal(dims$height_px, 1080)
  expect_equal(dims$dpi, 150)
  expect_equal(dims$context, "export_png")
})

test_that("get_context_dimensions fails on invalid context", {
  expect_error(
    get_context_dimensions("invalid_context"),
    "Unknown plot context"
  )
})

# VALIDATE_PLOT_CONTEXT ========================================================

test_that("validate_plot_context accepts valid contexts", {
  valid_contexts <- c("analysis", "export_preview", "export_pdf", "export_png", "export_pptx")

  for (ctx in valid_contexts) {
    expect_true(validate_plot_context(ctx, stop_on_invalid = FALSE))
    expect_silent(validate_plot_context(ctx, stop_on_invalid = TRUE))
  }
})

test_that("validate_plot_context rejects invalid contexts", {
  expect_error(
    validate_plot_context("invalid", stop_on_invalid = TRUE),
    "Invalid plot context"
  )

  expect_false(validate_plot_context("invalid", stop_on_invalid = FALSE))
})

# CONTEXT-AWARE PLOT GENERATION ================================================

test_that("generateSPCPlot accepts plot_context parameter", {
  skip_if_not_installed("BFHcharts")

  # Create minimal test data
  test_data <- data.frame(
    x = 1:10,
    y = rnorm(10, mean = 100, sd = 10)
  )

  config <- list(
    x_col = "x",
    y_col = "y",
    n_col = NULL
  )

  # Test that plot generation works with context parameter
  # Note: BFHcharts may produce warnings about fonts - this is expected
  result <- generateSPCPlot(
    data = test_data,
    config = config,
    chart_type = "i",
    viewport_width = 800,
    viewport_height = 600,
    plot_context = "analysis"
  )

  expect_type(result, "list")
  expect_true("plot" %in% names(result))
})

test_that("generateSPCPlot works with all context types", {
  skip_if_not_installed("BFHcharts")

  test_data <- data.frame(
    x = 1:10,
    y = rnorm(10, mean = 100, sd = 10)
  )

  config <- list(
    x_col = "x",
    y_col = "y",
    n_col = NULL
  )

  contexts <- c("analysis", "export_preview", "export_pdf", "export_png", "export_pptx")

  for (ctx in contexts) {
    dims <- get_context_dimensions(ctx)

    result <- generateSPCPlot(
      data = test_data,
      config = config,
      chart_type = "i",
      viewport_width = dims$width_px,
      viewport_height = dims$height_px,
      plot_context = ctx
    )

    expect_type(result, "list")
    expect_true("plot" %in% names(result))
    expect_s3_class(result$plot, "ggplot")
  }
})

test_that("generateSPCPlot fails on invalid context", {
  skip_if_not_installed("BFHcharts")

  test_data <- data.frame(
    x = 1:10,
    y = rnorm(10, mean = 100, sd = 10)
  )

  config <- list(
    x_col = "x",
    y_col = "y",
    n_col = NULL
  )

  expect_error(
    generateSPCPlot(
      data = test_data,
      config = config,
      chart_type = "i",
      viewport_width = 800,
      viewport_height = 600,
      plot_context = "invalid_context"
    ),
    "Invalid plot context"
  )
})

# CACHE ISOLATION ==============================================================

test_that("Different contexts should have different cache keys", {
  # This is a conceptual test - actual cache key generation happens in mod_spc_chart_server.R
  # We verify that the context system supports cache isolation

  # Context identifiers should be unique
  contexts <- unlist(PLOT_CONTEXTS, use.names = FALSE)
  expect_equal(length(contexts), length(unique(contexts)))

  # Each context should produce different cache keys when combined with same data
  test_hash <- "abc123"
  cache_keys <- sapply(contexts, function(ctx) {
    digest::digest(list("spc_results", ctx, test_hash), algo = "xxhash64")
  })

  # All cache keys should be unique
  expect_equal(length(cache_keys), length(unique(cache_keys)))
})

test_that("Viewport dimensions affect cache keys", {
  # Different viewport dimensions should produce different cache keys
  test_hash <- "abc123"
  context <- "analysis"

  key1 <- digest::digest(
    list("spc_results", context, test_hash, 800, 600),
    algo = "xxhash64"
  )

  key2 <- digest::digest(
    list("spc_results", context, test_hash, 1024, 768),
    algo = "xxhash64"
  )

  expect_false(key1 == key2, info = "Different dimensions should produce different cache keys")
})

test_that("Same context and dimensions produce same cache key", {
  test_hash <- "abc123"
  context <- "analysis"
  width <- 800
  height <- 600

  key1 <- digest::digest(
    list("spc_results", context, test_hash, width, height),
    algo = "xxhash64"
  )

  key2 <- digest::digest(
    list("spc_results", context, test_hash, width, height),
    algo = "xxhash64"
  )

  expect_equal(key1, key2, info = "Identical inputs should produce identical cache keys")
})

# INTEGRATION TESTS ============================================================

test_that("Context system integrates with config system", {
  # Verify that context configs can be accessed alongside other configs
  expect_true(exists("PLOT_CONTEXTS"))
  expect_true(exists("PLOT_CONTEXT_DIMENSIONS"))

  # Should be able to combine context dimensions with UI config
  if (exists("UI_CONFIG")) {
    analysis_dims <- get_context_dimensions("analysis")
    expect_type(analysis_dims, "list")
  }
})

test_that("Export contexts have appropriate dimensions", {
  # Export preview should be reasonable size for screen display
  preview <- get_context_dimensions("export_preview")
  expect_true(preview$width_px >= 600)
  expect_true(preview$width_px <= 1200)
  expect_equal(preview$dpi, 96) # Screen DPI

  # PDF should be high resolution
  pdf <- get_context_dimensions("export_pdf")
  expect_equal(pdf$dpi, 300) # Print quality

  # PowerPoint should match slide dimensions
  pptx <- get_context_dimensions("export_pptx")
  expect_equal(pptx$dpi, 96) # Presentation DPI
  expect_true(pptx$width_px > pptx$height_px) # Landscape orientation
})

# REGRESSION TESTS =============================================================

test_that("Default plot_context parameter maintains backward compatibility", {
  skip_if_not_installed("BFHcharts")

  test_data <- data.frame(
    x = 1:10,
    y = rnorm(10, mean = 100, sd = 10)
  )

  config <- list(
    x_col = "x",
    y_col = "y",
    n_col = NULL
  )

  # Should work without specifying plot_context (defaults to "analysis")
  # Note: BFHcharts may produce warnings about fonts - this is expected
  result <- generateSPCPlot(
    data = test_data,
    config = config,
    chart_type = "i",
    viewport_width = 800,
    viewport_height = 600
    # plot_context not specified - should default to "analysis"
  )

  expect_type(result, "list")
  expect_true("plot" %in% names(result))
})
