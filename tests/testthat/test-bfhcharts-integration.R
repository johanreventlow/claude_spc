# test-bfhcharts-integration.R
# Minimal integration test for BFHcharts functionality
# Issue: BFHchart migration feasibility testing

test_that("BFHcharts can create a simple run chart", {
  # Skip if BFHcharts not available
  skip_if_not_installed("BFHcharts")
  skip_if_not_installed("qicharts2")

  # Create minimal test data
  test_data <- data.frame(
    Dato = seq.Date(from = as.Date("2024-01-01"), by = "week", length.out = 20),
    Tæller = c(15, 18, 20, 17, 19, 22, 21, 18, 20, 23,
               25, 24, 22, 26, 28, 27, 25, 29, 30, 28)
  )

  # Test create_spc_chart (high-level API)
  plot_result <- expect_no_error(
    BFHcharts::create_spc_chart(
      data = test_data,
      x = Dato,
      y = Tæller,
      chart_type = "run",
      y_axis_unit = "count",
      chart_title = "Test Run Chart"
    )
  )

  # Verify result is a ggplot object
  expect_s3_class(plot_result, "gg")
  expect_s3_class(plot_result, "ggplot")
})

test_that("BFHcharts can create a run chart with phases", {
  skip_if_not_installed("BFHcharts")
  skip_if_not_installed("qicharts2")

  # Create test data with phase split
  test_data <- data.frame(
    Dato = seq.Date(from = as.Date("2024-01-01"), by = "week", length.out = 20),
    Tæller = c(15, 18, 20, 17, 19, 22, 21, 18, 20, 23,
               25, 24, 22, 26, 28, 27, 25, 29, 30, 28)
  )

  # Test with phase split at row 10
  plot_result <- expect_no_error(
    BFHcharts::create_spc_chart(
      data = test_data,
      x = Dato,
      y = Tæller,
      chart_type = "run",
      y_axis_unit = "count",
      part = c(10),
      chart_title = "Test Run Chart with Phases"
    )
  )

  expect_s3_class(plot_result, "gg")
})

test_that("BFHcharts can create P-chart with denominator", {
  skip_if_not_installed("BFHcharts")
  skip_if_not_installed("qicharts2")

  # Create test data with numerator and denominator
  test_data <- data.frame(
    Dato = seq.Date(from = as.Date("2024-01-01"), by = "week", length.out = 20),
    Tæller = c(5, 8, 10, 7, 9, 12, 11, 8, 10, 13,
               15, 14, 12, 16, 18, 17, 15, 19, 20, 18),
    Nævner = c(50, 55, 60, 52, 58, 62, 61, 58, 60, 63,
               65, 64, 62, 66, 68, 67, 65, 69, 70, 68)
  )

  # Test P-chart
  plot_result <- expect_no_error(
    BFHcharts::create_spc_chart(
      data = test_data,
      x = Dato,
      y = Tæller,
      n = Nævner,
      chart_type = "p",
      y_axis_unit = "percent",
      chart_title = "Test P-Chart"
    )
  )

  expect_s3_class(plot_result, "gg")
})

test_that("BFHcharts handles target value and text", {
  skip_if_not_installed("BFHcharts")
  skip_if_not_installed("qicharts2")

  test_data <- data.frame(
    Dato = seq.Date(from = as.Date("2024-01-01"), by = "week", length.out = 20),
    Tæller = c(15, 18, 20, 17, 19, 22, 21, 18, 20, 23,
               25, 24, 22, 26, 28, 27, 25, 29, 30, 28)
  )

  # Test with target
  plot_result <- expect_no_error(
    BFHcharts::create_spc_chart(
      data = test_data,
      x = Dato,
      y = Tæller,
      chart_type = "run",
      y_axis_unit = "count",
      target_value = 25,
      target_text = "Mål: 25",
      chart_title = "Test with Target"
    )
  )

  expect_s3_class(plot_result, "gg")
})

test_that("BFHcharts two-stage workflow with qicharts2", {
  skip_if_not_installed("BFHcharts")
  skip_if_not_installed("qicharts2")

  test_data <- data.frame(
    Dato = seq.Date(from = as.Date("2024-01-01"), by = "week", length.out = 20),
    Tæller = c(15, 18, 20, 17, 19, 22, 21, 18, 20, 23,
               25, 24, 22, 26, 28, 27, 25, 29, 30, 28)
  )

  # Stage 1: Get qic_data from qicharts2
  qic_result <- expect_no_error(
    qicharts2::qic(
      x = Dato,
      y = Tæller,
      data = test_data,
      chart = "run",
      return.data = TRUE
    )
  )

  expect_true(is.data.frame(qic_result))
  expect_true("x" %in% names(qic_result))
  expect_true("y" %in% names(qic_result))
  expect_true("cl" %in% names(qic_result))

  # Stage 2: Create plot with BFHcharts
  plot_config <- BFHcharts::spc_plot_config(
    chart_title = "Two-Stage Test",
    y_axis_unit = "count"
  )

  plot_result <- expect_no_error(
    BFHcharts::bfh_spc_plot(
      qic_data = qic_result,
      plot_config = plot_config
    )
  )

  expect_s3_class(plot_result, "gg")
})
