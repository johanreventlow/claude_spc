test_that("chart_type_to_ui_type mapping is correct", {
  source("R/utils_y_axis_model.R")

  # Proportion charts → percent
  expect_equal(chart_type_to_ui_type("p"), "percent")
  expect_equal(chart_type_to_ui_type("pp"), "percent")

  # Rate charts → rate
  expect_equal(chart_type_to_ui_type("u"), "rate")
  expect_equal(chart_type_to_ui_type("up"), "rate")

  # Time between → time
  expect_equal(chart_type_to_ui_type("t"), "time")

  # Count/measurement/others → count
  expect_equal(chart_type_to_ui_type("i"), "count")
  expect_equal(chart_type_to_ui_type("mr"), "count")
  expect_equal(chart_type_to_ui_type("c"), "count")
  expect_equal(chart_type_to_ui_type("g"), "count")
  expect_equal(chart_type_to_ui_type("unknown_type"), "count")
})

test_that("run chart default y-axis with denominator presence", {
  source("R/utils_y_axis_model.R")

  # RUN + with N → percent
  expect_equal(decide_default_y_axis_ui_type("run", n_present = TRUE), "percent")

  # RUN + without N → count
  expect_equal(decide_default_y_axis_ui_type("run", n_present = FALSE), "count")
})

test_that("run chart denominator toggle semantics (unit-only)", {
  source("R/utils_y_axis_model.R")

  # Conceptual toggle: blank → selected N should imply percent
  expect_equal(decide_default_y_axis_ui_type("run", n_present = TRUE), "percent")

  # Conceptual toggle: selected N → blank should imply count
  expect_equal(decide_default_y_axis_ui_type("run", n_present = FALSE), "count")
})

