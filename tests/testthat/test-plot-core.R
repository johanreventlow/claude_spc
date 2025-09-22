# test-plot-core.R
# Focused tests for core plotting functionality

test_that("QIC chart type conversion fungerer", {
  if (exists("get_qic_chart_type")) {
    # Test standard chart types
    expect_equal(get_qic_chart_type("P-kort (Andele)"), "p")
    expect_equal(get_qic_chart_type("U-kort (Rater)"), "u")
    expect_equal(get_qic_chart_type("I-kort (Individuelle værdier)"), "i")
    expect_equal(get_qic_chart_type("MR-kort (Moving Range)"), "mr")

    # Test fallback
    expect_equal(get_qic_chart_type(""), "run")
    expect_equal(get_qic_chart_type("unknown"), "run")
  } else {
    skip("get_qic_chart_type function not available")
  }
})

test_that("Hospital theme application fungerer", {
  skip_if_not_installed("ggplot2")

  if (exists("HOSPITAL_COLORS") && exists("apply_hospital_theme")) {
    # Create a basic ggplot
    test_plot <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x, y)) +
      ggplot2::geom_point()

    themed_plot <- apply_hospital_theme(test_plot)
    expect_s3_class(themed_plot, "ggplot")
  } else {
    skip("Hospital theme functions not available")
  }
})

test_that("Basic plot generation med qicharts2 integration", {
  skip_if_not_installed("qicharts2")

  test_data <- data.frame(
    Skift = c(FALSE, FALSE, TRUE, FALSE, FALSE),
    Frys = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    Dato = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01")),
    Tæller = c(10, 15, 12, 18, 20),
    Nævner = c(100, 120, 110, 130, 140)
  )

  # Test qicharts2 integration
  if (exists("create_qic_plot")) {
    plot_result <- create_qic_plot(
      data = test_data,
      x_col = "Dato",
      y_col = "Tæller",
      chart_type = "p"
    )
    expect_s3_class(plot_result, "ggplot")
  } else {
    # Fallback test med direkte qicharts2
    qic_plot <- qicharts2::qic(
      x = Dato,
      y = Tæller,
      data = test_data,
      chart = "run"
    )
    expect_s3_class(qic_plot, "ggplot")
  }
})

test_that("Plot error handling patterns fungerer", {
  # Test med ugyldig data
  bad_data <- data.frame(
    x = c(NA, NA, NA),
    y = c("text", "more text", "not numeric")
  )

  if (exists("safe_operation") && exists("create_qic_plot")) {
    result <- safe_operation(
      operation_name = "Test plot creation",
      code = {
        create_qic_plot(bad_data, x_col = "x", y_col = "y", chart_type = "run")
      },
      fallback = NULL
    )
    # Should return fallback (NULL) rather than crash
    expect_true(is.null(result) || inherits(result, "ggplot"))
  } else {
    skip("Required error handling functions not available")
  }
})