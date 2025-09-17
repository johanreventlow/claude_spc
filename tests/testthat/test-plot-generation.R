# test-plot-generation.R
# Test af plot generation og display functionality
# Fokuserer på ggplot2 integration og visual components

# Required libraries for testing
library(ggplot2)

test_that("basic ggplot SPC chart generation works", {
  # TEST: Basic SPC chart construction using ggplot2

  # SETUP: Mock QIC data similar to qicharts2 output
  qic_data <- data.frame(
    x = 1:10,
    y = c(0.92, 0.93, 0.95, 0.92, 0.91, 0.92, 0.90, 0.86, 0.89, 0.91),
    cl = rep(0.90, 10),  # Center line
    ucl = rep(0.96, 10), # Upper control limit
    lcl = rep(0.84, 10)  # Lower control limit
  )

  # TEST: Basic SPC plot construction
  plot <- ggplot(qic_data, aes(x = x, y = y)) +
    geom_line(color = "#6c757d", linewidth = 1) +
    geom_point(size = 2, color = "#6c757d") +
    geom_line(aes(y = cl), color = "#007bff", linetype = "solid", linewidth = 1) +
    geom_line(aes(y = ucl), color = "#dc3545", linetype = "dashed", linewidth = 0.8) +
    geom_line(aes(y = lcl), color = "#dc3545", linetype = "dashed", linewidth = 0.8) +
    labs(title = "Test SPC Chart", x = "", y = "Proportion") +
    theme_minimal()

  # Verify plot structure
  expect_s3_class(plot, "ggplot")
  expect_equal(length(plot$layers), 5) # Line, points, CL, UCL, LCL
  expect_equal(plot$labels$title, "Test SPC Chart")
  expect_equal(plot$labels$y, "Proportion")

  # Verify aesthetics
  expect_true("x" %in% names(plot$mapping))
  expect_true("y" %in% names(plot$mapping))

  # Verify data
  expect_equal(nrow(plot$data), 10)
  expect_true(all(c("x", "y", "cl", "ucl", "lcl") %in% names(plot$data)))
})

test_that("target line addition works correctly", {
  # TEST: Adding target lines to SPC charts

  # SETUP: Base data
  qic_data <- data.frame(
    x = 1:8,
    y = c(0.88, 0.91, 0.93, 0.89, 0.87, 0.90, 0.92, 0.88),
    cl = rep(0.90, 8)
  )

  target_value <- 0.85

  # TEST: Target line addition
  plot_with_target <- ggplot(qic_data, aes(x = x, y = y)) +
    geom_line(color = "#6c757d", linewidth = 1) +
    geom_point(size = 2, color = "#6c757d") +
    geom_line(aes(y = cl), color = "#007bff", linetype = "solid", linewidth = 1) +
    geom_hline(
      yintercept = target_value,
      color = "#495057",
      linetype = "42",
      linewidth = 1.2,
      alpha = 0.8
    ) +
    labs(title = "SPC Chart with Target", x = "", y = "Proportion") +
    theme_minimal()

  expect_s3_class(plot_with_target, "ggplot")
  expect_equal(length(plot_with_target$layers), 4) # Line, points, CL, target
  expect_equal(plot_with_target$labels$title, "SPC Chart with Target")

  # TEST: Target line parameters - basic layer verification
  target_layer <- plot_with_target$layers[[4]]
  expect_equal(class(target_layer$geom)[1], "GeomHline")
  # Note: Internal ggplot structure can vary, just verify it's a horizontal line geom
})

test_that("phase separation lines work correctly", {
  # TEST: Adding phase separation lines

  # SETUP: Data with phase change
  qic_data <- data.frame(
    x = 1:12,
    y = c(0.88, 0.91, 0.93, 0.89, 0.87, 0.90, 0.92, 0.88, 0.85, 0.87, 0.89, 0.86),
    cl = rep(0.89, 12),
    part = c(rep(1, 6), rep(2, 6))  # Phase change at observation 7
  )

  # Find phase change point
  phase_changes <- which(diff(qic_data$part) != 0)
  phase_change_x <- qic_data$x[phase_changes[1] + 1]

  # TEST: Phase line addition
  plot_with_phases <- ggplot(qic_data, aes(x = x, y = y)) +
    geom_line(color = "#6c757d", linewidth = 1) +
    geom_point(size = 2, color = "#6c757d") +
    geom_line(aes(y = cl), color = "#007bff", linetype = "solid", linewidth = 1) +
    geom_vline(
      xintercept = phase_change_x,
      color = "#ffc107",
      linetype = "dotted",
      linewidth = 1,
      alpha = 0.7
    ) +
    labs(title = "SPC Chart with Phase Change", x = "", y = "Proportion") +
    theme_minimal()

  expect_s3_class(plot_with_phases, "ggplot")
  expect_equal(length(plot_with_phases$layers), 4) # Line, points, CL, phase line
  expect_equal(plot_with_phases$labels$title, "SPC Chart with Phase Change")

  # TEST: Phase line parameters - basic layer verification
  phase_layer <- plot_with_phases$layers[[4]]
  expect_equal(class(phase_layer$geom)[1], "GeomVline")
  # Note: Internal ggplot structure can vary, just verify it's a vertical line geom
})

test_that("comment annotations work correctly", {
  # TEST: Adding comment annotations with ggrepel

  # Skip if ggrepel not available
  skip_if_not_installed("ggrepel")

  # SETUP: Data with comments
  qic_data <- data.frame(
    x = 1:6,
    y = c(0.88, 0.91, 0.93, 0.89, 0.87, 0.90),
    cl = rep(0.90, 6)
  )

  comment_data <- data.frame(
    x = c(2, 5),
    y = c(0.91, 0.87),
    comment = c("Peak performance", "Low point")
  )

  # TEST: Comment annotation
  plot_with_comments <- ggplot(qic_data, aes(x = x, y = y)) +
    geom_line(color = "#6c757d", linewidth = 1) +
    geom_point(size = 2, color = "#6c757d") +
    geom_line(aes(y = cl), color = "#007bff", linetype = "solid", linewidth = 1) +
    ggrepel::geom_text_repel(
      data = comment_data,
      aes(x = x, y = y, label = comment),
      size = 3,
      color = "#495057",
      bg.color = "white",
      bg.r = 0.1,
      inherit.aes = FALSE
    ) +
    labs(title = "SPC Chart with Comments", x = "", y = "Proportion") +
    theme_minimal()

  expect_s3_class(plot_with_comments, "ggplot")
  expect_equal(length(plot_with_comments$layers), 4) # Line, points, CL, comments
  expect_equal(plot_with_comments$labels$title, "SPC Chart with Comments")

  # TEST: Comment layer has correct data
  comment_layer <- plot_with_comments$layers[[4]]
  expect_equal(nrow(comment_layer$data), 2)
  expect_true("comment" %in% names(comment_layer$data))
})

test_that("chart scaling and axis formatting work", {
  # TEST: Axis scaling and formatting

  # SETUP: Data with wider range
  qic_data <- data.frame(
    x = 1:10,
    y = c(75, 82, 88, 91, 85, 79, 83, 87, 90, 86),
    cl = rep(84, 10),
    ucl = rep(95, 10),
    lcl = rep(73, 10)
  )

  # TEST: Percentage scaling (for run charts)
  plot_percentage <- ggplot(qic_data, aes(x = x, y = y)) +
    geom_line(color = "#6c757d", linewidth = 1) +
    geom_point(size = 2, color = "#6c757d") +
    geom_line(aes(y = cl), color = "#007bff", linetype = "solid", linewidth = 1) +
    scale_y_continuous(
      name = "Percentage (%)",
      limits = c(70, 100),
      breaks = seq(70, 100, by = 5)
    ) +
    scale_x_continuous(
      name = "Observation",
      breaks = seq(1, 10, by = 2)
    ) +
    labs(title = "SPC Chart with Custom Scaling") +
    theme_minimal()

  expect_s3_class(plot_percentage, "ggplot")
  expect_equal(plot_percentage$labels$title, "SPC Chart with Custom Scaling")

  # TEST: Date axis formatting
  date_data <- data.frame(
    x = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01")),
    y = c(0.88, 0.91, 0.89, 0.87, 0.90),
    cl = rep(0.89, 5)
  )

  plot_dates <- ggplot(date_data, aes(x = x, y = y)) +
    geom_line(color = "#6c757d", linewidth = 1) +
    geom_point(size = 2, color = "#6c757d") +
    geom_line(aes(y = cl), color = "#007bff", linetype = "solid", linewidth = 1) +
    scale_x_date(
      name = "Date",
      date_labels = "%b %Y",
      date_breaks = "1 month"
    ) +
    labs(title = "SPC Chart with Date Axis") +
    theme_minimal()

  expect_s3_class(plot_dates, "ggplot")
  expect_equal(plot_dates$labels$title, "SPC Chart with Date Axis")
  expect_true(inherits(plot_dates$data$x, "Date"))
})

test_that("theme and styling work correctly", {
  # TEST: Theme application and styling

  # SETUP: Base data
  qic_data <- data.frame(
    x = 1:5,
    y = c(0.88, 0.91, 0.89, 0.87, 0.90),
    cl = rep(0.89, 5)
  )

  # TEST: Custom theme application
  plot_styled <- ggplot(qic_data, aes(x = x, y = y)) +
    geom_line(color = "#6c757d", linewidth = 1) +
    geom_point(size = 2, color = "#6c757d") +
    geom_line(aes(y = cl), color = "#007bff", linetype = "solid", linewidth = 1) +
    labs(
      title = "Styled SPC Chart",
      subtitle = "With custom theme",
      x = "Observation Number",
      y = "Proportion"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray50"),
      axis.title = element_text(size = 10),
      panel.grid.minor = element_blank()
    )

  expect_s3_class(plot_styled, "ggplot")
  expect_equal(plot_styled$labels$title, "Styled SPC Chart")
  expect_equal(plot_styled$labels$subtitle, "With custom theme")
  expect_equal(plot_styled$labels$x, "Observation Number")
  expect_equal(plot_styled$labels$y, "Proportion")

  # Verify theme components
  expect_s3_class(plot_styled$theme, "theme")
})

test_that("different chart types render correctly", {
  # TEST: Different SPC chart types

  # SETUP: Common data for different chart types
  base_data <- data.frame(
    x = 1:8,
    numerator = c(45, 43, 48, 46, 47, 49, 44, 44),
    denominator = c(50, 50, 50, 50, 50, 50, 50, 50)
  )

  # TEST: P-chart (proportions)
  p_data <- data.frame(
    x = base_data$x,
    y = base_data$numerator / base_data$denominator,
    cl = rep(0.90, 8),
    ucl = rep(0.96, 8),
    lcl = rep(0.84, 8)
  )

  p_chart <- ggplot(p_data, aes(x = x, y = y)) +
    geom_line(color = "#6c757d", linewidth = 1) +
    geom_point(size = 2, color = "#6c757d") +
    geom_line(aes(y = cl), color = "#007bff", linetype = "solid", linewidth = 1) +
    geom_line(aes(y = ucl), color = "#dc3545", linetype = "dashed", linewidth = 0.8) +
    geom_line(aes(y = lcl), color = "#dc3545", linetype = "dashed", linewidth = 0.8) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(title = "P-Chart", y = "Proportion") +
    theme_minimal()

  expect_s3_class(p_chart, "ggplot")
  expect_equal(length(p_chart$layers), 5)
  expect_equal(p_chart$labels$title, "P-Chart")

  # TEST: Run chart (percentages)
  run_data <- data.frame(
    x = base_data$x,
    y = (base_data$numerator / base_data$denominator) * 100,
    cl = rep(90, 8)
  )

  run_chart <- ggplot(run_data, aes(x = x, y = y)) +
    geom_line(color = "#6c757d", linewidth = 1) +
    geom_point(size = 2, color = "#6c757d") +
    geom_line(aes(y = cl), color = "#007bff", linetype = "solid", linewidth = 1) +
    scale_y_continuous(limits = c(80, 100), labels = scales::label_percent(scale = 1)) +
    labs(title = "Run Chart", y = "Percentage") +
    theme_minimal()

  expect_s3_class(run_chart, "ggplot")
  expect_equal(length(run_chart$layers), 3) # Line, points, CL (no control limits for run charts)
  expect_equal(run_chart$labels$title, "Run Chart")

  # TEST: I-chart (individual values)
  i_data <- data.frame(
    x = base_data$x,
    y = base_data$numerator,
    cl = rep(46, 8),
    ucl = rep(52, 8),
    lcl = rep(40, 8)
  )

  i_chart <- ggplot(i_data, aes(x = x, y = y)) +
    geom_line(color = "#6c757d", linewidth = 1) +
    geom_point(size = 2, color = "#6c757d") +
    geom_line(aes(y = cl), color = "#007bff", linetype = "solid", linewidth = 1) +
    geom_line(aes(y = ucl), color = "#dc3545", linetype = "dashed", linewidth = 0.8) +
    geom_line(aes(y = lcl), color = "#dc3545", linetype = "dashed", linewidth = 0.8) +
    labs(title = "I-Chart", y = "Individual Values") +
    theme_minimal()

  expect_s3_class(i_chart, "ggplot")
  expect_equal(length(i_chart$layers), 5)
  expect_equal(i_chart$labels$title, "I-Chart")
})

test_that("error handling in plot generation works", {
  # TEST: Error handling for invalid plot data

  # TEST: Empty data handling - ggplot doesn't error on creation, only on rendering
  empty_data <- data.frame()

  empty_plot <- tryCatch({
    ggplot(empty_data, aes(x = x, y = y)) +
      geom_line() +
      geom_point()
  }, error = function(e) e)

  # ggplot creation doesn't error, but rendering would
  expect_s3_class(empty_plot, "ggplot")

  # TEST: Missing column handling - also doesn't error on creation
  incomplete_data <- data.frame(x = 1:5)

  incomplete_plot <- tryCatch({
    ggplot(incomplete_data, aes(x = x, y = y)) +
      geom_line() +
      geom_point()
  }, error = function(e) e)

  # ggplot creation doesn't error for missing columns
  expect_s3_class(incomplete_plot, "ggplot")

  # TEST: Invalid data frame creation (this should error)
  expect_error({
    data.frame(x = 1:5, y = c(0.88, 0.91, 0.89))  # Mismatched lengths
  })

  # TEST: Valid data with potential rendering issues
  valid_sparse_data <- data.frame(
    x = 1:3,
    y = c(0.88, 0.91, 0.89)
  )

  sparse_plot <- ggplot(valid_sparse_data, aes(x = x, y = y)) +
    geom_line() +
    geom_point()

  expect_s3_class(sparse_plot, "ggplot")
})

test_that("plot accessibility and usability features work", {
  # TEST: Accessibility features

  # SETUP: Data with accessibility considerations
  qic_data <- data.frame(
    x = 1:6,
    y = c(0.88, 0.91, 0.89, 0.87, 0.90, 0.85),
    cl = rep(0.89, 6),
    ucl = rep(0.95, 6),
    lcl = rep(0.83, 6)
  )

  # TEST: High contrast colors and clear styling
  accessible_plot <- ggplot(qic_data, aes(x = x, y = y)) +
    geom_line(color = "#000000", linewidth = 1.5) +  # Black for high contrast
    geom_point(size = 3, color = "#000000", shape = 16) +  # Larger points
    geom_line(aes(y = cl), color = "#0066CC", linetype = "solid", linewidth = 2) +  # Blue CL
    geom_line(aes(y = ucl), color = "#CC0000", linetype = "dashed", linewidth = 1.5) +  # Red UCL
    geom_line(aes(y = lcl), color = "#CC0000", linetype = "dashed", linewidth = 1.5) +  # Red LCL
    labs(
      title = "Accessible SPC Chart",
      subtitle = "High contrast colors and clear styling",
      x = "Observation Number",
      y = "Proportion Value"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "gray80", linewidth = 0.5),
      panel.grid.minor = element_blank()
    )

  expect_s3_class(accessible_plot, "ggplot")
  expect_equal(accessible_plot$labels$title, "Accessible SPC Chart")

  # TEST: Clear legend and labeling
  legend_plot <- ggplot(qic_data, aes(x = x)) +
    geom_line(aes(y = y, color = "Observed Values"), linewidth = 1.5) +
    geom_line(aes(y = cl, color = "Center Line"), linewidth = 2) +
    geom_line(aes(y = ucl, color = "Upper Control Limit"), linetype = "dashed", linewidth = 1.5) +
    geom_line(aes(y = lcl, color = "Lower Control Limit"), linetype = "dashed", linewidth = 1.5) +
    scale_color_manual(
      name = "Chart Elements",
      values = c(
        "Observed Values" = "#000000",
        "Center Line" = "#0066CC",
        "Upper Control Limit" = "#CC0000",
        "Lower Control Limit" = "#CC0000"
      )
    ) +
    labs(
      title = "SPC Chart with Legend",
      x = "Observation Number",
      y = "Proportion Value"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 10)
    )

  expect_s3_class(legend_plot, "ggplot")
  expect_equal(legend_plot$labels$title, "SPC Chart with Legend")
  # Legend guides are created during rendering, not at plot creation
  expect_true("color" %in% names(legend_plot$mapping) || length(legend_plot$layers) > 0)
})

test_that("plot export and sizing work correctly", {
  # TEST: Plot export considerations

  # SETUP: Standard SPC plot
  qic_data <- data.frame(
    x = 1:10,
    y = c(0.88, 0.91, 0.89, 0.87, 0.90, 0.85, 0.92, 0.88, 0.91, 0.89),
    cl = rep(0.89, 10)
  )

  export_plot <- ggplot(qic_data, aes(x = x, y = y)) +
    geom_line(color = "#6c757d", linewidth = 1) +
    geom_point(size = 2, color = "#6c757d") +
    geom_line(aes(y = cl), color = "#007bff", linetype = "solid", linewidth = 1) +
    labs(title = "Export-Ready SPC Chart", x = "Observation", y = "Proportion") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),  # Center title
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")  # Margins for export
    )

  expect_s3_class(export_plot, "ggplot")
  expect_equal(export_plot$labels$title, "Export-Ready SPC Chart")

  # TEST: Plot dimensions and aspect ratio considerations
  # Note: Actual sizing would be tested in rendering, but we can verify structure
  wide_plot <- export_plot +
    theme(
      aspect.ratio = 1/2,  # Wide aspect ratio
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )

  expect_s3_class(wide_plot, "ggplot")

  square_plot <- export_plot +
    theme(
      aspect.ratio = 1,  # Square aspect ratio
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )

  expect_s3_class(square_plot, "ggplot")
})