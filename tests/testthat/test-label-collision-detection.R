# Test: Label Collision Detection & Robust Label Placement
#
# Verificerer at CL/Target labels ikke overlapper hinanden eller linjer
# gennem collision detection helpers.

test_that("compute_label_boxes_data_units returnerer korrekte dimensioner", {
  skip_if_not(require("ggplot2", quietly = TRUE))
  skip_if_not(require("ggrepel", quietly = TRUE))

  # Setup test data
  test_data <- data.frame(
    x = as.Date("2024-01-01") + 0:10,
    y = c(50, 55, 48, 52, 54, 49, 51, 53, 50, 52, 51),
    cl = 51,
    target = 50,
    part = 1
  )

  # Create basic plot
  plot <- ggplot2::ggplot(test_data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line() +
    ggplot2::geom_line(ggplot2::aes(y = cl), color = "blue")

  # Create label data
  label_data <- data.frame(
    x_numeric = as.numeric(test_data$x[nrow(test_data)]),
    y_header = c(51, 50),
    header_label = c("NUV. NIVEAU", "MÅL"),
    value_label = c("51", "50"),
    label = c("{.12 **NUV. NIVEAU**}\n{.36 **51**}", "{.12 **MÅL**}\n{.36 **50**}"),
    text_color = c("#009CE8", "#565656"),
    stringsAsFactors = FALSE
  )

  # Mock style
  style <- marquee::classic_style()

  # Compute boxes
  boxes <- compute_label_boxes_data_units(
    plot = plot,
    label_data = label_data,
    style = style,
    size = 6,
    lineheight = 0.9,
    family = "sans"
  )

  # Assertions
  expect_s3_class(boxes, "data.frame")
  expect_equal(nrow(boxes), 2)
  expect_true(all(c("id", "xmin", "xmax", "ymin", "ymax") %in% names(boxes)))
  expect_true(all(boxes$xmax > boxes$xmin))
  expect_true(all(boxes$ymax > boxes$ymin))
})

test_that("buffer_lines_as_obstacles genererer obstacles korrekt", {
  test_data <- data.frame(
    x = 1:10,
    y = c(50, 55, 48, 52, 54, 49, 51, 53, 50, 52),
    cl = 51,
    target = 50,
    part = 1
  )

  obstacles <- buffer_lines_as_obstacles(test_data, pad_line = 1)

  expect_type(obstacles, "list")
  expect_true("cl" %in% names(obstacles))
  expect_true("target" %in% names(obstacles))
  expect_s3_class(obstacles$cl, "data.frame")
  expect_s3_class(obstacles$target, "data.frame")

  # CL obstacles skal have linje-segmenter
  if (nrow(obstacles$cl) > 0) {
    expect_true(all(c("xmin", "xmax", "ymin", "ymax") %in% names(obstacles$cl)))
    expect_true(all(obstacles$cl$xmax >= obstacles$cl$xmin))
    expect_true(all(obstacles$cl$ymax > obstacles$cl$ymin))
  }

  # Target obstacles skal have linje-segmenter
  if (nrow(obstacles$target) > 0) {
    expect_true(all(c("xmin", "xmax", "ymin", "ymax") %in% names(obstacles$target)))
    expect_true(all(obstacles$target$xmax >= obstacles$target$xmin))
    expect_true(all(obstacles$target$ymax > obstacles$target$ymin))
  }
})

test_that("derive_box_padding returnerer rimelig værdi", {
  boxes <- data.frame(
    id = 1:2,
    xmin = c(1, 1),
    xmax = c(2, 2),
    ymin = c(50, 48),
    ymax = c(52, 50)
  )

  padding <- derive_box_padding(boxes)

  expect_type(padding, "double")
  expect_true(padding >= 0.2)
  expect_true(padding <= 1.0)
})

test_that("assert_no_label_overlaps detekterer overlap", {
  # Non-overlapping boxes
  boxes_ok <- data.frame(
    id = 1:2,
    xmin = c(1, 5),
    xmax = c(2, 6),
    ymin = c(50, 48),
    ymax = c(52, 50)
  )

  expect_true(assert_no_label_overlaps(boxes_ok))

  # Overlapping boxes
  boxes_overlap <- data.frame(
    id = 1:2,
    xmin = c(1, 1.5),
    xmax = c(2, 2.5),
    ymin = c(50, 50.5),
    ymax = c(52, 52.5)
  )

  expect_error(
    assert_no_label_overlaps(boxes_overlap),
    "Label overlap detected"
  )
})

test_that("assert_no_overlap_with_lines detekterer linje-overlap", {
  boxes <- data.frame(
    id = 1,
    xmin = 1,
    xmax = 2,
    ymin = 50,
    ymax = 52
  )

  # Non-overlapping obstacles
  cl_obs_ok <- data.frame(
    xmin = 1,
    xmax = 10,
    ymin = 48,
    ymax = 49
  )

  target_obs_ok <- data.frame(
    xmin = 1,
    xmax = 10,
    ymin = 45,
    ymax = 46
  )

  expect_true(assert_no_overlap_with_lines(boxes, cl_obs_ok, target_obs_ok))

  # Overlapping obstacle
  cl_obs_overlap <- data.frame(
    xmin = 1,
    xmax = 10,
    ymin = 51,
    ymax = 52
  )

  expect_error(
    assert_no_overlap_with_lines(boxes, cl_obs_overlap, target_obs_ok),
    "overlapper centerline obstacle"
  )
})

test_that("assert_inside_panel detekterer panel-overløb", {
  boxes <- data.frame(
    id = 1,
    xmin = 1,
    xmax = 2,
    ymin = 50,
    ymax = 52
  )

  # Inside panel
  expect_true(assert_inside_panel(boxes, x_range = c(0, 10), y_range = c(40, 60)))

  # Outside panel (x)
  expect_error(
    assert_inside_panel(boxes, x_range = c(3, 10), y_range = c(40, 60)),
    "uden for panel bounds"
  )

  # Outside panel (y)
  expect_error(
    assert_inside_panel(boxes, x_range = c(0, 10), y_range = c(53, 60)),
    "uden for panel bounds"
  )
})

test_that("Determinisme: seed=1 giver identiske label positioner", {
  skip_if_not(require("ggplot2", quietly = TRUE))
  skip_if_not(require("ggrepel", quietly = TRUE))
  skip_if_not("geom_marquee_repel" %in% getNamespaceExports("ggrepel"))

  # Setup test data
  test_data <- data.frame(
    Dato = as.Date("2024-01-01") + 0:20,
    Tæller = c(45, 48, 50, 52, 51, 49, 53, 50, 48, 51, 52, 54, 50, 49, 51, 53, 52, 50, 51, 49, 50),
    Nævner = rep(100, 21)
  )

  config <- list(
    x_col = "Dato",
    y_col = "Tæller",
    n_col = "Nævner"
  )

  set.seed(1)
  result1 <- generateSPCPlot(
    data = test_data,
    config = config,
    chart_type = "p",
    target_value = 0.5,
    y_axis_unit = "percent"
  )

  set.seed(1)
  result2 <- generateSPCPlot(
    data = test_data,
    config = config,
    chart_type = "p",
    target_value = 0.5,
    y_axis_unit = "percent"
  )

  # Extract label data from ggplot build
  built1 <- ggplot2::ggplot_build(result1$plot)
  built2 <- ggplot2::ggplot_build(result2$plot)

  # Find label layers (geom_marquee_repel)
  label_layers1 <- which(sapply(built1$data, function(x) "label" %in% names(x)))
  label_layers2 <- which(sapply(built2$data, function(x) "label" %in% names(x)))

  if (length(label_layers1) > 0 && length(label_layers2) > 0) {
    labels1 <- built1$data[[label_layers1[1]]]
    labels2 <- built2$data[[label_layers2[1]]]

    # Compare positions
    expect_equal(labels1$x, labels2$x, tolerance = 1e-6)
    expect_equal(labels1$y, labels2$y, tolerance = 1e-6)
  } else {
    skip("Ingen label-lag fundet i plot")
  }
})

test_that("Integration: Full plot med collision detection virker", {
  skip_if_not(require("ggplot2", quietly = TRUE))
  skip_if_not(require("ggrepel", quietly = TRUE))
  skip_if_not("geom_marquee_repel" %in% getNamespaceExports("ggrepel"))

  # Setup realistic clinical data
  test_data <- data.frame(
    Dato = as.Date("2024-01-01") + seq(0, 365, by = 30),
    Værdi = c(250, 265, 248, 270, 255, 262, 258, 252, 268, 260, 255, 263, 258),
    stringsAsFactors = FALSE
  )

  config <- list(
    x_col = "Dato",
    y_col = "Værdi",
    n_col = NULL
  )

  # Generate plot med target (tæt på centerline for at teste collision)
  result <- generateSPCPlot(
    data = test_data,
    config = config,
    chart_type = "i",
    target_value = 260,
    y_axis_unit = "count"
  )

  expect_s3_class(result$plot, "ggplot")
  expect_s3_class(result$qic_data, "data.frame")

  # Verify plot renders uden fejl
  expect_silent({
    built <- ggplot2::ggplot_build(result$plot)
  })
})
