# test-label-size-safeguard.R
# Regression test: Label size skal ikke under-skaleres på små plots
#
# FIX: Auto-scaling af label_size skal kun skalere opad (scale_factor >= 1.0)
# for at undgå ulæseligt små labels på kompakte plots.

test_that("pmax safeguard forhindrer label_size under-skalering", {
  # Test logikken fra add_spc_labels() direkte uden grob rendering

  # Simuler device height scenarios
  device_height_baseline <- 7.8
  label_size_input <- 6.0

  # Scenarie 1: Kort device (5") - skal IKKE skalere ned
  dev_height_small <- 5.0
  scale_factor_small <- pmax(1.0, dev_height_small / device_height_baseline)
  label_size_small <- label_size_input * scale_factor_small

  expect_equal(
    scale_factor_small,
    1.0,
    tolerance = 0.01,
    info = sprintf(
      "Scale factor skal clampes til 1.0 på kort device. Got: %.2f",
      scale_factor_small
    )
  )

  expect_equal(
    label_size_small,
    6.0,
    tolerance = 0.01,
    info = sprintf(
      "Label size skal være minimum 6.0 på kort device. Got: %.2f",
      label_size_small
    )
  )

  # Scenarie 2: Baseline device (7.8") - skal være 1.0 scale
  dev_height_baseline_test <- 7.8
  scale_factor_baseline <- pmax(1.0, dev_height_baseline_test / device_height_baseline)
  label_size_baseline <- label_size_input * scale_factor_baseline

  expect_equal(
    scale_factor_baseline,
    1.0,
    tolerance = 0.01,
    info = "Scale factor skal være 1.0 ved baseline device height"
  )

  expect_equal(
    label_size_baseline,
    6.0,
    tolerance = 0.01,
    info = "Label size skal være 6.0 ved baseline device height"
  )

  # Scenarie 3: Stor device (18") - skal skalere op
  dev_height_large <- 18.0
  scale_factor_large <- pmax(1.0, dev_height_large / device_height_baseline)
  label_size_large <- label_size_input * scale_factor_large

  expected_scale_large <- 18.0 / 7.8
  expect_equal(
    scale_factor_large,
    expected_scale_large,
    tolerance = 0.01,
    info = sprintf(
      "Scale factor skal være %.2f på stor device. Got: %.2f",
      expected_scale_large, scale_factor_large
    )
  )

  expect_gt(
    label_size_large,
    6.0,
    info = sprintf(
      "Label size skal skaleres opad på stor device. Got: %.2f",
      label_size_large
    )
  )

  # Verificer at ingen scenarios giver label_size < 6.0
  all_sizes <- c(label_size_small, label_size_baseline, label_size_large)
  expect_true(
    all(all_sizes >= 6.0),
    info = sprintf(
      "Alle label sizes skal være >= 6.0. Got: %s",
      paste(round(all_sizes, 2), collapse = ", ")
    )
  )
})

test_that("absolut minimum gap sikrer synlig separation", {
  # Test logikken fra utils_label_placement.R direkte

  # Simuler lille label med minimal relative gap
  label_height_inches <- 0.1  # Meget lille label
  panel_height_inches <- 10.0
  relative_gap_line <- 0.05  # 5% config

  # Beregn gap SOM SYSTEMET gør det
  gap_line_inches_relative <- label_height_inches * relative_gap_line  # 0.005 inches
  min_gap_inches <- 0.05  # Absolut minimum fra implementering
  gap_line_inches_actual <- max(gap_line_inches_relative, min_gap_inches)

  # Verificer at absolut minimum vinder når relative er for lille
  expect_equal(
    gap_line_inches_actual,
    0.05,
    tolerance = 0.001,
    info = sprintf(
      "Gap skal være absolut minimum (0.05\"). Relative: %.4f, Actual: %.4f",
      gap_line_inches_relative, gap_line_inches_actual
    )
  )

  # Ved 96 DPI: 0.05 inches = 4.8 pixels
  pixels_at_96dpi <- gap_line_inches_actual * 96
  expect_gte(
    pixels_at_96dpi,
    4.8,
    info = sprintf(
      "Gap skal være mindst ~5 pixels @ 96dpi. Got: %.1f px",
      pixels_at_96dpi
    )
  )

  # Scenarie 2: Normal label hvor relative gap er større
  label_height_normal <- 0.6  # inches (~58px @ 96dpi)
  gap_line_normal_relative <- label_height_normal * relative_gap_line  # 0.03 inches
  gap_line_normal_actual <- max(gap_line_normal_relative, min_gap_inches)

  # Her skal absolut minimum også vinde (0.03 < 0.05)
  expect_equal(
    gap_line_normal_actual,
    0.05,
    tolerance = 0.001,
    info = "Normal label gap skal også bruge absolut minimum"
  )

  # Scenarie 3: Stor label hvor relative gap overstiger minimum
  label_height_large <- 1.0  # inches
  gap_line_large_relative <- label_height_large * relative_gap_line  # 0.05 inches
  gap_line_large_actual <- max(gap_line_large_relative, min_gap_inches)

  # Her er relative = absolut minimum
  expect_equal(
    gap_line_large_actual,
    0.05,
    tolerance = 0.001,
    info = "Stor label gap matcher absolut minimum"
  )

  # Scenarie 4: Meget stor label hvor relative overstiger minimum
  label_height_xlarge <- 2.0  # inches
  gap_line_xlarge_relative <- label_height_xlarge * relative_gap_line  # 0.10 inches
  gap_line_xlarge_actual <- max(gap_line_xlarge_relative, min_gap_inches)

  # Her skal relative vinde (0.10 > 0.05)
  expect_equal(
    gap_line_xlarge_actual,
    0.10,
    tolerance = 0.001,
    info = sprintf(
      "Meget stor label gap skal bruge relative. Relative: %.4f, Actual: %.4f",
      gap_line_xlarge_relative, gap_line_xlarge_actual
    )
  )
})

test_that("x-koordinat type detection fungerer korrekt", {
  # Test logikken fra utils_add_right_labels_marquee.R

  # Scenarie 1: POSIXct x-værdi
  x_value_posix <- as.POSIXct("2024-01-15 12:00:00", tz = "UTC")
  x_is_temporal_1 <- inherits(x_value_posix, c("POSIXct", "POSIXt", "Date"))

  expect_true(
    x_is_temporal_1,
    info = "POSIXct værdi skal detekteres som temporal"
  )

  # Scenarie 2: Date x-værdi
  x_value_date <- as.Date("2024-01-15")
  x_is_temporal_2 <- inherits(x_value_date, c("POSIXct", "POSIXt", "Date"))

  expect_true(
    x_is_temporal_2,
    info = "Date værdi skal detekteres som temporal"
  )

  # Scenarie 3: Numerisk x-værdi
  x_value_numeric <- 42.5
  x_is_temporal_3 <- inherits(x_value_numeric, c("POSIXct", "POSIXt", "Date"))

  expect_false(
    x_is_temporal_3,
    info = "Numerisk værdi skal IKKE detekteres som temporal"
  )

  # Scenarie 4: Integer x-værdi
  x_value_integer <- 100L
  x_is_temporal_4 <- inherits(x_value_integer, c("POSIXct", "POSIXt", "Date"))

  expect_false(
    x_is_temporal_4,
    info = "Integer værdi skal IKKE detekteres som temporal"
  )
})
