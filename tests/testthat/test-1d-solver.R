test_that("1D solver undgår begge linjer og indbyrdes overlap", {
  skip_if_not(exists("place_labels_right_1d", mode = "function"))

  # To labels tæt på hinanden: CL=91, Target=90
  y_desired <- c(91, 90)
  half_heights <- c(0.25, 0.25)   # 0.5 fuld højde
  pad_line <- 0.5                  # forbudsbånd ±0.5 om hver linje

  # Forbudsbånd: begge linjer for begge labels
  intervals <- list(
    list(c(91 - pad_line, 91 + pad_line), c(90 - pad_line, 90 + pad_line)),
    list(c(91 - pad_line, 91 + pad_line), c(90 - pad_line, 90 + pad_line))
  )

  min_gap <- 0.1
  y_bounds <- c(80, 120)

  y_placed <- place_labels_right_1d(
    y_desired = y_desired,
    half_heights = half_heights,
    forbid_intervals = intervals,
    min_gap = min_gap,
    y_bounds = y_bounds
  )

  # Helper checks
  no_overlap_with_line <- function(y, hh, y_line, pad) all(abs(y - y_line) >= (hh + pad) - 1e-9)
  no_overlap_between_labels <- function(y, hh, gap) {
    ord <- order(y); y <- y[ord]; hh <- hh[ord]
    (y[2] - hh[2]) >= (y[1] + hh[1] + gap - 1e-9)
  }

  expect_true(no_overlap_with_line(y_placed[1], half_heights[1], 91, pad_line))
  expect_true(no_overlap_with_line(y_placed[1], half_heights[1], 90, pad_line))
  expect_true(no_overlap_with_line(y_placed[2], half_heights[2], 91, pad_line))
  expect_true(no_overlap_with_line(y_placed[2], half_heights[2], 90, pad_line))
  expect_true(no_overlap_between_labels(y_placed, half_heights, min_gap))
})

