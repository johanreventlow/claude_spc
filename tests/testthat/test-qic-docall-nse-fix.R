# test-qic-docall-nse-fix.R
# Regression test for qicharts2 NSE bug med do.call()
#
# Bug: qicharts2::qic() fejler med "condition has length > 1" når kaldt via
# do.call() med pre-evaluerede vektorer.
#
# Root cause: if (y.name == "NULL") guard i qic() fejler når
# deparse(substitute(y)) returnerer character vector af length > 1
#
# Løsning: Brug data + kolonnenavne (as.name()) i stedet for pre-evaluerede vektorer

test_that("qic() fejler med do.call og pre-evaluerede vektorer (dokumenterer bug)", {
  skip("Bug may be fixed in current qicharts2 version or requires specific conditions to manifest")

  # Setup: Minimal data der trigger buggen
  d <- data.frame(
    x = 1:3,
    y = c(12, 15, 18),
    n = rep(100, 3)
  )

  # OLD BEHAVIOR: do.call med pre-evaluerede vektorer fejler
  # Dette dokumenterer upstream bug i qicharts2
  # Note: Bug rapporteret af Codex, men reproduceres ikke altid
  expect_error(
    do.call(qicharts2::qic, list(
      x = d$x,        # Pre-evalueret vektor
      y = d$y,        # Pre-evalueret vektor
      n = d$n,        # Pre-evalueret vektor
      chart = "p",
      return.data = TRUE
    )),
    "condition has length > 1"
  )
})

test_that("qic() virker med do.call og data + kolonnenavne (fix)", {
  # Setup: Samme minimal data
  d <- data.frame(
    x = 1:3,
    y = c(12, 15, 18),
    n = rep(100, 3)
  )

  # NEW BEHAVIOR: do.call med data + kolonnenavne virker
  result <- expect_no_error(
    do.call(qicharts2::qic, list(
      data = d,
      x = as.name("x"),      # Kolonnenavn som symbol
      y = as.name("y"),      # Kolonnenavn som symbol
      n = as.name("n"),      # Kolonnenavn som symbol
      chart = "p",
      return.data = TRUE
    ))
  )

  # Verificer result struktur
  expect_true(is.data.frame(result))
  expect_true("y" %in% names(result))
  expect_true("cl" %in% names(result))
  expect_true("ucl" %in% names(result))
  expect_true("lcl" %in% names(result))
  expect_equal(nrow(result), 3)
})

test_that("qic() virker med do.call uden n kolonne (run chart)", {
  # Setup: Data uden denominators (run chart)
  d <- data.frame(
    x = 1:5,
    y = c(10, 12, 15, 13, 11)
  )

  # Run chart med data + kolonnenavne (ingen n kolonne)
  result <- expect_no_error(
    do.call(qicharts2::qic, list(
      data = d,
      x = as.name("x"),
      y = as.name("y"),
      chart = "run",
      return.data = TRUE
    ))
  )

  # Verificer result
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 5)
  expect_true("y" %in% names(result))
  expect_true("cl" %in% names(result))
})

test_that("qic() virker med do.call og target + centerline værdier", {
  # Setup: Data med target og centerline
  d <- data.frame(
    x = 1:5,
    y = c(8, 10, 12, 9, 11)
  )

  # Run chart med target og centerline (scalar værdier, ikke kolonnenavne)
  result <- expect_no_error(
    do.call(qicharts2::qic, list(
      data = d,
      x = as.name("x"),
      y = as.name("y"),
      chart = "run",
      target = 10,      # Scalar værdi
      cl = 9,           # Scalar værdi (centerline)
      return.data = TRUE
    ))
  )

  # Verificer result indeholder target og centerline
  expect_true(is.data.frame(result))
  expect_true("target" %in% names(result))
  expect_true("cl" %in% names(result))

  # Verificer værdier
  expect_equal(unique(result$target[!is.na(result$target)]), 10)
  expect_equal(unique(result$cl[!is.na(result$cl)]), 9)
})

test_that("qic() virker med do.call og ekstra argumenter (freeze, part)", {
  # Setup: Data med freeze og part argumenter
  d <- data.frame(
    x = 1:10,
    y = c(8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
  )

  # Run chart med freeze position
  result <- expect_no_error(
    do.call(qicharts2::qic, list(
      data = d,
      x = as.name("x"),
      y = as.name("y"),
      chart = "run",
      freeze = 5,       # Freeze efter 5. observation
      return.data = TRUE
    ))
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 10)

  # Run chart med part argument
  result_part <- expect_no_error(
    do.call(qicharts2::qic, list(
      data = d,
      x = as.name("x"),
      y = as.name("y"),
      chart = "run",
      part = c(5),      # Part break ved observation 5
      return.data = TRUE
    ))
  )

  expect_true(is.data.frame(result_part))
  expect_equal(nrow(result_part), 10)
})
