# test-target-label-formatting.R
# Tests for målværdi label formatting med foranstillede tegn

test_that("format_target_prefix() håndterer >= korrekt", {
  # Test >= med tal
  result <- format_target_prefix(">=90")
  expect_equal(result, "\U226590")

  result <- format_target_prefix(">= 90")
  expect_equal(result, "\U2265 90")

  result <- format_target_prefix(">=  80.5")
  expect_equal(result, "\U2265  80.5")
})

test_that("format_target_prefix() håndterer <= korrekt", {
  # Test <= med tal
  result <- format_target_prefix("<=25")
  expect_equal(result, "\U226425")

  result <- format_target_prefix("<= 25")
  expect_equal(result, "\U2264 25")

  result <- format_target_prefix("<=  10.2")
  expect_equal(result, "\U2264  10.2")
})

test_that("format_target_prefix() håndterer < uden tal (pil ned)", {
  # Test < uden tal → pil ned
  result <- format_target_prefix("<")
  expect_equal(result, "\U2193")

  # Test < med kun whitespace
  result <- format_target_prefix("<  ")
  expect_equal(result, "\U2193")

  # Test < med tal (skal bevares som <)
  result <- format_target_prefix("<25")
  expect_equal(result, "<25")

  result <- format_target_prefix("< 25")
  expect_equal(result, "< 25")
})

test_that("format_target_prefix() håndterer > uden tal (pil op)", {
  # Test > uden tal → pil op
  result <- format_target_prefix(">")
  expect_equal(result, "\U2191")

  # Test > med kun whitespace
  result <- format_target_prefix(">  ")
  expect_equal(result, "\U2191")

  # Test > med tal (skal bevares som >)
  result <- format_target_prefix(">90")
  expect_equal(result, ">90")

  result <- format_target_prefix("> 90")
  expect_equal(result, "> 90")
})

test_that("format_target_prefix() håndterer input uden foranstillet tegn", {
  # Plain tal uden operator
  result <- format_target_prefix("80")
  expect_equal(result, "80")

  result <- format_target_prefix("  90.5  ")
  expect_equal(result, "  90.5  ")

  # Tekst uden operator
  result <- format_target_prefix("ingen målværdi")
  expect_equal(result, "ingen målværdi")
})

test_that("format_target_prefix() håndterer NULL og tomme strenge", {
  # NULL input
  result <- format_target_prefix(NULL)
  expect_equal(result, "")

  # Tom streng
  result <- format_target_prefix("")
  expect_equal(result, "")

  # Kun whitespace
  result <- format_target_prefix("   ")
  expect_equal(result, "   ")
})

test_that("format_target_prefix() håndterer edge cases", {
  # Multiple operators (skal kun behandle første)
  result <- format_target_prefix(">=<=90")
  expect_equal(result, "\U2264=90")

  # Operator i midten (skal ikke behandles)
  result <- format_target_prefix("90>=80")
  expect_equal(result, "90>=80")

  # Negative tal
  result <- format_target_prefix(">=-5")
  expect_equal(result, "\U2265-5")

  result <- format_target_prefix("<-10")
  expect_equal(result, "<-10")
})

test_that("format_target_prefix() bevarer decimal separatorer", {
  # Punkt som decimal separator
  result <- format_target_prefix(">=90.5")
  expect_equal(result, "\U226590.5")

  # Komma som decimal separator (dansk format)
  result <- format_target_prefix(">=90,5")
  expect_equal(result, "\U226590,5")

  result <- format_target_prefix("<=25,3")
  expect_equal(result, "\U226425,3")
})

test_that("create_responsive_label() bruger 'UDVIKLINGSMÅL' som header", {
  # Test at header er korrekt
  label <- create_responsive_label(
    header = "UDVIKLINGSMÅL",
    value = "\U226590%",
    label_size = 6
  )

  # Check at UDVIKLINGSMÅL er i output
  expect_match(label, "UDVIKLINGSMÅL")

  # Check at value er i output
  expect_match(label, "\U226590%")
})

test_that("Integration: målværdi formatering i add_spc_labels()", {
  skip_if_not_installed("qicharts2")
  skip_if_not_installed("ggplot2")

  # Opret simple test data
  test_data <- data.frame(
    x = 1:10,
    y = c(85, 88, 90, 92, 87, 89, 91, 93, 88, 90),
    cl = rep(89.3, 10),
    target = rep(90, 10)
  )

  # Opret simple ggplot
  p <- ggplot2::ggplot(test_data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggplot2::geom_line()

  # Test at funktionen kan køre uden fejl
  expect_silent({
    plot_with_labels <- add_spc_labels(
      plot = p,
      qic_data = test_data,
      y_axis_unit = "percent",
      verbose = FALSE
    )
  })
})

test_that("has_arrow_symbol() detekterer pil-symboler korrekt", {
  # Test down arrow
  expect_true(has_arrow_symbol("\U2193"))

  # Test up arrow
  expect_true(has_arrow_symbol("\U2191"))

  # Test kombineret tekst med down arrow
  expect_true(has_arrow_symbol("UDVIKLINGSMÅL: \U2193"))

  # Test kombineret tekst med up arrow
  expect_true(has_arrow_symbol("UDVIKLINGSMÅL: \U2191"))

  # Test tekst uden pil-symboler
  expect_false(has_arrow_symbol("≥90%"))
  expect_false(has_arrow_symbol("≤25"))
  expect_false(has_arrow_symbol("Normal tekst"))

  # Test NULL og tom streng
  expect_false(has_arrow_symbol(NULL))
  expect_false(has_arrow_symbol(""))
  expect_false(has_arrow_symbol(character(0)))
})

test_that("add_spc_labels() sætter suppress_targetline attribute ved pil-symboler", {
  skip_if_not_installed("ggplot2")

  # Opret simple test data
  test_data <- data.frame(
    x = 1:10,
    y = c(85, 88, 90, 92, 87, 89, 91, 93, 88, 90),
    cl = rep(89.3, 10),
    target = rep(90, 10),
    part = rep(1, 10)
  )

  # Opret simple ggplot
  p <- ggplot2::ggplot(test_data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggplot2::geom_line()

  # Test med down arrow
  plot_with_down_arrow <- add_spc_labels(
    plot = p,
    qic_data = test_data,
    y_axis_unit = "percent",
    target_text = "<",
    verbose = FALSE
  )

  expect_true(attr(plot_with_down_arrow, "suppress_targetline"))
  expect_equal(attr(plot_with_down_arrow, "arrow_type"), "down")

  # Test med up arrow
  plot_with_up_arrow <- add_spc_labels(
    plot = p,
    qic_data = test_data,
    y_axis_unit = "percent",
    target_text = ">",
    verbose = FALSE
  )

  expect_true(attr(plot_with_up_arrow, "suppress_targetline"))
  expect_equal(attr(plot_with_up_arrow, "arrow_type"), "up")

  # Test uden pil-symbol
  plot_normal <- add_spc_labels(
    plot = p,
    qic_data = test_data,
    y_axis_unit = "percent",
    target_text = ">=90",
    verbose = FALSE
  )

  expect_false(attr(plot_normal, "suppress_targetline"))
  expect_null(attr(plot_normal, "arrow_type"))
})
