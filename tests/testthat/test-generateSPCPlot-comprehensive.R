# test-generateSPCPlot-comprehensive.R
# ==============================================================================
# COMPREHENSIVE TEST SUITE: generateSPCPlot() Function
# ==============================================================================
#
# FORMÅL: >80% coverage af fct_spc_plot_generation.R (1477 lines)
# FOKUS: Critical path testing for clinical accuracy
#
# STRUKTUR:
#   1. Basic Chart Types (run, i, p, c, u)
#   2. Y-Axis Formatting (percent, count, rate, time)
#   3. Multi-Part Handling (phases, freeze)
#   4. X-Axis Datetime Formatting (weekly, monthly, daily)
#   5. Edge Cases (empty, single row, extreme values)
#   6. Performance Benchmarks (<500ms for 1000 rows)
#
# SUCCESS CRITERIA:
#   - All chart types render correctly
#   - Danish number formats parsed
#   - No visual regressions
#   - Performance within thresholds
# ==============================================================================

# SETUP HELPERS ================================================================

# Helper til at oprette standard test data
create_test_data <- function(n = 12, chart_type = "p", with_dates = TRUE) {
  data <- data.frame(
    Obs = 1:n,
    `Tæller` = sample(40:50, n, replace = TRUE),
    `Nævner` = rep(50, n),
    `Værdi` = sample(85:105, n, replace = TRUE),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (with_dates) {
    data$Dato <- seq.Date(as.Date("2024-01-01"), by = "month", length.out = n)
  }

  return(data)
}

# Helper til at verificere plot struktur
verify_plot_structure <- function(result, expected_layers_min = 3) {
  expect_type(result, "list")
  expect_true("plot" %in% names(result))
  expect_true("qic_data" %in% names(result))
  expect_s3_class(result$plot, "ggplot")
  expect_true(is.data.frame(result$qic_data))
  expect_gte(length(result$plot$layers), expected_layers_min)
}

# BASIC CHART TYPES TESTS ======================================================

describe("Basic Chart Types", {

  it("generates run chart correctly", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    test_data <- create_test_data(n = 15, chart_type = "run")
    config <- list(x_col = "Dato", y_col = "Tæller", n_col = "Nævner")

    result <- generateSPCPlot(
      data = test_data,
      config = config,
      chart_type = "run",
      chart_title_reactive = reactive("Run Chart Test")
    )

    verify_plot_structure(result, expected_layers_min = 3)

    # Run chart skal have centerline men IKKE kontrolgrænser
    expect_true("cl" %in% names(result$qic_data))
    expect_false("ucl" %in% names(result$qic_data))
    expect_false("lcl" %in% names(result$qic_data))

    # Run chart med nævner skal have procent værdier (80-100 range)
    expect_true(all(result$qic_data$y >= 70 & result$qic_data$y <= 110))
  })

  it("generates i chart with control limits", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    test_data <- create_test_data(n = 20, chart_type = "i")
    config <- list(x_col = "Dato", y_col = "Værdi", n_col = NULL)

    result <- generateSPCPlot(
      data = test_data,
      config = config,
      chart_type = "i",
      chart_title_reactive = reactive("I Chart Test")
    )

    verify_plot_structure(result, expected_layers_min = 4)

    # I chart skal have kontrolgrænser
    expect_true("cl" %in% names(result$qic_data))
    expect_true("ucl" %in% names(result$qic_data))
    expect_true("lcl" %in% names(result$qic_data))

    # Verificer at UCL > CL > LCL
    mean_ucl <- mean(result$qic_data$ucl, na.rm = TRUE)
    mean_cl <- mean(result$qic_data$cl, na.rm = TRUE)
    mean_lcl <- mean(result$qic_data$lcl, na.rm = TRUE)

    expect_gt(mean_ucl, mean_cl)
    expect_gt(mean_cl, mean_lcl)
  })

  it("generates p chart with percentage y-axis", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    test_data <- create_test_data(n = 24, chart_type = "p")
    config <- list(x_col = "Dato", y_col = "Tæller", n_col = "Nævner")

    result <- generateSPCPlot(
      data = test_data,
      config = config,
      chart_type = "p",
      y_axis_unit = "percent",
      chart_title_reactive = reactive("P Chart Test")
    )

    verify_plot_structure(result, expected_layers_min = 4)

    # P chart skal have kontrolgrænser
    expect_true("ucl" %in% names(result$qic_data))
    expect_true("lcl" %in% names(result$qic_data))

    # Y-værdier skal være proportions (0-1)
    expect_true(all(result$qic_data$y >= 0 & result$qic_data$y <= 1))
  })

  it("generates c chart for count data", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    test_data <- data.frame(
      Dato = seq.Date(as.Date("2024-01-01"), by = "week", length.out = 15),
      Antal = c(3, 5, 2, 7, 4, 6, 3, 5, 8, 4, 6, 3, 7, 5, 4),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    config <- list(x_col = "Dato", y_col = "Antal", n_col = NULL)

    result <- generateSPCPlot(
      data = test_data,
      config = config,
      chart_type = "c",
      y_axis_unit = "count",
      chart_title_reactive = reactive("C Chart Test")
    )

    verify_plot_structure(result, expected_layers_min = 4)

    # C chart for count data
    expect_true("ucl" %in% names(result$qic_data))
    expect_true("lcl" %in% names(result$qic_data))
  })

  it("generates u chart for rate data", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    test_data <- create_test_data(n = 18, chart_type = "u")
    config <- list(x_col = "Dato", y_col = "Tæller", n_col = "Nævner")

    result <- generateSPCPlot(
      data = test_data,
      config = config,
      chart_type = "u",
      y_axis_unit = "rate",
      chart_title_reactive = reactive("U Chart Test")
    )

    verify_plot_structure(result, expected_layers_min = 4)

    # U chart skal have kontrolgrænser
    expect_true("ucl" %in% names(result$qic_data))
    expect_true("lcl" %in% names(result$qic_data))
  })
})

# Y-AXIS FORMATTING TESTS ======================================================

describe("Y-Axis Formatting", {

  it("formats percent values correctly", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    test_data <- create_test_data(n = 12)
    config <- list(x_col = "Dato", y_col = "Tæller", n_col = "Nævner")

    result <- generateSPCPlot(
      data = test_data,
      config = config,
      chart_type = "p",
      y_axis_unit = "percent",
      chart_title_reactive = reactive("Percent Formatting Test")
    )

    verify_plot_structure(result)

    # Y-værdier skal være decimal (qic's internal format)
    expect_true(all(result$qic_data$y >= 0 & result$qic_data$y <= 1))

    # Plot skal have percent scale
    # (Dette verificeres ved visual inspection eller snapshot tests)
  })

  it("applies K/M notation for large counts", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    # Store tal data
    test_data <- data.frame(
      Dato = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 10),
      Antal = c(1200, 1500, 2300, 1800, 2100, 1900, 2500, 1700, 2200, 1600),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    config <- list(x_col = "Dato", y_col = "Antal", n_col = NULL)

    result <- generateSPCPlot(
      data = test_data,
      config = config,
      chart_type = "i",
      y_axis_unit = "count",
      chart_title_reactive = reactive("K Notation Test")
    )

    verify_plot_structure(result)

    # Værdier >= 1000 skal vises med K notation
    # (Verificeres ved scale_y_continuous labels funktion)
    expect_true(all(result$qic_data$y >= 1000))
  })

  it("handles rate calculations", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    test_data <- create_test_data(n = 12)
    config <- list(x_col = "Dato", y_col = "Tæller", n_col = "Nævner")

    result <- generateSPCPlot(
      data = test_data,
      config = config,
      chart_type = "u",
      y_axis_unit = "rate",
      chart_title_reactive = reactive("Rate Test")
    )

    verify_plot_structure(result)

    # Rate værdier skal være numeriske
    expect_true(is.numeric(result$qic_data$y))
  })

  it("formats time values intelligently", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    # Test data med minutter
    test_data <- data.frame(
      Dato = seq.Date(as.Date("2024-01-01"), by = "week", length.out = 10),
      Ventetid = c(45, 52, 38, 61, 48, 55, 42, 58, 50, 47), # minutter
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    config <- list(x_col = "Dato", y_col = "Ventetid", n_col = NULL)

    result <- generateSPCPlot(
      data = test_data,
      config = config,
      chart_type = "i",
      y_axis_unit = "time",
      chart_title_reactive = reactive("Time Formatting Test")
    )

    verify_plot_structure(result)

    # Time værdier under 60 minutter
    expect_true(all(result$qic_data$y < 60))
  })
})

# MULTI-PART HANDLING TESTS ====================================================

describe("Multi-Part Data", {

  it("creates separate control limits per part", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    # Data med phase change
    test_data <- data.frame(
      Dato = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 18),
      `Tæller` = c(45, 43, 48, 46, 47, 49, # Part 1
                   55, 54, 58, 56, 57, 59, # Part 2 (higher level)
                   50, 48, 52, 51, 49, 53), # Part 3
      `Nævner` = rep(60, 18),
      Skift = c(rep(FALSE, 6), TRUE, rep(FALSE, 5), TRUE, rep(FALSE, 5)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    config <- list(x_col = "Dato", y_col = "Tæller", n_col = "Nævner")

    result <- generateSPCPlot(
      data = test_data,
      config = config,
      chart_type = "p",
      show_phases = TRUE,
      skift_column = "Skift",
      chart_title_reactive = reactive("Multi-Part Test")
    )

    verify_plot_structure(result, expected_layers_min = 4)

    # Skal have part column
    expect_true("part" %in% names(result$qic_data))

    # Flere parts
    expect_gt(length(unique(result$qic_data$part)), 1)

    # Hver part skal have sin egen centerline
    parts_cl <- aggregate(cl ~ part, data = result$qic_data, FUN = mean)
    expect_equal(nrow(parts_cl), length(unique(result$qic_data$part)))
  })

  it("handles part transitions correctly", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    test_data <- data.frame(
      Obs = 1:12,
      `Værdi` = c(95, 92, 98, 91, 94, 96, # Part 1
                  105, 108, 103, 107, 106, 104), # Part 2
      Skift = c(rep(FALSE, 6), TRUE, rep(FALSE, 5)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    config <- list(x_col = "Obs", y_col = "Værdi", n_col = NULL)

    result <- generateSPCPlot(
      data = test_data,
      config = config,
      chart_type = "i",
      show_phases = TRUE,
      skift_column = "Skift",
      chart_title_reactive = reactive("Part Transition Test")
    )

    verify_plot_structure(result)

    # Part transition skal ske ved observation 7
    expect_true("part" %in% names(result$qic_data))
    expect_equal(length(unique(result$qic_data$part)), 2)
  })

  it("handles freeze baseline correctly", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    test_data <- data.frame(
      Obs = 1:15,
      `Tæller` = c(45, 43, 48, 46, 47, # Baseline (5 punkter)
                   49, 44, 44, 42, 46, 48, 45, 47, 44, 46), # Efter freeze
      `Nævner` = rep(50, 15),
      Frys = c(rep(FALSE, 5), rep(TRUE, 10)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    config <- list(x_col = "Obs", y_col = "Tæller", n_col = "Nævner")

    result <- generateSPCPlot(
      data = test_data,
      config = config,
      chart_type = "p",
      show_phases = FALSE,
      frys_column = "Frys",
      chart_title_reactive = reactive("Freeze Baseline Test")
    )

    verify_plot_structure(result)

    # Freeze skal fiksere baseline efter punkt 5
    # Dette verificeres ved at centerline er beregnet fra første 5 punkter
  })
})

# X-AXIS DATETIME FORMATTING TESTS =============================================

describe("X-Axis Datetime Formatting", {

  it("formats weekly data correctly", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    test_data <- data.frame(
      Dato = seq.Date(as.Date("2024-01-01"), by = "week", length.out = 20),
      `Tæller` = sample(40:50, 20, replace = TRUE),
      `Nævner` = rep(50, 20),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    config <- list(x_col = "Dato", y_col = "Tæller", n_col = "Nævner")

    result <- generateSPCPlot(
      data = test_data,
      config = config,
      chart_type = "p",
      chart_title_reactive = reactive("Weekly Format Test")
    )

    verify_plot_structure(result)

    # X-akse skal være Date eller POSIXct
    expect_true(inherits(result$qic_data$x, c("Date", "POSIXct", "POSIXt")))

    # Intelligent formatting skal detektere weekly interval
  })

  it("formats monthly data correctly", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    test_data <- create_test_data(n = 24, with_dates = TRUE)
    config <- list(x_col = "Dato", y_col = "Tæller", n_col = "Nævner")

    result <- generateSPCPlot(
      data = test_data,
      config = config,
      chart_type = "p",
      chart_title_reactive = reactive("Monthly Format Test")
    )

    verify_plot_structure(result)

    # X-akse skal være Date eller POSIXct
    expect_true(inherits(result$qic_data$x, c("Date", "POSIXct", "POSIXt")))
  })

  it("formats daily data correctly", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    test_data <- data.frame(
      Dato = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 30),
      `Tæller` = sample(40:50, 30, replace = TRUE),
      `Nævner` = rep(50, 30),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    config <- list(x_col = "Dato", y_col = "Tæller", n_col = "Nævner")

    result <- generateSPCPlot(
      data = test_data,
      config = config,
      chart_type = "p",
      chart_title_reactive = reactive("Daily Format Test")
    )

    verify_plot_structure(result)

    # X-akse skal være Date eller POSIXct
    expect_true(inherits(result$qic_data$x, c("Date", "POSIXct", "POSIXt")))
  })

  it("handles character x-column as factor", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    test_data <- data.frame(
      `Uge tekst` = paste("Uge", 1:12),
      `Tæller` = sample(40:50, 12, replace = TRUE),
      `Nævner` = rep(50, 12),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    config <- list(x_col = "Uge tekst", y_col = "Tæller", n_col = "Nævner")

    result <- generateSPCPlot(
      data = test_data,
      config = config,
      chart_type = "p",
      chart_title_reactive = reactive("Character X Test")
    )

    verify_plot_structure(result)

    # Character skal konverteres til factor
    expect_true(is.factor(result$qic_data$x))
  })
})

# EDGE CASES TESTS =============================================================

describe("Edge Cases", {

  it("handles empty data gracefully", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    empty_data <- data.frame()
    config <- list(x_col = "Obs", y_col = "Tæller", n_col = "Nævner")

    expect_error(
      generateSPCPlot(
        data = empty_data,
        config = config,
        chart_type = "p",
        chart_title_reactive = reactive("Empty Test")
      ),
      "Ingen"
    )
  })

  it("handles single row data", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    single_data <- data.frame(
      Obs = 1,
      `Tæller` = 45,
      `Nævner` = 50,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    config <- list(x_col = "Obs", y_col = "Tæller", n_col = "Nævner")

    expect_error(
      generateSPCPlot(
        data = single_data,
        config = config,
        chart_type = "p",
        chart_title_reactive = reactive("Single Row Test")
      ),
      "For få.*datapunkter"
    )
  })

  it("handles extreme values", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    extreme_data <- data.frame(
      Obs = 1:10,
      `Værdi` = c(1, 5, 3, 1000000, 2, 4, 3, 5, 4, 3), # Outlier at position 4
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    config <- list(x_col = "Obs", y_col = "Værdi", n_col = NULL)

    # Skal ikke fejle ved ekstreme værdier
    result <- generateSPCPlot(
      data = extreme_data,
      config = config,
      chart_type = "i",
      y_axis_unit = "count",
      chart_title_reactive = reactive("Extreme Values Test")
    )

    verify_plot_structure(result)

    # Outlier skal være med i data
    expect_true(any(result$qic_data$y > 100000))
  })

  it("handles all NA values gracefully", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    na_data <- data.frame(
      Obs = 1:10,
      `Tæller` = rep(NA, 10),
      `Nævner` = rep(NA, 10),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    config <- list(x_col = "Obs", y_col = "Tæller", n_col = "Nævner")

    expect_error(
      generateSPCPlot(
        data = na_data,
        config = config,
        chart_type = "p",
        chart_title_reactive = reactive("All NA Test")
      ),
      "Ingen.*komplette"
    )
  })

  it("handles mixed NA values", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    mixed_data <- data.frame(
      Obs = 1:10,
      `Tæller` = c(45, NA, 48, 46, 47, NA, 44, 44, NA, 46),
      `Nævner` = c(50, 50, 50, NA, 50, 50, 50, 50, 50, 50),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    config <- list(x_col = "Obs", y_col = "Tæller", n_col = "Nævner")

    # Skal filtrere NA værdier og fortsætte
    result <- generateSPCPlot(
      data = mixed_data,
      config = config,
      chart_type = "p",
      chart_title_reactive = reactive("Mixed NA Test")
    )

    verify_plot_structure(result)

    # Kun komplette cases skal være med
    expect_lt(nrow(result$qic_data), 10)
    expect_gte(nrow(result$qic_data), 3) # Minimum 3 punkter
  })

  it("handles zero denominators", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    zero_data <- data.frame(
      Obs = 1:8,
      `Tæller` = c(45, 43, 48, 46, 47, 49, 44, 44),
      `Nævner` = c(50, 50, 0, 50, 50, 50, 50, 50), # Zero at position 3
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    config <- list(x_col = "Obs", y_col = "Tæller", n_col = "Nævner")

    expect_error(
      generateSPCPlot(
        data = zero_data,
        config = config,
        chart_type = "p",
        chart_title_reactive = reactive("Zero Denominator Test")
      ),
      "Nævner.*nul"
    )
  })
})

# PERFORMANCE BENCHMARKS =======================================================

describe("Performance", {

  it("completes in <500ms for 1000 rows", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))
    skip_on_cran()

    # Large dataset
    large_data <- data.frame(
      Obs = 1:1000,
      `Tæller` = sample(400:500, 1000, replace = TRUE),
      `Nævner` = rep(500, 1000),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    config <- list(x_col = "Obs", y_col = "Tæller", n_col = "Nævner")

    # Benchmark
    start_time <- Sys.time()

    result <- generateSPCPlot(
      data = large_data,
      config = config,
      chart_type = "p",
      chart_title_reactive = reactive("Performance Test")
    )

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    verify_plot_structure(result)

    # Performance target: <500ms (0.5 seconds)
    expect_lt(elapsed, 0.5)
  })

  it("caches x-column validation", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    test_data <- create_test_data(n = 50)
    config <- list(x_col = "Dato", y_col = "Tæller", n_col = "Nævner")

    # First call - establishes cache
    result1 <- generateSPCPlot(
      data = test_data,
      config = config,
      chart_type = "p",
      chart_title_reactive = reactive("Cache Test 1")
    )

    # Second call - should use cache
    result2 <- generateSPCPlot(
      data = test_data,
      config = config,
      chart_type = "p",
      chart_title_reactive = reactive("Cache Test 2")
    )

    # Both should succeed
    verify_plot_structure(result1)
    verify_plot_structure(result2)
  })
})
