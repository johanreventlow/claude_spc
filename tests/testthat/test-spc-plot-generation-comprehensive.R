# test-spc-plot-generation-comprehensive.R
# Comprehensive tests for SPC plot generation functionality
# Covers generateSPCPlot function, qicharts2 integration, and Danish number parsing
# Tests the most complex statistical functionality (725+ lines) for clinical accuracy

test_that("generateSPCPlot basic functionality works with valid data", {
  # TEST: Core generateSPCPlot function with standard Danish SPC data

  # Skip if generateSPCPlot function not available
  skip_if_not(exists("generateSPCPlot", mode = "function"), "generateSPCPlot function not available")

  # SETUP: Standard Danish clinical data format
  test_data <- data.frame(
    Dato = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 12),
    `Tæller` = c(45, 43, 48, 46, 47, 49, 44, 44, 42, 46, 48, 45),
    `Nævner` = c(50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  config <- list(
    x_col = "Dato",
    y_col = "Tæller",
    n_col = "Nævner"
  )

  # TEST: Basic plot generation
  result <- generateSPCPlot(
    data = test_data,
    config = config,
    chart_type = "p",
    chart_title_reactive = reactive("Test P-Chart")
  )

  # Verify structure
  expect_type(result, "list")
  expect_true("plot" %in% names(result))
  expect_true("qic_data" %in% names(result))
  expect_s3_class(result$plot, "ggplot")
  expect_true(is.data.frame(result$qic_data))

  # Verify qic_data has required columns
  expect_true(all(c("x", "y", "cl") %in% names(result$qic_data)))
  expect_equal(nrow(result$qic_data), 12)

  # Verify plot layers (basic SPC components)
  expect_gte(length(result$plot$layers), 3) # At minimum: line, points, centerline
})

test_that("generateSPCPlot handles Danish number format parsing", {
  # TEST: Danish number formats with comma as decimal separator

  # Skip if generateSPCPlot function not available
  skip_if_not(exists("generateSPCPlot", mode = "function"), "generateSPCPlot function not available")

  # SETUP: Data with Danish number formats
  test_data <- data.frame(
    Obs = 1:8,
    `Tæller` = c("45,5", "43,2", "48,7", "46,1", "47,0", "49,3", "44,8", "44,2"),
    `Nævner` = c("50,0", "50,0", "50,0", "50,0", "50,0", "50,0", "50,0", "50,0"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  config <- list(
    x_col = "Obs",
    y_col = "Tæller",
    n_col = "Nævner"
  )

  # TEST: Parse Danish decimal format
  result <- generateSPCPlot(
    data = test_data,
    config = config,
    chart_type = "p",
    chart_title_reactive = reactive("Danish Numbers Test")
  )

  expect_s3_class(result$plot, "ggplot")
  expect_true(is.data.frame(result$qic_data))

  # Verify Danish numbers were parsed correctly
  expect_true(all(result$qic_data$y >= 0 & result$qic_data$y <= 1)) # P-chart proportions
  expect_false(any(is.na(result$qic_data$y))) # No conversion failures
})

test_that("generateSPCPlot handles different chart types correctly", {
  # TEST: All supported SPC chart types

  # Skip if generateSPCPlot function not available
  skip_if_not(exists("generateSPCPlot", mode = "function"), "generateSPCPlot function not available")

  # SETUP: Base data for different chart types
  base_data <- data.frame(
    Obs = 1:10,
    `Tæller` = c(45, 43, 48, 46, 47, 49, 44, 44, 42, 46),
    `Nævner` = c(50, 50, 50, 50, 50, 50, 50, 50, 50, 50),
    `Værdi` = c(95, 92, 98, 91, 94, 96, 89, 93, 90, 97),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # TEST: P-chart (proportions)
  p_config <- list(x_col = "Obs", y_col = "Tæller", n_col = "Nævner")
  p_result <- generateSPCPlot(base_data, p_config, "p", chart_title_reactive = reactive("P-Chart Test"))

  expect_s3_class(p_result$plot, "ggplot")
  expect_true("ucl" %in% names(p_result$qic_data)) # P-charts have control limits
  expect_true("lcl" %in% names(p_result$qic_data))

  # TEST: Run chart (no control limits)
  run_config <- list(x_col = "Obs", y_col = "Tæller", n_col = "Nævner")
  run_result <- generateSPCPlot(base_data, run_config, "run", chart_title_reactive = reactive("Run Chart Test"))

  expect_s3_class(run_result$plot, "ggplot")
  # Run charts should have percentage scaling for ratio data
  expect_true(all(run_result$qic_data$y >= 80 & run_result$qic_data$y <= 100)) # Percentage range

  # TEST: I-chart (individual values)
  i_config <- list(x_col = "Obs", y_col = "Værdi", n_col = NULL)
  i_result <- generateSPCPlot(base_data, i_config, "i", chart_title_reactive = reactive("I-Chart Test"))

  expect_s3_class(i_result$plot, "ggplot")
  expect_true("ucl" %in% names(i_result$qic_data)) # I-charts have control limits
  expect_true("lcl" %in% names(i_result$qic_data))
})

test_that("generateSPCPlot date handling and formatting works", {
  # TEST: Date column processing and intelligent formatting

  # Skip if generateSPCPlot function not available
  skip_if_not(exists("generateSPCPlot", mode = "function"), "generateSPCPlot function not available")

  # SETUP: Data with different date formats
  date_data <- data.frame(
    `Dato` = seq.Date(as.Date("2024-01-01"), by = "week", length.out = 15),
    `Tæller` = c(45, 43, 48, 46, 47, 49, 44, 44, 42, 46, 48, 45, 47, 44, 46),
    `Nævner` = rep(50, 15),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  config <- list(
    x_col = "Dato",
    y_col = "Tæller",
    n_col = "Nævner"
  )

  # TEST: Weekly date intervals
  weekly_result <- generateSPCPlot(
    data = date_data,
    config = config,
    chart_type = "p",
    chart_title_reactive = reactive("Weekly Data Test")
  )

  expect_s3_class(weekly_result$plot, "ggplot")
  expect_true(inherits(weekly_result$qic_data$x, c("Date", "POSIXct", "POSIXt")))

  # TEST: Monthly date intervals
  monthly_data <- data.frame(
    `Dato` = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 12),
    `Tæller` = c(45, 43, 48, 46, 47, 49, 44, 44, 42, 46, 48, 45),
    `Nævner` = rep(50, 12),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  monthly_result <- generateSPCPlot(
    data = monthly_data,
    config = config,
    chart_type = "p",
    chart_title_reactive = reactive("Monthly Data Test")
  )

  expect_s3_class(monthly_result$plot, "ggplot")
  expect_true(inherits(monthly_result$qic_data$x, c("Date", "POSIXct", "POSIXt")))

  # TEST: Character x-column handling (e.g., "Uge tekst")
  char_data <- data.frame(
    `Uge` = paste("Uge", 1:8),
    `Tæller` = c(45, 43, 48, 46, 47, 49, 44, 44),
    `Nævner` = rep(50, 8),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  char_config <- list(x_col = "Uge", y_col = "Tæller", n_col = "Nævner")
  char_result <- generateSPCPlot(
    data = char_data,
    config = char_config,
    chart_type = "p",
    chart_title_reactive = reactive("Character X Test")
  )

  expect_s3_class(char_result$plot, "ggplot")
  expect_true(is.factor(char_result$qic_data$x)) # Character columns become factors
})

test_that("generateSPCPlot phase and freeze functionality works", {
  # TEST: Phase separation lines and freeze baseline

  # Skip if generateSPCPlot function not available
  skip_if_not(exists("generateSPCPlot", mode = "function"), "generateSPCPlot function not available")

  # SETUP: Data with phase changes
  phase_data <- data.frame(
    Obs = 1:15,
    `Tæller` = c(45, 43, 48, 46, 47, 49, 44, 44, 42, 46, 48, 45, 47, 44, 46),
    `Nævner` = rep(50, 15),
    Skift = c(rep(FALSE, 7), rep(TRUE, 8)), # Phase change at observation 8
    Frys = c(rep(FALSE, 5), rep(TRUE, 10)), # Freeze baseline at observation 6
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  config <- list(
    x_col = "Obs",
    y_col = "Tæller",
    n_col = "Nævner"
  )

  # TEST: With phases and freeze
  phase_result <- generateSPCPlot(
    data = phase_data,
    config = config,
    chart_type = "p",
    show_phases = TRUE,
    skift_column = "Skift",
    frys_column = "Frys",
    chart_title_reactive = reactive("Phase Test")
  )

  expect_s3_class(phase_result$plot, "ggplot")
  expect_true(is.data.frame(phase_result$qic_data))

  # Verify phase information in qic_data
  if ("part" %in% names(phase_result$qic_data)) {
    expect_true(length(unique(phase_result$qic_data$part)) > 1) # Multiple phases
  }

  # TEST: Visual verification - phase separation lines should be added
  # Note: Actual visual verification would require plot rendering
  expect_gte(length(phase_result$plot$layers), 4) # Additional layers for phase lines
})

test_that("generateSPCPlot target line functionality works", {
  # TEST: Target line addition and positioning

  # Skip if generateSPCPlot function not available
  skip_if_not(exists("generateSPCPlot", mode = "function"), "generateSPCPlot function not available")

  # SETUP: Standard data
  test_data <- data.frame(
    Obs = 1:8,
    `Tæller` = c(45, 43, 48, 46, 47, 49, 44, 44),
    `Nævner` = rep(50, 8),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  config <- list(x_col = "Obs", y_col = "Tæller", n_col = "Nævner")

  # TEST: With target line
  target_value <- 0.95
  target_result <- generateSPCPlot(
    data = test_data,
    config = config,
    chart_type = "p",
    target_value = target_value,
    chart_title_reactive = reactive("Target Line Test")
  )

  expect_s3_class(target_result$plot, "ggplot")

  # TEST: Target line should add an additional layer
  expect_gte(length(target_result$plot$layers), 4) # Line, points, CL, target

  # TEST: Without target line
  no_target_result <- generateSPCPlot(
    data = test_data,
    config = config,
    chart_type = "p",
    target_value = NULL,
    chart_title_reactive = reactive("No Target Test")
  )

  # Should have one fewer layer than with target
  expect_lt(length(no_target_result$plot$layers), length(target_result$plot$layers))
})

test_that("generateSPCPlot comment annotations work", {
  # TEST: Comment data processing and ggrepel integration

  # Skip if generateSPCPlot function not available
  skip_if_not(exists("generateSPCPlot", mode = "function"), "generateSPCPlot function not available")

  # SETUP: Data with comments
  comment_data <- data.frame(
    Obs = 1:6,
    `Tæller` = c(45, 43, 48, 46, 47, 40), # Last point is notably low
    `Nævner` = rep(50, 6),
    Kommentar = c("", "", "", "", "", "System nedbrud"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  config <- list(x_col = "Obs", y_col = "Tæller", n_col = "Nævner")

  # TEST: With comments
  comment_result <- generateSPCPlot(
    data = comment_data,
    config = config,
    chart_type = "p",
    kommentar_column = "Kommentar",
    chart_title_reactive = reactive("Comment Test")
  )

  expect_s3_class(comment_result$plot, "ggplot")

  # TEST: Comment annotations should add layers
  expect_gte(length(comment_result$plot$layers), 4) # Basic layers + comment

  # TEST: Without comments
  no_comment_result <- generateSPCPlot(
    data = comment_data,
    config = config,
    chart_type = "p",
    kommentar_column = NULL,
    chart_title_reactive = reactive("No Comment Test")
  )

  # Should have fewer layers than with comments
  expect_lt(length(no_comment_result$plot$layers), length(comment_result$plot$layers))
})

test_that("generateSPCPlot error handling works correctly", {
  # TEST: Various error conditions and defensive programming

  # Skip if generateSPCPlot function not available
  skip_if_not(exists("generateSPCPlot", mode = "function"), "generateSPCPlot function not available")

  # TEST: Empty data
  empty_data <- data.frame()
  empty_config <- list(x_col = "Obs", y_col = "Tæller", n_col = "Nævner")

  expect_error(
    generateSPCPlot(empty_data, empty_config, "p", chart_title_reactive = reactive("Empty Test")),
    "Ingen"
  )

  # TEST: Missing required columns
  incomplete_data <- data.frame(Obs = 1:5, stringsAsFactors = FALSE)
  expect_error(
    generateSPCPlot(incomplete_data, empty_config, "p", chart_title_reactive = reactive("Missing Test")),
    "Y-kolonne"
  )

  # TEST: All NA values
  na_data <- data.frame(
    Obs = 1:5,
    `Tæller` = rep(NA, 5),
    `Nævner` = rep(NA, 5),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  expect_error(
    generateSPCPlot(na_data, empty_config, "p", chart_title_reactive = reactive("NA Test")),
    "Ingen.*komplette"
  )

  # TEST: Zero denominators
  zero_data <- data.frame(
    Obs = 1:5,
    `Tæller` = c(1, 2, 3, 4, 5),
    `Nævner` = c(10, 0, 10, 10, 10), # One zero denominator
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  expect_error(
    generateSPCPlot(zero_data, empty_config, "p", chart_title_reactive = reactive("Zero Test")),
    "Nævner.*nul"
  )

  # TEST: Too few data points
  small_data <- data.frame(
    Obs = 1:2,
    `Tæller` = c(45, 43),
    `Nævner` = c(50, 50),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  expect_error(
    generateSPCPlot(small_data, empty_config, "p", chart_title_reactive = reactive("Small Test")),
    "For få.*datapunkter.*minimum 3"
  )

  # TEST: character(0) handling in config
  bad_config <- list(
    x_col = character(0),
    y_col = "Tæller",
    n_col = character(0)
  )

  valid_data <- data.frame(
    Obs = 1:5,
    `Tæller` = c(45, 43, 48, 46, 47),
    `Nævner` = c(50, 50, 50, 50, 50),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Should handle character(0) gracefully by setting to NULL
  expect_no_error(
    generateSPCPlot(valid_data, bad_config, "i", chart_title_reactive = reactive("Bad Config Test"))
  )
})

test_that("generateSPCPlot performance and caching works", {
  # TEST: Performance features and caching mechanisms

  # Skip if generateSPCPlot function not available
  skip_if_not(exists("generateSPCPlot", mode = "function"), "generateSPCPlot function not available")

  # SETUP: Larger dataset to trigger performance considerations
  large_data <- data.frame(
    Obs = 1:50,
    `Tæller` = sample(40:50, 50, replace = TRUE),
    `Nævner` = rep(50, 50),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  config <- list(x_col = "Obs", y_col = "Tæller", n_col = "Nævner")

  # TEST: First run (should establish cache)
  start_time <- Sys.time()
  result1 <- generateSPCPlot(
    data = large_data,
    config = config,
    chart_type = "p",
    chart_title_reactive = reactive("Performance Test 1")
  )
  first_duration <- as.numeric(Sys.time() - start_time)

  expect_s3_class(result1$plot, "ggplot")

  # TEST: Second run with same data (should use cache)
  start_time <- Sys.time()
  result2 <- generateSPCPlot(
    data = large_data,
    config = config,
    chart_type = "p",
    chart_title_reactive = reactive("Performance Test 2")
  )
  second_duration <- as.numeric(Sys.time() - start_time)

  expect_s3_class(result2$plot, "ggplot")

  # Note: Caching effects may be minimal for this dataset size in tests
  # Main goal is ensuring performance optimizations don't break functionality
})

test_that("generateSPCPlot hospital theme integration works", {
  # TEST: Hospital branding and theme application

  # Skip if generateSPCPlot function not available
  skip_if_not(exists("generateSPCPlot", mode = "function"), "generateSPCPlot function not available")

  # SETUP: Standard test data
  test_data <- data.frame(
    Obs = 1:8,
    `Tæller` = c(45, 43, 48, 46, 47, 49, 44, 44),
    `Nævner` = rep(50, 8),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  config <- list(x_col = "Obs", y_col = "Tæller", n_col = "Nævner")

  # TEST: Basic plot generation
  result <- generateSPCPlot(
    data = test_data,
    config = config,
    chart_type = "p",
    chart_title_reactive = reactive("Hospital Theme Test")
  )

  expect_s3_class(result$plot, "ggplot")

  # TEST: Hospital theme can be applied
  # Use skip_if_not for helper functions
  if (exists("applyHospitalTheme", mode = "function")) {
    themed_plot <- applyHospitalTheme(result$plot)
    expect_s3_class(themed_plot, "ggplot")
  }

  # TEST: Color scheme should use hospital colors (if HOSPITAL_COLORS exists)
  # Use skip_if_not for configuration objects
  if (exists("HOSPITAL_COLORS")) {
    # Verify hospital colors are defined
    expect_true(is.list(HOSPITAL_COLORS))
    expect_true("primary" %in% names(HOSPITAL_COLORS) ||
                "hospitalblue" %in% names(HOSPITAL_COLORS))
  }
})

test_that("generateSPCPlot Danish clinical data patterns work", {
  # TEST: Real-world Danish clinical data patterns

  # Skip if generateSPCPlot function not available
  skip_if_not(exists("generateSPCPlot", mode = "function"), "generateSPCPlot function not available")

  # SETUP: Typical Danish hospital data format
  danish_data <- data.frame(
    `Måned` = c("Jan 2024", "Feb 2024", "Mar 2024", "Apr 2024",
                "Maj 2024", "Jun 2024", "Jul 2024", "Aug 2024"),
    `Genindlæggelser` = c(12, 8, 15, 11, 9, 13, 7, 10),
    `Samlede indlæggelser` = c(150, 145, 160, 155, 148, 158, 142, 152),
    `Måletarget (%)` = rep(8, 8),
    `Kommentar` = c("", "", "Ferieperiode", "", "", "", "Sommerferie", ""),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  config <- list(
    x_col = "Måned",
    y_col = "Genindlæggelser",
    n_col = "Samlede indlæggelser"
  )

  # TEST: Danish clinical data processing
  result <- generateSPCPlot(
    data = danish_data,
    config = config,
    chart_type = "p",
    target_value = 0.08, # 8% target
    kommentar_column = "Kommentar",
    chart_title_reactive = reactive("Genindlæggelsesrate")
  )

  expect_s3_class(result$plot, "ggplot")
  expect_true(is.data.frame(result$qic_data))

  # Verify proportions are calculated correctly
  expect_true(all(result$qic_data$y >= 0 & result$qic_data$y <= 1))

  # TEST: Target line should be visible at 8%
  expect_gte(length(result$plot$layers), 4) # Including target line

  # TEST: Comments should be processed
  expect_gte(length(result$plot$layers), 5) # Including comment annotations

  # TEST: Character month labels should be converted to factors
  expect_true(is.factor(result$qic_data$x))
})