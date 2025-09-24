# test-centerline-handling.R
# Sikrer at baseline (centerline) input anvendes identisk med målværdi

skip_if_not(exists("build_qic_arguments", mode = "function"), "SPC plot helpers ikke tilgængelige")
skip_if_not(exists("execute_qic_call", mode = "function"), "qic execution helper ikke tilgængelig")
skip_if_not(exists("prepare_qic_data_parameters", mode = "function"), "QIC data forberedelse ikke tilgængelig")
skip_if_not(exists("validate_x_column_format", mode = "function"), "X-kolonne validering mangler")
skip_if_not(exists("parse_danish_target", mode = "function"), "Dansk målværdi parser mangler")
skip_if_not(exists("prepare_qic_data_optimized", mode = "function"), "Optimized QIC data helper ikke tilgængelig")
skip_if_not(exists("preprocess_spc_data_optimized", mode = "function"), "Optimized preprocessing helper ikke tilgængelig")


get_centerline_from_qic <- function(qic_args, chart_type, config) {
  qic_result <- execute_qic_call(qic_args, chart_type = chart_type, config = config)
  unique(qic_result$cl)
}


test_that("centerline anvendes korrekt for decimale datasæt", {
  test_data <- data.frame(
    Dato = seq.Date(as.Date("2024-01-01"), by = "week", length.out = 8),
    Måling = c(0.55, 0.62, 0.58, 0.64, 0.6, 0.59, 0.63, 0.57),
    check.names = FALSE
  )

  config <- list(x_col = "Dato", y_col = "Måling", n_col = NULL)

  x_validation <- validate_x_column_format(test_data, config$x_col, "observation")
  prepared <- prepare_qic_data_parameters(test_data, config, x_validation)

  input_value <- "80%"
  target_value <- parse_danish_target(input_value, test_data$Måling, "percent")
  centerline_value <- parse_danish_target(input_value, test_data$Måling, "percent")

  expect_equal(target_value, 0.8)
  expect_equal(centerline_value, 0.8)

  qic_args <- build_qic_arguments(
    data = prepared$data,
    x_col_for_qic = prepared$x_col_for_qic,
    y_col_name = prepared$y_col_name,
    n_col_name = prepared$n_col_name,
    chart_type = "i",
    freeze_position = NULL,
    part_positions = NULL,
    centerline_value = centerline_value
  )

  centerline_result <- get_centerline_from_qic(qic_args, "i", config)
  expect_true(all(abs(centerline_result - centerline_value) < 1e-8))
})


test_that("centerline anvendes korrekt for procentdatasæt med nævner", {
  test_data <- data.frame(
    Dato = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 6),
    `Antal succes` = c(80, 82, 79, 83, 81, 84),
    `Antal total` = rep(100, 6),
    check.names = FALSE
  )

  config <- list(x_col = "Dato", y_col = "Antal succes", n_col = "Antal total")

  x_validation <- validate_x_column_format(test_data, config$x_col, "observation")
  prepared <- prepare_qic_data_parameters(test_data, config, x_validation)

  input_value <- "80%"
  target_value <- parse_danish_target(input_value, test_data$`Antal succes`, "percent")
  centerline_value <- parse_danish_target(input_value, test_data$`Antal succes`, "percent")

  expect_equal(target_value, 80)
  expect_equal(centerline_value, 80)

  qic_args <- build_qic_arguments(
    data = prepared$data,
    x_col_for_qic = prepared$x_col_for_qic,
    y_col_name = prepared$y_col_name,
    n_col_name = prepared$n_col_name,
    chart_type = "run",
    freeze_position = NULL,
    part_positions = NULL,
    centerline_value = centerline_value
  )

  centerline_result <- get_centerline_from_qic(qic_args, "run", config)
  expect_true(all(abs(centerline_result - centerline_value) < 1e-8))
})


test_that("prepare_qic_data_optimized bruger cl-parameter til baseline", {
  test_data <- data.frame(
    Dato = seq.Date(as.Date("2024-01-01"), by = "week", length.out = 10),
    Måling = c(72, 75, 71, 74, 76, 73, 75, 74, 72, 73),
    check.names = FALSE
  )

  config <- list(x_col = "Dato", y_col = "Måling", n_col = NULL)

  preprocessed <- preprocess_spc_data_optimized(test_data, config)

  centerline_value <- 74
  target_value <- 75

  qic_params <- prepare_qic_data_optimized(
    preprocessed_data = preprocessed,
    chart_type = "run",
    target_value = target_value,
    centerline_value = centerline_value,
    show_phases = FALSE,
    skift_column = NULL,
    frys_column = NULL
  )

  expect_equal(qic_params$target, target_value)
  expect_equal(qic_params$cl, centerline_value)
})


test_that("prepare_qic_data_optimized normaliserer referenceværdier for run charts med nævner", {
  test_data <- data.frame(
    Dato = seq.Date(as.Date("2024-01-01"), by = "week", length.out = 6),
    `Antal succes` = c(80, 82, 79, 83, 81, 84),
    `Antal total` = rep(100, 6),
    check.names = FALSE
  )

  config <- list(x_col = "Dato", y_col = "Antal succes", n_col = "Antal total")

  preprocessed <- preprocess_spc_data_optimized(test_data, config)

  centerline_value <- 80
  target_value <- 75

  qic_params <- prepare_qic_data_optimized(
    preprocessed_data = preprocessed,
    chart_type = "run",
    target_value = target_value,
    centerline_value = centerline_value,
    show_phases = FALSE,
    skift_column = NULL,
    frys_column = NULL
  )

  expect_equal(qic_params$target, target_value / 100)
  expect_equal(qic_params$cl, centerline_value / 100)
})
