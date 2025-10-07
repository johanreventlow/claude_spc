# Test script til at undersøge forskelle mellem baseline og målværdi håndtering
# Dette script sammenligner direkte hvordan parse_danish_target fungerer for begge felter

# Load via pkgload or fall back to global.R
if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(here::here(), quiet = TRUE)
} else {
  source('../../global.R')
}

# Test data som kan give forskellige resultater
test_inputs <- c("80%", "0.8", "80", "68,5%", "3,14", "120")
test_y_data_decimal <- c(0.7, 0.8, 0.9, 0.75, 0.82)
test_y_data_percent <- c(70, 80, 90, 75, 82)
test_y_unit <- "percent"

cat("=== REPRODUKTION AF BASELINE VS MÅLVÆRDI PROBLEM ===\n\n")

cat("Test Y-data (decimal format):", test_y_data_decimal, "\n")
cat("Test Y-data (percent format):", test_y_data_percent, "\n")
cat("Y-axis unit:", test_y_unit, "\n\n")

for (input_val in test_inputs) {
  cat("Input:", input_val, "\n")

  # Simulér target_value_reactive (målværdi)
  target_result_decimal <- parse_danish_target(input_val, parse_danish_number(test_y_data_decimal), test_y_unit)
  target_result_percent <- parse_danish_target(input_val, parse_danish_number(test_y_data_percent), test_y_unit)
  target_result_no_data <- parse_danish_target(input_val, NULL, test_y_unit)

  # Simulér centerline_value_reactive (baseline) - skulle være identisk
  baseline_result_decimal <- parse_danish_target(input_val, parse_danish_number(test_y_data_decimal), test_y_unit)
  baseline_result_percent <- parse_danish_target(input_val, parse_danish_number(test_y_data_percent), test_y_unit)
  baseline_result_no_data <- parse_danish_target(input_val, NULL, test_y_unit)

  cat("  Målværdi (decimal Y-data):", target_result_decimal, "\n")
  cat("  Baseline (decimal Y-data):", baseline_result_decimal, "\n")
  cat("  MATCH:", identical(target_result_decimal, baseline_result_decimal), "\n")

  cat("  Målværdi (percent Y-data):", target_result_percent, "\n")
  cat("  Baseline (percent Y-data):", baseline_result_percent, "\n")
  cat("  MATCH:", identical(target_result_percent, baseline_result_percent), "\n")

  cat("  Målværdi (no Y-data):", target_result_no_data, "\n")
  cat("  Baseline (no Y-data):", baseline_result_no_data, "\n")
  cat("  MATCH:", identical(target_result_no_data, baseline_result_no_data), "\n")

  cat("\n")
}

cat("=== DIREKTE SAMMENLIGNING AF REACTIVE LOGIK ===\n\n")

# Simulér den faktiske reactive logik fra fct_visualization_server.R
simulate_reactive_logic <- function(input_value, field_name) {
  # Mock app_state og column_config
  mock_data <- data.frame(
    Værdi = test_y_data_percent
  )
  mock_config <- list(y_col = "Værdi")
  mock_y_unit <- test_y_unit

  if (is.null(input_value) || input_value == "") {
    return(NULL)
  }

  # Hent y-akse data og enhed til smart konvertering (som i fct_visualization_server.R)
  data <- mock_data
  config <- mock_config
  y_unit <- mock_y_unit

  if (!is.null(data) && !is.null(config) && !is.null(config$y_col) && config$y_col %in% names(data)) {
    y_data <- data[[config$y_col]]
    y_numeric <- parse_danish_number(y_data)
    result <- parse_danish_target(input_value, y_numeric, y_unit)
    cat(field_name, "med Y-data:", result, "\n")
    return(result)
  } else {
    result <- parse_danish_target(input_value, NULL, y_unit)
    cat(field_name, "uden Y-data:", result, "\n")
    return(result)
  }
}

for (input_val in test_inputs[1:3]) {  # Test kun de første 3
  cat("\nTesting input:", input_val, "\n")
  target_result <- simulate_reactive_logic(input_val, "Target")
  baseline_result <- simulate_reactive_logic(input_val, "Baseline")
  cat("Resultater identiske:", identical(target_result, baseline_result), "\n")
}

cat("\n=== KONKLUSION ===\n")
cat("Hvis resultater er identiske, er problemet ikke i parse_danish_target funktionen.\n")
cat("Problemet kan være:\n")
cat("1. Forskellig timing af reactive updates\n")
cat("2. Forskellige Y-data tilgængelige under beregning\n")
cat("3. Forskellig column_config tilgængelig\n")
cat("4. Race condition mellem felterne\n")