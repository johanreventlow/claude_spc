# test-critical-fixes-regression.R
# Regression tests for kritiske fixes - forhindrer gentagelse af specifikke bugs
# Baseret på commit db5a3ba fixes

# Setup ----------------------------------------------------------------
source(file.path("..", "..", "global.R"))

test_that("OBSERVER_PRIORITIES dobbelt-definition regression test", {
  # REGRESSION: Forhindrer at OBSERVER_PRIORITIES defineres flere steder
  # Bug: Tidligere var priorities defineret i både config_system_config.R og config_observer_priorities.R

  # Test: Verify kun én autoritativ definition
  expect_true(exists("OBSERVER_PRIORITIES"),
              info = "OBSERVER_PRIORITIES skal eksistere")

  # Test: Search for potential duplicate definitions
  # Note: Dette kan kun testes statisk ved build-time, men vi tester struktur her

  priority_source_count <- 0

  # Check for structure consistency (indikator for single source)
  required_structure <- c("STATE_MANAGEMENT", "AUTO_DETECT", "DATA_PROCESSING",
                         "UI_SYNC", "PLOT_GENERATION", "STATUS_UPDATES",
                         "CLEANUP", "LOGGING")

  all_present <- all(required_structure %in% names(OBSERVER_PRIORITIES))
  expect_true(all_present,
              info = "Alle required priorities skal være tilgængelige fra single source")

  # Test: Verify legacy aliases are properly mapped (ikke gendefinserted)
  legacy_mappings <- list(
    "HIGH" = "STATE_MANAGEMENT",
    "MEDIUM" = "DATA_PROCESSING",
    "LOW" = "UI_SYNC",
    "LOWEST" = "CLEANUP"
  )

  for (alias in names(legacy_mappings)) {
    expected_target <- legacy_mappings[[alias]]
    expect_equal(OBSERVER_PRIORITIES[[alias]], OBSERVER_PRIORITIES[[expected_target]],
                 info = paste("Alias", alias, "skal mappe til", expected_target))
  }
})

test_that("Logging API details parameter backward compatibility regression", {
  # REGRESSION: Sikr at tilføjelse af details parameter ikke bryder eksisterende kald
  # Bug: Nye parametre kan bryde eksisterende log calls

  # Test: Old-style logging calls skal stadig virke
  expect_no_error({
    log_warn("Simple warning uden details")
    log_info("Simple info uden details")
    log_error("Simple error uden details")
  }, info = "Legacy logging calls uden details skal virke")

  # Test: .context parameter skal stadig virke
  expect_no_error({
    log_warn("Warning med .context", .context = "REGRESSION_TEST")
    log_info("Info med .context", .context = "REGRESSION_TEST")
    log_error("Error med .context", .context = "REGRESSION_TEST")
  }, info = "Legacy .context parameter skal virke")

  # Test: Mixed old og new style calls
  expect_no_error({
    log_warn("Mixed call", .context = "OLD_STYLE")
    log_info(
      message = "New style call",
      component = "[NEW_STYLE]",
      details = list(test = "value")
    )
  }, info = "Mixed logging styles skal være kompatible")

  # Test: Parameter order flexibility
  expect_no_error({
    log_error(
      component = "[ORDER_TEST]",
      message = "Parameter order test",
      details = list(order = "flexible")
    )
    log_warn(
      details = list(first = "details"),
      message = "Details first test",
      component = "[ORDER_TEST]"
    )
  }, info = "Parameter order skal være fleksibel")
})

test_that("Input sanitization regex double brackets regression", {
  # REGRESSION: Forhindrer dobbelte kantede parenteser i regex patterns
  # Bug: allowed_chars havde "[A-Za-z...]" i stedet for "A-Za-z..."

  # Test: Verify korrekt regex pattern anvendelse
  test_inputs <- c(
    "ValidInput123",
    "Dansk_æøå_tekst",
    "Input.med-punktum",
    "Input med mellemrum"
  )

  for (input in test_inputs) {
    result <- sanitize_user_input(input, html_escape = FALSE)

    # Should preserve all valid characters
    expect_equal(result, input,
                 info = paste("Valid input skal bevares:", input))
  }

  # Test: Verify problematic characters fjernes korrekt
  problematic_inputs <- list(
    "Test@email.com" = "Testemail.com",
    "Input&with&special" = "Inputwithspecial",
    "Html<script>tag" = "Htmlscripttag",
    "Path/with\\slashes" = "Pathwithslashes"
  )

  for (input in names(problematic_inputs)) {
    expected <- problematic_inputs[[input]]
    result <- sanitize_user_input(input, html_escape = FALSE)

    expect_equal(result, expected,
                 info = paste("Problematic input skal sanitiseres korrekt:", input))
  }

  # Test: Verify danske karakterer bevares specifikt
  danish_test <- "Størrelse før ændring af æøå karakterer"
  result <- sanitize_user_input(danish_test, html_escape = FALSE)
  expect_true(grepl("Størrelse", result), "Størrelse skal bevares")
  expect_true(grepl("før", result), "før skal bevares")
  expect_true(grepl("ændring", result), "ændring skal bevares")
  expect_true(grepl("æøå", result), "æøå skal bevares")
})

test_that("DESCRIPTION dependencies alignment regression", {
  # REGRESSION: Sikr at DESCRIPTION Imports matcher faktiske pkg::function() calls
  # Bug: Missing imports can cause failures in clean environments

  # Read DESCRIPTION file og extract Imports
  desc_file <- file.path("..", "..", "DESCRIPTION")
  skip_if_not(file.exists(desc_file), "DESCRIPTION file not found")

  desc_content <- readLines(desc_file)
  imports_line <- grep("^Imports:", desc_content, value = TRUE)

  # Extract import packages (basic parsing)
  if (length(imports_line) > 0) {
    # Find all lines until next section
    imports_start <- grep("^Imports:", desc_content)
    imports_end <- grep("^[A-Z][a-z]+:", desc_content)
    imports_end <- imports_end[imports_end > imports_start][1]

    if (is.na(imports_end)) imports_end <- length(desc_content)

    imports_text <- paste(desc_content[imports_start:(imports_end-1)], collapse = " ")
    imports_text <- gsub("Imports:\\s*", "", imports_text)

    # Extract package names (simplified)
    packages <- gsub("\\s*\\([^)]+\\)", "", imports_text)
    packages <- strsplit(packages, ",")[[1]]
    packages <- trimws(packages)
    packages <- packages[packages != ""]

    # Test: Critical packages should be imported
    critical_packages <- c("shiny", "DT", "dplyr", "ggplot2", "qicharts2",
                          "purrr", "stringr", "readr", "testthat")

    for (pkg in critical_packages) {
      expect_true(pkg %in% packages,
                  info = paste("Critical package skal være i Imports:", pkg))
    }
  }
})

test_that("Event system priority consistency regression", {
  # REGRESSION: Verify at event system bruger konsistente priorities
  # Bug: Inconsistent priority usage can cause race conditions

  skip_if_not(exists("OBSERVER_PRIORITIES"), "OBSERVER_PRIORITIES not available")

  # Test: Critical events skal bruge HIGH priorities
  high_priority_events <- c("STATE_MANAGEMENT", "DATA_PROCESSING")
  medium_priority_events <- c("AUTO_DETECT", "UI_SYNC")
  low_priority_events <- c("CLEANUP", "LOGGING", "STATUS_UPDATES")

  # Test priority ordering
  for (i in 1:(length(high_priority_events)-1)) {
    current <- OBSERVER_PRIORITIES[[high_priority_events[i]]]
    next_event <- OBSERVER_PRIORITIES[[high_priority_events[i+1]]]
    expect_gte(current, next_event,
               info = paste("High priority events skal have højere værdier:",
                           high_priority_events[i], "vs", high_priority_events[i+1]))
  }

  # Test: Medium priorities skal være lavere end high
  for (high_event in high_priority_events) {
    for (medium_event in medium_priority_events) {
      expect_gt(OBSERVER_PRIORITIES[[high_event]], OBSERVER_PRIORITIES[[medium_event]],
                info = paste(high_event, "skal have højere prioritet end", medium_event))
    }
  }

  # Test: Low priorities skal være lavest
  for (medium_event in medium_priority_events) {
    for (low_event in low_priority_events) {
      expect_gt(OBSERVER_PRIORITIES[[medium_event]], OBSERVER_PRIORITIES[[low_event]],
                info = paste(medium_event, "skal have højere prioritet end", low_event))
    }
  }
})

test_that("File extension validation security regression", {
  # REGRESSION: Prevent malicious file extensions
  # Bug: Weak file validation can allow dangerous uploads

  # Test: Dangerous extensions skal rejectes
  dangerous_extensions <- c(
    "exe", "bat", "cmd", "com", "scr", "vbs", "js", "jar",
    "php", "asp", "jsp", "py", "pl", "sh", "ps1"
  )

  for (ext in dangerous_extensions) {
    result <- validate_file_extension(ext)
    expect_false(result,
                 info = paste("Dangerous extension skal rejectes:", ext))

    # Test med dots
    result_with_dot <- validate_file_extension(paste0(".", ext))
    expect_false(result_with_dot,
                 info = paste("Dangerous extension med dot skal rejectes:", ext))
  }

  # Test: Safe extensions skal accepteres
  safe_extensions <- c("csv", "xlsx", "xls", "CSV", "XLSX", "XLS")

  for (ext in safe_extensions) {
    result <- validate_file_extension(ext)
    expect_true(result,
                info = paste("Safe extension skal accepteres:", ext))
  }

  # Test: Case insensitivity
  mixed_case_extensions <- c("Csv", "XLSX", "xLs")
  for (ext in mixed_case_extensions) {
    result <- validate_file_extension(ext)
    expect_true(result,
                info = paste("Mixed case extension skal accepteres:", ext))
  }
})

test_that("Logging level configuration consistency regression", {
  # REGRESSION: Sikr logging level configuration virker konsistent
  # Bug: Inconsistent logging levels can hide critical errors

  # Store original log level
  original_level <- Sys.getenv("SPC_LOG_LEVEL", "INFO")

  # Test: Level setting functions
  expect_no_error({
    set_log_level_development()
    current_level <- get_log_level_name()
    expect_equal(current_level, "DEBUG")
  })

  expect_no_error({
    set_log_level_production()
    current_level <- get_log_level_name()
    expect_equal(current_level, "WARN")
  })

  expect_no_error({
    set_log_level_quiet()
    current_level <- get_log_level_name()
    expect_equal(current_level, "ERROR")
  })

  # Test: Custom level setting
  expect_no_error({
    set_log_level("INFO")
    current_level <- get_log_level_name()
    expect_equal(current_level, "INFO")
  })

  # Test: Invalid level handling
  expect_error(set_log_level("INVALID"),
               info = "Invalid log level skal give error")

  # Restore original level
  if (nzchar(original_level)) {
    Sys.setenv(SPC_LOG_LEVEL = original_level)
  } else {
    Sys.unsetenv("SPC_LOG_LEVEL")
  }
})

test_that("Safe operation fallback execution regression", {
  # REGRESSION: Verify safe_operation fallback bliver kaldt korrekt
  # Bug: Fallback functions might not execute properly on errors

  skip_if_not(exists("safe_operation"), "safe_operation function not available")

  # Test: Fallback execution på error
  fallback_executed <- FALSE
  fallback_parameter_received <- NULL

  result <- safe_operation(
    operation_name = "Test fallback execution",
    code = {
      stop("Intentional test error")
    },
    fallback = function(error_obj) {
      fallback_executed <<- TRUE
      fallback_parameter_received <<- error_obj
      return("FALLBACK_SUCCESS")
    }
  )

  expect_true(fallback_executed,
              info = "Fallback skal blive executed på error")
  expect_equal(result, "FALLBACK_SUCCESS",
               info = "Fallback return value skal bruges")
  expect_true(inherits(fallback_parameter_received, "simpleError"),
              info = "Fallback skal modtage error object som parameter")

  # Test: No fallback på success
  fallback_executed_success <- FALSE

  result_success <- safe_operation(
    operation_name = "Test successful operation",
    code = {
      return("SUCCESS")
    },
    fallback = function(error_obj) {
      fallback_executed_success <<- TRUE
      return("SHOULD_NOT_EXECUTE")
    }
  )

  expect_false(fallback_executed_success,
               info = "Fallback skal ikke execute på success")
  expect_equal(result_success, "SUCCESS",
               info = "Success value skal returneres direkte")
})