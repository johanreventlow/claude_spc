# test-critical-fixes-security.R
# Supplerende sikkerhedstests til kritiske fixes
# Fokus på edge cases, security og integration testing som ikke er dækket i test-critical-fixes.R

# Setup ----------------------------------------------------------------
# Use helper.R's pkgload setup instead of sourcing global.R
# helper.R is automatically loaded by testthat

test_that("Input sanitization forhindrer SQL injection patterns", {
  # Test SQL injection prevention - kritisk sikkerhedsmål
  sql_patterns <- c(
    "'; DROP TABLE users; --",
    "1' OR '1'='1",
    "admin'/*",
    "UNION SELECT * FROM sensitive_data",
    "'; INSERT INTO users VALUES('hacker', 'admin'); --",
    "1; DELETE FROM data WHERE id=1; --"
  )

  for (pattern in sql_patterns) {
    result <- sanitize_user_input(pattern, html_escape = FALSE)

    # Verify farlige SQL keywords er fjernet eller neutraliseret
    expect_false(grepl("DROP|INSERT|DELETE|UNION|SELECT", result, ignore.case = TRUE),
                 info = paste("SQL injection pattern skal neutraliseres:", pattern))

    # Verify semi-colon statements er håndteret
    expect_false(grepl(";", result, fixed = TRUE),
                 info = paste("SQL statement terminators skal fjernes:", pattern))

    # Verify comment indicators er fjernet
    expect_false(grepl("--|\\/\\*", result),
                 info = paste("SQL comment indicators skal fjernes:", pattern))
  }
})

test_that("Input sanitization forhindrer path traversal attacks", {
  # Test path traversal prevention - kritisk for file operations
  path_traversal_patterns <- c(
    "../../../etc/passwd",
    "..\\\\windows\\\\system32\\\\config",
    "%2e%2e%2f%2e%2e%2f%2e%2e%2f",
    "....//....//....//",
    "..\\\\..\\\\..\\\\",
    "/var/log/../../etc/shadow"
  )

  for (pattern in path_traversal_patterns) {
    result <- sanitize_user_input(pattern, html_escape = FALSE)

    # Verify dot-dot sequences er fjernet
    expect_false(grepl("\\.\\.|%2e%2e", result, ignore.case = TRUE),
                 info = paste("Path traversal dots skal fjernes:", pattern))

    # Verify slash sequences er begrænsede
    expect_false(grepl("/{2,}|\\\\{2,}", result),
                 info = paste("Multiple slashes skal normaliseres:", pattern))
  }
})

test_that("Input sanitization håndterer Unicode edge cases", {
  # Test Unicode handling - vigtig for danske tegn og internationale data

  # Test emoji og special Unicode som kan bruges til obfuscation
  emoji_input <- "Test 😀🎉 data med emoji"
  result1 <- sanitize_user_input(emoji_input, html_escape = FALSE)
  expect_equal(result1, "Test  data med emoji",
               info = "Emoji skal fjernes men bevare spacing")

  # Test Unicode normalization consistency
  unicode_combined <- "cafe\u0301"  # café med combining accent
  unicode_composed <- "café"       # café som single characters
  result1 <- sanitize_user_input(unicode_combined, html_escape = FALSE)
  result2 <- sanitize_user_input(unicode_composed, html_escape = FALSE)
  expect_equal(result1, result2,
               info = "Unicode normalization skal være konsistent")

  # Test zero-width og control characters
  zero_width_input <- "Test\u200B\u200C\u200D\uFEFFdata"
  result <- sanitize_user_input(zero_width_input, html_escape = FALSE)
  expect_equal(result, "Testdata",
               info = "Zero-width og control characters skal fjernes")

  # Test potentielt farlige Unicode ranges
  dangerous_unicode <- "Test\u2028\u2029\u0085data"  # Line/paragraph separators
  result <- sanitize_user_input(dangerous_unicode, html_escape = FALSE)
  expect_false(grepl("[\u2028\u2029\u0085]", result),
               info = "Farlige Unicode separators skal fjernes")
})

test_that("Column name sanitization håndterer kliniske data patterns", {
  # Test realistiske kolonnenavne fra SPC/klinisk kontekst

  clinical_columns <- c(
    "Antal_indlæggelser_før_intervention",
    "Læge-patient_ratio_2024",
    "Måling (enheds-specifik)",
    "Data/resultater & kommentarer",
    "Tid_i_timer:minutter",
    "90%_percentil_værdi",
    "CPR-nummer_anonymiseret"
  )

  for (col_name in clinical_columns) {
    result <- sanitize_column_name(col_name)

    # Should preserve meaningful parts
    expect_true(nchar(result) > 0,
                info = paste("Column name should not be empty:", col_name))

    # Should preserve Danish characters
    expect_true(all(stringr::str_detect(result, "[æøåÆØÅ]") ==
                   stringr::str_detect(col_name, "[æøåÆØÅ]")),
                info = paste("Danish characters should be preserved:", col_name))

    # Should handle special characters safely
    expect_false(grepl("[()&%:]", result),
                 info = paste("Special characters should be sanitized:", col_name))
  }
})

test_that("Logging API performance under load", {
  # Test performance regression - kritisk for production stability
  #
  # Performance skalering baseret på miljø:
  # - Lokal development: 1.0s (baseline target)
  # - CI/automatiseret: 2.5s (accounting for shared resources)
  # - Generisk Unix/Windows: 2.0s (moderate overhead)
  #
  # Rationale: CI-systemer og delte miljøer har typisk:
  # - Delt CPU/IO med andre jobs
  # - Virtualisering overhead
  # - Potentielt langsommere disk I/O
  # - Variable systembelastning

  # Detect execution environment
  is_ci <- isTRUE(as.logical(Sys.getenv("CI", "FALSE"))) ||
           isTRUE(as.logical(Sys.getenv("GITHUB_ACTIONS", "FALSE"))) ||
           isTRUE(as.logical(Sys.getenv("GITLAB_CI", "FALSE"))) ||
           nzchar(Sys.getenv("JENKINS_URL"))

  # Determine platform characteristics
  platform <- Sys.info()[["sysname"]]
  is_windows <- platform == "Windows"

  # Calculate performance threshold based on environment
  base_threshold <- 1.0  # Baseline: 500 logs in 1 second

  if (is_ci) {
    # CI environments: 2.5x baseline (most lenient)
    time_threshold <- base_threshold * 2.5
    environment_label <- "CI/Automated"
  } else if (is_windows) {
    # Windows: 2x baseline (filesystem typically slower)
    time_threshold <- base_threshold * 2.0
    environment_label <- "Windows Development"
  } else {
    # Local development (Unix-like): baseline target
    time_threshold <- base_threshold
    environment_label <- "Local Development"
  }

  # Setup performance tracking
  start_time <- Sys.time()

  # Test sustained logging load
  for (i in 1:500) {
    log_debug(
      message = "Performance test iteration",
      component = "[PERFORMANCE_TEST]",
      details = list(
        iteration = i,
        timestamp = Sys.time(),
        sample_data = runif(5),
        text_data = paste("Sample text", i)
      )
    )
  }

  duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  # Performance requirement: Scaled based on environment
  expect_lt(
    duration,
    time_threshold,
    info = sprintf(
      "500 structured log calls should complete within %.1fs on %s (actual: %.2fs)",
      time_threshold,
      environment_label,
      duration
    )
  )

  # Test memory efficiency - ingen store memory leaks
  gc_before <- gc()
  start_memory <- sum(gc_before[,2])

  for (i in 1:100) {
    log_warn(
      message = "Memory test",
      component = "[MEMORY_TEST]",
      details = list(large_object = rep("data", 100))
    )
  }

  gc_after <- gc()
  end_memory <- sum(gc_after[,2])
  memory_growth <- end_memory - start_memory

  # Memory growth should be reasonable (< 10MB for 100 logs)
  expect_lt(memory_growth, 10,
            info = "Memory growth should be bounded during repeated logging")
})

test_that("OBSERVER_PRIORITIES runtime integration fungerer", {
  # Test actual observer registration og execution order

  skip_if_not(exists("reactiveVal"), message = "Shiny reactive functions not available")

  # Setup test reactive value
  test_trigger <- reactiveVal(0)
  execution_order <- character(0)

  # Register observers med forskellige priorities
  observeEvent(test_trigger(), priority = OBSERVER_PRIORITIES$STATE_MANAGEMENT, {
    execution_order <<- c(execution_order, "HIGH_PRIORITY")
  })

  observeEvent(test_trigger(), priority = OBSERVER_PRIORITIES$UI_SYNC, {
    execution_order <<- c(execution_order, "LOW_PRIORITY")
  })

  observeEvent(test_trigger(), priority = OBSERVER_PRIORITIES$CLEANUP, {
    execution_order <<- c(execution_order, "LOWEST_PRIORITY")
  })

  # Trigger execution
  isolate({
    test_trigger(1)

    # Allow observers to execute
    Sys.sleep(0.1)

    # Verify execution order respects priorities
    expect_equal(execution_order[1], "HIGH_PRIORITY",
                 info = "STATE_MANAGEMENT should execute first")
    expect_equal(execution_order[2], "LOW_PRIORITY",
                 info = "UI_SYNC should execute second")
    expect_equal(execution_order[3], "LOWEST_PRIORITY",
                 info = "CLEANUP should execute last")
  })
})

test_that("Error boundaries fungerer med structured logging", {
  # Test fejlhåndtering gennem logging system

  # Test fejl med details formatting
  expect_no_error({
    tryCatch({
      stop("Simuleret fejl")
    }, error = function(e) {
      log_error(
        message = "Error caught and logged",
        component = "[ERROR_TEST]",
        details = list(
          error_class = class(e),
          error_message = conditionMessage(e),
          stack_trace = "simulated_trace"
        )
      )
    })
  }, info = "Error logging med structured details skal ikke fejle")

  # Test circular reference handling i error details
  circular_obj <- list(data = "test")
  circular_obj$self_ref <- circular_obj

  expect_no_error({
    log_error(
      message = "Circular reference test",
      component = "[CIRCULAR_TEST]",
      details = list(
        circular_data = circular_obj,
        additional_info = "This should not break"
      )
    )
  }, info = "Circular references i details skal håndteres gracefully")
})

test_that("Input validation edge cases håndteres", {
  # Test extreme input scenarios som kan forekomme i production

  # Very long input strings (potential DoS)
  very_long_input <- paste(rep("A", 10000), collapse = "")
  result <- sanitize_user_input(very_long_input, max_length = 100)
  expect_equal(nchar(result), 100,
               info = "Very long input should be truncated to max_length")

  # NULL and edge case inputs
  expect_equal(sanitize_user_input(NULL), "",
               info = "NULL input should return empty string")
  expect_equal(sanitize_user_input(character(0)), "",
               info = "Empty character vector should return empty string")
  expect_equal(sanitize_user_input(NA_character_), "",
               info = "NA character should return empty string")

  # Mixed encoding inputs
  mixed_encoding <- "Normal text mixed with \u00e9\u00f1\u00fc special chars"
  result <- sanitize_user_input(mixed_encoding)
  expect_true(nchar(result) > 0,
               info = "Mixed encoding should be handled gracefully")

  # Binary-like input that might confuse regex
  binary_like <- "\\x00\\x01\\x02\\xff"
  result <- sanitize_user_input(binary_like, html_escape = FALSE)
  expect_equal(result, "x00x01x02xff",
               info = "Binary-like sequences should be cleaned predictably")
})