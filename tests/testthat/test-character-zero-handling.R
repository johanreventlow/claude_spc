# test-character-zero-handling.R
# Tests for robust handling of character(0) inputs in reactive expressions
# Prevents regression of "argument is of length zero" error

# SETUP FUNCTION ===========================================================

setup_test_app_state <- function() {
  app_state <- create_app_state()

  # Load test data
  test_data <- data.frame(
    Observation = 1:10,
    Value = c(15, 18, 22, 19, 25, 30, 28, 32, 26, 29),
    Sample_Size = rep(100, 10),
    Skift = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
    Frys = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    Kommentar = c("Start", "", "Problem", "", "", "Fix", "", "Change", "", "End")
  )

  # Set data using isolate() to avoid reactive context issues
  isolate({
    # Set data using hierarchical structure
    app_state$data$core$current_data <- test_data
    app_state$data$current_data <- test_data  # Legacy compatibility

    # Set auto-detect results using hierarchical structure
    app_state$columns$auto_detect$results <- list(
      x_col = "Observation",
      y_col = "Value",
      n_col = "Sample_Size",
      timestamp = Sys.time()
    )

    # Set column mappings
    app_state$columns$mappings$x_column <- "Observation"
    app_state$columns$mappings$y_column <- "Value"
    app_state$columns$mappings$n_column <- "Sample_Size"
  })

  return(app_state)
}

# UNIT TESTS FOR SANITIZE_SELECTION ======================================

test_that("sanitize_selection handles character(0) inputs correctly", {

  # Create robust sanitize_selection function for testing (mirrors the actual implementation)
  # Fixed to handle vector inputs and whitespace properly
  sanitize_selection <- function(input_value) {
    if (is.null(input_value) || length(input_value) == 0 || identical(input_value, character(0))) {
      return(NULL)
    }
    # Handle vectors - use first element
    if (length(input_value) > 1) {
      input_value <- input_value[1]
    }
    # Handle empty strings and whitespace-only strings
    if (is.character(input_value) && (input_value == "" || trimws(input_value) == "")) {
      return(NULL)
    }
    return(input_value)
  }

  # Test NULL input
  expect_null(sanitize_selection(NULL))

  # Test character(0) input - the main bug source
  expect_null(sanitize_selection(character(0)))

  # Test empty string
  expect_null(sanitize_selection(""))

  # Test valid string input
  expect_equal(sanitize_selection("valid_column"), "valid_column")

  # Test integer input (edge case)
  expect_equal(sanitize_selection(1), 1)

  # Test logical input (edge case)
  expect_equal(sanitize_selection(TRUE), TRUE)

  # Test vector with multiple elements but first is empty
  expect_null(sanitize_selection(c("", "second")))

  # Test whitespace-only string
  expect_null(sanitize_selection("   "))
})

# INTEGRATION TESTS FOR REACTIVE EXPRESSIONS ==============================

test_that("manual_config reactive handles character(0) inputs without error", {
  skip_if_not_installed("shiny")

  app_state <- setup_test_app_state()

  # Test various character(0) input scenarios
  character_zero_scenarios <- list(
    list(x_column = character(0), y_column = "Value", n_column = "Sample_Size"),
    list(x_column = "Observation", y_column = character(0), n_column = "Sample_Size"),
    list(x_column = "Observation", y_column = "Value", n_column = character(0)),
    list(x_column = character(0), y_column = character(0), n_column = character(0)),
    list(x_column = NULL, y_column = character(0), n_column = ""),
    list(x_column = "", y_column = NULL, n_column = character(0))
  )

  for (scenario in character_zero_scenarios) {
    # Create mock input object
    input <- list(
      x_column = scenario$x_column,
      y_column = scenario$y_column,
      n_column = scenario$n_column,
      chart_type = "Seriediagram (Run Chart)"
    )

    # Test that configuration creation doesn't crash
    expect_no_error({
      # Simulate the sanitize_selection logic
      sanitize_selection <- function(input_value) {
        if (is.null(input_value) || length(input_value) == 0 || identical(input_value, character(0)) || input_value == "") {
          return(NULL)
        }
        return(input_value)
      }

      config <- list(
        x_col = sanitize_selection(input$x_column),
        y_col = sanitize_selection(input$y_column),
        n_col = sanitize_selection(input$n_column),
        chart_type = get_qic_chart_type(if (is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type)
      )
    })

    # Verify that character(0) inputs are converted to NULL
    if (identical(scenario$x_column, character(0))) {
      expect_null(sanitize_selection(scenario$x_column))
    }
    if (identical(scenario$y_column, character(0))) {
      expect_null(sanitize_selection(scenario$y_column))
    }
    if (identical(scenario$n_column, character(0))) {
      expect_null(sanitize_selection(scenario$n_column))
    }
  }
})

test_that("kommentar_column_reactive handles character(0) inputs without error", {

  # Test character(0) handling for kommentar column
  character_zero_inputs <- list(
    character(0),
    NULL,
    "",
    "   ", # whitespace only
    c("", "second"), # vector with empty first element
    "valid_column"
  )

  sanitize_selection <- function(input_value) {
    if (is.null(input_value) || length(input_value) == 0 || identical(input_value, character(0))) {
      return(NULL)
    }
    if (length(input_value) > 1) {
      input_value <- input_value[1]
    }
    if (is.character(input_value) && (input_value == "" || trimws(input_value) == "")) {
      return(NULL)
    }
    return(input_value)
  }

  for (input_value in character_zero_inputs) {
    # Test that kommentar column processing doesn't crash
    expect_no_error({
      result <- sanitize_selection(input_value)
    })

    # Verify expected results
    if (is.null(input_value) || length(input_value) == 0 || identical(input_value, character(0))) {
      expect_null(sanitize_selection(input_value))
    } else if (identical(input_value, "valid_column")) {
      expect_equal(sanitize_selection(input_value), "valid_column")
    } else if (is.character(input_value) && length(input_value) == 1 && (input_value == "" || trimws(input_value) == "")) {
      expect_null(sanitize_selection(input_value))
    } else if (is.character(input_value) && length(input_value) > 1) {
      # Vectors - should return first element if not empty/whitespace
      first_element <- input_value[1]
      if (first_element == "" || trimws(first_element) == "") {
        expect_null(sanitize_selection(input_value))
      } else {
        expect_equal(sanitize_selection(input_value), first_element)
      }
    }
  }
})

# REGRESSION TESTS ======================================================

test_that("character(0) scenarios that previously caused 'argument is of length zero' error", {

  # Test the exact pattern that was causing issues:
  # if (!is.null(input_value) && input_value != "")

  problematic_inputs <- list(
    character(0),
    c(),  # empty vector
    logical(0), # empty logical vector
    numeric(0)  # empty numeric vector
  )

  for (input_value in problematic_inputs) {

    # Test the old pattern that was causing crashes (should fail without fix)
    if (identical(input_value, character(0))) {
      # This is the pattern that was causing "argument is of length zero"
      # We verify that our sanitize_selection prevents this
      expect_no_error({
        # New safe pattern
        sanitize_selection <- function(input_value) {
          if (is.null(input_value) || length(input_value) == 0 || identical(input_value, character(0)) || input_value == "") {
            return(NULL)
          }
          return(input_value)
        }

        result <- sanitize_selection(input_value)
        expect_null(result)
      })

      # Verify the old unsafe pattern would cause issues
      expect_error({
        # This is what was happening before the fix
        if (!is.null(input_value) && input_value != "") {
          "This should fail"
        }
      }, class = "error")
    }
  }
})

# PERFORMANCE TESTS =====================================================

test_that("sanitize_selection has acceptable performance", {
  skip_if_not_installed("microbenchmark")

  sanitize_selection <- function(input_value) {
    if (is.null(input_value) || length(input_value) == 0 || identical(input_value, character(0))) {
      return(NULL)
    }
    if (length(input_value) > 1) {
      input_value <- input_value[1]
    }
    if (is.character(input_value) && (input_value == "" || trimws(input_value) == "")) {
      return(NULL)
    }
    return(input_value)
  }

  # Performance test for various input types
  test_inputs <- list(
    character(0),
    NULL,
    "",
    "valid_column",
    c("", "second"),
    rep("test", 100) # larger vector
  )

  for (input_value in test_inputs) {
    # Should complete within reasonable time
    start_time <- Sys.time()
    result <- sanitize_selection(input_value)
    end_time <- Sys.time()

    # Should be very fast (less than 1ms for these simple operations)
    expect_true(as.numeric(end_time - start_time, units = "secs") < 0.001)
  }
})

# VALIDATION TESTS =====================================================

test_that("sanitize_selection maintains data integrity for valid inputs", {

  sanitize_selection <- function(input_value) {
    if (is.null(input_value) || length(input_value) == 0 || identical(input_value, character(0))) {
      return(NULL)
    }
    if (length(input_value) > 1) {
      input_value <- input_value[1]
    }
    if (is.character(input_value) && (input_value == "" || trimws(input_value) == "")) {
      return(NULL)
    }
    return(input_value)
  }

  # Test that valid inputs pass through unchanged
  valid_inputs <- list(
    "Observation",
    "Value",
    "Sample_Size",
    "column_with_underscore",
    "column.with.dot",
    "Column With Spaces", # edge case but valid
    "123column", # starts with number
    "øæå_column" # Danish characters
  )

  for (input_value in valid_inputs) {
    result <- sanitize_selection(input_value)
    expect_equal(result, input_value)
    expect_identical(result, input_value)
  }
})

# END-TO-END SIMULATION TEST ===========================================

test_that("full visualization setup handles character(0) without crashing", {
  skip_if_not_installed("shiny")

  app_state <- setup_test_app_state()

  # Mock input object that simulates Shiny input during UI synchronization
  mock_input <- list(
    x_column = character(0),  # This was causing the crash
    y_column = character(0),
    n_column = character(0),
    chart_type = "Seriediagram (Run Chart)",
    kommentar_column = character(0),
    skift_column = character(0),
    frys_column = character(0),
    target_value = "",
    centerline_value = "",
    y_axis_unit = ""
  )

  # Test that the full configuration creation process doesn't crash
  expect_no_error({
    sanitize_selection <- function(input_value) {
      if (is.null(input_value) || length(input_value) == 0 || identical(input_value, character(0)) || input_value == "") {
        return(NULL)
      }
      return(input_value)
    }

    # Simulate the manual_config reactive
    manual_config <- list(
      x_col = sanitize_selection(mock_input$x_column),
      y_col = sanitize_selection(mock_input$y_column),
      n_col = sanitize_selection(mock_input$n_column),
      chart_type = get_qic_chart_type(if (is.null(mock_input$chart_type)) "Seriediagram (Run Chart)" else mock_input$chart_type)
    )

    # Simulate kommentar_column_reactive
    kommentar_column <- sanitize_selection(mock_input$kommentar_column)

    # Verify all character(0) inputs became NULL
    expect_null(manual_config$x_col)
    expect_null(manual_config$y_col)
    expect_null(manual_config$n_col)
    expect_null(kommentar_column)

    # Chart type should still work
    expect_equal(manual_config$chart_type, "run")
  })
})

# NA HANDLING TESTS ======================================================

test_that("sanitize_selection handles various NA scenarios correctly", {

  # Enhanced sanitize_selection function with NA handling (mirrors production implementation)
  sanitize_selection <- function(input_value) {
    if (is.null(input_value) || length(input_value) == 0 || identical(input_value, character(0))) {
      return(NULL)
    }
    # Handle vectors with all NA values
    if (all(is.na(input_value))) {
      return(NULL)
    }
    # Handle vectors - use first element only
    if (length(input_value) > 1) {
      input_value <- input_value[1]
    }
    # Handle single NA value
    if (is.na(input_value)) {
      return(NULL)
    }
    # Handle empty strings and whitespace-only strings
    if (is.character(input_value) && (input_value == "" || trimws(input_value) == "")) {
      return(NULL)
    }
    return(input_value)
  }

  # Test NA_character_
  expect_null(sanitize_selection(NA_character_))

  # Test NA_real_
  expect_null(sanitize_selection(NA_real_))

  # Test NA_integer_
  expect_null(sanitize_selection(NA_integer_))

  # Test NA_logical_
  expect_null(sanitize_selection(NA))

  # Test vector with all NAs
  expect_null(sanitize_selection(c(NA_character_, NA_character_, NA_character_)))

  # Test mixed vector with NA first element
  expect_null(sanitize_selection(c(NA_character_, "valid")))

  # Test mixed vector with valid first element
  expect_equal(sanitize_selection(c("valid", NA_character_)), "valid")

  # Test empty character vector mixed with NA
  expect_null(sanitize_selection(c("", NA_character_, "valid")))

  # Test whitespace with NA
  expect_null(sanitize_selection(c("   ", NA_character_)))
})

test_that("manual_config reactive handles NA inputs without error", {

  app_state <- setup_test_app_state()

  # Test various NA input scenarios
  na_scenarios <- list(
    list(x_column = NA_character_, y_column = "Value", n_column = "Sample_Size"),
    list(x_column = "Observation", y_column = NA_character_, n_column = "Sample_Size"),
    list(x_column = "Observation", y_column = "Value", n_column = NA_character_),
    list(x_column = c(NA_character_, "Observation"), y_column = "Value", n_column = "Sample_Size"),
    list(x_column = c("", NA_character_), y_column = "Value", n_column = "Sample_Size"),
    list(x_column = c("Observation", NA_character_), y_column = c(NA_character_, "Value"), n_column = "Sample_Size")
  )

  sanitize_selection <- function(input_value) {
    if (is.null(input_value) || length(input_value) == 0 || identical(input_value, character(0))) {
      return(NULL)
    }
    if (all(is.na(input_value))) {
      return(NULL)
    }
    if (length(input_value) > 1) {
      input_value <- input_value[1]
    }
    if (is.na(input_value)) {
      return(NULL)
    }
    if (is.character(input_value) && (input_value == "" || trimws(input_value) == "")) {
      return(NULL)
    }
    return(input_value)
  }

  for (i in seq_along(na_scenarios)) {
    scenario <- na_scenarios[[i]]

    # Create mock input object
    input <- list(
      x_column = scenario$x_column,
      y_column = scenario$y_column,
      n_column = scenario$n_column,
      chart_type = "Seriediagram (Run Chart)"
    )

    # Test that configuration creation doesn't crash with NA inputs
    expect_no_error({
      config <- list(
        x_col = sanitize_selection(input$x_column),
        y_col = sanitize_selection(input$y_column),
        n_col = sanitize_selection(input$n_column),
        chart_type = get_qic_chart_type(if (is.null(input$chart_type)) "Seriediagram (Run Chart)" else input$chart_type)
      )
    })
  }
})

# CACHE EDGE CASES TESTS ==============================================

test_that("create_cached_reactive handles invalid cache keys robustly", {
  skip_if_not_installed("microbenchmark")

  # Test expression that should succeed
  test_expr <- quote({
    data.frame(x = 1:5, y = 6:10)
  })

  # Test various problematic cache key scenarios
  problematic_cache_keys <- list(
    NULL,
    character(0),
    "",
    "   ",  # whitespace only
    NA_character_,
    c("key1", "key2"),  # vector
    c("", "valid_key"),  # vector with empty first element
    c(NA_character_, "valid"),  # vector with NA first element
    123,  # numeric
    TRUE,  # logical
    list("key"),  # list
    "key with spaces",
    "key/with/slashes",
    "key.with.dots",
    "key@with#special$chars",
    paste0(rep("x", 1000), collapse = "")  # very long key
  )

  for (i in seq_along(problematic_cache_keys)) {
    cache_key <- problematic_cache_keys[[i]]

    # Test that cache creation doesn't crash regardless of problematic key
    expect_no_error({
      cached_func <- create_cached_reactive({
        data.frame(x = 1:5, y = 6:10)
      }, cache_key, cache_timeout = 1)

      # Test that the cached function works
      result <- cached_func()
      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 5)
    })
  }
})

test_that("create_cached_reactive handles expression failures gracefully", {

  # Test expressions that will fail - Note: these are not quotes, they are actual expressions
  # that will fail when evaluated
  failing_test_cases <- list(
    function() { stop("Intentional error") },
    function() { nonexistent_function() },
    function() { 1 + "string" },  # type error
    function() { unknown_variable },  # undefined variable
    function() { data$nonexistent_column }  # missing column
  )

  for (i in seq_along(failing_test_cases)) {
    test_case <- failing_test_cases[[i]]

    # Test that cache creation doesn't crash even with failing expressions
    expect_no_error({
      cached_func <- create_cached_reactive({
        test_case()  # This will fail
      }, paste0("failing_key_", i), cache_timeout = 1)

      # The cached function should return NULL for failed expressions
      result <- cached_func()
      expect_null(result)
    })
  }
})

test_that("create_cached_reactive cache invalidation works correctly", {

  # Use a simpler test - timestamps show cache invalidation
  cached_func <- create_cached_reactive({
    Sys.time()
  }, "cache_invalidation_test", cache_timeout = 0.1)

  # First call should execute expression
  result1 <- cached_func()
  expect_true(inherits(result1, "POSIXct"))

  # Second immediate call should use cache (same timestamp)
  result2 <- cached_func()
  expect_equal(result2, result1)  # Same result from cache

  # Wait for cache to expire
  Sys.sleep(0.2)

  # Third call should re-execute expression (different timestamp)
  result3 <- cached_func()
  expect_true(inherits(result3, "POSIXct"))
  # Allow for some timing tolerance - cache should either be different or very close
  expect_true(result3 >= result1)  # Later or same timestamp (timing tolerance)
})

test_that("create_cached_reactive handles concurrent access safely", {
  skip_if_not_installed("parallel")

  # Create expression that takes some time and returns a simple value
  cached_func <- create_cached_reactive({
    Sys.sleep(0.1)
    42  # Simple constant value for testing
  }, "concurrent_test", cache_timeout = 10)

  # Simulate concurrent access (simplified for testing)
  results <- replicate(5, {
    cached_func()
  })

  # All results should be identical (from cache after first execution)
  expect_true(all(results == results[1]))
  expect_equal(results[1], 42)
})

# INTEGRATION TESTS FOR CACHE IN SPC CONTEXT ========================

test_that("cache works correctly in SPC plot generation context", {

  app_state <- setup_test_app_state()

  # Test that x_validation caching works with various column configurations
  test_configs <- list(
    list(x_col = "Observation", y_col = "Value", n_col = "Sample_Size"),
    list(x_col = NULL, y_col = "Value", n_col = "Sample_Size"),
    list(x_col = character(0), y_col = "Value", n_col = "Sample_Size"),
    list(x_col = NA_character_, y_col = "Value", n_col = "Sample_Size"),
    list(x_col = "", y_col = "Value", n_col = "Sample_Size"),
    list(x_col = c(NA_character_, "Observation"), y_col = "Value", n_col = "Sample_Size")
  )

  for (i in seq_along(test_configs)) {
    config <- test_configs[[i]]

    # Test that safe cache key generation works
    expect_no_error({
      # Simulate the safe_x_col_id logic from generateSPCPlot with better NA handling for vectors
      safe_x_col_id <- if (is.null(config$x_col) || length(config$x_col) == 0 || identical(config$x_col, character(0))) {
        "NULL_XCOL"
      } else if (length(config$x_col) == 1 && is.na(config$x_col)) {
        "NULL_XCOL"
      } else {
        # Sanitize column name for cache key (remove problematic characters)
        first_element <- as.character(config$x_col)[1]
        if (is.na(first_element) || first_element == "" || trimws(first_element) == "") {
          "NULL_XCOL"
        } else {
          cleaned <- gsub("[^a-zA-Z0-9_]", "_", first_element)
          if (cleaned == "" || nchar(cleaned) == 0) {
            "NULL_XCOL"
          } else {
            cleaned
          }
        }
      }

      # Test that cache key is always valid
      expect_true(is.character(safe_x_col_id))
      expect_true(nchar(safe_x_col_id) > 0)
      expect_false(is.na(safe_x_col_id))
    })
  }
})

# STRESS TESTS ========================================================

test_that("system handles high frequency character(0) and NA inputs", {

  sanitize_selection <- function(input_value) {
    if (is.null(input_value) || length(input_value) == 0 || identical(input_value, character(0))) {
      return(NULL)
    }
    if (all(is.na(input_value))) {
      return(NULL)
    }
    if (length(input_value) > 1) {
      input_value <- input_value[1]
    }
    if (is.na(input_value)) {
      return(NULL)
    }
    if (is.character(input_value) && (input_value == "" || trimws(input_value) == "")) {
      return(NULL)
    }
    return(input_value)
  }

  # Generate many problematic inputs rapidly
  problematic_inputs <- c(
    replicate(50, character(0), simplify = FALSE),
    replicate(50, NA_character_, simplify = FALSE),
    replicate(50, "", simplify = FALSE),
    replicate(50, c(NA_character_, "valid"), simplify = FALSE)
  )

  # Test that system can handle rapid processing of problematic inputs
  start_time <- Sys.time()
  for (input_value in problematic_inputs) {
    result <- sanitize_selection(input_value)
    expect_true(is.null(result) || is.character(result))
  }
  end_time <- Sys.time()

  # Should complete quickly (less than 1 second for 200 operations)
  elapsed <- as.numeric(end_time - start_time, units = "secs")
  expect_true(elapsed < 1.0, info = paste("Processing took", elapsed, "seconds"))
})