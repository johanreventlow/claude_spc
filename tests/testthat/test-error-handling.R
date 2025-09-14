# test-error-handling.R
# Tests af error handling funktioner

test_that("log_error fungerer korrekt", {
  # Test basic logging (uden session) - forventer output
  expect_output(log_error("Test besked", level = "info"), "INFO")
  expect_output(log_error("Test warning", level = "warning"), "WARNING")
  expect_output(log_error("Test error", level = "error"), "ERROR")
  
  # Test med invalid level bruger level som givet
  expect_output(log_error("Test unknown", level = "unknown"), "UNKNOWN")
})

test_that("safe_operation håndterer fejl korrekt", {
  # Test succesfuld operation
  result <- safe_operation(
    "test operation",
    code = 2 + 2,
    fallback = 0
  )
  expect_equal(result, 4)
  
  # Test operation der fejler
  result <- safe_operation(
    "failing operation", 
    code = stop("Test fejl"),
    fallback = "fallback_value"
  )
  expect_equal(result, "fallback_value")
  
  # Test operation med NULL fallback
  result <- safe_operation(
    "failing operation",
    code = stop("Test fejl"),
    fallback = NULL
  )
  expect_null(result)
})

test_that("observer_manager fungerer", {
  # Test observer manager creation
  manager <- observer_manager()
  
  # Test count starter på 0
  expect_equal(manager$count(), 0)
  
  # Test tilføjelse af mock observer
  mock_observer <- list(destroy = function() NULL)
  id <- manager$add(mock_observer, "test_observer")
  
  expect_equal(manager$count(), 1)
  expect_equal(id, "test_observer")
  
  # Test fjernelse
  manager$remove("test_observer")
  expect_equal(manager$count(), 0)
  
  # Test cleanup_all
  manager$add(mock_observer, "obs1")
  manager$add(mock_observer, "obs2")
  expect_equal(manager$count(), 2)
  
  manager$cleanup_all()
  expect_equal(manager$count(), 0)
})

test_that("create_empty_session_data returnerer korrekt struktur", {
  empty_data <- create_empty_session_data()
  
  # Test at alle påkrævede kolonner er til stede
  expected_cols <- c("Skift", "Frys", "Dato", "Tæller", "Nævner", "Kommentar")
  expect_true(all(expected_cols %in% names(empty_data)))
  
  # Test at der er 20 rækker
  expect_equal(nrow(empty_data), 20)
  
  # Test at Skift og Frys er logiske
  expect_true(is.logical(empty_data$Skift))
  expect_true(is.logical(empty_data$Frys))
  
  # Test at numeriske kolonner er numeric
  expect_true(is.numeric(empty_data$Tæller))
  expect_true(is.numeric(empty_data$Nævner))
})