# test-error-handling.R
# Tests af error handling funktioner

test_that("log_error fungerer korrekt", {
  # Temporarily set log level to DEBUG for testing
  old_level <- Sys.getenv("SPC_LOG_LEVEL", "")
  Sys.setenv(SPC_LOG_LEVEL = "DEBUG")

  # Test basic logging med nye logging system
  expect_output(log_error("Test besked", "TEST"), "ERROR.*TEST.*Test besked")
  expect_output(log_info("Test info", "TEST"), "INFO.*TEST.*Test info")
  expect_output(log_warn("Test warning", "TEST"), "WARN.*TEST.*Test warning")
  expect_output(log_debug("Test debug", "TEST"), "DEBUG.*TEST.*Test debug")

  # Restore original log level
  if (old_level == "") {
    Sys.unsetenv("SPC_LOG_LEVEL")
  } else {
    Sys.setenv(SPC_LOG_LEVEL = old_level)
  }
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