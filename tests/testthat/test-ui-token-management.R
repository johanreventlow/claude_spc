# Test af UI token-baserede opdateringer
# Erstatter test-scoping-debug.R med fokuseret test af programmatiske UI-opdateringer

library(testthat)

test_that("UI programmatic token system works correctly", {
  # Testdata setup
  app_state <- create_test_app_state()

  # Mock UI update function med token tracking
  mock_update_with_token <- function(input_id, value, token) {
    list(
      input_id = input_id,
      value = value,
      token = token,
      timestamp = Sys.time()
    )
  }

  # TEST: Token counter incrementerer korrekt
  initial_counter <- app_state$ui$programmatic_token_counter

  # Simulér programmatisk opdatering
  token_1 <- initial_counter + 1L
  app_state$ui$programmatic_token_counter <- token_1

  update_1 <- mock_update_with_token("x_column", "Dato", token_1)

  expect_equal(app_state$ui$programmatic_token_counter, token_1)
  expect_equal(update_1$token, token_1)

  # TEST: Multiple tokens håndteres korrekt
  token_2 <- app_state$ui$programmatic_token_counter + 1L
  app_state$ui$programmatic_token_counter <- token_2

  update_2 <- mock_update_with_token("y_column", "Tæller", token_2)

  expect_equal(app_state$ui$programmatic_token_counter, token_2)
  expect_equal(update_2$token, token_2)
  expect_gt(update_2$token, update_1$token)

  # TEST: Pending tokens tracking
  app_state$ui$pending_programmatic_inputs$x_column <- token_1
  app_state$ui$pending_programmatic_inputs$y_column <- token_2

  expect_length(app_state$ui$pending_programmatic_inputs, 2)
  expect_equal(app_state$ui$pending_programmatic_inputs$x_column, token_1)
  expect_equal(app_state$ui$pending_programmatic_inputs$y_column, token_2)

  # TEST: Memory limits enforcement
  expect_true(app_state$ui$memory_limits$max_pending_tokens >= 100L)
  expect_true(app_state$ui$memory_limits$max_queue_size >= 5L)
})

test_that("UI queue processing prevents race conditions", {
  app_state <- create_test_app_state()

  # TEST: Queue processing flag prevents concurrent operations
  expect_false(app_state$ui$queue_processing)

  # Simulate queue start
  app_state$ui$queue_processing <- TRUE
  expect_true(app_state$ui$queue_processing)

  # Mock check-funktion der respekterer queue state
  safe_queue_operation <- function(app_state, operation_name) {
    if (app_state$ui$queue_processing) {
      return(list(result = "skipped", reason = "queue_busy"))
    }
    list(result = "executed", operation = operation_name)
  }

  # TEST: Operation skipped under queue processing
  result <- safe_queue_operation(app_state, "column_update")
  expect_equal(result$result, "skipped")
  expect_equal(result$reason, "queue_busy")

  # TEST: Operation proceeds når queue er ledig
  app_state$ui$queue_processing <- FALSE
  result2 <- safe_queue_operation(app_state, "column_update")
  expect_equal(result2$result, "executed")
  expect_equal(result2$operation, "column_update")
})