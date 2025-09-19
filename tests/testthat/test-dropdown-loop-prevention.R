# test-dropdown-loop-prevention.R
# Opdaterede tests for loop-beskyttelse og token-håndtering i UI-synk logikken

stub_update_selectize <- function(recorder) {
  function(session, inputId, choices = NULL, selected = NULL, ...) {
    recorder$push(list(
      input_id = inputId,
      choices = choices,
      selected = selected
    ))
    invisible(NULL)
  }
}

create_recorder <- function() {
  storage <- list()
  list(
    push = function(entry) {
      storage[[length(storage) + 1]] <<- entry
    },
    data = function() storage
  )
}

test_that("safe_programmatic_ui_update bruger token-baseret beskyttelse og opdaterer metrikker", {
  app_state <- create_test_app_state()

  executed <- 0L
  result <- safe_programmatic_ui_update(
    session = list(),
    app_state = app_state,
    update_function = function() {
      executed <<- executed + 1L
    },
    delay_ms = 0
  )

  expect_null(result)
  expect_equal(executed, 1L)
  expect_equal(app_state$ui$performance_metrics$total_updates, 1L)
  expect_gte(app_state$ui$performance_metrics$avg_update_duration_ms, 0)
  expect_equal(app_state$ui$programmatic_token_counter, 1L)
  expect_false(isTRUE(app_state$ui$updating_programmatically))
  expect_true(length(app_state$ui$queued_updates) >= 1L)
})

test_that("safe_programmatic_ui_update køer opdateringer når en kører", {
  app_state <- create_test_app_state()
  app_state$ui$updating_programmatically <- TRUE
  app_state$ui$memory_limits$max_queue_size <- 2L

  queued <- safe_programmatic_ui_update(
    session = list(),
    app_state = app_state,
    update_function = function() {},
    delay_ms = 10
  )

  expect_null(queued)
  expect_equal(length(app_state$ui$queued_updates), 1L)
  expect_match(app_state$ui$queued_updates[[1]]$queue_id, "queue_")
  expect_equal(app_state$ui$performance_metrics$queued_updates, 1L)
})

test_that("safe_programmatic_ui_update registrerer tokens for programatiske input", {
  app_state <- create_test_app_state()
  recorder <- create_recorder()

  # Shadow updateSelectizeInput i global env, så funktionen ikke kræver Shiny-session
  assign("updateSelectizeInput", stub_update_selectize(recorder), envir = .GlobalEnv)
  on.exit(rm("updateSelectizeInput", envir = .GlobalEnv), add = TRUE)

  safe_programmatic_ui_update(
    session = list(),
    app_state = app_state,
    update_function = function() {
      updateSelectizeInput(NULL, "x_column", choices = c("", "Dato"), selected = "Dato")
      updateSelectizeInput(NULL, "y_column", choices = c("", "Tæller"), selected = "Tæller")
    },
    delay_ms = 0
  )

  expect_gte(app_state$ui$programmatic_token_counter, 1L)

  recorded <- recorder$data()
  expect_equal(length(recorded), 2L)
  expect_equal(recorded[[1]]$selected, "Dato")
  expect_equal(recorded[[2]]$selected, "Tæller")
})
