test_that("No autodetect on excelR table edits (column_changed)", {
  skip_if_not_installed("shiny")

  create_server <- function() {
    function(input, output, session) {
      app_state <- create_app_state()
      emit <- create_emit_api(app_state)

      # Setup unified event listeners
      setup_event_listeners(app_state, emit, input, output, session)

      session$userData$app_state <- app_state
      session$userData$emit <- emit
      session$userData$get_event <- function(name) {
        shiny::withReactiveDomain(session, {
          shiny::isolate(app_state$events[[name]])
        })
      }
    }
  }

  shiny::testServer(create_server(), {
    emit <- session$userData$emit
    get_event <- session$userData$get_event

    # Baseline: no autodetect started
    base_auto <- get_event("auto_detection_started")

    # Simulate data load context → should trigger autodetect
    emit$data_updated("file_loaded")
    after_file <- get_event("auto_detection_started")
    expect_equal(after_file, base_auto + 1L)

    # Simulate table edit context → should NOT trigger autodetect
    emit$data_updated("column_changed")
    after_edit <- get_event("auto_detection_started")
    expect_equal(after_edit, after_file)
  })
})

test_that("Manual column mapping prevents auto-detect during table edits", {
  skip_if_not_installed("shiny")

  create_server <- function() {
    function(input, output, session) {
      app_state <- create_app_state()
      emit <- create_emit_api(app_state)

      setup_event_listeners(app_state, emit, input, output, session)

      session$userData$app_state <- app_state
      session$userData$emit <- emit
      session$userData$get_event <- function(name) {
        shiny::withReactiveDomain(session, {
          shiny::isolate(app_state$events[[name]])
        })
      }
    }
  }

  shiny::testServer(create_server(), {
    app_state <- session$userData$app_state
    emit <- session$userData$emit
    get_event <- session$userData$get_event

    test_data <- data.frame(
      Dato = as.Date("2023-01-01") + 0:2,
      Tæller = c(1, 2, 3),
      Nævner = c(5, 5, 5)
    )

    set_current_data(app_state, test_data)

    session$setInputs(x_column = "Dato")
    session$setInputs(y_column = "Tæller")
    session$setInputs(n_column = "Nævner")
    session$flushReact()

    expect_true(shiny::isolate(app_state$columns$detector$user_has_mapped))

    base_auto <- get_event("auto_detection_started")

    emit$data_updated("column_changed")
    session$flushReact()

    after_edit <- get_event("auto_detection_started")
    expect_equal(after_edit, base_auto)
    expect_true(shiny::isolate(app_state$columns$detector$user_has_mapped))
    expect_null(shiny::isolate(app_state$columns$detector$reason))
  })
})

test_that("Manual auto-detect bypasses manual mapping guard", {
  skip_if_not_installed("shiny")

  create_server <- function() {
    function(input, output, session) {
      app_state <- create_app_state()
      emit <- create_emit_api(app_state)

      setup_event_listeners(app_state, emit, input, output, session)

      session$userData$app_state <- app_state
      session$userData$emit <- emit
      session$userData$get_event <- function(name) {
        shiny::withReactiveDomain(session, {
          shiny::isolate(app_state$events[[name]])
        })
      }
    }
  }

  shiny::testServer(create_server(), {
    app_state <- session$userData$app_state
    emit <- session$userData$emit
    get_event <- session$userData$get_event

    test_data <- data.frame(
      Dato = as.Date("2023-01-05") + 0:2,
      Tæller = c(4, 5, 6),
      Nævner = c(7, 8, 9)
    )

    set_current_data(app_state, test_data)

    session$setInputs(x_column = "Dato")
    session$setInputs(y_column = "Tæller")
    session$setInputs(n_column = "Nævner")
    session$flushReact()

    expect_true(shiny::isolate(app_state$columns$detector$user_has_mapped))

    base_auto <- get_event("auto_detection_started")

    emit$manual_autodetect_button()
    session$flushReact()

    after_manual <- get_event("auto_detection_started")
    expect_equal(after_manual, base_auto + 1L)
    expect_null(shiny::isolate(app_state$columns$detector$reason))
    expect_false(shiny::isolate(app_state$columns$detector$allowed))
  })
})

