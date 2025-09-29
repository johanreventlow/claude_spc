test_that("No autodetect on excelR table edits (table_cells_edited)", {
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
    app_state <- session$userData$app_state

    # Baseline: no autodetect started
    base_auto <- get_event("auto_detection_started")
    base_nav <- get_event("navigation_changed")

    # Simulate data load context → should trigger autodetect
    emit$data_updated("file_loaded")
    after_file <- get_event("auto_detection_started")
    expect_equal(after_file, base_auto + 1L)

    # Simulate table edit context → should NOT trigger autodetect
    emit$data_updated("table_cells_edited")
    after_edit <- get_event("auto_detection_started")
    expect_equal(after_edit, after_file)

    after_nav <- get_event("navigation_changed")
    expect_equal(after_nav, base_nav + 1L)
  })
})

test_that("n_column stays cleared during table edit refresh", {
  skip_if_not_installed("shiny")

  create_server <- function() {
    function(input, output, session) {
      app_state <- create_app_state()
      emit <- create_emit_api(app_state)

      # Preload data for column updates
      app_state$data$current_data <- data.frame(
        Dato = as.Date(c("2024-01-01", "2024-01-02")),
        Tæller = c(10, 12),
        Nævner = c(100, 110)
      )

      # Seed historical mapping to mimic previous autodetection
      app_state$columns$mappings$n_column <- "Nævner"
      app_state$columns$n_column <- "Nævner"

      setup_event_listeners(app_state, emit, input, output, session)

      session$userData$app_state <- app_state
      session$userData$emit <- emit
    }
  }

  shiny::testServer(create_server(), {
    emit <- session$userData$emit
    app_state <- session$userData$app_state

    # Populate choices (session context avoids heavy autodetect)
    emit$data_updated("session_data")
    session$flushReact()

    expect_equal(session$input$n_column, "Nævner")

    # User clears denominator field
    session$setInputs(n_column = "")
    session$flushReact()

    expect_equal(session$input$n_column, "")
    expect_equal(app_state$columns$n_column, "")
    expect_equal(shiny::isolate(app_state$ui_cache$n_column_input), "")

    # Mapping still holds historical value
    expect_equal(app_state$columns$mappings$n_column, "Nævner")

    # Table edit only triggers plot refresh (no dropdown refresh)
    emit$data_updated("table_cells_edited")
    session$flushReact()

    expect_equal(session$input$n_column, "")
    expect_equal(app_state$columns$n_column, "")
    expect_equal(shiny::isolate(app_state$ui_cache$n_column_input), "")
  })
})

