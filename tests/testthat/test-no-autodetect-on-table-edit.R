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

