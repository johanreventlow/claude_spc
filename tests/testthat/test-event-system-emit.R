# test-event-system-emit.R
# Tests for event emit API domain handling

library(testthat)

create_emit_test_server <- function() {
  function(input, output, session) {
    app_state <- create_app_state()
    emit <- create_emit_api(app_state)

    session$userData$app_state <- app_state
    session$userData$emit <- emit
    session$userData$get_event_value <- function(name) {
      shiny::withReactiveDomain(session, {
        shiny::isolate(app_state$events[[name]])
      })
    }
  }
}

test_that("emit data_loaded kan trigges uden aktivt reactive domain", {
  skip_if_not_installed("shiny")

  shiny::testServer(create_emit_test_server(), {
    emit <- session$userData$emit
    get_event_value <- session$userData$get_event_value

    expect_type(emit, "list")
    expect_true("data_loaded" %in% names(emit))
    expect_true(is.function(emit$data_loaded))

    expect_error(emit$data_loaded(), NA)
    expect_equal(get_event_value("data_loaded"), 1L)
  })
})
