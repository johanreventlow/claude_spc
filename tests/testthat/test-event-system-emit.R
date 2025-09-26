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

# FASE 2.1: CONSOLIDATED ERROR EVENT TESTS =====================================

test_that("Consolidated error events work correctly", {
  skip_if_not_installed("shiny")

  shiny::testServer(create_emit_test_server(), {
    app_state <- session$userData$app_state
    emit <- session$userData$emit
    get_event_value <- session$userData$get_event_value

    # Test main error_occurred function with context
    emit$error_occurred(error_type = "validation", context = "data_upload", details = list(file = "test.csv"))

    # Verify event was fired
    expect_equal(get_event_value("error_occurred"), 1L)

    # Verify error context was stored
    expect_equal(app_state$last_error_context$type, "validation")
    expect_equal(app_state$last_error_context$context, "data_upload")
    expect_true(!is.null(app_state$last_error_context$timestamp))

    # Test another error type
    emit$error_occurred(error_type = "processing", context = "qic_calculation", details = list(chart_type = "p"))
    expect_equal(get_event_value("error_occurred"), 2L)
    expect_equal(app_state$last_error_context$type, "processing")
  })
})

test_that("Legacy error functions map to consolidated event", {
  skip_if_not_installed("shiny")

  shiny::testServer(create_emit_test_server(), {
    emit <- session$userData$emit
    get_event_value <- session$userData$get_event_value
    app_state <- session$userData$app_state

    initial_count <- get_event_value("error_occurred")

    # Test legacy validation_error
    emit$validation_error()
    expect_equal(get_event_value("error_occurred"), initial_count + 1L)
    expect_equal(app_state$last_error_context$type, "validation")
    expect_equal(app_state$last_error_context$context, "legacy_validation_error")

    # Test legacy processing_error
    emit$processing_error()
    expect_equal(get_event_value("error_occurred"), initial_count + 2L)
    expect_equal(app_state$last_error_context$type, "processing")
    expect_equal(app_state$last_error_context$context, "legacy_processing_error")

    # Test legacy network_error
    emit$network_error()
    expect_equal(get_event_value("error_occurred"), initial_count + 3L)
    expect_equal(app_state$last_error_context$type, "network")
    expect_equal(app_state$last_error_context$context, "legacy_network_error")
  })
})

test_that("Recovery completed event still works independently", {
  skip_if_not_installed("shiny")

  shiny::testServer(create_emit_test_server(), {
    emit <- session$userData$emit
    get_event_value <- session$userData$get_event_value

    # Test recovery_completed event (should remain separate)
    emit$recovery_completed()
    expect_equal(get_event_value("recovery_completed"), 1L)

    # Verify it doesn't affect error_occurred counter
    expect_equal(get_event_value("error_occurred"), 0L)
  })
})

test_that("Error context storage handles edge cases", {
  skip_if_not_installed("shiny")

  shiny::testServer(create_emit_test_server(), {
    emit <- session$userData$emit
    app_state <- session$userData$app_state

    # Test with minimal parameters
    emit$error_occurred()
    expect_equal(app_state$last_error_context$type, "general")
    expect_true(is.null(app_state$last_error_context$context))
    expect_true(is.null(app_state$last_error_context$details))

    # Test with complex details
    complex_details <- list(
      nested = list(data = "test", count = 42),
      array = c(1, 2, 3),
      timestamp = Sys.time()
    )

    emit$error_occurred(
      error_type = "complex_test",
      context = "unit_testing",
      details = complex_details
    )

    expect_equal(app_state$last_error_context$details$nested$data, "test")
    expect_equal(app_state$last_error_context$details$nested$count, 42)
    expect_equal(length(app_state$last_error_context$details$array), 3)
  })
})

# FASE 2.2: CONSOLIDATED DATA EVENT TESTS =================================

test_that("Consolidated data events work correctly", {
  skip_if_not_installed("shiny")

  shiny::testServer(create_emit_test_server(), {
    app_state <- session$userData$app_state
    emit <- session$userData$emit
    get_event_value <- session$userData$get_event_value

    # Test main data_updated function with context
    emit$data_updated(context = "file_upload")

    # Verify event was fired
    expect_equal(get_event_value("data_updated"), 1L)

    # Verify data update context was stored
    expect_equal(app_state$last_data_update_context$context, "file_upload")
    expect_true(!is.null(app_state$last_data_update_context$timestamp))

    # Test another data update type
    emit$data_updated(context = "user_modification")
    expect_equal(get_event_value("data_updated"), 2L)
    expect_equal(app_state$last_data_update_context$context, "user_modification")
  })
})

test_that("Legacy data functions fire consolidated event", {
  skip_if_not_installed("shiny")

  shiny::testServer(create_emit_test_server(), {
    emit <- session$userData$emit
    get_event_value <- session$userData$get_event_value
    app_state <- session$userData$app_state

    initial_data_updated <- get_event_value("data_updated")
    initial_data_loaded <- get_event_value("data_loaded")

    # Test legacy data_loaded
    emit$data_loaded()

    # Should fire both consolidated and legacy events
    expect_equal(get_event_value("data_updated"), initial_data_updated + 1L)
    expect_equal(get_event_value("data_loaded"), initial_data_loaded + 1L)
    expect_equal(app_state$last_data_update_context$context, "legacy_data_loaded")

    # Test legacy data_changed
    initial_data_updated <- get_event_value("data_updated")
    initial_data_changed <- get_event_value("data_changed")

    emit$data_changed()

    # Should fire both consolidated and legacy events
    expect_equal(get_event_value("data_updated"), initial_data_updated + 1L)
    expect_equal(get_event_value("data_changed"), initial_data_changed + 1L)
    expect_equal(app_state$last_data_update_context$context, "legacy_data_changed")
  })
})

test_that("Data event context handling works correctly", {
  skip_if_not_installed("shiny")

  shiny::testServer(create_emit_test_server(), {
    emit <- session$userData$emit
    app_state <- session$userData$app_state

    # Test various contexts
    contexts <- c("general", "file_upload", "user_edit", "batch_import")

    for (i in seq_along(contexts)) {
      emit$data_updated(context = contexts[i])

      expect_equal(app_state$last_data_update_context$context, contexts[i])
      expect_true(!is.null(app_state$last_data_update_context$timestamp))
    }
  })
})

test_that("Data event consolidation reduces event complexity", {
  skip_if_not_installed("shiny")

  shiny::testServer(create_emit_test_server(), {
    emit <- session$userData$emit
    get_event_value <- session$userData$get_event_value

    # Test that one data_updated call can replace multiple legacy calls
    emit$data_updated(context = "comprehensive_update")

    # One consolidated event instead of separate events
    expect_equal(get_event_value("data_updated"), 1L)

    # Legacy events remain at 0 when using new API
    expect_equal(get_event_value("data_loaded"), 0L)
    expect_equal(get_event_value("data_changed"), 0L)
  })
})
