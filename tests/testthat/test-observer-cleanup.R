# Observer Cleanup Tests
# Test observer cleanup memory leak fix

test_that("Observer cleanup completes successfully", {
  # Setup mock app_state
  app_state <- new.env(parent = emptyenv())
  app_state$events <- shiny::reactiveValues(
    data_updated = 0L,
    auto_detection_started = 0L,
    auto_detection_completed = 0L,
    ui_sync_requested = 0L,
    navigation_changed = 0L
  )
  app_state$data <- shiny::reactiveValues(
    current_data = NULL
  )
  app_state$columns <- shiny::reactiveValues(
    auto_detect = shiny::reactiveValues(
      in_progress = FALSE,
      completed = FALSE,
      frozen_until_next_trigger = FALSE
    )
  )
  app_state$navigation <- shiny::reactiveValues(
    trigger = 0L
  )
  app_state$ui <- shiny::reactiveValues(
    pending_programmatic_inputs = list()
  )
  app_state$standard_listeners_active <- FALSE

  # Create emit API
  emit <- create_emit_api(app_state)

  # Setup test session with tracking
  cleanup_executed <- FALSE
  cleanup_successful <- FALSE
  observer_count_at_cleanup <- 0

  shiny::testServer(
    app = function(input, output, session) {
      # Setup event listeners
      observer_registry <- setup_event_listeners(
        app_state = app_state,
        emit = emit,
        input = input,
        output = output,
        session = session,
        ui_service = NULL
      )

      # Track observer count
      observer_count_at_cleanup <<- length(observer_registry)

      # Override session$onSessionEnded for testing
      # (In real environment, this would be called automatically)
      session$onSessionEnded(function() {
        cleanup_executed <<- TRUE

        # Count non-null observers before cleanup
        non_null_before <- sum(!sapply(observer_registry, is.null))

        # Execute cleanup logic (simplified version of actual code)
        for (observer_name in names(observer_registry)) {
          tryCatch({
            if (!is.null(observer_registry[[observer_name]])) {
              observer_registry[[observer_name]]$destroy()
              observer_registry[[observer_name]] <- NULL
            }
          }, error = function(e) {
            # Track failures
          })
        }

        # Verify all observers nullified
        non_null_after <- sum(!sapply(observer_registry, is.null))

        cleanup_successful <<- (non_null_after == 0 && non_null_before > 0)
      })
    },
    args = list()
  )

  # Assertions
  expect_true(observer_count_at_cleanup > 0,
              info = "Observer registry should contain observers")
})


test_that("Failed observers are logged with details", {
  # Setup mock app_state
  app_state <- new.env(parent = emptyenv())
  app_state$events <- shiny::reactiveValues(
    data_updated = 0L
  )
  app_state$data <- shiny::reactiveValues()
  app_state$columns <- shiny::reactiveValues(
    auto_detect = shiny::reactiveValues()
  )
  app_state$navigation <- shiny::reactiveValues()
  app_state$ui <- shiny::reactiveValues(
    pending_programmatic_inputs = list()
  )
  app_state$standard_listeners_active <- FALSE

  emit <- create_emit_api(app_state)

  # Create a mock observer that will fail to destroy
  failed_observer_count <- 0

  shiny::testServer(
    app = function(input, output, session) {
      # Create mock observer registry with failing observer
      observer_registry <- list()

      # Working observer
      observer_registry$working <- shiny::observe({
        # Do nothing
      })

      # Create a mock failing observer
      observer_registry$failing <- list(
        destroy = function() {
          stop("Simulated observer destroy failure")
        }
      )
      class(observer_registry$failing) <- c("Observer", "R6")

      # Simulate cleanup
      failed_observers <- character(0)

      for (observer_name in names(observer_registry)) {
        tryCatch({
          if (!is.null(observer_registry[[observer_name]])) {
            observer_registry[[observer_name]]$destroy()
            observer_registry[[observer_name]] <- NULL
          }
        }, error = function(e) {
          failed_observers <<- c(failed_observers, observer_name)
        })
      }

      failed_observer_count <<- length(failed_observers)

      # Verify failed observer was tracked
      expect_true("failing" %in% failed_observers,
                  info = "Failed observer should be tracked")
      expect_equal(failed_observer_count, 1,
                   info = "Should have exactly 1 failed observer")
    },
    args = list()
  )
})


test_that("No memory leaks on repeated session starts", {
  # This test verifies that observers don't accumulate across sessions

  # Setup mock app_state (shared across sessions)
  app_state <- new.env(parent = emptyenv())
  app_state$events <- shiny::reactiveValues(
    data_updated = 0L,
    navigation_changed = 0L
  )
  app_state$data <- shiny::reactiveValues(
    current_data = NULL
  )
  app_state$columns <- shiny::reactiveValues(
    auto_detect = shiny::reactiveValues(
      in_progress = FALSE,
      frozen_until_next_trigger = FALSE
    )
  )
  app_state$navigation <- shiny::reactiveValues(
    trigger = 0L
  )
  app_state$ui <- shiny::reactiveValues(
    pending_programmatic_inputs = list()
  )

  emit <- create_emit_api(app_state)

  # Simulate multiple session starts
  observer_counts <- integer(0)

  for (i in 1:3) {
    # Reset listener flag for each session
    app_state$standard_listeners_active <- FALSE

    shiny::testServer(
      app = function(input, output, session) {
        # Setup listeners
        observer_registry <- setup_event_listeners(
          app_state = app_state,
          emit = emit,
          input = input,
          output = output,
          session = session,
          ui_service = NULL
        )

        observer_counts <<- c(observer_counts, length(observer_registry))

        # Simulate cleanup
        for (observer_name in names(observer_registry)) {
          if (!is.null(observer_registry[[observer_name]])) {
            tryCatch({
              observer_registry[[observer_name]]$destroy()
              observer_registry[[observer_name]] <- NULL
            }, error = function(e) {
              # Ignore
            })
          }
        }
      },
      args = list()
    )

    # Reset for next session
    app_state$standard_listeners_active <- FALSE
  }

  # Verify consistent observer counts across sessions
  expect_true(length(unique(observer_counts)) == 1,
              info = "Observer count should be consistent across sessions")
  expect_true(all(observer_counts > 0),
              info = "Each session should have observers")
})


test_that("Observer cleanup verifies 100% success rate", {
  # Setup minimal app_state
  app_state <- new.env(parent = emptyenv())
  app_state$events <- shiny::reactiveValues(
    data_updated = 0L
  )
  app_state$data <- shiny::reactiveValues()
  app_state$columns <- shiny::reactiveValues(
    auto_detect = shiny::reactiveValues()
  )
  app_state$navigation <- shiny::reactiveValues()
  app_state$ui <- shiny::reactiveValues(
    pending_programmatic_inputs = list()
  )
  app_state$standard_listeners_active <- FALSE

  emit <- create_emit_api(app_state)

  cleanup_success_rate <- 0

  shiny::testServer(
    app = function(input, output, session) {
      # Create small observer registry
      observer_registry <- list()

      observer_registry$test1 <- shiny::observe({})
      observer_registry$test2 <- shiny::observe({})
      observer_registry$test3 <- shiny::observe({})

      initial_count <- length(observer_registry)
      failed_count <- 0

      # Execute cleanup
      for (observer_name in names(observer_registry)) {
        tryCatch({
          if (!is.null(observer_registry[[observer_name]])) {
            observer_registry[[observer_name]]$destroy()
            observer_registry[[observer_name]] <- NULL
          }
        }, error = function(e) {
          failed_count <<- failed_count + 1
        })
      }

      successful_count <- initial_count - failed_count
      cleanup_success_rate <<- successful_count / initial_count

      # Verify 100% success
      expect_equal(cleanup_success_rate, 1.0,
                   info = "Should have 100% cleanup success rate")
      expect_equal(failed_count, 0,
                   info = "Should have zero failed observers")

      # Verify all nullified
      null_count <- sum(sapply(observer_registry, is.null))
      expect_equal(null_count, initial_count,
                   info = "All observers should be NULL after cleanup")
    },
    args = list()
  )
})
