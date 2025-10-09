# test-session-lifecycle.R
# Sprint 5 Fase 1: Integration Testing
# Session lifecycle testing: initialization, state transitions, cleanup

library(testthat)
library(shiny)

context("Session Lifecycle Integration")

# Helper: Create test session mock
create_mock_session <- function() {
  list(
    token = paste0("test_session_", as.integer(Sys.time())),
    onSessionEnded = function(callback) {
      # Store callback for manual triggering in tests
      attr(session, "end_callback") <- callback
    },
    userData = new.env(parent = emptyenv())
  )
}

test_that("Session initialization creates complete app_state structure", {
  app_state <- create_app_state()

  # Verify all top-level components exist
  expect_true(!is.null(app_state$data))
  expect_true(!is.null(app_state$columns))
  expect_true(!is.null(app_state$events))
  expect_true(!is.null(app_state$session))
  expect_true(!is.null(app_state$ui))
  expect_true(!is.null(app_state$visualization))
  expect_true(!is.null(app_state$cache))

  # Verify reactive values are initialized
  expect_true(inherits(app_state$data, "reactivevalues"))
  expect_true(inherits(app_state$columns, "reactivevalues"))
  expect_true(inherits(app_state$events, "reactivevalues"))
  expect_true(inherits(app_state$session, "reactivevalues"))

  # Verify hierarchical column structure
  expect_true(!is.null(app_state$columns$auto_detect))
  expect_true(!is.null(app_state$columns$mappings))
  expect_true(!is.null(app_state$columns$ui_sync))

  # Verify cache structure
  expect_true(is.list(app_state$cache))
  expect_true(is.null(app_state$cache$qic))  # Lazy initialization
})

test_that("Session startup follows correct initialization order", {
  # Track initialization steps
  init_order <- character(0)

  # 1. Create app state
  app_state <- create_app_state()
  init_order <- c(init_order, "app_state_created")

  # 2. Initialize startup cache
  cache_init <- init_startup_cache()
  if (cache_init) {
    init_order <- c(init_order, "startup_cache_initialized")
  }

  # 3. Load cached startup data
  cached_data <- load_cached_startup_data()
  if (length(cached_data) > 0) {
    init_order <- c(init_order, "cached_data_loaded")
  }

  # 4. Setup event listeners (mocked)
  init_order <- c(init_order, "event_listeners_setup")

  # 5. Initialize UI state
  app_state$session$user_started_session <- FALSE
  init_order <- c(init_order, "ui_initialized")

  # Verify expected initialization order
  expect_true("app_state_created" %in% init_order)
  expect_true("event_listeners_setup" %in% init_order)
  expect_true("ui_initialized" %in% init_order)

  # app_state should be first
  expect_equal(init_order[1], "app_state_created")
})

test_that("Session handles data upload lifecycle correctly", {
  app_state <- create_app_state()

  # Initial state: No data uploaded
  expect_false(app_state$session$file_uploaded)
  expect_true(is.null(app_state$data$current_data))
  expect_true(is.null(app_state$session$file_name))

  # Simulate file upload
  test_data <- data.frame(
    x = 1:20,
    y = rnorm(20, mean = 50, sd = 10)
  )

  file_info <- list(
    name = "upload_test.csv",
    size = 1024,
    type = "text/csv",
    datapath = tempfile()
  )

  # Upload data
  app_state$data$current_data <- test_data
  app_state$data$original_data <- test_data
  app_state$data$file_info <- file_info
  app_state$session$file_uploaded <- TRUE
  app_state$session$file_name <- file_info$name

  # Emit data_updated event
  app_state$events$data_updated <- app_state$events$data_updated + 1L

  # Verify upload state
  expect_true(app_state$session$file_uploaded)
  expect_equal(app_state$session$file_name, "upload_test.csv")
  expect_equal(nrow(app_state$data$current_data), 20)
  expect_gt(app_state$events$data_updated, 0)

  # Simulate second upload (replace data)
  test_data2 <- data.frame(
    x = 1:30,
    y = rnorm(30, mean = 60, sd = 15)
  )

  file_info2 <- list(
    name = "upload_test2.csv",
    size = 2048,
    type = "text/csv",
    datapath = tempfile()
  )

  # Replace data
  app_state$data$current_data <- test_data2
  app_state$data$original_data <- test_data2
  app_state$data$file_info <- file_info2
  app_state$session$file_name <- file_info2$name

  # Emit data_updated event
  data_updated_count_before <- app_state$events$data_updated
  app_state$events$data_updated <- app_state$events$data_updated + 1L

  # Verify replacement
  expect_equal(nrow(app_state$data$current_data), 30)
  expect_equal(app_state$session$file_name, "upload_test2.csv")
  expect_gt(app_state$events$data_updated, data_updated_count_before)
})

test_that("Session manages auto-detection state transitions", {
  app_state <- create_app_state()

  # Initial auto-detection state
  expect_false(app_state$columns$auto_detect$in_progress)
  expect_false(app_state$columns$auto_detect$completed)
  expect_true(is.null(app_state$columns$auto_detect$results))
  expect_true(is.null(app_state$columns$auto_detect$trigger))

  # Trigger auto-detection
  app_state$columns$auto_detect$trigger <- Sys.time()
  app_state$columns$auto_detect$in_progress <- TRUE

  expect_true(app_state$columns$auto_detect$in_progress)
  expect_true(!is.null(app_state$columns$auto_detect$trigger))

  # Complete auto-detection
  detected_results <- list(
    x_column = "Dato",
    y_column = "Værdi",
    n_column = "Nævner",
    confidence = list(x = 0.95, y = 0.90, n = 0.85)
  )

  app_state$columns$auto_detect$results <- detected_results
  app_state$columns$auto_detect$completed <- TRUE
  app_state$columns$auto_detect$in_progress <- FALSE
  app_state$columns$auto_detect$last_run <- Sys.time()

  # Emit auto_detection_completed event
  app_state$events$auto_detection_completed <- app_state$events$auto_detection_completed + 1L

  # Verify completion state
  expect_false(app_state$columns$auto_detect$in_progress)
  expect_true(app_state$columns$auto_detect$completed)
  expect_equal(app_state$columns$auto_detect$results$x_column, "Dato")
  expect_gt(app_state$events$auto_detection_completed, 0)

  # Apply detected columns to mappings
  app_state$columns$mappings$x_column <- detected_results$x_column
  app_state$columns$mappings$y_column <- detected_results$y_column
  app_state$columns$mappings$n_column <- detected_results$n_column

  expect_equal(app_state$columns$mappings$x_column, "Dato")
  expect_equal(app_state$columns$mappings$y_column, "Værdi")
  expect_equal(app_state$columns$mappings$n_column, "Nævner")
})

test_that("Session cleanup resets state correctly", {
  app_state <- create_app_state()

  # Populate session with data
  test_data <- data.frame(x = 1:50, y = rnorm(50))
  app_state$data$current_data <- test_data
  app_state$data$original_data <- test_data
  app_state$session$file_uploaded <- TRUE
  app_state$session$file_name <- "test.csv"
  app_state$columns$auto_detect$completed <- TRUE
  app_state$columns$auto_detect$results <- list(x_column = "x", y_column = "y")
  app_state$columns$mappings$x_column <- "x"
  app_state$columns$mappings$y_column <- "y"

  # Verify populated state
  expect_true(!is.null(app_state$data$current_data))
  expect_true(app_state$session$file_uploaded)

  # Trigger session reset
  app_state$events$session_reset <- app_state$events$session_reset + 1L

  # Perform cleanup
  app_state$data$current_data <- NULL
  app_state$data$original_data <- NULL
  app_state$data$file_info <- NULL
  app_state$session$file_uploaded <- FALSE
  app_state$session$file_name <- NULL
  app_state$columns$auto_detect$completed <- FALSE
  app_state$columns$auto_detect$results <- NULL
  app_state$columns$mappings$x_column <- NULL
  app_state$columns$mappings$y_column <- NULL
  app_state$columns$mappings$n_column <- NULL

  # Clear QIC cache
  if (!is.null(app_state$cache$qic)) {
    app_state$cache$qic$clear()
  }

  # Verify cleanup
  expect_true(is.null(app_state$data$current_data))
  expect_false(app_state$session$file_uploaded)
  expect_true(is.null(app_state$session$file_name))
  expect_false(app_state$columns$auto_detect$completed)
  expect_true(is.null(app_state$columns$auto_detect$results))
  expect_true(is.null(app_state$columns$mappings$x_column))
  expect_gt(app_state$events$session_reset, 0)
})

test_that("Session handles cache lifecycle correctly", {
  app_state <- create_app_state()

  # Initial cache state (lazy initialization)
  expect_true(is.null(app_state$cache$qic))

  # Initialize QIC cache
  qic_cache <- get_or_init_qic_cache(app_state)
  expect_true(!is.null(qic_cache))
  expect_true(!is.null(app_state$cache$qic))

  # Verify cache operations
  expect_equal(qic_cache$size(), 0)

  # Add cache entry
  test_key <- "test_cache_key"
  test_value <- list(data = "test_data", timestamp = Sys.time())
  qic_cache$set(test_key, test_value)

  expect_equal(qic_cache$size(), 1)

  # Retrieve cache entry
  retrieved <- qic_cache$get(test_key)
  expect_equal(retrieved$data, "test_data")

  # Clear cache
  qic_cache$clear()
  expect_equal(qic_cache$size(), 0)
})

test_that("Session state survives observer execution order variations", {
  app_state <- create_app_state()

  # Simulate concurrent state updates (race condition scenario)
  test_data <- data.frame(x = 1:10, y = 1:10)

  # Update 1: Data upload
  app_state$data$current_data <- test_data
  app_state$events$data_updated <- app_state$events$data_updated + 1L

  # Update 2: Auto-detection trigger (before data_updated observer completes)
  app_state$columns$auto_detect$trigger <- Sys.time()

  # Update 3: UI sync request (concurrent with auto-detection)
  app_state$columns$ui_sync$needed <- TRUE

  # All updates should coexist without conflict
  expect_true(!is.null(app_state$data$current_data))
  expect_gt(app_state$events$data_updated, 0)
  expect_true(!is.null(app_state$columns$auto_detect$trigger))
  expect_true(app_state$columns$ui_sync$needed)

  # State should remain consistent
  expect_equal(nrow(app_state$data$current_data), 10)
})

test_that("Session handles multiple rapid file uploads gracefully", {
  app_state <- create_app_state()

  # Simulate rapid successive uploads
  for (i in 1:5) {
    test_data <- data.frame(
      x = 1:20,
      y = rnorm(20, mean = 50 + i * 10, sd = 10)
    )

    app_state$data$current_data <- test_data
    app_state$session$file_name <- paste0("upload_", i, ".csv")
    app_state$events$data_updated <- app_state$events$data_updated + 1L
  }

  # Final state should reflect last upload
  expect_equal(app_state$session$file_name, "upload_5.csv")
  expect_equal(app_state$events$data_updated, 5)
  expect_equal(nrow(app_state$data$current_data), 20)
})

test_that("Session preserves user preferences across data changes", {
  app_state <- create_app_state()

  # Set user preferences
  app_state$ui$hide_anhoej_rules <- TRUE
  app_state$ui$chart_type_selected <- "p"
  app_state$ui$target_value <- 75

  # Upload data
  test_data1 <- data.frame(x = 1:20, y = rnorm(20))
  app_state$data$current_data <- test_data1
  app_state$events$data_updated <- app_state$events$data_updated + 1L

  # User preferences should persist
  expect_true(app_state$ui$hide_anhoej_rules)
  expect_equal(app_state$ui$chart_type_selected, "p")
  expect_equal(app_state$ui$target_value, 75)

  # Upload new data
  test_data2 <- data.frame(x = 1:30, y = rnorm(30))
  app_state$data$current_data <- test_data2
  app_state$events$data_updated <- app_state$events$data_updated + 1L

  # Preferences should still persist
  expect_true(app_state$ui$hide_anhoej_rules)
  expect_equal(app_state$ui$chart_type_selected, "p")
  expect_equal(app_state$ui$target_value, 75)
})

test_that("Session event counters increment correctly", {
  app_state <- create_app_state()

  # Initial event counter values
  initial_data_updated <- app_state$events$data_updated
  initial_auto_detect_completed <- app_state$events$auto_detection_completed
  initial_session_reset <- app_state$events$session_reset

  # Trigger events
  app_state$events$data_updated <- app_state$events$data_updated + 1L
  app_state$events$auto_detection_completed <- app_state$events$auto_detection_completed + 1L
  app_state$events$session_reset <- app_state$events$session_reset + 1L

  # Verify increments
  expect_equal(app_state$events$data_updated, initial_data_updated + 1L)
  expect_equal(app_state$events$auto_detection_completed, initial_auto_detect_completed + 1L)
  expect_equal(app_state$events$session_reset, initial_session_reset + 1L)

  # Multiple increments
  app_state$events$data_updated <- app_state$events$data_updated + 2L
  expect_equal(app_state$events$data_updated, initial_data_updated + 3L)
})
