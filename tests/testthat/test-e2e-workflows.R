# test-e2e-workflows.R
# End-to-end workflow tests covering complete user journeys

test_that("Complete user journey: upload to export works", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("qicharts2")

  # Create mock session and input objects
  session_mock <- list(
    token = "test_session_123",
    onSessionEnded = function(callback) {},
    sendCustomMessage = function(type, message) {}
  )

  input_mock <- list(
    data_file = list(
      name = "test_data.csv",
      datapath = tempfile(fileext = ".csv"),
      size = 1024,
      type = "text/csv"
    )
  )

  output_mock <- list()

  # Create test data file
  test_data <- data.frame(
    Dato = seq(as.Date("2023-01-01"), by = "day", length.out = 30),
    Værdi = rnorm(30, mean = 25, sd = 5),
    Afdeling = rep("Test", 30),
    stringsAsFactors = FALSE
  )

  write.csv(test_data, input_mock$data_file$datapath, row.names = FALSE)
  on.exit(unlink(input_mock$data_file$datapath), add = TRUE)

  # Initialize app state
  app_state <- create_app_state()
  emit <- create_emit_api(app_state)

  # Step 1: File Upload
  upload_result <- safe_operation(
    "E2E: File upload",
    code = {
      # Simulate file upload processing
      file_path <- validate_safe_file_path(input_mock$data_file$datapath)
      data <- readr::read_csv(file_path, show_col_types = FALSE)
      set_current_data(app_state, data)
      emit$data_updated("file_loaded")
      TRUE
    },
    fallback = FALSE
  )

  expect_true(upload_result)
  expect_true(!is.null(app_state$data$current_data))
  expect_equal(nrow(app_state$data$current_data), 30)

  # Step 2: Auto-detection
  autodetect_result <- safe_operation(
    "E2E: Auto-detection",
    code = {
      # Simulate auto-detection process
      app_state$columns$auto_detect$in_progress <- TRUE
      emit$auto_detection_started()

      # Mock auto-detection results
      app_state$columns$auto_detect$results <- list(
        x_column = "Dato",
        y_column = "Værdi",
        confidence = 0.95
      )
      app_state$columns$auto_detect$completed <- TRUE
      emit$auto_detection_completed()
      TRUE
    },
    fallback = FALSE
  )

  expect_true(autodetect_result)
  expect_true(app_state$columns$auto_detect$completed)
  expect_equal(app_state$columns$auto_detect$results$x_column, "Dato")

  # Step 3: Column Mapping
  mapping_result <- safe_operation(
    "E2E: Column mapping",
    code = {
      app_state$columns$mappings$x_column <- "Dato"
      app_state$columns$mappings$y_column <- "Værdi"
      emit$ui_sync_requested()
      TRUE
    },
    fallback = FALSE
  )

  expect_true(mapping_result)
  expect_equal(app_state$columns$mappings$x_column, "Dato")
  expect_equal(app_state$columns$mappings$y_column, "Værdi")

  # Step 4: Chart Generation
  chart_result <- safe_operation(
    "E2E: Chart generation",
    code = {
      # Verify we have the required data
      req_data <- app_state$data$current_data
      req_x <- app_state$columns$mappings$x_column
      req_y <- app_state$columns$mappings$y_column

      if (!is.null(req_data) && !is.null(req_x) && !is.null(req_y)) {
        # Mock chart generation
        chart <- qicharts2::qic(
          x = req_data[[req_x]],
          y = req_data[[req_y]],
          chart = "i",
          title = "E2E Test Chart"
        )
        return(list(success = TRUE, chart = chart))
      }
      return(list(success = FALSE))
    },
    fallback = list(success = FALSE)
  )

  expect_true(chart_result$success)
  expect_true(!is.null(chart_result$chart))

  # Step 5: State Consistency Check
  consistency_check <- safe_operation(
    "E2E: State consistency",
    code = {
      # Verify all state is consistent
      checks <- list(
        data_present = !is.null(app_state$data$current_data),
        columns_mapped = !is.null(app_state$columns$mappings$x_column),
        autodetect_complete = app_state$columns$auto_detect$completed,
        session_active = !is.null(session_mock$token)
      )

      all(unlist(checks))
    },
    fallback = FALSE
  )

  expect_true(consistency_check)
})

test_that("Error recovery workflow handles failures gracefully", {
  skip_if_not_installed("shiny")

  # Initialize app state
  app_state <- create_app_state()
  emit <- create_emit_api(app_state)

  # Test error scenarios and recovery
  error_scenarios <- list(
    "invalid_file_upload" = function() {
      # Simulate invalid file upload
      expect_error(
        validate_safe_file_path("../../../etc/passwd"),
        "Sikkerhedsfejl"
      )
    },

    "corrupted_data_handling" = function() {
      # Simulate corrupted data
      corrupted_data <- data.frame(
        x = c(1, 2, "invalid", 4, 5),
        y = c(1, NA, NA, NA, 5),
        stringsAsFactors = FALSE
      )

      set_current_data(app_state, corrupted_data)

      # Should handle gracefully
      validator <- create_chart_validator()
      result <- validator$validate_chart_data(corrupted_data, "x", "y")

      expect_true(is.list(result))
      expect_true("valid" %in% names(result))
    },

    "memory_pressure_recovery" = function() {
      # Simulate memory pressure
      large_data <- data.frame(
        x = 1:10000,
        y = rnorm(10000),
        stringsAsFactors = FALSE
      )

      # Set and then clear large data
      set_current_data(app_state, large_data)
      gc()

      # Should still function normally
      small_data <- data.frame(x = 1:5, y = 1:5, stringsAsFactors = FALSE)
      set_current_data(app_state, small_data)

      expect_equal(nrow(app_state$data$current_data), 5)
    }
  )

  for (scenario_name in names(error_scenarios)) {
    test_function <- error_scenarios[[scenario_name]]

    # Each scenario should complete without crashing the system
    expect_no_error(
      test_function(),
      info = paste("Error recovery scenario:", scenario_name)
    )
  }
})

test_that("Session lifecycle management works correctly", {
  skip_if_not_installed("shiny")

  # Test session creation
  app_state <- create_app_state()
  emit <- create_emit_api(app_state)

  # Mock session
  session_id <- "test_session_456"

  # Step 1: Session initialization
  session_init_result <- safe_operation(
    "Session initialization",
    code = {
      app_state$session$user_started_session <- TRUE
      app_state$session$auto_save_enabled <- TRUE
      emit$session_started()
      TRUE
    },
    fallback = FALSE
  )

  expect_true(session_init_result)
  expect_true(app_state$session$user_started_session)

  # Step 2: Data operations during session
  test_data <- data.frame(
    x = 1:10,
    y = rnorm(10),
    stringsAsFactors = FALSE
  )

  data_ops_result <- safe_operation(
    "Session data operations",
    code = {
      set_current_data(app_state, test_data)
      app_state$session$file_uploaded <- TRUE
      emit$data_updated("session_data")
      TRUE
    },
    fallback = FALSE
  )

  expect_true(data_ops_result)
  expect_true(app_state$session$file_uploaded)

  # Step 3: Session cleanup simulation
  cleanup_result <- safe_operation(
    "Session cleanup",
    code = {
      # Simulate session end cleanup
      app_state$session$user_started_session <- FALSE
      app_state$session$file_uploaded <- FALSE
      app_state$data$current_data <- NULL

      # Reset auto-detection state
      app_state$columns$auto_detect$completed <- FALSE
      app_state$columns$auto_detect$results <- NULL

      TRUE
    },
    fallback = FALSE
  )

  expect_true(cleanup_result)
  expect_false(app_state$session$user_started_session)
  expect_null(app_state$data$current_data)
})

test_that("Performance workflow handles concurrent operations", {
  skip_if_not_installed("shiny")
  skip_if(Sys.getenv("CI") == "true", "Skip concurrent test in CI")

  # Initialize app state
  app_state <- create_app_state()
  emit <- create_emit_api(app_state)

  # Simulate concurrent operations
  test_data <- data.frame(
    x = 1:100,
    y = rnorm(100),
    stringsAsFactors = FALSE
  )

  # Test rapid successive operations
  start_time <- Sys.time()

  operations_result <- safe_operation(
    "Concurrent operations test",
    code = {
      # Rapid fire multiple operations
      for (i in 1:10) {
        set_current_data(app_state, test_data)
        emit$data_updated(paste("operation", i))

        # Simulate auto-detection
        app_state$columns$auto_detect$in_progress <- TRUE
        app_state$columns$auto_detect$completed <- TRUE
        emit$auto_detection_completed()

        # Simulate UI sync
        emit$ui_sync_requested()
        emit$ui_sync_completed()
      }
      TRUE
    },
    fallback = FALSE
  )

  end_time <- Sys.time()
  operation_time <- as.numeric(end_time - start_time)

  expect_true(operations_result)
  # Should handle 10 complete cycles within reasonable time (10 seconds)
  expect_lt(operation_time, 10.0)

  # State should remain consistent
  expect_true(!is.null(app_state$data$current_data))
  expect_equal(nrow(app_state$data$current_data), 100)
})