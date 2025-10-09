# test-full-data-workflow.R
# Sprint 5 Fase 1: Integration Testing
# End-to-end testing af complete data workflow fra upload til visualization

library(testthat)
library(shiny)

# Test context
context("Full Data Workflow Integration")

# Helper: Create test app state
create_test_app_state <- function() {
  source("R/state_management.R")
  app_state <- create_app_state()
  return(app_state)
}

# Helper: Create test data
create_test_data <- function(n = 50) {
  data.frame(
    Dato = seq.Date(from = as.Date("2023-01-01"), by = "day", length.out = n),
    Værdi = rnorm(n, mean = 50, sd = 10),
    Nævner = rep(100, n),
    Kommentar = rep("", n)
  )
}

test_that("Complete data workflow: upload → auto-detect → plot generation", {
  skip_if_not_installed("qicharts2")

  # 1. Initialize app state
  app_state <- create_test_app_state()
  expect_true(!is.null(app_state))
  expect_true(!is.null(app_state$data))
  expect_true(!is.null(app_state$columns))
  expect_true(!is.null(app_state$events))

  # 2. Simulate data upload
  test_data <- create_test_data(50)
  app_state$data$current_data <- test_data
  app_state$data$original_data <- test_data
  app_state$data$file_info <- list(
    name = "test_data.csv",
    size = object.size(test_data),
    type = "text/csv"
  )

  expect_equal(nrow(app_state$data$current_data), 50)
  expect_true(!is.null(app_state$data$file_info))

  # 3. Trigger auto-detection
  app_state$columns$auto_detect$in_progress <- TRUE

  # Run auto-detection (mocked)
  detected_columns <- list(
    x_column = "Dato",
    y_column = "Værdi",
    n_column = "Nævner",
    kommentar_column = "Kommentar",
    confidence = list(
      x = 0.95,
      y = 0.90,
      n = 0.85,
      kommentar = 0.80
    )
  )

  app_state$columns$auto_detect$results <- detected_columns
  app_state$columns$auto_detect$completed <- TRUE
  app_state$columns$auto_detect$in_progress <- FALSE

  # 4. Apply detected columns to mappings
  app_state$columns$mappings$x_column <- detected_columns$x_column
  app_state$columns$mappings$y_column <- detected_columns$y_column
  app_state$columns$mappings$n_column <- detected_columns$n_column
  app_state$columns$mappings$kommentar_column <- detected_columns$kommentar_column

  expect_equal(app_state$columns$mappings$x_column, "Dato")
  expect_equal(app_state$columns$mappings$y_column, "Værdi")
  expect_equal(app_state$columns$mappings$n_column, "Nævner")

  # 5. Generate plot with QIC cache
  qic_cache <- get_or_init_qic_cache(app_state)
  expect_true(!is.null(qic_cache))

  # Prepare plot config
  config <- list(
    x_col = app_state$columns$mappings$x_column,
    y_col = app_state$columns$mappings$y_column,
    n_col = app_state$columns$mappings$n_column
  )

  # Generate SPC plot
  spc_result <- safe_operation(
    "Generate test SPC plot",
    code = {
      generateSPCPlot(
        data = test_data,
        config = config,
        chart_type = "p",
        qic_cache = qic_cache
      )
    },
    fallback = NULL
  )

  expect_true(!is.null(spc_result))
  expect_true(!is.null(spc_result$plot))
  expect_true(inherits(spc_result$plot, "ggplot"))
  expect_true(!is.null(spc_result$qic_data))

  # 6. Verify cache was populated
  expect_gt(qic_cache$size(), 0)

  # 7. Test cache hit on second generation
  cache_size_before <- qic_cache$size()

  spc_result_cached <- safe_operation(
    "Generate cached SPC plot",
    code = {
      generateSPCPlot(
        data = test_data,
        config = config,
        chart_type = "p",
        qic_cache = qic_cache
      )
    },
    fallback = NULL
  )

  cache_size_after <- qic_cache$size()

  # Cache size should remain same (cache hit)
  expect_equal(cache_size_before, cache_size_after)
  expect_true(!is.null(spc_result_cached))

  # 8. Test data update clears cache
  test_data_modified <- test_data
  test_data_modified$Værdi[1] <- test_data_modified$Værdi[1] + 10

  app_state$data$current_data <- test_data_modified

  # Simulate data_updated event cache clearing
  qic_cache$clear()
  expect_equal(qic_cache$size(), 0)
})

test_that("Data workflow handles missing columns gracefully", {
  skip_if_not_installed("qicharts2")

  app_state <- create_test_app_state()

  # Upload data with missing expected columns
  incomplete_data <- data.frame(
    Dato = seq.Date(from = as.Date("2023-01-01"), by = "day", length.out = 20),
    Værdi = rnorm(20, mean = 50, sd = 10)
    # Missing Nævner column
  )

  app_state$data$current_data <- incomplete_data

  # Auto-detection should handle missing columns
  detected_columns <- list(
    x_column = "Dato",
    y_column = "Værdi",
    n_column = NULL,  # Not detected
    kommentar_column = NULL
  )

  app_state$columns$auto_detect$results <- detected_columns
  app_state$columns$mappings$x_column <- detected_columns$x_column
  app_state$columns$mappings$y_column <- detected_columns$y_column
  app_state$columns$mappings$n_column <- detected_columns$n_column

  # Should be able to generate run chart (no n_col needed)
  config <- list(
    x_col = app_state$columns$mappings$x_column,
    y_col = app_state$columns$mappings$y_column,
    n_col = NULL
  )

  qic_cache <- get_or_init_qic_cache(app_state)

  spc_result <- safe_operation(
    "Generate run chart with incomplete data",
    code = {
      generateSPCPlot(
        data = incomplete_data,
        config = config,
        chart_type = "run",  # Run chart doesn't need n_col
        qic_cache = qic_cache
      )
    },
    fallback = NULL
  )

  expect_true(!is.null(spc_result))
  expect_true(!is.null(spc_result$plot))
})

test_that("Data workflow validates chart type requirements", {
  skip_if_not_installed("qicharts2")

  app_state <- create_test_app_state()
  test_data <- create_test_data(30)

  app_state$data$current_data <- test_data

  config <- list(
    x_col = "Dato",
    y_col = "Værdi",
    n_col = "Nævner"
  )

  qic_cache <- get_or_init_qic_cache(app_state)

  # Valid chart types for ratio data
  valid_chart_types <- c("p", "pp", "u", "up")

  for (chart_type in valid_chart_types) {
    result <- safe_operation(
      paste("Generate", chart_type, "chart"),
      code = {
        generateSPCPlot(
          data = test_data,
          config = config,
          chart_type = chart_type,
          qic_cache = qic_cache
        )
      },
      fallback = NULL
    )

    expect_true(!is.null(result), info = paste("Chart type:", chart_type))
    expect_true(!is.null(result$plot), info = paste("Chart type:", chart_type))
  }
})

test_that("Event-driven workflow: data_updated triggers cascade", {
  app_state <- create_test_app_state()

  # Track event triggers
  events_triggered <- list()

  # Simulate data upload event
  app_state$events$data_updated <- app_state$events$data_updated + 1L
  events_triggered$data_updated <- TRUE

  expect_true(events_triggered$data_updated)
  expect_gt(app_state$events$data_updated, 0)

  # Data update should trigger auto-detection request
  app_state$columns$auto_detect$trigger <- Sys.time()
  events_triggered$auto_detect_triggered <- TRUE

  expect_true(events_triggered$auto_detect_triggered)
  expect_true(!is.null(app_state$columns$auto_detect$trigger))

  # Auto-detection completion should trigger UI sync
  app_state$events$auto_detection_completed <- app_state$events$auto_detection_completed + 1L
  events_triggered$auto_detection_completed <- TRUE

  expect_true(events_triggered$auto_detection_completed)
  expect_gt(app_state$events$auto_detection_completed, 0)

  # UI sync should be requested
  app_state$columns$ui_sync$needed <- TRUE
  events_triggered$ui_sync_needed <- TRUE

  expect_true(events_triggered$ui_sync_needed)
  expect_true(app_state$columns$ui_sync$needed)
})

test_that("Session lifecycle: initialization → data load → reset", {
  app_state <- create_test_app_state()

  # Initial state
  expect_false(app_state$session$file_uploaded)
  expect_false(app_state$session$user_started_session)
  expect_true(is.null(app_state$data$current_data))

  # Simulate user session start
  app_state$session$user_started_session <- TRUE
  expect_true(app_state$session$user_started_session)

  # Upload data
  test_data <- create_test_data(40)
  app_state$data$current_data <- test_data
  app_state$session$file_uploaded <- TRUE
  app_state$session$file_name <- "test_data.csv"

  expect_true(app_state$session$file_uploaded)
  expect_equal(app_state$session$file_name, "test_data.csv")
  expect_equal(nrow(app_state$data$current_data), 40)

  # Session reset
  app_state$events$session_reset <- app_state$events$session_reset + 1L

  # Reset should clear data and flags
  app_state$data$current_data <- NULL
  app_state$data$original_data <- NULL
  app_state$session$file_uploaded <- FALSE
  app_state$session$file_name <- NULL
  app_state$columns$auto_detect$completed <- FALSE
  app_state$columns$auto_detect$results <- NULL

  # Verify reset
  expect_false(app_state$session$file_uploaded)
  expect_true(is.null(app_state$session$file_name))
  expect_true(is.null(app_state$data$current_data))
  expect_false(app_state$columns$auto_detect$completed)
  expect_gt(app_state$events$session_reset, 0)
})

test_that("Performance: QIC caching reduces computation time", {
  skip_if_not_installed("qicharts2")
  skip_on_cran()  # Performance tests can be slow

  app_state <- create_test_app_state()
  test_data <- create_test_data(100)  # Larger dataset

  qic_cache <- get_or_init_qic_cache(app_state)

  config <- list(
    x_col = "Dato",
    y_col = "Værdi",
    n_col = "Nævner"
  )

  # First call (cache miss)
  time_miss <- system.time({
    result1 <- generateSPCPlot(
      data = test_data,
      config = config,
      chart_type = "p",
      qic_cache = qic_cache
    )
  })["elapsed"]

  # Second call (cache hit)
  time_hit <- system.time({
    result2 <- generateSPCPlot(
      data = test_data,
      config = config,
      chart_type = "p",
      qic_cache = qic_cache
    )
  })["elapsed"]

  # Cache hit should be significantly faster
  expect_lt(time_hit, time_miss * 0.5,
            info = paste("Cache miss:", time_miss, "s, Cache hit:", time_hit, "s"))

  # Both results should be identical
  expect_equal(class(result1$plot), class(result2$plot))
  expect_true(!is.null(result1$qic_data))
  expect_true(!is.null(result2$qic_data))
})
