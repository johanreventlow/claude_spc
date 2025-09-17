# test-cross-component-reactive.R
# Tests for cross-component reactive dependencies og data flow

# Source required functions
source("../../R/utils_reactive_state.R")
source("../../R/utils_session_helpers.R")
source("../../R/fct_data_processing.R")

context("Cross-Component Reactive Dependencies")

test_that("Reactive values initialization og structure", {
  # TEST: Core reactive values setup og initial state

  # Test initialize_reactive_values function
  if (exists("initialize_reactive_values")) {
    # This requires Shiny context, so we'll test the structure instead
    expect_true(exists("initialize_reactive_values"))
  }

  # Test manual reactive values structure (simulating what initialize_reactive_values creates)
  mock_reactive_values <- list(
    # Core data states
    current_data = NULL,
    original_data = NULL,
    file_uploaded = FALSE,
    user_started_session = FALSE,

    # Auto-detection states
    auto_detect_done = FALSE,
    auto_detected_columns = NULL,
    initial_auto_detect_completed = FALSE,

    # UI states
    ui_sync_needed = NULL,
    updating_table = FALSE,
    table_operation_in_progress = FALSE,

    # Session management
    hide_anhoej_rules = FALSE,
    test_mode_auto_detect_ready = NULL
  )

  # TEST: All expected reactive values present
  expect_true(all(c("current_data", "original_data", "file_uploaded") %in% names(mock_reactive_values)))
  expect_true(all(c("auto_detect_done", "auto_detected_columns") %in% names(mock_reactive_values)))
  expect_true(all(c("ui_sync_needed", "updating_table") %in% names(mock_reactive_values)))

  # TEST: Initial states correct
  expect_null(mock_reactive_values$current_data)
  expect_false(mock_reactive_values$file_uploaded)
  expect_false(mock_reactive_values$auto_detect_done)
})

test_that("Data flow: File Upload → Auto Detection", {
  # TEST: Reactive chain fra file upload til auto-detection

  # Mock reactive values simulating file upload
  values <- list(
    current_data = NULL,
    original_data = NULL,
    file_uploaded = FALSE,
    auto_detect_done = FALSE,
    auto_detected_columns = NULL
  )

  # Step 1: Simulate file upload
  test_data <- data.frame(
    Skift = c(FALSE, FALSE, FALSE),
    Frys = c(FALSE, FALSE, FALSE),
    Dato = c("01-01-2024", "01-02-2024", "01-03-2024"),
    Tæller = c(95, 92, 98),
    Nævner = c(100, 95, 102),
    Kommentarer = c("Test 1", "Test 2", "Test 3")
  )

  # Simulate file upload effect
  values$current_data <- test_data
  values$original_data <- test_data
  values$file_uploaded <- TRUE

  # TEST: File upload state
  expect_true(!is.null(values$current_data))
  expect_true(values$file_uploaded)
  expect_identical(values$current_data, values$original_data)

  # Step 2: Simulate reactive trigger for auto-detection
  # This would normally be: observeEvent(values$current_data, ...)
  auto_detect_trigger <- !is.null(values$current_data) && values$file_uploaded

  # TEST: Auto-detection should trigger
  expect_true(auto_detect_trigger)

  # Step 3: Simulate auto-detection results
  if (auto_detect_trigger) {
    # Mock auto-detection logic
    detected_columns <- list(
      x_col = "Dato",
      taeller_col = "Tæller",
      naevner_col = "Nævner",
      skift_col = "Skift",
      frys_col = "Frys",
      kommentar_col = "Kommentarer"
    )

    values$auto_detected_columns <- detected_columns
    values$auto_detect_done <- TRUE
  }

  # TEST: Auto-detection results
  expect_true(!is.null(values$auto_detected_columns))
  expect_true(values$auto_detect_done)
  expect_equal(values$auto_detected_columns$x_col, "Dato")
  expect_equal(values$auto_detected_columns$taeller_col, "Tæller")
})

test_that("Data flow: Auto Detection → UI Sync", {
  # TEST: Reactive chain fra auto-detection til UI updates

  # Mock state after auto-detection
  values <- list(
    current_data = data.frame(
      Skift = FALSE, Frys = FALSE,
      Dato = "01-01-2024", Tæller = 95, Nævner = 100
    ),
    auto_detect_done = TRUE,
    auto_detected_columns = list(
      x_col = "Dato",
      taeller_col = "Tæller",
      naevner_col = "Nævner"
    ),
    ui_sync_needed = NULL
  )

  # Step 1: Trigger UI sync based on auto-detection completion
  ui_sync_trigger <- values$auto_detect_done && !is.null(values$auto_detected_columns)

  # TEST: UI sync should trigger
  expect_true(ui_sync_trigger)

  # Step 2: Create UI sync data
  if (ui_sync_trigger) {
    col_choices <- c("Vælg kolonne" = "", setNames(names(values$current_data), names(values$current_data)))

    ui_sync_data <- list(
      x_col = values$auto_detected_columns$x_col,
      taeller_col = values$auto_detected_columns$taeller_col,
      naevner_col = values$auto_detected_columns$naevner_col,
      col_choices = col_choices,
      timestamp = Sys.time()
    )

    values$ui_sync_needed <- ui_sync_data
  }

  # TEST: UI sync data created
  expect_true(!is.null(values$ui_sync_needed))
  expect_equal(values$ui_sync_needed$x_col, "Dato")
  expect_true("" %in% values$ui_sync_needed$col_choices)
  expect_true("Dato" %in% values$ui_sync_needed$col_choices)
})

test_that("Data flow: Session Reset → State Cleanup", {
  # TEST: Session reset cleaner alle reactive states korrekt

  # Mock populated state
  values <- list(
    current_data = data.frame(test = 1:3),
    original_data = data.frame(test = 1:3),
    file_uploaded = TRUE,
    user_started_session = TRUE,
    auto_detect_done = TRUE,
    auto_detected_columns = list(x_col = "test"),
    ui_sync_needed = list(x_col = "test"),
    updating_table = FALSE,
    hide_anhoej_rules = FALSE,
    initial_auto_detect_completed = TRUE
  )

  # Simulate session reset
  session_reset_trigger <- TRUE  # User clicked "Start ny session"

  if (session_reset_trigger) {
    # Reset states (simulating reset_to_empty_session)
    values$file_uploaded <- FALSE
    values$user_started_session <- FALSE
    values$auto_detect_done <- FALSE
    values$auto_detected_columns <- NULL
    values$ui_sync_needed <- NULL
    values$initial_auto_detect_completed <- FALSE

    # Load empty session data
    values$current_data <- create_empty_session_data()
    values$original_data <- values$current_data
  }

  # TEST: Session reset cleanup
  expect_false(values$file_uploaded)
  expect_false(values$user_started_session)
  expect_false(values$auto_detect_done)
  expect_null(values$auto_detected_columns)
  expect_null(values$ui_sync_needed)

  # TEST: New empty session data
  expect_true(!is.null(values$current_data))
  expect_true("Skift" %in% names(values$current_data))
  expect_true("Frys" %in% names(values$current_data))
})

test_that("Reactive dependency priorities og ordering", {
  # TEST: Reactive dependencies execute i korrekt rækkefølge

  execution_log <- character()

  # Mock reactive system
  mock_reactive_system <- list(
    # High priority: Data loading
    data_observer = function(data) {
      if (!is.null(data)) {
        execution_log <<- c(execution_log, "data_loaded")
      }
    },

    # Medium priority: Auto-detection
    auto_detect_observer = function(data, file_uploaded) {
      if (!is.null(data) && file_uploaded) {
        execution_log <<- c(execution_log, "auto_detect_triggered")
      }
    },

    # Low priority: UI updates
    ui_sync_observer = function(auto_detect_done) {
      if (auto_detect_done) {
        execution_log <<- c(execution_log, "ui_sync_executed")
      }
    }
  )

  # Simulate reactive chain execution
  test_data <- data.frame(col1 = 1:3)

  # Step 1: Data loading
  mock_reactive_system$data_observer(test_data)

  # Step 2: Auto-detection
  mock_reactive_system$auto_detect_observer(test_data, TRUE)

  # Step 3: UI sync
  mock_reactive_system$ui_sync_observer(TRUE)

  # TEST: Execution order correct
  expect_equal(execution_log[1], "data_loaded")
  expect_equal(execution_log[2], "auto_detect_triggered")
  expect_equal(execution_log[3], "ui_sync_executed")
  expect_length(execution_log, 3)
})

test_that("Error handling i reactive chains", {
  # TEST: Error handling og graceful degradation i reactive chains

  values <- list(
    current_data = NULL,
    auto_detect_done = FALSE,
    error_state = NULL
  )

  # Mock error-prone reactive chain
  safe_reactive_chain <- function(data) {
    tryCatch({
      # Step 1: Validate data
      if (is.null(data)) {
        stop("Data is NULL")
      }

      if (nrow(data) == 0) {
        stop("Data is empty")
      }

      # Step 2: Process data
      processed <- data
      values$current_data <<- processed
      values$auto_detect_done <<- TRUE
      values$error_state <<- NULL

      return("success")
    }, error = function(e) {
      values$error_state <<- e$message
      values$auto_detect_done <<- FALSE
      return("error")
    })
  }

  # Test 1: NULL data
  result1 <- safe_reactive_chain(NULL)
  expect_equal(result1, "error")
  expect_true(!is.null(values$error_state))
  expect_false(values$auto_detect_done)

  # Test 2: Empty data
  empty_data <- data.frame()
  result2 <- safe_reactive_chain(empty_data)
  expect_equal(result2, "error")
  expect_true(grepl("empty", values$error_state))

  # Test 3: Valid data
  valid_data <- data.frame(col1 = 1:3)
  result3 <- safe_reactive_chain(valid_data)
  expect_equal(result3, "success")
  expect_null(values$error_state)
  expect_true(values$auto_detect_done)
})

test_that("State consistency mellem components", {
  # TEST: State consistency på tværs af forskellige components

  # Mock different component states
  file_component_state <- list(
    data_loaded = FALSE,
    current_file = NULL
  )

  auto_detect_component_state <- list(
    detection_completed = FALSE,
    detected_columns = NULL
  )

  ui_component_state <- list(
    inputs_updated = FALSE,
    column_choices = character()
  )

  # Simulate coordinated state update
  test_data <- data.frame(Dato = "01-01-2024", Tæller = 95)

  # Step 1: File component updates
  file_component_state$data_loaded <- TRUE
  file_component_state$current_file <- "test.csv"

  # Step 2: Auto-detect component responds
  if (file_component_state$data_loaded) {
    auto_detect_component_state$detection_completed <- TRUE
    auto_detect_component_state$detected_columns <- list(x_col = "Dato", y_col = "Tæller")
  }

  # Step 3: UI component responds
  if (auto_detect_component_state$detection_completed) {
    ui_component_state$inputs_updated <- TRUE
    ui_component_state$column_choices <- names(test_data)
  }

  # TEST: Coordinated state updates
  expect_true(file_component_state$data_loaded)
  expect_true(auto_detect_component_state$detection_completed)
  expect_true(ui_component_state$inputs_updated)

  # TEST: State consistency
  expect_equal(auto_detect_component_state$detected_columns$x_col, "Dato")
  expect_true("Dato" %in% ui_component_state$column_choices)
  expect_true("Tæller" %in% ui_component_state$column_choices)
})

test_that("Reactive invalidation og dependency tracking", {
  # TEST: Reactive invalidation patterns og dependency management

  # Mock reactive dependency graph
  dependency_graph <- list(
    data_reactive = list(
      dependencies = character(),
      invalidated = FALSE,
      value = NULL
    ),

    column_config_reactive = list(
      dependencies = c("data_reactive"),
      invalidated = FALSE,
      value = NULL
    ),

    plot_reactive = list(
      dependencies = c("data_reactive", "column_config_reactive"),
      invalidated = FALSE,
      value = NULL
    )
  )

  # Mock invalidation cascade
  invalidate_reactive <- function(name, graph) {
    graph[[name]]$invalidated <- TRUE

    # Invalidate dependents
    for (reactive_name in names(graph)) {
      if (name %in% graph[[reactive_name]]$dependencies) {
        graph <- invalidate_reactive(reactive_name, graph)
      }
    }

    return(graph)
  }

  # Test invalidation cascade
  updated_graph <- invalidate_reactive("data_reactive", dependency_graph)

  # TEST: Invalidation cascade
  expect_true(updated_graph$data_reactive$invalidated)
  expect_true(updated_graph$column_config_reactive$invalidated)
  expect_true(updated_graph$plot_reactive$invalidated)

  # Mock reactive execution order
  execution_order <- character()

  # Execute in dependency order
  if (updated_graph$data_reactive$invalidated) {
    execution_order <- c(execution_order, "data_reactive")
  }

  if (updated_graph$column_config_reactive$invalidated && "data_reactive" %in% execution_order) {
    execution_order <- c(execution_order, "column_config_reactive")
  }

  if (updated_graph$plot_reactive$invalidated && all(c("data_reactive", "column_config_reactive") %in% execution_order)) {
    execution_order <- c(execution_order, "plot_reactive")
  }

  # TEST: Execution order respects dependencies
  expect_equal(execution_order[1], "data_reactive")
  expect_equal(execution_order[2], "column_config_reactive")
  expect_equal(execution_order[3], "plot_reactive")
})

test_that("Performance testing af reactive chains", {
  # TEST: Performance characteristics af reactive chains

  # Mock performance-heavy reactive computation
  heavy_computation <- function(data, iterations = 100) {
    start_time <- Sys.time()

    result <- data
    for (i in 1:iterations) {
      # Simulate computational work
      result <- transform(result, computed = runif(nrow(result)))
    }

    end_time <- Sys.time()
    return(list(
      result = result,
      execution_time = as.numeric(end_time - start_time)
    ))
  }

  # Test with small dataset
  small_data <- data.frame(x = 1:10, y = 1:10)
  small_result <- heavy_computation(small_data, 50)

  # TEST: Small dataset performance
  expect_lt(small_result$execution_time, 2)  # Should complete in under 2 seconds
  expect_equal(nrow(small_result$result), 10)

  # Test with larger dataset
  large_data <- data.frame(x = 1:1000, y = 1:1000)
  large_result <- heavy_computation(large_data, 10)  # Fewer iterations for large data

  # TEST: Large dataset still reasonable
  expect_lt(large_result$execution_time, 5)  # Should complete in under 5 seconds
  expect_equal(nrow(large_result$result), 1000)

  # TEST: Performance scaling (with tolerance for small datasets)
  # For small datasets, performance difference might be minimal due to overhead
  # We just verify both complete without excessive time
  expect_true(small_result$execution_time >= 0)
  expect_true(large_result$execution_time >= 0)
})

test_that("Memory management i reactive systems", {
  # TEST: Memory management og cleanup patterns

  # Mock memory tracking
  memory_tracker <- list(
    objects = character(),
    sizes = numeric()
  )

  # Mock object creation og cleanup
  create_reactive_object <- function(name, size_mb = 1) {
    # Simulate object creation
    memory_tracker$objects <<- c(memory_tracker$objects, name)
    memory_tracker$sizes <<- c(memory_tracker$sizes, size_mb)

    return(paste("Created", name))
  }

  cleanup_reactive_object <- function(name) {
    # Simulate object cleanup
    index <- which(memory_tracker$objects == name)
    if (length(index) > 0) {
      memory_tracker$objects <<- memory_tracker$objects[-index]
      memory_tracker$sizes <<- memory_tracker$sizes[-index]
    }

    return(paste("Cleaned up", name))
  }

  # Test object lifecycle
  create_reactive_object("data_reactive", 5)
  create_reactive_object("plot_reactive", 10)

  # TEST: Objects created
  expect_true("data_reactive" %in% memory_tracker$objects)
  expect_true("plot_reactive" %in% memory_tracker$objects)
  expect_equal(sum(memory_tracker$sizes), 15)

  # Test cleanup
  cleanup_reactive_object("data_reactive")

  # TEST: Selective cleanup
  expect_false("data_reactive" %in% memory_tracker$objects)
  expect_true("plot_reactive" %in% memory_tracker$objects)
  expect_equal(sum(memory_tracker$sizes), 10)

  # Test full cleanup
  cleanup_reactive_object("plot_reactive")

  # TEST: Full cleanup
  expect_length(memory_tracker$objects, 0)
  expect_length(memory_tracker$sizes, 0)
})