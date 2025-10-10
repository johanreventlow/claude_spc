# test-utils-state-accessors.R
# Unit tests for state accessor functions (H17)
# Tests encapsulated get_/set_ functions from utils_state_accessors.R
# Ensures consistent isolate() usage and type safety

# ============================================================================
# DATA ACCESSORS
# ============================================================================

test_that("get_current_data works correctly", {
  # SETUP
  app_state <- create_app_state()
  test_data <- data.frame(x = 1:3, y = 4:6)

  # TEST: Returns NULL when no data
  result <- get_current_data(app_state)
  expect_null(result)

  # TEST: Returns data when set
  app_state$data$current_data <- test_data
  result <- get_current_data(app_state)
  expect_equal(result, test_data)
  expect_equal(nrow(result), 3)
})

test_that("set_current_data works correctly", {
  # SETUP
  app_state <- create_app_state()
  test_data <- data.frame(a = 1:5, b = letters[1:5])

  # TEST: Sets data correctly
  set_current_data(app_state, test_data)
  result <- shiny::isolate(app_state$data$current_data)
  expect_equal(result, test_data)
  expect_equal(nrow(result), 5)

  # TEST: Can update to NULL
  set_current_data(app_state, NULL)
  result <- shiny::isolate(app_state$data$current_data)
  expect_null(result)
})

test_that("get_original_data and set_original_data work correctly", {
  # SETUP
  app_state <- create_app_state()
  original <- data.frame(original_col = c("a", "b", "c"))

  # TEST: Initial NULL
  expect_null(get_original_data(app_state))

  # TEST: Set and get
  set_original_data(app_state, original)
  result <- get_original_data(app_state)
  expect_equal(result, original)
})

test_that("is_table_updating and set_table_updating work correctly", {
  # SETUP
  app_state <- create_app_state()

  # TEST: Default FALSE
  expect_false(is_table_updating(app_state))

  # TEST: Set TRUE
  set_table_updating(app_state, TRUE)
  expect_true(is_table_updating(app_state))

  # TEST: Set FALSE
  set_table_updating(app_state, FALSE)
  expect_false(is_table_updating(app_state))
})

# ============================================================================
# COLUMN MANAGEMENT ACCESSORS
# ============================================================================

test_that("get_autodetect_status returns correct structure", {
  # SETUP
  app_state <- create_app_state()

  # TEST: Initial status
  status <- get_autodetect_status(app_state)
  expect_type(status, "list")
  expect_named(status, c("in_progress", "completed", "results", "frozen"))
  expect_false(status$in_progress)
  expect_false(status$completed)
  expect_null(status$results)
  expect_false(status$frozen)

  # TEST: After setting values
  shiny::isolate({
    app_state$columns$auto_detect$in_progress <- TRUE
    app_state$columns$auto_detect$completed <- TRUE
    app_state$columns$auto_detect$results <- list(x = "Dato", y = "Værdi")
    app_state$columns$auto_detect$frozen_until_next_trigger <- TRUE
  })

  status <- get_autodetect_status(app_state)
  expect_true(status$in_progress)
  expect_true(status$completed)
  expect_equal(status$results, list(x = "Dato", y = "Værdi"))
  expect_true(status$frozen)
})

test_that("set_autodetect_in_progress works correctly", {
  # SETUP
  app_state <- create_app_state()

  # TEST: Set TRUE
  set_autodetect_in_progress(app_state, TRUE)
  expect_true(shiny::isolate(app_state$columns$auto_detect$in_progress))

  # TEST: Set FALSE
  set_autodetect_in_progress(app_state, FALSE)
  expect_false(shiny::isolate(app_state$columns$auto_detect$in_progress))
})

test_that("set_autodetect_completed works correctly", {
  # SETUP
  app_state <- create_app_state()

  # TEST: Set TRUE
  set_autodetect_completed(app_state, TRUE)
  expect_true(shiny::isolate(app_state$columns$auto_detect$completed))

  # TEST: Set FALSE
  set_autodetect_completed(app_state, FALSE)
  expect_false(shiny::isolate(app_state$columns$auto_detect$completed))
})

test_that("set_autodetect_results works correctly and updates timestamp", {
  # SETUP
  app_state <- create_app_state()
  test_results <- list(
    x_column = "Dato",
    y_column = "Tæller",
    n_column = "Nævner"
  )

  # TEST: Set results
  before_time <- Sys.time()
  set_autodetect_results(app_state, test_results)
  after_time <- Sys.time()

  result <- shiny::isolate(app_state$columns$auto_detect$results)
  expect_equal(result, test_results)

  # TEST: Timestamp was set
  timestamp <- shiny::isolate(app_state$columns$auto_detect$last_run)
  expect_true(inherits(timestamp, c("POSIXct", "POSIXt")))
  expect_true(timestamp >= before_time && timestamp <= after_time)
})

test_that("set_autodetect_frozen works correctly", {
  # SETUP
  app_state <- create_app_state()

  # TEST: Set TRUE
  set_autodetect_frozen(app_state, TRUE)
  expect_true(shiny::isolate(app_state$columns$auto_detect$frozen_until_next_trigger))

  # TEST: Set FALSE
  set_autodetect_frozen(app_state, FALSE)
  expect_false(shiny::isolate(app_state$columns$auto_detect$frozen_until_next_trigger))
})

test_that("get_column_mappings returns all columns", {
  # SETUP
  app_state <- create_app_state()

  # Set all mappings
  shiny::isolate({
    app_state$columns$mappings$x_column <- "Dato"
    app_state$columns$mappings$y_column <- "Tæller"
    app_state$columns$mappings$n_column <- "Nævner"
    app_state$columns$mappings$cl_column <- "CL"
    app_state$columns$mappings$skift_column <- "Skift"
    app_state$columns$mappings$frys_column <- "Frys"
    app_state$columns$mappings$kommentar_column <- "Kommentar"
  })

  # TEST: Get all mappings
  mappings <- get_column_mappings(app_state)
  expect_type(mappings, "list")
  expect_named(mappings, c(
    "x_column", "y_column", "n_column", "cl_column",
    "skift_column", "frys_column", "kommentar_column"
  ))
  expect_equal(mappings$x_column, "Dato")
  expect_equal(mappings$y_column, "Tæller")
  expect_equal(mappings$n_column, "Nævner")
  expect_equal(mappings$cl_column, "CL")
  expect_equal(mappings$skift_column, "Skift")
  expect_equal(mappings$frys_column, "Frys")
  expect_equal(mappings$kommentar_column, "Kommentar")
})

test_that("get_column_mapping retrieves specific column", {
  # SETUP
  app_state <- create_app_state()
  shiny::isolate({
    app_state$columns$mappings$x_column <- "X_Test"
    app_state$columns$mappings$y_column <- "Y_Test"
  })

  # TEST: Get specific columns
  expect_equal(get_column_mapping(app_state, "x_column"), "X_Test")
  expect_equal(get_column_mapping(app_state, "y_column"), "Y_Test")
  expect_null(get_column_mapping(app_state, "n_column"))

  # TEST: Invalid column returns NULL
  expect_null(get_column_mapping(app_state, "invalid_column"))
})

test_that("update_column_mapping updates single column safely", {
  # SETUP
  app_state <- create_app_state()

  # TEST: Update valid column
  update_column_mapping(app_state, "x_column", "New_X")
  expect_equal(shiny::isolate(app_state$columns$mappings$x_column), "New_X")

  # TEST: Update another column
  update_column_mapping(app_state, "y_column", "New_Y")
  expect_equal(shiny::isolate(app_state$columns$mappings$y_column), "New_Y")

  # TEST: Invalid column name logs warning and does nothing
  # (silently ignored per implementation)
  update_column_mapping(app_state, "invalid_column", "Should_Not_Set")
  expect_false("invalid_column" %in% names(shiny::isolate(as.list(app_state$columns$mappings))))
})

test_that("update_column_mappings updates multiple columns", {
  # SETUP
  app_state <- create_app_state()

  mappings <- list(
    x_column = "Batch_X",
    y_column = "Batch_Y",
    n_column = "Batch_N"
  )

  # TEST: Batch update
  update_column_mappings(app_state, mappings)

  expect_equal(shiny::isolate(app_state$columns$mappings$x_column), "Batch_X")
  expect_equal(shiny::isolate(app_state$columns$mappings$y_column), "Batch_Y")
  expect_equal(shiny::isolate(app_state$columns$mappings$n_column), "Batch_N")

  # TEST: Invalid columns in batch are ignored
  bad_mappings <- list(
    skift_column = "Valid",
    invalid_key = "Should_Be_Ignored"
  )
  update_column_mappings(app_state, bad_mappings)
  expect_equal(shiny::isolate(app_state$columns$mappings$skift_column), "Valid")
  expect_false("invalid_key" %in% names(shiny::isolate(as.list(app_state$columns$mappings))))
})

# ============================================================================
# VISUALIZATION STATE ACCESSORS
# ============================================================================

test_that("is_plot_ready and set_plot_ready work correctly", {
  # SETUP
  app_state <- create_app_state()

  # TEST: Default FALSE
  expect_false(is_plot_ready(app_state))

  # TEST: Set TRUE
  set_plot_ready(app_state, TRUE)
  expect_true(is_plot_ready(app_state))

  # TEST: Set FALSE
  set_plot_ready(app_state, FALSE)
  expect_false(is_plot_ready(app_state))
})

test_that("get_plot_warnings and set_plot_warnings work correctly", {
  # SETUP
  app_state <- create_app_state()

  # TEST: Default empty character vector
  expect_equal(get_plot_warnings(app_state), character(0))

  # TEST: Set warnings
  warnings <- c("Warning 1", "Warning 2")
  set_plot_warnings(app_state, warnings)
  expect_equal(get_plot_warnings(app_state), warnings)

  # TEST: Clear warnings
  set_plot_warnings(app_state, character(0))
  expect_equal(get_plot_warnings(app_state), character(0))
})

test_that("get_plot_object and set_plot_object work correctly", {
  # SETUP
  app_state <- create_app_state()

  # TEST: Initially NULL
  expect_null(get_plot_object(app_state))

  # TEST: Set plot object (mock ggplot)
  mock_plot <- structure(list(data = data.frame(x = 1:3)), class = "gg")
  set_plot_object(app_state, mock_plot)

  result <- get_plot_object(app_state)
  expect_s3_class(result, "gg")
  expect_equal(result$data, data.frame(x = 1:3))
})

test_that("is_plot_generating and set_plot_generating work correctly", {
  # SETUP
  app_state <- create_app_state()

  # TEST: Default FALSE
  expect_false(is_plot_generating(app_state))

  # TEST: Circuit breaker pattern - set TRUE during generation
  set_plot_generating(app_state, TRUE)
  expect_true(is_plot_generating(app_state))

  # TEST: Release circuit breaker after generation
  set_plot_generating(app_state, FALSE)
  expect_false(is_plot_generating(app_state))
})

# ============================================================================
# SESSION STATE ACCESSORS
# ============================================================================

test_that("is_file_uploaded and set_file_uploaded work correctly", {
  # SETUP
  app_state <- create_app_state()

  # TEST: Default FALSE
  expect_false(is_file_uploaded(app_state))

  # TEST: Set TRUE after upload
  set_file_uploaded(app_state, TRUE)
  expect_true(is_file_uploaded(app_state))

  # TEST: Reset to FALSE
  set_file_uploaded(app_state, FALSE)
  expect_false(is_file_uploaded(app_state))
})

test_that("is_user_session_started and set_user_session_started work correctly", {
  # SETUP
  app_state <- create_app_state()

  # TEST: Default FALSE
  expect_false(is_user_session_started(app_state))

  # TEST: Set TRUE when user starts session
  set_user_session_started(app_state, TRUE)
  expect_true(is_user_session_started(app_state))
})

# ============================================================================
# ERROR STATE ACCESSORS
# ============================================================================

test_that("get_last_error and set_last_error work correctly", {
  # SETUP
  app_state <- create_app_state()

  # TEST: Initially NULL
  expect_null(get_last_error(app_state))

  # TEST: Set error
  error_info <- list(
    type = "validation",
    context = "file_upload",
    message = "Invalid CSV format",
    timestamp = Sys.time()
  )
  set_last_error(app_state, error_info)

  result <- get_last_error(app_state)
  expect_equal(result$type, "validation")
  expect_equal(result$context, "file_upload")
  expect_equal(result$message, "Invalid CSV format")

  # TEST: Error count incremented
  expect_equal(get_error_count(app_state), 1L)

  # TEST: Multiple errors increment count
  set_last_error(app_state, list(type = "processing", context = "qic"))
  expect_equal(get_error_count(app_state), 2L)
})

test_that("get_error_count works correctly", {
  # SETUP
  app_state <- create_app_state()

  # TEST: Initial count 0
  expect_equal(get_error_count(app_state), 0L)

  # TEST: Count increases with errors
  set_last_error(app_state, list(type = "error1"))
  expect_equal(get_error_count(app_state), 1L)

  set_last_error(app_state, list(type = "error2"))
  expect_equal(get_error_count(app_state), 2L)
})

# ============================================================================
# TEST MODE ACCESSORS
# ============================================================================

test_that("is_test_mode_enabled and set_test_mode_enabled work correctly", {
  # SETUP
  app_state <- create_app_state()

  # TEST: Default FALSE
  expect_false(is_test_mode_enabled(app_state))

  # TEST: Enable test mode
  set_test_mode_enabled(app_state, TRUE)
  expect_true(is_test_mode_enabled(app_state))

  # TEST: Disable test mode
  set_test_mode_enabled(app_state, FALSE)
  expect_false(is_test_mode_enabled(app_state))
})

test_that("get_test_mode_startup_phase and set_test_mode_startup_phase work correctly", {
  # SETUP
  app_state <- create_app_state()

  # TEST: Default "initializing"
  expect_equal(get_test_mode_startup_phase(app_state), "initializing")

  # TEST: Set valid phases
  set_test_mode_startup_phase(app_state, "data_ready")
  expect_equal(get_test_mode_startup_phase(app_state), "data_ready")

  set_test_mode_startup_phase(app_state, "ui_ready")
  expect_equal(get_test_mode_startup_phase(app_state), "ui_ready")

  set_test_mode_startup_phase(app_state, "complete")
  expect_equal(get_test_mode_startup_phase(app_state), "complete")

  # TEST: Invalid phase is rejected (logged warning, no change)
  set_test_mode_startup_phase(app_state, "invalid_phase")
  expect_equal(get_test_mode_startup_phase(app_state), "complete") # Should remain unchanged
})

# ============================================================================
# UI STATE ACCESSORS
# ============================================================================

test_that("is_anhoej_rules_hidden and set_anhoej_rules_hidden work correctly", {
  # SETUP
  app_state <- create_app_state()

  # TEST: Default FALSE
  expect_false(is_anhoej_rules_hidden(app_state))

  # TEST: Hide rules
  set_anhoej_rules_hidden(app_state, TRUE)
  expect_true(is_anhoej_rules_hidden(app_state))

  # TEST: Show rules
  set_anhoej_rules_hidden(app_state, FALSE)
  expect_false(is_anhoej_rules_hidden(app_state))
})

test_that("is_y_axis_autoset_done and set_y_axis_autoset_done work correctly", {
  # SETUP
  app_state <- create_app_state()

  # TEST: Default FALSE
  expect_false(is_y_axis_autoset_done(app_state))

  # TEST: Mark as done after auto-set
  set_y_axis_autoset_done(app_state, TRUE)
  expect_true(is_y_axis_autoset_done(app_state))

  # TEST: Reset for new data load
  set_y_axis_autoset_done(app_state, FALSE)
  expect_false(is_y_axis_autoset_done(app_state))
})

# ============================================================================
# INTEGRATION TESTS - Accessor Isolation
# ============================================================================

test_that("accessors properly isolate reactive context", {
  # SETUP
  app_state <- create_app_state()
  test_data <- data.frame(x = 1:5)

  # TEST: Accessors can be called outside reactive context
  # (isolate() ensures no dependency creation)
  expect_silent({
    set_current_data(app_state, test_data)
    result <- get_current_data(app_state)
  })

  expect_equal(result, test_data)
})

test_that("accessors maintain state encapsulation", {
  # SETUP
  app_state <- create_app_state()

  # TEST: Setting via accessor updates state
  set_table_updating(app_state, TRUE)
  expect_true(is_table_updating(app_state))

  # TEST: Direct state access matches accessor
  expect_true(shiny::isolate(app_state$data$updating_table))

  # TEST: Accessor provides consistent view
  direct_value <- shiny::isolate(app_state$data$updating_table)
  accessor_value <- is_table_updating(app_state)
  expect_identical(direct_value, accessor_value)
})

test_that("accessors handle NULL and missing values correctly", {
  # SETUP
  app_state <- create_app_state()

  # TEST: get_ functions return NULL/defaults for unset values
  expect_null(get_current_data(app_state))
  expect_null(get_original_data(app_state))
  expect_false(is_table_updating(app_state)) # %||% FALSE
  expect_equal(get_plot_warnings(app_state), character(0)) # %||% character(0)

  # TEST: set_ functions accept NULL
  set_current_data(app_state, data.frame(x = 1))
  set_current_data(app_state, NULL)
  expect_null(get_current_data(app_state))
})

test_that("column mapping accessors validate input", {
  # SETUP
  app_state <- create_app_state()

  # TEST: Valid columns are accepted
  valid_columns <- c("x_column", "y_column", "n_column", "cl_column",
                     "skift_column", "frys_column", "kommentar_column")

  for (col in valid_columns) {
    update_column_mapping(app_state, col, paste0("test_", col))
    expect_equal(get_column_mapping(app_state, col), paste0("test_", col))
  }

  # TEST: Invalid column is rejected silently
  update_column_mapping(app_state, "bogus_column", "should_not_set")
  expect_null(get_column_mapping(app_state, "bogus_column"))
})
