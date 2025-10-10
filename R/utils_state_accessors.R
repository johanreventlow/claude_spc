#' State Accessor Functions
#'
#' Getter and setter functions for encapsulated app_state access.
#'
#' ## Architecture
#'
#' Instead of direct app_state access with inconsistent isolate() usage,
#' this module provides **accessor functions** that:
#'
#' **Benefits**:
#' - Consistent isolate() usage (prevents reactive dependency bugs)
#' - Encapsulation of state structure (easier to refactor schema)
#' - Type safety through validation
#' - Self-documenting code (function names describe purpose)
#' - Single source of truth for state access patterns
#'
#' ## Usage
#'
#' ```r
#' # Before (inconsistent, error-prone):
#' data <- isolate(app_state$data$current_data)
#' app_state$columns$mappings$x_column <- "Dato"  # Missing isolate!
#'
#' # After (consistent, safe):
#' data <- get_current_data(app_state)
#' set_column_mapping(app_state, "x_column", "Dato")
#' ```
#'
#' @name utils_state_accessors
NULL

# ============================================================================
# DATA ACCESSORS
# ============================================================================

#' Get Current Data
#'
#' Safely retrieves the current data from app_state.
#'
#' @param app_state Centralized app state
#'
#' @return Data frame or NULL
#'
#' @export
get_current_data <- function(app_state) {
  shiny::isolate(app_state$data$current_data)
}

#' Set Current Data
#'
#' Safely sets the current data in app_state.
#'
#' @param app_state Centralized app state
#' @param value Data frame to set
#'
#' @export
set_current_data <- function(app_state, value) {
  shiny::isolate({
    app_state$data$current_data <- value
  })
}

#' Get Original Data
#'
#' Safely retrieves the original (backup) data from app_state.
#'
#' @param app_state Centralized app state
#'
#' @return Data frame or NULL
#'
#' @export
get_original_data <- function(app_state) {
  shiny::isolate(app_state$data$original_data)
}

#' Set Original Data
#'
#' Safely sets the original (backup) data in app_state.
#'
#' @param app_state Centralized app state
#' @param value Data frame to set
#'
#' @export
set_original_data <- function(app_state, value) {
  shiny::isolate({
    app_state$data$original_data <- value
  })
}

#' Check if Table is Updating
#'
#' Safely checks if a table operation is in progress.
#'
#' @param app_state Centralized app state
#'
#' @return Logical
#'
#' @export
is_table_updating <- function(app_state) {
  shiny::isolate(app_state$data$updating_table %||% FALSE)
}

#' Set Table Updating Flag
#'
#' Safely sets the table updating flag.
#'
#' @param app_state Centralized app state
#' @param value Logical value
#'
#' @export
set_table_updating <- function(app_state, value) {
  shiny::isolate({
    app_state$data$updating_table <- value
  })
}

# ============================================================================
# COLUMN MANAGEMENT ACCESSORS
# ============================================================================

#' Get Auto-Detection Status
#'
#' Retrieves the current auto-detection status.
#'
#' @param app_state Centralized app state
#'
#' @return Named list with in_progress, completed, results, frozen
#'
#' @export
get_autodetect_status <- function(app_state) {
  shiny::isolate({
    list(
      in_progress = app_state$columns$auto_detect$in_progress %||% FALSE,
      completed = app_state$columns$auto_detect$completed %||% FALSE,
      results = app_state$columns$auto_detect$results,
      frozen = app_state$columns$auto_detect$frozen_until_next_trigger %||% FALSE
    )
  })
}

#' Set Auto-Detection In Progress
#'
#' Safely sets the auto-detection in_progress flag.
#'
#' @param app_state Centralized app state
#' @param value Logical value
#'
#' @export
set_autodetect_in_progress <- function(app_state, value) {
  shiny::isolate({
    app_state$columns$auto_detect$in_progress <- value
  })
}

#' Set Auto-Detection Completed
#'
#' Safely sets the auto-detection completed flag.
#'
#' @param app_state Centralized app state
#' @param value Logical value
#'
#' @export
set_autodetect_completed <- function(app_state, value) {
  shiny::isolate({
    app_state$columns$auto_detect$completed <- value
  })
}

#' Set Auto-Detection Results
#'
#' Safely stores auto-detection results.
#'
#' @param app_state Centralized app state
#' @param results Auto-detection results (named list)
#'
#' @export
set_autodetect_results <- function(app_state, results) {
  shiny::isolate({
    app_state$columns$auto_detect$results <- results
    app_state$columns$auto_detect$last_run <- Sys.time()
  })
}

#' Set Auto-Detection Frozen
#'
#' Safely sets the frozen_until_next_trigger flag.
#'
#' @param app_state Centralized app state
#' @param value Logical value
#'
#' @export
set_autodetect_frozen <- function(app_state, value) {
  shiny::isolate({
    app_state$columns$auto_detect$frozen_until_next_trigger <- value
  })
}

#' Get Column Mappings
#'
#' Retrieves all current column mappings.
#'
#' @param app_state Centralized app state
#'
#' @return Named list with x_column, y_column, n_column, etc.
#'
#' @export
get_column_mappings <- function(app_state) {
  shiny::isolate({
    list(
      x_column = app_state$columns$mappings$x_column,
      y_column = app_state$columns$mappings$y_column,
      n_column = app_state$columns$mappings$n_column,
      cl_column = app_state$columns$mappings$cl_column,
      skift_column = app_state$columns$mappings$skift_column,
      frys_column = app_state$columns$mappings$frys_column,
      kommentar_column = app_state$columns$mappings$kommentar_column
    )
  })
}

#' Get Specific Column Mapping
#'
#' Retrieves a specific column mapping value.
#'
#' @param app_state Centralized app state
#' @param column Column name (x_column, y_column, n_column, etc.)
#'
#' @return Column value or NULL
#'
#' @export
get_column_mapping <- function(app_state, column) {
  shiny::isolate({
    switch(column,
      x_column = app_state$columns$mappings$x_column,
      y_column = app_state$columns$mappings$y_column,
      n_column = app_state$columns$mappings$n_column,
      cl_column = app_state$columns$mappings$cl_column,
      skift_column = app_state$columns$mappings$skift_column,
      frys_column = app_state$columns$mappings$frys_column,
      kommentar_column = app_state$columns$mappings$kommentar_column,
      NULL
    )
  })
}

#' Update Column Mapping
#'
#' Safely updates a single column mapping.
#'
#' @param app_state Centralized app state
#' @param column Column name (x_column, y_column, n_column, etc.)
#' @param value New value for the column
#'
#' @details
#' Validates that the column name is one of the recognized mapping types
#' before updating. Invalid column names are silently ignored with a warning.
#'
#' @export
update_column_mapping <- function(app_state, column, value) {
  valid_columns <- c(
    "x_column", "y_column", "n_column", "cl_column",
    "skift_column", "frys_column", "kommentar_column"
  )

  if (!column %in% valid_columns) {
    if (exists("log_warn", mode = "function")) {
      log_warn(
        paste("Invalid column mapping:", column),
        .context = "STATE_ACCESSOR"
      )
    }
    return(invisible(NULL))
  }

  shiny::isolate({
    app_state$columns$mappings[[column]] <- value
  })
}

#' Update All Column Mappings
#'
#' Safely updates multiple column mappings at once.
#'
#' @param app_state Centralized app state
#' @param mappings Named list of column mappings
#'
#' @export
update_column_mappings <- function(app_state, mappings) {
  valid_columns <- c(
    "x_column", "y_column", "n_column", "cl_column",
    "skift_column", "frys_column", "kommentar_column"
  )

  shiny::isolate({
    for (col in names(mappings)) {
      if (col %in% valid_columns) {
        app_state$columns$mappings[[col]] <- mappings[[col]]
      }
    }
  })
}

# ============================================================================
# VISUALIZATION STATE ACCESSORS
# ============================================================================

#' Check if Plot is Ready
#'
#' Safely checks if the plot is ready to display.
#'
#' @param app_state Centralized app state
#'
#' @return Logical
#'
#' @export
is_plot_ready <- function(app_state) {
  shiny::isolate(app_state$visualization$plot_ready %||% FALSE)
}

#' Set Plot Ready Flag
#'
#' Safely sets the plot ready flag.
#'
#' @param app_state Centralized app state
#' @param value Logical value
#'
#' @export
set_plot_ready <- function(app_state, value) {
  shiny::isolate({
    app_state$visualization$plot_ready <- value
  })
}

#' Get Plot Warnings
#'
#' Retrieves current plot warnings.
#'
#' @param app_state Centralized app state
#'
#' @return Character vector of warnings
#'
#' @export
get_plot_warnings <- function(app_state) {
  shiny::isolate(app_state$visualization$plot_warnings %||% character(0))
}

#' Set Plot Warnings
#'
#' Safely sets plot warnings.
#'
#' @param app_state Centralized app state
#' @param warnings Character vector of warnings
#'
#' @export
set_plot_warnings <- function(app_state, warnings) {
  shiny::isolate({
    app_state$visualization$plot_warnings <- warnings
  })
}

#' Get Plot Object
#'
#' Retrieves the current plot object.
#'
#' @param app_state Centralized app state
#'
#' @return ggplot object or NULL
#'
#' @export
get_plot_object <- function(app_state) {
  shiny::isolate(app_state$visualization$plot_object)
}

#' Set Plot Object
#'
#' Safely stores the plot object.
#'
#' @param app_state Centralized app state
#' @param plot ggplot object
#'
#' @export
set_plot_object <- function(app_state, plot) {
  shiny::isolate({
    app_state$visualization$plot_object <- plot
  })
}

#' Check if Plot Generation in Progress
#'
#' Checks the circuit breaker flag for overlapping plot generations.
#'
#' @param app_state Centralized app state
#'
#' @return Logical
#'
#' @export
is_plot_generating <- function(app_state) {
  shiny::isolate(
    app_state$visualization$plot_generation_in_progress %||% FALSE
  )
}

#' Set Plot Generation Flag
#'
#' Sets the circuit breaker flag for plot generation.
#'
#' @param app_state Centralized app state
#' @param value Logical value
#'
#' @export
set_plot_generating <- function(app_state, value) {
  shiny::isolate({
    app_state$visualization$plot_generation_in_progress <- value
  })
}

# ============================================================================
# SESSION STATE ACCESSORS
# ============================================================================

#' Check if File was Uploaded
#'
#' Safely checks if a file has been uploaded this session.
#'
#' @param app_state Centralized app state
#'
#' @return Logical
#'
#' @export
is_file_uploaded <- function(app_state) {
  shiny::isolate(app_state$session$file_uploaded %||% FALSE)
}

#' Set File Uploaded Flag
#'
#' Safely sets the file uploaded flag.
#'
#' @param app_state Centralized app state
#' @param value Logical value
#'
#' @export
set_file_uploaded <- function(app_state, value) {
  shiny::isolate({
    app_state$session$file_uploaded <- value
  })
}

#' Check if User Started Session
#'
#' Safely checks if the user has actively started the session.
#'
#' @param app_state Centralized app state
#'
#' @return Logical
#'
#' @export
is_user_session_started <- function(app_state) {
  shiny::isolate(app_state$session$user_started_session %||% FALSE)
}

#' Set User Session Started Flag
#'
#' Safely sets the user session started flag.
#'
#' @param app_state Centralized app state
#' @param value Logical value
#'
#' @export
set_user_session_started <- function(app_state, value) {
  shiny::isolate({
    app_state$session$user_started_session <- value
  })
}

# ============================================================================
# ERROR STATE ACCESSORS
# ============================================================================

#' Get Last Error
#'
#' Retrieves the last error details.
#'
#' @param app_state Centralized app state
#'
#' @return List with error details or NULL
#'
#' @export
get_last_error <- function(app_state) {
  shiny::isolate(app_state$errors$last_error)
}

#' Set Last Error
#'
#' Safely records error details.
#'
#' @param app_state Centralized app state
#' @param error_info List with type, context, message, timestamp
#'
#' @export
set_last_error <- function(app_state, error_info) {
  shiny::isolate({
    app_state$errors$last_error <- error_info
    app_state$errors$error_count <- app_state$errors$error_count + 1L
  })
}

#' Get Error Count
#'
#' Retrieves the total error count for this session.
#'
#' @param app_state Centralized app state
#'
#' @return Integer count
#'
#' @export
get_error_count <- function(app_state) {
  shiny::isolate(app_state$errors$error_count %||% 0L)
}

# ============================================================================
# TEST MODE ACCESSORS
# ============================================================================

#' Check if Test Mode Enabled
#'
#' Safely checks if test mode is enabled.
#'
#' @param app_state Centralized app state
#'
#' @return Logical
#'
#' @export
is_test_mode_enabled <- function(app_state) {
  shiny::isolate(app_state$test_mode$enabled %||% FALSE)
}

#' Set Test Mode Enabled
#'
#' Safely sets the test mode enabled flag.
#'
#' @param app_state Centralized app state
#' @param value Logical value
#'
#' @export
set_test_mode_enabled <- function(app_state, value) {
  shiny::isolate({
    app_state$test_mode$enabled <- value
  })
}

#' Get Test Mode Startup Phase
#'
#' Retrieves the current test mode startup phase.
#'
#' @param app_state Centralized app state
#'
#' @return Character: "initializing", "data_ready", "ui_ready", "complete"
#'
#' @export
get_test_mode_startup_phase <- function(app_state) {
  shiny::isolate(app_state$test_mode$startup_phase %||% "initializing")
}

#' Set Test Mode Startup Phase
#'
#' Safely sets the test mode startup phase.
#'
#' @param app_state Centralized app state
#' @param phase Character phase name
#'
#' @export
set_test_mode_startup_phase <- function(app_state, phase) {
  valid_phases <- c("initializing", "data_ready", "ui_ready", "complete")

  if (!phase %in% valid_phases) {
    if (exists("log_warn", mode = "function")) {
      log_warn(
        paste("Invalid test mode phase:", phase),
        .context = "STATE_ACCESSOR"
      )
    }
    return(invisible(NULL))
  }

  shiny::isolate({
    app_state$test_mode$startup_phase <- phase
  })
}

# ============================================================================
# UI STATE ACCESSORS
# ============================================================================

#' Check if Anhoej Rules Hidden
#'
#' Safely checks if Anhoej rules should be hidden in UI.
#'
#' @param app_state Centralized app state
#'
#' @return Logical
#'
#' @export
is_anhoej_rules_hidden <- function(app_state) {
  shiny::isolate(app_state$ui$hide_anhoej_rules %||% FALSE)
}

#' Set Anhoej Rules Hidden Flag
#'
#' Safely sets the hide Anhoej rules flag.
#'
#' @param app_state Centralized app state
#' @param value Logical value
#'
#' @export
set_anhoej_rules_hidden <- function(app_state, value) {
  shiny::isolate({
    app_state$ui$hide_anhoej_rules <- value
  })
}

#' Check if Y-Axis Unit Auto-Set Done
#'
#' Checks if the Y-axis unit has been auto-set for this data load.
#'
#' @param app_state Centralized app state
#'
#' @return Logical
#'
#' @export
is_y_axis_autoset_done <- function(app_state) {
  shiny::isolate(app_state$ui$y_axis_unit_autoset_done %||% FALSE)
}

#' Set Y-Axis Auto-Set Done Flag
#'
#' Safely sets the Y-axis auto-set done flag.
#'
#' @param app_state Centralized app state
#' @param value Logical value
#'
#' @export
set_y_axis_autoset_done <- function(app_state, value) {
  shiny::isolate({
    app_state$ui$y_axis_unit_autoset_done <- value
  })
}
