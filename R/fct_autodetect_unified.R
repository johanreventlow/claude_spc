# fct_autodetect_unified.R
# Unified Autodetect Engine - Single source of truth for all autodetection
# Replaces multiple overlapping functions with event-driven, robust system

#' Unified Autodetect Engine
#'
#' Central function for all autodetection scenarios in SPC app.
#' Handles session start, file upload, and manual triggers with frozen state management.
#'
#' @param data Data frame to analyze, or NULL for session start scenario
#' @param trigger_type One of: "session_start", "file_upload", "manual"
#' @param app_state Centralized app state environment
#' @param emit Event emission functions list
#' @return Invisibly returns detection results, primarily works via state updates
#' @family autodetect_functions
#' @export
autodetect_engine <- function(data = NULL,
                              trigger_type = c("session_start", "file_upload", "manual"),
                              app_state,
                              emit) {
  # Input validation
  trigger_type <- match.arg(trigger_type)
  if (is.null(app_state)) {
    stop("app_state is required for unified autodetect engine")
  }
  if (is.null(emit)) {
    stop("emit functions are required for unified autodetect engine")
  }

  # Use centralized reactive state system - no initialization needed
  # State is handled by app_state$columns$auto_detect reactiveValues

  # Session ID for logging
  session_id <- if (exists("session", envir = parent.frame())) {
    get("session", envir = parent.frame())$token
  } else {
    "unknown"
  }

  log_debug_block("UNIFIED_AUTODETECT", "Starting unified autodetect engine")
  log_debug_kv(
    trigger_type = trigger_type,
    data_available = !is.null(data),
    frozen_state = shiny::isolate(app_state$columns$auto_detect$frozen_until_next_trigger) %||% FALSE,
    .context = "UNIFIED_AUTODETECT"
  )

  # 1. TRIGGER VALIDATION - smart unfreezing when data is available
  frozen_state <- shiny::isolate(app_state$columns$auto_detect$frozen_until_next_trigger) %||% FALSE

  # SMART UNFREEZE: If we have data available and we're frozen, automatically unfreeze
  if (frozen_state && !is.null(data) && nrow(data) > 0 && trigger_type == "file_upload") {
    shiny::isolate(app_state$columns$auto_detect$frozen_until_next_trigger <- FALSE)
    frozen_state <- FALSE
  }

  if (frozen_state && trigger_type != "manual") {
    return(invisible(NULL))
  }

  # 2. SCENARIO ROUTING - based on trigger type and data availability
  if (is.null(data) || nrow(data) == 0) {
    # Session start / name-only scenario
    col_names <- if (is.null(data)) character(0) else names(data)
    results <- detect_columns_name_based(col_names, app_state)
  } else {
    # Full data analysis scenario
    log_debug_kv(
      column_names = paste(names(data), collapse = ", "),
      .context = "UNIFIED_AUTODETECT"
    )
    results <- detect_columns_full_analysis(data, app_state)
  }

  # 3. STATE UPDATE & FREEZE
  # Update all column mappings in unified location - pass app_state for direct updates
  app_state$columns <- update_all_column_mappings(results, app_state$columns, app_state)

  # Set frozen state to prevent re-running until next legitimate trigger
  shiny::isolate({
    app_state$columns$auto_detect$frozen_until_next_trigger <- TRUE
    app_state$columns$auto_detect$last_run <- list(
      trigger = trigger_type,
      timestamp = Sys.time(),
      data_rows = if (!is.null(data)) nrow(data) else 0,
      data_cols = if (!is.null(data)) ncol(data) else 0,
      results_summary = list(
        x_column = results$x_col,
        y_column = results$y_col,
        n_column = results$n_col,
        cl_column = results$cl_col
      )
    )
  })

  log_debug_kv(
    autodetect_completed = TRUE,
    .context = "UNIFIED_AUTODETECT"
  )
  # 4. UI SYNC & LOGGING
  log_autodetect_decisions(results, trigger_type, session_id)

  # Emit completion event for UI updates
  emit$auto_detection_completed()

  return(invisible(results))
}

#' Detect Columns Based on Names Only
#'
#' Name-based column detection for scenarios without data content.
#' Renamed and improved version of detect_columns_name_only.
#'
#' @param col_names Character vector of column names
#' @param app_state Centralized app state (optional for logging)
#' @return List with detected column mappings
#' @export
detect_columns_name_based <- function(col_names, app_state = NULL) {
  log_debug_block("NAME_BASED_DETECT", "Starting name-based column detection")

  if (length(col_names) == 0) {
    return(list(
      x_col = NULL, y_col = NULL, n_col = NULL,
      skift_col = NULL, frys_col = NULL, kommentar_col = NULL
    ))
  }

  log_debug_kv(
    column_names = paste(col_names, collapse = ", "),
    .context = "NAME_BASED_DETECT"
  )

  # Initialize results
  x_col <- NULL
  y_col <- NULL  # Changed from taeller_col for consistency
  n_col <- NULL  # Changed from naevner_col for consistency
  skift_col <- NULL
  frys_col <- NULL
  kommentar_col <- NULL

  col_names_lower <- tolower(col_names)

  # X-column (date/time detection) - Enhanced time-specific patterns (tidyverse)
  dato_patterns <- c("dato", "date", "tid", "time", "år", "year", "måned", "month",
    "uge", "week", "dag", "day", "periode", "period", "kvartal", "quarter",
    "jan", "feb", "mar", "apr", "maj", "jun",
    "jul", "aug", "sep", "okt", "nov", "dec")

  # Tidyverse: Use purrr::map_lgl to find first matching pattern
  matching_patterns <- purrr::map_lgl(dato_patterns, ~ any(stringr::str_detect(col_names_lower, .x)))
  if (any(matching_patterns)) {
    first_match_pattern <- dato_patterns[which(matching_patterns)[1]]
    x_col <- col_names[stringr::str_detect(col_names_lower, first_match_pattern)][1]
  }

  # Y-column (count patterns) - tidyverse conversion
  count_patterns <- c("tæller", "tael", "num", "count", "værdi", "value", "antal")

  matching_count_patterns <- purrr::map_lgl(count_patterns, ~ any(stringr::str_detect(col_names_lower, .x)))
  if (any(matching_count_patterns)) {
    first_count_pattern <- count_patterns[which(matching_count_patterns)[1]]
    y_col <- col_names[stringr::str_detect(col_names_lower, first_count_pattern)][1]
  }

  # N-column (denominator patterns) - tidyverse conversion
  denom_patterns <- c("nævner", "naev", "denom", "total", "samlet")

  matching_denom_patterns <- purrr::map_lgl(denom_patterns, ~ any(stringr::str_detect(col_names_lower, .x)))
  if (any(matching_denom_patterns)) {
    first_denom_pattern <- denom_patterns[which(matching_denom_patterns)[1]]
    n_col <- col_names[stringr::str_detect(col_names_lower, first_denom_pattern)][1]
  }

  # Special columns - tidyverse conversion
  skift_patterns <- c("skift", "shift", "ugedag", "weekday")
  matching_skift_patterns <- purrr::map_lgl(skift_patterns, ~ any(stringr::str_detect(col_names_lower, .x)))
  if (any(matching_skift_patterns)) {
    first_skift_pattern <- skift_patterns[which(matching_skift_patterns)[1]]
    skift_col <- col_names[stringr::str_detect(col_names_lower, first_skift_pattern)][1]
  }

  frys_patterns <- c("frys", "freeze", "frossen", "frozen")
  matching_frys_patterns <- purrr::map_lgl(frys_patterns, ~ any(stringr::str_detect(col_names_lower, .x)))
  if (any(matching_frys_patterns)) {
    first_frys_pattern <- frys_patterns[which(matching_frys_patterns)[1]]
    frys_col <- col_names[stringr::str_detect(col_names_lower, first_frys_pattern)][1]
  }

  comment_patterns <- c("kommentar", "comment", "note", "bemærkning")
  matching_comment_patterns <- purrr::map_lgl(comment_patterns, ~ any(stringr::str_detect(col_names_lower, .x)))
  if (any(matching_comment_patterns)) {
    first_comment_pattern <- comment_patterns[which(matching_comment_patterns)[1]]
    kommentar_col <- col_names[stringr::str_detect(col_names_lower, first_comment_pattern)][1]
  }

  # Compile results
  results <- list(
    x_col = x_col,
    y_col = y_col,
    n_col = n_col,
    skift_col = skift_col,
    frys_col = frys_col,
    kommentar_col = kommentar_col
  )

  log_debug_kv(
    x_col = ifelse(is.null(results$x_col), "NULL", results$x_col),
    y_col = ifelse(is.null(results$y_col), "NULL", results$y_col),
    n_col = ifelse(is.null(results$n_col), "NULL", results$n_col),
    .context = "NAME_BASED_DETECT"
  )

  return(results)
}

#' Detect Columns with Full Data Analysis
#'
#' Comprehensive column detection using data content, types, and statistical analysis.
#' Incorporates robust date detection with lubridate.
#'
#' @param data Data frame to analyze
#' @param app_state Centralized app state (optional)
#' @return List with detected column mappings and confidence scores
detect_columns_full_analysis <- function(data, app_state = NULL) {
  log_debug_block("FULL_DATA_DETECT", "Starting full data analysis detection")

  if (is.null(data) || nrow(data) == 0 || ncol(data) == 0) {
    return(list(
      x_col = NULL, y_col = NULL, n_col = NULL,
      skift_col = NULL, frys_col = NULL, kommentar_col = NULL
    ))
  }

  log_debug_kv(
    column_names = paste(names(data), collapse = ", "),
    .context = "FULL_DATA_DETECT"
  )

  # Start with name-based detection as foundation
  name_based_results <- detect_columns_name_based(names(data), app_state)

  # Enhance with data-driven detection

  # 1. ROBUST DATE DETECTION using lubridate
  date_candidates <- detect_date_columns_robust(data)

  # Choose best date column based on detection confidence - tidyverse conversion
  best_date_col <- NULL
  if (length(date_candidates) > 0) {
    # Sort by score and take highest confidence using purrr::map_dbl
    scores <- purrr::map_dbl(date_candidates, ~ .x$score)
    sorted_indices <- order(scores, decreasing = TRUE)
    sorted_dates <- date_candidates[sorted_indices]
    best_date_col <- names(sorted_dates)[1]
  }

  # 2. NUMERIC COLUMN ANALYSIS
  numeric_candidates <- find_numeric_columns(data)

  # Score and rank numeric columns for Y and N roles
  y_candidates <- score_column_candidates(data, numeric_candidates, role = "y_column")
  n_candidates <- score_column_candidates(data, numeric_candidates, role = "n_column")

  # 3. COMBINE RESULTS with preference for data-driven detection
  results <- list(
    x_col = best_date_col %||% name_based_results$x_col,
    y_col = if (length(y_candidates) > 0) names(y_candidates)[1] else name_based_results$y_col,
    n_col = if (length(n_candidates) > 0) names(n_candidates)[1] else name_based_results$n_col,
    skift_col = name_based_results$skift_col,  # These remain name-based
    frys_col = name_based_results$frys_col,
    kommentar_col = name_based_results$kommentar_col
  )

  log_debug_kv(
    y_col_final = ifelse(is.null(results$y_col), "NULL", results$y_col),
    n_col_final = ifelse(is.null(results$n_col), "NULL", results$n_col),
    data_driven_improvements = !is.null(best_date_col) || length(y_candidates) > 0,
    .context = "FULL_DATA_DETECT"
  )

  return(results)
}

#' Update All Column Mappings in Unified State
#'
#' Centralized function to update all column mappings consistently.
#'
#' @param results Detection results from autodetect engine
#' @param existing_columns Existing column state (optional)
#' @return Updated column state
update_all_column_mappings <- function(results, existing_columns = NULL, app_state = NULL) {
  log_debug_block("UPDATE_MAPPINGS", "Updating column mappings in unified state")

  # SMART APP_STATE DETECTION: If app_state not provided, try to find it from parent environment
  if (is.null(app_state)) {
    # Look for app_state in the calling environment chain using purrr::detect
    parent_frames <- purrr::map(1:10, ~ parent.frame(.x))
    app_state_env <- purrr::detect(parent_frames, ~ exists("app_state", envir = .x))
    if (!is.null(app_state_env)) {
      app_state <- get("app_state", envir = app_state_env)
    }
  }

  # DIRECT APP_STATE UPDATE: If app_state is provided, update it directly
  if (!is.null(app_state)) {
    # Update individual column mappings directly in app_state (with isolate for reactive safety)
    shiny::isolate({
      if (!is.null(results$x_col)) {
        app_state$columns$mappings$x_column <- results$x_col
      }
      if (!is.null(results$y_col)) {
        app_state$columns$mappings$y_column <- results$y_col
      }
      if (!is.null(results$n_col)) {
        app_state$columns$mappings$n_column <- results$n_col
      }
      if (!is.null(results$skift_col)) {
        app_state$columns$mappings$skift_column <- results$skift_col
      }
      if (!is.null(results$frys_col)) {
        app_state$columns$mappings$frys_column <- results$frys_col
      }
      if (!is.null(results$kommentar_col)) {
        app_state$columns$mappings$kommentar_column <- results$kommentar_col
      }

      # Store complete results for backward compatibility
      shiny::isolate({
        app_state$columns$auto_detect$results <- results
        # Note: results already stored above in auto_detect$results

        # Mark as completed
        app_state$columns$auto_detect$completed <- TRUE
        app_state$columns$auto_detect$in_progress <- FALSE
      })
    })
  }

  # LEGACY SUPPORT: Return updated list for backward compatibility
  if (is.null(existing_columns)) {
    existing_columns <- list()
  }

  # Update individual column mappings
  if (!is.null(results$x_col)) existing_columns$x_column <- results$x_col
  if (!is.null(results$y_col)) existing_columns$y_column <- results$y_col
  if (!is.null(results$n_col)) existing_columns$n_column <- results$n_col
  if (!is.null(results$skift_col)) existing_columns$skift_column <- results$skift_col
  if (!is.null(results$frys_col)) existing_columns$frys_column <- results$frys_col
  if (!is.null(results$kommentar_col)) existing_columns$kommentar_column <- results$kommentar_col

  # Store complete results for backward compatibility
  existing_columns$auto_detect_results <- results
  # Note: Hierarchical structure is only maintained in app_state, not in legacy return value

  # Mark as completed
  existing_columns$auto_detect_completed <- TRUE
  existing_columns$auto_detect_in_progress <- FALSE

  return(existing_columns)
}

#' Log Autodetect Decisions for Debugging
#'
#' Structured logging of autodetection decisions for development and debugging.
#'
#' @param results Detection results
#' @param trigger_type Type of trigger that initiated detection
#' @param session_id Session identifier for tracking
log_autodetect_decisions <- function(results, trigger_type, session_id = NULL) {
  log_debug_block("AUTODETECT_DECISIONS", "Logging autodetect decisions")

  log_debug_kv(
    x_column_detected = !is.null(results$x_col),
    y_column_detected = !is.null(results$y_col),
    n_column_detected = !is.null(results$n_col),
    comment_column_found = !is.null(results$kommentar_col),
    trigger_type = trigger_type,
    .context = "AUTODETECT_DECISIONS"
  )

  # Autodetect logging handled by existing log_debug() system
}
