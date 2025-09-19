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

  # Initialize autodetect state if not exists
  if (is.null(app_state$autodetect)) {
    app_state$autodetect <- list(
      frozen_until_next_trigger = FALSE,
      last_run = NULL
    )
  }

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
    frozen_state = isolate(app_state$autodetect$frozen_until_next_trigger) %||% FALSE,
    .context = "UNIFIED_AUTODETECT"
  )

  # 1. TRIGGER VALIDATION - smart unfreezing when data is available
  frozen_state <- isolate(app_state$autodetect$frozen_until_next_trigger) %||% FALSE

  # SMART UNFREEZE: If we have data available and we're frozen, automatically unfreeze
  if (frozen_state && !is.null(data) && nrow(data) > 0 && trigger_type == "file_upload") {
    log_debug("SMART UNFREEZE: Data available, unfreezing autodetect system automatically", .context = "UNIFIED_AUTODETECT")
    app_state$autodetect$frozen_until_next_trigger <- FALSE
    frozen_state <- FALSE
  }

  if (frozen_state && trigger_type != "manual") {
    log_debug("Autodetect skipped - system frozen until next trigger (use manual to override)",
              .context = "UNIFIED_AUTODETECT")
    return(invisible(NULL))
  }

  # 2. SCENARIO ROUTING - based on trigger type and data availability
  log_debug(paste("Routing to scenario:", trigger_type), .context = "UNIFIED_AUTODETECT")

  if (is.null(data) || nrow(data) == 0) {
    # Session start / name-only scenario
    log_debug("Name-only detection mode (no data or empty data)", .context = "UNIFIED_AUTODETECT")
    col_names <- if (is.null(data)) character(0) else names(data)
    results <- detect_columns_name_based(col_names, app_state)
  } else {
    # Full data analysis scenario
    log_debug("Full data analysis mode", .context = "UNIFIED_AUTODETECT")
    log_debug_kv(
      data_dimensions = paste(dim(data), collapse = "x"),
      column_names = paste(names(data), collapse = ", "),
      .context = "UNIFIED_AUTODETECT"
    )
    results <- detect_columns_full_analysis(data, app_state)
  }

  # 3. STATE UPDATE & FREEZE
  log_debug("Updating app_state with detection results", .context = "UNIFIED_AUTODETECT")

  # Update all column mappings in unified location - pass app_state for direct updates
  app_state$columns <- update_all_column_mappings(results, app_state$columns, app_state)

  # Set frozen state to prevent re-running until next legitimate trigger
  app_state$autodetect$frozen_until_next_trigger <- TRUE
  app_state$autodetect$last_run <- list(
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

  log_debug_kv(
    frozen_state = "TRUE",
    trigger_type = trigger_type,
    timestamp = as.character(Sys.time()),
    .context = "UNIFIED_AUTODETECT"
  )
  log_debug("✅ Autodetect frozen until next trigger", .context = "UNIFIED_AUTODETECT")

  # 4. UI SYNC & LOGGING
  log_autodetect_decisions(results, trigger_type, session_id)

  # Emit completion event for UI updates
  emit$auto_detection_completed()

  log_debug("✅ Unified autodetect engine completed", .context = "UNIFIED_AUTODETECT")

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
detect_columns_name_based <- function(col_names, app_state = NULL) {
  log_debug_block("NAME_BASED_DETECT", "Starting name-based column detection")

  if (length(col_names) == 0) {
    log_debug("No column names provided - returning empty results", .context = "NAME_BASED_DETECT")
    return(list(
      x_col = NULL, y_col = NULL, n_col = NULL,
      skift_col = NULL, frys_col = NULL, kommentar_col = NULL
    ))
  }

  log_debug_kv(
    column_count = length(col_names),
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

  # X-column (date/time detection) - FASE 4: Enhanced tidsspecifikke patterns
  dato_patterns <- c("dato", "date", "tid", "time", "år", "year", "måned", "month",
                     "uge", "week", "dag", "day", "periode", "period", "kvartal", "quarter",
                     "jan", "feb", "mar", "apr", "maj", "jun",
                     "jul", "aug", "sep", "okt", "nov", "dec")
  for (pattern in dato_patterns) {
    dato_idx <- which(grepl(pattern, col_names_lower, ignore.case = TRUE))
    if (length(dato_idx) > 0) {
      x_col <- col_names[dato_idx[1]]
      log_debug_kv(x_column_detected = x_col, pattern_used = pattern, .context = "NAME_BASED_DETECT")
      break
    }
  }

  # Fallback: first column if no date pattern found
  if (is.null(x_col) && length(col_names) > 0) {
    x_col <- col_names[1]
    log_debug_kv(x_column_fallback = x_col, .context = "NAME_BASED_DETECT")
  }

  # Y-column (count/value detection) - enhanced patterns
  count_patterns <- c("tæller", "tael", "num", "count", "værdi", "value", "antal")
  for (pattern in count_patterns) {
    count_idx <- which(grepl(pattern, col_names_lower, ignore.case = TRUE))
    if (length(count_idx) > 0) {
      y_col <- col_names[count_idx[1]]
      log_debug_kv(y_column_detected = y_col, pattern_used = pattern, .context = "NAME_BASED_DETECT")
      break
    }
  }

  # N-column (denominator detection) - enhanced patterns
  denom_patterns <- c("nævner", "naev", "denom", "total", "samlet")
  for (pattern in denom_patterns) {
    denom_idx <- which(grepl(pattern, col_names_lower, ignore.case = TRUE))
    if (length(denom_idx) > 0) {
      n_col <- col_names[denom_idx[1]]
      log_debug_kv(n_column_detected = n_col, pattern_used = pattern, .context = "NAME_BASED_DETECT")
      break
    }
  }

  # Exact matches for control columns
  skift_idx <- which(grepl("^skift$", col_names_lower, ignore.case = TRUE))
  if (length(skift_idx) > 0) {
    skift_col <- col_names[skift_idx[1]]
    log_debug_kv(skift_column_detected = skift_col, .context = "NAME_BASED_DETECT")
  }

  frys_idx <- which(grepl("^frys$", col_names_lower, ignore.case = TRUE))
  if (length(frys_idx) > 0) {
    frys_col <- col_names[frys_idx[1]]
    log_debug_kv(frys_column_detected = frys_col, .context = "NAME_BASED_DETECT")
  }

  # Comment column - enhanced patterns
  comment_patterns <- c("kommentar", "comment", "note", "bemærk", "beskrivelse", "description")
  for (pattern in comment_patterns) {
    comment_idx <- which(grepl(pattern, col_names_lower, ignore.case = TRUE))
    if (length(comment_idx) > 0) {
      kommentar_col <- col_names[comment_idx[1]]
      log_debug_kv(comment_column_detected = kommentar_col, pattern_used = pattern, .context = "NAME_BASED_DETECT")
      break
    }
  }

  # Results summary
  results <- list(
    x_col = x_col,
    y_col = y_col,
    n_col = n_col,
    skift_col = skift_col,
    frys_col = frys_col,
    kommentar_col = kommentar_col
  )

  log_debug("Name-based detection results:", .context = "NAME_BASED_DETECT")
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
    log_debug("No data for full analysis - falling back to empty results", .context = "FULL_DATA_DETECT")
    return(list(
      x_col = NULL, y_col = NULL, n_col = NULL,
      skift_col = NULL, frys_col = NULL, kommentar_col = NULL
    ))
  }

  log_debug_kv(
    data_dimensions = paste(dim(data), collapse = "x"),
    column_names = paste(names(data), collapse = ", "),
    .context = "FULL_DATA_DETECT"
  )

  # Start with name-based detection as foundation
  name_based_results <- detect_columns_name_based(names(data), app_state)

  # Enhance with data-driven detection

  # 1. ROBUST DATE DETECTION using lubridate
  date_candidates <- detect_date_columns_robust(data)

  # Choose best date column based on detection confidence
  best_date_col <- NULL
  if (length(date_candidates) > 0) {
    # Sort by score and take highest confidence
    sorted_dates <- date_candidates[order(sapply(date_candidates, function(x) x$score), decreasing = TRUE)]
    best_date_col <- names(sorted_dates)[1]
    log_debug_kv(
      best_date_column = best_date_col,
      confidence_score = sorted_dates[[1]]$score,
      .context = "FULL_DATA_DETECT"
    )
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

  log_debug("Full analysis detection results:", .context = "FULL_DATA_DETECT")
  log_debug_kv(
    x_col_final = ifelse(is.null(results$x_col), "NULL", results$x_col),
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
    # Look for app_state in the calling environment chain
    for (i in 1:10) {
      env <- parent.frame(i)
      if (exists("app_state", envir = env)) {
        app_state <- get("app_state", envir = env)
        log_debug("Found app_state in parent environment (frame", i, ")", .context = "UPDATE_MAPPINGS")
        break
      }
    }
  }

  # DIRECT APP_STATE UPDATE: If app_state is provided, update it directly
  if (!is.null(app_state)) {
    log_debug("Updating app_state$columns directly with autodetect results", .context = "UPDATE_MAPPINGS")

    # Update individual column mappings directly in app_state
    if (!is.null(results$x_col)) {
      app_state$columns$mappings$x_column <- results$x_col
      log_debug(paste("Set app_state$columns$mappings$x_column =", results$x_col), .context = "UPDATE_MAPPINGS")
    }
    if (!is.null(results$y_col)) {
      app_state$columns$mappings$y_column <- results$y_col
      log_debug(paste("Set app_state$columns$mappings$y_column =", results$y_col), .context = "UPDATE_MAPPINGS")
    }
    if (!is.null(results$n_col)) {
      app_state$columns$mappings$n_column <- results$n_col
      log_debug(paste("Set app_state$columns$mappings$n_column =", results$n_col), .context = "UPDATE_MAPPINGS")
    }
    if (!is.null(results$skift_col)) {
      app_state$columns$mappings$skift_column <- results$skift_col
      log_debug(paste("Set app_state$columns$mappings$skift_column =", results$skift_col), .context = "UPDATE_MAPPINGS")
    }
    if (!is.null(results$frys_col)) {
      app_state$columns$mappings$frys_column <- results$frys_col
      log_debug(paste("Set app_state$columns$mappings$frys_column =", results$frys_col), .context = "UPDATE_MAPPINGS")
    }
    if (!is.null(results$kommentar_col)) {
      app_state$columns$mappings$kommentar_column <- results$kommentar_col
      log_debug(paste("Set app_state$columns$mappings$kommentar_column =", results$kommentar_col), .context = "UPDATE_MAPPINGS")
    }

    # Store complete results for backward compatibility
    app_state$columns$auto_detect$results <- results
    app_state$columns$auto_detected_columns <- results

    # Mark as completed
    app_state$columns$auto_detect$completed <- TRUE
    app_state$columns$auto_detect$in_progress <- FALSE

    log_debug("✅ Direct app_state update completed", .context = "UPDATE_MAPPINGS")
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
  existing_columns$auto_detected_columns <- results

  # Mark as completed
  existing_columns$auto_detect_completed <- TRUE
  existing_columns$auto_detect_in_progress <- FALSE

  log_debug("✅ Column mappings updated in unified state", .context = "UPDATE_MAPPINGS")

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
    trigger_type = trigger_type,
    session_id = session_id %||% "unknown",
    x_column_decision = results$x_col %||% "NULL",
    y_column_decision = results$y_col %||% "NULL",
    n_column_decision = results$n_col %||% "NULL",
    control_columns_found = !is.null(results$skift_col) || !is.null(results$frys_col),
    comment_column_found = !is.null(results$kommentar_col),
    .context = "AUTODETECT_DECISIONS"
  )

  # TODO: Integration with lgr for structured logging when implemented
  # lgr::lgr$info("Autodetect completed", trigger = trigger_type, results = results)
}