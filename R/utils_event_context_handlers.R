#' Event Context Handlers (Strategy Pattern)
#'
#' Context-aware event handling using strategy pattern to reduce cyclomatic
#' complexity and improve maintainability.
#'
#' ## Architecture
#'
#' Instead of complex if/else chains based on context strings, this module
#' implements the **Strategy Pattern** where each context type has its own
#' handler function.
#'
#' **Benefits**:
#' - Reduced cyclomatic complexity (12 → ~3 per function)
#' - Easier to add new context types (just add handler)
#' - Better testability (test each handler independently)
#' - Self-documenting code (function names describe behavior)
#'
#' ## Usage
#'
#' ```r
#' # In event listeners:
#' observeEvent(app_state$events$data_updated, {
#'   update_context <- app_state$last_data_update_context
#'   handle_data_update_by_context(
#'     update_context,
#'     app_state,
#'     emit,
#'     input,
#'     output,
#'     session,
#'     ui_service
#'   )
#' })
#' ```
#'
#' @name utils_event_context_handlers
NULL

# ============================================================================
# CONTEXT CLASSIFICATION
# ============================================================================

#' Classify Update Context
#'
#' Determines the context type from update metadata.
#'
#' @param update_context Update context metadata (list with $context field)
#'
#' @return One of: "load", "table_edit", "data_change", "general"
#'
#' @details
#' Context classification logic:
#' - **load**: Data upload, file loading, new data
#' - **table_edit**: Direct table cell editing
#' - **data_change**: Column changes, modifications
#' - **general**: Fallback for unclassified contexts
#'
#' @examples
#' \dontrun{
#' classify_update_context(list(context = "file_upload"))
#' # Returns: "load"
#'
#' classify_update_context(list(context = "table_cells_edited"))
#' # Returns: "table_edit"
#' }
#'
#' @export
classify_update_context <- function(update_context) {
  if (is.null(update_context) || is.null(update_context$context)) {
    return("general")
  }

  context <- update_context$context
  ctx_lower <- tolower(context)

  # Table cell editing (exact match for precision)
  if (identical(context, "table_cells_edited")) {
    return("table_edit")
  }

  # Load contexts (upload, file, new data)
  if (grepl("load|upload|file|new", ctx_lower, ignore.case = TRUE)) {
    return("load")
  }

  # Data change contexts (edit, modify, change)
  if (grepl("change|edit|modify|column", ctx_lower, ignore.case = FALSE)) {
    return("data_change")
  }

  # Fallback
  "general"
}

#' Resolve Column Update Reason
#'
#' Determines the reason for a column update based on context string.
#'
#' @param context Context string from data update event
#'
#' @return One of: "upload", "edit", "session", "manual"
#'
#' @details
#' Used for logging and behavior branching in column choice updates.
#'
#' @examples
#' \dontrun{
#' resolve_column_update_reason("file_upload") # Returns: "upload"
#' resolve_column_update_reason("table_cells_edited") # Returns: "edit"
#' }
#'
#' @export
resolve_column_update_reason <- function(context) {
  if (is.null(context)) {
    return("manual")
  }

  ctx <- tolower(context)

  if (grepl("edit|change|modify|column", ctx, ignore.case = FALSE)) {
    return("edit")
  }

  if (grepl("session", ctx, ignore.case = FALSE)) {
    return("session")
  }

  if (grepl("load|upload|file|new", ctx, ignore.case = FALSE)) {
    return("upload")
  }

  "manual"
}

# ============================================================================
# CONTEXT-SPECIFIC HANDLERS (STRATEGY PATTERN)
# ============================================================================

#' Handle Load Context
#'
#' Handles data loading scenarios (file upload, new data).
#'
#' @param app_state Centralized app state
#' @param emit Event emission API
#'
#' @details
#' **Behavior**:
#' - Triggers auto-detection if data is available
#' - Unfreezes auto-detect system
#'
#' **Event Chain**:
#' 1. data_updated (load context)
#' 2. → auto_detection_started
#' 3. → auto_detection_completed
#' 4. → ui_sync_needed
#'
#' @keywords internal
handle_load_context <- function(app_state, emit) {
  # Data loading path - trigger auto-detection
  if (!is.null(app_state$data$current_data)) {
    emit$auto_detection_started()
    log_debug(
      "Load context: Triggered auto-detection",
      .context = "EVENT_CONTEXT_HANDLER"
    )
  } else {
    log_debug(
      "Load context: No data available, skipping auto-detection",
      .context = "EVENT_CONTEXT_HANDLER"
    )
  }
}

#' Handle Table Edit Context
#'
#' Handles direct table cell editing.
#'
#' @param app_state Centralized app state
#' @param emit Event emission API
#'
#' @details
#' **Behavior**:
#' - Does NOT trigger auto-detection (preserve user's column mappings)
#' - Triggers navigation change (updates plots)
#' - Triggers visualization update
#'
#' **Event Chain**:
#' 1. data_updated (table_edit context)
#' 2. → navigation_changed
#' 3. → visualization_update_needed
#'
#' @keywords internal
handle_table_edit_context <- function(app_state, emit) {
  # Table cells edited - update plots without changing columns
  emit$navigation_changed()
  emit$visualization_update_needed()

  log_debug(
    "Table edit context: Updated plots without auto-detection",
    .context = "EVENT_CONTEXT_HANDLER"
  )
}

#' Handle Data Change Context
#'
#' Handles data structure changes (column changes, modifications).
#'
#' @param app_state Centralized app state
#' @param emit Event emission API
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param ui_service UI service for UI updates
#' @param context Original context string (for reason classification)
#'
#' @details
#' **Behavior**:
#' - Updates column choices (dropdown options)
#' - Triggers navigation change (updates plots)
#' - Triggers visualization update
#' - Does NOT trigger auto-detection (user has control)
#'
#' **Event Chain**:
#' 1. data_updated (data_change context)
#' 2. → update_column_choices_unified()
#' 3. → navigation_changed
#' 4. → visualization_update_needed
#'
#' @keywords internal
handle_data_change_context <- function(app_state, emit, input, output, session, ui_service, context) {
  # Data change path - update column choices AND trigger plot regeneration
  column_update_reason <- resolve_column_update_reason(context)

  safe_operation(
    "Update column choices on data change",
    code = {
      update_column_choices_unified(
        app_state,
        input,
        output,
        session,
        ui_service,
        reason = column_update_reason
      )
    }
  )

  emit$navigation_changed()
  emit$visualization_update_needed()

  log_debug_kv(
    message = "Data change context: Updated choices and plots",
    reason = column_update_reason,
    .context = "EVENT_CONTEXT_HANDLER"
  )
}

#' Handle General Context
#'
#' Handles general data updates without specific context.
#'
#' @param app_state Centralized app state
#' @param emit Event emission API
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param ui_service UI service for UI updates
#' @param context Original context string (for reason classification)
#'
#' @details
#' **Behavior**:
#' - Updates column choices (dropdown options)
#' - Does NOT trigger auto-detection (conservative fallback)
#' - Does NOT trigger navigation/plot updates
#'
#' **Event Chain**:
#' 1. data_updated (general context)
#' 2. → update_column_choices_unified()
#'
#' @keywords internal
handle_general_context <- function(app_state, emit, input, output, session, ui_service, context) {
  # General data update - NO autodetect by default (only update choices)
  column_update_reason <- resolve_column_update_reason(context)

  safe_operation(
    "Update column choices on data update (general)",
    code = {
      update_column_choices_unified(
        app_state,
        input,
        output,
        session,
        ui_service,
        reason = column_update_reason
      )
    }
  )

  log_debug_kv(
    message = "General context: Updated choices only",
    reason = column_update_reason,
    .context = "EVENT_CONTEXT_HANDLER"
  )
}

# ============================================================================
# MAIN DISPATCHER (STRATEGY PATTERN)
# ============================================================================

#' Handle Data Update By Context
#'
#' Main dispatcher using strategy pattern to handle data updates based on context.
#'
#' @param update_context Update context metadata (list with $context field)
#' @param app_state Centralized app state
#' @param emit Event emission API
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param ui_service UI service for UI updates
#'
#' @details
#' ## Strategy Pattern Implementation
#'
#' This function classifies the update context and delegates to the appropriate
#' handler function, reducing cyclomatic complexity from ~12 to ~3.
#'
#' **Handler Mapping**:
#' - `load` → `handle_load_context()`
#' - `table_edit` → `handle_table_edit_context()`
#' - `data_change` → `handle_data_change_context()`
#' - `general` → `handle_general_context()`
#'
#' ## Cyclomatic Complexity Reduction
#'
#' **Before** (nested if/else):
#' ```r
#' if (is_load_context) {
#'   if (!is.null(data)) { ... }
#' } else if (is_table_edit) {
#'   ...
#' } else if (is_change_context) {
#'   safe_operation({ ... })
#'   emit$navigation_changed()
#'   emit$visualization_update_needed()
#' } else {
#'   safe_operation({ ... })
#' }
#' ```
#' **Complexity**: 12
#'
#' **After** (strategy pattern):
#' ```r
#' context_type <- classify_update_context(update_context)
#' switch(context_type, ...)
#' ```
#' **Complexity**: ~3
#'
#' @export
handle_data_update_by_context <- function(
  update_context,
  app_state,
  emit,
  input,
  output,
  session,
  ui_service
) {
  # Classify context type
  context_type <- classify_update_context(update_context)

  # Get original context string for reason classification
  context_string <- if (!is.null(update_context)) {
    update_context$context %||% "general"
  } else {
    "general"
  }

  # Dispatch to appropriate handler (STRATEGY PATTERN)
  switch(context_type,
    load = handle_load_context(
      app_state = app_state,
      emit = emit
    ),
    table_edit = handle_table_edit_context(
      app_state = app_state,
      emit = emit
    ),
    data_change = handle_data_change_context(
      app_state = app_state,
      emit = emit,
      input = input,
      output = output,
      session = session,
      ui_service = ui_service,
      context = context_string
    ),
    general = handle_general_context(
      app_state = app_state,
      emit = emit,
      input = input,
      output = output,
      session = session,
      ui_service = ui_service,
      context = context_string
    ),

    # Fallback (should never reach)
    {
      log_warn(
        paste("Unknown context type:", context_type),
        .context = "EVENT_CONTEXT_HANDLER"
      )
      handle_general_context(
        app_state,
        emit,
        input,
        output,
        session,
        ui_service,
        context_string
      )
    }
  )

  log_debug_kv(
    message = "Data update context handled",
    context_type = context_type,
    context_string = context_string,
    .context = "EVENT_CONTEXT_HANDLER"
  )
}
