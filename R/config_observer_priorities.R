# ==============================================================================
# CONFIG_OBSERVER_PRIORITIES.R
# ==============================================================================
# FORMÅL: Observer execution priorities til at forhindre race conditions gennem
#         deterministisk eksekveringsrækkefølge. Wide gaps mellem levels sikrer
#         korrekt state management → data processing → UI updates flow.
#
# ANVENDES AF:
#   - Alle observeEvent() calls med priority parameter
#   - Race condition prevention system
#   - Event-bus listeners i utils_event_system.R
#
# RELATERET:
#   - utils_event_system.R - Event listener setup
#   - CLAUDE.md Section 3.1.1 - Race Condition Prevention
#   - See: docs/CONFIGURATION.md for complete guide
# ==============================================================================

# OBSERVER MANAGEMENT ================================

#' Observer Priorities for Race Condition Prevention
#'
#' Prioriteret observer eksekveringsrækkefølge til at forhindre race conditions
#' gennem deterministisk eksekveringsrækkefølge. Wide gaps mellem levels sikrer
#' korrekt state management → data processing → UI updates flow.
#'
#' @format Named list med numeriske prioritetsværdier
#' @details
#' Hierarki (højere tal = højere prioritet):
#' - STATE_MANAGEMENT (2000): Kritisk state management
#' - AUTO_DETECT (1500): Auto-detection logic
#' - DATA_PROCESSING (1250): Data operations
#' - UI_SYNC (750): UI synchronization
#' - PLOT_GENERATION (600): Plot rendering
#' - STATUS_UPDATES (500): Status indicators
#' - CLEANUP (200): Cleanup operations
#' - LOGGING (100): Monitoring og logging
#'
#' Compatibility aliases: HIGH, MEDIUM, LOW, LOWEST
#' Legacy aliases (deprecated): highest, high, medium, low, lowest
#'
#' @export
#' @examples
#' \dontrun{
#' observeEvent(input$data, priority = OBSERVER_PRIORITIES$STATE_MANAGEMENT, {
#'   # Critical state update
#' })
#' }
OBSERVER_PRIORITIES <- list(
  # Høj prioritet - kritisk state management
  STATE_MANAGEMENT = 2000, # Critical state operations

  # Medium prioritet - data processing
  AUTO_DETECT = 1500, # Auto-detection logic
  DATA_PROCESSING = 1250, # Data operations

  # Lav prioritet - UI updates og visuel feedback
  UI_SYNC = 750, # UI synchronization
  PLOT_GENERATION = 600, # Plot rendering
  STATUS_UPDATES = 500, # Status indicators

  # Meget lav prioritet - cleanup og logging
  CLEANUP = 200, # Cleanup operations
  LOGGING = 100, # Monitoring and logging

  # Compatibility aliases for legacy code
  HIGH = 2000, # Maps to STATE_MANAGEMENT
  MEDIUM = 1250, # Maps to DATA_PROCESSING
  LOW = 750, # Maps to UI_SYNC
  LOWEST = 200, # Maps to CLEANUP

  # Legacy aliases (deprecated but supported for backward compatibility)
  highest = 2000, # Use HIGH instead
  high = 1500, # Use AUTO_DETECT instead
  medium = 1000, # Use MEDIUM instead
  low = 500, # Use LOW instead
  lowest = 100 # Use LOWEST instead
)

## Observer Priority Helpers -----

#' Get Observer Priority by Name
#'
#' Standardised priority access function for safe retrieval of priority values.
#'
#' @param priority_name Character string indicating priority level
#' @return Numeric priority value
#' @export
#' @examples
#' \dontrun{
#' get_priority("STATE_MANAGEMENT") # Returns 2000
#' }
get_priority <- function(priority_name) {
  priority_value <- OBSERVER_PRIORITIES[[priority_name]]
  if (is.null(priority_value)) {
    stop(paste("Unknown priority:", priority_name))
  }
  return(priority_value)
}

#' Observer Priority Convenience Functions
#'
#' Quick access functions for commonly used priority levels.
#'
#' @name priority_helpers
#' @return Numeric priority value
#' @export
#' @rdname priority_helpers
#' @examples
#' \dontrun{
#' observeEvent(input$data, priority = priority_high(), {})
#' }
priority_high <- function() OBSERVER_PRIORITIES$STATE_MANAGEMENT

#' @rdname priority_helpers
#' @export
priority_medium <- function() OBSERVER_PRIORITIES$DATA_PROCESSING

#' @rdname priority_helpers
#' @export
priority_low <- function() OBSERVER_PRIORITIES$UI_SYNC

#' @rdname priority_helpers
#' @export
priority_cleanup <- function() OBSERVER_PRIORITIES$CLEANUP
