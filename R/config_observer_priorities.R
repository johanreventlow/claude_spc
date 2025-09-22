# Observer Priorities Configuration
# Extracted from global.R for better modularity

# OBSERVER MANAGEMENT ================================

## Observer Priorities -----
# Phase 3: Prioriteret observer eksekveringsrækkefølge
# Enhanced with wider gaps to prevent race conditions
OBSERVER_PRIORITIES <- list(
  # Høj prioritet - kritisk state management
  STATE_MANAGEMENT = 2000,    # Critical state operations

  # Medium prioritet - data processing
  AUTO_DETECT = 1500,         # Auto-detection logic
  DATA_PROCESSING = 1250,     # Data operations

  # Lav prioritet - UI updates og visuel feedback
  UI_SYNC = 750,              # UI synchronization
  PLOT_GENERATION = 600,      # Plot rendering
  STATUS_UPDATES = 500,       # Status indicators

  # Meget lav prioritet - cleanup og logging
  CLEANUP = 200,              # Cleanup operations
  LOGGING = 100               # Monitoring and logging
)

## Observer Priority Helpers -----
# Standardised priority access functions

get_priority <- function(priority_name) {
  priority_value <- OBSERVER_PRIORITIES[[priority_name]]
  if (is.null(priority_value)) {
    stop(paste("Unknown priority:", priority_name))
  }
  return(priority_value)
}

# Convenience functions for common priorities
priority_high <- function() OBSERVER_PRIORITIES$STATE_MANAGEMENT
priority_medium <- function() OBSERVER_PRIORITIES$DATA_PROCESSING
priority_low <- function() OBSERVER_PRIORITIES$UI_SYNC
priority_cleanup <- function() OBSERVER_PRIORITIES$CLEANUP