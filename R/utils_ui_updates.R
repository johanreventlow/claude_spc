# utils_ui_updates.R
# Centralized UI update service for unified patterns

#' Create UI Update Service
#'
#' Creates a centralized service for managing UI updates in a consistent way.
#' This service provides unified patterns for column choice updates, form field updates,
#' and other common UI operations.
#'
#' @param session Shiny session object
#' @param app_state Centralized app state
#' @return List of UI update functions
#'
#' @examples
#' \dontrun{
#' ui_service <- create_ui_update_service(session, app_state)
#' ui_service$update_column_choices()
#' }
#'
#' @export
create_ui_update_service <- function(session, app_state) {

  #' Update Column Choices
  #'
  #' Unified function for updating column choice inputs across the app.
  #' Handles both automatic choice generation from current data and manual choices.
  #'
  #' @param choices Named vector of choices. If NULL, generated from current_data
  #' @param selected Named list of selected values for each column
  #' @param columns Vector of column input IDs to update
  #' @param clear_selections If TRUE, clear all selections
  #'
  update_column_choices <- function(choices = NULL, selected = NULL, columns = c("x_column", "y_column", "n_column", "skift_column", "frys_column", "kommentar_column"), clear_selections = FALSE) {
    cat("DROPDOWN_DEBUG: update_column_choices called for:", paste(columns, collapse = ", "), "\n")
    cat(paste("DROPDOWN_DEBUG: Parameters - choices:", if(!is.null(choices)) paste0("[", length(choices), " items]") else "NULL",
                   "selected:", if(!is.null(selected)) paste0("[", length(selected), " items]") else "NULL",
                   "clear_selections:", clear_selections, "\n"))

    if (!is.null(selected)) {
      for (col in names(selected)) {
        cat(paste("DROPDOWN_DEBUG: Selected value for", col, ":", selected[[col]], "\n"))
      }
    }

    # Generate choices from current data if not provided
    if (is.null(choices)) {
      current_data <- app_state$data$current_data
      if (!is.null(current_data)) {
        all_cols <- names(current_data)
        choices <- setNames(
          c("", all_cols),
          c("Vælg kolonne...", all_cols)
        )
        log_debug("Generated", length(all_cols), "column choices from current data", .context = "UI_SERVICE")
      } else {
        choices <- setNames("", "Vælg kolonne...")
        log_debug("No current data available, using empty choices", .context = "UI_SERVICE")
      }
    }

    # Handle selections
    if (clear_selections) {
      selected <- setNames(rep("", length(columns)), columns)
      log_debug("Clearing all selections", .context = "UI_SERVICE")
    } else if (is.null(selected)) {
      # AUTO-READ FROM APP_STATE: When no selections provided, read current values from app_state
      selected <- list()
      for (col in columns) {
        # Priority: input[[col]] > app_state$columns[[col]] > ""
        current_val <- tryCatch({
          if (!is.null(input[[col]]) && input[[col]] != "") {
            input[[col]]
          } else if (!is.null(isolate(app_state$columns[[col]]))) {
            isolate(app_state$columns[[col]])
          } else {
            ""
          }
        }, error = function(e) {
          log_debug(paste("Error reading", col, "from app_state:", e$message), .context = "UI_SERVICE")
          ""
        })
        selected[[col]] <- current_val
        log_debug(paste("Auto-read selection for", col, ":", current_val), .context = "UI_SERVICE")
      }
    }

    # Update each column input using safe wrapper to prevent loops
    safe_programmatic_ui_update(session, app_state, function() {
      tryCatch({
        for (col in columns) {
          selected_value <- if (!is.null(selected) && col %in% names(selected)) selected[[col]] else ""
          updateSelectizeInput(session, col, choices = choices, selected = selected_value)
          log_debug("Updated", col, "with selected:", selected_value, .context = "UI_SERVICE")
        }
        log_debug("✅ Column choices updated successfully", .context = "UI_SERVICE")
      }, error = function(e) {
        log_error(paste("Error updating column choices:", e$message), "UI_SERVICE")
      })
    })
  }

  #' Update Form Fields
  #'
  #' Unified function for updating form field inputs from metadata.
  #' Used for session restore and metadata loading operations.
  #'
  #' @param metadata List containing field values to update
  #' @param fields Vector of field names to update. If NULL, updates all available fields
  #'
  update_form_fields <- function(metadata, fields = NULL) {
    log_debug("Updating form fields from metadata", .context = "UI_SERVICE")

    if (is.null(fields)) {
      # Default fields to update
      fields <- c("indicator_title", "unit_select", "unit_custom", "indicator_description",
                  "chart_type", "x_column", "y_column", "n_column", "target_value",
                  "centerline_value", "y_axis_unit")
    }

    isolate({
      tryCatch({
        for (field in fields) {
          if (!is.null(metadata[[field]])) {
            if (field == "indicator_title") {
              updateTextInput(session, field, value = metadata[[field]])
            } else if (field == "unit_custom") {
              updateTextInput(session, field, value = metadata[[field]])
            } else if (field == "indicator_description") {
              updateTextAreaInput(session, field, value = metadata[[field]])
            } else if (field == "target_value") {
              updateTextInput(session, field, value = metadata[[field]])
            } else if (field == "centerline_value") {
              updateTextInput(session, field, value = metadata[[field]])
            } else if (field %in% c("unit_select", "chart_type", "x_column", "y_column", "n_column", "y_axis_unit")) {
              updateSelectizeInput(session, field, selected = metadata[[field]])
            }
            log_debug("Updated", field, "to:", metadata[[field]], .context = "UI_SERVICE")
          }
        }
        log_debug("✅ Form fields updated successfully", .context = "UI_SERVICE")
      }, error = function(e) {
        log_error("Error updating form fields:", e$message, "UI_SERVICE")
      })
    })
  }

  #' Reset Form Fields
  #'
  #' Unified function for resetting form fields to default values.
  #' Used for "Start ny session" and similar reset operations.
  #'
  reset_form_fields <- function() {
    log_debug("Resetting form fields to defaults", .context = "UI_SERVICE")

    isolate({
      tryCatch({
        # Reset text inputs
        updateTextInput(session, "indicator_title", value = "")
        updateTextInput(session, "unit_custom", value = "")
        updateTextInput(session, "target_value", value = "")
        updateTextInput(session, "centerline_value", value = "")

        # Reset select inputs
        updateSelectizeInput(session, "unit_select", selected = "")
        updateSelectizeInput(session, "chart_type", selected = "run")
        updateSelectizeInput(session, "y_axis_unit", selected = "count")

        # Reset column choices (will be empty until data is loaded)
        update_column_choices(clear_selections = TRUE)

        log_debug("✅ Form fields reset successfully", .context = "UI_SERVICE")
      }, error = function(e) {
        log_error("Error resetting form fields:", e$message, "UI_SERVICE")
      })
    })
  }

  #' Show/Hide UI Elements
  #'
  #' Unified function for showing/hiding UI elements conditionally.
  #'
  #' @param element_id Character string of element ID
  #' @param show Logical, whether to show (TRUE) or hide (FALSE) the element
  #'
  toggle_ui_element <- function(element_id, show = TRUE) {
    tryCatch({
      if (show) {
        shinyjs::show(element_id)
        log_debug("Showed element:", element_id, .context = "UI_SERVICE")
      } else {
        shinyjs::hide(element_id)
        log_debug("Hid element:", element_id, .context = "UI_SERVICE")
      }
    }, error = function(e) {
      log_error("Error toggling element", element_id, ":", e$message, "UI_SERVICE")
    })
  }

  #' Validate Form Fields
  #'
  #' Enhanced function for validating form field values with feedback.
  #'
  #' @param field_rules List of validation rules for each field
  #' @param show_feedback Whether to show validation feedback to the user
  #'
  validate_form_fields <- function(field_rules, show_feedback = TRUE) {
    log_debug("Validating form fields", .context = "UI_SERVICE")

    validation_results <- list(valid = TRUE, errors = list())

    tryCatch({
      for (field_name in names(field_rules)) {
        rule <- field_rules[[field_name]]
        field_value <- session$input[[field_name]]

        # Required field validation
        if (isTRUE(rule$required) && (is.null(field_value) || field_value == "")) {
          validation_results$valid <- FALSE
          validation_results$errors[[field_name]] <- "Dette felt er påkrævet"

          if (show_feedback) {
            shinyjs::addClass(field_name, "has-error")
          }
        }

        # Numeric validation
        if (!is.null(rule$type) && rule$type == "numeric" && !is.null(field_value) && field_value != "") {
          if (is.na(as.numeric(field_value))) {
            validation_results$valid <- FALSE
            validation_results$errors[[field_name]] <- "Skal være et tal"

            if (show_feedback) {
              shinyjs::addClass(field_name, "has-error")
            }
          }
        }

        # Custom validation function
        if (!is.null(rule$validator) && is.function(rule$validator)) {
          custom_result <- rule$validator(field_value)
          if (!isTRUE(custom_result)) {
            validation_results$valid <- FALSE
            validation_results$errors[[field_name]] <- custom_result

            if (show_feedback) {
              shinyjs::addClass(field_name, "has-error")
            }
          }
        }

        # Remove error styling if field is valid
        if (show_feedback && !field_name %in% names(validation_results$errors)) {
          shinyjs::removeClass(field_name, "has-error")
        }
      }

      log_debug("Form validation completed, valid:", validation_results$valid, .context = "UI_SERVICE")

    }, error = function(e) {
      log_error("Error during form validation:", e$message, "UI_SERVICE")
      validation_results$valid <- FALSE
      validation_results$errors[["general"]] <- "Validationsfejl"
    })

    return(validation_results)
  }

  #' Show User Feedback
  #'
  #' Unified function for showing user feedback (notifications, modals, etc.).
  #'
  #' @param message Character string with the message to show
  #' @param type Type of feedback: "success", "info", "warning", "error"
  #' @param duration Duration in seconds (NULL for persistent)
  #' @param modal Whether to show as modal dialog instead of notification
  #'
  show_user_feedback <- function(message, type = "info", duration = 3, modal = FALSE) {
    log_debug("Showing user feedback:", type, "-", message, .context = "UI_SERVICE")

    tryCatch({
      if (modal) {
        # Show as modal dialog
        showModal(modalDialog(
          title = switch(type,
                        "success" = "Success",
                        "info" = "Information",
                        "warning" = "Advarsel",
                        "error" = "Fejl",
                        "Information"),
          message,
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      } else {
        # Show as notification
        shiny_type <- switch(type,
                            "success" = "message",
                            "info" = "default",
                            "warning" = "warning",
                            "error" = "error",
                            "default")

        showNotification(message, type = shiny_type, duration = duration)
      }

      log_debug("✅ User feedback shown successfully", .context = "UI_SERVICE")

    }, error = function(e) {
      log_error("Error showing user feedback:", e$message, "UI_SERVICE")
    })
  }

  #' Update UI State Conditionally
  #'
  #' Enhanced function for updating UI state based on conditions.
  #'
  #' @param conditions Named list of conditions and corresponding UI updates
  #'
  update_ui_conditionally <- function(conditions) {
    log_debug("Updating UI conditionally based on", length(conditions), "conditions", .context = "UI_SERVICE")

    tryCatch({
      for (condition_name in names(conditions)) {
        condition_spec <- conditions[[condition_name]]

        # Evaluate condition
        condition_met <- if (is.function(condition_spec$condition)) {
          condition_spec$condition()
        } else {
          condition_spec$condition
        }

        log_debug("Condition", condition_name, ":", condition_met, .context = "UI_SERVICE")

        if (condition_met) {
          # Execute actions for true condition
          if (!is.null(condition_spec$actions$show)) {
            for (element in condition_spec$actions$show) {
              shinyjs::show(element)
            }
          }

          if (!is.null(condition_spec$actions$hide)) {
            for (element in condition_spec$actions$hide) {
              shinyjs::hide(element)
            }
          }

          if (!is.null(condition_spec$actions$enable)) {
            for (element in condition_spec$actions$enable) {
              shinyjs::enable(element)
            }
          }

          if (!is.null(condition_spec$actions$disable)) {
            for (element in condition_spec$actions$disable) {
              shinyjs::disable(element)
            }
          }

          if (!is.null(condition_spec$actions$update)) {
            for (update_spec in condition_spec$actions$update) {
              do.call(update_spec$func, update_spec$args)
            }
          }
        }
      }

      log_debug("✅ Conditional UI updates completed", .context = "UI_SERVICE")

    }, error = function(e) {
      log_error("Error in conditional UI updates:", e$message, "UI_SERVICE")
    })
  }

  # Return enhanced service interface
  list(
    update_column_choices = update_column_choices,
    update_form_fields = update_form_fields,
    reset_form_fields = reset_form_fields,
    toggle_ui_element = toggle_ui_element,
    validate_form_fields = validate_form_fields,
    show_user_feedback = show_user_feedback,
    update_ui_conditionally = update_ui_conditionally
  )
}

#' Create UI Update Events
#'
#' Add UI update events to the existing app_state$events.
#' These events trigger centralized UI updates through the event system.
#'
#' @param app_state Existing app state with events
#'
add_ui_update_events <- function(app_state) {
  # Add UI update events to existing events
  app_state$events$ui_update_needed <- 0L
  app_state$events$column_choices_changed <- 0L
  app_state$events$form_reset_needed <- 0L
  app_state$events$form_restore_needed <- 0L

  log_debug("UI update events added to app_state", .context = "UI_SERVICE")
  return(app_state)
}

#' Add UI Update Emit Functions
#'
#' Add UI update emit functions to the existing emit API.
#'
#' @param emit Existing emit API
#' @param app_state App state containing events
#'
add_ui_update_emit_functions <- function(emit, app_state) {
  # Add UI update emit functions
  emit$ui_update_needed <- function() {
    isolate({
      app_state$events$ui_update_needed <- app_state$events$ui_update_needed + 1L
      log_debug("ui_update_needed emitted:", app_state$events$ui_update_needed, .context = "EVENT")
    })
  }

  emit$column_choices_changed <- function() {
    isolate({
      app_state$events$column_choices_changed <- app_state$events$column_choices_changed + 1L
      log_debug("column_choices_changed emitted:", app_state$events$column_choices_changed, .context = "EVENT")
    })
  }

  emit$form_reset_needed <- function() {
    isolate({
      app_state$events$form_reset_needed <- app_state$events$form_reset_needed + 1L
      log_debug("form_reset_needed emitted:", app_state$events$form_reset_needed, .context = "EVENT")
    })
  }

  emit$form_restore_needed <- function() {
    isolate({
      app_state$events$form_restore_needed <- app_state$events$form_restore_needed + 1L
      log_debug("form_restore_needed emitted:", app_state$events$form_restore_needed, .context = "EVENT")
    })
  }

  log_debug("UI update emit functions added", .context = "UI_SERVICE")
  return(emit)
}

#' Safe Programmatic UI Update Wrapper (Enhanced with Intelligent Flag Clearing)
#'
#' Advanced wrapper function that prevents circular event loops during programmatic UI updates.
#' Uses intelligent flag-clearing strategy with session$onFlushed for immediate clearing after
#' Shiny processes updates, with later::later() safety fallback for robust operation.
#'
#' KEY FEATURES:
#' - Configurable delay from LOOP_PROTECTION_DELAYS constants
#' - Single-reset guarantee to prevent double flag clearing
#' - session$onFlushed primary strategy for immediate response
#' - later::later() safety fallback with double delay
#' - Comprehensive timing logging for performance optimization
#' - Freeze-aware logging that respects autodetect state without interference
#' - Automatic session cleanup on error conditions
#'
#' TIMING STRATEGY:
#' 1. session$onFlushed(): Immediate clearing after Shiny processes UI updates
#' 2. later::later() fallback: Safety net with 2x delay if onFlushed doesn't fire
#' 3. Synchronous delay: Last resort if later package unavailable
#'
#' @param session Shiny session object (must support onFlushed for optimal performance)
#' @param app_state Centralized app state with UI protection flags and autodetect state
#' @param update_function Function to execute with protection (should contain updateSelectizeInput calls)
#' @param delay_ms Delay in milliseconds (default: uses LOOP_PROTECTION_DELAYS$default from constants)
#'
#' @examples
#' \dontrun{
#' # Standard usage with default 500ms delay
#' safe_programmatic_ui_update(session, app_state, function() {
#'   updateSelectizeInput(session, "x_column", choices = choices, selected = "Dato")
#'   updateSelectizeInput(session, "y_column", choices = choices, selected = "Tæller")
#' })
#'
#' # Custom delay for slow environments
#' safe_programmatic_ui_update(session, app_state, function() {
#'   updateSelectizeInput(session, "x_column", choices = choices, selected = "Dato")
#' }, delay_ms = LOOP_PROTECTION_DELAYS$conservative)
#' }
#'
#' @export
safe_programmatic_ui_update <- function(session, app_state, update_function, delay_ms = NULL) {
  # Use configured delay from constants if not specified
  if (is.null(delay_ms)) {
    delay_ms <- LOOP_PROTECTION_DELAYS$default
  }

  update_start_time <- Sys.time()

  # FREEZE-AWARE LOGGING: Observe freeze state without modification
  freeze_state <- if (!is.null(app_state$autodetect)) {
    isolate(app_state$autodetect$frozen_until_next_trigger) %||% FALSE
  } else { FALSE }

  cat(paste("DROPDOWN_DEBUG: ⭐ Starting safe programmatic UI update with", delay_ms, "ms delay",
                 "(autodetect frozen:", freeze_state, ")\n"))

  tryCatch({
    # RACE CONDITION FIX: Handle overlapping calls more intelligently
    current_flag_state <- isolate(app_state$ui$updating_programmatically)
    if (isTRUE(current_flag_state)) {
      cat("DROPDOWN_DEBUG: ⚠️ Another update in progress - clearing flag and proceeding\n")
      # Force clear the flag to handle race conditions between autodetect calls
      app_state$ui$updating_programmatically <- FALSE
      app_state$ui$flag_reset_scheduled <- TRUE
      # Small delay to ensure any pending callbacks complete
      Sys.sleep(0.1)
    }

    # Set protection flag to prevent input observers from firing
    app_state$ui$updating_programmatically <- TRUE
    app_state$ui$last_programmatic_update <- update_start_time
    app_state$ui$flag_reset_scheduled <- FALSE

    cat("DROPDOWN_DEBUG: ✅ LOOP_PROTECTION flag set to TRUE, executing UI updates\n")

    # TOKEN GENERATION: Generate unique token for this UI update session
    app_state$ui$programmatic_token_counter <- isolate(app_state$ui$programmatic_token_counter) + 1L
    session_token <- paste0("token_", isolate(app_state$ui$programmatic_token_counter), "_", format(update_start_time, "%H%M%S_%f"))
    cat(paste("TOKEN_DEBUG: Generated session token:", session_token, "\n"))

    # DROPDOWN DEBUGGING + TOKEN TRACKING: Wrap updateSelectizeInput to log and track tokens
    original_updateSelectizeInput <- updateSelectizeInput
    updateSelectizeInput <- function(session, inputId, choices = NULL, selected = NULL, ...) {
      cat(paste("DROPDOWN_DEBUG: Updating", inputId,
                     "with choices:", if(!is.null(choices)) paste0("[", length(choices), " items]") else "NULL",
                     "selected:", if(!is.null(selected)) paste0("'", selected, "'") else "NULL", "\n"))

      if (!is.null(choices) && length(choices) > 0) {
        cat(paste("DROPDOWN_DEBUG: Choices for", inputId, ":", paste(names(choices), "=", choices, collapse = ", "), "\n"))
      }

      # TOKEN TRACKING: Record this programmatic input with token
      if (!is.null(selected)) {
        input_token <- paste0(session_token, "_", inputId)
        app_state$ui$pending_programmatic_inputs[[inputId]] <- list(
          token = input_token,
          value = selected,
          timestamp = Sys.time(),
          session_token = session_token
        )
        cat(paste("TOKEN_DEBUG: Token", input_token, "assigned to", inputId, "with value", paste0("'", selected, "'"), "\n"))
      }

      result <- original_updateSelectizeInput(session, inputId, choices = choices, selected = selected, ...)
      cat(paste("DROPDOWN_DEBUG: updateSelectizeInput completed for", inputId, "\n"))
      return(result)
    }

    # Execute the UI updates with debugging wrapper
    tryCatch({
      update_function()
    }, finally = {
      # Restore original function
      updateSelectizeInput <- original_updateSelectizeInput
    })

    update_completed_time <- Sys.time()
    execution_time_ms <- as.numeric(difftime(update_completed_time, update_start_time, units = "secs")) * 1000
    log_debug(paste("LOOP_PROTECTION: UI updates completed in", round(execution_time_ms, 2), "ms"), .context = "LOOP_PROTECTION")

    # INTELLIGENT FLAG CLEARING: Try session$onFlushed first, then later::later() as fallback
    clear_protection_flag <- function() {
      if (!isTRUE(isolate(app_state$ui$flag_reset_scheduled))) {
        app_state$ui$flag_reset_scheduled <- TRUE
        app_state$ui$updating_programmatically <- FALSE
        flag_clear_time <- Sys.time()
        total_time_ms <- as.numeric(difftime(flag_clear_time, update_start_time, units = "secs")) * 1000
        cat(paste("DROPDOWN_DEBUG: 🚫 LOOP_PROTECTION flag cleared after", round(total_time_ms, 2), "ms total\n"))
      }
    }

    # TEST ENVIRONMENT: Clear flag immediately for test compatibility
    is_test_environment <- (is.null(session) || is.list(session))
    if (is_test_environment) {
      cat("DROPDOWN_DEBUG: Test environment detected - clearing flag immediately\n")
      clear_protection_flag()
    } else {
      # PRODUCTION: Try session$onFlushed for immediate clearing after Shiny processes updates
      if (!is.null(session) && !is.null(session$onFlushed)) {
        cat("DROPDOWN_DEBUG: Using session$onFlushed for immediate flag clearing\n")
        session$onFlushed(clear_protection_flag, once = TRUE)

        # Safety fallback with later::later() in case onFlushed doesn't fire
        if (requireNamespace("later", quietly = TRUE)) {
          later::later(function() {
            if (isTRUE(isolate(app_state$ui$updating_programmatically))) {
              cat("DROPDOWN_DEBUG: Safety fallback triggered - onFlushed didn't fire\n")
              clear_protection_flag()
            }
          }, delay = (delay_ms * 2) / 1000)  # Double delay for safety fallback
        }
      } else {
        # Fallback to later::later() if session$onFlushed not available
        cat("DROPDOWN_DEBUG: session$onFlushed not available, using later::later()\n")
        if (requireNamespace("later", quietly = TRUE)) {
          later::later(clear_protection_flag, delay = delay_ms / 1000)
        } else {
          # Last resort: synchronous delay (not recommended for production)
          cat("DROPDOWN_DEBUG: later package not available, using synchronous delay\n")
          Sys.sleep(delay_ms / 1000)
          clear_protection_flag()
        }
      }
    }

  }, error = function(e) {
    # ENSURE CLEANUP: Always clear protection flag on error
    app_state$ui$updating_programmatically <- FALSE
    app_state$ui$flag_reset_scheduled <- TRUE
    error_time <- Sys.time()
    total_error_time_ms <- as.numeric(difftime(error_time, update_start_time, units = "secs")) * 1000
    log_error(paste("LOOP_PROTECTION: Error after", round(total_error_time_ms, 2), "ms:", e$message), "LOOP_PROTECTION")
    stop(e)
  })
}