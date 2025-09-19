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
    log_debug("Updating column choices for:", paste(columns, collapse = ", "), .context = "UI_SERVICE")

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

#' Safe Programmatic UI Update Wrapper
#'
#' Wrapper function that prevents circular event loops during programmatic UI updates.
#' Sets protection flag before updates and clears it afterwards to prevent input observers
#' from triggering new events during programmatic changes.
#'
#' @param session Shiny session object
#' @param app_state Centralized app state with UI protection flags
#' @param update_function Function to execute with protection (should contain updateSelectizeInput calls)
#' @param delay_ms Delay in milliseconds before clearing protection flag (default: 100ms)
#'
#' @examples
#' \dontrun{
#' safe_programmatic_ui_update(session, app_state, function() {
#'   updateSelectizeInput(session, "x_column", choices = choices, selected = "Dato")
#'   updateSelectizeInput(session, "y_column", choices = choices, selected = "Tæller")
#' })
#' }
#'
#' @export
safe_programmatic_ui_update <- function(session, app_state, update_function, delay_ms = 200) {
  log_debug("Starting safe programmatic UI update", .context = "LOOP_PROTECTION")

  tryCatch({
    # Set protection flag to prevent input observers from firing
    app_state$ui$updating_programmatically <- TRUE
    app_state$ui$last_programmatic_update <- Sys.time()

    log_debug("Protection flag set - programmatic updates active", .context = "LOOP_PROTECTION")

    # Execute the UI updates
    update_function()

    log_debug("UI updates completed, scheduling protection flag clear", .context = "LOOP_PROTECTION")

    # Clear protection flag after short delay to ensure all reactive chains complete
    if (requireNamespace("later", quietly = TRUE)) {
      later::later(function() {
        app_state$ui$updating_programmatically <- FALSE
        log_debug("Protection flag cleared - programmatic updates complete", .context = "LOOP_PROTECTION")
      }, delay = delay_ms / 1000)
    } else {
      # Fallback if later package not available
      Sys.sleep(delay_ms / 1000)
      app_state$ui$updating_programmatically <- FALSE
      log_debug("Protection flag cleared (fallback) - programmatic updates complete", .context = "LOOP_PROTECTION")
    }

  }, error = function(e) {
    # Ensure protection flag is cleared even on error
    app_state$ui$updating_programmatically <- FALSE
    log_error(paste("Error in safe programmatic UI update:", e$message), "LOOP_PROTECTION")
    stop(e)
  })
}