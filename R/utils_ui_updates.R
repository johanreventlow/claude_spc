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
  update_column_choices <- function(choices = NULL, selected = NULL, columns = c("x_column", "y_column", "n_column", "cl_column"), clear_selections = FALSE) {
    cat("DEBUG: [UI_SERVICE] Updating column choices for:", paste(columns, collapse = ", "), "\n")

    # Generate choices from current data if not provided
    if (is.null(choices)) {
      current_data <- app_state$data$current_data
      if (!is.null(current_data)) {
        all_cols <- names(current_data)
        choices <- setNames(
          c("", all_cols),
          c("Vælg kolonne...", all_cols)
        )
        cat("DEBUG: [UI_SERVICE] Generated", length(all_cols), "column choices from current data\n")
      } else {
        choices <- setNames("", "Vælg kolonne...")
        cat("DEBUG: [UI_SERVICE] No current data available, using empty choices\n")
      }
    }

    # Handle selections
    if (clear_selections) {
      selected <- setNames(rep("", length(columns)), columns)
      cat("DEBUG: [UI_SERVICE] Clearing all selections\n")
    }

    # Update each column input
    tryCatch({
      for (col in columns) {
        selected_value <- if (!is.null(selected) && col %in% names(selected)) selected[[col]] else ""
        updateSelectizeInput(session, col, choices = choices, selected = selected_value)
        cat("DEBUG: [UI_SERVICE] Updated", col, "with selected:", selected_value, "\n")
      }
      cat("DEBUG: [UI_SERVICE] ✅ Column choices updated successfully\n")
    }, error = function(e) {
      cat("DEBUG: [UI_SERVICE] ❌ Error updating column choices:", e$message, "\n")
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
    cat("DEBUG: [UI_SERVICE] Updating form fields from metadata\n")

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
            cat("DEBUG: [UI_SERVICE] Updated", field, "to:", metadata[[field]], "\n")
          }
        }
        cat("DEBUG: [UI_SERVICE] ✅ Form fields updated successfully\n")
      }, error = function(e) {
        cat("DEBUG: [UI_SERVICE] ❌ Error updating form fields:", e$message, "\n")
      })
    })
  }

  #' Reset Form Fields
  #'
  #' Unified function for resetting form fields to default values.
  #' Used for "Start ny session" and similar reset operations.
  #'
  reset_form_fields <- function() {
    cat("DEBUG: [UI_SERVICE] Resetting form fields to defaults\n")

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

        cat("DEBUG: [UI_SERVICE] ✅ Form fields reset successfully\n")
      }, error = function(e) {
        cat("DEBUG: [UI_SERVICE] ❌ Error resetting form fields:", e$message, "\n")
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
        cat("DEBUG: [UI_SERVICE] Showed element:", element_id, "\n")
      } else {
        shinyjs::hide(element_id)
        cat("DEBUG: [UI_SERVICE] Hid element:", element_id, "\n")
      }
    }, error = function(e) {
      cat("DEBUG: [UI_SERVICE] ❌ Error toggling element", element_id, ":", e$message, "\n")
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
    cat("DEBUG: [UI_SERVICE] Validating form fields\n")

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

      cat("DEBUG: [UI_SERVICE] Form validation completed, valid:", validation_results$valid, "\n")

    }, error = function(e) {
      cat("DEBUG: [UI_SERVICE] ❌ Error during form validation:", e$message, "\n")
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
    cat("DEBUG: [UI_SERVICE] Showing user feedback:", type, "-", message, "\n")

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

      cat("DEBUG: [UI_SERVICE] ✅ User feedback shown successfully\n")

    }, error = function(e) {
      cat("DEBUG: [UI_SERVICE] ❌ Error showing user feedback:", e$message, "\n")
    })
  }

  #' Update UI State Conditionally
  #'
  #' Enhanced function for updating UI state based on conditions.
  #'
  #' @param conditions Named list of conditions and corresponding UI updates
  #'
  update_ui_conditionally <- function(conditions) {
    cat("DEBUG: [UI_SERVICE] Updating UI conditionally based on", length(conditions), "conditions\n")

    tryCatch({
      for (condition_name in names(conditions)) {
        condition_spec <- conditions[[condition_name]]

        # Evaluate condition
        condition_met <- if (is.function(condition_spec$condition)) {
          condition_spec$condition()
        } else {
          condition_spec$condition
        }

        cat("DEBUG: [UI_SERVICE] Condition", condition_name, ":", condition_met, "\n")

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

      cat("DEBUG: [UI_SERVICE] ✅ Conditional UI updates completed\n")

    }, error = function(e) {
      cat("DEBUG: [UI_SERVICE] ❌ Error in conditional UI updates:", e$message, "\n")
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

  cat("DEBUG: [UI_SERVICE] UI update events added to app_state\n")
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
      cat("DEBUG: [EVENT] ui_update_needed emitted:", app_state$events$ui_update_needed, "\n")
    })
  }

  emit$column_choices_changed <- function() {
    isolate({
      app_state$events$column_choices_changed <- app_state$events$column_choices_changed + 1L
      cat("DEBUG: [EVENT] column_choices_changed emitted:", app_state$events$column_choices_changed, "\n")
    })
  }

  emit$form_reset_needed <- function() {
    isolate({
      app_state$events$form_reset_needed <- app_state$events$form_reset_needed + 1L
      cat("DEBUG: [EVENT] form_reset_needed emitted:", app_state$events$form_reset_needed, "\n")
    })
  }

  emit$form_restore_needed <- function() {
    isolate({
      app_state$events$form_restore_needed <- app_state$events$form_restore_needed + 1L
      cat("DEBUG: [EVENT] form_restore_needed emitted:", app_state$events$form_restore_needed, "\n")
    })
  }

  cat("DEBUG: [UI_SERVICE] UI update emit functions added\n")
  return(emit)
}