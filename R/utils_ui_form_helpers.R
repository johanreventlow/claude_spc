# utils_ui_form_helpers.R
# Ekstraherede UI form helper utilities fra utils_ui_ui_updates.R
# (file renamed 2024-09 to golem convention: ui_utils_* → utils_ui_*)
# Forbedrer modularity og separation of concerns

#' Form Field Update Manager
#'
#' Factory for creating form field update functions with standardized patterns.
#' Ekstraeret fra utils_ui_ui_updates.R for bedre testability og genbrug.
#'
#' @param session Shiny session object
#' @return List med form update funktioner
#' @export
create_form_update_service <- function(session) {
  # Update individual form field based on type
  # field_id: Field identifier
  # value: New value
  # field_type: "text", "select", "textarea"
  update_field_by_type <- function(field_id, value, field_type = "text") {
    safe_operation(
      paste("Update form field:", field_id),
      code = {
        switch(field_type,
          "text" = shiny::updateTextInput(session, field_id, value = value),
          "select" = shiny::updateSelectizeInput(session, field_id, selected = value),
          "textarea" = {
            if (exists("updateTextAreaInput", mode = "function")) {
              updateTextAreaInput(session, field_id, value = value)
            } else {
              shiny::updateTextAreaInput(session, field_id, value = value)
            }
          },
          {
            log_warn(paste("Unknown field type:", field_type), "FORM_SERVICE")
            shiny::updateTextInput(session, field_id, value = value)
          }
        )
      },
      fallback = NULL,
      error_type = "form_update"
    )
  }

  # Field type mapping for form fields
  .field_types <- list(
    indicator_title = "text",
    unit_custom = "text",
    target_value = "text",
    centerline_value = "text",
    indicator_description = "textarea",
    unit_select = "select",
    chart_type = "select",
    x_column = "select",
    y_column = "select",
    n_column = "select",
    y_axis_unit = "select"
  )

  # Default values for form reset
  .default_values <- list(
    indicator_title = "",
    unit_custom = "",
    target_value = "",
    centerline_value = "",
    indicator_description = "",
    unit_select = "",
    chart_type = "run",
    x_column = "",
    y_column = "",
    n_column = "",
    y_axis_unit = "count"
  )

  list(
    # Update multiple form fields from metadata
    update_from_metadata = function(metadata, fields = NULL) {
      if (is.null(fields)) {
        fields <- names(.field_types)
      }

      shiny::isolate({
        safe_operation(
          "Batch form field update from metadata",
          code = {
            # Update fields using tidyverse approach
            fields |>
              purrr::keep(~ !is.null(metadata[[.x]]) && .x %in% names(.field_types)) |>
              purrr::walk(~ update_field_by_type(.x, metadata[[.x]], .field_types[[.x]]))
          },
          fallback = NULL,
          error_type = "batch_form_update"
        )
      })
    },

    # Reset all form fields to defaults
    reset_to_defaults = function() {
      shiny::isolate({
        safe_operation(
          "Reset form fields to defaults",
          code = {
            # Reset fields using tidyverse approach
            names(.default_values) |>
              purrr::keep(~ .x %in% names(.field_types)) |>
              purrr::walk(~ update_field_by_type(.x, .default_values[[.x]], .field_types[[.x]]))
          },
          fallback = NULL,
          error_type = "form_reset"
        )
      })
    },

    # Update single field
    update_field = function(field_id, value) {
      field_type <- .field_types[[field_id]] %||% "text"
      update_field_by_type(field_id, value, field_type)
    },

    # Get field type for a given field ID
    get_field_type = function(field_id) {
      .field_types[[field_id]] %||% "text"
    },

    # Get default value for a field
    get_default_value = function(field_id) {
      .default_values[[field_id]] %||% ""
    }
  )
}

#' UI Element Visibility Manager
#'
#' Utility for managing UI element visibility with consistent patterns.
#' Ekstraeret fra utils_ui_ui_updates.R for bedre separation of concerns.
#'
#' @param session Shiny session object
#' @return List med visibility control funktioner
#' @export
create_ui_visibility_service <- function(session) {
  list(
    # Toggle single UI element visibility
    toggle_element = function(element_id, show = TRUE) {
      safe_operation(
        paste("Toggle UI element:", element_id),
        code = {
          if (show) {
            if (exists("shinyjs") && "shinyjs" %in% loadedNamespaces()) {
              shinyjs::show(element_id)
            }
          } else {
            if (exists("shinyjs") && "shinyjs" %in% loadedNamespaces()) {
              shinyjs::hide(element_id)
            }
          }
        },
        fallback = NULL,
        error_type = "ui_visibility"
      )
    },

    # Show multiple UI elements
    show_elements = function(element_ids) {
      for (element_id in element_ids) {
        toggle_element(element_id, show = TRUE)
      }
    },

    # Hide multiple UI elements
    hide_elements = function(element_ids) {
      for (element_id in element_ids) {
        toggle_element(element_id, show = FALSE)
      }
    },

    # Toggle multiple elements based on conditions
    toggle_conditional = function(element_conditions) {
      safe_operation(
        "Conditional UI element toggle",
        code = {
          for (element_id in names(element_conditions)) {
            show_condition <- element_conditions[[element_id]]
            toggle_element(element_id, show_condition)
          }
        },
        fallback = NULL,
        error_type = "conditional_toggle"
      )
    }
  )
}

#' Column Choice Update Manager
#'
#' Specialized service for updating column choice inputs with data-driven choices.
#' Ekstraeret fra utils_ui_ui_updates.R for better separation og testability.
#'
#' @param session Shiny session object
#' @param app_state Application state for data access
#' @return List med column choice update funktioner
#' @export
create_column_choice_service <- function(session, app_state) {
  # Standard column input IDs
  .standard_columns <- c("x_column", "y_column", "n_column", "skift_column", "frys_column", "kommentar_column")

  list(
    # Generate choices from current data
    generate_choices_from_data = function() {
      safe_operation(
        "Generate column choices from current data",
        code = {
          current_data <- app_state$data$current_data
          if (!is.null(current_data) && ncol(current_data) > 0) {
            all_cols <- names(current_data)
            return(setNames(
              c("", all_cols),
              c("Vælg kolonne...", all_cols)
            ))
          } else {
            return(setNames("", "Vælg kolonne..."))
          }
        },
        fallback = setNames("", "Vælg kolonne..."),
        error_type = "choice_generation"
      )
    },

    # Update column choices for specific columns
    update_column_choices = function(choices = NULL, selected = NULL, columns = .standard_columns, clear_selections = FALSE) {
      safe_operation(
        "Update column choices",
        code = {
          # Generate choices if not provided
          if (is.null(choices)) {
            choices <- generate_choices_from_data()
          }

          # Prepare selections
          if (clear_selections) {
            selected <- list()
          }

          # Update each column input
          for (column_id in columns) {
            selected_value <- if (clear_selections) "" else (selected[[column_id]] %||% "")

            shiny::updateSelectizeInput(
              session,
              column_id,
              choices = choices,
              selected = selected_value
            )
          }
        },
        fallback = NULL,
        error_type = "column_choice_update"
      )
    },

    # Update single column choice
    update_single_column = function(column_id, selected, choices = NULL) {
      if (is.null(choices)) {
        choices <- generate_choices_from_data()
      }

      safe_operation(
        paste("Update single column choice:", column_id),
        code = {
          shiny::updateSelectizeInput(
            session,
            column_id,
            choices = choices,
            selected = selected %||% ""
          )
        },
        fallback = NULL,
        error_type = "single_column_update"
      )
    },

    # Clear all column selections
    clear_all_selections = function() {
      update_column_choices(clear_selections = TRUE)
    },

    # Update from auto-detection results
    update_from_detection = function(detection_results) {
      if (is.null(detection_results)) {
        return()
      }

      safe_operation(
        "Update column choices from auto-detection",
        code = {
          choices <- generate_choices_from_data()
          selected <- list()

          # Map detection results to UI inputs
          if (!is.null(detection_results$x_column)) {
            selected$x_column <- detection_results$x_column
          }
          if (!is.null(detection_results$y_column)) {
            selected$y_column <- detection_results$y_column
          }
          if (!is.null(detection_results$n_column)) {
            selected$n_column <- detection_results$n_column
          }

          update_column_choices(choices = choices, selected = selected)
        },
        fallback = NULL,
        error_type = "detection_update"
      )
    }
  )
}

# Null coalescing operator is defined in utils_logging.R
