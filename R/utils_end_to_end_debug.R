# utils_end_to_end_debug.R
# Enhanced debug utilities for comprehensive end-to-end testing
# Phase 8: Detailed debugging infrastructure for complete system visibility

# END-TO-END DEBUG INFRASTRUCTURE ==========================================

## Enhanced session tracking with user interaction logging
debug_user_interaction <- function(action, details = NULL, session_id = NULL) {
  timestamp <- format(Sys.time(), "%H:%M:%S.%f")

  cat("DEBUG: [USER_INTERACTION] =====================================\n")
  cat("DEBUG: [USER_INTERACTION] Time:", timestamp, "\n")
  cat("DEBUG: [USER_INTERACTION] Action:", action, "\n")

  if (!is.null(details)) {
    if (is.list(details)) {
      for (key in names(details)) {
        cat("DEBUG: [USER_INTERACTION] ", key, ":", details[[key]], "\n")
      }
    } else {
      cat("DEBUG: [USER_INTERACTION] Details:", details, "\n")
    }
  }

  cat("DEBUG: [USER_INTERACTION] =====================================\n")

  # Also log to structured debug system
  debug_log(paste("User interaction:", action), "USER_INTERACTION", level = "INFO",
            context = details, session_id = session_id)
}

## Reactive chain step-by-step tracer
debug_reactive_execution <- function(reactive_name, step, input_values = NULL, output_value = NULL, session_id = NULL) {
  timestamp <- format(Sys.time(), "%H:%M:%S.%f")

  cat("DEBUG: [REACTIVE_TRACE] ===================================\n")
  cat("DEBUG: [REACTIVE_TRACE] Time:", timestamp, "\n")
  cat("DEBUG: [REACTIVE_TRACE] Reactive:", reactive_name, "\n")
  cat("DEBUG: [REACTIVE_TRACE] Step:", step, "\n")

  if (!is.null(input_values)) {
    cat("DEBUG: [REACTIVE_TRACE] Input values:\n")
    if (is.list(input_values)) {
      for (key in names(input_values)) {
        value_preview <- if (is.data.frame(input_values[[key]])) {
          paste("data.frame(", nrow(input_values[[key]]), "x", ncol(input_values[[key]]), ")")
        } else if (length(input_values[[key]]) > 1) {
          paste(class(input_values[[key]])[1], "length:", length(input_values[[key]]))
        } else {
          input_values[[key]]
        }
        cat("DEBUG: [REACTIVE_TRACE]   ", key, ":", value_preview, "\n")
      }
    }
  }

  if (!is.null(output_value)) {
    output_preview <- if (is.data.frame(output_value)) {
      paste("data.frame(", nrow(output_value), "x", ncol(output_value), ")")
    } else if (length(output_value) > 1) {
      paste(class(output_value)[1], "length:", length(output_value))
    } else {
      output_value
    }
    cat("DEBUG: [REACTIVE_TRACE] Output:", output_preview, "\n")
  }

  cat("DEBUG: [REACTIVE_TRACE] ===================================\n")

  # Structured logging
  debug_log(paste("Reactive execution:", reactive_name, "step:", step), "REACTIVE_TRACE", level = "TRACE",
            context = list(
              reactive_name = reactive_name,
              step = step,
              input_summary = if (!is.null(input_values)) names(input_values) else NULL,
              output_type = if (!is.null(output_value)) class(output_value)[1] else NULL
            ),
            session_id = session_id)
}

## State change detailed tracker
debug_state_change <- function(component, state_path, old_value, new_value, trigger = NULL, session_id = NULL) {
  timestamp <- format(Sys.time(), "%H:%M:%S.%f")

  cat("DEBUG: [STATE_CHANGE] =====================================\n")
  cat("DEBUG: [STATE_CHANGE] Time:", timestamp, "\n")
  cat("DEBUG: [STATE_CHANGE] Component:", component, "\n")
  cat("DEBUG: [STATE_CHANGE] State path:", state_path, "\n")

  old_preview <- if (is.data.frame(old_value)) {
    paste("data.frame(", nrow(old_value), "x", ncol(old_value), ")")
  } else if (length(old_value) > 1) {
    paste(class(old_value)[1], "length:", length(old_value))
  } else {
    old_value
  }

  new_preview <- if (is.data.frame(new_value)) {
    paste("data.frame(", nrow(new_value), "x", ncol(new_value), ")")
  } else if (length(new_value) > 1) {
    paste(class(new_value)[1], "length:", length(new_value))
  } else {
    new_value
  }

  cat("DEBUG: [STATE_CHANGE] Old value:", old_preview, "\n")
  cat("DEBUG: [STATE_CHANGE] New value:", new_preview, "\n")

  if (!is.null(trigger)) {
    cat("DEBUG: [STATE_CHANGE] Trigger:", trigger, "\n")
  }

  cat("DEBUG: [STATE_CHANGE] =====================================\n")

  # Structured logging
  debug_log(paste("State change in", component, "at", state_path), "STATE_CHANGE", level = "TRACE",
            context = list(
              component = component,
              state_path = state_path,
              old_type = class(old_value)[1],
              new_type = class(new_value)[1],
              trigger = trigger
            ),
            session_id = session_id)
}

## Workflow step tracer with detailed context
debug_workflow_step <- function(workflow_name, step_name, step_data = NULL, session_id = NULL) {
  timestamp <- format(Sys.time(), "%H:%M:%S.%f")

  cat("DEBUG: [WORKFLOW_STEP] ===================================\n")
  cat("DEBUG: [WORKFLOW_STEP] Time:", timestamp, "\n")
  cat("DEBUG: [WORKFLOW_STEP] Workflow:", workflow_name, "\n")
  cat("DEBUG: [WORKFLOW_STEP] Step:", step_name, "\n")

  if (!is.null(step_data)) {
    cat("DEBUG: [WORKFLOW_STEP] Step data:\n")
    if (is.list(step_data)) {
      for (key in names(step_data)) {
        value_preview <- if (is.data.frame(step_data[[key]])) {
          paste("data.frame(", nrow(step_data[[key]]), "x", ncol(step_data[[key]]), ")")
        } else if (length(step_data[[key]]) > 1) {
          paste(class(step_data[[key]])[1], "length:", length(step_data[[key]]))
        } else {
          step_data[[key]]
        }
        cat("DEBUG: [WORKFLOW_STEP]   ", key, ":", value_preview, "\n")
      }
    } else {
      cat("DEBUG: [WORKFLOW_STEP]   ", step_data, "\n")
    }
  }

  cat("DEBUG: [WORKFLOW_STEP] ===================================\n")

  # Structured logging
  debug_log(paste("Workflow step:", workflow_name, "->", step_name), "WORKFLOW_STEP", level = "INFO",
            context = step_data, session_id = session_id)
}

## Error boundary with comprehensive context
debug_error_boundary <- function(operation, error, context = NULL, session_id = NULL) {
  timestamp <- format(Sys.time(), "%H:%M:%S.%f")

  cat("DEBUG: [ERROR_BOUNDARY] ===================================\n")
  cat("DEBUG: [ERROR_BOUNDARY] ⚠️  ERROR DETECTED ⚠️\n")
  cat("DEBUG: [ERROR_BOUNDARY] Time:", timestamp, "\n")
  cat("DEBUG: [ERROR_BOUNDARY] Operation:", operation, "\n")
  cat("DEBUG: [ERROR_BOUNDARY] Error message:", error$message, "\n")
  cat("DEBUG: [ERROR_BOUNDARY] Error class:", class(error), "\n")

  if (!is.null(context)) {
    cat("DEBUG: [ERROR_BOUNDARY] Context:\n")
    if (is.list(context)) {
      for (key in names(context)) {
        cat("DEBUG: [ERROR_BOUNDARY]   ", key, ":", context[[key]], "\n")
      }
    } else {
      cat("DEBUG: [ERROR_BOUNDARY]   ", context, "\n")
    }
  }

  cat("DEBUG: [ERROR_BOUNDARY] Call stack:\n")
  call_stack <- sys.calls()
  for (i in length(call_stack):max(1, length(call_stack)-5)) {
    cat("DEBUG: [ERROR_BOUNDARY]   ", deparse(call_stack[[i]])[1], "\n")
  }

  cat("DEBUG: [ERROR_BOUNDARY] ===================================\n")

  # Structured logging
  debug_log(paste("Error in operation:", operation), "ERROR_BOUNDARY", level = "ERROR",
            context = list(
              operation = operation,
              error_message = error$message,
              error_class = class(error),
              context = context
            ),
            session_id = session_id)
}

## Memory and performance tracker
debug_performance_checkpoint <- function(checkpoint_name, additional_data = NULL, session_id = NULL) {
  timestamp <- format(Sys.time(), "%H:%M:%S.%f")

  # Get memory usage
  gc_info <- gc(verbose = FALSE)
  memory_used <- sum(gc_info[, "used"] * c(8, 8)) # Rough MB estimate

  cat("DEBUG: [PERFORMANCE] =====================================\n")
  cat("DEBUG: [PERFORMANCE] Time:", timestamp, "\n")
  cat("DEBUG: [PERFORMANCE] Checkpoint:", checkpoint_name, "\n")
  cat("DEBUG: [PERFORMANCE] Memory used (approx):", round(memory_used, 2), "MB\n")

  if (!is.null(additional_data)) {
    cat("DEBUG: [PERFORMANCE] Additional data:\n")
    if (is.list(additional_data)) {
      for (key in names(additional_data)) {
        cat("DEBUG: [PERFORMANCE]   ", key, ":", additional_data[[key]], "\n")
      }
    } else {
      cat("DEBUG: [PERFORMANCE]   ", additional_data, "\n")
    }
  }

  cat("DEBUG: [PERFORMANCE] =====================================\n")

  # Structured logging
  debug_log(paste("Performance checkpoint:", checkpoint_name), "PERFORMANCE", level = "INFO",
            context = list(
              checkpoint = checkpoint_name,
              memory_mb = round(memory_used, 2),
              additional_data = additional_data
            ),
            session_id = session_id)
}

# COMPREHENSIVE TEST SCENARIOS ==============================================

## End-to-end test scenario runner
run_e2e_test_scenario <- function(scenario_name, test_steps, session_id = NULL) {
  cat("DEBUG: [E2E_SCENARIO] =====================================\n")
  cat("DEBUG: [E2E_SCENARIO] 🧪 STARTING SCENARIO:", scenario_name, "\n")
  cat("DEBUG: [E2E_SCENARIO] =====================================\n")

  start_time <- Sys.time()
  debug_performance_checkpoint(paste("scenario_start", scenario_name), session_id = session_id)

  scenario_result <- list(
    scenario = scenario_name,
    start_time = start_time,
    steps = list(),
    success = TRUE,
    errors = list()
  )

  tryCatch({
    for (i in seq_along(test_steps)) {
      step <- test_steps[[i]]
      step_name <- names(test_steps)[i]

      cat("DEBUG: [E2E_SCENARIO] Step", i, ":", step_name, "\n")

      step_start <- Sys.time()
      step_result <- tryCatch({
        step()
        list(success = TRUE, error = NULL)
      }, error = function(e) {
        debug_error_boundary(paste("E2E step:", step_name), e, session_id = session_id)
        list(success = FALSE, error = e$message)
      })

      step_duration <- as.numeric(Sys.time() - step_start, units = "secs")

      scenario_result$steps[[step_name]] <- list(
        success = step_result$success,
        duration = step_duration,
        error = step_result$error
      )

      if (!step_result$success) {
        scenario_result$success <- FALSE
        scenario_result$errors[[step_name]] <- step_result$error
        cat("DEBUG: [E2E_SCENARIO] ❌ Step failed:", step_name, "\n")
        break
      } else {
        cat("DEBUG: [E2E_SCENARIO] ✅ Step completed:", step_name, "(", round(step_duration, 3), "s )\n")
      }
    }
  }, error = function(e) {
    debug_error_boundary(paste("E2E scenario:", scenario_name), e, session_id = session_id)
    scenario_result$success <- FALSE
    scenario_result$errors[["scenario_level"]] <- e$message
  })

  total_duration <- as.numeric(Sys.time() - start_time, units = "secs")
  scenario_result$total_duration <- total_duration

  debug_performance_checkpoint(paste("scenario_end", scenario_name),
                                list(total_duration = total_duration), session_id = session_id)

  cat("DEBUG: [E2E_SCENARIO] =====================================\n")
  if (scenario_result$success) {
    cat("DEBUG: [E2E_SCENARIO] ✅ SCENARIO COMPLETED:", scenario_name, "\n")
  } else {
    cat("DEBUG: [E2E_SCENARIO] ❌ SCENARIO FAILED:", scenario_name, "\n")
  }
  cat("DEBUG: [E2E_SCENARIO] Total duration:", round(total_duration, 3), "seconds\n")
  cat("DEBUG: [E2E_SCENARIO] =====================================\n")

  return(scenario_result)
}