# utils_end_to_end_debug.R
# Enhanced debug utilities for comprehensive end-to-end testing

# END-TO-END DEBUG INFRASTRUCTURE ==========================================

## Enhanced session tracking with user interaction logging
debug_user_interaction <- function(action, details = NULL, session_id = NULL) {
  timestamp <- format(Sys.time(), "%H:%M:%S.%f")

  log_debug_block("USER_INTERACTION", paste("User interaction:", action))
  log_debug("Time:", timestamp, .context = "USER_INTERACTION")
  log_debug("Action:", action, .context = "USER_INTERACTION")

  if (!is.null(details)) {
    if (is.list(details)) {
      log_debug_kv(.list_data = details, .context = "USER_INTERACTION")
    } else {
      log_debug("Details:", details, .context = "USER_INTERACTION")
    }
  }

  log_debug_block("USER_INTERACTION", "User interaction completed", type = "stop")

  # Also log to structured debug system
  debug_log(paste("User interaction:", action), "USER_INTERACTION", level = "INFO",
            context = details, session_id = session_id)
}

## Reactive chain step-by-step tracer
debug_reactive_execution <- function(reactive_name, step, input_values = NULL, output_value = NULL, session_id = NULL) {
  timestamp <- format(Sys.time(), "%H:%M:%S.%f")

  log_debug_block("REACTIVE_TRACE", paste("Reactive execution:", reactive_name))
  log_debug("Time:", timestamp, .context = "REACTIVE_TRACE")
  log_debug("Reactive:", reactive_name, .context = "REACTIVE_TRACE")
  log_debug("Step:", step, .context = "REACTIVE_TRACE")

  if (!is.null(input_values)) {
    log_debug("Input values:", .context = "REACTIVE_TRACE")
    if (is.list(input_values)) {
      value_previews <- list()
      for (key in names(input_values)) {
        value_preview <- if (is.data.frame(input_values[[key]])) {
          paste("data.frame(", nrow(input_values[[key]]), "x", ncol(input_values[[key]]), ")")
        } else if (length(input_values[[key]]) > 1) {
          paste(class(input_values[[key]])[1], "length:", length(input_values[[key]]))
        } else {
          input_values[[key]]
        }
        value_previews[[key]] <- value_preview
      }
      log_debug_kv(.list_data = value_previews, .context = "REACTIVE_TRACE")
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
    log_debug("Output:", output_preview, .context = "REACTIVE_TRACE")
  }

  log_debug_block("REACTIVE_TRACE", "Reactive execution completed", type = "stop")

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

  log_debug_block("STATE_CHANGE", paste("State change in", component))
  log_debug_kv(
    time = timestamp,
    component = component,
    state_path = state_path,
    .context = "STATE_CHANGE"
  )

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

  log_debug_kv(
    old_value = old_preview,
    new_value = new_preview,
    .context = "STATE_CHANGE"
  )

  if (!is.null(trigger)) {
    log_debug("Trigger:", trigger, .context = "STATE_CHANGE")
  }

  log_debug_block("STATE_CHANGE", "State change completed", type = "stop")

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

  log_debug_block("WORKFLOW_STEP", paste("Workflow step:", workflow_name, "-", step_name))
  log_debug_kv(
    time = timestamp,
    workflow = workflow_name,
    step = step_name,
    .context = "WORKFLOW_STEP"
  )

  if (!is.null(step_data)) {
    log_debug("Step data:", .context = "WORKFLOW_STEP")
    if (is.list(step_data)) {
      for (key in names(step_data)) {
        value_preview <- if (is.data.frame(step_data[[key]])) {
          paste("data.frame(", nrow(step_data[[key]]), "x", ncol(step_data[[key]]), ")")
        } else if (length(step_data[[key]]) > 1) {
          paste(class(step_data[[key]])[1], "length:", length(step_data[[key]]))
        } else {
          step_data[[key]]
        }
        log_debug(key, ":", value_preview, .context = "WORKFLOW_STEP")
      }
    } else {
      log_debug(step_data, .context = "WORKFLOW_STEP")
    }
  }

  log_debug_block("WORKFLOW_STEP", "Workflow step completed", type = "stop")

  # Structured logging
  debug_log(paste("Workflow step:", workflow_name, "->", step_name), "WORKFLOW_STEP", level = "INFO",
            context = step_data, session_id = session_id)
}

## Error boundary with comprehensive context
debug_error_boundary <- function(operation, error, context = NULL, session_id = NULL) {
  timestamp <- format(Sys.time(), "%H:%M:%S.%f")

  log_debug_block("ERROR_BOUNDARY", "⚠️ ERROR DETECTED ⚠️")
  log_debug_kv(
    time = timestamp,
    operation = operation,
    error_message = error$message,
    error_class = paste(class(error), collapse = ", "),
    .context = "ERROR_BOUNDARY"
  )

  if (!is.null(context)) {
    log_debug("Context:", .context = "ERROR_BOUNDARY")
    if (is.list(context)) {
      for (key in names(context)) {
        log_debug(key, ":", context[[key]], .context = "ERROR_BOUNDARY")
      }
    } else {
      log_debug(context, .context = "ERROR_BOUNDARY")
    }
  }

  log_debug("Call stack:", .context = "ERROR_BOUNDARY")
  call_stack <- sys.calls()
  for (i in length(call_stack):max(1, length(call_stack)-5)) {
    log_debug(deparse(call_stack[[i]])[1], .context = "ERROR_BOUNDARY")
  }

  log_debug_block("ERROR_BOUNDARY", "Error boundary completed", type = "stop")

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

  log_debug_block("PERFORMANCE", paste("Performance checkpoint:", checkpoint_name))
  log_debug_kv(
    time = timestamp,
    checkpoint = checkpoint_name,
    memory_used_mb = round(memory_used, 2),
    .context = "PERFORMANCE"
  )

  if (!is.null(additional_data)) {
    log_debug("Additional data:", .context = "PERFORMANCE")
    if (is.list(additional_data)) {
      for (key in names(additional_data)) {
        log_debug(key, ":", additional_data[[key]], .context = "PERFORMANCE")
      }
    } else {
      log_debug(additional_data, .context = "PERFORMANCE")
    }
  }

  log_debug_block("PERFORMANCE", "Performance checkpoint completed", type = "stop")

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
  log_debug_block("E2E_SCENARIO", paste("🧪 STARTING SCENARIO:", scenario_name))

  start_time <- Sys.time()
  debug_performance_checkpoint(paste("scenario_start", scenario_name), session_id = session_id)

  scenario_result <- list(
    scenario = scenario_name,
    start_time = start_time,
    steps = list(),
    success = TRUE,
    errors = list()
  )

  safe_operation(
    paste("E2E scenario execution:", scenario_name),
    code = {
      for (i in seq_along(test_steps)) {
        step <- test_steps[[i]]
        step_name <- names(test_steps)[i]

        log_debug("Step", i, ":", step_name, .context = "E2E_SCENARIO")

        step_start <- Sys.time()
        step_result <- safe_operation(
          paste("E2E test step:", step_name),
          code = {
            step()
            list(success = TRUE, error = NULL)
          },
          fallback = function(e) {
            debug_error_boundary(paste("E2E step:", step_name), e, session_id = session_id)
            list(success = FALSE, error = e$message)
          },
          error_type = "processing"
        )

        step_duration <- as.numeric(Sys.time() - step_start, units = "secs")

        scenario_result$steps[[step_name]] <- list(
          success = step_result$success,
          duration = step_duration,
          error = step_result$error
        )

        if (!step_result$success) {
          scenario_result$success <- FALSE
          scenario_result$errors[[step_name]] <- step_result$error
          log_debug("❌ Step failed:", step_name, .context = "E2E_SCENARIO")
          break
        } else {
          log_debug("✅ Step completed:", step_name, "(", round(step_duration, 3), "s )", .context = "E2E_SCENARIO")
        }
      }
    },
    fallback = {
      debug_error_boundary(paste("E2E scenario:", scenario_name), e, session_id = session_id)
      scenario_result$success <- FALSE
      scenario_result$errors[["scenario_level"]] <- e$message
    },
    error_type = "processing"
  )

  total_duration <- as.numeric(Sys.time() - start_time, units = "secs")
  scenario_result$total_duration <- total_duration

  debug_performance_checkpoint(paste("scenario_end", scenario_name),
                                list(total_duration = total_duration), session_id = session_id)

  log_debug_block("E2E_SCENARIO", "Scenario summary", type = "stop")
  if (scenario_result$success) {
    log_debug("✅ SCENARIO COMPLETED:", scenario_name, .context = "E2E_SCENARIO")
  } else {
    log_debug("❌ SCENARIO FAILED:", scenario_name, .context = "E2E_SCENARIO")
  }
  log_debug("Total duration:", round(total_duration, 3), "seconds", .context = "E2E_SCENARIO")
  log_debug_block("E2E_SCENARIO", "Scenario summary", type = "stop")

  return(scenario_result)
}