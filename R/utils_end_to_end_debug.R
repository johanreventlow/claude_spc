# utils_end_to_end_debug.R
# Enhanced debug utilities for comprehensive end-to-end testing

# END-TO-END DEBUG INFRASTRUCTURE ==========================================

## Enhanced session tracking with user interaction logging
debug_user_interaction <- function(action, details = NULL, session_id = NULL) {
  timestamp <- format(Sys.time(), "%H:%M:%S.%f")

  log_debug_block("USER_INTERACTION", paste("User interaction:", action))
  if (!is.null(details) && is.list(details)) {
    log_debug_kv(.list_data = details, .context = "USER_INTERACTION")
  }
  log_debug_block("USER_INTERACTION", "User interaction completed", type = "stop")

  # Also log to structured debug system
  debug_log(paste("User interaction:", action), "USER_INTERACTION", level = "INFO",
            context = details, session_id = session_id)
}

## Reactive chain step-by-step tracer
debug_reactive_execution <- function(reactive_name, step, input_values = NULL, output_value = NULL, session_id = NULL) {
  timestamp <- format(Sys.time(), "%H:%M:%S.%f")

  log_debug_block("REACTIVE_TRACE", paste("Reactive execution:", reactive_name, "step:", step))
  # Input/output validation available in structured debug_log below
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

  log_debug_block("STATE_CHANGE", paste("State change in", component, "at", state_path))
  # Value details available in structured debug_log below
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
  # Step data details available in structured debug_log below
  log_debug_block("WORKFLOW_STEP", "Workflow step completed", type = "stop")

  # Structured logging
  debug_log(paste("Workflow step:", workflow_name, "->", step_name), "WORKFLOW_STEP", level = "INFO",
            context = step_data, session_id = session_id)
}

## Error boundary with comprehensive context
debug_error_boundary <- function(operation, error, context = NULL, session_id = NULL) {
  timestamp <- format(Sys.time(), "%H:%M:%S.%f")

  log_debug_block("ERROR_BOUNDARY", paste("ERROR:", operation, "-", error$message))
  # Full error details available in structured debug_log below
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

  log_debug_block("PERFORMANCE", paste("Performance checkpoint:", checkpoint_name, "(", round(memory_used, 2), "MB)"))
  # Performance details available in structured debug_log below
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

        # Step execution tracked in structured logging

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
          break
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

  status <- if (scenario_result$success) "COMPLETED" else "FAILED"
  log_debug_block("E2E_SCENARIO", paste("Scenario", status, ":", scenario_name, "(", round(total_duration, 3), "s)"))
  # Detailed results available in returned scenario_result object

  return(scenario_result)
}