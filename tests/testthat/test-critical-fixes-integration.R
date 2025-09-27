# test-critical-fixes-integration.R
# Integration tests for kritiske fixes
# Fokus på event system integration, state management og cross-component functionality

# Setup ----------------------------------------------------------------
source(file.path("..", "..", "global.R"))

test_that("Event system priorities fungerer i integration", {
  # Test complete event chain med korrekte priorities

  skip_if_not(exists("app_state"), message = "app_state not available in test environment")

  # Mock basic app_state structure hvis det ikke eksisterer
  if (!exists("app_state") || is.null(app_state)) {
    app_state <- list(
      events = reactiveValues(
        data_updated = 0,
        auto_detection_started = 0,
        ui_sync_requested = 0,
        error_occurred = 0
      ),
      data = reactiveValues(
        current_data = NULL,
        updating_table = FALSE
      ),
      columns = reactiveValues(
        auto_detect = reactiveValues(in_progress = FALSE)
      )
    )
  }

  # Track execution order for integration verification
  execution_log <- character(0)

  # Mock critical event handlers med korrekte priorities
  observeEvent(app_state$events$data_updated,
               priority = OBSERVER_PRIORITIES$STATE_MANAGEMENT, {
    execution_log <<- c(execution_log, "STATE_MANAGEMENT_DATA_UPDATE")
    log_debug("State management handling data update",
              .context = "INTEGRATION_TEST")
  })

  observeEvent(app_state$events$auto_detection_started,
               priority = OBSERVER_PRIORITIES$AUTO_DETECT, {
    execution_log <<- c(execution_log, "AUTO_DETECT_HANDLER")
    log_debug("Auto detection handler triggered",
              .context = "INTEGRATION_TEST")
  })

  observeEvent(app_state$events$ui_sync_requested,
               priority = OBSERVER_PRIORITIES$UI_SYNC, {
    execution_log <<- c(execution_log, "UI_SYNC_HANDLER")
    log_debug("UI sync handler triggered",
              .context = "INTEGRATION_TEST")
  })

  # Trigger event chain
  isolate({
    app_state$events$data_updated <- app_state$events$data_updated + 1
    app_state$events$auto_detection_started <- app_state$events$auto_detection_started + 1
    app_state$events$ui_sync_requested <- app_state$events$ui_sync_requested + 1

    # Allow observers to execute
    Sys.sleep(0.1)

    # Verify execution order follows priorities
    expect_true(length(execution_log) >= 3,
                info = "All event handlers should execute")

    # Find positions in execution log
    state_pos <- which(execution_log == "STATE_MANAGEMENT_DATA_UPDATE")[1]
    autodetect_pos <- which(execution_log == "AUTO_DETECT_HANDLER")[1]
    ui_pos <- which(execution_log == "UI_SYNC_HANDLER")[1]

    # Verify STATE_MANAGEMENT executes before AUTO_DETECT
    if (!is.na(state_pos) && !is.na(autodetect_pos)) {
      expect_lt(state_pos, autodetect_pos,
                info = "STATE_MANAGEMENT should execute before AUTO_DETECT")
    }

    # Verify AUTO_DETECT executes before UI_SYNC
    if (!is.na(autodetect_pos) && !is.na(ui_pos)) {
      expect_lt(autodetect_pos, ui_pos,
                info = "AUTO_DETECT should execute before UI_SYNC")
    }
  })
})

test_that("Error handling integration med logging og priorities", {
  # Test complete error handling chain

  skip_if_not(exists("safe_operation"), message = "safe_operation not available")

  # Mock error tracking
  error_events <- character(0)
  logged_errors <- character(0)

  # Test safe_operation med logging integration
  result <- safe_operation(
    operation_name = "Test integration operation",
    code = {
      # Simulate operation that might fail
      if (runif(1) > 0.5) {
        stop("Simulated integration error")
      } else {
        return("Success")
      }
    },
    fallback = function(e) {
      error_events <<- c(error_events, "FALLBACK_EXECUTED")
      log_error(
        message = "Integration test error handled",
        component = "[INTEGRATION_ERROR]",
        details = list(
          error_type = "simulated",
          context = "test_integration",
          recovery_action = "fallback_executed"
        )
      )
      return("FALLBACK_RESULT")
    }
  )

  # Verify either success or proper error handling
  expect_true(result %in% c("Success", "FALLBACK_RESULT"),
              info = "safe_operation should return either success or fallback result")

  # If error occurred, verify proper logging
  if (result == "FALLBACK_RESULT") {
    expect_true("FALLBACK_EXECUTED" %in% error_events,
                info = "Fallback should be executed on error")
  }
})

test_that("Input sanitization integration med UI components", {
  # Test input sanitization i realistic UI scenarios

  # Mock realistic SPC column names som brugere indtaster
  realistic_inputs <- list(
    column_mappings = list(
      x_column = "Dato for måling",
      y_column = "Antal utilsigtede hændelser / måned",
      n_column = "Total antal procedurer (nævner)",
      cl_column = "Centerline værdi (%)",
      skift_column = "Intervention dato & beskrivelse",
      kommentar_column = "Læge kommentarer / noter"
    ),
    file_names = c(
      "SPC_data_2024_marts.csv",
      "Kvalitetsdata (før & efter).xlsx",
      "Månedlige målinger - 2024.csv"
    ),
    search_terms = c(
      "hændelse før intervention",
      "måling > 95% konfidensinterval",
      "data fra afd. 12A & 12B"
    )
  )

  # Test column mapping sanitization
  for (col_type in names(realistic_inputs$column_mappings)) {
    original_value <- realistic_inputs$column_mappings[[col_type]]
    sanitized <- sanitize_column_name(original_value)

    expect_true(nchar(sanitized) > 0,
                info = paste("Sanitized column name should not be empty:", col_type))

    # Should preserve meaningful Danish content
    expect_true(grepl("måling|antal|dato|værdi|kommentar", sanitized, ignore.case = TRUE),
                info = paste("Should preserve meaningful content:", col_type))

    # Should remove problematic characters but keep structure
    expect_false(grepl("[()&%]", sanitized),
                 info = paste("Should remove problematic characters:", col_type))
  }

  # Test file name validation integration
  for (file_name in realistic_inputs$file_names) {
    # Extract extension
    ext <- tools::file_ext(file_name)
    is_valid <- validate_file_extension(ext)

    expect_true(is_valid,
                info = paste("Realistic file extension should be valid:", file_name))

    # Test file name sanitization
    sanitized_name <- sanitize_user_input(file_name, max_length = 100)
    expect_true(nchar(sanitized_name) > 0,
                info = paste("Sanitized file name should not be empty:", file_name))
  }

  # Test search term sanitization
  for (search_term in realistic_inputs$search_terms) {
    sanitized <- sanitize_user_input(search_term, max_length = 200)

    expect_true(nchar(sanitized) > 0,
                info = paste("Sanitized search term should not be empty:", search_term))

    # Should preserve Danish content
    expect_true(grepl("hændelse|måling|data|afd", sanitized, ignore.case = TRUE),
                info = paste("Should preserve Danish search content:", search_term))
  }
})

test_that("Cross-component reactive chain med priorities", {
  # Test komplet reactive chain på tværs af komponenter

  skip_if_not(exists("reactiveVal"), message = "Shiny reactive functions not available")

  # Setup test data chain
  test_data <- data.frame(
    Dato = as.Date("2024-01-01") + 0:9,
    Tæller = sample(1:10, 10),
    Nævner = sample(20:30, 10),
    stringsAsFactors = FALSE
  )

  # Mock data loading chain
  data_reactive <- reactiveVal(NULL)
  columns_detected <- reactiveVal(NULL)
  ui_updated <- reactiveVal(FALSE)

  # Chain 1: Data loading (HIGH priority)
  observeEvent(data_reactive(), priority = OBSERVER_PRIORITIES$STATE_MANAGEMENT, {
    if (!is.null(data_reactive())) {
      log_info(
        message = "Data loaded in reactive chain",
        component = "[CHAIN_TEST]",
        details = list(
          rows = nrow(data_reactive()),
          cols = ncol(data_reactive())
        )
      )

      # Trigger auto-detection
      columns_detected(names(data_reactive()))
    }
  })

  # Chain 2: Column detection (MEDIUM priority)
  observeEvent(columns_detected(), priority = OBSERVER_PRIORITIES$AUTO_DETECT, {
    if (!is.null(columns_detected())) {
      log_info(
        message = "Columns detected in reactive chain",
        component = "[CHAIN_TEST]",
        details = list(
          detected_columns = columns_detected()
        )
      )

      # Trigger UI update
      ui_updated(TRUE)
    }
  })

  # Chain 3: UI update (LOW priority)
  observeEvent(ui_updated(), priority = OBSERVER_PRIORITIES$UI_SYNC, {
    if (isTRUE(ui_updated())) {
      log_info(
        message = "UI updated in reactive chain",
        component = "[CHAIN_TEST]",
        details = list(
          update_completed = TRUE
        )
      )
    }
  })

  # Execute complete chain
  isolate({
    data_reactive(test_data)

    # Allow chain to complete
    Sys.sleep(0.2)

    # Verify chain completion
    expect_equal(columns_detected(), names(test_data),
                 info = "Column detection should complete")
    expect_true(ui_updated(),
               info = "UI update should complete")
  })
})

test_that("OBSERVER_PRIORITIES helper functions integration", {
  # Test helper functions i realistic usage scenarios

  # Test get_priority function i event registration
  expect_no_error({
    test_priority <- get_priority("STATE_MANAGEMENT")
    expect_equal(test_priority, 2000)
  })

  expect_error(get_priority("INVALID_PRIORITY"),
               info = "Invalid priority should throw error")

  # Test convenience functions
  expect_equal(priority_high(), OBSERVER_PRIORITIES$STATE_MANAGEMENT)
  expect_equal(priority_medium(), OBSERVER_PRIORITIES$DATA_PROCESSING)
  expect_equal(priority_low(), OBSERVER_PRIORITIES$UI_SYNC)
  expect_equal(priority_cleanup(), OBSERVER_PRIORITIES$CLEANUP)

  # Test usage i realistic observer registration
  if (exists("reactiveVal")) {
    test_val <- reactiveVal(0)

    expect_no_error({
      observeEvent(test_val(), priority = priority_high(), {
        # High priority handler
      })

      observeEvent(test_val(), priority = get_priority("AUTO_DETECT"), {
        # Auto-detect handler
      })

      observeEvent(test_val(), priority = priority_cleanup(), {
        # Cleanup handler
      })
    }, info = "Helper functions should work in observer registration")
  }
})

test_that("Memory management under sustained load", {
  # Test memory efficiency i complete integration scenarios

  # Initial memory snapshot
  gc_initial <- gc()
  initial_memory <- sum(gc_initial[,2])

  # Simulate sustained app usage
  for (cycle in 1:50) {
    # Mock data loading cycle
    mock_data <- data.frame(
      x = runif(100),
      y = runif(100),
      z = sample(letters, 100, replace = TRUE),
      stringsAsFactors = FALSE
    )

    # Mock sanitization
    col_names <- names(mock_data)
    sanitized_names <- sapply(col_names, sanitize_column_name)

    # Mock logging
    log_debug(
      message = "Processing cycle",
      .context = "MEMORY_TEST",
      details = list(
        cycle = cycle,
        data_size = object.size(mock_data),
        timestamp = Sys.time()
      )
    )

    # Cleanup every 10 cycles
    if (cycle %% 10 == 0) {
      gc()
    }
  }

  # Final memory snapshot
  gc_final <- gc()
  final_memory <- sum(gc_final[,2])
  memory_growth <- final_memory - initial_memory

  # Memory growth should be reasonable (< 20MB for 50 cycles)
  expect_lt(memory_growth, 20,
            info = paste("Memory growth should be bounded. Growth:", memory_growth, "MB"))

  # No major memory leaks should be detected
  expect_lt(memory_growth / 50, 0.5,
            info = "Per-cycle memory growth should be minimal (< 0.5MB/cycle)")
})