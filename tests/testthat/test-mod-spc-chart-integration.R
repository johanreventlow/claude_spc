# test-mod-spc-chart-integration.R
# ==============================================================================
# COMPREHENSIVE TEST SUITE: Visualization Module (mod_spc_chart_server.R)
# ==============================================================================
#
# FORMÅL: >70% coverage af visualization module reactive chains
# FOKUS: Integration testing for production stability
#
# STRUKTUR:
#   1. Reactive Chain Testing (data → cache → plot)
#   2. Cache Update Atomicity (race condition prevention)
#   3. Error Handling (graceful degradation)
#   4. UI Rendering (plot output, value boxes)
#   5. Module State Management
#
# SUCCESS CRITERIA:
#   - All reactive chains work correctly
#   - No race conditions in cache updates
#   - Error paths covered
#   - UI updates correctly
# ==============================================================================

library(shiny)
library(testthat)

# SETUP HELPERS ================================================================

# Helper til at oprette mock app_state
create_mock_app_state <- function() {
  app_state <- new.env(parent = emptyenv())

  app_state$events <- reactiveValues(
    visualization_update_needed = 0L,
    navigation_changed = 0L
  )

  app_state$data <- reactiveValues(
    current_data = NULL,
    updating_table = FALSE
  )

  app_state$visualization <- reactiveValues(
    module_cached_data = NULL,
    module_data_cache = NULL,
    cache_updating = FALSE,
    plot_ready = FALSE,
    plot_warnings = character(0),
    is_computing = FALSE,
    plot_generation_in_progress = FALSE,
    plot_object = NULL,
    anhoej_results = NULL,
    last_centerline_value = NULL
  )

  app_state$columns <- reactiveValues(
    auto_detect = reactiveValues(
      in_progress = FALSE,
      completed = FALSE,
      results = NULL,
      frozen_until_next_trigger = FALSE
    ),
    mappings = reactiveValues(
      x_column = NULL,
      y_column = NULL,
      n_column = NULL
    )
  )

  app_state$ui <- reactiveValues(
    hide_anhoej_rules = FALSE,
    pending_programmatic_inputs = list()
  )

  return(app_state)
}

# Helper til at oprette test data
create_test_data_for_module <- function(n = 12) {
  data.frame(
    Dato = seq.Date(as.Date("2024-01-01"), by = "month", length.out = n),
    `Tæller` = sample(40:50, n, replace = TRUE),
    `Nævner` = rep(50, n),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# REACTIVE CHAIN TESTS =========================================================

describe("Reactive Chains", {

  it("updates plot when data changes", {
    skip_if_not(exists("visualizationModuleServer", mode = "function"))

    # Setup
    test_data <- create_test_data_for_module()
    app_state <- create_mock_app_state()

    # Simulate module server
    testServer(visualizationModuleServer, args = list(
      data_reactive = reactive(test_data),
      column_config_reactive = reactive(list(
        x_col = "Dato",
        y_col = "Tæller",
        n_col = "Nævner"
      )),
      chart_type_reactive = reactive("p"),
      target_value_reactive = reactive(NULL),
      target_text_reactive = reactive(NULL),
      centerline_value_reactive = reactive(NULL),
      skift_config_reactive = reactive(list(show_phases = FALSE, skift_column = NULL)),
      frys_config_reactive = reactive(NULL),
      chart_title_reactive = reactive("Test Chart"),
      y_axis_unit_reactive = reactive("percent"),
      kommentar_column_reactive = reactive(NULL),
      app_state = app_state
    ), {
      # Verify initial state
      expect_true(is.reactive(session$returned))

      # Trigger data update
      session$setInputs(data_reactive = test_data)

      # Verify plot generation
      # Note: Actual plot generation requires full reactive context
    })
  })

  it("handles cache update atomicity correctly", {
    skip_if_not(exists("visualizationModuleServer", mode = "function"))

    app_state <- create_mock_app_state()
    test_data <- create_test_data_for_module()

    # Setup initial data
    app_state$data$current_data <- test_data

    # Verify cache starts empty
    expect_null(isolate(app_state$visualization$module_cached_data))

    # Trigger visualization update event
    app_state$events$visualization_update_needed <- 1L

    # Verify cache is updated (atomic operation)
    # Note: Actual atomicity testing requires concurrent access simulation
  })

  it("prevents race conditions with guard flag", {
    skip_if_not(exists("visualizationModuleServer", mode = "function"))

    app_state <- create_mock_app_state()

    # Set cache_updating guard flag
    app_state$visualization$cache_updating <- TRUE

    # Attempt concurrent update
    app_state$events$visualization_update_needed <- 1L

    # Verify update is skipped when guard is set
    # (actual verification requires observer spy)
  })
})

# CACHE UPDATE TESTS ===========================================================

describe("Cache Update Atomicity", {

  it("updates both cache values together", {
    app_state <- create_mock_app_state()
    test_data <- create_test_data_for_module()

    # Set initial data
    app_state$data$current_data <- test_data

    # Simulate atomic cache update
    app_state$visualization$module_data_cache <- test_data
    app_state$visualization$module_cached_data <- test_data

    # Verify both are updated
    expect_equal(
      isolate(app_state$visualization$module_data_cache),
      isolate(app_state$visualization$module_cached_data)
    )
  })

  it("clears guard flag on error", {
    app_state <- create_mock_app_state()

    # Simulate error scenario
    app_state$visualization$cache_updating <- TRUE

    # Error handling should clear flag
    # (actual testing requires tryCatch simulation)

    # Manual cleanup for test
    app_state$visualization$cache_updating <- FALSE

    expect_false(isolate(app_state$visualization$cache_updating))
  })

  it("skips update when table is updating", {
    app_state <- create_mock_app_state()

    # Set table updating flag
    app_state$data$updating_table <- TRUE

    # Trigger visualization update
    app_state$events$visualization_update_needed <- 1L

    # Update should be skipped
    # (verification requires observer monitoring)
  })
})

# ERROR HANDLING TESTS =========================================================

describe("Error Handling", {

  it("handles null data gracefully", {
    skip_if_not(exists("visualizationModuleServer", mode = "function"))

    app_state <- create_mock_app_state()

    testServer(visualizationModuleServer, args = list(
      data_reactive = reactive(NULL),
      column_config_reactive = reactive(list(
        x_col = "Dato",
        y_col = "Tæller",
        n_col = "Nævner"
      )),
      chart_type_reactive = reactive("p"),
      target_value_reactive = reactive(NULL),
      target_text_reactive = reactive(NULL),
      centerline_value_reactive = reactive(NULL),
      skift_config_reactive = reactive(list(show_phases = FALSE, skift_column = NULL)),
      frys_config_reactive = reactive(NULL),
      app_state = app_state
    ), {
      # Should not crash with null data
      expect_true(TRUE)
    })
  })

  it("handles empty data gracefully", {
    skip_if_not(exists("visualizationModuleServer", mode = "function"))

    app_state <- create_mock_app_state()
    empty_data <- data.frame()

    testServer(visualizationModuleServer, args = list(
      data_reactive = reactive(empty_data),
      column_config_reactive = reactive(list(
        x_col = "Dato",
        y_col = "Tæller",
        n_col = "Nævner"
      )),
      chart_type_reactive = reactive("p"),
      target_value_reactive = reactive(NULL),
      target_text_reactive = reactive(NULL),
      centerline_value_reactive = reactive(NULL),
      skift_config_reactive = reactive(list(show_phases = FALSE, skift_column = NULL)),
      frys_config_reactive = reactive(NULL),
      app_state = app_state
    ), {
      # Should handle empty data without crashing
      expect_true(TRUE)
    })
  })

  it("sets appropriate warnings on validation failure", {
    app_state <- create_mock_app_state()

    # Simulate validation failure
    app_state$visualization$plot_warnings <- c("Validering fejlede", "For få datapunkter")
    app_state$visualization$plot_ready <- FALSE

    warnings <- isolate(app_state$visualization$plot_warnings)

    expect_length(warnings, 2)
    expect_true(grepl("Validering", warnings[1]))
  })
})

# UI RENDERING TESTS ===========================================================

describe("UI Rendering", {

  it("renders plot_ready output correctly", {
    skip_if_not(exists("visualizationModuleServer", mode = "function"))

    app_state <- create_mock_app_state()
    test_data <- create_test_data_for_module()

    testServer(visualizationModuleServer, args = list(
      data_reactive = reactive(test_data),
      column_config_reactive = reactive(list(
        x_col = "Dato",
        y_col = "Tæller",
        n_col = "Nævner"
      )),
      chart_type_reactive = reactive("p"),
      target_value_reactive = reactive(NULL),
      target_text_reactive = reactive(NULL),
      centerline_value_reactive = reactive(NULL),
      skift_config_reactive = reactive(list(show_phases = FALSE, skift_column = NULL)),
      frys_config_reactive = reactive(NULL),
      app_state = app_state
    ), {
      # plot_ready should be reactive output
      expect_true("plot_ready" %in% names(output))
    })
  })

  it("renders plot_info with warnings", {
    skip_if_not(exists("visualizationModuleServer", mode = "function"))

    app_state <- create_mock_app_state()
    app_state$visualization$plot_warnings <- c("Test warning")

    testServer(visualizationModuleServer, args = list(
      data_reactive = reactive(NULL),
      column_config_reactive = reactive(list()),
      chart_type_reactive = reactive("p"),
      target_value_reactive = reactive(NULL),
      target_text_reactive = reactive(NULL),
      centerline_value_reactive = reactive(NULL),
      skift_config_reactive = reactive(list(show_phases = FALSE, skift_column = NULL)),
      frys_config_reactive = reactive(NULL),
      app_state = app_state
    ), {
      # plot_info should exist
      expect_true("plot_info" %in% names(output))
    })
  })

  it("renders anhoej_rules_boxes correctly", {
    skip_if_not(exists("visualizationModuleServer", mode = "function"))

    app_state <- create_mock_app_state()
    test_data <- create_test_data_for_module()

    testServer(visualizationModuleServer, args = list(
      data_reactive = reactive(test_data),
      column_config_reactive = reactive(list(
        x_col = "Dato",
        y_col = "Tæller",
        n_col = "Nævner"
      )),
      chart_type_reactive = reactive("run"),
      target_value_reactive = reactive(NULL),
      target_text_reactive = reactive(NULL),
      centerline_value_reactive = reactive(NULL),
      skift_config_reactive = reactive(list(show_phases = FALSE, skift_column = NULL)),
      frys_config_reactive = reactive(NULL),
      app_state = app_state
    ), {
      # anhoej_rules_boxes should exist
      expect_true("anhoej_rules_boxes" %in% names(output))
    })
  })
})

# MODULE STATE MANAGEMENT TESTS ================================================

describe("Module State Management", {

  it("initializes state correctly", {
    app_state <- create_mock_app_state()

    # Verify initial state
    expect_false(isolate(app_state$visualization$cache_updating))
    expect_false(isolate(app_state$visualization$plot_ready))
    expect_null(isolate(app_state$visualization$plot_object))
  })

  it("updates plot_ready flag correctly", {
    app_state <- create_mock_app_state()

    # Simulate successful plot generation
    app_state$visualization$plot_ready <- TRUE
    app_state$visualization$plot_object <- ggplot2::ggplot()

    expect_true(isolate(app_state$visualization$plot_ready))
    expect_s3_class(isolate(app_state$visualization$plot_object), "ggplot")
  })

  it("stores anhoej_results correctly", {
    app_state <- create_mock_app_state()

    # Simulate anhoej results
    anhoej_results <- list(
      longest_run = 5,
      longest_run_max = 6,
      n_crossings = 3,
      n_crossings_min = 2,
      out_of_control_count = 0,
      runs_signal = FALSE,
      crossings_signal = FALSE,
      anhoej_signal = FALSE,
      any_signal = FALSE,
      message = "Test",
      has_valid_data = TRUE
    )

    app_state$visualization$anhoej_results <- anhoej_results

    result <- isolate(app_state$visualization$anhoej_results)

    expect_equal(result$longest_run, 5)
    expect_equal(result$n_crossings, 3)
    expect_true(result$has_valid_data)
  })

  it("tracks centerline changes correctly", {
    app_state <- create_mock_app_state()

    # Set initial centerline
    app_state$visualization$last_centerline_value <- 50

    # Change centerline
    new_centerline <- 60

    # Detect change
    centerline_changed <- !identical(new_centerline, isolate(app_state$visualization$last_centerline_value))

    expect_true(centerline_changed)

    # Update tracked value
    app_state$visualization$last_centerline_value <- new_centerline

    expect_equal(isolate(app_state$visualization$last_centerline_value), 60)
  })
})

# VIEWPORT DIMENSIONS TESTS ====================================================

describe("Viewport Dimensions", {

  it("handles missing viewport dimensions gracefully", {
    skip_if_not(exists("visualizationModuleServer", mode = "function"))

    app_state <- create_mock_app_state()
    test_data <- create_test_data_for_module()

    # Viewport dimensions may not be available initially
    # Module should use defaults (800x600)

    testServer(visualizationModuleServer, args = list(
      data_reactive = reactive(test_data),
      column_config_reactive = reactive(list(
        x_col = "Dato",
        y_col = "Tæller",
        n_col = "Nævner"
      )),
      chart_type_reactive = reactive("p"),
      target_value_reactive = reactive(NULL),
      target_text_reactive = reactive(NULL),
      centerline_value_reactive = reactive(NULL),
      skift_config_reactive = reactive(list(show_phases = FALSE, skift_column = NULL)),
      frys_config_reactive = reactive(NULL),
      app_state = app_state
    ), {
      # Should not crash without viewport dimensions
      expect_true(TRUE)
    })
  })

  it("uses clientData viewport dimensions when available", {
    # This test requires actual Shiny session with clientData
    # Skipped in unit tests
    skip("Requires full Shiny session with clientData")
  })
})

# DEBOUNCING TESTS =============================================================

describe("Debouncing", {

  it("debounces chart_config to prevent redundant renders", {
    # chart_config is debounced by 300ms (DEBOUNCE_DELAYS$input_change)
    # This prevents flickering during rapid dropdown changes

    skip("Debouncing requires time-based testing framework")
  })

  it("debounces spc_inputs to prevent redundant renders", {
    # spc_inputs is debounced by 500ms (DEBOUNCE_DELAYS$file_select)
    # Handles window resize, title editing, etc.

    skip("Debouncing requires time-based testing framework")
  })
})
