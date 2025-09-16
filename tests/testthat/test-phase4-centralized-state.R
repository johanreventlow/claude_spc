# test-phase4-centralized-state.R
# Tests af Phase 4 centraliseret state management funktionalitet

test_that("create_app_state opretter korrekt schema", {
  # Test at create_app_state funktionen eksisterer
  expect_true(exists("create_app_state"))

  # Opret app state
  app_state <- create_app_state()

  # Verificer hovedstrukturen
  expect_type(app_state, "list")
  expect_true("data" %in% names(app_state))
  expect_true("columns" %in% names(app_state))
  expect_true("test_mode" %in% names(app_state))
  expect_true("session" %in% names(app_state))
  expect_true("ui" %in% names(app_state))

  # Verificer data structure
  expect_true("current_data" %in% names(app_state$data))
  expect_true("original_data" %in% names(app_state$data))
  expect_true("file_info" %in% names(app_state$data))
  expect_true("updating_table" %in% names(app_state$data))
  expect_true("table_operation_in_progress" %in% names(app_state$data))
  expect_true("table_operation_cleanup_needed" %in% names(app_state$data))
  expect_true("table_version" %in% names(app_state$data))

  # Verificer columns structure
  expect_true("auto_detect" %in% names(app_state$columns))
  expect_true("mappings" %in% names(app_state$columns))
  expect_true("ui_sync" %in% names(app_state$columns))

  # Verificer auto_detect sub-structure
  expect_true("in_progress" %in% names(app_state$columns$auto_detect))
  expect_true("completed" %in% names(app_state$columns$auto_detect))
  expect_true("trigger" %in% names(app_state$columns$auto_detect))
  expect_true("results" %in% names(app_state$columns$auto_detect))

  # Verificer mappings sub-structure
  expect_true("x_column" %in% names(app_state$columns$mappings))
  expect_true("y_column" %in% names(app_state$columns$mappings))
  expect_true("n_column" %in% names(app_state$columns$mappings))
  expect_true("cl_column" %in% names(app_state$columns$mappings))

  # Verificer ui_sync sub-structure
  expect_true("needed" %in% names(app_state$columns$ui_sync))
  expect_true("last_sync_time" %in% names(app_state$columns$ui_sync))

  # Verificer session structure
  expect_true("auto_save_enabled" %in% names(app_state$session))
  expect_true("restoring_session" %in% names(app_state$session))
  expect_true("file_uploaded" %in% names(app_state$session))
  expect_true("user_started_session" %in% names(app_state$session))
  expect_true("last_save_time" %in% names(app_state$session))
  expect_true("file_name" %in% names(app_state$session))

  # Verificer ui structure
  expect_true("hide_anhoej_rules" %in% names(app_state$ui))

  # Verificer default værdier
  expect_null(app_state$data$current_data)
  expect_null(app_state$data$original_data)
  expect_null(app_state$data$file_info)
  expect_false(app_state$data$updating_table)
  expect_false(app_state$data$table_operation_in_progress)
  expect_false(app_state$data$table_operation_cleanup_needed)
  expect_equal(app_state$data$table_version, 0)

  expect_false(app_state$columns$auto_detect$in_progress)
  expect_false(app_state$columns$auto_detect$completed)
  expect_null(app_state$columns$auto_detect$trigger)
  expect_null(app_state$columns$auto_detect$results)

  expect_null(app_state$columns$mappings$x_column)
  expect_null(app_state$columns$mappings$y_column)
  expect_null(app_state$columns$mappings$n_column)
  expect_null(app_state$columns$mappings$cl_column)

  expect_null(app_state$columns$ui_sync$needed)
  expect_null(app_state$columns$ui_sync$last_sync_time)

  expect_true(app_state$session$auto_save_enabled)
  expect_false(app_state$session$restoring_session)
  expect_false(app_state$session$file_uploaded)
  expect_false(app_state$session$user_started_session)
  expect_null(app_state$session$last_save_time)
  expect_null(app_state$session$file_name)

  expect_false(app_state$ui$hide_anhoej_rules)
})

test_that("dual state sync patterns virker korrekt", {
  # Test dual state access patterns - simulerer Phase 4 logik

  # Simuler at use_centralized_state og app_state er tilgængelige
  use_centralized_state <- TRUE
  app_state <- create_app_state()

  # Mock values for gammelt system
  values <- list(
    current_data = data.frame(Dato = c("01-01-2022"), Tæller = c(10)),
    hide_anhoej_rules = TRUE,
    auto_detected_columns = list(x_col = "Dato", y_col = "Tæller", n_col = NULL)
  )

  # Test dual-state logic for current_data
  current_data_check <- if (exists("use_centralized_state") && use_centralized_state && exists("app_state")) {
    app_state$data$current_data
  } else {
    values$current_data
  }

  # Når app_state$data$current_data er NULL, skal det falde tilbage til values
  expect_null(current_data_check)  # Initially NULL in centralized state

  # Sæt data i centralized state
  app_state$data$current_data <- data.frame(Dato = c("01-02-2022"), Tæller = c(20))

  # Nu skulle dual-state logic bruge centralized version
  current_data_check <- if (exists("use_centralized_state") && use_centralized_state && exists("app_state")) {
    app_state$data$current_data
  } else {
    values$current_data
  }

  expect_equal(nrow(current_data_check), 1)
  expect_equal(current_data_check$Tæller[1], 20)

  # Test dual-state logic for hide_anhoej_rules
  hide_anhoej_rules_check <- if (exists("use_centralized_state") && use_centralized_state && exists("app_state")) {
    app_state$ui$hide_anhoej_rules
  } else {
    values$hide_anhoej_rules
  }

  expect_false(hide_anhoej_rules_check)  # Default fra app_state

  # Sæt hide_anhoej_rules i centralized state
  app_state$ui$hide_anhoej_rules <- TRUE

  hide_anhoej_rules_check <- if (exists("use_centralized_state") && use_centralized_state && exists("app_state")) {
    app_state$ui$hide_anhoej_rules
  } else {
    values$hide_anhoej_rules
  }

  expect_true(hide_anhoej_rules_check)

  # Test dual-state logic for auto_detected_columns
  auto_columns_check <- if (exists("use_centralized_state") && use_centralized_state && exists("app_state")) {
    app_state$columns$auto_detect$results
  } else {
    values$auto_detected_columns
  }

  expect_null(auto_columns_check)  # Initially NULL in centralized state

  # Sæt auto_detected_columns i centralized state
  app_state$columns$auto_detect$results <- list(x_col = "NewDate", y_col = "NewCount", n_col = "NewDenom")

  auto_columns_check <- if (exists("use_centralized_state") && use_centralized_state && exists("app_state")) {
    app_state$columns$auto_detect$results
  } else {
    values$auto_detected_columns
  }

  expect_equal(auto_columns_check$x_col, "NewDate")
  expect_equal(auto_columns_check$y_col, "NewCount")
  expect_equal(auto_columns_check$n_col, "NewDenom")
})

test_that("state consistency mellem old og new systems", {
  # Test at data konsistens bevares mellem systemer

  app_state <- create_app_state()

  # Simuler data i gammelt system
  old_values <- list(
    current_data = data.frame(
      Skift = c(FALSE, FALSE),
      Frys = c(FALSE, FALSE),
      Dato = c("01-01-2022", "01-02-2022"),
      Tæller = c(10, 15),
      Nævner = c(100, 150),
      Kommentar = c(NA, "Test")
    ),
    hide_anhoej_rules = TRUE,
    auto_detected_columns = list(
      x_col = "Dato",
      y_col = "Tæller",
      n_col = "Nævner"
    ),
    auto_detect_in_progress = FALSE,
    initial_auto_detect_completed = TRUE
  )

  # Simuler sync til nyt system
  app_state$data$current_data <- old_values$current_data
  app_state$ui$hide_anhoej_rules <- old_values$hide_anhoej_rules
  app_state$columns$auto_detect$results <- old_values$auto_detected_columns
  app_state$columns$auto_detect$in_progress <- old_values$auto_detect_in_progress
  app_state$columns$auto_detect$completed <- old_values$initial_auto_detect_completed

  # Verificer konsistens
  expect_equal(nrow(app_state$data$current_data), 2)
  expect_equal(names(app_state$data$current_data), c("Skift", "Frys", "Dato", "Tæller", "Nævner", "Kommentar"))
  expect_equal(app_state$data$current_data$Tæller, c(10, 15))
  expect_equal(app_state$data$current_data$Nævner, c(100, 150))

  expect_true(app_state$ui$hide_anhoej_rules)

  expect_equal(app_state$columns$auto_detect$results$x_col, "Dato")
  expect_equal(app_state$columns$auto_detect$results$y_col, "Tæller")
  expect_equal(app_state$columns$auto_detect$results$n_col, "Nævner")

  expect_false(app_state$columns$auto_detect$in_progress)
  expect_true(app_state$columns$auto_detect$completed)
})

test_that("centralized state management edge cases", {
  # Test edge cases og error scenarios

  app_state <- create_app_state()

  # Test med NULL data
  app_state$data$current_data <- NULL
  expect_null(app_state$data$current_data)

  # Test med tom data frame
  app_state$data$current_data <- data.frame()
  expect_equal(nrow(app_state$data$current_data), 0)
  expect_equal(ncol(app_state$data$current_data), 0)

  # Test med invalid column names
  test_data <- data.frame(x = 1, y = 2)
  names(test_data) <- c("", "invalid name with spaces")
  app_state$data$current_data <- test_data

  expect_equal(names(app_state$data$current_data), c("", "invalid name with spaces"))
  expect_equal(nrow(app_state$data$current_data), 1)

  # Test UI state toggle
  expect_false(app_state$ui$hide_anhoej_rules)  # Default
  app_state$ui$hide_anhoej_rules <- TRUE
  expect_true(app_state$ui$hide_anhoej_rules)
  app_state$ui$hide_anhoej_rules <- FALSE
  expect_false(app_state$ui$hide_anhoej_rules)

  # Test session state management
  expect_true(app_state$session$auto_save_enabled)  # Default
  expect_false(app_state$session$restoring_session)
  expect_false(app_state$session$file_uploaded)

  app_state$session$file_uploaded <- TRUE
  app_state$session$file_name <- "test_file.csv"
  expect_true(app_state$session$file_uploaded)
  expect_equal(app_state$session$file_name, "test_file.csv")

  # Test auto-detect state management
  expect_false(app_state$columns$auto_detect$in_progress)
  expect_false(app_state$columns$auto_detect$completed)

  app_state$columns$auto_detect$in_progress <- TRUE
  expect_true(app_state$columns$auto_detect$in_progress)

  app_state$columns$auto_detect$in_progress <- FALSE
  app_state$columns$auto_detect$completed <- TRUE
  expect_false(app_state$columns$auto_detect$in_progress)
  expect_true(app_state$columns$auto_detect$completed)

  # Test table versioning
  expect_equal(app_state$data$table_version, 0)
  app_state$data$table_version <- app_state$data$table_version + 1
  expect_equal(app_state$data$table_version, 1)

  # Test table operation flags
  expect_false(app_state$data$updating_table)
  expect_false(app_state$data$table_operation_in_progress)
  expect_false(app_state$data$table_operation_cleanup_needed)

  app_state$data$updating_table <- TRUE
  app_state$data$table_operation_in_progress <- TRUE
  expect_true(app_state$data$updating_table)
  expect_true(app_state$data$table_operation_in_progress)
})