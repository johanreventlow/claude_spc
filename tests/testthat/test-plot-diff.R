# test-plot-diff.R
# Test suite for utils_plot_diff.R
# Comprehensive coverage for intelligent plot update detection

test_that("calculate_plot_metadata_hash genererer konsistent hash", {
  metadata <- list(
    title = "SPC Chart",
    target_value = 10,
    centerline_value = 8,
    chart_type = "run"
  )

  hash1 <- calculate_plot_metadata_hash(metadata)
  hash2 <- calculate_plot_metadata_hash(metadata)

  expect_equal(hash1, hash2)
  expect_true(nchar(hash1) > 0)
})

test_that("calculate_plot_metadata_hash detekterer ændringer", {
  metadata1 <- list(title = "Original Title", target_value = 10)
  metadata2 <- list(title = "Changed Title", target_value = 10)

  hash1 <- calculate_plot_metadata_hash(metadata1)
  hash2 <- calculate_plot_metadata_hash(metadata2)

  expect_false(identical(hash1, hash2))
})

test_that("calculate_plot_metadata_hash ignorerer irrelevante felter", {
  # Kun relevante felter påvirker hash
  metadata1 <- list(title = "Title", target_value = 10, extra_field = "ignored")
  metadata2 <- list(title = "Title", target_value = 10, different_extra = "also_ignored")

  hash1 <- calculate_plot_metadata_hash(metadata1)
  hash2 <- calculate_plot_metadata_hash(metadata2)

  expect_equal(hash1, hash2)
})

test_that("calculate_plot_data_hash håndterer NULL data", {
  config <- list(x_col = "x", y_col = "y")
  hash <- calculate_plot_data_hash(NULL, config)

  expect_equal(hash, "EMPTY_DATA")
})

test_that("calculate_plot_data_hash håndterer empty data frame", {
  data <- data.frame()
  config <- list(x_col = "x", y_col = "y")
  hash <- calculate_plot_data_hash(data, config)

  expect_equal(hash, "EMPTY_DATA")
})

test_that("calculate_plot_data_hash genererer konsistent hash for samme data", {
  data <- data.frame(Dato = 1:10, Værdi = rnorm(10))
  config <- list(x_col = "Dato", y_col = "Værdi")

  hash1 <- calculate_plot_data_hash(data, config)
  hash2 <- calculate_plot_data_hash(data, config)

  expect_equal(hash1, hash2)
})

test_that("calculate_plot_data_hash detekterer data ændringer", {
  data1 <- data.frame(Dato = 1:10, Værdi = rnorm(10, seed = 42))
  data2 <- data.frame(Dato = 1:10, Værdi = rnorm(10, seed = 43))
  config <- list(x_col = "Dato", y_col = "Værdi")

  hash1 <- calculate_plot_data_hash(data1, config)
  hash2 <- calculate_plot_data_hash(data2, config)

  expect_false(identical(hash1, hash2))
})

test_that("calculate_plot_data_hash fokuserer kun på relevante kolonner", {
  data1 <- data.frame(Dato = 1:10, Værdi = 11:20, Extra = letters[1:10])
  data2 <- data.frame(Dato = 1:10, Værdi = 11:20, Extra = LETTERS[1:10])
  config <- list(x_col = "Dato", y_col = "Værdi")

  hash1 <- calculate_plot_data_hash(data1, config)
  hash2 <- calculate_plot_data_hash(data2, config)

  # Extra kolonne ændringer skal ikke påvirke hash
  expect_equal(hash1, hash2)
})

test_that("detect_plot_update_type returnerer 'none' når ingen ændringer", {
  data <- data.frame(x = 1:10, y = rnorm(10))
  metadata <- list(title = "Title", target = 10)
  config <- list(x_col = "x", y_col = "y", chart_type = "run")

  current_state <- list(data = data, metadata = metadata, config = config)
  previous_state <- current_state

  diff <- detect_plot_update_type(current_state, previous_state)

  expect_equal(diff$update_type, "none")
  expect_equal(length(diff$changed_fields), 0)
})

test_that("detect_plot_update_type detekterer metadata-only changes", {
  data <- data.frame(x = 1:10, y = rnorm(10))
  config <- list(x_col = "x", y_col = "y", chart_type = "run")

  current_state <- list(
    data = data,
    metadata = list(title = "New Title", target = 10),
    config = config
  )

  previous_state <- list(
    data = data,
    metadata = list(title = "Old Title", target = 10),
    config = config
  )

  diff <- detect_plot_update_type(current_state, previous_state)

  expect_equal(diff$update_type, "metadata_only")
  expect_true("title" %in% diff$changed_fields)
})

test_that("detect_plot_update_type detekterer data changes", {
  config <- list(x_col = "x", y_col = "y", chart_type = "run")
  metadata <- list(title = "Title", target = 10)

  current_state <- list(
    data = data.frame(x = 1:10, y = rnorm(10, seed = 42)),
    metadata = metadata,
    config = config
  )

  previous_state <- list(
    data = data.frame(x = 1:10, y = rnorm(10, seed = 43)),
    metadata = metadata,
    config = config
  )

  diff <- detect_plot_update_type(current_state, previous_state)

  expect_equal(diff$update_type, "data_changed")
})

test_that("detect_plot_update_type håndterer NULL previous state", {
  current_state <- list(
    data = data.frame(x = 1:10, y = rnorm(10)),
    metadata = list(title = "Title"),
    config = list(x_col = "x", y_col = "y")
  )

  diff <- detect_plot_update_type(current_state, NULL)

  expect_equal(diff$update_type, "data_changed")
  expect_equal(diff$changed_fields, "initial_render")
})

test_that("identify_changed_metadata_fields finder alle ændrede felter", {
  current_meta <- list(title = "New", target = 10, centerline = 5)
  previous_meta <- list(title = "Old", target = 10, centerline = 8)

  changed <- identify_changed_metadata_fields(current_meta, previous_meta)

  expect_true("title" %in% changed)
  expect_true("centerline" %in% changed)
  expect_false("target" %in% changed)
})

test_that("identify_changed_metadata_fields håndterer NULL værdier", {
  current_meta <- list(title = "Title", target = NULL)
  previous_meta <- list(title = "Title", target = 10)

  changed <- identify_changed_metadata_fields(current_meta, previous_meta)

  expect_true("target" %in% changed)
  expect_false("title" %in% changed)
})

test_that("identify_changed_metadata_fields håndterer nye felter", {
  current_meta <- list(title = "Title", new_field = "new")
  previous_meta <- list(title = "Title")

  changed <- identify_changed_metadata_fields(current_meta, previous_meta)

  expect_true("new_field" %in% changed)
  expect_false("title" %in% changed)
})

test_that("apply_metadata_update opdaterer plot title", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)
  plot <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + labs(title = "Old Title")

  new_meta <- list(title = "New Title")
  updated_plot <- apply_metadata_update(plot, new_meta, changed_fields = "title")

  # Verificer at title blev opdateret
  expect_true(!is.null(updated_plot$labels$title))
  expect_equal(updated_plot$labels$title, "New Title")
})

test_that("apply_metadata_update opdaterer y-axis label", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)
  plot <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()

  new_meta <- list(y_axis_unit = "count")
  updated_plot <- apply_metadata_update(plot, new_meta, changed_fields = "y_axis_unit")

  # Verificer at y-label blev opdateret
  expect_true(!is.null(updated_plot$labels$y))
})

test_that("apply_metadata_update håndterer NULL plot gracefully", {
  new_meta <- list(title = "Title")
  result <- apply_metadata_update(NULL, new_meta, changed_fields = "title")

  expect_null(result)
})

test_that("apply_metadata_update håndterer empty changed_fields", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)
  plot <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()

  new_meta <- list(title = "Title")
  result <- apply_metadata_update(plot, new_meta, changed_fields = character(0))

  # Plot skal returneres uændret
  expect_equal(result, plot)
})

test_that("create_plot_state_snapshot genererer komplet snapshot", {
  data <- data.frame(x = 1:10, y = rnorm(10))
  metadata <- list(title = "Title", target = 10)
  config <- list(x_col = "x", y_col = "y")

  snapshot <- create_plot_state_snapshot(data, metadata, config)

  expect_true(!is.null(snapshot$data))
  expect_true(!is.null(snapshot$metadata))
  expect_true(!is.null(snapshot$config))
  expect_true(!is.null(snapshot$data_hash))
  expect_true(!is.null(snapshot$metadata_hash))
  expect_true(!is.null(snapshot$timestamp))
})

test_that("create_plot_state_snapshot håndterer NULL input gracefully", {
  snapshot <- create_plot_state_snapshot(NULL, NULL, NULL)

  # Med safe_operation skal vi få NULL fallback ved errors
  expect_true(is.null(snapshot) || !is.null(snapshot$data_hash))
})

test_that("Integration: Full metadata-only update workflow", {
  # Simuler en typisk metadata-only update
  data <- data.frame(Dato = 1:20, Værdi = rnorm(20))
  config <- list(x_col = "Dato", y_col = "Værdi", chart_type = "run")

  # Initial state
  initial_meta <- list(title = "Original Title", target_value = 10, chart_type = "run")
  initial_state <- create_plot_state_snapshot(data, initial_meta, config)

  # Updated state (only title changed)
  updated_meta <- list(title = "Updated Title", target_value = 10, chart_type = "run")
  updated_state <- create_plot_state_snapshot(data, updated_meta, config)

  # Detect diff
  diff <- detect_plot_update_type(updated_state, initial_state)

  # Verificer metadata-only detection
  expect_equal(diff$update_type, "metadata_only")
  expect_true("title" %in% diff$changed_fields)
  expect_false("target_value" %in% diff$changed_fields)
})

test_that("Integration: Full data change workflow", {
  # Simuler en data ændring
  config <- list(x_col = "Dato", y_col = "Værdi", chart_type = "run")
  metadata <- list(title = "Chart Title", target_value = 10)

  # Initial state
  initial_data <- data.frame(Dato = 1:20, Værdi = rnorm(20, seed = 42))
  initial_state <- create_plot_state_snapshot(initial_data, metadata, config)

  # Updated state (data changed)
  updated_data <- data.frame(Dato = 1:25, Værdi = rnorm(25, seed = 42))
  updated_state <- create_plot_state_snapshot(updated_data, metadata, config)

  # Detect diff
  diff <- detect_plot_update_type(updated_state, initial_state)

  # Verificer data change detection
  expect_equal(diff$update_type, "data_changed")
})

test_that("Performance: Hash calculation er hurtig", {
  skip_if_not_installed("microbenchmark")

  # Stor datasæt for performance test
  large_data <- data.frame(
    x = 1:1000,
    y = rnorm(1000),
    z = runif(1000)
  )
  config <- list(x_col = "x", y_col = "y")

  # Benchmark hash calculation
  result <- microbenchmark::microbenchmark(
    calculate_plot_data_hash(large_data, config),
    times = 100
  )

  # Hash calculation skal være under 10ms median
  median_time_ms <- median(result$time) / 1e6
  expect_lt(median_time_ms, 10)
})
