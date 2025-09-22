# test-performance.R
# Performance tests and benchmarks for critical SPC app functions

test_that("File upload performance is acceptable", {
  skip_if_not_installed("microbenchmark")

  # Create test CSV data of reasonable size
  large_test_data <- data.frame(
    Dato = rep(paste0("0", 1:9, "-01-2024"), 100),
    Tæller = sample(1:50, 900, replace = TRUE),
    Nævner = sample(100:200, 900, replace = TRUE)
  )

  temp_file <- tempfile(fileext = ".csv")
  write.csv(large_test_data, temp_file, row.names = FALSE)

  if (exists("handle_csv_upload")) {
    benchmark_result <- microbenchmark::microbenchmark(
      csv_upload = handle_csv_upload(temp_file, NULL, NULL, NULL),
      times = 5
    )

    # CSV upload should complete in reasonable time (under 1 second median)
    median_time_ms <- median(benchmark_result$time) / 1e6
    expect_lt(median_time_ms, 1000, info = paste("CSV upload took", median_time_ms, "ms"))
  }

  unlink(temp_file)
})

test_that("Auto-detection performance scales with data size", {
  skip_if_not_installed("microbenchmark")

  # Test with different data sizes
  small_data <- data.frame(
    x = 1:10,
    y = rnorm(10),
    z = sample(letters, 10, replace = TRUE)
  )

  medium_data <- data.frame(
    x = 1:100,
    y = rnorm(100),
    z = sample(letters, 100, replace = TRUE)
  )

  large_data <- data.frame(
    x = 1:1000,
    y = rnorm(1000),
    z = sample(letters, 1000, replace = TRUE)
  )

  if (exists("find_numeric_columns")) {
    small_time <- system.time({
      find_numeric_columns(small_data)
    })

    medium_time <- system.time({
      find_numeric_columns(medium_data)
    })

    large_time <- system.time({
      find_numeric_columns(large_data)
    })

    # Performance should scale reasonably (not exponentially)
    expect_lt(large_time[["elapsed"]], small_time[["elapsed"]] * 50)
  }
})

test_that("Reactive performance under rapid updates", {
  skip_if_not_installed("shiny")

  if (exists("create_app_state")) {
    app_state <- create_app_state()

    # Measure time for rapid state updates
    start_time <- Sys.time()

    for (i in 1:100) {
      shiny::isolate({
        app_state$columns$mappings$x_column <- paste("col", i)
        app_state$events$ui_sync_needed <- app_state$events$ui_sync_needed + 1L
      })
    }

    end_time <- Sys.time()
    total_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

    # 100 updates should complete quickly (under 1 second)
    expect_lt(total_time, 1.0)
  }
})

test_that("Plot generation performance with large datasets", {
  skip_if_not_installed("qicharts2")
  skip_if_not_installed("microbenchmark")

  # Large dataset for plotting
  large_plot_data <- data.frame(
    x = seq(as.Date("2024-01-01"), by = "day", length.out = 365),
    y = cumsum(rnorm(365, mean = 0.1, sd = 0.05)) + 10,
    n = rep(100, 365)
  )

  benchmark_result <- microbenchmark::microbenchmark(
    run_chart = qicharts2::qic(x = x, y = y, data = large_plot_data, chart = "run"),
    p_chart = qicharts2::qic(x = x, y = y, n = n, data = large_plot_data, chart = "p"),
    times = 3
  )

  # Plot generation should be reasonable even with large data
  max_time_ms <- max(benchmark_result$time) / 1e6
  expect_lt(max_time_ms, 5000, info = paste("Plot generation took max", max_time_ms, "ms"))
})

test_that("Memory usage stays within bounds", {
  if (exists("create_app_state")) {
    # Measure memory before
    gc()
    mem_before <- as.numeric(object.size(ls(envir = .GlobalEnv)))

    # Create multiple app states (simulating memory usage)
    states <- list()
    for (i in 1:10) {
      states[[i]] <- create_app_state()
      # Add some test data
      shiny::isolate({
        states[[i]]$data$current_data <- data.frame(
          x = 1:100,
          y = rnorm(100)
        )
      })
    }

    # Measure memory after
    gc()
    mem_after <- as.numeric(object.size(ls(envir = .GlobalEnv)))

    # Memory increase should be reasonable
    memory_increase_mb <- (mem_after - mem_before) / (1024^2)
    expect_lt(memory_increase_mb, 50, info = paste("Memory increased by", memory_increase_mb, "MB"))

    # Cleanup
    rm(states)
    gc()
  }
})

test_that("Concurrent operations don't degrade performance significantly", {
  skip_if_not_installed("parallel")

  test_data <- data.frame(
    Dato = rep("01-01-2024", 100),
    Tæller = 1:100,
    Nævner = rep(100, 100)
  )

  if (exists("appears_numeric")) {
    # Sequential timing
    sequential_time <- system.time({
      for (i in 1:10) {
        appears_numeric(test_data$Tæller)
      }
    })

    # Parallel timing (if supported)
    if (.Platform$OS.type != "windows") {
      parallel_time <- system.time({
        parallel::mclapply(1:10, function(i) {
          appears_numeric(test_data$Tæller)
        }, mc.cores = 2)
      })

      # Parallel should not be significantly slower
      expect_lt(parallel_time[["elapsed"]], sequential_time[["elapsed"]] * 2)
    }
  }
})