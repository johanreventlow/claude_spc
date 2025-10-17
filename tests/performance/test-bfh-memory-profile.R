# test-bfh-memory-profile.R
# Task 033 Stream 2: BFHcharts Memory Profiling Test Suite
#
# Comprehensive memory profiling tests for BFHchart backend. Detects memory leaks,
# measures memory growth over extended sessions, and profiles single render operations.
#
# Dependencies: profvis, bench, testthat, BFHcharts
# Target: Memory growth < 50 MB in 4-hour session simulation

library(testthat)

# Skip if required packages not available
skip_if_not_installed_profiling <- function() {
  if (!requireNamespace("profvis", quietly = TRUE)) {
    skip("profvis package not available")
  }
  if (!requireNamespace("bench", quietly = TRUE)) {
    skip("bench package not available")
  }
  if (!requireNamespace("BFHcharts", quietly = TRUE)) {
    skip("BFHcharts package not available")
  }
}

# Test Data Generator (reuse from benchmark suite)
generate_test_data <- function(n_rows, chart_type = "run", seed = 42) {
  set.seed(seed)

  data <- data.frame(
    dato = seq.Date(Sys.Date() - n_rows + 1, Sys.Date(), by = "day"),
    vaerdi = rnorm(n_rows, mean = 100, sd = 15)
  )

  if (chart_type %in% c("p", "c", "u")) {
    data$naevner <- sample(50:200, n_rows, replace = TRUE)
    if (chart_type == "p") {
      data$vaerdi <- pmin(data$vaerdi, data$naevner)
    }
  }

  if (chart_type %in% c("xbar", "s")) {
    data$naevner <- sample(5:10, n_rows, replace = TRUE)
  }

  return(data)
}

# Memory Leak Detection Tests
# ===========================

test_that("BFHchart no memory leaks - long session (1000 renders)", {
  skip_if_not_installed_profiling()

  # Generate test data
  data <- generate_test_data(100, "run")

  # Baseline memory measurement
  gc(full = TRUE, verbose = FALSE)
  Sys.sleep(0.5) # Allow GC to complete
  mem_start <- bench::bench_process_memory()$rss

  cat("\n=== BFHchart Memory Leak Test (1000 renders) ===\n")
  cat(sprintf("Starting memory: %.2f MB\n", mem_start / 1024^2))

  # Simulate 4-hour session (1000 renders)
  # At 1 render per 14.4 seconds = 4 hours
  n_iterations <- 1000
  memory_checkpoints <- c()

  for (i in 1:n_iterations) {
    # Render chart
    result <- compute_spc_results_bfh(
      data = data,
      x_var = "dato",
      y_var = "vaerdi",
      chart_type = "run"
    )

    # Clean up
    rm(result)

    # Memory checkpoint every 100 iterations
    if (i %% 100 == 0) {
      gc(full = FALSE, verbose = FALSE)
      mem_current <- bench::bench_process_memory()$rss
      mem_growth_mb <- (mem_current - mem_start) / 1024^2

      memory_checkpoints <- c(memory_checkpoints, mem_growth_mb)

      cat(sprintf(
        "Iteration %d: Memory = %.2f MB (growth: %+.2f MB)\n",
        i,
        mem_current / 1024^2,
        mem_growth_mb
      ))
    }

    # Early exit if memory growth exceeds threshold
    if (i %% 100 == 0) {
      if ((bench::bench_process_memory()$rss - mem_start) / 1024^2 > 100) {
        cat("⚠️ Memory growth exceeding 100 MB, stopping early\n")
        break
      }
    }
  }

  # Final memory measurement
  gc(full = TRUE, verbose = FALSE)
  Sys.sleep(0.5)
  mem_end <- bench::bench_process_memory()$rss
  mem_growth <- (mem_end - mem_start) / 1024^2 # MB

  cat(sprintf("\nFinal memory: %.2f MB\n", mem_end / 1024^2))
  cat(sprintf("Total memory growth: %.2f MB\n", mem_growth))

  # Assertion: Memory growth should be < 50 MB
  expect_lt(
    mem_growth,
    50,
    label = sprintf("Memory leak detected: %.2f MB growth exceeds 50 MB threshold", mem_growth)
  )

  # Save memory profile data
  saveRDS(
    list(
      start_memory = mem_start,
      end_memory = mem_end,
      growth_mb = mem_growth,
      checkpoints = memory_checkpoints,
      iterations = n_iterations
    ),
    "tests/performance/memory_profile_long_session.rds"
  )

  cat("\n✅ No memory leaks detected\n")
})

test_that("BFHchart memory usage - different data sizes", {
  skip_if_not_installed_profiling()

  data_sizes <- c(50, 100, 500, 1000)
  memory_usage <- list()

  cat("\n=== BFHchart Memory Usage by Data Size ===\n")

  for (n in data_sizes) {
    data <- generate_test_data(n, "i")

    # Measure memory before and after
    gc(full = TRUE, verbose = FALSE)
    mem_before <- bench::bench_process_memory()$rss

    # Render chart
    result <- compute_spc_results_bfh(
      data = data,
      x_var = "dato",
      y_var = "vaerdi",
      chart_type = "i"
    )

    # Measure memory after
    mem_after <- bench::bench_process_memory()$rss
    mem_used_mb <- (mem_after - mem_before) / 1024^2

    memory_usage[[paste0("n_", n)]] <- list(
      data_size = n,
      memory_mb = mem_used_mb
    )

    cat(sprintf("n=%d: %.2f MB\n", n, mem_used_mb))

    # Clean up
    rm(result)
    gc(full = FALSE, verbose = FALSE)
  }

  # Save memory usage data
  saveRDS(memory_usage, "tests/performance/memory_usage_by_size.rds")

  cat("\n✅ Memory usage profiling complete\n")
})

test_that("BFHchart memory usage - different chart types", {
  skip_if_not_installed_profiling()

  chart_types <- c("run", "i", "p", "c", "u", "xbar", "s")
  memory_usage <- list()

  cat("\n=== BFHchart Memory Usage by Chart Type ===\n")

  for (chart_type in chart_types) {
    data <- generate_test_data(100, chart_type)

    # Measure memory before and after
    gc(full = TRUE, verbose = FALSE)
    mem_before <- bench::bench_process_memory()$rss

    # Render chart
    if (chart_type %in% c("p", "c", "u", "xbar", "s")) {
      result <- compute_spc_results_bfh(
        data = data,
        x_var = "dato",
        y_var = "vaerdi",
        n_var = "naevner",
        chart_type = chart_type
      )
    } else {
      result <- compute_spc_results_bfh(
        data = data,
        x_var = "dato",
        y_var = "vaerdi",
        chart_type = chart_type
      )
    }

    # Measure memory after
    mem_after <- bench::bench_process_memory()$rss
    mem_used_mb <- (mem_after - mem_before) / 1024^2

    memory_usage[[chart_type]] <- list(
      chart_type = chart_type,
      memory_mb = mem_used_mb
    )

    cat(sprintf("%s: %.2f MB\n", chart_type, mem_used_mb))

    # Clean up
    rm(result)
    gc(full = FALSE, verbose = FALSE)
  }

  # Save memory usage data
  saveRDS(memory_usage, "tests/performance/memory_usage_by_chart_type.rds")

  cat("\n✅ Chart type memory profiling complete\n")
})

# Profvis Integration Tests
# =========================

test_that("BFHchart single render profiling with profvis", {
  skip_if_not_installed_profiling()

  # Large dataset for detailed profiling
  data <- generate_test_data(1000, "run")

  cat("\n=== Profvis Memory and Performance Profiling ===\n")

  # Profile single render
  prof <- profvis::profvis({
    result <- compute_spc_results_bfh(
      data = data,
      x_var = "dato",
      y_var = "vaerdi",
      chart_type = "run"
    )
  })

  # Save profile as HTML
  output_path <- "tests/performance/bfh_profile.html"
  htmlwidgets::saveWidget(prof, output_path, selfcontained = TRUE)

  # Verify profile was saved
  expect_true(file.exists(output_path))

  cat(sprintf("Profile saved to: %s\n", output_path))
  cat("\n✅ Profvis profiling complete\n")
})

test_that("BFHchart multi-chart profiling", {
  skip_if_not_installed_profiling()

  # Profile multiple chart types in sequence
  data_run <- generate_test_data(200, "run")
  data_i <- generate_test_data(200, "i")
  data_p <- generate_test_data(200, "p")

  cat("\n=== Multi-Chart Profvis Profiling ===\n")

  prof <- profvis::profvis({
    result1 <- compute_spc_results_bfh(
      data = data_run,
      x_var = "dato",
      y_var = "vaerdi",
      chart_type = "run"
    )

    result2 <- compute_spc_results_bfh(
      data = data_i,
      x_var = "dato",
      y_var = "vaerdi",
      chart_type = "i"
    )

    result3 <- compute_spc_results_bfh(
      data = data_p,
      x_var = "dato",
      y_var = "vaerdi",
      n_var = "naevner",
      chart_type = "p"
    )
  })

  # Save profile
  output_path <- "tests/performance/bfh_profile_multi_chart.html"
  htmlwidgets::saveWidget(prof, output_path, selfcontained = TRUE)

  expect_true(file.exists(output_path))

  cat(sprintf("Multi-chart profile saved to: %s\n", output_path))
  cat("\n✅ Multi-chart profiling complete\n")
})

# Memory Stability Tests
# ======================

test_that("BFHchart memory stability - repeated renders same data", {
  skip_if_not_installed_profiling()

  data <- generate_test_data(100, "i")

  # Measure memory growth over 50 identical renders
  gc(full = TRUE, verbose = FALSE)
  mem_start <- bench::bench_process_memory()$rss

  cat("\n=== Memory Stability Test (50 identical renders) ===\n")

  for (i in 1:50) {
    result <- compute_spc_results_bfh(
      data = data,
      x_var = "dato",
      y_var = "vaerdi",
      chart_type = "i"
    )
    rm(result)

    if (i %% 10 == 0) {
      gc(full = FALSE, verbose = FALSE)
    }
  }

  gc(full = TRUE, verbose = FALSE)
  mem_end <- bench::bench_process_memory()$rss
  mem_growth <- (mem_end - mem_start) / 1024^2

  cat(sprintf("Memory growth (50 renders): %.2f MB\n", mem_growth))

  # For identical renders, memory growth should be minimal
  expect_lt(
    mem_growth,
    10,
    label = sprintf("Memory instability detected: %.2f MB growth for identical renders", mem_growth)
  )

  cat("\n✅ Memory stability verified\n")
})

test_that("BFHchart cleanup - verify objects released", {
  skip_if_not_installed_profiling()

  # Check that temporary objects are properly released
  gc(full = TRUE, verbose = FALSE)
  mem_baseline <- bench::bench_process_memory()$rss

  # Create and destroy multiple results
  for (i in 1:10) {
    data <- generate_test_data(100, "run")
    result <- compute_spc_results_bfh(
      data = data,
      x_var = "dato",
      y_var = "vaerdi",
      chart_type = "run"
    )
    rm(data, result)
  }

  # Force full GC
  gc(full = TRUE, verbose = FALSE)
  Sys.sleep(0.5)
  mem_after_gc <- bench::bench_process_memory()$rss

  mem_diff <- (mem_after_gc - mem_baseline) / 1024^2

  cat(sprintf("\nMemory after cleanup: %+.2f MB from baseline\n", mem_diff))

  # Memory should return close to baseline after GC
  expect_lt(
    abs(mem_diff),
    20,
    label = sprintf("Objects not properly released: %.2f MB retained", mem_diff)
  )

  cat("\n✅ Object cleanup verified\n")
})
