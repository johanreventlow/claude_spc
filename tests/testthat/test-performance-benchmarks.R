# test-performance-benchmarks.R
# ==============================================================================
# PERFORMANCE BENCHMARKING SUITE
# ==============================================================================
#
# FORMÅL: Måle og dokumentere kritiske performance metrics
# FOKUS: Plot generation, cache efficiency, memory usage, reactive overhead
#
# STRUKTUR:
#   1. Plot Generation Timing (target: <500ms for standard, <2s for large)
#   2. Cache Hit Rates (target: >80% hit rate)
#   3. Memory Usage Profiling (target: <100MB baseline)
#   4. Reactive Chain Overhead (target: <100ms per chain)
#   5. Data Processing Performance (target: <200ms for 1000 rows)
#   6. Auto-Detection Performance (target: <100ms for standard data)
#
# SUCCESS CRITERIA:
#   - All benchmarks within target thresholds
#   - Performance regression detection
#   - Actionable performance insights
#   - Reproducible results
# ==============================================================================

library(testthat)
library(bench)

# SETUP HELPERS ================================================================

# Helper til at oprette benchmark data
create_benchmark_data <- function(n_rows = 100,
                                   n_cols = 3,
                                   include_dates = TRUE,
                                   include_factors = FALSE) {

  base_data <- data.frame(
    Dato = if (include_dates) {
      seq.Date(as.Date("2024-01-01"), by = "day", length.out = n_rows)
    } else {
      1:n_rows
    },
    Tæller = sample(40:50, n_rows, replace = TRUE),
    Nævner = rep(50, n_rows),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Add extra columns if requested
  if (n_cols > 3) {
    for (i in 4:n_cols) {
      base_data[[paste0("Extra_", i - 3)]] <- rnorm(n_rows)
    }
  }

  # Add factor columns if requested
  if (include_factors) {
    base_data$Gruppe <- factor(sample(c("A", "B", "C"), n_rows, replace = TRUE))
  }

  return(base_data)
}

# Helper til at måle memory usage
measure_memory_usage <- function(expr) {
  gc(verbose = FALSE, reset = TRUE)
  mem_before <- as.numeric(gc(verbose = FALSE)[2, 2])

  result <- force(expr)

  mem_after <- as.numeric(gc(verbose = FALSE)[2, 2])
  mem_used <- mem_after - mem_before

  list(
    result = result,
    memory_mb = mem_used,
    memory_bytes = mem_used * 1024^2
  )
}

# PLOT GENERATION TIMING =======================================================

describe("Plot Generation Performance", {

  it("generates standard SPC plot in <500ms", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    test_data <- create_benchmark_data(n_rows = 100)

    benchmark_result <- bench::mark(
      plot = generateSPCPlot(
        data = test_data,
        x_col = "Dato",
        y_col = "Tæller",
        n_col = "Nævner",
        chart_type = "p",
        chart_title = "Performance Test"
      ),
      iterations = 10,
      check = FALSE
    )

    median_time_ms <- as.numeric(benchmark_result$median) * 1000

    # Log results
    message(sprintf(
      "Standard plot generation: %.2f ms (target: <500ms)",
      median_time_ms
    ))

    expect_lt(median_time_ms, 500)
  })

  it("generates large dataset plot in <2s", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    large_data <- create_benchmark_data(n_rows = 1000)

    benchmark_result <- bench::mark(
      plot = generateSPCPlot(
        data = large_data,
        x_col = "Dato",
        y_col = "Tæller",
        n_col = "Nævner",
        chart_type = "p",
        chart_title = "Large Dataset Test"
      ),
      iterations = 5,
      check = FALSE
    )

    median_time_ms <- as.numeric(benchmark_result$median) * 1000

    message(sprintf(
      "Large dataset plot generation: %.2f ms (target: <2000ms)",
      median_time_ms
    ))

    expect_lt(median_time_ms, 2000)
  })

  it("generates plot with phases efficiently", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    phase_data <- create_benchmark_data(n_rows = 100)
    phase_data$Skift <- c(rep(1, 50), rep(2, 50))

    benchmark_result <- bench::mark(
      plot_with_phases = generateSPCPlot(
        data = phase_data,
        x_col = "Dato",
        y_col = "Tæller",
        n_col = "Nævner",
        chart_type = "p",
        chart_title = "Phase Test",
        skift_config = list(show_phases = TRUE, skift_column = "Skift")
      ),
      iterations = 10,
      check = FALSE
    )

    median_time_ms <- as.numeric(benchmark_result$median) * 1000

    message(sprintf(
      "Plot with phases: %.2f ms (target: <800ms)",
      median_time_ms
    ))

    expect_lt(median_time_ms, 800)
  })

  it("handles multiple chart types efficiently", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    test_data <- create_benchmark_data(n_rows = 100)
    chart_types <- c("p", "c", "u", "i", "run")

    results <- lapply(chart_types, function(chart_type) {
      benchmark_result <- bench::mark(
        plot = generateSPCPlot(
          data = test_data,
          x_col = "Dato",
          y_col = "Tæller",
          n_col = if (chart_type %in% c("p", "u")) "Nævner" else NULL,
          chart_type = chart_type,
          chart_title = paste("Type:", chart_type)
        ),
        iterations = 5,
        check = FALSE
      )

      median_time_ms <- as.numeric(benchmark_result$median) * 1000

      list(
        chart_type = chart_type,
        median_ms = median_time_ms
      )
    })

    # All chart types should generate in <500ms
    for (result in results) {
      message(sprintf(
        "Chart type '%s': %.2f ms",
        result$chart_type,
        result$median_ms
      ))
      expect_lt(result$median_ms, 500)
    }
  })
})

# CACHE PERFORMANCE ============================================================

describe("Cache Efficiency", {

  it("achieves >80% cache hit rate for repeated data", {
    skip_if_not(exists("load_cached_startup_data", mode = "function"))
    skip_if_not(exists("cache_startup_data", mode = "function"))

    # Simulate cache behavior
    test_data <- list(
      hospital_branding = list(name = "Test Hospital", colors = list()),
      observer_priorities = list(HIGH = 10, MEDIUM = 5, LOW = 1),
      chart_types = list(p = "proportion", c = "count")
    )

    # Cache data
    cache_startup_data(test_data)

    # Measure cache hits
    hits <- 0
    misses <- 0
    iterations <- 100

    for (i in 1:iterations) {
      cached <- load_cached_startup_data()
      if (!is.null(cached)) {
        hits <- hits + 1
      } else {
        misses <- misses + 1
      }
    }

    hit_rate <- hits / iterations * 100

    message(sprintf(
      "Cache hit rate: %.1f%% (target: >80%%)",
      hit_rate
    ))

    expect_gt(hit_rate, 80)
  })

  it("cache retrieval is faster than computation", {
    skip_if_not(exists("load_cached_startup_data", mode = "function"))
    skip_if_not(exists("get_hospital_colors", mode = "function"))

    # Benchmark cache retrieval
    cache_time <- bench::mark(
      cached = load_cached_startup_data(),
      iterations = 50,
      check = FALSE
    )

    # Benchmark fresh computation
    compute_time <- bench::mark(
      computed = get_hospital_colors(),
      iterations = 50,
      check = FALSE
    )

    cache_ms <- as.numeric(cache_time$median) * 1000
    compute_ms <- as.numeric(compute_time$median) * 1000
    speedup <- compute_ms / cache_ms

    message(sprintf(
      "Cache: %.2f ms, Compute: %.2f ms, Speedup: %.1fx",
      cache_ms,
      compute_ms,
      speedup
    ))

    expect_lt(cache_ms, compute_ms)
  })

  it("visualization cache updates are atomic", {
    skip("Requires full reactive context for atomic cache testing")
  })
})

# MEMORY USAGE PROFILING =======================================================

describe("Memory Usage", {

  it("baseline app state uses <100MB", {
    skip_if_not(exists("create_app_state", mode = "function"))

    mem_result <- measure_memory_usage({
      app_state <- create_app_state()
      app_state
    })

    message(sprintf(
      "App state memory usage: %.2f MB (target: <100MB)",
      mem_result$memory_mb
    ))

    expect_lt(mem_result$memory_mb, 100)
  })

  it("standard plot generation uses <50MB additional memory", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    test_data <- create_benchmark_data(n_rows = 100)

    mem_result <- measure_memory_usage({
      plot <- generateSPCPlot(
        data = test_data,
        x_col = "Dato",
        y_col = "Tæller",
        n_col = "Nævner",
        chart_type = "p",
        chart_title = "Memory Test"
      )
      plot
    })

    message(sprintf(
      "Plot generation memory: %.2f MB (target: <50MB)",
      mem_result$memory_mb
    ))

    expect_lt(mem_result$memory_mb, 50)
  })

  it("large dataset processing memory scales linearly", {
    skip_if_not(exists("detect_columns_full_analysis", mode = "function"))

    sizes <- c(100, 500, 1000)
    memory_usage <- numeric(length(sizes))

    for (i in seq_along(sizes)) {
      test_data <- create_benchmark_data(n_rows = sizes[i])

      mem_result <- measure_memory_usage({
        result <- detect_columns_full_analysis(test_data)
        result
      })

      memory_usage[i] <- mem_result$memory_mb

      message(sprintf(
        "Auto-detection for %d rows: %.2f MB",
        sizes[i],
        memory_usage[i]
      ))
    }

    # Memory should scale roughly linearly (not exponentially)
    # Ratio of memory for 1000 vs 100 should be ~10x, allow up to 15x
    memory_ratio <- memory_usage[3] / memory_usage[1]
    expect_lt(memory_ratio, 15)
  })

  it("no memory leaks in repeated operations", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    test_data <- create_benchmark_data(n_rows = 100)

    # Measure memory for first operation
    mem_first <- measure_memory_usage({
      plot1 <- generateSPCPlot(
        data = test_data,
        x_col = "Dato",
        y_col = "Tæller",
        n_col = "Nævner",
        chart_type = "p",
        chart_title = "Leak Test 1"
      )
      plot1
    })

    # Force garbage collection
    gc(verbose = FALSE)

    # Measure memory for second operation
    mem_second <- measure_memory_usage({
      plot2 <- generateSPCPlot(
        data = test_data,
        x_col = "Dato",
        y_col = "Tæller",
        n_col = "Nævner",
        chart_type = "p",
        chart_title = "Leak Test 2"
      )
      plot2
    })

    # Memory usage should be similar (allow 20% variation)
    memory_increase <- abs(mem_second$memory_mb - mem_first$memory_mb)
    memory_increase_pct <- memory_increase / mem_first$memory_mb * 100

    message(sprintf(
      "Memory increase: %.2f MB (%.1f%%, target: <20%%)",
      memory_increase,
      memory_increase_pct
    ))

    expect_lt(memory_increase_pct, 20)
  })
})

# REACTIVE CHAIN OVERHEAD ======================================================

describe("Reactive Chain Performance", {

  it("data_loaded → auto_detection chain completes in <100ms", {
    skip("Requires full reactive context with testServer")
  })

  it("auto_detection_completed → ui_sync chain completes in <50ms", {
    skip("Requires full reactive context with testServer")
  })

  it("high priority observers execute before low priority", {
    skip("Requires full reactive context with observer monitoring")
  })

  it("debounced inputs reduce reactive overhead", {
    skip("Requires time-based testing framework")
  })
})

# DATA PROCESSING PERFORMANCE ==================================================

describe("Data Processing Performance", {

  it("processes 1000 rows in <200ms", {
    skip_if_not(exists("detect_columns_full_analysis", mode = "function"))

    large_data <- create_benchmark_data(n_rows = 1000, n_cols = 10)

    benchmark_result <- bench::mark(
      processing = detect_columns_full_analysis(large_data),
      iterations = 10,
      check = FALSE
    )

    median_time_ms <- as.numeric(benchmark_result$median) * 1000

    message(sprintf(
      "Process 1000 rows: %.2f ms (target: <200ms)",
      median_time_ms
    ))

    expect_lt(median_time_ms, 200)
  })

  it("CSV parsing performance scales linearly", {
    skip_if_not(exists("read_csv_auto", mode = "function"))

    # This test requires actual CSV files
    skip("Requires CSV test fixtures")
  })

  it("Danish character handling has minimal overhead", {
    skip_if_not(exists("detect_columns_name_based", mode = "function"))

    # Create data with and without Danish characters
    ascii_cols <- c("Teller", "Nevner", "Dato")
    danish_cols <- c("Tæller", "Nævner", "Dato")

    ascii_time <- bench::mark(
      ascii = detect_columns_name_based(ascii_cols),
      iterations = 100,
      check = FALSE
    )

    danish_time <- bench::mark(
      danish = detect_columns_name_based(danish_cols),
      iterations = 100,
      check = FALSE
    )

    ascii_ms <- as.numeric(ascii_time$median) * 1000
    danish_ms <- as.numeric(danish_time$median) * 1000
    overhead_pct <- (danish_ms - ascii_ms) / ascii_ms * 100

    message(sprintf(
      "Danish character overhead: %.1f%% (target: <10%%)",
      overhead_pct
    ))

    expect_lt(overhead_pct, 10)
  })
})

# AUTO-DETECTION PERFORMANCE ===================================================

describe("Auto-Detection Performance", {

  it("detects columns in standard data in <100ms", {
    skip_if_not(exists("detect_columns_full_analysis", mode = "function"))

    test_data <- create_benchmark_data(n_rows = 100)

    benchmark_result <- bench::mark(
      detection = detect_columns_full_analysis(test_data),
      iterations = 20,
      check = FALSE
    )

    median_time_ms <- as.numeric(benchmark_result$median) * 1000

    message(sprintf(
      "Auto-detection: %.2f ms (target: <100ms)",
      median_time_ms
    ))

    expect_lt(median_time_ms, 100)
  })

  it("handles ambiguous data without performance degradation", {
    skip_if_not(exists("detect_columns_full_analysis", mode = "function"))

    # Create ambiguous data (multiple potential Y columns)
    ambiguous_data <- data.frame(
      Dato = seq.Date(as.Date("2024-01-01"), by = "day", length.out = 100),
      Tæller1 = sample(40:50, 100, replace = TRUE),
      Tæller2 = sample(40:50, 100, replace = TRUE),
      Tæller3 = sample(40:50, 100, replace = TRUE),
      Nævner = rep(50, 100),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    benchmark_result <- bench::mark(
      ambiguous = detect_columns_full_analysis(ambiguous_data),
      iterations = 20,
      check = FALSE
    )

    median_time_ms <- as.numeric(benchmark_result$median) * 1000

    message(sprintf(
      "Ambiguous detection: %.2f ms (target: <150ms)",
      median_time_ms
    ))

    expect_lt(median_time_ms, 150)
  })

  it("frozen state check overhead is negligible", {
    skip_if_not(exists("detect_columns_full_analysis", mode = "function"))

    test_data <- create_benchmark_data(n_rows = 100)

    # Normal detection
    normal_time <- bench::mark(
      normal = detect_columns_full_analysis(test_data),
      iterations = 50,
      check = FALSE
    )

    # With frozen check (simulated)
    frozen_time <- bench::mark(
      frozen = {
        frozen <- FALSE  # Simulate frozen state check
        if (!frozen) detect_columns_full_analysis(test_data)
      },
      iterations = 50,
      check = FALSE
    )

    normal_ms <- as.numeric(normal_time$median) * 1000
    frozen_ms <- as.numeric(frozen_time$median) * 1000
    overhead_pct <- (frozen_ms - normal_ms) / normal_ms * 100

    message(sprintf(
      "Frozen state check overhead: %.1f%% (target: <5%%)",
      overhead_pct
    ))

    expect_lt(abs(overhead_pct), 5)
  })
})

# PERFORMANCE REGRESSION DETECTION =============================================

describe("Performance Regression Detection", {

  it("tracks performance metrics over time", {
    skip("Requires performance metrics storage system")
  })

  it("alerts on >10% performance degradation", {
    skip("Requires baseline performance metrics")
  })

  it("benchmarks are reproducible across runs", {
    skip_if_not(exists("generateSPCPlot", mode = "function"))

    test_data <- create_benchmark_data(n_rows = 100)

    # Run benchmark twice
    run1 <- bench::mark(
      plot = generateSPCPlot(
        data = test_data,
        x_col = "Dato",
        y_col = "Tæller",
        n_col = "Nævner",
        chart_type = "p",
        chart_title = "Reproducibility Test"
      ),
      iterations = 10,
      check = FALSE
    )

    run2 <- bench::mark(
      plot = generateSPCPlot(
        data = test_data,
        x_col = "Dato",
        y_col = "Tæller",
        n_col = "Nævner",
        chart_type = "p",
        chart_title = "Reproducibility Test"
      ),
      iterations = 10,
      check = FALSE
    )

    time1_ms <- as.numeric(run1$median) * 1000
    time2_ms <- as.numeric(run2$median) * 1000
    variance_pct <- abs(time2_ms - time1_ms) / time1_ms * 100

    message(sprintf(
      "Run variance: %.1f%% (target: <15%%)",
      variance_pct
    ))

    # Allow 15% variance between runs
    expect_lt(variance_pct, 15)
  })
})

# SUMMARY REPORT ===============================================================

describe("Performance Summary", {

  it("generates performance report", {
    # This test would aggregate all benchmark results and generate
    # a summary report for CI/CD integration

    skip("Performance reporting infrastructure not yet implemented")
  })
})
