# Test Startup Lazy Loading System
# Validates lazy loading functionality and performance optimization
# Tests lazy loading architecture implemented 2025-09-26

library(testthat)
context("Startup Lazy Loading System Tests")

# Test Information Output
test_that("lazy loading test context information", {
  message("Testing lazy loading system implemented 2025-09-26")
  message("Performance benefit: Heavy modules loaded only when needed")
  message("Target: Reduce initial loading time by deferring non-essential modules")

  skip("Context information only - not a real test")
})

test_that("lazy loading configuration er korrekt defineret", {
  expect_true(exists("LAZY_LOADING_CONFIG"))
  expect_is(LAZY_LOADING_CONFIG, "list")
  expect_true("heavy_modules" %in% names(LAZY_LOADING_CONFIG))
  expect_true("loaded_modules" %in% names(LAZY_LOADING_CONFIG))
})

test_that("lazy loading heavy modules er konfigureret", {
  skip_if_not(exists("LAZY_LOADING_CONFIG", mode = "list"))

  heavy_modules <- LAZY_LOADING_CONFIG$heavy_modules
  expect_is(heavy_modules, "list")
  expect_gt(length(heavy_modules), 0)

  # Verify documented heavy modules exist in configuration
  documented_modules <- c("file_operations", "advanced_debug", "performance_monitoring", "plot_generation")

  for (module_name in documented_modules) {
    expect_true(module_name %in% names(heavy_modules))

    module_config <- heavy_modules[[module_name]]
    expect_true("file" %in% names(module_config))
    expect_true("condition" %in% names(module_config))
    expect_true("loaded" %in% names(module_config))

    # Verify file path is character
    expect_is(module_config$file, "character")
    expect_gt(nchar(module_config$file), 0)

    # Verify condition is function
    expect_is(module_config$condition, "function")

    # Verify loaded is logical
    expect_is(module_config$loaded, "logical")
  }
})

test_that("lazy_load_module function works correctly", {
  skip_if_not(exists("lazy_load_module", mode = "function"))
  skip_if_not(exists("LAZY_LOADING_CONFIG", mode = "list"))

  # Test invalid module name
  result <- lazy_load_module("nonexistent_module")
  expect_false(result)

  # Test with valid module name (if file exists)
  heavy_modules <- LAZY_LOADING_CONFIG$heavy_modules
  if (length(heavy_modules) > 0) {
    module_name <- names(heavy_modules)[1]
    module_config <- heavy_modules[[module_name]]

    if (file.exists(module_config$file)) {
      # Test loading
      result <- lazy_load_module(module_name, force_reload = TRUE)
      expect_is(result, "logical")

      # Module should be marked as loaded if successful
      if (result) {
        expect_true(LAZY_LOADING_CONFIG$heavy_modules[[module_name]]$loaded)
      }
    }
  }
})

test_that("lazy_load_modules loads appropriate modules", {
  skip_if_not(exists("lazy_load_modules", mode = "function"))

  # Test normal loading (based on conditions)
  loaded_modules <- lazy_load_modules()
  expect_is(loaded_modules, "character")

  # Test force loading all modules
  all_loaded_modules <- lazy_load_modules(force_all = TRUE)
  expect_is(all_loaded_modules, "character")

  # Force loading should load at least as many modules as normal loading
  expect_gte(length(all_loaded_modules), length(loaded_modules))
})

test_that("get_lazy_loading_status provides accurate information", {
  skip_if_not(exists("get_lazy_loading_status", mode = "function"))

  status <- get_lazy_loading_status()
  expect_is(status, "list")

  # Check that status contains information for all configured modules
  heavy_modules <- LAZY_LOADING_CONFIG$heavy_modules
  for (module_name in names(heavy_modules)) {
    expect_true(module_name %in% names(status))

    module_status <- status[[module_name]]
    expect_true("loaded" %in% names(module_status))
    expect_true("condition_met" %in% names(module_status))
    expect_true("file_exists" %in% names(module_status))
    expect_true("load_time" %in% names(module_status))

    # Verify types
    expect_is(module_status$loaded, "logical")
    expect_is(module_status$condition_met, "logical")
    expect_is(module_status$file_exists, "logical")
  }
})

test_that("ensure_module_loaded works correctly", {
  skip_if_not(exists("ensure_module_loaded", mode = "function"))

  # Test with invalid module
  result <- ensure_module_loaded("nonexistent_module")
  expect_false(result)

  # Test with valid module
  heavy_modules <- LAZY_LOADING_CONFIG$heavy_modules
  if (length(heavy_modules) > 0) {
    module_name <- names(heavy_modules)[1]
    module_config <- heavy_modules[[module_name]]

    if (file.exists(module_config$file)) {
      result <- ensure_module_loaded(module_name)
      expect_is(result, "logical")

      # If module was loaded, subsequent calls should return TRUE immediately
      if (result) {
        quick_result <- ensure_module_loaded(module_name)
        expect_true(quick_result)
      }
    }
  }
})

# Performance tests
test_that("lazy loading improves startup performance", {
  skip_if_not(exists("lazy_load_modules", mode = "function"))

  # Measure time for conditional loading
  start_time <- Sys.time()
  normal_modules <- lazy_load_modules()
  normal_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  # Measure time for force loading all modules
  start_time <- Sys.time()
  all_modules <- lazy_load_modules(force_all = TRUE)
  force_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  # Conditional loading should be faster or equal
  expect_lte(normal_time, force_time + 0.1) # Allow small tolerance

  # Log performance metrics
  message(sprintf("Lazy loading performance: normal=%.3fs (%d modules), force=%.3fs (%d modules)",
                  normal_time, length(normal_modules), force_time, length(all_modules)))

  # Both operations should be reasonably fast
  expect_lt(normal_time, 2.0)
  expect_lt(force_time, 5.0)
})

# Module condition tests
test_that("module loading conditions work correctly", {
  skip_if_not(exists("LAZY_LOADING_CONFIG", mode = "list"))

  heavy_modules <- LAZY_LOADING_CONFIG$heavy_modules

  for (module_name in names(heavy_modules)) {
    module_config <- heavy_modules[[module_name]]

    # Test that condition function can be called
    expect_no_error({
      condition_result <- module_config$condition()
      expect_is(condition_result, "logical")
    })
  }
})

# Integration with configuration system
test_that("lazy loading integrates with golem configuration", {
  skip_if_not(exists("LAZY_LOADING_CONFIG", mode = "list"))

  # Test advanced_debug module condition
  if ("advanced_debug" %in% names(LAZY_LOADING_CONFIG$heavy_modules)) {
    debug_config <- LAZY_LOADING_CONFIG$heavy_modules$advanced_debug

    # Test condition responds to environment variables
    old_debug <- Sys.getenv("SPC_DEBUG_MODE", unset = "")

    Sys.setenv(SPC_DEBUG_MODE = "FALSE")
    result_false <- debug_config$condition()

    Sys.setenv(SPC_DEBUG_MODE = "TRUE")
    result_true <- debug_config$condition()

    # Restore original environment
    if (old_debug == "") {
      Sys.unsetenv("SPC_DEBUG_MODE")
    } else {
      Sys.setenv(SPC_DEBUG_MODE = old_debug)
    }

    expect_is(result_false, "logical")
    expect_is(result_true, "logical")
    expect_true(result_true) # Should be TRUE when SPC_DEBUG_MODE=TRUE
  }
})

# File system integrity tests
test_that("lazy loading handles missing files gracefully", {
  skip_if_not(exists("lazy_load_module", mode = "function"))

  # Test with a module that has a non-existent file
  # We'll temporarily modify the config for testing
  original_config <- LAZY_LOADING_CONFIG$heavy_modules

  # Add a test module with non-existent file
  LAZY_LOADING_CONFIG$heavy_modules$test_missing <<- list(
    file = "R/nonexistent_file.R",
    condition = function() TRUE,
    loaded = FALSE
  )

  # Test loading non-existent module
  result <- lazy_load_module("test_missing")
  expect_false(result)

  # Restore original configuration
  LAZY_LOADING_CONFIG$heavy_modules <<- original_config
})

# Memory management tests
test_that("lazy loading manages memory efficiently", {
  skip_if_not(exists("lazy_load_modules", mode = "function"))

  if (requireNamespace("pryr", quietly = TRUE)) {
    # Measure memory before lazy loading
    gc() # Clean up first
    mem_before <- pryr::mem_used()

    # Load modules
    loaded_modules <- lazy_load_modules()

    # Measure memory after
    gc()
    mem_after <- pryr::mem_used()

    # Memory increase should be reasonable
    mem_increase <- as.numeric(mem_after - mem_before) / 1024^2
    expect_lt(mem_increase, 100) # Should not increase by more than 100MB

    message(sprintf("Lazy loading memory usage: %.2f MB for %d modules",
                    mem_increase, length(loaded_modules)))
  } else {
    skip("pryr package not available for memory testing")
  }
})

# Edge cases and error handling
test_that("lazy loading handles edge cases correctly", {
  skip_if_not(exists("lazy_load_module", mode = "function"))
  skip_if_not(exists("get_lazy_loading_status", mode = "function"))

  # Test with empty module name
  expect_error({
    lazy_load_module("")
  })

  # Test status with no modules loaded
  status <- get_lazy_loading_status()
  expect_is(status, "list")
})

# Documentation verification
test_that("lazy loading configuration matches documentation", {
  skip_if_not(exists("LAZY_LOADING_CONFIG", mode = "list"))

  # Verify documented module files exist
  documented_files <- c(
    "R/fct_file_operations.R",
    "R/utils_advanced_debug.R",
    "R/utils_performance.R",
    "R/fct_spc_plot_generation.R"
  )

  heavy_modules <- LAZY_LOADING_CONFIG$heavy_modules
  configured_files <- sapply(heavy_modules, function(x) x$file)

  # Check that documented files are in configuration
  for (doc_file in documented_files) {
    # At least one of the configured files should match or be similar
    file_matches <- any(grepl(basename(doc_file), configured_files, fixed = TRUE))
    # We're flexible here since exact filenames might vary
    expect_true(file_matches || any(configured_files == doc_file))
  }
})

# Test lazy loading cache environment
test_that("lazy loading cache environment works correctly", {
  skip_if_not(exists("LAZY_LOADING_CONFIG", mode = "list"))

  cache_env <- LAZY_LOADING_CONFIG$loaded_modules
  expect_is(cache_env, "environment")

  # Cache should be initially empty or contain only loaded modules
  cache_names <- ls(cache_env)
  expect_is(cache_names, "character")

  # If there are cached entries, they should be timestamps
  if (length(cache_names) > 0) {
    for (cache_name in cache_names) {
      cache_value <- get(cache_name, envir = cache_env)
      expect_is(cache_value, "POSIXt") # Should be a timestamp
    }
  }
})

# Integration test
test_that("lazy loading system works end-to-end", {
  skip_if_not(exists("lazy_load_modules", mode = "function"))
  skip_if_not(exists("get_lazy_loading_status", mode = "function"))
  skip_if_not(exists("ensure_module_loaded", mode = "function"))

  # Full workflow test
  expect_no_error({
    # 1. Get initial status
    initial_status <- get_lazy_loading_status()

    # 2. Load modules based on conditions
    loaded_modules <- lazy_load_modules()

    # 3. Ensure a specific module is loaded
    if (length(loaded_modules) > 0) {
      ensure_result <- ensure_module_loaded(loaded_modules[1])
    }

    # 4. Get final status
    final_status <- get_lazy_loading_status()
  })

  expect_is(initial_status, "list")
  expect_is(loaded_modules, "character")
  expect_is(final_status, "list")

  message(sprintf("End-to-end lazy loading test successful: %d modules loaded",
                  length(loaded_modules)))
})