# ==============================================================================
# PHASE 1: QIC PLOT REPRODUCIBILITY - ROOT CAUSE INVESTIGATION
# ==============================================================================
#
# Formål: Identificer kilden til non-deterministisk plot generation.
# Phase 1 diagnostic viste at pure grob measurements ER deterministiske,
# så problemet må ligge i qicharts2::qic() eller ggplot2::ggplot_build().
#
# Hypoteser at teste:
# 1. Random seed påvirker plot generation (jitter, sampling, etc.)
# 2. ggplot_build() proces har floating point variance
# 3. qicharts2 bruger random elements internt
#
# ==============================================================================

library(testthat)

# ==============================================================================
# TEST 1: REPRODUCIBILITY MED set.seed()
# ==============================================================================

test_that("qic plots er reproducerbare med set.seed()", {
  skip_if_not_installed("qicharts2")
  skip_if_not_installed("digest")

  # Test data
  test_data <- data.frame(
    x = 1:20,
    y = c(10, 12, 11, 13, 15, 14, 16, 18, 17, 19,
          20, 22, 21, 23, 25, 24, 26, 28, 27, 29)
  )

  # Generate plot 10 times with samme seed
  cat("\n=== TESTING WITH set.seed() ===\n")
  hashes_with_seed <- replicate(10, {
    set.seed(12345)  # Fixed seed
    p <- qicharts2::qic(x = x, y = y, data = test_data, chart = "run")
    gt <- ggplot2::ggplot_build(p)

    # Hash the plot structure
    hash <- digest::digest(list(
      data = gt$data,
      layout = gt$layout
    ), algo = "xxhash64")

    cat(sprintf("Hash: %s\n", hash))
    hash
  })

  unique_hashes_seed <- unique(hashes_with_seed)
  cat(sprintf("Unique hashes with seed: %d\n", length(unique_hashes_seed)))

  # Generate plot 10 times WITHOUT seed
  cat("\n=== TESTING WITHOUT set.seed() ===\n")
  hashes_no_seed <- replicate(10, {
    # No set.seed() call
    p <- qicharts2::qic(x = x, y = y, data = test_data, chart = "run")
    gt <- ggplot2::ggplot_build(p)

    hash <- digest::digest(list(
      data = gt$data,
      layout = gt$layout
    ), algo = "xxhash64")

    cat(sprintf("Hash: %s\n", hash))
    hash
  })

  unique_hashes_no_seed <- unique(hashes_no_seed)
  cat(sprintf("Unique hashes without seed: %d\n", length(unique_hashes_no_seed)))

  # Test: Med seed skal være deterministisk
  expect_length(unique_hashes_seed, 1,
                label = "With set.seed(), plots should be identical")

  # Report: Uden seed - hvor mange unikke?
  cat(sprintf("\n✓ With seed: %d unique (target: 1)\n", length(unique_hashes_seed)))
  cat(sprintf("  Without seed: %d unique (diagnosing variance)\n", length(unique_hashes_no_seed)))
})

# ==============================================================================
# TEST 2: LAYER-SPECIFIC REPRODUCIBILITY
# ==============================================================================

test_that("ggplot layers er individuelt reproducerbare", {
  skip_if_not_installed("qicharts2")

  test_data <- data.frame(
    x = 1:20,
    y = c(10, 12, 11, 13, 15, 14, 16, 18, 17, 19,
          20, 22, 21, 23, 25, 24, 26, 28, 27, 29)
  )

  # Generate plot once
  set.seed(12345)
  p <- qicharts2::qic(x = x, y = y, data = test_data, chart = "run")
  gt <- ggplot2::ggplot_build(p)

  cat("\n=== ANALYZING PLOT LAYERS ===\n")
  cat(sprintf("Number of layers: %d\n", length(gt$data)))

  # Analyze each layer
  for (i in seq_along(gt$data)) {
    layer_data <- gt$data[[i]]
    cat(sprintf("\nLayer %d:\n", i))
    cat(sprintf("  Columns: %s\n", paste(names(layer_data), collapse = ", ")))
    cat(sprintf("  Rows: %d\n", nrow(layer_data)))

    # Check for random-looking columns
    numeric_cols <- names(layer_data)[sapply(layer_data, is.numeric)]
    for (col in numeric_cols) {
      vals <- layer_data[[col]]
      if (length(unique(vals)) > 1) {
        cat(sprintf("  %s: min=%.6f max=%.6f range=%.6f\n",
                    col, min(vals, na.rm = TRUE), max(vals, na.rm = TRUE),
                    max(vals, na.rm = TRUE) - min(vals, na.rm = TRUE)))
      }
    }
  }

  # Test layer data reproducibility
  set.seed(12345)
  p2 <- qicharts2::qic(x = x, y = y, data = test_data, chart = "run")
  gt2 <- ggplot2::ggplot_build(p2)

  # Compare layer data
  for (i in seq_along(gt$data)) {
    layer1 <- gt$data[[i]]
    layer2 <- gt2$data[[i]]

    # Check if layers are identical
    identical_check <- identical(layer1, layer2)

    if (!identical_check) {
      # Find differences
      cat(sprintf("\nLayer %d: NOT identical - investigating...\n", i))
      for (col in names(layer1)) {
        if (!identical(layer1[[col]], layer2[[col]])) {
          cat(sprintf("  Column '%s' differs\n", col))
          if (is.numeric(layer1[[col]])) {
            diff <- abs(layer1[[col]] - layer2[[col]])
            max_diff <- max(diff, na.rm = TRUE)
            cat(sprintf("    Max difference: %.10f\n", max_diff))
          }
        }
      }
    }
  }

  expect_true(TRUE)  # Always pass, this is diagnostic
})

# ==============================================================================
# TEST 3: ISOLATE ggplot_build() VARIANCE
# ==============================================================================

test_that("ggplot_build() er deterministisk for simple plots", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("digest")

  # Create simple ggplot (without qicharts2)
  test_data <- data.frame(x = 1:10, y = 1:10)

  cat("\n=== TESTING PURE ggplot_build() ===\n")
  hashes <- replicate(10, {
    p <- ggplot2::ggplot(test_data, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point() +
      ggplot2::geom_line()

    gt <- ggplot2::ggplot_build(p)

    hash <- digest::digest(list(
      data = gt$data,
      layout = gt$layout
    ), algo = "xxhash64")

    cat(sprintf("Hash: %s\n", hash))
    hash
  })

  unique_hashes <- unique(hashes)
  cat(sprintf("\nUnique hashes: %d (target: 1)\n", length(unique_hashes)))

  expect_length(unique_hashes, 1,
                label = "Pure ggplot_build() should be deterministic")
})

# ==============================================================================
# TEST 4: IDENTIFY RANDOM ELEMENTS IN qicharts2
# ==============================================================================

test_that("qicharts2 internals kan identificeres", {
  skip_if_not_installed("qicharts2")

  test_data <- data.frame(
    x = 1:20,
    y = c(10, 12, 11, 13, 15, 14, 16, 18, 17, 19,
          20, 22, 21, 23, 25, 24, 26, 28, 27, 29)
  )

  # Generate plot and inspect structure
  set.seed(12345)
  p <- qicharts2::qic(x = x, y = y, data = test_data, chart = "run")

  cat("\n=== INSPECTING QIC PLOT STRUCTURE ===\n")

  # Check plot class
  cat(sprintf("Plot class: %s\n", paste(class(p), collapse = ", ")))

  # Check layers
  cat(sprintf("Number of layers: %d\n", length(p$layers)))

  for (i in seq_along(p$layers)) {
    layer <- p$layers[[i]]
    geom_name <- class(layer$geom)[1]
    stat_name <- class(layer$stat)[1]
    position_name <- class(layer$position)[1]

    cat(sprintf("\nLayer %d:\n", i))
    cat(sprintf("  Geom: %s\n", geom_name))
    cat(sprintf("  Stat: %s\n", stat_name))
    cat(sprintf("  Position: %s\n", position_name))

    # Check for jitter or other position adjustments
    if (grepl("jitter|dodge|stack", tolower(position_name))) {
      cat(sprintf("  ⚠️ Position adjustment detected: %s\n", position_name))
    }
  }

  # Check theme
  if (!is.null(p$theme)) {
    cat("\nTheme elements present\n")
  }

  expect_true(TRUE)  # Diagnostic test
})
