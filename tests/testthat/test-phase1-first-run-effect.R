# ==============================================================================
# PHASE 1: FIRST-RUN EFFECT INVESTIGATION
# ==============================================================================
#
# FUND: qicharts2 plots har en "first-run effect" hvor første generation
# giver et andet output end efterfølgende runs, selv med set.seed().
#
# Hypotese: Noget package-level state bliver initialiseret ved første kald.
#
# ==============================================================================

library(testthat)

test_that("First-run effect kan isoleres og elimineres", {
  skip_if_not_installed("qicharts2")
  skip_if_not_installed("digest")

  test_data <- data.frame(
    x = 1:20,
    y = c(10, 12, 11, 13, 15, 14, 16, 18, 17, 19,
          20, 22, 21, 23, 25, 24, 26, 28, 27, 29)
  )

  cat("\n=== TESTING FIRST-RUN EFFECT ===\n")

  # Generate 5 plots in sequence with same seed
  hashes <- character(5)
  for (i in 1:5) {
    set.seed(12345)
    p <- qicharts2::qic(x = x, y = y, data = test_data, chart = "run")
    gt <- ggplot2::ggplot_build(p)

    hash <- digest::digest(list(
      data = gt$data,
      layout = gt$layout
    ), algo = "xxhash64")

    hashes[i] <- hash
    cat(sprintf("Run %d: %s\n", i, hash))
  }

  # Analyze pattern
  unique_hashes <- unique(hashes)
  cat(sprintf("\nTotal unique hashes: %d\n", length(unique_hashes)))

  # Check if first differs from rest
  if (length(unique_hashes) == 2) {
    first_hash <- hashes[1]
    rest_hash <- unique(hashes[-1])

    if (length(rest_hash) == 1 && first_hash != rest_hash) {
      cat("✓ CONFIRMED: First-run effect detected!\n")
      cat(sprintf("  First run: %s\n", first_hash))
      cat(sprintf("  Rest: %s\n", rest_hash))
    }
  } else if (length(unique_hashes) == 1) {
    cat("✓ All runs identical - no first-run effect\n")
  } else {
    cat("✗ Complex variance pattern - needs deeper investigation\n")
    cat(sprintf("  Hashes: %s\n", paste(hashes, collapse = ", ")))
  }

  # WORKAROUND: Warm-up run
  cat("\n=== TESTING WARM-UP WORKAROUND ===\n")
  cat("Running warm-up qic() call...\n")

  # Warm-up call (discard result)
  set.seed(99999)
  warmup <- qicharts2::qic(x = x, y = y, data = test_data, chart = "run")

  cat("Warm-up complete. Now testing reproducibility...\n\n")

  # Now test reproducibility after warm-up
  hashes_after_warmup <- replicate(5, {
    set.seed(12345)
    p <- qicharts2::qic(x = x, y = y, data = test_data, chart = "run")
    gt <- ggplot2::ggplot_build(p)

    hash <- digest::digest(list(
      data = gt$data,
      layout = gt$layout
    ), algo = "xxhash64")

    cat(sprintf("Hash: %s\n", hash))
    hash
  })

  unique_after_warmup <- unique(hashes_after_warmup)
  cat(sprintf("\nUnique hashes after warm-up: %d (target: 1)\n", length(unique_after_warmup)))

  # Test passes if warm-up fixes it
  expect_length(unique_after_warmup, 1,
                label = "After warm-up, all plots should be identical")
})

test_that("First-run difference kan karakteriseres", {
  skip_if_not_installed("qicharts2")

  test_data <- data.frame(
    x = 1:10,
    y = 1:10
  )

  cat("\n=== COMPARING FIRST VS SECOND RUN ===\n")

  # First run
  set.seed(12345)
  p1 <- qicharts2::qic(x = x, y = y, data = test_data, chart = "run")
  gt1 <- ggplot2::ggplot_build(p1)

  # Second run (same seed)
  set.seed(12345)
  p2 <- qicharts2::qic(x = x, y = y, data = test_data, chart = "run")
  gt2 <- ggplot2::ggplot_build(p2)

  # Compare layer by layer
  cat(sprintf("Comparing %d layers\n", length(gt1$data)))

  differences_found <- FALSE

  for (i in seq_along(gt1$data)) {
    layer1 <- gt1$data[[i]]
    layer2 <- gt2$data[[i]]

    if (!identical(layer1, layer2)) {
      differences_found <- TRUE
      cat(sprintf("\n✗ Layer %d differs:\n", i))

      # Find which columns differ
      for (col in names(layer1)) {
        if (!identical(layer1[[col]], layer2[[col]])) {
          cat(sprintf("  Column '%s':\n", col))

          if (is.numeric(layer1[[col]])) {
            diff_vals <- abs(layer1[[col]] - layer2[[col]])
            max_diff <- max(diff_vals, na.rm = TRUE)
            mean_diff <- mean(diff_vals, na.rm = TRUE)

            cat(sprintf("    Max diff: %.10f\n", max_diff))
            cat(sprintf("    Mean diff: %.10f\n", mean_diff))

            # Sample differences
            non_zero <- which(diff_vals > 1e-10)
            if (length(non_zero) > 0 && length(non_zero) <= 5) {
              cat(sprintf("    Differing indices: %s\n", paste(non_zero, collapse = ", ")))
              for (idx in head(non_zero, 3)) {
                cat(sprintf("      [%d]: %.10f vs %.10f\n",
                           idx, layer1[[col]][idx], layer2[[col]][idx]))
              }
            }
          } else {
            # Non-numeric differences
            diff_idx <- which(layer1[[col]] != layer2[[col]])
            if (length(diff_idx) > 0 && length(diff_idx) <= 5) {
              cat(sprintf("    Differing indices: %s\n", paste(diff_idx, collapse = ", ")))
            }
          }
        }
      }
    } else {
      cat(sprintf("✓ Layer %d: identical\n", i))
    }
  }

  if (!differences_found) {
    cat("\n✓ All layers identical between first and second run\n")
    cat("  (First-run effect may have been eliminated by previous test)\n")
  }

  expect_true(TRUE)  # Diagnostic test
})
