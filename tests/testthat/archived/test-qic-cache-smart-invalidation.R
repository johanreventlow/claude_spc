# Smart QIC Cache Invalidation Tests
# Test context-aware cache invalidation strategy

test_that("Cache preserved on cosmetic changes", {
  # Setup mock app_state with cache
  app_state <- list(
    cache = list(
      qic = list(
        clear = function() {
          cache_cleared <<- TRUE
        }
      )
    )
  )

  cache_cleared <- FALSE

  # Trigger cache invalidation with cosmetic context
  update_context <- list(context = "comment_updated")
  invalidate_qic_cache_smart(app_state, update_context)

  # Cache should NOT be cleared
  expect_false(cache_cleared, info = "Cache should be preserved for cosmetic changes")
})


test_that("Cache cleared on structural changes", {
  # Setup mock app_state with cache
  cache_cleared <- FALSE

  app_state <- list(
    cache = list(
      qic = list(
        clear = function() {
          cache_cleared <<- TRUE
        }
      )
    )
  )

  # Test each structural change context
  structural_contexts <- c("upload", "column_added", "column_removed", "load", "new")

  for (ctx in structural_contexts) {
    cache_cleared <- FALSE
    update_context <- list(context = ctx)
    invalidate_qic_cache_smart(app_state, update_context)

    expect_true(cache_cleared,
                info = paste("Cache should be cleared for structural change:", ctx))
  }
})


test_that("Selective invalidation on value changes", {
  # Setup mock app_state with cache
  cache_cleared <- FALSE

  app_state <- list(
    cache = list(
      qic = list(
        clear = function() {
          cache_cleared <<- TRUE
        }
      )
    ),
    columns = list(
      mappings = list(
        chart_type = "i"
      )
    )
  )

  # Test value change contexts (selective invalidation)
  value_contexts <- c("table_cells_edited", "value_change", "edit", "modify")

  for (ctx in value_contexts) {
    cache_cleared <- FALSE
    update_context <- list(context = ctx)
    invalidate_qic_cache_smart(app_state, update_context)

    # For now, selective still clears cache (pattern-based invalidation to be implemented)
    expect_true(cache_cleared,
                info = paste("Cache should be invalidated for value change:", ctx))
  }
})


test_that("Conservative clear on unknown context", {
  # Setup mock app_state with cache
  cache_cleared <- FALSE

  app_state <- list(
    cache = list(
      qic = list(
        clear = function() {
          cache_cleared <<- TRUE
        }
      )
    )
  )

  # Trigger cache invalidation with unknown context
  update_context <- list(context = "unknown_context_xyz")
  invalidate_qic_cache_smart(app_state, update_context)

  # Cache SHOULD be cleared conservatively
  expect_true(cache_cleared, info = "Cache should be cleared conservatively for unknown contexts")
})


test_that("Graceful handling when cache is NULL", {
  # Setup app_state WITHOUT cache
  app_state <- list(
    cache = NULL
  )

  # Should not crash
  expect_silent({
    invalidate_qic_cache_smart(app_state, list(context = "upload"))
  })

  # Setup app_state with cache but no qic
  app_state <- list(
    cache = list(
      qic = NULL
    )
  )

  expect_silent({
    invalidate_qic_cache_smart(app_state, list(context = "upload"))
  })
})


test_that("Context extraction handles different input formats", {
  # Setup mock app_state
  cache_cleared <- FALSE

  app_state <- list(
    cache = list(
      qic = list(
        clear = function() {
          cache_cleared <<- TRUE
        }
      )
    )
  )

  # Test with NULL context (should use fallback "general")
  cache_cleared <- FALSE
  invalidate_qic_cache_smart(app_state, NULL)
  expect_true(cache_cleared, info = "NULL context should trigger conservative clear")

  # Test with character context
  cache_cleared <- FALSE
  invalidate_qic_cache_smart(app_state, list(context = "upload"))
  expect_true(cache_cleared, info = "Character context should work")

  # Test with list context
  cache_cleared <- FALSE
  invalidate_qic_cache_smart(app_state, list(context = "comment_updated", details = list(foo = "bar")))
  expect_false(cache_cleared, info = "List context with details should preserve cache for comments")
})


test_that("Cache hit rate improvement measurable", {
  # Simulate 10 operations with different contexts
  cache_clears <- 0
  cache_preserves <- 0

  app_state <- list(
    cache = list(
      qic = list(
        clear = function() {
          cache_clears <<- cache_clears + 1
        }
      )
    )
  )

  # Realistic usage pattern
  operations <- list(
    list(context = "upload"),           # Clear (structural)
    list(context = "comment_updated"),  # Preserve (cosmetic)
    list(context = "comment_updated"),  # Preserve (cosmetic)
    list(context = "table_cells_edited"), # Clear (value change)
    list(context = "comment_updated"),  # Preserve (cosmetic)
    list(context = "navigation"),       # Preserve (cosmetic)
    list(context = "comment_updated"),  # Preserve (cosmetic)
    list(context = "value_change"),     # Clear (value change)
    list(context = "comment_updated"),  # Preserve (cosmetic)
    list(context = "ui_update")         # Preserve (cosmetic)
  )

  for (op in operations) {
    initial_clears <- cache_clears
    invalidate_qic_cache_smart(app_state, op)

    if (cache_clears == initial_clears) {
      cache_preserves <- cache_preserves + 1
    }
  }

  # Expected: 3 clears (upload, table_cells_edited, value_change)
  #           7 preserves (6x comment_updated, 1x navigation, 1x ui_update)
  expect_equal(cache_clears, 3, info = "Should clear cache 3 times")
  expect_equal(cache_preserves, 7, info = "Should preserve cache 7 times")

  # Cache preservation rate should be 70%
  preservation_rate <- cache_preserves / length(operations)
  expect_gt(preservation_rate, 0.6, info = "Cache preservation rate should be >60%")
})
