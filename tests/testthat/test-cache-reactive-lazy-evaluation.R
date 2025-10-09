# Tests for Bug #3: create_cached_reactive Eager Evaluation
# Problem: reactive_expr is evaluated immediately when function is called,
# not when the returned reactive invalidates

library(testthat)
library(shiny)

# Bug #3: Eager evaluation prevents reactive invalidation ----

test_that("create_cached_reactive evaluates lazily, not eagerly", {
  skip_if_not(exists("create_cached_reactive"),
              "create_cached_reactive not available")

  testServer(expr = {
    # SETUP: Create reactive dependency
    counter <- reactiveVal(0)
    evaluation_count <- 0

    # Create cached reactive with dependency on counter
    cached <- create_cached_reactive(
      reactive_expr = {
        evaluation_count <<- evaluation_count + 1
        counter() * 2
      },
      cache_key = "test_lazy",
      cache_timeout = 1  # Short timeout for testing
    )

    # VERIFY: Expression should NOT have been evaluated yet
    # (This is the bug - it currently DOES evaluate)
    expect_equal(evaluation_count, 0,
                 info = "Expression should not evaluate until reactive is accessed")

    # Access the reactive
    result1 <- cached()

    # NOW it should have evaluated
    expect_equal(evaluation_count, 1,
                 info = "Should evaluate when accessed")
    expect_equal(result1, 0, info = "counter() is 0, so result is 0")

    # Change dependency
    counter(5)

    # Force reactive flush
    flush()

    # Access again - should re-evaluate due to invalidation
    result2 <- cached()

    expect_equal(evaluation_count, 2,
                 info = "Should re-evaluate when dependency changes")
    expect_equal(result2, 10, info = "counter() is 5, so result is 10")
  })
})

test_that("create_cached_reactive responds to reactive dependencies", {
  skip_if_not(exists("create_cached_reactive"),
              "create_cached_reactive not available")

  testServer(expr = {
    # SETUP: Multiple reactive dependencies
    val1 <- reactiveVal(1)
    val2 <- reactiveVal(10)

    # Create cached reactive depending on both
    cached <- create_cached_reactive(
      reactive_expr = {
        val1() + val2()
      },
      cache_key = "test_deps",
      cache_timeout = 5
    )

    # Initial access
    result1 <- cached()
    expect_equal(result1, 11)  # 1 + 10

    # Change val1
    val1(5)
    flush()

    result2 <- cached()
    expect_equal(result2, 15, info = "Should reflect val1 change")  # 5 + 10

    # Change val2
    val2(20)
    flush()

    result3 <- cached()
    expect_equal(result3, 25, info = "Should reflect val2 change")  # 5 + 20
  })
})

test_that("create_cached_reactive caches within timeout period", {
  skip_if_not(exists("create_cached_reactive"),
              "create_cached_reactive not available")

  testServer(expr = {
    # SETUP
    counter <- reactiveVal(0)
    eval_count <- 0

    cached <- create_cached_reactive(
      reactive_expr = {
        eval_count <<- eval_count + 1
        counter() * 3
      },
      cache_key = "test_caching",
      cache_timeout = 10  # 10 second timeout
    )

    # First access
    result1 <- cached()
    expect_equal(eval_count, 1)
    expect_equal(result1, 0)

    # Second access immediately (should use cache)
    result2 <- cached()
    expect_equal(eval_count, 1, info = "Should use cached value")
    expect_equal(result2, 0)

    # Change dependency and flush
    counter(7)
    flush()

    # Third access (should invalidate and recompute)
    result3 <- cached()
    expect_equal(eval_count, 2, info = "Should recompute after invalidation")
    expect_equal(result3, 21)  # 7 * 3
  })
})

test_that("create_cached_reactive handles cache expiration", {
  skip_if_not(exists("create_cached_reactive"),
              "create_cached_reactive not available")

  testServer(expr = {
    counter <- reactiveVal(1)
    eval_count <- 0

    # Very short cache timeout
    cached <- create_cached_reactive(
      reactive_expr = {
        eval_count <<- eval_count + 1
        counter() + 100
      },
      cache_key = "test_expiry",
      cache_timeout = 0.1  # 100ms timeout
    )

    # First access
    result1 <- cached()
    expect_equal(eval_count, 1)
    expect_equal(result1, 101)

    # Wait for cache to expire
    Sys.sleep(0.2)

    # Second access (cache expired, should recompute)
    result2 <- cached()
    expect_equal(eval_count, 2, info = "Should recompute after cache expiry")
    expect_equal(result2, 101)
  })
})

test_that("create_cached_reactive works with complex reactive expressions", {
  skip_if_not(exists("create_cached_reactive"),
              "create_cached_reactive not available")

  testServer(expr = {
    # SETUP: Simulate complex data processing
    data_val <- reactiveVal(data.frame(x = 1:5, y = 6:10))

    cached_processing <- create_cached_reactive(
      reactive_expr = {
        df <- data_val()
        df$z <- df$x + df$y
        sum(df$z)
      },
      cache_key = "complex_expr",
      cache_timeout = 5
    )

    # Initial computation
    result1 <- cached_processing()
    expect_equal(result1, sum(c(7, 9, 11, 13, 15)))  # 55

    # Change data
    data_val(data.frame(x = 1:3, y = 10:12))
    flush()

    # Should recompute with new data
    result2 <- cached_processing()
    expect_equal(result2, sum(c(11, 13, 15)))  # 39
  })
})

# Edge case: Function vs expression ----

test_that("create_cached_reactive handles both functions and expressions", {
  skip_if_not(exists("create_cached_reactive"),
              "create_cached_reactive not available")

  testServer(expr = {
    val <- reactiveVal(5)

    # Test with function
    cached_fn <- create_cached_reactive(
      reactive_expr = function() val() * 2,
      cache_key = "test_function",
      cache_timeout = 5
    )

    result_fn <- cached_fn()
    expect_equal(result_fn, 10)

    # Test with expression (quoted)
    cached_expr <- create_cached_reactive(
      reactive_expr = { val() * 3 },
      cache_key = "test_expression",
      cache_timeout = 5
    )

    result_expr <- cached_expr()
    expect_equal(result_expr, 15)
  })
})

# Performance verification ----

test_that("create_cached_reactive provides performance benefit", {
  skip_if_not(exists("create_cached_reactive"),
              "create_cached_reactive not available")
  skip_on_ci()

  testServer(expr = {
    # Expensive computation
    compute_count <- 0

    expensive_cached <- create_cached_reactive(
      reactive_expr = {
        compute_count <<- compute_count + 1
        Sys.sleep(0.01)  # Simulate expensive operation
        runif(100)
      },
      cache_key = "expensive",
      cache_timeout = 10
    )

    # First call - will compute
    start1 <- Sys.time()
    result1 <- expensive_cached()
    time1 <- as.numeric(Sys.time() - start1)
    expect_equal(compute_count, 1)
    expect_gte(time1, 0.01, info = "First call should take time")

    # Second call - should use cache (fast)
    start2 <- Sys.time()
    result2 <- expensive_cached()
    time2 <- as.numeric(Sys.time() - start2)
    expect_equal(compute_count, 1, info = "Should not recompute")
    expect_lt(time2, 0.005, info = "Cached call should be very fast")

    # Results should be identical
    expect_identical(result1, result2)
  })
})
