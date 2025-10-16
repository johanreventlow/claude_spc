test_that("unlock_cache_statistics respekterer injiceret namespace og undgår global lookup", {
  fake_ns <- new.env(parent = emptyenv())

  assign(".panel_cache_stats", list(dummy = TRUE), envir = fake_ns)
  assign(".grob_cache_stats", list(dummy = TRUE), envir = fake_ns)
  assign(".panel_cache_config", list(dummy = TRUE), envir = fake_ns)
  assign(".grob_cache_config", list(dummy = TRUE), envir = fake_ns)

  lockBinding(".panel_cache_stats", fake_ns)
  lockBinding(".grob_cache_stats", fake_ns)
  lockBinding(".panel_cache_config", fake_ns)
  lockBinding(".grob_cache_config", fake_ns)

  expect_true(bindingIsLocked(".panel_cache_stats", fake_ns))
  expect_true(bindingIsLocked(".grob_cache_stats", fake_ns))
  expect_true(bindingIsLocked(".panel_cache_config", fake_ns))
  expect_true(bindingIsLocked(".grob_cache_config", fake_ns))

  base_env <- baseenv()
  original_asNamespace <- base::asNamespace
  original_getNamespace <- base::getNamespace

  unlockBinding("asNamespace", base_env)
  unlockBinding("getNamespace", base_env)

  on.exit({
    assign("asNamespace", original_asNamespace, envir = base_env)
    assign("getNamespace", original_getNamespace, envir = base_env)
    lockBinding("asNamespace", base_env)
    lockBinding("getNamespace", base_env)
  }, add = TRUE)

  assign(
    "asNamespace",
    function(name, ...) {
      if (identical(name, "SPCify")) {
        stop("asNamespace må ikke blive kaldt for SPCify")
      }
      original_asNamespace(name, ...)
    },
    envir = base_env
  )
  assign(
    "getNamespace",
    function(name, ...) {
      if (identical(name, "SPCify")) {
        stop("getNamespace må ikke blive kaldt for SPCify")
      }
      original_getNamespace(name, ...)
    },
    envir = base_env
  )

  expect_no_error(
    suppressMessages(
      SPCify:::unlock_cache_statistics(ns = fake_ns)
    )
  )

  expect_false(bindingIsLocked(".panel_cache_stats", fake_ns))
  expect_false(bindingIsLocked(".grob_cache_stats", fake_ns))
  expect_false(bindingIsLocked(".panel_cache_config", fake_ns))
  expect_false(bindingIsLocked(".grob_cache_config", fake_ns))
})
