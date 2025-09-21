library(testthat)

# Ensure global variables required by run_app are present to avoid sourcing heavy dependencies
setup_required_globals <- function() {
  vars <- list(
    HOSPITAL_NAME = "Test Hospital",
    my_theme = NULL,
    HOSPITAL_LOGO_PATH = "logo.png"
  )

  original <- list()
  for (nm in names(vars)) {
    if (exists(nm, envir = .GlobalEnv, inherits = FALSE)) {
      original[[nm]] <- get(nm, envir = .GlobalEnv, inherits = FALSE)
    } else {
      original[[nm]] <- NULL
    }
    assign(nm, vars[[nm]], envir = .GlobalEnv)
  }

  original
}

restore_globals <- function(original) {
  for (nm in names(original)) {
    if (is.null(original[[nm]])) {
      if (exists(nm, envir = .GlobalEnv, inherits = FALSE)) {
        rm(list = nm, envir = .GlobalEnv)
      }
    } else {
      assign(nm, original[[nm]], envir = .GlobalEnv)
    }
  }
}

test_that("run_app forwards custom port to runApp", {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    skip("pkgload er ikke installeret i testmiljøet")
  }

  dev_package_fn <- tryCatch(
    get("dev_package", envir = asNamespace("pkgload")),
    error = function(e) NULL
  )
  if (is.null(dev_package_fn)) {
    skip("pkgload::dev_package() er ikke tilgængelig i denne konfiguration")
  }

  pkgload_session <- tryCatch(dev_package_fn(), error = function(e) NULL)
  if (is.null(pkgload_session)) {
    skip("pkgload::dev_package() returnerer NULL - ingen load_all() session aktiv")
  }

  original <- setup_required_globals()
  on.exit(restore_globals(original), add = TRUE)

  captured <- new.env(parent = emptyenv())

  fake_runApp <- function(app, port = NULL, launch.browser = TRUE) {
    captured$app <- app
    captured$port <- port
    captured$launch.browser <- launch.browser
    "runApp-called"
  }

  fake_shinyApp <- function(ui, server, ...) {
    list(ui = ui, server = server, options = list(...))
  }

  fake_set_app_options <- function(options) invisible(options)
  fake_get_app_option <- function(option, default = NULL) NULL
  fake_log_debug <- function(...) invisible(NULL)

  result <- with_mocked_bindings(
    run_app(port = 5050, launch_browser = TRUE),
    runApp = fake_runApp,
    shinyApp = fake_shinyApp,
    set_app_options = fake_set_app_options,
    get_app_option = fake_get_app_option,
    log_debug = fake_log_debug,
    app_ui = function() "ui",
    app_server = function(...) NULL,
    `shiny::addResourcePath` = function(...) NULL,
    `rstudioapi::isAvailable` = function() FALSE
  )

  expect_equal(result, "runApp-called")
  expect_equal(captured$port, 5050)
  expect_true(captured$launch.browser)
})
