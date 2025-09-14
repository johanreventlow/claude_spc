# test-app-basic.R
# Grundlæggende tests af SPC app funktionalitet

test_that("App kan starte uden fejl", {
  skip_on_ci() # Skip på CI servere uden browser
  
  # Midlertidig deaktivering af TEST_MODE for controlled testing
  old_test_mode <- TEST_MODE_AUTO_LOAD
  assign("TEST_MODE_AUTO_LOAD", FALSE, envir = .GlobalEnv)
  
  app <- AppDriver$new(
    app_dir = "../..", # Peg til rod af project
    name = "spc-app-basic",
    timeout = 15000,
    load_timeout = 10000
  )
  
  # Test at app loader
  expect_true(app$get_url() != "")
  
  # Test at hovedelementer er til stede i page_navbar struktur
  page_html <- app$get_html("body")
  expect_true(nchar(page_html) > 100)  # Page har indhold
  
  # Test at navbar eller vigtige komponenter er til stede
  expect_true(
    grepl("navbar|SPC|Upload|indicator", page_html, ignore.case = TRUE)
  )
  
  # Gendan TEST_MODE
  assign("TEST_MODE_AUTO_LOAD", old_test_mode, envir = .GlobalEnv)
  
  app$stop()
})

test_that("Velkomstside vises korrekt", {
  skip_on_ci()
  
  old_test_mode <- TEST_MODE_AUTO_LOAD
  assign("TEST_MODE_AUTO_LOAD", FALSE, envir = .GlobalEnv)
  
  app <- AppDriver$new(
    app_dir = "../..",
    name = "spc-welcome-test",
    timeout = 15000
  )
  
  # Test at velkomstbesked eller upload område er synligt
  page_content <- app$get_html("body")
  expect_true(
    grepl("SPC|Upload|Velkommen", page_content, ignore.case = TRUE)
  )
  
  assign("TEST_MODE_AUTO_LOAD", old_test_mode, envir = .GlobalEnv)
  app$stop()
})