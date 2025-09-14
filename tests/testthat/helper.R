# helper.R
# Test setup og fælles funktioner

library(shiny)
library(shinytest2)
library(testthat)

# Source app komponenter for test
# Naviger til projekt rod og load global.R
project_root <- here::here()
if (file.exists(file.path(project_root, "global.R"))) {
  source(file.path(project_root, "global.R"), local = FALSE)
} else if (file.exists("../../global.R")) {
  source("../../global.R", local = FALSE)
}

# Test data setup
create_test_data <- function() {
  data.frame(
    Skift = rep(FALSE, 10),
    Frys = rep(FALSE, 10), 
    Dato = seq.Date(as.Date("2024-01-01"), by = "month", length.out = 10),
    Tæller = c(90, 85, 92, 88, 94, 91, 87, 93, 89, 95),
    Nævner = c(100, 95, 100, 98, 102, 99, 96, 101, 97, 103),
    stringsAsFactors = FALSE
  )
}

# Helper funktion til at vente på app ready state
wait_for_app_ready <- function(app, timeout = 10) {
  Sys.sleep(2) # Basic wait for app initialization
  TRUE
}