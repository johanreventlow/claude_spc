# testthat.R
# Test runner for SPC App

library(testthat)

# Conditional loading af shinytest2 for at undgå ubetingede dependencies
if (requireNamespace("shinytest2", quietly = TRUE)) {
  library(shinytest2)
}

# Test directory
test_check("claude_spc")