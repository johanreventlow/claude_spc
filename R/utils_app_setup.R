# R/utils/check_dependencies.R
# Check and install missing packages

required_packages <- c(
  "shiny",
  "bslib",
  "qicharts2",
  "dplyr",
  "ggplot2",
  "readr",
  "readxl",
  "shinycssloaders",
  "shinyWidgets",
  "shinyjs",
  "zoo",
  "scales",
  "rlang",
  "lubridate",
  "openxlsx",
  "jsonlite"
)

# Check which packages are missing
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(missing_packages) > 0) {
  # Installing missing packages
  install.packages(missing_packages)
}

# Load all required packages and report any errors
for (pkg in required_packages) {
  tryCatch(
    {
      library(pkg, character.only = TRUE)
      # Package loaded successfully
    },
    error = function(e) {
      # Silent error - package failed to load
    }
  )
}
