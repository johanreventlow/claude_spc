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
  log_info("Installing missing packages:", paste(missing_packages, collapse = ", "), "PACKAGE_SETUP")
  install.packages(missing_packages)
} else {
  log_info("All required packages are already installed.", "PACKAGE_SETUP")
}

# Load all required packages and report any errors
for (pkg in required_packages) {
  tryCatch(
    {
      library(pkg, character.only = TRUE)
      log_info("✓ Loaded", pkg, "PACKAGE_SETUP")
    },
    error = function(e) {
      # Silent error - package failed to load
    }
  )
}
