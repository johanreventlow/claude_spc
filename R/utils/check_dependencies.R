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
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages)
} else {
  cat("All required packages are already installed.\n")
}

# Load all required packages and report any errors
for (pkg in required_packages) {
  tryCatch({
    library(pkg, character.only = TRUE)
    cat("✓ Loaded", pkg, "\n")
  }, error = function(e) {
    cat("✗ Failed to load", pkg, ":", e$message, "\n")
  })
}