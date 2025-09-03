# run_app_safely.R - Safe app startup with error checking

# 1. Check dependencies first
cat("=== Checking Dependencies ===\n")
source("R/utils/check_dependencies.R")

# 2. Check file structure
cat("\n=== Checking File Structure ===\n")
required_files <- c(
  "global.R",
  "app.R", 
  "R/modules/data_module.R",
  "R/modules/visualization_module.R",
  "R/modules/local_storage_module.R",
  "www/Logo_Bispebjerg_og Frederiksberg_RGB_neg.png"
)

all_files_exist <- TRUE
for (file in required_files) {
  if (file.exists(file)) {
    cat("✓ Found", file, "\n")
  } else {
    cat("✗ Missing", file, "\n")
    all_files_exist <- FALSE
  }
}

if (!all_files_exist) {
  stop("Missing required files - please check your file structure!")
}

# 3. Test global.R loading
cat("\n=== Testing Global.R ===\n")
tryCatch({
  source("global.R")
  cat("✓ Global.R loaded successfully\n")
}, error = function(e) {
  cat("✗ Error loading Global.R:", e$message, "\n")
  stop("Cannot proceed without Global.R")
})

# 4. Test modules loading
cat("\n=== Testing Modules ===\n")
modules <- c(
  "R/modules/data_module.R",
  "R/modules/visualization_module.R", 
  "R/modules/local_storage_module.R"
)

for (module in modules) {
  tryCatch({
    source(module)
    cat("✓ Loaded", module, "\n")
  }, error = function(e) {
    cat("✗ Error loading", module, ":", e$message, "\n")
  })
}

# 5. Start app
cat("\n=== Starting Shiny App ===\n")
cat("If app fails to start, check the R console for error messages.\n")
cat("Common issues:\n")
cat("  - Missing packages (install with install.packages())\n")
cat("  - Syntax errors in code files\n") 
cat("  - Missing image files in www/ folder\n\n")

# Option to run in different modes
cat("Choose startup mode:\n")
cat("1. Normal (default)\n")
cat("2. Debug mode (with more logging)\n")
cat("3. Port 3838 (if default port busy)\n")

# For now, just run normally
tryCatch({
  shiny::runApp(port = 3838, launch.browser = TRUE)
}, error = function(e) {
  cat("ERROR starting app:", e$message, "\n")
  cat("Try running: shiny::runApp() manually for more detailed error info.\n")
})