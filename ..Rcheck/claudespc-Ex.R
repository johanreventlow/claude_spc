pkgname <- "claudespc"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('claudespc')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("get_app_option")
### * get_app_option

flush(stderr()); flush(stdout())

### Name: get_app_option
### Title: Get Application Option (Golem-style)
### Aliases: get_app_option

### ** Examples

## Not run: 
##D # Get with default
##D debug_level <- get_app_option("debug_level", "INFO")
##D 
##D # Check if in test mode
##D is_test_mode <- get_app_option("test_mode", FALSE)
## End(Not run)



cleanEx()
nameEx("get_golem_config")
### * get_golem_config

flush(stderr()); flush(stdout())

### Name: get_golem_config
### Title: Load Golem Configuration from YAML
### Aliases: get_golem_config

### ** Examples

## Not run: 
##D # Load development configuration
##D dev_config <- get_golem_config("development")
##D 
##D # Load production configuration
##D prod_config <- get_golem_config("production")
##D 
##D # Load from custom path
##D config <- get_golem_config("development", "custom/path/config.yml")
## End(Not run)



cleanEx()
nameEx("handle_csv_upload")
### * handle_csv_upload

flush(stderr()); flush(stdout())

### Name: handle_csv_upload
### Title: Håndter CSV fil upload med dansk formattering
### Aliases: handle_csv_upload

### ** Examples

## Not run: 
##D # Upload CSV fil
##D result <- handle_csv_upload("data/spc_data.csv", values)
##D if (is.null(result)) {
##D   message("CSV uploaded successfully")
##D } else {
##D   message("Error:", result)
##D }
## End(Not run)




cleanEx()
nameEx("run_app")
### * run_app

flush(stderr()); flush(stdout())

### Name: run_app
### Title: Run the SPC Shiny Application
### Aliases: run_app

### ** Examples

## Not run: 
##D # Basic usage
##D run_app()
##D 
##D # Development with specific options
##D run_app(port = 4040, options = list(test_mode = TRUE))
##D 
##D # Production deployment
##D run_app(launch_browser = TRUE, options = list(production_mode = TRUE))
## End(Not run)




cleanEx()
nameEx("run_dev")
### * run_dev

flush(stderr()); flush(stdout())

### Name: run_dev
### Title: Development Application Runner (Golem-style)
### Aliases: run_dev

### ** Examples

## Not run: 
##D # Basic development run
##D run_dev()
##D 
##D # Development with specific settings
##D run_dev(port = 5050, debug_level = "DEBUG")
## End(Not run)



cleanEx()
nameEx("set_app_options")
### * set_app_options

flush(stderr()); flush(stdout())

### Name: set_app_options
### Title: Set Application Options (Golem-style)
### Aliases: set_app_options

### ** Examples

## Not run: 
##D # Set development options
##D set_app_options(list(
##D   test_mode = TRUE,
##D   debug_level = "DEBUG",
##D   auto_load_data = TRUE
##D ))
##D 
##D # Set production options
##D set_app_options(list(
##D   production_mode = TRUE,
##D   debug_level = "ERROR",
##D   auto_restore = TRUE
##D ))
## End(Not run)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
