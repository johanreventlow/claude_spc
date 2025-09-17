# SPC App Developer Guide

Denne guide hjælper udviklere med at forstå, modificere og udvide SPC App'en.

---

## Getting Started

### Prerequisites
- R version 4.5.1 eller nyere
- RStudio (anbefalet)
- Git for version control

### Opsætning af Development Environment

1. **Clone repository**
```bash
git clone <repository-url>
cd claude_spc
```

2. **Installer dependencies**
```r
# Kør i R konsol
install.packages(c("shiny", "bslib", "qicharts2", "dplyr", "ggplot2",
                   "readr", "readxl", "shinycssloaders", "shinyWidgets",
                   "shinyjs", "zoo", "scales", "rlang", "lubridate",
                   "openxlsx", "waiter", "yaml", "later"))
```

3. **Verificer installation**
```r
# Kør app'en første gang
shiny::runApp()
```

### Development Configuration

Sæt disse flags i `global.R` til development:

```r
TEST_MODE_AUTO_LOAD <- TRUE      # Auto-indlæser test data
AUTO_RESTORE_ENABLED <- FALSE    # Deaktiver session restore
```

For debug logging:
```bash
# I terminal før R start
export SPC_LOG_LEVEL=DEBUG
```

---

## Code Architecture & Patterns

### Fil Struktur
```
R/
├── fct_*.R           # Pure functions - business logic
├── mod_*.R           # Shiny modules (UI + Server)
├── utils_*.R         # Utility functions
├── server/           # Server components
├── ui/               # UI components
└── data/             # Test data
```

### Naming Conventions

**Funktioner:**
- `fct_*` - Pure functions, testable business logic
- `mod_*` - Shiny modules (suffix `UI` og `Server`)
- `utils_*` - Utility functions
- `setup_*` - Setup/initialization functions

**Variabler:**
- `snake_case` for variable names
- `UPPER_CASE` for constants
- `camelCase` for reactive values (Shiny convention)

**Files:**
- `kebab-case.R` for file names
- Descriptive prefixes (`fct_`, `mod_`, `utils_`)

### Reactive Patterns

#### ✅ Anbefalede Patterns

**1. Isolate expensive operations:**
```r
expensive_calculation <- reactive({
  input$trigger  # dependency
  isolate({
    # Expensive computation that doesn't need to rerun on every input change
    complex_computation(some_stable_data())
  })
})
```

**2. Use debounced reactives for user input:**
```r
debounced_input <- debounce(reactive(input$user_text), 1000)  # 1 second delay
```

**3. Centralized state management (Phase 4):**
```r
# Use app_state instead of scattered reactive values
app_state$data$current_data <- new_data
# Falls back to old system for compatibility
data <- if (exists("app_state")) app_state$data$current_data else values$current_data
```

#### ❌ Anti-Patterns at undgå

**1. Reactive chains der er for lange:**
```r
# BAD - creates complex dependency chain
reactive1 <- reactive({ input$a + 1 })
reactive2 <- reactive({ reactive1() + 1 })
reactive3 <- reactive({ reactive2() + 1 })  # Too deep

# GOOD - flatten when possible
combined_reactive <- reactive({
  base_value <- input$a
  base_value + 3  # Direct calculation
})
```

**2. Uncontrolled observer proliferation:**
```r
# BAD - too many observers
observe({ ... })  # Observer 1
observe({ ... })  # Observer 2
observe({ ... })  # Observer 3

# GOOD - consolidated observers
observeEvent(list(input$a, input$b, input$c), {
  # Handle multiple inputs in one observer
})
```

---

## State Management (Phase 4)

### Centralized App State

The app bruger et centraliseret state system som er defineret i `global.R`:

```r
app_state <- create_app_state()
```

### Accessing State

Brug sempre dual-state pattern for bagudkompatibilitet:

```r
# Template for state access
current_data <- if (exists("use_centralized_state") &&
                   use_centralized_state &&
                   exists("app_state")) {
  app_state$data$current_data
} else {
  values$current_data  # Fallback til gamle system
}
```

### Updating State

**Centralized updates:**
```r
# Update centralized state
app_state$data$current_data <- new_data
app_state$session$file_uploaded <- TRUE

# For backwards compatibility, også update gamle system hvis det eksisterer
if (exists("values")) {
  values$current_data <- new_data
  values$file_uploaded <- TRUE
}
```

### State Debugging

Brug logging til at debugge state changes:

```r
log_debug(paste("State updated:", names(app_state$data)), "STATE_MGMT")
log_debug(paste("Current data rows:", nrow(app_state$data$current_data)), "STATE_MGMT")
```

---

## Logging System

### Configured Logging

Applikationen bruger et konfigureret logging system implementeret i `R/utils_logging.R`.

#### Log Levels
```r
# Environment variable controls level
# SPC_LOG_LEVEL=DEBUG  (shows all)
# SPC_LOG_LEVEL=INFO   (shows INFO, WARN, ERROR)
# SPC_LOG_LEVEL=WARN   (shows WARN, ERROR)
# SPC_LOG_LEVEL=ERROR  (shows ERROR only)
```

#### Usage Patterns

**Basic logging:**
```r
log_debug("Processing started", "COMPONENT_NAME")
log_info("File loaded successfully", "FILE_UPLOAD")
log_warn("Invalid data detected", "DATA_VALIDATION")
log_error("Critical error occurred", "ERROR_HANDLING")
```

**Component Tags:**
Use descriptive component tags for better debugging:

- `DATA_PROC` - Data processing operations
- `AUTO_DETECT` - Auto-detection functionality
- `FILE_UPLOAD` - File operations
- `VISUALIZATION` - Plot generation
- `UI_SYNC` - UI synchronization
- `TEST_MODE` - Test mode operations
- `ERROR_HANDLING` - Error scenarios
- `STATE_MGMT` - State management operations

**Performance Logging:**
```r
start_time <- Sys.time()
# ... expensive operation ...
end_time <- Sys.time()
log_debug(paste("Operation completed in",
                round(end_time - start_time, 2), "seconds"), "PERFORMANCE")
```

### Migration fra cat() DEBUG

Når du konverterer gamle debug statements:

```r
# OLD
cat("DEBUG: [COMPONENT] Message here\n")

# NEW
log_debug("Message here", "COMPONENT")
```

---

## File Operations

### Support File Formats

Appen understøtter:
- **CSV** files (dansk locale, decimal comma)
- **Excel** files (.xlsx, .xls)

### Adding New File Format

1. **Extend `validate_and_read_file()`** i `fct_file_operations.R`:

```r
validate_and_read_file <- function(file_path, file_name) {
  # ... existing logic ...

  # Add new format
  if (tools::file_ext(file_name) == "new_format") {
    return(read_new_format(file_path))
  }
}
```

2. **Add format-specific reader:**
```r
read_new_format <- function(file_path) {
  tryCatch({
    # Format-specific parsing logic
    data <- your_parsing_function(file_path)

    # Apply standard processing
    processed_data <- ensure_standard_columns(data)

    return(processed_data)
  }, error = function(e) {
    log_error(paste("Failed to read new format:", e$message), "FILE_UPLOAD")
    return(NULL)
  })
}
```

3. **Update UI file input restrictions** i UI komponenter.

4. **Add tests** i `tests/testthat/test-file-operations.R`.

---

## SPC Chart Integration

### qicharts2 Integration

Appen bruger `qicharts2` til SPC chart generation med hospital theming.

### Adding New Chart Types

1. **Extend chart type mappings** i `global.R`:

```r
CHART_TYPES_DA <- list(
  # Existing types...
  "Ny Chart Type (Beskrivelse)" = "new_chart_code"
)
```

2. **Update mapping function:**
```r
get_qic_chart_type <- function(danish_selection) {
  # ... existing logic ...

  if (danish_selection == "Ny Chart Type (Beskrivelse)") {
    return("new_chart_code")
  }
}
```

3. **Add validation i chart generation logic.**

4. **Update tests og documentation.**

### Hospital Theming

Charts bruges automatic hospital theming baseret på `_brand.yml`:

```r
# Theme application sker i plot generation
plot <- plot + HOSPITAL_THEME()
```

For at tilpasse theme:
1. Modificer `HOSPITAL_THEME()` function i `global.R`
2. Update `_brand.yml` med nye farver/settings
3. Test på forskellige chart typer

---

## Testing

### Test Structure

```
tests/testthat/
├── test-phase4-centralized-state.R    # State management tests
├── test-logging-system.R              # Logging system tests
├── test-auto-detection.R              # Auto-detection tests
├── test-end-to-end-app-flow.R        # Integration tests
├── test-ui-synchronization.R         # UI sync tests
├── test-error-handling.R             # Error handling tests
└── ... (additional specialized tests)
```

### Running Tests

```r
# All tests
testthat::test_dir("tests/testthat")

# Specific test file
testthat::test_file("tests/testthat/test-auto-detection.R")

# Skip app tests (faster for unit testing)
testthat::test_dir("tests/testthat", filter = "^(?!test-app)")
```

### Writing Tests

**Unit Test Template:**
```r
test_that("function_name behaves correctly", {
  # Arrange
  input_data <- data.frame(Dato = "01-01-2022", Tæller = 10)

  # Act
  result <- your_function(input_data)

  # Assert
  expect_equal(nrow(result), 1)
  expect_true("processed_column" %in% names(result))
})
```

**Integration Test Template:**
```r
test_that("end-to-end workflow works", {
  # Setup test data
  test_file <- "path/to/test/data.csv"

  # Simulate workflow
  data <- validate_and_read_file(test_file, "test.csv")
  processed <- ensure_standard_columns(data)
  detected <- auto_detect_columns(processed)

  # Verify end state
  expect_true(!is.null(detected))
  expect_true("auto_detected" %in% names(detected))
})
```

**State Management Tests:**
```r
test_that("centralized state updates correctly", {
  # Create fresh state
  app_state <- create_app_state()

  # Update state
  app_state$data$current_data <- test_data

  # Verify update
  expect_equal(nrow(app_state$data$current_data), expected_rows)
})
```

### Test Coverage Goals

- **Unit Tests:** >85% for core functions
- **Integration Tests:** Major workflows covered
- **State Management:** >95% coverage
- **Error Handling:** All error paths tested

---

## Debugging & Troubleshooting

### Common Issues

**1. Reactive Loop Errors**
```
Error: Maximum number of reactive updates exceeded
```

**Solution:** Check for circular reactive dependencies
```r
# Find the loop by adding logging
log_debug("Reactive A triggered", "DEBUG_REACTIVE")
log_debug("Reactive B triggered", "DEBUG_REACTIVE")
```

**2. State Inconsistency**
```
Values don't match between old and new state systems
```

**Solution:** Check dual-state sync patterns
```r
# Debug state consistency
log_debug(paste("Old state:", values$current_data), "STATE_DEBUG")
log_debug(paste("New state:", app_state$data$current_data), "STATE_DEBUG")
```

**3. File Upload Failures**
```
Error reading CSV file
```

**Solution:** Check encoding og delimiter
```r
# Debug file reading
log_debug(paste("File encoding detected:", encoding), "FILE_DEBUG")
log_debug(paste("File size:", file.size(path)), "FILE_DEBUG")
```

### Debug Tools

**1. Enable debug logging:**
```bash
export SPC_LOG_LEVEL=DEBUG
```

**2. Check reactive invalidation:**
```r
# Add to reactive expressions for debugging
observe({
  cat("Reactive invalidated at:", Sys.time(), "\n")
  your_reactive_logic()
})
```

**3. State inspection:**
```r
# Check current app state
str(app_state)
summary(app_state$data$current_data)
```

---

## Performance Best Practices

### Reactive Performance

**1. Minimize reactive dependencies:**
```r
# BAD - triggers on every input change
expensive_calc <- reactive({
  input$many_inputs
  heavy_computation()
})

# GOOD - isolate expensive parts
expensive_calc <- reactive({
  trigger <- input$trigger_button
  isolate({
    heavy_computation(input$stable_data)
  })
})
```

**2. Use debouncing for user inputs:**
```r
# Debounce text input to avoid excessive updates
debounced_text <- debounce(reactive(input$text_input), 500)
```

**3. Cache expensive computations:**
```r
# Cache results when possible
cached_result <- reactiveVal()

expensive_operation <- reactive({
  if (!is.null(cached_result()) &&
      hash(input$data) == last_hash()) {
    return(cached_result())
  }

  result <- heavy_computation(input$data)
  cached_result(result)
  return(result)
})
```

### Memory Management

**1. Clean up observers:**
```r
# Use observer_manager for automatic cleanup
observers <- observer_manager()
obs_id <- observers$add(observe({ ... }), "my_observer")

# Cleanup when done
observers$remove(obs_id)
```

**2. Limit data size:**
```r
# Check data size before processing
if (object.size(data) > 100 * 1024^2) {  # 100MB limit
  log_warn("Large dataset detected", "PERFORMANCE")
  # Consider sampling or chunked processing
}
```

---

## Error Handling Patterns

### Safe Operations

Use `safe_operation()` wrapper for critical operations:

```r
result <- safe_operation(
  "data processing",
  code = {
    processed_data <- complex_operation(raw_data)
    return(processed_data)
  },
  fallback = NULL,
  session = session,
  show_user = TRUE
)
```

### Custom Error Handling

```r
validate_data <- function(data) {
  tryCatch({
    # Validation logic
    if (nrow(data) == 0) {
      stop("Data is empty")
    }

    if (!all(c("Dato", "Tæller") %in% names(data))) {
      stop("Required columns missing")
    }

    return(TRUE)

  }, error = function(e) {
    log_error(paste("Data validation failed:", e$message), "VALIDATION")
    showNotification(
      paste("Data fejl:", e$message),
      type = "error",
      duration = 8
    )
    return(FALSE)
  })
}
```

### User-Friendly Error Messages

```r
# Convert technical errors to user-friendly messages
user_friendly_error <- function(technical_error) {
  error_mappings <- list(
    "file not found" = "Filen kunne ikke findes. Tjek filstien.",
    "encoding error" = "Filen har forkert tegnsæt. Prøv at gemme som UTF-8.",
    "parse error" = "Filen kan ikke læses. Tjek filformat."
  )

  for (pattern in names(error_mappings)) {
    if (grepl(pattern, technical_error, ignore.case = TRUE)) {
      return(error_mappings[[pattern]])
    }
  }

  return("En uventet fejl opstod. Tjek logs for detaljer.")
}
```

---

## Contributing Guidelines

### Code Style

1. **Danske kommentarer** for business logic
2. **Engelske kommentarer** for technical implementation
3. **Konsistent indentation** (2 spaces)
4. **Descriptive function names**
5. **Clear variable names**

### Commit Messages

Brug danske commit beskeder:
```
fix(spc): ret fejl i p-chart beregning
feat(ui): tilføj kompakt visning af data
chore(tests): opdater snapshot for ny funktionalitet
```

### Pull Request Process

1. **Create feature branch** fra `master`
2. **Implement changes** med tests
3. **Run full test suite** (`testthat::test_dir("tests/testthat")`)
4. **Update documentation** hvis nødvendigt
5. **Create pull request** med beskrivelse på dansk

### Testing Requirements

Før pull request:
- ✅ Alle tests skal bestå (`FAIL 0`)
- ✅ Nye funktioner skal have tests
- ✅ Code coverage skal holdes på nuværende niveau
- ✅ App skal køre uden fejl lokalt

---

## Troubleshooting Common Development Issues

### R Session Issues

**Problem:** R session hangs eller crasher
```
Session Aborted
R encountered a fatal error
```

**Solution:**
1. Check for infinite reactive loops
2. Restart R session
3. Check memory usage med `memory.size()` (Windows) / `pryr::mem_used()` (Unix)

### Package Loading Issues

**Problem:** Pakker kan ikke loades
```
Error in library(packagename) : there is no package called 'packagename'
```

**Solution:**
```r
# Check installed packages
installed.packages()[,"Package"]

# Reinstall missing package
install.packages("packagename")

# Check R version compatibility
R.version.string
```

### Port Conflicts

**Problem:** Shiny app kan ikke starte på default port
```
Error: Failed to create server
```

**Solution:**
```r
# Use different port
shiny::runApp(port = 8080)

# Or find available port
shiny::runApp(port = httpuv::randomPort())
```

---

## Advanced Topics

### Custom Shiny Modules

Template for new Shiny module:

```r
# Module UI
newModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    # UI elements with ns() wrapped IDs
    selectInput(ns("input"), "Label:", choices = c("A", "B")),
    plotOutput(ns("plot"))
  )
}

# Module Server
newModuleServer <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {

    # Reactive logic
    processed_data <- reactive({
      req(data_reactive())
      # Process data based on input$input
      data_reactive() %>% filter(category == input$input)
    })

    # Output rendering
    output$plot <- renderPlot({
      req(processed_data())
      ggplot(processed_data(), aes(x, y)) + geom_point()
    })

    # Return reactive values for parent
    return(list(
      processed_data = processed_data
    ))
  })
}
```

### Performance Profiling

```r
# Profile app performance
profvis::profvis({
  # Run specific operations
  data <- validate_and_read_file("large_file.csv", "test.csv")
  result <- auto_detect_columns(data)
})
```

### Memory Profiling

```r
# Check memory usage
gc()  # Force garbage collection
mem_used()  # Current memory usage

# Profile memory over time
memory_profile <- function() {
  cat("Memory usage:", mem_used(), "\n")
}

# Call at different points in workflow
memory_profile()  # Before operation
expensive_operation()
memory_profile()  # After operation
```

---

## Function Documentation med roxygen2

### Documentation Standards

Alle eksporterede funktioner og vigtige interne funktioner skal dokumenteres med roxygen2. Dette sikrer konsistent API dokumentation og letter maintainability.

#### roxygen2 Syntax Guide

**Basis template:**
```r
#' Kort beskrivelse af funktionen
#'
#' Længere beskrivelse der forklarer funktionens formål,
#' use cases og eventuelle side effects. Beskriv også
#' special behavior eller PHASE 4 compatibility.
#'
#' @param param_name Beskrivelse af parameter inklusive type og format
#' @param optional_param Beskrivelse (optional) - marker optional parameters
#'
#' @details
#' Detaljeret forklaring af algoritme, implementation choices
#' eller særlige considerations. Brug itemize/enumerate
#' for strukturerede lister.
#'
#' @return Beskrivelse af return value med struktur hvis complex
#'
#' @examples
#' \dontrun{
#' # Praktisk eksempel på funktions brug
#' result <- function_name(param1, param2)
#' }
#'
#' @seealso \code{\link{related_function1}}, \code{\link{related_function2}}
function_name <- function(param_name, optional_param = NULL) {
```

#### Documentation Categories

**Core SPC Functions:**
```r
#' @family spc_calculations
#' @keywords spc control_charts statistical_process_control
```

**Data Processing Functions:**
```r
#' @family data_processing
#' @keywords data_validation column_detection auto_detection
```

**File Operations:**
```r
#' @family file_operations
#' @keywords csv excel file_upload danish_locale
```

**UI/UX Functions:**
```r
#' @family ui_helpers
#' @keywords shiny reactive user_interface
```

#### Danish Documentation Standards

**Beskrivelse på dansk:**
- Funktions formål og behavior på dansk
- Parameter beskrivelser på dansk
- Examples med danske variable navne hvor relevant

**English for technical terms:**
- Function names og parameter names på engelsk
- Technical keywords på engelsk
- Code examples kan blande dansk/engelsk naturligt

**Eksempel:**
```r
#' Auto-detekter SPC kolonner baseret på kolonnenavne
#'
#' Intelligent matching af kolonnenavne til standard SPC kolonner.
#' Understøtter både danske og engelske kolonnenavne med fallback strategies.
#'
#' @param col_names Character vector med tilgængelige kolonnenavne
#' @param input Shiny input object (kan være NULL)
#' @param session Shiny session object for UI opdateringer
#'
#' @return List med detekterede kolonner og UI sync data
#'
#' @examples
#' \dontrun{
#' kolonner <- c("Dato", "Tæller", "Nævner", "Kommentar")
#' result <- detect_columns_name_only(kolonner, NULL, session, values)
#' }
```

### Generation og Maintenance

**Generer dokumentation:**
```r
# Install roxygen2 if needed
install.packages("roxygen2")

# Generate/update documentation
roxygen2::roxygenise()

# View generated help
?function_name
help(package = "claude_spc")
```

**Documentation workflow:**
1. **Skriv roxygen2 comments** når du opretter ny funktion
2. **Update dokumentation** når du ændrer function signature
3. **Test examples** - sørg for at examples kører uden fejl
4. **Cross-reference** - link til relaterede funktioner
5. **Review generated docs** - tjek at formatering er korrekt

### Documentation Coverage Goals

**Mandatory documentation:**
- Alle setup_* funktioner (app initialization)
- Alle detect_* funktioner (auto-detection logic)
- Alle handle_* funktioner (file operations)
- Alle validate_* funktioner (data validation)

**Nice-to-have documentation:**
- Komplekse interne utility funktioner
- Observer functions med kompleks logic
- Error handling wrapper functions

**Skip documentation:**
- Simple helper functions (< 10 lines)
- Internal debug functions
- Temporary/experimental code

### Examples Best Practices

**Good examples:**
```r
#' @examples
#' \dontrun{
#' # Standard SPC kolonner
#' cols <- c("Dato", "Tæller", "Nævner", "Skift", "Frys")
#' result <- detect_columns_name_only(cols, NULL, session, values)
#' print(result$x_col)  # "Dato"
#'
#' # Engelske kolonner
#' cols_en <- c("Date", "Count", "Total", "Phase")
#' result <- detect_columns_name_only(cols_en, NULL, session, values)
#' print(result$x_col)  # "Date"
#' }
```

**Avoid:**
```r
#' @examples
#' \dontrun{
#' # Dette er et eksempel  (for generic)
#' result <- function_name(data)  (too simple)
#' }
```

---

## Resources

### Internal Documentation
- `TECHNICAL_IMPROVEMENTS_PLAN.md` - Roadmap og improvement tracking
- `SHINY_BEST_PRACTICES_FASER.md` - Phase-based development approach
- `TESTING.md` - Test framework guide
- `MODULE_STRUCTURE.md` - Module organization principles

### External Resources
- [Shiny Development Best Practices](https://shiny.rstudio.com/articles/)
- [qicharts2 Documentation](https://cran.r-project.org/package=qicharts2)
- [Reactive Programming Guide](https://shiny.rstudio.com/articles/reactivity-overview.html)
- [R Package Development](https://r-pkgs.org/)

### Community
- [Shiny Community Forum](https://community.rstudio.com/c/shiny)
- [R4DS Slack Community](https://r4ds.io/join)

---

*Denne guide opdateres kontinuerligt. For spørgsmål eller forslag til forbedringer, opret en issue i repository.*