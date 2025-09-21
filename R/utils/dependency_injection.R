# utils_dependency_injection.R
# Dependency injection patterns for better testability and modularity

#' Create dependency container for app components
#'
#' Centraliserer alle dependencies i et struktureret container object.
#' Dette letter testing og gør app arkitekturen mere modulær ved at
#' reducere tight coupling mellem komponenter.
#'
#' @param config List med app konfiguration (optional)
#' @param test_mode Logical - om app kører i test mode (default FALSE)
#'
#' @return List med alle app dependencies
#'
#' @details
#' Dependency container indeholder:
#' \itemize{
#'   \item config: App konfiguration og constants
#'   \item services: Business logic services
#'   \item repositories: Data access layer
#'   \item validators: Input validation functions
#'   \item utilities: Helper functions
#' }
#'
#' @examples
#' \dontrun{
#' # Standard production dependencies
#' deps <- create_dependency_container()
#'
#' # Test mode med mock dependencies
#' test_deps <- create_dependency_container(test_mode = TRUE)
#' }
#'
#' @family dependency_injection
#' @export
create_dependency_container <- function(config = NULL, test_mode = FALSE) {

  # Default config hvis ikke angivet
  if (is.null(config)) {
    config <- list(
      encoding = DEFAULT_ENCODING,
      csv_separators = CSV_SEPARATORS,
      ui_widths = UI_INPUT_WIDTHS,
      spc_colors = SPC_COLORS,
      log_components = LOG_COMPONENTS
    )
  }

  # Services layer - business logic
  services <- create_services_layer(test_mode)

  # Repository layer - data access
  repositories <- create_repository_layer(test_mode)

  # Validation layer - input validation
  validators <- create_validation_layer()

  # Utilities layer - helper functions
  utilities <- create_utilities_layer()

  list(
    config = config,
    services = services,
    repositories = repositories,
    validators = validators,
    utilities = utilities,
    test_mode = test_mode
  )
}

#' Create services layer for business logic
#'
#' Opretter service objekter til business logic uden direct dependencies
#' til Shiny eller external systems. Dette gør dem lettere at teste.
#'
#' @param test_mode Logical - om services skal være mock versions
#'
#' @return List med service objekter
#'
#' @family dependency_injection
create_services_layer <- function(test_mode = FALSE) {

  if (test_mode) {
    # Mock services til testing
    list(
      column_detection = create_mock_column_detection_service(),
      file_processing = create_mock_file_processing_service(),
      spc_calculation = create_mock_spc_calculation_service()
    )
  } else {
    # Production services
    list(
      column_detection = create_column_detection_service(),
      file_processing = create_file_processing_service(),
      spc_calculation = create_spc_calculation_service()
    )
  }
}

#' Create repository layer for data access
#'
#' Opretter repository objekter til data access med consistent interface.
#' Repositories håndterer alle file I/O og data persistence operationer.
#'
#' @param test_mode Logical - om repositories skal være mock versions
#'
#' @return List med repository objekter
#'
#' @family dependency_injection
create_repository_layer <- function(test_mode = FALSE) {

  if (test_mode) {
    # Mock repositories til testing
    list(
      file_repository = create_mock_file_repository(),
      session_repository = create_mock_session_repository()
    )
  } else {
    # Production repositories
    list(
      file_repository = create_file_repository(),
      session_repository = create_session_repository()
    )
  }
}

#' Create validation layer
#'
#' Opretter validation objekter til input validation med reusable rules.
#' Alle validation logic samles her for consistency og testability.
#'
#' @return List med validator objekter
#'
#' @family dependency_injection
create_validation_layer <- function() {
  list(
    data_validator = create_data_validator(),
    file_validator = create_file_validator(),
    column_validator = create_column_validator(),
    numeric_validator = create_numeric_validator()
  )
}

#' Create utilities layer
#'
#' Opretter utility objekter til helper functions uden side effects.
#' Pure functions der kan genbruges på tværs af app components.
#'
#' @return List med utility objekter
#'
#' @family dependency_injection
create_utilities_layer <- function() {
  list(
    date_utils = create_date_utilities(),
    string_utils = create_string_utilities(),
    format_utils = create_format_utilities(),
    ui_utils = create_ui_utilities()
  )
}

#' Inject dependencies into function
#'
#' Utility function til at injicere dependencies ind i functions
#' på en clean måde. Reducerer parameter passing complexity.
#'
#' @param func Function der skal have dependencies injected
#' @param deps Dependency container
#' @param ... Additional parameters til function
#'
#' @return Function result med injected dependencies
#'
#' @examples
#' \dontrun{
#' # Inject dependencies into function
#' result <- inject_dependencies(
#'   process_data,
#'   deps,
#'   data = my_data,
#'   options = my_options
#' )
#' }
#'
#' @family dependency_injection
#' @export
inject_dependencies <- function(func, deps, ...) {
  # Create function med dependencies som closure
  function_with_deps <- function(...) {
    func(deps = deps, ...)
  }

  # Execute function med provided arguments
  function_with_deps(...)
}

# PLACEHOLDER FUNCTIONS ===========================================================
# Disse funktioner implementeres i separate filer når arkitekturen er stabil

create_column_detection_service <- function() {
  list(
    detect_by_name = function(column_names) {
      # Use unified autodetect_engine instead of detect_columns_name_only
      # Note: Requires proper app_state and emit context in actual usage
      # Create dummy data frame for name-only detection
      dummy_data <- data.frame(matrix(NA, nrow = 0, ncol = length(column_names)))
      names(dummy_data) <- column_names

      autodetect_engine(
        data = dummy_data,
        trigger_type = "session_start",
        app_state = NULL,  # Should be injected in real usage
        emit = NULL        # Should be injected in real usage
      )
    },
    detect_by_data = function(data) detect_columns_with_data(data),
    validate_detection = function(detection_result) validate_column_detection(detection_result)
  )
}

create_file_processing_service <- function() {
  list(
    read_csv = function(file_path) handle_csv_upload(file_path, NULL),
    read_excel = function(file_path) handle_excel_upload(file_path, NULL, NULL),
    validate_file = function(file_path) validate_file_format(file_path)
  )
}

create_spc_calculation_service <- function() {
  list(
    validate_x_format = function(data, x_col, unit) validate_x_column_format(data, x_col, unit),
    calculate_spc = function(data, chart_type) calculate_spc_chart(data, chart_type),
    format_plot = function(plot_data, options) format_spc_plot(plot_data, options)
  )
}

# Mock services for testing
create_mock_column_detection_service <- function() {
  list(
    detect_by_name = function(column_names) list(x_col = "Dato", y_col = "Tæller"),
    detect_by_data = function(data) list(x_col = names(data)[1], y_col = names(data)[2]),
    validate_detection = function(detection_result) TRUE
  )
}

create_mock_file_processing_service <- function() {
  list(
    read_csv = function(file_path) data.frame(Dato = "01-01-2024", Tæller = 95),
    read_excel = function(file_path) data.frame(Dato = "01-01-2024", Tæller = 95),
    validate_file = function(file_path) TRUE
  )
}

create_mock_spc_calculation_service <- function() {
  list(
    validate_x_format = function(data, x_col, unit) list(x_data = 1:10, is_date = FALSE),
    calculate_spc = function(data, chart_type) list(plot_data = data, statistics = list()),
    format_plot = function(plot_data, options) ggplot2::ggplot()
  )
}

# Repository implementations
create_file_repository <- function() {
  list(
    save_file = function(data, path) write.csv(data, path),
    load_file = function(path) read.csv(path),
    delete_file = function(path) unlink(path)
  )
}

create_session_repository <- function() {
  list(
    save_session = function(session_data, id) NULL,  # Implement session persistence
    load_session = function(id) NULL,                # Implement session loading
    delete_session = function(id) NULL               # Implement session cleanup
  )
}

# Mock repositories
create_mock_file_repository <- function() {
  list(
    save_file = function(data, path) TRUE,
    load_file = function(path) data.frame(test = 1),
    delete_file = function(path) TRUE
  )
}

create_mock_session_repository <- function() {
  list(
    save_session = function(session_data, id) TRUE,
    load_session = function(id) list(test_data = TRUE),
    delete_session = function(id) TRUE
  )
}

# Validator implementations
create_data_validator <- function() {
  list(
    validate_columns = function(data) all(c("x", "y") %in% names(data)),
    validate_types = function(data) TRUE,
    validate_completeness = function(data) !any(is.na(data))
  )
}

create_file_validator <- function() {
  list(
    validate_extension = function(path) grepl("\\.(csv|xlsx)$", path),
    validate_size = function(path) file.size(path) < 50 * 1024 * 1024,  # 50MB limit
    validate_encoding = function(path) TRUE
  )
}

create_column_validator <- function() {
  list(
    validate_column_names = function(names) !any(duplicated(names)),
    validate_column_types = function(data) TRUE,
    validate_required_columns = function(data, required) all(required %in% names(data))
  )
}

create_numeric_validator <- function() {
  list(
    validate_range = function(value, min, max) value >= min & value <= max,
    validate_positive = function(value) value > 0,
    validate_percentage = function(value) value >= 0 & value <= 100
  )
}

# Utility implementations
create_date_utilities <- function() {
  list(
    parse_danish_date = function(date_string) as.Date(date_string, format = "%d-%m-%Y"),
    format_for_display = function(date) format(date, "%d-%m-%Y"),
    detect_date_format = function(date_strings) "%d-%m-%Y"
  )
}

create_string_utilities <- function() {
  list(
    clean_column_names = function(names) gsub("[^A-Za-z0-9_]", "_", names),
    normalize_text = function(text) trimws(tolower(text)),
    extract_numbers = function(text) as.numeric(gsub("[^0-9.,]", "", text))
  )
}

create_format_utilities <- function() {
  list(
    format_percentage = function(value) paste0(round(value * 100, 1), "%"),
    format_number = function(value, digits = 2) round(value, digits),
    format_currency = function(value) paste0(value, " kr.")
  )
}

create_ui_utilities <- function() {
  list(
    create_loading_spinner = function() shiny::div(class = "spinner-border"),
    create_error_message = function(msg) shiny::div(class = "alert alert-danger", msg),
    create_success_message = function(msg) shiny::div(class = "alert alert-success", msg)
  )
}