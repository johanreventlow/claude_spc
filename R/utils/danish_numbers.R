# R/utils/danish_numbers.R
# Utility functions for handling Danish number formats (comma as decimal separator)

# Convert Danish number string to numeric (handles both comma and dot)
parse_danish_number <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(numeric(0))
  }
  
  # Handle vectors
  if (length(x) > 1) {
    return(sapply(x, parse_danish_number, USE.NAMES = FALSE))
  }
  
  # Convert to character if needed
  x <- as.character(x)
  
  # Return NA if empty or whitespace only
  if (is.na(x) || trimws(x) == "") {
    return(NA_real_)
  }
  
  # Remove whitespace
  x <- trimws(x)
  
  # Handle special cases
  if (x == "" || is.na(x)) {
    return(NA_real_)
  }
  
  # Replace comma with dot for decimal separation
  x_normalized <- gsub(",", ".", x)
  
  # Convert to numeric
  result <- suppressWarnings(as.numeric(x_normalized))
  
  return(result)
}

# Format numeric as Danish (dot to comma)
format_danish_number <- function(x, digits = NULL) {
  if (is.null(x) || length(x) == 0) {
    return(character(0))
  }
  
  # Handle NA values
  if (all(is.na(x))) {
    return(rep(NA_character_, length(x)))
  }
  
  # Format numbers
  if (is.null(digits)) {
    formatted <- format(x, scientific = FALSE, trim = TRUE)
  } else {
    formatted <- formatC(x, digits = digits, format = "f")
  }
  
  # Replace dot with comma
  formatted <- gsub("\\.", ",", formatted)
  
  return(formatted)
}

# Parse target value with Danish number support
parse_danish_target <- function(target_input, y_data = NULL) {
  if (is.null(target_input) || target_input == "" || is.na(target_input)) {
    return(NULL)
  }
  
  # Remove whitespace
  target_input <- trimws(target_input)
  
  # Check if input contains % symbol
  has_percent <- grepl("%", target_input)
  
  # Remove % symbol and parse as Danish number
  clean_input <- gsub("%", "", target_input)
  numeric_value <- parse_danish_number(clean_input)
  
  if (is.na(numeric_value)) {
    return(NULL)  # Invalid input
  }
  
  # If no y_data provided, return as-is (but convert % to decimal if present)
  if (is.null(y_data)) {
    if (has_percent && numeric_value > 1) {
      return(numeric_value / 100)
    }
    return(numeric_value)
  }
  
  # Use existing scale detection logic
  source("R/modules/visualization_helpers.R", local = TRUE)
  scale_type <- detectYAxisScale(y_data)
  
  # Convert based on scale and input format
  if (scale_type == "decimal") {
    # Y-axis is 0-1 scale
    if (has_percent || numeric_value > 1) {
      return(numeric_value / 100)  # Convert to decimal
    } else {
      return(numeric_value)  # Already decimal
    }
  } else if (scale_type == "percent") {
    # Y-axis is 0-100+ scale  
    if (has_percent) {
      return(numeric_value)  # Remove % but keep value
    } else if (numeric_value <= 1) {
      return(numeric_value * 100)  # Convert decimal to percent
    } else {
      return(numeric_value)  # Already in percent range
    }
  } else {
    # Y-axis is integer/rate scale
    if (has_percent) {
      return(numeric_value)  # Remove % symbol but keep numeric value
    } else {
      return(numeric_value)  # Use as-is
    }
  }
}

# Validate that a string can be parsed as a Danish number
is_valid_danish_number <- function(x) {
  if (is.null(x) || is.na(x) || trimws(x) == "") {
    return(FALSE)
  }
  
  # Try to parse - if it returns NA, it's invalid
  parsed <- parse_danish_number(x)
  return(!is.na(parsed))
}