# R/utils/danish_numbers.R
# Hjælpefunktioner til håndtering af danske talformater (komma som decimalseparator)

# DANSK TAL KONVERTERING =====================================================

## Konverter dansk talstreng til numerisk værdi
# Håndterer både komma og punktum som decimalseparator
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

## Formatér numerisk værdi som dansk (punktum til komma)
# Konverterer numeriske værdier til dansk format med komma
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

## Behandl målværdi med dansk talunderstøttelse
# Håndterer målværdier med danske talformater og procenttegn
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
    return(NULL) # Invalid input
  }

  # If no y_data provided, return as-is (but convert % to decimal if present)
  if (is.null(y_data)) {
    if (has_percent && numeric_value > 1) {
      return(numeric_value / 100)
    }
    return(numeric_value)
  }

  # Use scale detection logic
  scale_type <- detect_y_axis_scale(y_data)

  # Convert based on scale and input format
  if (scale_type == "decimal") {
    # Y-axis is 0-1 scale
    if (has_percent || numeric_value > 1) {
      return(numeric_value / 100) # Convert to decimal
    } else {
      return(numeric_value) # Already decimal
    }
  } else if (scale_type == "percent") {
    # Y-axis is 0-100+ scale
    if (has_percent) {
      return(numeric_value) # Remove % but keep value
    } else if (numeric_value <= 1) {
      return(numeric_value * 100) # Convert decimal to percent
    } else {
      return(numeric_value) # Already in percent range
    }
  } else {
    # Y-axis is integer/rate scale
    if (has_percent) {
      return(numeric_value) # Remove % symbol but keep numeric value
    } else {
      return(numeric_value) # Use as-is
    }
  }
}

## Validér om streng kan fortolkes som dansk tal
# Tjekker om en given streng er et gyldigt dansk talformat
is_valid_danish_number <- function(x) {
  if (is.null(x) || is.na(x) || trimws(x) == "") {
    return(FALSE)
  }

  # Try to parse - if it returns NA, it's invalid
  parsed <- parse_danish_number(x)
  return(!is.na(parsed))
}

## Y-akse Skalerings Detektering
# Automatisk detektering af passende Y-akse format (decimal, procent, heltal)
detect_y_axis_scale <- function(y_data) {
  if (is.null(y_data) || length(y_data) == 0) {
    return("integer")
  }

  # Remove NA values
  y_clean <- y_data[!is.na(y_data)]

  if (length(y_clean) == 0) {
    return("integer")
  }

  max_val <- max(y_clean)
  min_val <- min(y_clean)

  # Rule 1: Decimal scale (0-1)
  if (max_val <= 1.0) {
    return("decimal")
  }

  # Rule 2: Percent scale (0-100+ with most values looking like percentages)
  if (min_val >= 0 && max_val <= 200) {
    # Check if most values look like percentages (0-100 range)
    percent_like_count <- sum(y_clean >= 0 & y_clean <= 100)
    if (percent_like_count / length(y_clean) >= 0.7) { # 70% threshold
      return("percent")
    }
  }

  # Rule 3: Integer/rate scale
  return("integer")
}
