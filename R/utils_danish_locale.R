# R/utils/danish_numbers.R
# Hjælpefunktioner til håndtering af danske talformater (komma som decimalseparator)

# DANSK TAL KONVERTERING =====================================================

## Konverter dansk talstreng til numerisk værdi
# Håndterer både komma og punktum som decimalseparator samt procent/promille symboler
parse_danish_number <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(numeric(0))
  }

  # Handle vectors using tidyverse approach
  if (length(x) > 1) {
    return(purrr::map_dbl(x, parse_danish_number))
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

  # Remove common symbols that appear in Danish data
  # - Remove % and ‰ symbols (but keep the numeric value)
  # - Remove thousand separators (spaces or dots in specific patterns)
  x_cleaned <- x
  x_cleaned <- gsub("[%‰]", "", x_cleaned)  # Remove percent and permille symbols
  x_cleaned <- gsub("\\s+", "", x_cleaned)  # Remove spaces
  x_cleaned <- trimws(x_cleaned)

  # Replace comma with dot for decimal separation
  x_normalized <- gsub(",", ".", x_cleaned)

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
# Prioriterer brugerens eksplicitte y-akse enhedsvalg over automatisk skala-detektion
parse_danish_target <- function(target_input, y_data = NULL, y_axis_unit = NULL) {
  if (is.null(target_input) || target_input == "" || is.na(target_input)) {
    return(NULL)
  }

  # Remove whitespace
  target_input <- trimws(target_input)

  # Check if input contains % or ‰ symbols
  has_percent <- grepl("%", target_input)
  has_permille <- grepl("‰", target_input)

  # Invalid input: indeholder både % og ‰ symboler
  if (has_percent && has_permille) {
    return(NULL) # Invalid format
  }

  # Remove % and ‰ symbols and parse as Danish number
  clean_input <- gsub("[%‰]", "", target_input)
  numeric_value <- parse_danish_number(clean_input)

  if (is.na(numeric_value)) {
    return(NULL) # Invalid input
  }

  # Prioriter eksplicit y_axis_unit over automatisk detektion
  if (!is.null(y_axis_unit) && y_axis_unit != "") {
    return(convert_by_unit_type(numeric_value, y_axis_unit, has_percent, has_permille))
  }

  # Fallback: Brug automatisk skala-detektion hvis ingen enhed er specificeret
  if (is.null(y_data)) {
    # Ingen y_data og ingen unit - brug simple konverteringsregler
    if (has_permille) {
      return(numeric_value) # Behold promille-værdi som-den-er
    } else if (has_percent && numeric_value > 1) {
      return(numeric_value / 100) # Konverter procent til decimal
    }
    return(numeric_value)
  }

  # Use scale detection logic som fallback
  scale_type <- detect_y_axis_scale(y_data)

  # Convert based on detected scale and input format
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

## Konverter baseret på eksplicit enhedstype
# Håndterer konvertering baseret på brugerens y-akse enhedsvalg
convert_by_unit_type <- function(numeric_value, y_axis_unit, has_percent, has_permille) {
  if (y_axis_unit == "percent") {
    # Bruger har valgt procent som enhed - behandl input som procent
    if (has_percent) {
      return(numeric_value) # 80% → 80
    } else if (numeric_value <= 1 && !has_permille) {
      return(numeric_value * 100) # 0.8 → 80
    } else {
      return(numeric_value) # 80 → 80
    }
  } else if (y_axis_unit == "permille") {
    # Bruger har valgt promille som enhed
    if (has_permille) {
      return(numeric_value) # 8‰ → 8
    } else if (numeric_value <= 1 && !has_percent) {
      return(numeric_value * 1000) # 0.008 → 8
    } else {
      return(numeric_value) # 8 → 8
    }
  } else if (y_axis_unit %in% c("count", "rate_1000", "rate_100000", "days", "hours", "grams", "kg", "dkk")) {
    # Absolutte enheder - behold numerisk værdi
    if (has_percent) {
      return(numeric_value) # 80% → 80 (fjern % men behold tal)
    } else if (has_permille) {
      return(numeric_value) # 8‰ → 8 (fjern ‰ men behold tal)
    } else {
      return(numeric_value) # 80 → 80
    }
  } else {
    # Ukendt enhed - fallback til simple regler
    if (has_permille) {
      return(numeric_value)
    } else if (has_percent && numeric_value > 1) {
      return(numeric_value / 100)
    } else {
      return(numeric_value)
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
