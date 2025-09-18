# fct_autodetect_helpers.R
# Supporting functions for unified autodetect engine
# Focused on robust date detection and column scoring

#' Robust Date Column Detection using Lubridate
#'
#' Detects date columns using lubridate with priority for Danish formats.
#' Uses comprehensive format testing with success rate thresholds.
#'
#' @param data Data frame to analyze
#' @param success_threshold Minimum success rate (0-1) for date parsing
#' @return Named list of date candidates with scores and suggested formats
detect_date_columns_robust <- function(data, success_threshold = 0.8) {
  log_debug_block("DATE_DETECT", "Starting robust date detection")

  if (is.null(data) || ncol(data) == 0) {
    log_debug("No data for date detection", .context = "DATE_DETECT")
    return(list())
  }

  date_candidates <- list()

  # Danish date formats - prioritized for SPC context
  danish_formats <- c("dmy", "dmY", "d/m/Y", "d-m-Y", "d.m.Y", "d m Y")

  # International formats - fallback
  intl_formats <- c("ymd", "mdy", "Ymd", "mdY", "Y-m-d", "m/d/Y")

  log_debug_kv(
    analyzing_columns = ncol(data),
    danish_formats_count = length(danish_formats),
    intl_formats_count = length(intl_formats),
    .context = "DATE_DETECT"
  )

  for (col_name in names(data)) {
    col_data <- data[[col_name]]

    # Skip if already Date/POSIXct class
    if (inherits(col_data, c("Date", "POSIXct", "POSIXt"))) {
      date_candidates[[col_name]] <- list(
        score = 1.0,
        suggested_format = "native_date_class",
        reason = "Already date/time class"
      )
      log_debug_kv(
        column = col_name,
        result = "native_date_class",
        .context = "DATE_DETECT"
      )
      next
    }

    # Skip non-character/non-factor columns
    if (!is.character(col_data) && !is.factor(col_data)) {
      next
    }

    # Convert factor to character for testing
    if (is.factor(col_data)) {
      col_data <- as.character(col_data)
    }

    # Remove missing values for testing
    non_missing <- col_data[!is.na(col_data) & col_data != ""]
    if (length(non_missing) == 0) {
      next
    }

    # Test sample (first 10 non-missing values for performance)
    test_sample <- head(non_missing, 10)

    log_debug_kv(
      testing_column = col_name,
      sample_values = paste(head(test_sample, 3), collapse = ", "),
      .context = "DATE_DETECT"
    )

    # Test Danish formats first
    best_score <- 0
    best_format <- NULL
    best_reason <- NULL

    for (format_type in c("danish", "international")) {
      formats_to_test <- if (format_type == "danish") danish_formats else intl_formats

      for (format in formats_to_test) {
        success_rate <- test_date_parsing_format(test_sample, format)

        if (success_rate > best_score) {
          best_score <- success_rate
          best_format <- format
          best_reason <- paste(format_type, format, "parsing")
        }

        # Early exit if we find perfect Danish format match
        if (format_type == "danish" && success_rate >= 0.95) {
          break
        }
      }

      # Prefer Danish formats - break if we found good Danish match
      if (format_type == "danish" && best_score >= success_threshold) {
        break
      }
    }

    # Add to candidates if meets threshold
    if (best_score >= success_threshold) {
      date_candidates[[col_name]] <- list(
        score = best_score,
        suggested_format = best_format,
        reason = best_reason
      )

      log_debug_kv(
        column = col_name,
        score = round(best_score, 3),
        format = best_format,
        .context = "DATE_DETECT"
      )
    }
  }

  log_debug_kv(
    date_candidates_found = length(date_candidates),
    candidate_names = paste(names(date_candidates), collapse = ", "),
    .context = "DATE_DETECT"
  )

  return(date_candidates)
}

#' Test Date Parsing for a Specific Format
#'
#' Tests how well a specific lubridate format parses a sample of values.
#'
#' @param test_sample Character vector of values to test
#' @param format Lubridate format string (e.g., "dmy", "ymd")
#' @return Success rate (0-1)
test_date_parsing_format <- function(test_sample, format) {
  if (length(test_sample) == 0) return(0)

  successful_parses <- 0
  total_attempts <- length(test_sample)

  for (value in test_sample) {
    tryCatch({
      # Use appropriate lubridate function based on format
      parsed_date <- switch(format,
        "dmy" = lubridate::dmy(value),
        "dmY" = lubridate::dmy(value),
        "ymd" = lubridate::ymd(value),
        "mdy" = lubridate::mdy(value),
        "Ymd" = lubridate::ymd(value),
        "mdY" = lubridate::mdy(value),
        # For complex formats, use parse_date_time
        lubridate::parse_date_time(value, format, quiet = TRUE)
      )

      if (!is.na(parsed_date)) {
        successful_parses <- successful_parses + 1
      }
    }, error = function(e) {
      # Parse failed - don't increment successful_parses
    })
  }

  return(successful_parses / total_attempts)
}

#' Find Numeric Columns Suitable for Analysis
#'
#' Identifies columns that are numeric or can be reasonably converted to numeric.
#'
#' @param data Data frame to analyze
#' @return Character vector of numeric column names
find_numeric_columns <- function(data) {
  log_debug_block("NUMERIC_DETECT", "Finding numeric columns")

  if (is.null(data) || ncol(data) == 0) {
    return(character(0))
  }

  numeric_cols <- character(0)

  for (col_name in names(data)) {
    col_data <- data[[col_name]]

    # Direct numeric columns
    if (is.numeric(col_data)) {
      numeric_cols <- c(numeric_cols, col_name)
      next
    }

    # Test if character/factor can be converted to numeric
    if (is.character(col_data) || is.factor(col_data)) {
      # Convert factor to character first
      if (is.factor(col_data)) {
        col_data <- as.character(col_data)
      }

      # Test conversion on non-missing values
      non_missing <- col_data[!is.na(col_data) & col_data != ""]
      if (length(non_missing) == 0) next

      # Test sample for performance
      test_sample <- head(non_missing, 20)

      # Try to convert to numeric
      converted <- suppressWarnings(as.numeric(test_sample))
      success_rate <- sum(!is.na(converted)) / length(test_sample)

      # If most values convert successfully, consider it numeric
      if (success_rate >= 0.8) {
        numeric_cols <- c(numeric_cols, col_name)
        log_debug_kv(
          convertible_column = col_name,
          success_rate = round(success_rate, 3),
          .context = "NUMERIC_DETECT"
        )
      }
    }
  }

  log_debug_kv(
    numeric_columns_found = length(numeric_cols),
    numeric_column_names = paste(numeric_cols, collapse = ", "),
    .context = "NUMERIC_DETECT"
  )

  return(numeric_cols)
}

#' Score Column Candidates for Specific Roles
#'
#' Scores numeric columns for suitability as Y (count) or N (denominator) columns.
#' Uses name patterns, data characteristics, and statistical properties.
#'
#' @param data Data frame containing the columns
#' @param numeric_candidates Character vector of numeric column names
#' @param role Either "y_column" or "n_column"
#' @return Named numeric vector of scores (higher = better)
score_column_candidates <- function(data, numeric_candidates, role = c("y_column", "n_column")) {
  role <- match.arg(role)

  log_debug_block("COLUMN_SCORING", paste("Scoring columns for", role))

  if (length(numeric_candidates) == 0) {
    return(numeric(0))
  }

  scores <- setNames(numeric(length(numeric_candidates)), numeric_candidates)

  for (col_name in numeric_candidates) {
    col_data <- data[[col_name]]
    score <- 0

    # 1. NAME-BASED SCORING (30% weight)
    name_score <- score_by_name_patterns(col_name, role) * 0.3

    # 2. DATA CHARACTERISTICS (40% weight)
    char_score <- score_by_data_characteristics(col_data, role) * 0.4

    # 3. STATISTICAL PROPERTIES (30% weight)
    stat_score <- score_by_statistical_properties(col_data, role) * 0.3

    total_score <- name_score + char_score + stat_score
    scores[col_name] <- total_score

    log_debug_kv(
      column = col_name,
      role = role,
      name_score = round(name_score, 3),
      char_score = round(char_score, 3),
      stat_score = round(stat_score, 3),
      total_score = round(total_score, 3),
      .context = "COLUMN_SCORING"
    )
  }

  # Sort by score (descending)
  scores <- sort(scores, decreasing = TRUE)

  log_debug_kv(
    role = role,
    best_candidate = if (length(scores) > 0) names(scores)[1] else "none",
    best_score = if (length(scores) > 0) round(scores[1], 3) else 0,
    .context = "COLUMN_SCORING"
  )

  return(scores)
}

#' Score Column Name Patterns
#'
#' Scores column names based on pattern matching for specific roles.
#'
#' @param col_name Column name to score
#' @param role Target role ("y_column" or "n_column")
#' @return Score between 0 and 1
score_by_name_patterns <- function(col_name, role) {
  col_lower <- tolower(col_name)

  if (role == "y_column") {
    # Patterns for count/value columns
    y_patterns <- c("tæller", "tael", "count", "num", "antal", "værdi", "value")
    for (pattern in y_patterns) {
      if (grepl(pattern, col_lower)) {
        return(1.0)  # Perfect match
      }
    }

    # Partial matches
    partial_patterns <- c("sum", "total", "rate", "procent", "pct")
    for (pattern in partial_patterns) {
      if (grepl(pattern, col_lower)) {
        return(0.7)  # Good match
      }
    }
  } else if (role == "n_column") {
    # Patterns for denominator columns
    n_patterns <- c("nævner", "naev", "denom", "total", "samlet")
    for (pattern in n_patterns) {
      if (grepl(pattern, col_lower)) {
        return(1.0)  # Perfect match
      }
    }

    # Partial matches
    partial_patterns <- c("sum", "basis", "base")
    for (pattern in partial_patterns) {
      if (grepl(pattern, col_lower)) {
        return(0.7)  # Good match
      }
    }
  }

  return(0.0)  # No pattern match
}

#' Score Data Characteristics
#'
#' Scores columns based on data type and range characteristics.
#'
#' @param col_data Vector of column data
#' @param role Target role ("y_column" or "n_column")
#' @return Score between 0 and 1
score_by_data_characteristics <- function(col_data, role) {
  # Remove missing values
  clean_data <- col_data[!is.na(col_data)]
  if (length(clean_data) == 0) return(0)

  # Convert to numeric if needed
  if (!is.numeric(clean_data)) {
    clean_data <- suppressWarnings(as.numeric(as.character(clean_data)))
    clean_data <- clean_data[!is.na(clean_data)]
    if (length(clean_data) == 0) return(0)
  }

  score <- 0

  # Positive values preferred
  if (all(clean_data >= 0)) {
    score <- score + 0.3
  }

  # Integer values often better for counts
  if (all(clean_data == floor(clean_data))) {
    score <- score + 0.2
  }

  # Reasonable range (not too extreme)
  data_range <- max(clean_data) - min(clean_data)
  if (data_range > 0 && data_range < 10000) {  # Reasonable for most SPC contexts
    score <- score + 0.3
  }

  # Role-specific scoring
  if (role == "y_column") {
    # Y columns often have variability
    if (length(unique(clean_data)) > 1) {
      score <- score + 0.2
    }
  } else if (role == "n_column") {
    # N columns might be more stable
    cv <- sd(clean_data) / mean(clean_data)
    if (cv < 0.5) {  # Lower coefficient of variation
      score <- score + 0.2
    }
  }

  return(min(score, 1.0))  # Cap at 1.0
}

#' Score Statistical Properties
#'
#' Scores columns based on statistical properties relevant to SPC analysis.
#'
#' @param col_data Vector of column data
#' @param role Target role ("y_column" or "n_column")
#' @return Score between 0 and 1
score_by_statistical_properties <- function(col_data, role) {
  # Remove missing values
  clean_data <- col_data[!is.na(col_data)]
  if (length(clean_data) < 2) return(0)

  # Convert to numeric if needed
  if (!is.numeric(clean_data)) {
    clean_data <- suppressWarnings(as.numeric(as.character(clean_data)))
    clean_data <- clean_data[!is.na(clean_data)]
    if (length(clean_data) < 2) return(0)
  }

  score <- 0

  # Basic statistical properties
  mean_val <- mean(clean_data)
  sd_val <- sd(clean_data)

  # Non-zero mean and standard deviation
  if (mean_val > 0) score <- score + 0.2
  if (sd_val > 0) score <- score + 0.2

  # Reasonable distribution (not too skewed)
  if (length(clean_data) >= 5) {
    # Simple skewness check
    median_val <- median(clean_data)
    if (abs(mean_val - median_val) / sd_val < 1) {  # Not too skewed
      score <- score + 0.3
    }
  }

  # Role-specific properties
  if (role == "y_column") {
    # Y columns should have some variability for meaningful SPC
    cv <- sd_val / mean_val
    if (cv > 0.1 && cv < 2) {  # Reasonable coefficient of variation
      score <- score + 0.3
    }
  } else if (role == "n_column") {
    # N columns often larger than Y columns (for rates)
    if (mean_val >= 10) {  # Reasonable denominator size
      score <- score + 0.3
    }
  }

  return(min(score, 1.0))  # Cap at 1.0
}