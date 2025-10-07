# Performance Helper Functions
# Supporting functions for performance optimizations

#' Appears Numeric
#'
#' Check if a character vector appears to contain numeric data
#'
#' @param x Character vector to check
#' @return Logical indicating if data appears numeric
#'
appears_numeric <- function(x) {
  # TEST FIX: Accept numeric columns as numeric
  if (is.numeric(x)) {
    return(TRUE)
  }

  if (!is.character(x)) {
    return(FALSE)
  }

  # Sample a few values for efficiency
  sample_size <- min(10, length(x))
  sample_data <- x[!is.na(x)][1:sample_size]

  if (length(sample_data) == 0) {
    return(FALSE)
  }

  # Check if values can be converted to numeric
  suppressWarnings({
    converted <- as.numeric(gsub("[,.]", ".", gsub("[^0-9,.-]", "", sample_data)))
    success_rate <- sum(!is.na(converted)) / length(sample_data)
  })

  return(success_rate > 0.7) # 70% success rate threshold
}

#' Appears Date
#'
#' Check if a character vector appears to contain date data
#'
#' @param x Character vector to check
#' @return Logical indicating if data appears to be dates
#'
appears_date <- function(x) {
  if (!is.character(x)) {
    return(FALSE)
  }

  # Sample a few values for efficiency
  sample_size <- min(10, length(x))
  sample_data <- x[!is.na(x)][1:sample_size]

  if (length(sample_data) == 0) {
    return(FALSE)
  }

  # Common date patterns
  date_patterns <- c(
    "\\d{4}-\\d{2}-\\d{2}", # YYYY-MM-DD
    "\\d{2}-\\d{2}-\\d{4}", # DD-MM-YYYY
    "\\d{2}/\\d{2}/\\d{4}", # DD/MM/YYYY
    "\\d{4}/\\d{2}/\\d{2}" # YYYY/MM/DD
  )

  # Check pattern matches
  pattern_matches <- 0
  for (pattern in date_patterns) {
    if (any(grepl(pattern, sample_data))) {
      pattern_matches <- pattern_matches + 1
    }
  }

  # TEST FIX: Try actual date parsing with tryCatch to handle errors
  parse_success_rate <- 0
  tryCatch(
    {
      suppressWarnings({
        parsed_dates <- as.Date(sample_data)
        parse_success_rate <- sum(!is.na(parsed_dates)) / length(sample_data)
      })
    },
    error = function(e) {
      # If parsing fails completely, treat as non-date
      parse_success_rate <<- 0
    }
  )

  return(pattern_matches > 0 || parse_success_rate > 0.5)
}

#' Parse Danish Number Vectorized
#'
#' Efficiently parse Danish number format with vectorized operations
#'
#' @param x Character vector with Danish number format
#' @return Numeric vector
#'
parse_danish_number_vectorized <- function(x) {
  if (!is.character(x)) {
    return(as.numeric(x))
  }

  # Vectorized cleaning and conversion
  # Replace Danish decimal comma with period
  cleaned <- gsub(",", ".", x)
  # Remove thousands separators (space or period when not decimal)
  cleaned <- gsub("\\s+", "", cleaned)
  # Convert to numeric
  suppressWarnings(as.numeric(cleaned))
}

#' Parse Danish Date Vectorized
#'
#' Efficiently parse Danish date format with vectorized operations
#'
#' @param x Character vector with Danish date format
#' @return Date vector
#'
parse_danish_date_vectorized <- function(x) {
  if (!is.character(x)) {
    return(as.Date(x))
  }

  # Try multiple date formats common in Danish data
  date_formats <- c(
    "%d-%m-%Y", # DD-MM-YYYY
    "%d/%m/%Y", # DD/MM/YYYY
    "%Y-%m-%d", # YYYY-MM-DD
    "%Y/%m/%d", # YYYY/MM/DD
    "%d.%m.%Y" # DD.MM.YYYY
  )

  result <- rep(as.Date(NA), length(x))

  for (format in date_formats) {
    if (all(!is.na(result))) break # All dates parsed successfully

    missing_indices <- is.na(result)
    if (any(missing_indices)) {
      suppressWarnings({
        parsed <- as.Date(x[missing_indices], format = format)
        success_indices <- !is.na(parsed)
        result[missing_indices][success_indices] <- parsed[success_indices]
      })
    }
  }

  return(result)
}

#' Ensure Standard Columns
#'
#' Ensure data has standard column naming and structure
#'
#' @param data Data frame to standardize
#' @return Standardized data frame
#' @export
ensure_standard_columns <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(data)
  }

  # NOTE: Removed automatic empty column removal as empty columns can be meaningful
  # Empty columns are preserved to allow users to work with their data structure as intended

  # Only ensure reasonable column names (remove problematic characters, ensure uniqueness)
  names(data) <- make.names(names(data), unique = TRUE)

  return(data)
}

#' Validate Numeric Column
#'
#' Check if a column contains valid numeric data
#'
#' @param data Data frame to check
#' @param column_name Name of column to validate
#' @return NULL if valid, error message if invalid
#' @export
validate_numeric_column <- function(data, column_name) {
  if (!column_name %in% names(data)) {
    return(paste("Column", column_name, "not found"))
  }

  column_data <- data[[column_name]]
  if (is.numeric(column_data)) {
    return(NULL)
  }

  # Try to convert to numeric
  tryCatch(
    {
      as.numeric(column_data)
      return(NULL)
    },
    error = function(e) {
      return(paste("Column", column_name, "cannot be converted to numeric"))
    },
    warning = function(w) {
      return(NULL) # Warnings (like NAs) are acceptable
    }
  )
}

#' Safe Date Parse
#'
#' Safely parse dates with multiple format attempts
#'
#' @param date_vector Vector of dates to parse
#' @return Parsed dates or original vector if parsing fails
#' @export
safe_date_parse <- function(date_vector) {
  if (is.null(date_vector) || length(date_vector) == 0) {
    return(date_vector)
  }

  # Common Danish date formats
  formats <- c(
    "%d-%m-%Y", # DD-MM-YYYY
    "%d/%m/%Y", # DD/MM/YYYY
    "%Y-%m-%d", # YYYY-MM-DD
    "%d.%m.%Y", # DD.MM.YYYY
    "%d-%m-%y", # DD-MM-YY
    "%d/%m/%y" # DD/MM/YY
  )

  for (fmt in formats) {
    tryCatch(
      {
        parsed <- as.Date(date_vector, format = fmt)
        if (sum(!is.na(parsed)) > 0) {
          return(parsed)
        }
      },
      error = function(e) {
        # Continue to next format
      }
    )
  }

  # If all formats fail, try automatic parsing
  tryCatch(
    {
      return(as.Date(date_vector))
    },
    error = function(e) {
      return(date_vector) # Return original if all parsing fails
    }
  )
}

#' Validate SPC Requirements
#'
#' Validate that data meets basic SPC requirements
#'
#' @param data Data frame to validate
#' @return Validated data frame or error
#'
validate_spc_requirements <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    stop("Data er tomt eller ikke tilgængeligt")
  }

  if (ncol(data) < 2) {
    stop("Data skal have mindst 2 kolonner for SPC analyse")
  }

  if (nrow(data) < 3) {
    stop("Data skal have mindst 3 rækker for SPC analyse")
  }

  return(data)
}

#' Add Comments Optimized
#'
#' Add comments to plot with optimization - matches extract_comment_data approach
#'
#' @param plot ggplot object
#' @param data Original data frame
#' @param kommentar_column Column name for comments
#' @param qic_data QIC processed data with transformed coordinates
#'
add_comments_optimized <- function(plot, data, kommentar_column, qic_data) {
  if (is.null(kommentar_column) || !kommentar_column %in% names(data) || is.null(qic_data)) {
    return(plot)
  }

  # Use same approach as extract_comment_data for consistency
  comments_raw <- data[[kommentar_column]]

  # Create comment data frame aligned with qic_data
  comment_data <- data.frame(
    x = qic_data$x,
    y = qic_data$y,
    comment = comments_raw[1:nrow(qic_data)], # Ensure same length as qic_data
    stringsAsFactors = FALSE
  )

  # Filter to only non-empty comments
  comment_data <- comment_data[
    !is.na(comment_data$comment) &
      trimws(comment_data$comment) != "",
  ]

  if (nrow(comment_data) == 0) {
    return(plot)
  }

  # Truncate very long comments
  if (nrow(comment_data) > 0) {
    comment_data$comment <- dplyr::if_else(
      nchar(comment_data$comment) > 40,
      stringr::str_c(substr(comment_data$comment, 1, 37), "..."),
      comment_data$comment
    )
  }

  # Get hospital colors for consistent styling
  hospital_colors <- get_hospital_colors()

  # Add comments with ggrepel for better positioning
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    plot <- plot + ggrepel::geom_text_repel(
      data = comment_data,
      ggplot2::aes(x = x, y = y, label = comment),
      size = 3,
      color = hospital_colors$darkgrey,
      bg.color = "white",
      bg.r = 0.1,
      box.padding = 0.5,
      point.padding = 0.5,
      segment.color = hospital_colors$mediumgrey,
      segment.size = 0.3,
      nudge_x = .15,
      nudge_y = .5,
      segment.curvature = -1e-20,
      arrow = grid::arrow(length = grid::unit(0.015, "npc")),
      max.overlaps = Inf,
      inherit.aes = FALSE
    )
  } else {
    # Fallback without ggrepel - use same styling as main function
    plot <- plot + ggplot2::geom_text(
      data = comment_data,
      ggplot2::aes(x = x, y = y, label = comment),
      size = 3,
      color = hospital_colors$darkgrey,
      vjust = -0.5
    )
  }

  return(plot)
}

#' Should Garbage Collect
#'
#' Intelligent decision on when to run garbage collection
#'
#' @return Logical indicating if GC should be run
#'
should_gc <- function() {
  # Check memory usage
  mem_info <- gc(verbose = FALSE)
  total_mb <- sum(mem_info[, "used"]) * 1.048576 # Convert to MB

  # Run GC if memory usage is high (> 500MB) or every 10th call
  static_gc_counter <- get("gc_counter", envir = .GlobalEnv, inherits = FALSE)
  if (is.null(static_gc_counter)) {
    static_gc_counter <- 0
  }

  static_gc_counter <- static_gc_counter + 1
  assign("gc_counter", static_gc_counter, envir = .GlobalEnv)

  return(total_mb > 500 || static_gc_counter %% 10 == 0)
}

#' Performance Monitoring Utilities
#'

#' Start Performance Timer
#'
#' @param operation_name Name of operation being timed
#' @return Start time
#'
start_perf_timer <- function(operation_name) {
  start_time <- Sys.time()
  # log_debug(paste("Started:", operation_name), "PERFORMANCE_TIMER")
  return(start_time)
}

#' End Performance Timer
#'
#' @param start_time Start time from start_perf_timer
#' @param operation_name Name of operation being timed
#'
end_perf_timer <- function(start_time, operation_name) {
  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
  # log_debug(paste("Completed:", operation_name, "in", round(duration, 3), "seconds"), "PERFORMANCE_TIMER")
  return(duration)
}

#' Cache Statistics
#'
#' Get statistics about cache usage
#'
get_cache_statistics <- function() {
  # Count cache entries
  data_cache_count <- length(ls(envir = .cache_env))
  plot_cache_count <- length(ls(envir = .plot_cache_env))

  # Calculate cache sizes (approximate)
  data_cache_size <- object.size(.cache_env)
  plot_cache_size <- object.size(.plot_cache_env)

  list(
    data_cache_entries = data_cache_count,
    plot_cache_entries = plot_cache_count,
    data_cache_size_mb = as.numeric(data_cache_size) / 1024^2,
    plot_cache_size_mb = as.numeric(plot_cache_size) / 1024^2,
    total_cache_size_mb = as.numeric(data_cache_size + plot_cache_size) / 1024^2
  )
}

#' Clear Performance Caches
#'
#' Clear all performance caches to free memory
#'
clear_performance_caches <- function() {
  # Clear data cache
  rm(list = ls(envir = .cache_env), envir = .cache_env)

  # Clear plot cache
  rm(list = ls(envir = .plot_cache_env), envir = .plot_cache_env)

  # Run garbage collection
  gc(verbose = FALSE)

  # log_debug("Performance caches cleared", .context = "PERFORMANCE")
}
