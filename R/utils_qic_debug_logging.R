# utils_qic_debug_logging.R
# Detaljeret debug logging for qicharts2::qic() kald
# Bruges til troubleshooting af anhoej-rules og qic parametre

#' Log QIC input parametre til debugging
#'
#' Logger alle input parametre til qicharts2::qic() kald for debugging.
#' Kun aktiv ved DEBUG log niveau for at undgå performance impact.
#'
#' @param qic_args Liste med argumenter til qic() kaldet
#' @param call_context Kontekst for kaldet (f.eks. "generateSPCPlot", "server_optimization")
#' @param call_number Valgfrit kald nummer for sporing
#'
#' @return invisible(NULL)
#' @export
log_qic_inputs <- function(qic_args, call_context = "UNKNOWN", call_number = NULL) {
  if (!.should_log("DEBUG")) {
    return(invisible(NULL))
  }

  log_debug_block("QIC_INPUT", paste("QIC INPUT LOGGING - Context:", call_context))

  # Basic call information
  if (!is.null(call_number)) {
    log_debug(paste("Call number:", call_number), .context = "QIC_INPUT")
  }
  log_debug(paste("Context:", call_context), .context = "QIC_INPUT")

  # Chart type and basic parameters
  chart_type <- qic_args$chart %||% "UNKNOWN"
  log_debug(paste("Chart type:", chart_type), .context = "QIC_INPUT")

  # Data information
  if (!is.null(qic_args$data)) {
    data_info <- safe_operation(
      "Log data structure",
      code = {
        paste("Data rows:", nrow(qic_args$data), "| cols:", ncol(qic_args$data))
      },
      fallback = function(e) "Data structure unknown"
    )
    log_debug(data_info, .context = "QIC_INPUT")

    # Column names if data provided
    if (is.data.frame(qic_args$data)) {
      log_debug(paste("Column names:", paste(names(qic_args$data), collapse = ", ")),
        .context = "QIC_INPUT"
      )
    }
  }

  # Column specifications (NSE)
  if (!is.null(qic_args$x)) {
    x_col <- if (is.name(qic_args$x)) as.character(qic_args$x) else as.character(qic_args$x)
    log_debug(paste("X column:", x_col), .context = "QIC_INPUT")
  }

  if (!is.null(qic_args$y)) {
    y_col <- if (is.name(qic_args$y)) as.character(qic_args$y) else as.character(qic_args$y)
    log_debug(paste("Y column:", y_col), .context = "QIC_INPUT")
  }

  if (!is.null(qic_args$n)) {
    n_col <- if (is.name(qic_args$n)) as.character(qic_args$n) else as.character(qic_args$n)
    log_debug(paste("N column:", n_col), .context = "QIC_INPUT")
  }

  # Control parameters
  if (!is.null(qic_args$freeze)) {
    log_debug(paste("Freeze position:", qic_args$freeze), .context = "QIC_INPUT")
  }

  if (!is.null(qic_args$part)) {
    part_str <- if (length(qic_args$part) > 0) {
      paste("[", paste(qic_args$part, collapse = ", "), "]")
    } else {
      "[]"
    }
    log_debug(paste("Part positions:", part_str), .context = "QIC_INPUT")
  }

  if (!is.null(qic_args$cl)) {
    log_debug(paste("Centerline:", qic_args$cl), .context = "QIC_INPUT")
  }

  if (!is.null(qic_args$target)) {
    log_debug(paste("Target:", qic_args$target), .context = "QIC_INPUT")
  }

  # Return data flag
  return_data <- qic_args$return.data %||% FALSE
  log_debug(paste("Return data:", return_data), .context = "QIC_INPUT")

  # Sample data values (first few rows for debugging)
  safe_operation(
    "Log sample data values",
    code = {
      if (!is.null(qic_args$data) && is.data.frame(qic_args$data)) {
        # Y values sample
        if (!is.null(qic_args$y)) {
          y_col_name <- if (is.name(qic_args$y)) as.character(qic_args$y) else as.character(qic_args$y)
          if (y_col_name %in% names(qic_args$data)) {
            y_values <- qic_args$data[[y_col_name]]
            if (length(y_values) > 0) {
              sample_y <- head(y_values, 5)
              log_debug(paste("Y sample values:", paste(sample_y, collapse = ", ")),
                .context = "QIC_INPUT"
              )
            }
          }
        }

        # N values sample (if ratio chart)
        if (!is.null(qic_args$n)) {
          n_col_name <- if (is.name(qic_args$n)) as.character(qic_args$n) else as.character(qic_args$n)
          if (n_col_name %in% names(qic_args$data)) {
            n_values <- qic_args$data[[n_col_name]]
            if (length(n_values) > 0) {
              sample_n <- head(n_values, 5)
              log_debug(paste("N sample values:", paste(sample_n, collapse = ", ")),
                .context = "QIC_INPUT"
              )
            }
          }
        }
      }
    },
    fallback = function(e) {
      log_debug("Could not extract sample data values", .context = "QIC_INPUT")
    }
  )

  log_debug_block("QIC_INPUT", "QIC input logging completed", type = "stop")
  invisible(NULL)
}

#' Log QIC output resultater til debugging
#'
#' Logger alle resultater fra qicharts2::qic() kald inklusiv anhoej-rules.
#' Kun aktiv ved DEBUG log niveau.
#'
#' @param qic_result Resultat fra qic() kaldet
#' @param call_context Kontekst for kaldet
#' @param call_number Valgfrit kald nummer for sporing
#' @param success Logical indicating if qic call succeeded
#'
#' @return invisible(NULL)
#' @export
log_qic_results <- function(qic_result, call_context = "UNKNOWN", call_number = NULL, success = TRUE) {
  if (!.should_log("DEBUG")) {
    return(invisible(NULL))
  }

  log_debug_block("QIC_RESULT", paste("QIC RESULT LOGGING - Context:", call_context))

  # Basic call information
  if (!is.null(call_number)) {
    log_debug(paste("Call number:", call_number), .context = "QIC_RESULT")
  }
  log_debug(paste("Context:", call_context), .context = "QIC_RESULT")
  log_debug(paste("Success:", success), .context = "QIC_RESULT")

  if (!success || is.null(qic_result)) {
    log_debug("QIC call failed or returned NULL", .context = "QIC_RESULT")
    log_debug_block("QIC_RESULT", "QIC result logging completed (failed)", type = "stop")
    return(invisible(NULL))
  }

  # Determine result type
  result_type <- safe_operation(
    "Determine result type",
    code = {
      if (inherits(qic_result, "ggplot")) {
        "ggplot"
      } else if (is.data.frame(qic_result)) {
        "data.frame"
      } else if (is.list(qic_result) && !is.null(qic_result$data)) {
        "qic_object_with_data"
      } else {
        paste("unknown:", class(qic_result)[1])
      }
    },
    fallback = function(e) "unknown"
  )
  log_debug(paste("Result type:", result_type), .context = "QIC_RESULT")

  # Extract data for analysis
  qic_data <- safe_operation(
    "Extract qic data",
    code = {
      if (is.data.frame(qic_result)) {
        qic_result
      } else if (is.list(qic_result) && !is.null(qic_result$data)) {
        qic_result$data
      } else {
        NULL
      }
    },
    fallback = function(e) NULL
  )

  if (!is.null(qic_data) && is.data.frame(qic_data)) {
    # Basic data structure
    log_debug(paste("Result rows:", nrow(qic_data), "| cols:", ncol(qic_data)),
      .context = "QIC_RESULT"
    )
    log_debug(paste("Column names:", paste(names(qic_data), collapse = ", ")),
      .context = "QIC_RESULT"
    )

    # Core QIC columns
    qic_columns <- c("y", "cl", "ucl", "lcl")
    available_qic_cols <- intersect(qic_columns, names(qic_data))
    if (length(available_qic_cols) > 0) {
      log_debug(paste("QIC columns present:", paste(available_qic_cols, collapse = ", ")),
        .context = "QIC_RESULT"
      )

      # Sample values from core columns
      for (col in available_qic_cols) {
        col_values <- qic_data[[col]]
        if (length(col_values) > 0) {
          sample_vals <- head(col_values, 5)
          na_count <- sum(is.na(col_values))
          log_debug(
            paste(
              col, "sample:", paste(sample_vals, collapse = ", "),
              "| NA count:", na_count, "/", length(col_values)
            ),
            .context = "QIC_RESULT"
          )
        }
      }
    }

    # ANHOEJ RULES ANALYSIS - Dette er kritisk for debugging
    anhoej_columns <- c("longest.run", "n.crossings", "n.runs", "runs.signal")
    available_anhoej_cols <- intersect(anhoej_columns, names(qic_data))

    if (length(available_anhoej_cols) > 0) {
      log_debug("=== ANHOEJ RULES ANALYSIS ===", .context = "QIC_RESULT")
      log_debug(paste("Anhoej columns present:", paste(available_anhoej_cols, collapse = ", ")),
        .context = "QIC_RESULT"
      )

      for (col in available_anhoej_cols) {
        col_values <- qic_data[[col]]
        if (length(col_values) > 0) {
          na_count <- sum(is.na(col_values))
          non_na_values <- col_values[!is.na(col_values)]

          if (length(non_na_values) > 0) {
            if (col %in% c("longest.run", "n.crossings", "n.runs")) {
              # Numeric anhoej columns
              log_debug(
                paste(
                  col, "- NA count:", na_count, "/", length(col_values),
                  "| Non-NA values:", paste(head(non_na_values, 10), collapse = ", ")
                ),
                .context = "QIC_RESULT"
              )
            } else {
              # Logical/other anhoej columns
              log_debug(
                paste(
                  col, "- NA count:", na_count, "/", length(col_values),
                  "| Unique values:", paste(unique(head(non_na_values, 10)), collapse = ", ")
                ),
                .context = "QIC_RESULT"
              )
            }
          } else {
            log_debug(paste(col, "- ALL VALUES ARE NA (", na_count, "/", length(col_values), ")"),
              .context = "QIC_RESULT"
            )
          }
        }
      }

      # Extract final anhoej results (typically from first/last row)
      if ("longest.run" %in% names(qic_data)) {
        longest_run_final <- safe_operation(
          "Extract longest.run final value",
          code = {
            # qicharts2 typically puts final results in the last non-NA value
            longest_vals <- qic_data$longest.run[!is.na(qic_data$longest.run)]
            if (length(longest_vals) > 0) longest_vals[length(longest_vals)] else NA
          },
          fallback = function(e) NA
        )
        log_debug(paste("FINAL longest.run value:", longest_run_final), .context = "QIC_RESULT")
      }

      if ("n.crossings" %in% names(qic_data)) {
        n_crossings_final <- safe_operation(
          "Extract n.crossings final value",
          code = {
            crossings_vals <- qic_data$n.crossings[!is.na(qic_data$n.crossings)]
            if (length(crossings_vals) > 0) crossings_vals[length(crossings_vals)] else NA
          },
          fallback = function(e) NA
        )
        log_debug(paste("FINAL n.crossings value:", n_crossings_final), .context = "QIC_RESULT")
      }

      log_debug("=== END ANHOEJ RULES ANALYSIS ===", .context = "QIC_RESULT")
    } else {
      log_debug("WARNING: No Anhoej rules columns found in result", .context = "QIC_RESULT")
    }

    # Additional diagnostic information
    if (".original_row_id" %in% names(qic_data)) {
      log_debug("Row ID tracking: Present (.original_row_id column found)", .context = "QIC_RESULT")
    } else {
      log_debug("Row ID tracking: Missing (.original_row_id column not found)", .context = "QIC_RESULT")
    }
  } else {
    log_debug("No data.frame available in qic result for detailed analysis", .context = "QIC_RESULT")
  }

  log_debug_block("QIC_RESULT", "QIC result logging completed", type = "stop")
  invisible(NULL)
}

#' Wrapper funktion der logger qic() kald før og efter
#'
#' Wrapper omkring qicharts2::qic() kald der automatisk logger
#' input parametre og output resultater for debugging.
#'
#' @param qic_args Liste med argumenter til qic()
#' @param call_context Kontekst beskrivelse
#' @param call_number Valgfrit kald nummer
#' @param ... Ekstra argumenter (ikke brugt)
#'
#' @return Resultat fra qic() kaldet
#' @export
log_qic_call_wrapper <- function(qic_args, call_context = "UNKNOWN", call_number = NULL, qic_cache = NULL, ...) {
  # SPRINT 4: Check if caching is enabled and available
  use_cache <- !is.null(qic_cache) && !is.null(qic_args$data)

  if (use_cache) {
    # Extract parameters for cache key generation
    cache_params <- list(
      x = qic_args$x,
      y = qic_args$y,
      chart = qic_args$chart,
      n = qic_args$n,
      cl = qic_args$cl,
      target = qic_args$target,
      multiply = qic_args$multiply,
      freeze = qic_args$freeze,
      breaks = qic_args$breaks,
      exclude = qic_args$exclude,
      part = qic_args$part
    )

    cache_key <- generate_qic_cache_key(qic_args$data, cache_params)

    # Try cache first
    cached_result <- qic_cache$get(cache_key)
    if (!is.null(cached_result)) {
      log_debug_kv(
        message = "QIC cache hit",
        cache_key = substr(cache_key, 1, 20),
        context = call_context,
        .context = "QIC_CACHE"
      )
      return(cached_result)
    }

    # Cache miss - log it
    log_debug_kv(
      message = "QIC cache miss - computing",
      cache_key = substr(cache_key, 1, 20),
      context = call_context,
      .context = "QIC_CACHE"
    )
  }

  if (!.should_log("DEBUG")) {
    # Ved ikke-DEBUG niveau, kør qic() direkte uden logging overhead
    result <- do.call(qicharts2::qic, qic_args)

    # SPRINT 4: Cache the result if caching is enabled
    if (use_cache) {
      qic_cache$set(cache_key, result)
    }

    return(result)
  }

  # Pre-call logging
  log_qic_inputs(qic_args, call_context, call_number)

  # Execute qic() call med error handling
  qic_result <- safe_operation(
    paste("QIC call execution -", call_context),
    code = {
      start_time <- Sys.time()
      result <- do.call(qicharts2::qic, qic_args)
      end_time <- Sys.time()

      execution_time <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 3)
      log_debug(paste("QIC execution time:", execution_time, "seconds"), .context = "QIC_TIMING")

      # SPRINT 4: Cache the result if caching is enabled
      if (use_cache) {
        qic_cache$set(cache_key, result, timeout = CACHE_CONFIG$default_timeout_seconds)
        log_debug_kv(
          message = "QIC result cached",
          cache_key = substr(cache_key, 1, 20),
          computation_time_ms = round(execution_time * 1000, 2),
          .context = "QIC_CACHE"
        )
      }

      result
    },
    fallback = function(e) {
      log_error(paste("QIC call failed:", e$message), .context = "QIC_ERROR")
      NULL
    }
  )

  # Post-call logging
  success <- !is.null(qic_result)
  log_qic_results(qic_result, call_context, call_number, success)

  return(qic_result)
}

#' Log sammenligning mellem forventede og faktiske anhoej resultater
#'
#' Hjælpefunktion til debugging af anhoej-rules diskrepanser.
#' Sammenligner forventede værdier med faktiske qic() output.
#'
#' @param qic_data Data fra qic() kald
#' @param expected_longest_run Forventet longest.run værdi
#' @param expected_n_crossings Forventet n.crossings værdi
#' @param context Kontekst for sammenligningen
#'
#' @return invisible(NULL)
#' @export
log_anhoej_comparison <- function(qic_data, expected_longest_run = NULL,
                                  expected_n_crossings = NULL, context = "COMPARISON") {
  if (!.should_log("DEBUG") || is.null(qic_data)) {
    return(invisible(NULL))
  }

  log_debug_block("ANHOEJ_COMPARISON", paste("Anhoej rules comparison -", context))

  if ("longest.run" %in% names(qic_data)) {
    actual_longest <- qic_data$longest.run[!is.na(qic_data$longest.run)]
    actual_final <- if (length(actual_longest) > 0) actual_longest[length(actual_longest)] else NA

    log_debug(
      paste(
        "Longest run - Expected:", expected_longest_run,
        "| Actual:", actual_final,
        "| Match:", identical(expected_longest_run, actual_final)
      ),
      .context = "ANHOEJ_COMPARISON"
    )
  }

  if ("n.crossings" %in% names(qic_data)) {
    actual_crossings <- qic_data$n.crossings[!is.na(qic_data$n.crossings)]
    actual_final <- if (length(actual_crossings) > 0) actual_crossings[length(actual_crossings)] else NA

    log_debug(
      paste(
        "N crossings - Expected:", expected_n_crossings,
        "| Actual:", actual_final,
        "| Match:", identical(expected_n_crossings, actual_final)
      ),
      .context = "ANHOEJ_COMPARISON"
    )
  }

  log_debug_block("ANHOEJ_COMPARISON", "Anhoej comparison completed", type = "stop")
  invisible(NULL)
}
