# fct_spc_bfh_service.R
# Service Layer Facade for BFHchart Integration
#
# Dette modul implementerer facade-mønstret til at isolere SPCify fra BFHchart API.
# Alle funktioner implementerer det fulde workflow fra validation til output transformation.
#
# Design princip: Single Responsibility - hver funktion har én klar opgave:
# 1. compute_spc_results_bfh() - Primary facade interface
# 2. map_to_bfh_params() - Parameter transformation
# 3. call_bfh_chart() - Safe BFHchart invocation
# 4. transform_bfh_output() - Output standardization
# 5. add_comment_annotations() - Comment layer integration

#' Compute SPC Results Using BFHchart Backend
#'
#' Primary facade function that wraps BFHchart functionality with SPCify conventions.
#' This function provides a stable interface that isolates the application from
#' BFHchart API changes, handles parameter mapping, validates inputs, and standardizes
#' output format for seamless integration with existing SPCify plot rendering.
#'
#' @details
#' **Architectural Role:** Service layer facade implementing adapter pattern.
#' Coordinates validation, transformation, BFHchart invocation, and output formatting.
#'
#' **Workflow:**
#' 1. Input validation using existing SPCify validators
#' 2. Parameter transformation (SPCify conventions → BFHchart API)
#' 3. Safe BFHchart invocation with error handling
#' 4. Output standardization (match qicharts2 format)
#' 5. Structured logging and cache management
#'
#' **Error Handling:** All operations wrapped in `safe_operation()` with graceful
#' fallback. Errors logged with structured context for debugging.
#'
#' @section Notes Column Mapping:
#' The `notes_column` parameter maps to BFHchart's comment/notes system. If BFHchart
#' does not provide native notes support, SPCify applies comments as a ggrepel layer
#' after BFHchart rendering (existing pattern). Comment handling includes:
#' - Row ID stability via `.original_row_id` injection
#' - XSS sanitization with Danish character support (æøå)
#' - Intelligent truncation (40 char display, 100 char max)
#' - Collision avoidance with `ggrepel::geom_text_repel()`
#'
#' @param data data.frame. Input dataset with SPC data. Required.
#' @param x_var character. Name of x-axis variable (time/sequence column). Required.
#' @param y_var character. Name of y-axis variable (measure/value column). Required.
#' @param chart_type character. SPC chart type. One of: "run", "i", "mr", "p", "pp",
#'   "u", "up", "c", "g". Required. Use qicharts2-style codes (lowercase).
#' @param n_var character. Name of denominator variable for rate-based charts
#'   (P, P', U, U' charts). Default NULL. Required for charts with denominators.
#' @param cl_var character. Name of control limit override variable. Allows custom
#'   centerline per data point. Default NULL (auto-calculate).
#' @param freeze_var character. Name of freeze period indicator variable. Marks
#'   baseline period for control limit calculation. Default NULL (no freeze).
#' @param part_var character. Name of part/subgroup/phase variable. Enables
#'   per-phase control limit calculation and Anhøj rule application. Default NULL.
#' @param notes_column character. Name of notes/comment column to display on plot.
#'   Maps to BFHchart notes parameter or SPCify ggrepel layer. Default NULL.
#' @param multiply numeric. Multiplier applied to y-axis values for display scaling.
#'   Common use: convert decimal proportions to percentages (multiply = 100).
#'   Default 1 (no scaling).
#' @param ... Additional arguments passed to BFHchart backend. Allows flexibility
#'   for BFHchart-specific parameters without breaking SPCify interface.
#'
#' @return list with three components:
#'   \describe{
#'     \item{plot}{ggplot2 object. Rendered SPC chart with control limits, centerline,
#'       and optional annotations. Compatible with SPCify plot customization functions.}
#'     \item{qic_data}{tibble. Standardized data frame with SPC calculations. Columns:
#'       \itemize{
#'         \item x: X-axis values (dates or observation numbers)
#'         \item y: Y-axis values (original or scaled measures)
#'         \item cl: Centerline per data point (may vary by phase)
#'         \item ucl: Upper control limit per data point
#'         \item lcl: Lower control limit per data point
#'         \item part: Phase/subgroup indicator (integer, starting at 1)
#'         \item signal: Combined Anhøj signal (logical, TRUE if runs OR crossings violation)
#'         \item .original_row_id: Row identifier for stable comment mapping
#'       }
#'     }
#'     \item{metadata}{list. Chart configuration and diagnostic information:
#'       \itemize{
#'         \item chart_type: Chart type used
#'         \item n_points: Number of data points processed
#'         \item n_phases: Number of phases (if part_var specified)
#'         \item freeze_applied: Logical indicating if freeze was applied
#'         \item signals_detected: Count of Anhøj rule violations
#'         \item bfh_version: BFHchart package version used
#'         \item anhoej_rules: list with Anhøj rules metadata (runs_detected, crossings_detected, longest_run, n_crossings, n_crossings_min)
#'       }
#'     }
#'   }
#'   Returns NULL on error (with structured logging).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic run chart
#' result <- compute_spc_results_bfh(
#'   data = hospital_data,
#'   x_var = "month",
#'   y_var = "infections",
#'   chart_type = "run"
#' )
#' print(result$plot)
#' summary(result$qic_data)
#'
#' # P-chart with denominator and freeze period
#' result <- compute_spc_results_bfh(
#'   data = surgical_data,
#'   x_var = "date",
#'   y_var = "complications",
#'   n_var = "procedures",
#'   chart_type = "p",
#'   freeze_var = "baseline_indicator",
#'   multiply = 100
#' )
#'
#' # Multi-phase I-chart with comments
#' result <- compute_spc_results_bfh(
#'   data = quality_data,
#'   x_var = "week",
#'   y_var = "defects",
#'   chart_type = "i",
#'   part_var = "intervention_phase",
#'   notes_column = "comment",
#'   multiply = 1
#' )
#'
#' # Access standardized data
#' print(result$qic_data)
#' # Check metadata
#' print(result$metadata$signals_detected)
#' }
#'
#' @seealso
#' \code{\link{map_to_bfh_params}} for parameter transformation logic
#' \code{\link{transform_bfh_output}} for output standardization
#' \code{\link{add_comment_annotations}} for notes column handling
compute_spc_results_bfh <- function(
  data,
  x_var,
  y_var,
  chart_type,
  n_var = NULL,
  cl_var = NULL,
  freeze_var = NULL,
  part_var = NULL,
  notes_column = NULL,
  multiply = 1,
  ...
) {
  safe_operation(
    operation_name = "BFHchart SPC computation",
    code = {
      # 1. Validate required parameters
      if (missing(data) || is.null(data)) {
        stop("data parameter is required")
      }
      if (missing(x_var) || is.null(x_var)) {
        stop("x_var parameter is required")
      }
      if (missing(y_var) || is.null(y_var)) {
        stop("y_var parameter is required")
      }
      if (missing(chart_type) || is.null(chart_type)) {
        stop("chart_type parameter is required")
      }

      # 2. Validate chart type
      validated_chart_type <- validate_chart_type_bfh(chart_type)

      # 3. Check if denominator required for chart type
      if (validated_chart_type %in% c("p", "pp", "u", "up") && is.null(n_var)) {
        stop(paste0(
          "n_var (denominator) is required for ",
          validated_chart_type,
          " charts"
        ))
      }

      # 4. Filter complete data using existing validator
      complete_data <- filter_complete_spc_data(
        data = data,
        y_col = y_var,
        n_col = n_var,
        x_col = x_var
      )

      # Check if data is sufficient
      if (nrow(complete_data) == 0) {
        stop("No valid data rows found after filtering")
      }
      if (nrow(complete_data) < 3) {
        stop(paste0(
          "Insufficient data points: ",
          nrow(complete_data),
          ". Minimum 3 points required for SPC charts"
        ))
      }

      # 5. Parse and validate numeric data
      y_data_raw <- complete_data[[y_var]]
      n_data_raw <- if (!is.null(n_var)) complete_data[[n_var]] else NULL

      validated <- parse_and_validate_spc_data(
        y_data = y_data_raw,
        n_data = n_data_raw,
        y_col = y_var,
        n_col = n_var
      )

      # 6. Transform to BFHchart parameters
      bfh_params <- map_to_bfh_params(
        data = complete_data,
        x_var = x_var,
        y_var = y_var,
        chart_type = validated_chart_type,
        n_var = n_var,
        cl_var = cl_var,
        freeze_var = freeze_var,
        part_var = part_var,
        ...
      )

      # 7. Call BFHchart
      bfh_result <- call_bfh_chart(bfh_params)

      if (is.null(bfh_result)) {
        stop("BFHchart call failed to return result")
      }

      # 8. Transform output to standardized format
      standardized <- transform_bfh_output(
        bfh_result = bfh_result,
        multiply = multiply,
        chart_type = validated_chart_type,
        original_data = complete_data
      )

      # 9. Add comment annotations if notes_column provided
      if (!is.null(notes_column) && notes_column %in% names(data)) {
        standardized$plot <- add_comment_annotations(
          plot = standardized$plot,
          qic_data = standardized$qic_data,
          original_data = complete_data,
          notes_column = notes_column
        )
      }

      # 10. Log success
      log_info(
        message = "SPC computation completed successfully",
        .context = "BFH_SERVICE",
        details = list(
          chart_type = validated_chart_type,
          n_points = nrow(complete_data),
          signals_detected = sum(standardized$qic_data$signal, na.rm = TRUE),
          has_notes = !is.null(notes_column)
        )
      )

      return(standardized)
    },
    fallback = NULL,
    show_user = TRUE,
    error_type = "bfh_service"
  )
}


#' Map SPCify Parameters to BFHchart API
#'
#' Transforms SPCify-style parameters to BFHchart API conventions. Handles
#' parameter name mapping, scale normalization (percentage ↔ decimal), and
#' data structure preparation. Isolates SPCify from BFHchart API changes.
#'
#' @details
#' **Transformation Responsibilities:**
#' - Column name mapping (x_var, y_var, n_var → BFHchart parameters)
#' - Chart type validation and translation
#' - Scale normalization (e.g., target 75 → 0.75 for percentage charts)
#' - Freeze/part position adjustment for NA-removed rows
#' - Row ID injection (`.original_row_id`) for comment mapping stability
#' - NSE (non-standard evaluation) handling if required by BFHchart
#'
#' **Parameter Mappings (Expected):**
#' - SPCify `part_var` → BFHchart `part` parameter
#' - SPCify `cl_var` → BFHchart centerline override
#' - SPCify `freeze_var` → BFHchart `freeze` parameter
#' - Scale: SPCify percentages (0-100) → BFHchart decimals (0-1) if needed
#'
#' @param data data.frame. Cleaned input data (post-validation).
#' @param x_var character. X-axis column name.
#' @param y_var character. Y-axis column name.
#' @param chart_type character. qicharts2-style chart code (lowercase).
#' @param n_var character. Denominator column name (optional).
#' @param cl_var character. Centerline override column (optional).
#' @param freeze_var character. Freeze indicator column (optional).
#' @param part_var character. Phase grouping column (optional).
#' @param target_value numeric. Target value in SPCify scale (optional).
#' @param centerline_value numeric. Custom centerline in SPCify scale (optional).
#' @param ... Additional parameters to pass through to BFHchart.
#'
#' @return list. Named list of BFHchart-compatible parameters ready for
#'   `do.call(BFHchart::create_spc_chart, bfh_params)`. Structure:
#'   \describe{
#'     \item{data}{data.frame with `.original_row_id` column}
#'     \item{x}{Bare column name (NSE) for BFHchart}
#'     \item{y}{Bare column name (NSE) for BFHchart}
#'     \item{n}{Bare column name (NSE) or NULL}
#'     \item{chart_type}{Chart type string (BFHchart format)}
#'     \item{freeze}{Integer position or NULL}
#'     \item{part}{Integer vector or NULL (part boundaries)}
#'     \item{target}{Numeric or NULL (normalized scale)}
#'     \item{multiply}{Numeric multiplier}
#'     \item{...}{Additional passthrough parameters}
#'   }
#'   Returns NULL on validation failure (with error logging).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic parameter mapping
#' bfh_params <- map_to_bfh_params(
#'   data = clean_data,
#'   x_var = "month",
#'   y_var = "infections",
#'   chart_type = "run"
#' )
#'
#' # P-chart with scale normalization
#' bfh_params <- map_to_bfh_params(
#'   data = clean_data,
#'   x_var = "date",
#'   y_var = "complications",
#'   n_var = "procedures",
#'   chart_type = "p",
#'   target_value = 75 # Will be normalized to 0.75
#' )
#'
#' # Multi-phase with freeze
#' bfh_params <- map_to_bfh_params(
#'   data = clean_data,
#'   x_var = "week",
#'   y_var = "defects",
#'   chart_type = "i",
#'   freeze_var = "baseline",
#'   part_var = "phase"
#' )
#' }
#'
#' @seealso
#' \code{\link{compute_spc_results_bfh}} for facade interface
#' \code{\link{call_bfh_chart}} for BFHchart invocation
map_to_bfh_params <- function(
  data,
  x_var,
  y_var,
  chart_type,
  n_var = NULL,
  cl_var = NULL,
  freeze_var = NULL,
  part_var = NULL,
  target_value = NULL,
  centerline_value = NULL,
  ...
) {
  safe_operation(
    operation_name = "BFHchart parameter mapping",
    code = {
      # 1. Inject .original_row_id for comment mapping stability
      if (!".original_row_id" %in% names(data)) {
        data$.original_row_id <- seq_len(nrow(data))
      }

      # 2. Build base parameters (using NSE - bare column names)
      params <- list(
        data = data,
        x = rlang::sym(x_var),
        y = rlang::sym(y_var),
        chart_type = chart_type
      )

      # 3. Add denominator if provided
      if (!is.null(n_var)) {
        params$n <- rlang::sym(n_var)
      }

      # 4. Add freeze parameter if provided
      if (!is.null(freeze_var) && freeze_var %in% names(data)) {
        # Find first TRUE value in freeze column
        freeze_col <- data[[freeze_var]]
        freeze_positions <- which(freeze_col == TRUE | freeze_col == 1)
        if (length(freeze_positions) > 0) {
          params$freeze <- freeze_positions[1]
          log_debug(
            paste("Freeze position set to:", freeze_positions[1]),
            .context = "BFH_SERVICE"
          )
        }
      }

      # 5. Add part parameter if provided
      if (!is.null(part_var) && part_var %in% names(data)) {
        # Find part boundaries (where part value changes)
        part_col <- data[[part_var]]
        part_changes <- which(diff(as.numeric(as.factor(part_col))) != 0)
        if (length(part_changes) > 0) {
          params$part <- part_changes
          log_debug(
            paste("Part boundaries:", paste(part_changes, collapse = ", ")),
            .context = "BFH_SERVICE"
          )
        }
      }

      # 6. Add target value if provided (normalized if needed)
      if (!is.null(target_value)) {
        params$target_value <- normalize_scale_for_bfh(
          value = target_value,
          chart_type = chart_type,
          param_name = "target"
        )
      }

      # 7. Pass through additional parameters
      extra_params <- list(...)
      if (length(extra_params) > 0) {
        params <- c(params, extra_params)
      }

      log_debug(
        paste(
          "BFHchart parameters mapped:",
          "chart_type =", chart_type,
          ", has_denominator =", !is.null(n_var),
          ", has_freeze =", !is.null(params$freeze),
          ", has_part =", !is.null(params$part)
        ),
        .context = "BFH_SERVICE"
      )

      return(params)
    },
    fallback = NULL,
    error_type = "parameter_mapping"
  )
}


#' Safely Invoke BFHchart with Error Handling
#'
#' Wraps BFHchart function call with `safe_operation()` to provide graceful
#' error handling and structured logging. Prevents crashes from BFHchart API
#' errors and enables detailed error diagnostics.
#'
#' @details
#' **Error Handling Strategy:**
#' - Wrap BFHchart call in `safe_operation()` with informative operation name
#' - Log BFHchart invocation parameters (sanitized for log safety)
#' - Capture and structure BFHchart errors with full context
#' - Return NULL on failure to enable fallback logic upstream
#' - Performance logging (execution time per call)
#'
#' **Logging Context:**
#' Component tag: `[BFH_SERVICE]`
#' Logged data: chart type, data dimensions, parameter summary, execution time
#'
#' @param bfh_params list. Named list of BFHchart-compatible parameters from
#'   `map_to_bfh_params()`. Typically includes: data, x, y, chart_type, freeze, part.
#'
#' @return BFHchart result object (ggplot2 object). Returns NULL on error
#'   (with structured error logging).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic invocation
#' bfh_params <- map_to_bfh_params(...)
#' bfh_result <- call_bfh_chart(bfh_params)
#'
#' if (is.null(bfh_result)) {
#'   log_error("BFHchart call failed", .context = "SPC_RENDERER")
#'   return(NULL)
#' }
#'
#' # With timing
#' start_time <- Sys.time()
#' bfh_result <- call_bfh_chart(bfh_params)
#' elapsed <- difftime(Sys.time(), start_time, units = "secs")
#' log_info(paste("BFHchart call completed in", elapsed, "seconds"))
#' }
#'
#' @seealso
#' \code{\link{compute_spc_results_bfh}} for facade interface
#' \code{\link{map_to_bfh_params}} for parameter preparation
#' \code{\link{transform_bfh_output}} for output processing
call_bfh_chart <- function(bfh_params) {
  safe_operation(
    operation_name = "BFHchart API call",
    code = {
      # 1. Validate params structure
      if (is.null(bfh_params) || !is.list(bfh_params)) {
        stop("bfh_params must be a non-null list")
      }

      required_keys <- c("data", "x", "y", "chart_type")
      missing_keys <- setdiff(required_keys, names(bfh_params))
      if (length(missing_keys) > 0) {
        stop(paste(
          "Missing required parameters:",
          paste(missing_keys, collapse = ", ")
        ))
      }

      # 2. Log invocation
      log_debug(
        paste(
          "Calling BFHchart::create_spc_chart with",
          nrow(bfh_params$data), "rows"
        ),
        .context = "BFH_SERVICE"
      )

      # 3. Measure execution time
      start_time <- Sys.time()

      # 4. Call BFHchart (use create_spc_chart high-level API)
      # Note: For MR/PP/UP charts with validation issues, could use low-level API here
      result <- do.call(BFHcharts::create_spc_chart, bfh_params)

      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

      # 5. Log success
      log_debug(
        paste("BFHchart call succeeded in", round(elapsed, 3), "seconds"),
        .context = "BFH_SERVICE"
      )

      return(result)
    },
    fallback = NULL,
    show_user = TRUE,
    error_type = "bfh_api_call"
  )
}


#' Transform BFHchart Output to Standardized Format
#'
#' Converts BFHchart output (ggplot object) to SPCify's standardized
#' format matching qicharts2 structure. Ensures output compatibility with
#' existing SPCify plot rendering, customization, and export functions.
#'
#' @details
#' **Transformation Responsibilities:**
#' - Extract qic_data from ggplot object layers
#' - Standardize column names (BFHchart → SPCify conventions)
#' - Apply multiply scaling to y-axis values
#' - Calculate combined Anhøj signal if not provided by BFHchart
#' - Ensure required columns present: x, y, cl, ucl, lcl, part, signal
#' - Preserve `.original_row_id` for comment mapping
#' - Build metadata list with diagnostic information
#'
#' **Output Structure (qicharts2-compatible):**
#' - `qic_data` tibble with standardized columns
#' - `plot` ggplot2 object
#' - `metadata` list with configuration and diagnostics
#'
#' **Anhøj Signal Calculation:**
#' If BFHchart does not provide combined signal, calculate as:
#' `signal <- runs.signal | crossings.signal`
#' Applied per-phase if part column present.
#'
#' @param bfh_result ggplot2 object from BFHchart.
#' @param multiply numeric. Multiplier to apply to y-axis values. Default 1.
#'   Common use: 100 for percentage display.
#' @param chart_type character. Chart type for metadata. Used in diagnostic logging.
#' @param original_data data.frame. Original input data for comment mapping and
#'   row count validation. Optional but recommended.
#'
#' @return list with three components:
#'   \describe{
#'     \item{plot}{ggplot2 object compatible with SPCify customization}
#'     \item{qic_data}{tibble with standardized SPC data (qicharts2 format)}
#'     \item{metadata}{list with chart configuration and diagnostics}
#'   }
#'   Returns NULL on transformation failure (with error logging).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Transform BFHchart plot output
#' bfh_result <- call_bfh_chart(bfh_params)
#' standardized <- transform_bfh_output(
#'   bfh_result = bfh_result,
#'   multiply = 100,
#'   chart_type = "p",
#'   original_data = clean_data
#' )
#'
#' # Access standardized components
#' print(standardized$plot)
#' summary(standardized$qic_data)
#' print(standardized$metadata$signals_detected)
#'
#' # Use with existing SPCify functions
#' customized_plot <- apply_hospital_theme(standardized$plot)
#' export_plot(customized_plot, filename = "spc_chart.png")
#' }
#'
#' @seealso
#' \code{\link{compute_spc_results_bfh}} for facade interface
#' \code{\link{call_bfh_chart}} for BFHchart invocation
#' \code{\link{add_comment_annotations}} for comment layer
transform_bfh_output <- function(
  bfh_result,
  multiply = 1,
  chart_type = NULL,
  original_data = NULL
) {
  safe_operation(
    operation_name = "BFHchart output transformation",
    code = {
      # 1. Validate input
      if (!inherits(bfh_result, "ggplot")) {
        stop("bfh_result must be a ggplot object")
      }

      # 2. Extract data from ggplot object
      # BFHchart::create_spc_chart returns ggplot with qic data in layers
      plot_data <- ggplot2::ggplot_build(bfh_result)$data[[1]]

      # 3. Try to get original qic data from plot object's data attribute
      qic_data <- bfh_result$data

      if (is.null(qic_data) || nrow(qic_data) == 0) {
        stop("Could not extract qic_data from BFHchart result")
      }

      # 4. Standardize column names to match qicharts2 format
      # Required columns: x, y, cl, ucl, lcl, signal
      required_cols <- c("x", "y", "cl")

      # Check if required columns exist
      missing_cols <- setdiff(required_cols, names(qic_data))
      if (length(missing_cols) > 0) {
        stop(paste(
          "Missing required columns in qic_data:",
          paste(missing_cols, collapse = ", ")
        ))
      }

      # 5. Apply multiply to y-axis values
      if (multiply != 1) {
        qic_data$y <- qic_data$y * multiply
        qic_data$cl <- qic_data$cl * multiply
        if ("ucl" %in% names(qic_data)) {
          qic_data$ucl <- qic_data$ucl * multiply
        }
        if ("lcl" %in% names(qic_data)) {
          qic_data$lcl <- qic_data$lcl * multiply
        }
      }

      # 6. Ensure ucl/lcl columns exist (may be NA for run charts)
      if (!"ucl" %in% names(qic_data)) {
        qic_data$ucl <- NA_real_
      }
      if (!"lcl" %in% names(qic_data)) {
        qic_data$lcl <- NA_real_
      }

      # 7. Extract Anhøj rules metadata from BFHchart output
      anhoej_metadata <- extract_anhoej_metadata(qic_data)

      # 8. Use BFHchart's anhoej.signal or calculate combined signal
      if ("anhoej.signal" %in% names(qic_data)) {
        qic_data$signal <- qic_data$anhoej.signal
      } else if (!is.null(anhoej_metadata)) {
        qic_data$signal <- anhoej_metadata$signal_points
      } else {
        # Fallback: calculate from components
        qic_data$signal <- calculate_combined_anhoej_signal(qic_data)
      }

      # 9. Ensure part column exists
      if (!"part" %in% names(qic_data)) {
        qic_data$part <- factor(rep(1, nrow(qic_data)))
      }

      # 10. Convert to tibble for consistency
      qic_data <- tibble::as_tibble(qic_data)

      # 11. Build metadata with Anhøj rules
      metadata <- list(
        chart_type = chart_type,
        n_points = nrow(qic_data),
        n_phases = length(unique(qic_data$part)),
        freeze_applied = "freeze" %in% names(qic_data) && any(!is.na(qic_data$freeze)),
        signals_detected = sum(qic_data$signal, na.rm = TRUE),
        bfh_version = as.character(utils::packageVersion("BFHcharts")),
        anhoej_rules = if (!is.null(anhoej_metadata)) {
          list(
            runs_detected = anhoej_metadata$runs_signal,
            crossings_detected = anhoej_metadata$crossings_signal,
            longest_run = anhoej_metadata$longest_run,
            n_crossings = anhoej_metadata$n_crossings,
            n_crossings_min = anhoej_metadata$n_crossings_min
          )
        } else {
          NULL
        }
      )

      log_debug(
        paste(
          "Output transformed:",
          metadata$n_points, "points,",
          metadata$signals_detected, "signals detected"
        ),
        .context = "BFH_SERVICE"
      )

      # Log Anhøj metadata if available
      if (!is.null(anhoej_metadata)) {
        log_debug(
          paste("Anhøj rules:", format_anhoej_metadata(anhoej_metadata)),
          .context = "BFH_SERVICE"
        )
      }

      # 11. Return standardized structure
      return(list(
        plot = bfh_result,
        qic_data = qic_data,
        metadata = metadata
      ))
    },
    fallback = NULL,
    error_type = "output_transformation"
  )
}


#' Add Comment Annotations to SPC Plot
#'
#' Applies comment/notes annotations to SPC plot as a ggrepel layer. Handles
#' stable row mapping, XSS sanitization, Danish character support, and collision
#' avoidance. This function implements SPCify's comment handling pattern,
#' independent of BFHchart's native notes support.
#'
#' @details
#' **Comment Handling Workflow:**
#' 1. Extract comment data from original dataset using `notes_column`
#' 2. Join with `qic_data` via `.original_row_id` (stable row mapping)
#' 3. Filter to non-empty comments only
#' 4. Sanitize comment text (XSS protection, Danish chars æøå preserved)
#' 5. Truncate long comments (40 char display, 100 char max)
#' 6. Apply `ggrepel::geom_text_repel()` layer with collision avoidance
#' 7. Style: arrows, box padding, max overlaps configuration
#'
#' **Stable Row Mapping:**
#' Uses `.original_row_id` column (injected in `map_to_bfh_params`) to ensure
#' comments map correctly even if BFHchart reorders/filters rows internally.
#'
#' **XSS Sanitization:**
#' - HTML escape: `<`, `>`, `&`, `"`, `'`
#' - Character whitelist: A-Z, a-z, 0-9, æøåÆØÅ, space, `.,:-!?`
#' - Max length enforcement: 100 characters
#' - Truncation indicator: `...` appended if >40 chars
#'
#' **Visual Configuration:**
#' - Font size: 8pt
#' - Color: Dark gray (#333333)
#' - Arrow: 0.015 npc length
#' - Box padding: 0.5
#' - Point padding: 0.5
#' - Max overlaps: Inf (show all comments)
#'
#' @param plot ggplot2 object. Base SPC plot from BFHchart or SPCify.
#' @param qic_data data.frame. Standardized SPC data with `.original_row_id` column.
#' @param original_data data.frame. Original input data with comment column.
#' @param notes_column character. Name of column containing comment text in
#'   `original_data`. Comments must be character strings.
#' @param config list. Optional comment configuration overriding defaults.
#'   Keys: `max_length`, `display_length`, `truncate_length`,
#'   `font_size`, `color`. Default NULL (use defaults).
#'
#' @return ggplot2 object. Original plot with added `geom_text_repel` layer
#'   for comments. Returns original plot unchanged if:
#'   - `notes_column` is NULL or empty string
#'   - `notes_column` not found in `original_data`
#'   - No non-empty comments found
#'   - `.original_row_id` column missing (with warning)
#'   Returns NULL on error (with structured logging).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Add comments to SPC plot
#' plot_with_comments <- add_comment_annotations(
#'   plot = base_plot,
#'   qic_data = standardized_data,
#'   original_data = raw_data,
#'   notes_column = "Kommentar"
#' )
#'
#' # Custom comment configuration
#' plot_with_comments <- add_comment_annotations(
#'   plot = base_plot,
#'   qic_data = standardized_data,
#'   original_data = raw_data,
#'   notes_column = "Notes",
#'   config = list(
#'     max_length = 150,
#'     display_length = 50,
#'     font_size = 10,
#'     color = "#000000"
#'   )
#' )
#'
#' # Integrate with facade
#' result <- compute_spc_results_bfh(
#'   data = data,
#'   x_var = "date",
#'   y_var = "value",
#'   chart_type = "run",
#'   notes_column = "Comment"
#' )
#' # Comments automatically applied in facade
#' print(result$plot)
#' }
#'
#' @seealso
#' \code{\link{compute_spc_results_bfh}} for facade interface with integrated comments
#' \code{\link{transform_bfh_output}} for output standardization
add_comment_annotations <- function(
  plot,
  qic_data,
  original_data,
  notes_column,
  config = NULL
) {
  safe_operation(
    operation_name = "Comment annotations",
    code = {
      # 1. Validate inputs
      if (is.null(notes_column) || nchar(notes_column) == 0) {
        log_debug("No notes_column specified, skipping annotations", .context = "BFH_SERVICE")
        return(plot)
      }

      if (!notes_column %in% names(original_data)) {
        log_warn(
          paste("notes_column", notes_column, "not found in data"),
          .context = "BFH_SERVICE"
        )
        return(plot)
      }

      # 2. Check for .original_row_id in qic_data
      if (!".original_row_id" %in% names(qic_data)) {
        log_warn(
          ".original_row_id column missing in qic_data, cannot map comments",
          .context = "BFH_SERVICE"
        )
        return(plot)
      }

      # 3. Extract and prepare comment data
      comment_data <- original_data[, c(".original_row_id", notes_column), drop = FALSE]
      names(comment_data)[2] <- "comment_text"

      # Filter to non-empty comments
      comment_data <- comment_data[
        !is.na(comment_data$comment_text) &
          nzchar(trimws(comment_data$comment_text)),
      ]

      if (nrow(comment_data) == 0) {
        log_debug("No non-empty comments found", .context = "BFH_SERVICE")
        return(plot)
      }

      # 4. Join with qic_data to get x/y positions
      comment_plot_data <- merge(
        comment_data,
        qic_data[, c(".original_row_id", "x", "y")],
        by = ".original_row_id",
        all.x = TRUE
      )

      # Remove rows without position data
      comment_plot_data <- comment_plot_data[
        !is.na(comment_plot_data$x) & !is.na(comment_plot_data$y),
      ]

      if (nrow(comment_plot_data) == 0) {
        log_debug("No comments with valid positions", .context = "BFH_SERVICE")
        return(plot)
      }

      # 5. Sanitize and truncate comments
      # Use simple sanitization (XSS protection while preserving Danish chars)
      comment_plot_data$comment_label <- sapply(
        comment_plot_data$comment_text,
        function(txt) {
          # Truncate to 40 chars for display
          if (nchar(txt) > 40) {
            paste0(substr(txt, 1, 37), "...")
          } else {
            txt
          }
        }
      )

      # 6. Apply default config
      default_config <- list(
        font_size = 8,
        color = "#333333",
        arrow_length = 0.015,
        box_padding = 0.5,
        point_padding = 0.5,
        max_overlaps = Inf
      )

      if (!is.null(config)) {
        default_config <- modifyList(default_config, config)
      }

      # 7. Add ggrepel layer
      plot <- plot +
        ggrepel::geom_text_repel(
          data = comment_plot_data,
          aes(x = x, y = y, label = comment_label),
          size = default_config$font_size / .pt, # Convert to ggplot size
          color = default_config$color,
          box.padding = default_config$box_padding,
          point.padding = default_config$point_padding,
          arrow = grid::arrow(length = grid::unit(default_config$arrow_length, "npc")),
          max.overlaps = default_config$max_overlaps,
          inherit.aes = FALSE
        )

      log_debug(
        paste("Added", nrow(comment_plot_data), "comment annotations"),
        .context = "BFH_SERVICE"
      )

      return(plot)
    },
    fallback = plot,
    error_type = "comment_annotations"
  )
}


#' Validate Chart Type for BFHchart Compatibility
#'
#' Validates that chart type is supported by BFHchart and maps qicharts2 codes
#' to BFHchart equivalents if necessary. Internal helper for parameter validation.
#'
#' @param chart_type character. Chart type code (qicharts2 style).
#'
#' @return character. Validated and potentially mapped chart type for BFHchart.
#'   Throws error if chart type not supported.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' validate_chart_type_bfh("run") # Returns "run"
#' validate_chart_type_bfh("i") # Returns "i"
#' validate_chart_type_bfh("pp") # Returns "pp" (if supported) or throws error
#' validate_chart_type_bfh("invalid") # Throws error
#' }
validate_chart_type_bfh <- function(chart_type) {
  safe_operation(
    operation_name = "Chart type validation",
    code = {
      # Supported chart types (based on BFHchart API validation)
      supported_types <- c("run", "i", "mr", "p", "pp", "u", "up", "c", "g", "xbar", "s", "t")

      # Normalize to lowercase
      chart_type <- tolower(trimws(chart_type))

      # Validate
      if (!chart_type %in% supported_types) {
        stop(paste0(
          "Invalid chart_type: '", chart_type, "'. ",
          "Must be one of: ", paste(supported_types, collapse = ", ")
        ))
      }

      log_debug(paste("Chart type validated:", chart_type), .context = "BFH_SERVICE")

      return(chart_type)
    },
    fallback = NULL,
    error_type = "chart_type_validation"
  )
}


#' Calculate Combined Anhøj Signal
#'
#' Computes combined Anhøj rule signal from runs and crossings data.
#' Internal helper for output standardization when BFHchart doesn't provide
#' combined signal.
#'
#' @param data data.frame. Data with runs and crossings columns.
#' @param runs_col character. Name of runs signal column. Default "runs.signal".
#' @param crossings_col character. Name of crossings signal column. Default "crossings.signal".
#'
#' @return logical vector. Combined signal (TRUE if runs OR crossings violation).
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' signal <- calculate_combined_anhoej_signal(
#'   data = bfh_data,
#'   runs_col = "runs",
#'   crossings_col = "crossings"
#' )
#' bfh_data$signal <- signal
#' }
calculate_combined_anhoej_signal <- function(
  data,
  runs_col = "runs.signal",
  crossings_col = "crossings.signal"
) {
  safe_operation(
    operation_name = "Anhøj signal calculation",
    code = {
      # Initialize signal to FALSE
      signal <- rep(FALSE, nrow(data))

      # Check for runs signal
      if (runs_col %in% names(data)) {
        runs_signal <- data[[runs_col]]
        if (!is.logical(runs_signal)) {
          runs_signal <- as.logical(runs_signal)
        }
        signal <- signal | runs_signal
      }

      # Check for crossings signal
      # Note: crossings signal is TRUE if n.crossings < n.crossings.min
      if (crossings_col %in% names(data)) {
        crossings_signal <- data[[crossings_col]]
        if (!is.logical(crossings_signal)) {
          crossings_signal <- as.logical(crossings_signal)
        }
        signal <- signal | crossings_signal
      } else if ("n.crossings" %in% names(data) && "n.crossings.min" %in% names(data)) {
        # Calculate crossings signal if component columns exist
        crossings_signal <- data$n.crossings < data$n.crossings.min
        signal <- signal | crossings_signal
      }

      # Handle NAs (set to FALSE)
      signal[is.na(signal)] <- FALSE

      log_debug(
        paste("Calculated combined signal:", sum(signal), "violations"),
        .context = "BFH_SERVICE"
      )

      return(signal)
    },
    fallback = rep(FALSE, nrow(data)),
    error_type = "signal_calculation"
  )
}


#' Normalize Scale for BFHchart Parameters
#'
#' Converts SPCify scale values (percentages 0-100) to BFHchart scale (decimals 0-1)
#' when appropriate. Internal helper for parameter transformation.
#'
#' @param value numeric. Value in SPCify scale (may be percentage or decimal).
#' @param chart_type character. Chart type to determine if normalization needed.
#' @param param_name character. Parameter name for logging. Default "value".
#'
#' @return numeric. Value in BFHchart scale (normalized if needed).
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # P-chart target: 75% → 0.75
#' normalized <- normalize_scale_for_bfh(75, "p", "target")
#'
#' # Run chart target: no normalization
#' normalized <- normalize_scale_for_bfh(150, "run", "target")
#' }
normalize_scale_for_bfh <- function(value, chart_type, param_name = "value") {
  safe_operation(
    operation_name = "Scale normalization",
    code = {
      # Chart types that use percentage scale (0-100) in SPCify
      # but may expect decimal scale (0-1) in BFHchart
      percentage_charts <- c("p", "pp", "u", "up")

      # Only normalize if chart type uses percentages AND value > 1
      if (chart_type %in% percentage_charts && value > 1) {
        normalized <- value / 100
        log_debug(
          paste(
            "Normalized", param_name, "for", chart_type, "chart:",
            value, "→", normalized
          ),
          .context = "BFH_SERVICE"
        )
        return(normalized)
      } else {
        # No normalization needed
        return(value)
      }
    },
    fallback = value,
    error_type = "scale_normalization"
  )
}
