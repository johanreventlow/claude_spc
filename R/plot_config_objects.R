#' Plot Configuration Objects
#'
#' Parameter objects for reducing function signature complexity.
#'
#' ## Architecture
#'
#' Instead of functions with 15+ parameters, this module provides **parameter
#' objects** that group related configuration into logical units.
#'
#' **Benefits**:
#' - Reduced function signatures (15 params → 3-5 params)
#' - Logical parameter grouping (easier to understand)
#' - Easier to extend (add fields without changing signatures)
#' - Self-documenting code (object names describe purpose)
#' - Backward compatibility through wrapper functions
#'
#' ## Usage
#'
#' ```r
#' # Before (15 parameters - hard to read):
#' plot <- generateSPCPlot(
#'   data, config, "p", target_value = 50, centerline_value = NULL,
#'   show_phases = FALSE, skift_column = NULL, frys_column = NULL,
#'   chart_title_reactive = "SPC Chart", y_axis_unit = "percent",
#'   kommentar_column = NULL, base_size = 14, viewport_width = 800,
#'   viewport_height = 600, target_text = "Target", qic_cache = NULL
#' )
#'
#' # After (3 parameter objects - clear and maintainable):
#' plot_cfg <- spc_plot_config(
#'   chart_type = "p",
#'   y_axis_unit = "percent",
#'   target_value = 50,
#'   target_text = "Target"
#' )
#'
#' viewport <- viewport_dims(width = 800, height = 600, base_size = 14)
#'
#' phases <- phase_config(
#'   show_phases = FALSE,
#'   skift_column = NULL,
#'   frys_column = NULL
#' )
#'
#' plot <- generateSPCPlot_v2(data, config, plot_cfg, viewport, phases)
#' ```
#'
#' @name plot_config_objects
NULL

# ============================================================================
# SPC PLOT CONFIGURATION
# ============================================================================

#' Create SPC Plot Configuration
#'
#' Creates a configuration object for SPC plot generation.
#'
#' @param chart_type Chart type (run, i, p, c, u, etc.)
#' @param y_axis_unit Y-axis unit ("count", "percent", "rate", "time")
#' @param target_value Numeric target value (optional)
#' @param target_text Text description of target (optional)
#' @param centerline_value Custom centerline value (optional)
#' @param kommentar_column Column name for comments (optional)
#' @param chart_title Chart title (optional)
#' @param qic_cache QIC cache object (optional)
#'
#' @return List with class "spc_plot_config"
#'
#' @details
#' This object groups all plot-specific configuration that controls
#' the appearance and behavior of the SPC chart.
#'
#' **Validation**:
#' - `chart_type` must be one of the valid SPC chart types
#' - `y_axis_unit` must be one of: count, percent, rate, time
#' - `target_value` must be numeric if provided
#'
#' @export
spc_plot_config <- function(
  chart_type = "run",
  y_axis_unit = "count",
  target_value = NULL,
  target_text = NULL,
  centerline_value = NULL,
  kommentar_column = NULL,
  chart_title = NULL,
  qic_cache = NULL
) {
  # Validation
  valid_chart_types <- c("run", "i", "mr", "xbar", "s", "t", "p", "c", "u", "g")
  if (!chart_type %in% valid_chart_types) {
    if (exists("log_warn", mode = "function")) {
      log_warn(
        paste("Invalid chart_type:", chart_type),
        .context = "PLOT_CONFIG"
      )
    }
  }

  valid_units <- c("count", "percent", "rate", "time")
  if (!y_axis_unit %in% valid_units) {
    if (exists("log_warn", mode = "function")) {
      log_warn(
        paste("Invalid y_axis_unit:", y_axis_unit),
        .context = "PLOT_CONFIG"
      )
    }
  }

  if (!is.null(target_value) && !is.numeric(target_value)) {
    if (exists("log_warn", mode = "function")) {
      log_warn(
        "target_value must be numeric",
        .context = "PLOT_CONFIG"
      )
    }
    target_value <- NULL
  }

  structure(
    list(
      chart_type = chart_type,
      y_axis_unit = y_axis_unit,
      target_value = target_value,
      target_text = target_text,
      centerline_value = centerline_value,
      kommentar_column = kommentar_column,
      chart_title = chart_title,
      qic_cache = qic_cache
    ),
    class = "spc_plot_config"
  )
}

#' Print SPC Plot Configuration
#'
#' @param x SPC plot configuration object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.spc_plot_config <- function(x, ...) {
  cat("SPC Plot Configuration:\n")
  cat("  Chart Type:", x$chart_type, "\n")
  cat("  Y-Axis Unit:", x$y_axis_unit, "\n")
  cat("  Target Value:", if (is.null(x$target_value)) "NULL" else x$target_value, "\n")
  cat("  Target Text:", if (is.null(x$target_text)) "NULL" else x$target_text, "\n")
  cat("  Centerline:", if (is.null(x$centerline_value)) "NULL" else x$centerline_value, "\n")
  invisible(x)
}

# ============================================================================
# VIEWPORT DIMENSIONS
# ============================================================================

#' Create Viewport Dimensions Configuration
#'
#' Creates a configuration object for plot viewport dimensions.
#'
#' @param width Viewport width in pixels (default: NULL for auto)
#' @param height Viewport height in pixels (default: NULL for auto)
#' @param base_size Base font size for scaling (default: 14)
#'
#' @return List with class "viewport_dims"
#'
#' @details
#' This object controls the physical dimensions and scaling of the plot.
#'
#' **Defaults**:
#' - If width/height are NULL, responsive sizing is used
#' - base_size controls responsive scaling of geoms and text
#' - Reference: base_size 14 provides original sizing
#'
#' @export
viewport_dims <- function(
  width = NULL,
  height = NULL,
  base_size = 14
) {
  # Validation
  if (!is.null(width) && (!is.numeric(width) || width <= 0)) {
    if (exists("log_warn", mode = "function")) {
      log_warn(
        "width must be positive numeric or NULL",
        .context = "PLOT_CONFIG"
      )
    }
    width <- NULL
  }

  if (!is.null(height) && (!is.numeric(height) || height <= 0)) {
    if (exists("log_warn", mode = "function")) {
      log_warn(
        "height must be positive numeric or NULL",
        .context = "PLOT_CONFIG"
      )
    }
    height <- NULL
  }

  if (!is.numeric(base_size) || base_size <= 0) {
    if (exists("log_warn", mode = "function")) {
      log_warn(
        "base_size must be positive numeric, defaulting to 14",
        .context = "PLOT_CONFIG"
      )
    }
    base_size <- 14
  }

  structure(
    list(
      width = width,
      height = height,
      base_size = base_size
    ),
    class = "viewport_dims"
  )
}

#' Print Viewport Dimensions
#'
#' @param x Viewport dimensions object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.viewport_dims <- function(x, ...) {
  cat("Viewport Dimensions:\n")
  cat("  Width:", if (is.null(x$width)) "Auto" else paste(x$width, "px"), "\n")
  cat("  Height:", if (is.null(x$height)) "Auto" else paste(x$height, "px"), "\n")
  cat("  Base Size:", x$base_size, "\n")
  invisible(x)
}

# ============================================================================
# PHASE CONFIGURATION
# ============================================================================

#' Create Phase Configuration
#'
#' Creates a configuration object for phase/shift handling.
#'
#' @param show_phases Logical, show phase separators (default: FALSE)
#' @param skift_column Column name for phase shifts (optional)
#' @param frys_column Column name for freeze points (optional)
#'
#' @return List with class "phase_config"
#'
#' @details
#' This object controls phase separation and control limit freezing.
#'
#' **Phase Handling**:
#' - `show_phases = TRUE` draws vertical separators between phases
#' - `skift_column` identifies which column marks phase changes
#' - `frys_column` identifies freeze points for control limits
#'
#' @export
phase_config <- function(
  show_phases = FALSE,
  skift_column = NULL,
  frys_column = NULL
) {
  # Validation
  if (!is.logical(show_phases)) {
    if (exists("log_warn", mode = "function")) {
      log_warn(
        "show_phases must be logical, defaulting to FALSE",
        .context = "PLOT_CONFIG"
      )
    }
    show_phases <- FALSE
  }

  if (!is.null(skift_column) && !is.character(skift_column)) {
    if (exists("log_warn", mode = "function")) {
      log_warn(
        "skift_column must be character or NULL",
        .context = "PLOT_CONFIG"
      )
    }
    skift_column <- NULL
  }

  if (!is.null(frys_column) && !is.character(frys_column)) {
    if (exists("log_warn", mode = "function")) {
      log_warn(
        "frys_column must be character or NULL",
        .context = "PLOT_CONFIG"
      )
    }
    frys_column <- NULL
  }

  structure(
    list(
      show_phases = show_phases,
      skift_column = skift_column,
      frys_column = frys_column
    ),
    class = "phase_config"
  )
}

#' Print Phase Configuration
#'
#' @param x Phase configuration object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.phase_config <- function(x, ...) {
  cat("Phase Configuration:\n")
  cat("  Show Phases:", x$show_phases, "\n")
  cat("  Skift Column:", if (is.null(x$skift_column)) "NULL" else x$skift_column, "\n")
  cat("  Frys Column:", if (is.null(x$frys_column)) "NULL" else x$frys_column, "\n")
  invisible(x)
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Extract Individual Parameters from Config Objects
#'
#' Internal helper to extract parameters from config objects back
#' to individual values for backward compatibility.
#'
#' @param plot_cfg SPC plot config object
#' @param viewport Viewport dims object
#' @param phases Phase config object
#'
#' @return Named list with all individual parameters
#'
#' @keywords internal
extract_params_from_configs <- function(plot_cfg, viewport, phases) {
  list(
    # From plot_cfg
    chart_type = plot_cfg$chart_type,
    y_axis_unit = plot_cfg$y_axis_unit,
    target_value = plot_cfg$target_value,
    target_text = plot_cfg$target_text,
    centerline_value = plot_cfg$centerline_value,
    kommentar_column = plot_cfg$kommentar_column,
    chart_title_reactive = plot_cfg$chart_title,
    qic_cache = plot_cfg$qic_cache,

    # From viewport
    viewport_width = viewport$width,
    viewport_height = viewport$height,
    base_size = viewport$base_size,

    # From phases
    show_phases = phases$show_phases,
    skift_column = phases$skift_column,
    frys_column = phases$frys_column
  )
}

#' Build Config Objects from Individual Parameters
#'
#' Internal helper to build config objects from individual parameters
#' for backward compatibility.
#'
#' @param chart_type Chart type
#' @param y_axis_unit Y-axis unit
#' @param target_value Target value
#' @param target_text Target text
#' @param centerline_value Centerline value
#' @param kommentar_column Comment column
#' @param chart_title_reactive Chart title
#' @param qic_cache QIC cache
#' @param viewport_width Viewport width
#' @param viewport_height Viewport height
#' @param base_size Base size
#' @param show_phases Show phases flag
#' @param skift_column Skift column
#' @param frys_column Frys column
#'
#' @return Named list with plot_cfg, viewport, phases objects
#'
#' @keywords internal
build_configs_from_params <- function(
  chart_type = "run",
  y_axis_unit = "count",
  target_value = NULL,
  target_text = NULL,
  centerline_value = NULL,
  kommentar_column = NULL,
  chart_title_reactive = NULL,
  qic_cache = NULL,
  viewport_width = NULL,
  viewport_height = NULL,
  base_size = 14,
  show_phases = FALSE,
  skift_column = NULL,
  frys_column = NULL
) {
  list(
    plot_cfg = spc_plot_config(
      chart_type = chart_type,
      y_axis_unit = y_axis_unit,
      target_value = target_value,
      target_text = target_text,
      centerline_value = centerline_value,
      kommentar_column = kommentar_column,
      chart_title = chart_title_reactive,
      qic_cache = qic_cache
    ),
    viewport = viewport_dims(
      width = viewport_width,
      height = viewport_height,
      base_size = base_size
    ),
    phases = phase_config(
      show_phases = show_phases,
      skift_column = skift_column,
      frys_column = frys_column
    )
  )
}

# ============================================================================
# BACKWARD COMPATIBILITY WRAPPER
# ============================================================================

#' Generate SPC Plot (New Parameter Objects API)
#'
#' Generates an SPC plot using parameter objects instead of individual parameters.
#' This is the new preferred API that reduces function signature complexity.
#'
#' @param data Data frame with SPC data
#' @param config Configuration object
#' @param plot_cfg SPC plot configuration object (from `spc_plot_config()`)
#' @param viewport Viewport dimensions object (from `viewport_dims()`)
#' @param phases Phase configuration object (from `phase_config()`)
#'
#' @return List with plot and qic_data
#'
#' @details
#' This function provides a cleaner API with logical parameter grouping:
#' - `plot_cfg`: All plot-specific configuration
#' - `viewport`: Physical dimensions and scaling
#' - `phases`: Phase/shift handling
#'
#' **Migration Example**:
#' ```r
#' # Old API (15 parameters):
#' plot <- generateSPCPlot(data, config, "p", target_value = 50, ...)
#'
#' # New API (3 parameter objects):
#' plot_cfg <- spc_plot_config(chart_type = "p", target_value = 50)
#' viewport <- viewport_dims(width = 800, height = 600)
#' phases <- phase_config(show_phases = FALSE)
#' plot <- generateSPCPlot_v2(data, config, plot_cfg, viewport, phases)
#' ```
#'
#' @export
generateSPCPlot_v2 <- function(data, config, plot_cfg, viewport, phases) {
  # Validate parameter objects
  if (!inherits(plot_cfg, "spc_plot_config")) {
    stop("plot_cfg must be an spc_plot_config object created with spc_plot_config()")
  }
  if (!inherits(viewport, "viewport_dims")) {
    stop("viewport must be a viewport_dims object created with viewport_dims()")
  }
  if (!inherits(phases, "phase_config")) {
    stop("phases must be a phase_config object created with phase_config()")
  }

  # Extract individual parameters for legacy generateSPCPlot call
  params <- extract_params_from_configs(plot_cfg, viewport, phases)

  # Call legacy function with extracted parameters
  generateSPCPlot(
    data = data,
    config = config,
    chart_type = params$chart_type,
    target_value = params$target_value,
    centerline_value = params$centerline_value,
    show_phases = params$show_phases,
    skift_column = params$skift_column,
    frys_column = params$frys_column,
    chart_title_reactive = params$chart_title_reactive,
    y_axis_unit = params$y_axis_unit,
    kommentar_column = params$kommentar_column,
    base_size = params$base_size,
    viewport_width = params$viewport_width,
    viewport_height = params$viewport_height,
    target_text = params$target_text,
    qic_cache = params$qic_cache
  )
}
