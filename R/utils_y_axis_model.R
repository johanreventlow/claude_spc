# utils_y_axis_model.R
# Y-akse datamodel: UI-typer → interne klasser → kortvalg

# Interne klasser (konstanter)
INTERNAL_CLASSES <- list(
  MEASUREMENT = "MEASUREMENT",
  COUNT = "COUNT",
  PROPORTION = "PROPORTION",
  RATE_INTERNAL = "RATE_INTERNAL",
  TIME_BETWEEN = "TIME_BETWEEN",
  COUNT_BETWEEN = "COUNT_BETWEEN"
)

#' Afgør intern klasse ud fra UI-type og data
#'
#' @param ui_type En af {"count" (TAL), "percent" (PROCENT), "rate" (RATE), "time" (TID)}
#' @param y Numeric vector (kan være heltal eller decimaltal)
#' @param n_present Logical – om N-kolonne er valgt (bruges som n/exposure)
#' @return Character – intern klasse
#' @export
determine_internal_class <- function(ui_type, y, n_present = FALSE) {
  ui <- tolower(ui_type %||% "count")

  if (ui == "percent") {
    return(INTERNAL_CLASSES$PROPORTION)
  }

  if (ui == "rate") {
    return(INTERNAL_CLASSES$RATE_INTERNAL)
  }

  if (ui == "time") {
    return(INTERNAL_CLASSES$TIME_BETWEEN)
  }

  # TAL (default = count/value)
  # COUNT hvis heltal ≥ 0 og ingen n/exposure; ellers MEASUREMENT
  y_num <- suppressWarnings(as.numeric(y))
  all_int <- all(!is.na(y_num)) && all(floor(y_num) == y_num) && all(y_num >= 0)
  if (all_int && !isTRUE(n_present)) {
    return(INTERNAL_CLASSES$COUNT)
  }
  return(INTERNAL_CLASSES$MEASUREMENT)
}

#' Foreslå korttype ud fra intern klasse
#'
#' @param internal_class Intern klasse fra determine_internal_class
#' @param n_present Logical – om N-kolonne er valgt (for P/U)
#' @param n_points Antal datapunkter (run chart fallback for små serier)
#' @return qicharts2-kode for korttype ("i", "c", "p", "u", "t", "g", "run")
#' @export
suggest_chart_type <- function(internal_class, n_present = FALSE, n_points = NA_integer_) {
  if (!is.na(n_points) && n_points < 12) {
    return("run")
  }

  ic <- toupper(internal_class %||% "MEASUREMENT")
  if (ic == INTERNAL_CLASSES$MEASUREMENT) {
    return("i")
  }
  if (ic == INTERNAL_CLASSES$COUNT) {
    return("c")
  }
  if (ic == INTERNAL_CLASSES$PROPORTION) {
    return("p")
  }
  if (ic == INTERNAL_CLASSES$RATE_INTERNAL) {
    return("u")
  }
  if (ic == INTERNAL_CLASSES$TIME_BETWEEN) {
    return("t")
  }
  if (ic == INTERNAL_CLASSES$COUNT_BETWEEN) {
    return("g")
  }
  return("run")
}

#' Vælg default Y-akse UI-type ud fra kontekst
#'
#' Særligt for run chart ønsker vi:
#' - Hvis både tæller og nævner er valgt: default = percent
#' - Hvis kun tæller: default = count
#' Brugeren kan altid overskrive.
#'
#' @param chart_type qicharts2-kode for korttype (fx "run")
#' @param n_present Logical – om N-kolonne er valgt
#' @return "percent" eller "count"
#' @export
decide_default_y_axis_ui_type <- function(chart_type, n_present) {
  ct <- get_qic_chart_type(chart_type)
  if (identical(ct, "run") && isTRUE(n_present)) {
    return("percent")
  }
  return("count")
}

#' Map diagramtype til Y-akse UI-type
#'
#' @param chart_type qicharts2-kode eller dansk label
#' @return one of {"count","percent","rate","time"}
#' @export
chart_type_to_ui_type <- function(chart_type) {
  ct <- get_qic_chart_type(chart_type)
  if (ct %in% c("p", "pp")) {
    return("percent")
  }
  if (ct %in% c("u", "up")) {
    return("rate")
  }
  if (ct == "t") {
    return("time")
  }
  # i, mr, c, g og fallback
  return("count")
}
