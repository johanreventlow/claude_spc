#' Input Sanitization Utilities
#'
#' Centraliseret sikkerhed for bruger input validation og sanitization.
#' Implementeret som del af security hardening efter tidyverse migration.
#'
#' @name input_sanitization
NULL

#' Sanitize User Input
#'
#' Sikker validation og cleaning af bruger input for at forhindre XSS og injection.
#'
#' @param input_value Input værdi at sanitize (character, numeric, eller NULL)
#' @param max_length Maksimum længde på output (default: 255)
#' @param allowed_chars Regex pattern for tilladte karakterer
#' @param html_escape Skal HTML karakterer escapes? (default: TRUE)
#'
#' @return Sanitized string, eller tom string hvis input er invalid
#'
#' @examples
#' # Basic sanitization
#' sanitize_user_input("Kolonne navn & data")
#'
#' # Column name sanitization
#' sanitize_user_input("x_column<script>", max_length = 50)
#'
#' # Numeric input validation
#' sanitize_user_input(123.45)
#'
#' @export
sanitize_user_input <- function(input_value,
                               max_length = 255,
                               allowed_chars = "A-Za-z0-9_æøåÆØÅ .-",
                               html_escape = TRUE) {

  # Håndter NULL og missing values
  if (is.null(input_value) || length(input_value) == 0) {
    return("")
  }

  # Convert til character og håndter numeriske inputs
  input_str <- if (is.numeric(input_value)) {
    as.character(input_value)
  } else {
    trimws(as.character(input_value))
  }

  # Begræns længde første - undgå DoS via store strings
  if (nchar(input_str) > max_length) {
    input_str <- substr(input_str, 1, max_length)
    log_warn(
      component = "[INPUT_SANITIZATION]",
      message = "Input truncated due to length limit",
      details = list(
        original_length = nchar(as.character(input_value)),
        max_length = max_length
      )
    )
  }

  # Fjern ikke-tilladte karakterer
  cleaned_str <- gsub(paste0("[^", allowed_chars, "]"), "", input_str)

  # HTML escape hvis requested (standard for UI output)
  if (html_escape && nchar(cleaned_str) > 0) {
    # Basis HTML escaping for XSS protection
    cleaned_str <- gsub("&", "&amp;", cleaned_str)
    cleaned_str <- gsub("<", "&lt;", cleaned_str)
    cleaned_str <- gsub(">", "&gt;", cleaned_str)
    cleaned_str <- gsub("\"", "&quot;", cleaned_str)
    cleaned_str <- gsub("'", "&#39;", cleaned_str)
  }

  return(cleaned_str)
}

#' Sanitize Column Names
#'
#' Specialiseret sanitization for kolonne navne i SPC kontekst.
#' Tillader danske karakterer og standard kolonne navn patterns.
#'
#' @param column_name Column name at validere
#'
#' @return Sanitized column name suitable for R data.frame operations
#'
#' @examples
#' sanitize_column_name("Dato & tid")  # Returns "Dato  tid"
#' sanitize_column_name("Y-værdi_1")  # Returns "Y-værdi_1"
#'
#' @export
sanitize_column_name <- function(column_name) {
  sanitize_user_input(
    input_value = column_name,
    max_length = 100,  # Kortere for kolonne navne
    allowed_chars = "A-Za-z0-9_æøåÆØÅ .-",  # Tillad danske karakterer og common patterns
    html_escape = TRUE
  )
}

#' Validate and Sanitize File Extensions
#'
#' Sikker validation af file extensions med whitelist approach.
#'
#' @param file_ext File extension (med eller uden '.')
#' @param allowed_extensions Vector af tilladte extensions (default: CSV, Excel)
#'
#' @return TRUE hvis valid, FALSE hvis invalid eller potentielt malicious
#'
#' @examples
#' validate_file_extension("csv")     # TRUE
#' validate_file_extension(".xlsx")   # TRUE
#' validate_file_extension("exe")     # FALSE
#'
#' @export
validate_file_extension <- function(file_ext, allowed_extensions = c("csv", "xlsx", "xls")) {

  if (is.null(file_ext) || length(file_ext) == 0) {
    return(FALSE)
  }

  # Normalize extension - fjern dots og convert til lowercase
  clean_ext <- gsub("^\\.", "", trimws(tolower(as.character(file_ext))))

  # Length check - undgå very long extensions
  if (nchar(clean_ext) > 10) {
    log_warn(
      component = "[FILE_VALIDATION]",
      message = "Suspicious file extension length detected",
      details = list(extension = file_ext, length = nchar(clean_ext))
    )
    return(FALSE)
  }

  # Whitelist check
  is_valid <- clean_ext %in% allowed_extensions

  if (!is_valid) {
    log_warn(
      component = "[FILE_VALIDATION]",
      message = "Invalid file extension rejected",
      details = list(
        attempted_extension = file_ext,
        allowed_extensions = allowed_extensions
      )
    )
  }

  return(is_valid)
}

#' Create Security Warning Message
#'
#' Standardiseret creation af sikkerhedsrelaterede warning messages til UI.
#'
#' @param field_name Navn på feltet der fejlede validation
#' @param issue_type Type af sikkerhedsproblem ("invalid_chars", "too_long", "invalid_format")
#' @param additional_info Ekstra information til brugeren
#'
#' @return Formatted warning message suitable for shiny UI
#'
#' @examples
#' create_security_warning("Kolonne navn", "invalid_chars")
#' create_security_warning("Fil navn", "too_long", "Maksimum 100 karakterer")
#'
#' @export
create_security_warning <- function(field_name, issue_type, additional_info = NULL) {

  # Sanitize field_name først for at undgå XSS i error messages
  safe_field_name <- sanitize_user_input(field_name, max_length = 50)

  base_message <- switch(issue_type,
    "invalid_chars" = paste0(safe_field_name, " indeholder ikke-tilladte karakterer"),
    "too_long" = paste0(safe_field_name, " er for langt"),
    "invalid_format" = paste0(safe_field_name, " har ugyldigt format"),
    "security_violation" = paste0("Sikkerhedsproblem med ", safe_field_name),
    paste0("Validation fejl i ", safe_field_name)  # Default fallback
  )

  # Tilføj additional info hvis givet
  if (!is.null(additional_info) && nchar(additional_info) > 0) {
    safe_additional <- sanitize_user_input(additional_info, max_length = 200)
    base_message <- paste0(base_message, ". ", safe_additional)
  }

  return(base_message)
}