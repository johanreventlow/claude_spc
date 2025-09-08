# data_validation.R
# Modul til validering af data til SPC analyse

# Dependencies ----------------------------------------------------------------
# Bruger funktioner fra danish_numbers.R

# VALIDERING FUNKTIONER =======================================================

## Valider datastruktur til SPC analyse
validateDataStructure <- function(data) {
  
  errors <- character(0)
  warnings <- character(0)
  
  # Check if data exists and has content
  if(is.null(data) || nrow(data) == 0) {
    errors <- c(errors, "Ingen data fundet i filen")
    return(list(valid = FALSE, errors = errors, warnings = warnings))
  }
  
  if(ncol(data) < 2) {
    errors <- c(errors, "Data skal have mindst 2 kolonner (tid og værdi)")
  }
  
  # Check for potential time/date columns
  potential_date_cols <- character(0)
  potential_numeric_cols <- character(0)
  
  for(col_name in names(data)) {
    col_data <- data[[col_name]]
    
    # Skip completely empty columns
    if(all(is.na(col_data))) {
      warnings <- c(warnings, paste("Kolonne", col_name, "er helt tom"))
      next
    }
    
    # Check for potential date columns
    char_data <- as.character(col_data)[!is.na(col_data)]
    if(length(char_data) > 0) {
      if(any(grepl("\\d{4}-\\d{2}-\\d{2}|\\d{2}/\\d{2}/\\d{4}|\\d{2}-\\d{2}-\\d{4}", char_data))) {
        potential_date_cols <- c(potential_date_cols, col_name)
      }
    }
    
    # Check for numeric columns (handle Danish decimal separator)
    if(is.numeric(col_data) || 
       sum(!is.na(parse_danish_number(col_data))) > length(col_data) * 0.8) {
      potential_numeric_cols <- c(potential_numeric_cols, col_name)
    }
  }
  
  # Validate minimum requirements
  if(length(potential_numeric_cols) == 0) {
    errors <- c(errors, "Ingen numeriske kolonner fundet - mindst én numerisk kolonne er påkrævet for SPC analyse")
  }
  
  if(length(potential_date_cols) == 0 && nrow(data) > 1) {
    warnings <- c(warnings, "Ingen dato-kolonner fundet - overvej at tilføje tidsstempel for tidsserie-analyse")
  }
  
  # Check data size
  if(nrow(data) < 10) {
    warnings <- c(warnings, paste("Kun", nrow(data), "datapunkter fundet - SPC analyse er mest pålidelig med mindst 15-20 punkter"))
  }
  
  # Check for missing values
  missing_pct <- round(sum(is.na(data)) / (nrow(data) * ncol(data)) * 100, 1)
  if(missing_pct > 20) {
    warnings <- c(warnings, paste("Høj andel manglende værdier:", missing_pct, "% - dette kan påvirke analyse-kvaliteten"))
  }
  
  # Return validation results
  return(list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings,
    potential_date_cols = potential_date_cols,
    potential_numeric_cols = potential_numeric_cols,
    summary = list(
      rows = nrow(data),
      cols = ncol(data),
      missing_pct = missing_pct
    )
  ))
}
