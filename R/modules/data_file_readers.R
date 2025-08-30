# R/modules/data_file_readers.R
# File reading functions for data module

library(readr)
library(readxl)

# Read CSV file with error handling - Danish-optimized
readCSVFile <- function(file_path, sep = ";", decimal = ",", encoding = "UTF-8", header = TRUE) {
  
  tryCatch({
    # For Danish CSV files, use read_csv2 when sep=";" and decimal=","
    if(sep == ";" && decimal == ",") {
      data <- readr::read_csv2(
        file_path,
        locale = readr::locale(
          decimal_mark = ",",
          grouping_mark = ".",
          encoding = encoding
        ),
        col_names = header,
        show_col_types = FALSE,
        trim_ws = TRUE
      )
    } else if(decimal == ",") {
      # Custom delimiter with comma decimal
      data <- readr::read_delim(
        file_path,
        delim = sep,
        locale = readr::locale(
          decimal_mark = ",",
          grouping_mark = ".",
          encoding = encoding
        ),
        col_names = header,
        show_col_types = FALSE,
        trim_ws = TRUE
      )
    } else {
      # Standard delimited file
      data <- readr::read_delim(
        file_path,
        delim = sep,
        locale = readr::locale(
          decimal_mark = decimal,
          grouping_mark = if(decimal == ",") "." else ",", 
          encoding = encoding
        ),
        col_names = header,
        show_col_types = FALSE,
        trim_ws = TRUE
      )
    }
    
    # Convert to data.frame and clean up
    data <- as.data.frame(data)
    
    # Clean column names (remove extra spaces, etc.)
    names(data) <- trimws(names(data))
    
    return(data)
    
  }, error = function(e1) {
    # Fallback: base R read functions
    tryCatch({
      data <- read.csv(
        file_path,
        sep = sep,
        dec = decimal,
        header = header,
        fileEncoding = encoding,
        stringsAsFactors = FALSE,
        strip.white = TRUE
      )
      
      # Clean column names
      names(data) <- trimws(names(data))
      
      return(data)
      
    }, error = function(e2) {
      stop(paste("Kunne ikke læse CSV fil. Første fejl:", e1$message, "• Fallback-fejl:", e2$message))
    })
  })
}

# Read Excel file with error handling
readExcelFile <- function(file_path) {
  
  tryCatch({
    # Get sheet names
    sheets <- readxl::excel_sheets(file_path)
    
    # Read first sheet
    data <- readxl::read_excel(
      file_path,
      sheet = sheets[1],
      col_names = TRUE,
      .name_repair = "minimal"
    )
    
    # Convert to data.frame
    return(as.data.frame(data))
    
  }, error = function(e) {
    stop(paste("Kunne ikke læse Excel fil:", e$message))
  })
}
