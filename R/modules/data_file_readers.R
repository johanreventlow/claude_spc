# data_file_readers.R
# Modul til læsning af datafiler med støtte for danske formater og separatorer

# Dependencies ----------------------------------------------------------------
library(readr)
library(readxl)

# CSV LÆSNING =================================================================

## Læs CSV fil med fejlhåndtering - dansk-optimeret
readCSVFile <- function(file_path, sep = ";", decimal = ",", encoding = "UTF-8", header = TRUE) {
  
  tryCatch({
    # Til danske CSV filer, brug read_csv2 når sep=";" og decimal=","
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
      # Brugerdefineret separator med komma decimal
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
      # Standard separeret fil
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
    
    # Konverter til data.frame og rens op
    data <- as.data.frame(data)
    
    # Rens kolonnenavne (fjern ekstra mellemrum osv.)
    names(data) <- trimws(names(data))
    
    return(data)
    
  }, error = function(e1) {
    # Fallback: basis R læsefunktioner
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
      
      # Rens kolonnenavne
      names(data) <- trimws(names(data))
      
      return(data)
      
    }, error = function(e2) {
      stop(paste("Kunne ikke læse CSV fil. Første fejl:", e1$message, "• Fallback-fejl:", e2$message))
    })
  })
}

# EXCEL LÆSNING ==============================================================

## Læs Excel fil med fejlhåndtering
readExcelFile <- function(file_path) {
  
  tryCatch({
    # Hent ark-navne
    sheets <- readxl::excel_sheets(file_path)
    
    # Læs første ark
    data <- readxl::read_excel(
      file_path,
      sheet = sheets[1],
      col_names = TRUE,
      .name_repair = "minimal"
    )
    
    # Konverter til data.frame
    return(as.data.frame(data))
    
  }, error = function(e) {
    stop(paste("Kunne ikke læse Excel fil:", e$message))
  })
}
