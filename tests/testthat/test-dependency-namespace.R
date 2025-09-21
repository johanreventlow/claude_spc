# Test der sikrer, at bestemte pakker kun bruges via pkg:: namespace

project_root <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)
all_r_files <- list.files(project_root, pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
relative_paths <- gsub(sprintf("^%s/?", project_root), "", all_r_files)
relevant_idx <- relative_paths == "global.R" | startsWith(relative_paths, "R/")
relevant_files <- all_r_files[relevant_idx]

forbidden_packages <- c(
  "shinycssloaders",
  "zoo",
  "scales",
  "openxlsx",
  "excelR",
  "shinylogs",
  "readr",
  "readxl",
  "lubridate",
  "stringi",
  "shinyWidgets",
  "shiny",
  "bslib",
  "ggplot2",
  "qicharts2"
)

pattern <- sprintf("\\blibrary\\((%s)\\)", paste0(forbidden_packages, collapse = "|"))

provide_offending_calls <- function(path) {
  lines <- readLines(path, warn = FALSE)
  matches <- grep(pattern, lines, value = FALSE, perl = TRUE)
  if (length(matches) == 0L) {
    return(character(0))
  }
  offending <- lines[matches]
  offending <- offending[!grepl("^\\s*#", offending)]
  if (length(offending) == 0L) {
    return(character(0))
  }
  sprintf("%s: %s", path, offending)
}

testthat::test_that("library-kald for aftalte pakker er fjernet", {
  offending_calls <- unlist(lapply(relevant_files, provide_offending_calls), use.names = FALSE)
  if (length(offending_calls) > 0L) {
    msg <- paste0(
      "Fjern library-kald for følgende pakker:\n",
      paste(offending_calls, collapse = "\n")
    )
    testthat::fail(msg)
  } else {
    testthat::expect_true(TRUE)
  }
})
