test_that("log_debug erstatter alle DEBUG-cat statements", {
  project_root <- normalizePath(file.path(testthat::test_path(".."), ".."), mustWork = TRUE)
  r_files <- list.files(project_root, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  debug_patterns <- c("cat\\s*\\(\\s*\\\"DEBUG", "cat\\s*\\(\\s*'DEBUG")
  offending_lines <- unlist(lapply(r_files, function(file_path) {
    lines <- readLines(file_path, warn = FALSE)
    match_indices <- unique(unlist(lapply(debug_patterns, function(pattern) {
      which(grepl(pattern, lines, perl = TRUE))
    })))
    match_indices <- sort(match_indices)
    if (length(match_indices) == 0L) {
      return(NULL)
    }
    paste0(file_path, ":", match_indices)
  }))
  expect(
    length(offending_lines) == 0,
    failure_message = paste(
      "Følgende cat(\"DEBUG statements skal konverteres til log_debug():",
      paste(offending_lines, collapse = "\n"),
      sep = "\n"
    )
  )
})
