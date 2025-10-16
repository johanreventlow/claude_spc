test_that("NAMESPACE eksporterer ikke generiske nøgleord", {
  namespace_path <- here::here("NAMESPACE")
  skip_if_not(file.exists(namespace_path), "NAMESPACE fil mangler")

  ns_lines <- readLines(namespace_path, warn = FALSE)

  simple_word_pattern <- '^export\\(("?)([a-z]+)\\1\\)$'
  simple_exports <- sub(simple_word_pattern, "\\2", ns_lines[grepl(simple_word_pattern, ns_lines)])

  expect_equal(
    length(simple_exports),
    0,
    info = paste("Uventede simple exports fundet:", paste(simple_exports, collapse = ", "))
  )
})
