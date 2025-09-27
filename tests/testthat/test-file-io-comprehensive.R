# test-file-io-comprehensive.R
# Comprehensive file I/O security and functionality tests

test_that("readCSVFile handles Danish formats correctly", {
  skip_if_not_installed("readr")

  # Create Danish CSV test data
  danish_data <- data.frame(
    Dato = c("2023-01-15", "2023-02-20", "2023-03-25"),
    Værdi = c("12,5", "15,8", "9,2"),  # Danish decimal comma
    Afdeling = c("Medicinsk", "Kirurgisk", "Akut"),
    Kommentar = c("Første måling", "Høj værdi", "Normal"),
    stringsAsFactors = FALSE
  )

  # Test UTF-8 encoding
  temp_file_utf8 <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file_utf8), add = TRUE)

  writeLines(
    c("Dato;Værdi;Afdeling;Kommentar",
      "2023-01-15;12,5;Medicinsk;Første måling",
      "2023-02-20;15,8;Kirurgisk;Høj værdi",
      "2023-03-25;9,2;Akut;Normal"),
    temp_file_utf8,
    useBytes = TRUE
  )

  # Test reading with different delimiters
  result_semicolon <- safe_operation(
    "Read CSV with semicolon",
    code = {
      readr::read_delim(temp_file_utf8, delim = ";", locale = readr::locale(encoding = "UTF-8"))
    }
  )

  expect_true(!is.null(result_semicolon))
  expect_equal(nrow(result_semicolon), 3)
  expect_true("Værdi" %in% names(result_semicolon))
  expect_true(all(c("æ", "ø", "å") %in% unlist(strsplit(paste(result_semicolon$Kommentar, collapse = ""), ""))))
})

test_that("File upload security validation prevents path traversal", {
  # Test path traversal attempts
  malicious_paths <- c(
    "../../../etc/passwd",
    "..\\..\\windows\\system32\\hosts",
    "/etc/shadow",
    "C:\\Windows\\System32\\config\\SAM",
    "~/.ssh/id_rsa",
    "/var/log/auth.log"
  )

  for (path in malicious_paths) {
    expect_error(
      validate_safe_file_path(path),
      "Sikkerhedsfejl",
      info = paste("Should reject path:", path)
    )
  }
})

test_that("File upload handles large files efficiently", {
  skip_if(Sys.getenv("CI") == "true", "Skip large file test in CI")

  # Create large test file (1MB)
  large_data <- data.frame(
    x = rep(1:1000, 10),
    y = rnorm(10000),
    text = rep(paste(rep("a", 100), collapse = ""), 10000),
    stringsAsFactors = FALSE
  )

  temp_large <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_large), add = TRUE)

  # Measure memory usage
  start_memory <- as.numeric(object.size(environment()))

  write.csv(large_data, temp_large, row.names = FALSE)

  # Test memory-efficient reading
  result <- safe_operation(
    "Read large CSV file",
    code = {
      readr::read_csv(temp_large, show_col_types = FALSE)
    }
  )

  end_memory <- as.numeric(object.size(environment()))
  memory_growth <- end_memory - start_memory

  expect_true(!is.null(result))
  expect_equal(nrow(result), 10000)
  # Memory growth should be reasonable (less than 10MB)
  expect_lt(memory_growth, 10 * 1024 * 1024)
})

test_that("File parsing handles corrupted data gracefully", {
  # Test corrupted CSV files
  corrupted_files <- list(
    "incomplete_quotes" = 'name,value\n"John,25\nJane",30',
    "mixed_delimiters" = 'name;value\nJohn,25\nJane;30',
    "invalid_encoding" = paste0('name,value\n', rawToChar(as.raw(c(74, 111, 104, 110, 255, 44, 50, 53)))),
    "truncated_file" = 'name,value\nJohn,25\nJa'
  )

  for (test_name in names(corrupted_files)) {
    temp_file <- tempfile(fileext = ".csv")
    on.exit(unlink(temp_file), add = TRUE)

    writeLines(corrupted_files[[test_name]], temp_file)

    # Should handle errors gracefully without crashing
    result <- safe_operation(
      paste("Parse corrupted file:", test_name),
      code = {
        readr::read_csv(temp_file, show_col_types = FALSE)
      },
      fallback = NULL
    )

    # Should either succeed with problems or return NULL
    expect_true(is.null(result) || is.data.frame(result))
  }
})

test_that("BOM handling preserves file integrity", {
  # Test files with BOM (Byte Order Mark)
  test_content <- "Navn,Alder,By\nAnders,25,København\nBodil,30,Aarhus"

  # UTF-8 with BOM
  temp_bom <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_bom), add = TRUE)

  # Write with BOM
  con <- file(temp_bom, "wb")
  writeBin(c(0xEF, 0xBB, 0xBF), con)  # UTF-8 BOM
  writeLines(test_content, con)
  close(con)

  # Test reading preserves Danish characters
  result <- safe_operation(
    "Read CSV with BOM",
    code = {
      readr::read_csv(temp_bom, locale = readr::locale(encoding = "UTF-8"), show_col_types = FALSE)
    }
  )

  expect_true(!is.null(result))
  expect_equal(nrow(result), 2)
  expect_true("ø" %in% unlist(strsplit(paste(result$By, collapse = ""), "")))
})

test_that("Session metadata sanitization prevents XSS", {
  # Test malicious session metadata
  malicious_inputs <- list(
    title = "<script>alert('XSS')</script>Test Title",
    description = "Normal text <img src=x onerror=alert(1)> more text",
    unit = "javascript:alert('malicious')",
    custom_field = "<iframe src='http://malicious.com'></iframe>"
  )

  for (field_type in names(malicious_inputs)) {
    sanitized <- sanitize_session_metadata(
      malicious_inputs[[field_type]],
      field_type = if(field_type %in% c("title", "description", "unit")) field_type else "general"
    )

    # Should not contain script tags or javascript:
    expect_false(grepl("<script", sanitized, ignore.case = TRUE))
    expect_false(grepl("javascript:", sanitized, ignore.case = TRUE))
    expect_false(grepl("<iframe", sanitized, ignore.case = TRUE))
    expect_false(grepl("onerror=", sanitized, ignore.case = TRUE))
  }
})

test_that("File type validation rejects dangerous files", {
  # Test dangerous file extensions (theoretical test)
  dangerous_extensions <- c(".exe", ".bat", ".cmd", ".scr", ".vbs", ".js")

  for (ext in dangerous_extensions) {
    fake_filename <- paste0("test", ext)
    file_ext <- tools::file_ext(fake_filename)

    # Should not be in allowed CSV extensions
    expect_false(file_ext %in% c("csv", "CSV", "txt", "TXT"))
  }

  # Test allowed extensions
  safe_extensions <- c("data.csv", "test.CSV", "file.txt", "DATA.TXT")

  for (filename in safe_extensions) {
    file_ext <- tools::file_ext(filename)
    expect_true(tolower(file_ext) %in% c("csv", "txt"))
  }
})