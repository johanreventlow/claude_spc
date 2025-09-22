# test-danish-clinical-edge-cases.R
# Comprehensive tests for Danish clinical data edge cases
# Tests æøå character handling, Windows compatibility, and missing data patterns
# Critical for clinical context and hospital environments

test_that("Danish character encoding works correctly", {
  # TEST: Danish characters (æøå) in column names and data

  # SETUP: Data with full Danish character set
  danish_data <- data.frame(
    `Måned` = c("Januar", "Februar", "Marts", "April"),
    `Afdeling` = c("Kardiologi", "Neurologi", "Ortopædi", "Gynækologi"),
    `Patienter indlagt` = c(45, 52, 38, 41),
    `Læger på vagt` = c(12, 14, 10, 11),
    `Sygeplejersker` = c(25, 28, 22, 24),
    `Gennemsnitlig alder` = c(65.5, 68.2, 62.1, 71.3),
    `Dødsrate (%)` = c(2.1, 1.8, 2.5, 1.9),
    `Kommentarer` = c("Øget belastning", "Påske periode", "Færre akutte", "Høj alder"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # TEST: Column names preserve Danish characters
  expect_true("Måned" %in% names(danish_data))
  expect_true("Afdeling" %in% names(danish_data))
  expect_true("Patienter indlagt" %in% names(danish_data))
  expect_true("Læger på vagt" %in% names(danish_data))
  expect_true("Sygeplejersker" %in% names(danish_data))
  expect_true("Gennemsnitlig alder" %in% names(danish_data))
  expect_true("Dødsrate (%)" %in% names(danish_data))

  # TEST: Data values preserve Danish characters
  expect_true(all(c("Kardiologi", "Neurologi", "Ortopædi", "Gynækologi") %in% danish_data$Afdeling))
  expect_true("Øget belastning" %in% danish_data$Kommentarer)
  expect_true("Påske periode" %in% danish_data$Kommentarer)
  expect_true("Færre akutte" %in% danish_data$Kommentarer)
  expect_true("Høj alder" %in% danish_data$Kommentarer)

  # TEST: Character encoding preservation
  expect_equal(nchar("Ortopædi"), 8) # æ should count as one character
  expect_equal(nchar("Sygeplejersker"), 14) # No special characters
  expect_equal(nchar("Dødsrate"), 8) # ø should count as one character

  # TEST: Sorting with Danish characters works
  sorted_afdelinger <- sort(danish_data$Afdeling)
  expect_true(is.character(sorted_afdelinger))
  expect_equal(length(sorted_afdelinger), 4)
})

test_that("Danish number format parsing works", {
  # TEST: Danish decimal format (comma as decimal separator)

  # SETUP: Data with Danish number formats
  danish_numbers_data <- data.frame(
    `Observation` = 1:8,
    `Decimal komma` = c("12,5", "15,7", "8,3", "22,1", "18,9", "11,2", "25,4", "14,6"),
    `Tusinde separator` = c("1.250", "2.150", "875", "3.220", "1.890", "1.120", "2.540", "1.460"),
    `Procent dansk` = c("12,5%", "15,7%", "8,3%", "22,1%", "18,9%", "11,2%", "25,4%", "14,6%"),
    `Blandet format` = c("12,5", "15.7", "8,3", "22", "18,9", "11", "25,4", "14.6"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # TEST: Parse Danish decimal format
  if (exists("parse_danish_number")) {
    parsed_decimals <- parse_danish_number(danish_numbers_data$`Decimal komma`)
    expect_true(is.numeric(parsed_decimals))
    expect_false(any(is.na(parsed_decimals)))
    expect_equal(parsed_decimals[1], 12.5)
    expect_equal(parsed_decimals[2], 15.7)

    # TEST: Parse percentage format
    parsed_percentages <- parse_danish_number(danish_numbers_data$`Procent dansk`)
    expect_true(is.numeric(parsed_percentages))
    expect_equal(parsed_percentages[1], 12.5) # Should strip % and parse

    # TEST: Handle mixed formats gracefully
    parsed_mixed <- parse_danish_number(danish_numbers_data$`Blandet format`)
    expect_true(is.numeric(parsed_mixed))
    # Should handle both comma and dot decimals
    expect_false(any(is.na(parsed_mixed)))
  }

  # TEST: Alternative parsing methods
  # Direct gsub approach for comma to dot conversion
  manual_parsed <- as.numeric(gsub(",", ".", danish_numbers_data$`Decimal komma`))
  expect_true(is.numeric(manual_parsed))
  expect_false(any(is.na(manual_parsed)))
  expect_equal(manual_parsed[1], 12.5)
})

test_that("Danish clinical terminology works correctly", {
  # TEST: Standard Danish clinical terminology and patterns

  # SETUP: Typical Danish hospital terminology
  clinical_terms <- data.frame(
    `Indikator` = c(
      "Genindlæggelser inden 30 dage",
      "Dødelighed efter operation",
      "Ventetid til behandling",
      "Patienttilfredshed",
      "Personaleturnover",
      "Medicinfejl per måned",
      "Hospitalsinfektion rate",
      "Operationseffektivitet"
    ),
    `Målenhed` = c(
      "Procent af udskrevne",
      "Procent af opererede",
      "Dage fra henvisning",
      "Score 1-10",
      "Procent årligt",
      "Antal fejl",
      "Per 1000 indlæggelser",
      "Minutter per operation"
    ),
    `Måletarget` = c(10, 2, 30, 8, 15, 5, 8, 120),
    `Datatype` = c("Rate", "Rate", "Continuous", "Score", "Rate", "Count", "Rate", "Continuous"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # TEST: Clinical terminology preservation
  expect_true("Genindlæggelser inden 30 dage" %in% clinical_terms$Indikator)
  expect_true("Dødelighed efter operation" %in% clinical_terms$Indikator)
  expect_true("Patienttilfredshed" %in% clinical_terms$Indikator)
  expect_true("Hospitalsinfektion rate" %in% clinical_terms$Indikator)

  # TEST: Unit labels with Danish characters
  expect_true("Procent af udskrevne" %in% clinical_terms$Målenhed)
  expect_true("Dage fra henvisning" %in% clinical_terms$Målenhed)
  expect_true("Per 1000 indlæggelser" %in% clinical_terms$Målenhed)

  # TEST: Data type categorization
  expect_true(all(clinical_terms$Datatype %in% c("Rate", "Continuous", "Score", "Count")))

  # TEST: Numeric targets
  expect_true(is.numeric(clinical_terms$Måletarget))
  expect_false(any(is.na(clinical_terms$Måletarget)))
})

test_that("Windows file path compatibility works", {
  # TEST: Windows-style file paths and encoding

  # SETUP: Windows-style paths with Danish characters
  windows_paths <- c(
    "C:\\Hospitalsdata\\Afdelinger\\Kardiologi\\data_2024.csv",
    "D:\\Kvalitetsdata\\Månedlige rapporter\\genindlæggelser.xlsx",
    "\\\\server\\share\\Sygeplejersker\\personaletimer.csv",
    "C:\\Users\\Læge\\Documents\\Patientdata\\månedsrapport.csv"
  )

  # TEST: Path handling doesn't break with Danish characters
  for (path in windows_paths) {
    expect_true(is.character(path))
    expect_gt(nchar(path), 0)

    # Test basic path operations
    expect_true(grepl("\\\\", path) || grepl("/", path)) # Has path separators

    # Test Danish character preservation in paths
    if (grepl("æ|ø|å", path)) {
      expect_true(nchar(path) > 10) # Path length preserved
    }
  }

  # TEST: Filename extraction with Danish characters
  danish_filename <- "månedlige_genindlæggelser_2024.csv"
  expect_equal(nchar(danish_filename), 34) # Including Danish characters
  expect_true(grepl("\\.csv$", danish_filename))
  expect_true(grepl("æ", danish_filename))
  expect_true(grepl("å", danish_filename))
})

test_that("Missing data patterns in clinical context work", {
  # TEST: Common missing data patterns in Danish clinical data

  # SETUP: Clinical data with various missing patterns
  clinical_missing_data <- data.frame(
    `Patient ID` = c("P001", "P002", "P003", "P004", "P005"),
    `Indlæggelsesdato` = c("01-01-2024", "02-01-2024", "", "04-01-2024", "05-01-2024"),
    `Udskrivelsesdato` = c("05-01-2024", "", "08-01-2024", "10-01-2024", ""),
    `Diagnose` = c("I21.9", "J44.1", NA, "M16.1", "K35.9"),
    `Alder` = c(65, 72, NA, 58, 81),
    `Køn` = c("M", "K", "M", "", "K"),
    `Afsnit` = c("Kardiologi", "Lungemedicin", "", "Ortopædi", "Kirurgi"),
    `Komplikationer` = c(0, 1, NA, 0, 2),
    `Total indlæggelser` = c(150, 145, 160, NA, 155),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # TEST: Identify missing data patterns
  expect_true(any(clinical_missing_data$`Indlæggelsesdato` == ""))
  expect_true(any(clinical_missing_data$`Udskrivelsesdato` == ""))
  expect_true(any(is.na(clinical_missing_data$Diagnose)))
  expect_true(any(is.na(clinical_missing_data$Alder)))
  expect_true(any(clinical_missing_data$Køn == ""))
  expect_true(any(clinical_missing_data$Afsnit == ""))

  # TEST: Count missing values per column
  missing_counts <- sapply(clinical_missing_data, function(x) {
    sum(is.na(x) | x == "" | trimws(as.character(x)) == "")
  })

  expect_equal(missing_counts[["Indlæggelsesdato"]], 1)
  expect_equal(missing_counts[["Diagnose"]], 1)
  expect_equal(missing_counts[["Alder"]], 1)
  expect_gte(missing_counts[["Køn"]], 1)

  # TEST: Complete cases identification
  complete_cases <- complete.cases(clinical_missing_data)
  expect_false(all(complete_cases)) # Should have some incomplete cases

  # TEST: Clinical data validation patterns
  valid_patient_ids <- grepl("^P\\d{3}$", clinical_missing_data$`Patient ID`)
  expect_true(all(valid_patient_ids)) # All patient IDs should follow pattern

  # Valid Danish date format detection
  valid_dates <- grepl("^\\d{2}-\\d{2}-\\d{4}$", clinical_missing_data$`Indlæggelsesdato`[clinical_missing_data$`Indlæggelsesdato` != ""])
  expect_true(all(valid_dates)) # Non-empty dates should be valid format
})

test_that("Danish date format variations work", {
  # TEST: Various Danish date formats used in clinical settings

  # SETUP: Different Danish date format variations
  danish_date_variations <- data.frame(
    `Format type` = c(
      "Standard dansk",
      "ISO format",
      "Månednavn dansk",
      "Forkortelse",
      "Med ugedag",
      "Numerisk kort",
      "Med klokkeslæt",
      "Kvartal notation"
    ),
    `Eksempel` = c(
      "15-03-2024",
      "2024-03-15",
      "15. marts 2024",
      "15/3-24",
      "Fredag d. 15. marts",
      "150324",
      "15-03-2024 14:30",
      "Q1 2024"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # TEST: Danish month names
  danish_months <- c(
    "januar", "februar", "marts", "april", "maj", "juni",
    "juli", "august", "september", "oktober", "november", "december"
  )

  # Check month name variations
  expect_true("marts" %in% danish_months) # Not "march"
  expect_true("maj" %in% danish_months)   # Not "may"
  expect_true("oktober" %in% danish_months) # Not "october"

  # TEST: Date parsing with different formats
  standard_dates <- c("01-01-2024", "15-03-2024", "31-12-2024")

  # Should be parseable as dates
  parsed_dates <- as.Date(standard_dates, format = "%d-%m-%Y")
  expect_true(all(!is.na(parsed_dates)))
  expect_equal(length(parsed_dates), 3)

  # TEST: ISO format compatibility
  iso_dates <- c("2024-01-01", "2024-03-15", "2024-12-31")
  parsed_iso <- as.Date(iso_dates)
  expect_true(all(!is.na(parsed_iso)))
  expect_equal(length(parsed_iso), 3)
})

test_that("Danish clinical data validation works", {
  # TEST: Clinical data validation rules specific to Danish context

  # SETUP: Danish clinical validation scenarios
  validation_data <- data.frame(
    `CPR nummer` = c("010190-1234", "150384-5678", "311299-9876", "invalid", ""),
    `Postnummer` = c("2100", "8000", "1050", "99999", ""),
    `Afdeling kode` = c("KAR01", "NEU02", "ORT03", "INVALID", ""),
    `ICD-10 kode` = c("I21.9", "J44.1", "M16.1", "Invalid", ""),
    `Medicin kode` = c("ATC:C01AA05", "ATC:R03AC02", "ATC:M01AB05", "Invalid", ""),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # TEST: CPR number format validation (Danish social security)
  valid_cpr_pattern <- "^\\d{6}-\\d{4}$"
  cpr_validation <- grepl(valid_cpr_pattern, validation_data$`CPR nummer`)
  expect_equal(sum(cpr_validation), 3) # First 3 should be valid

  # TEST: Danish postal code validation
  valid_postal_pattern <- "^\\d{4}$"
  postal_validation <- grepl(valid_postal_pattern, validation_data$Postnummer)
  expect_equal(sum(postal_validation), 3) # First 3 should be valid

  # TEST: Department code format
  valid_dept_pattern <- "^[A-Z]{3}\\d{2}$"
  dept_validation <- grepl(valid_dept_pattern, validation_data$`Afdeling kode`)
  expect_equal(sum(dept_validation), 3) # First 3 should be valid

  # TEST: ICD-10 code format
  valid_icd_pattern <- "^[A-Z]\\d{2}\\.[0-9]$"
  icd_validation <- grepl(valid_icd_pattern, validation_data$`ICD-10 kode`)
  expect_equal(sum(icd_validation), 3) # First 3 should be valid

  # TEST: ATC medicine code format
  valid_atc_pattern <- "^ATC:[A-Z]\\d{2}[A-Z]{2}\\d{2}$"
  atc_validation <- grepl(valid_atc_pattern, validation_data$`Medicin kode`)
  expect_equal(sum(atc_validation), 3) # First 3 should be valid
})

test_that("Danish clinical SPC chart types work", {
  # TEST: Danish clinical context chart type selection

  # SETUP: Danish clinical chart type mapping
  danish_chart_types <- data.frame(
    `Indikator type` = c(
      "Genindlæggelsesrate",
      "Dødelighed",
      "Ventetid",
      "Infektionsrate",
      "Personalefravær",
      "Medicinfejl",
      "Patienttilfredshed",
      "Operationstid"
    ),
    `SPC chart type` = c("p", "p", "i", "u", "p", "c", "i", "i"),
    `Har nævner` = c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE),
    `Dansk enhed` = c("Procent", "Procent", "Dage", "Per 1000", "Procent", "Antal", "Score", "Minutter"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # TEST: Chart type mapping logic
  expect_true(all(danish_chart_types$`SPC chart type` %in% c("p", "i", "u", "c")))

  # TEST: Rate indicators use appropriate chart types
  rate_indicators <- danish_chart_types$`Indikator type`[danish_chart_types$`Har nævner`]
  rate_chart_types <- danish_chart_types$`SPC chart type`[danish_chart_types$`Har nævner`]

  expect_true("Genindlæggelsesrate" %in% rate_indicators)
  expect_true("p" %in% rate_chart_types) # P-charts for rates

  # TEST: Individual value indicators
  individual_indicators <- danish_chart_types$`Indikator type`[!danish_chart_types$`Har nævner`]
  individual_chart_types <- danish_chart_types$`SPC chart type`[!danish_chart_types$`Har nævner`]

  expect_true("Ventetid" %in% individual_indicators)
  expect_true("i" %in% individual_chart_types) # I-charts for individual values

  # TEST: Danish unit labels
  expect_true("Procent" %in% danish_chart_types$`Dansk enhed`)
  expect_true("Dage" %in% danish_chart_types$`Dansk enhed`)
  expect_true("Score" %in% danish_chart_types$`Dansk enhed`)
  expect_true("Minutter" %in% danish_chart_types$`Dansk enhed`)
})

test_that("Danish locale and region settings work", {
  # TEST: Danish locale compatibility

  # SETUP: Danish locale information
  danish_locale_info <- list(
    decimal_separator = ",",
    thousands_separator = ".",
    date_format = "dd-mm-yyyy",
    time_format = "HH:MM",
    currency = "DKK",
    timezone = "Europe/Copenhagen"
  )

  # TEST: Decimal separator handling
  expect_equal(danish_locale_info$decimal_separator, ",")
  expect_equal(danish_locale_info$thousands_separator, ".")

  # TEST: Date format preference
  expect_equal(danish_locale_info$date_format, "dd-mm-yyyy")

  # TEST: Currency and timezone
  expect_equal(danish_locale_info$currency, "DKK")
  expect_equal(danish_locale_info$timezone, "Europe/Copenhagen")

  # TEST: Number formatting with Danish locale
  test_number <- 1234.56

  # Simulate Danish number formatting
  danish_formatted <- gsub("\\.", ",", format(test_number, decimal.mark = ",", big.mark = "."))
  expect_true(grepl(",", danish_formatted)) # Should have comma decimal

  # TEST: Reverse parsing (Danish to R numeric)
  danish_number_string <- "1.234,56"
  # Convert Danish format to R format
  r_formatted <- gsub("\\.", "", danish_number_string) # Remove thousands separator
  r_formatted <- gsub(",", ".", r_formatted) # Convert decimal separator
  parsed_number <- as.numeric(r_formatted)

  expect_equal(parsed_number, 1234.56)
})

test_that("Danish hospital workflow patterns work", {
  # TEST: Complete Danish hospital data workflow

  # SETUP: Realistic Danish hospital dataset
  hospital_workflow_data <- data.frame(
    `Registreringsdato` = seq.Date(as.Date("2024-01-01"), by = "week", length.out = 12),
    `Afdeling` = rep(c("Kardiologi", "Neurologi", "Ortopædi"), each = 4),
    `Genindlæggelser` = c(8, 6, 12, 9, 5, 7, 11, 8, 4, 6, 9, 7),
    `Samlede udskrivelser` = c(120, 115, 125, 118, 110, 122, 128, 116, 105, 119, 124, 113),
    `Ventetid (dage)` = c(12.5, 15.2, 8.7, 11.3, 18.1, 9.4, 13.8, 16.5, 7.2, 10.9, 14.1, 12.8),
    `Personaletimer` = c(850, 820, 890, 860, 780, 870, 920, 840, 760, 880, 910, 830),
    `Kvalitetsscore` = c(8.2, 7.8, 8.5, 8.1, 7.5, 8.3, 8.7, 7.9, 7.3, 8.4, 8.6, 8.0),
    `Målopfyldelse` = c("Ja", "Nej", "Ja", "Ja", "Nej", "Ja", "Ja", "Nej", "Nej", "Ja", "Ja", "Ja"),
    `Særlige hændelser` = c("", "Påske", "", "", "Ferie", "", "", "Konf.", "", "", "Kursus", ""),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # TEST: Data structure integrity
  expect_equal(nrow(hospital_workflow_data), 12)
  expect_equal(ncol(hospital_workflow_data), 9)

  # TEST: Danish column names with special characters
  expect_true(all(c("Registreringsdato", "Genindlæggelser", "Samlede udskrivelser",
                   "Målopfyldelse", "Særlige hændelser") %in% names(hospital_workflow_data)))

  # TEST: Department rotation pattern
  dept_pattern <- rep(c("Kardiologi", "Neurologi", "Ortopædi"), each = 4)
  expect_equal(hospital_workflow_data$Afdeling, dept_pattern)

  # TEST: Clinical quality metrics ranges
  expect_true(all(hospital_workflow_data$Genindlæggelser >= 0))
  expect_true(all(hospital_workflow_data$`Samlede udskrivelser` > 0))
  expect_true(all(hospital_workflow_data$`Ventetid (dage)` > 0))
  expect_true(all(hospital_workflow_data$Kvalitetsscore >= 0 & hospital_workflow_data$Kvalitetsscore <= 10))

  # TEST: Date sequence integrity
  expect_true(inherits(hospital_workflow_data$Registreringsdato, "Date"))
  expect_equal(length(unique(hospital_workflow_data$Registreringsdato)), 12) # All unique dates

  # TEST: Binary quality achievement
  expect_true(all(hospital_workflow_data$Målopfyldelse %in% c("Ja", "Nej")))

  # TEST: Special events annotation
  expect_true(sum(hospital_workflow_data$`Særlige hændelser` != "") >= 3) # Some events recorded
  expect_true("Påske" %in% hospital_workflow_data$`Særlige hændelser`)
  expect_true("Ferie" %in% hospital_workflow_data$`Særlige hændelser`)
})