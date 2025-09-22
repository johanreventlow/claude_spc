# Testing og Code Quality Guide

Dette dokument beskriver test og code quality setup for SPC appen.

## Test Framework

### shinytest2 + testthat

Projektet bruger `shinytest2` til end-to-end app testing og `testthat` til unit tests.

### Mappestruktur
```
tests/
├── testthat.R              # Test runner
└── testthat/
    ├── helper.R             # Test setup og hjælpefunktioner
    ├── test-app-basic.R     # App startup og grundlæggende UI tests
    ├── test-data-validation.R  # Data validering og hjælpefunktioner
    └── test-error-handling.R   # Error handling og robusthed
```

### Kørsel af Tests

```r
# Alle tests
testthat::test_dir("tests/testthat")

# Enkelt test fil
testthat::test_file("tests/testthat/test-data-validation.R")

# Unit tests (skip browser tests)
testthat::test_dir("tests/testthat", filter = "^(?!test-app)")
```

### Test Kategorier

**App Tests** (`test-app-*.R`):
- End-to-end app funktionalitet 
- Browser automation med shinytest2
- Skip på CI servere med `skip_on_ci()`

**Unit Tests** (`test-*.R`):
- Hjælpefunktioner og validering
- Business logic
- Error handling

## Code Quality

### Linting med lintr

Projektet bruger `lintr` til static code analysis.

**Konfiguration**: `.lintr` 
- Danske kommentarer: 120 karakter linje længde
- Snake_case og camelCase tilladt
- Mindre strikt på assignment og spacing

### Code Formatting med styler

Automatisk code formatting følger `tidyverse` style guide.

### Development Workflow

**Kør linting og styling**:
```r
source("dev/lint_and_style.R")
```

Dette script:
1. Kører lintr på alle R filer
2. Viser issues der skal rettes manuelt  
3. Aplicerer automatisk styling med styler
4. Giver status rapport

### Best Practices

**Før commit**:
1. Kør `source("dev/lint_and_style.R")`
2. Ret eventuelle linting issues
3. Kør relevante tests
4. Commit med beskrivende besked

**Test Guidelines**:
- Tests skal være uafhængige og reproducible
- Brug beskrivende test navne på dansk
- Mock eksterne dependencies
- Test både happy path og edge cases

**Code Quality**:
- Følg lintr anbefalinger
- Brug auto-styling til konsistent formatering
- Kommentér kompleks business logic
- Undgå for lange funktioner (> 50 linjer)