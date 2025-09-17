# SPC App - Statistical Process Control i R Shiny

En professionel Shiny applikation til **Statistical Process Control (SPC)** analyser med dansk interface og integration med qicharts2. Udviklet til klinisk kvalitetsarbejde med fokus på stabilitet, brugervenlighed og danske standarder.

## 🔧 Features

### Core Funktionalitet
- **SPC Charts**: I-kort, MR-kort, P-kort, U-kort med automatisk beregning af kontrolgrænser
- **Auto-detektion**: Intelligent kolonne matching baseret på navne og data karakteristika
- **Dansk Support**: Komplet understøttelse af danske karakterer, dato formater og CSV standarder
- **Excel/CSV Import**: Robust file håndtering med metadata preservation
- **Interactive UI**: Moderne Bootstrap interface med real-time feedback

### Tekniske Highlights
- **Centraliseret State Management**: Phase 4 arkitektur med dual-state compatibility
- **Test-Driven Development**: >95% test coverage med comprehensive integration tests
- **Robust Error Handling**: Graceful degradation og brugervenlig fejlhåndtering
- **Performance Optimized**: Debounced operations og memory management
- **Danish Locale**: ISO-8859-1 encoding, komma decimal separator, danske labels

## 🚀 Quick Start

### Prerequisites
```r
# Required R packages
install.packages(c("shiny", "DT", "qicharts2", "readr", "readxl",
                   "bslib", "shinyWidgets", "shinycssloaders", "waiter"))
```

### Start Application
```r
# Clone repository
git clone <repository-url>
cd claude_spc

# Start app
R -e "shiny::runApp('.', port = 3838)"

# Alternative med development settings
R -e "source('global.R'); shiny::runApp('.', port = 4040)"
```

### Development Mode
```r
# Enable development features
TEST_MODE_AUTO_LOAD <- TRUE
AUTO_RESTORE_ENABLED <- FALSE

# Start med debug logging
SPC_LOG_LEVEL=DEBUG R -e "shiny::runApp('.', port = 5050)"
```

## 📊 Usage Examples

### Basic SPC Analysis
1. **Upload Data**: CSV eller Excel fil med danske formater
2. **Auto-Detektion**: App matcher automatisk kolonner til SPC standarder
3. **Kolonne Setup**: Juster X-akse, Tæller, Nævner efter behov
4. **Chart Generation**: Vælg chart type og generer SPC plot
5. **Export Results**: Download Excel med session metadata

### Supported Data Formats
```csv
Dato;Tæller;Nævner;Skift;Frys;Kommentarer
01-01-2024;95;100;FALSE;FALSE;Baseline
01-02-2024;92;95;FALSE;FALSE;Normal
01-03-2024;98;102;TRUE;FALSE;Intervention
```

## 🏗️ Architecture

### Project Structure
```
claude_spc/
├── R/                          # R source files
│   ├── app_server.R           # Main server logic
│   ├── app_ui.R               # User interface
│   ├── fct_data_processing.R  # Data & column management
│   ├── fct_file_operations.R  # File I/O & uploads
│   ├── fct_spc_calculations.R # SPC computations
│   ├── utils_*.R              # Utility functions
│   └── constants.R            # App constants
├── tests/testthat/            # Test suites
├── docs/                      # Documentation
├── global.R                   # Global configuration
└── CLAUDE.md                  # Development guide
```

### State Management (Phase 4)
```r
# Centralized app state structure
app_state <- list(
  data = list(current_data = NULL, file_info = NULL),
  session = list(auto_save_enabled = TRUE, file_uploaded = FALSE),
  ui = list(hide_anhoej_rules = FALSE)
)

# Legacy compatibility maintained
values <- reactiveValues(...)  # Still supported
```

## 🧪 Testing

### Run Test Suite
```bash
# All tests
R -e "source('global.R'); testthat::test_dir('tests/testthat')"

# Specific test category
R -e "testthat::test_file('tests/testthat/test-name-only-detection-final.R')"
R -e "testthat::test_file('tests/testthat/test-end-to-end-app-flow.R')"
```

### Test Coverage Status
- **Name-only Detection**: 47 tests, >95% coverage
- **File Operations**: 39 tests, >90% coverage
- **Cross-component Reactive**: >85% coverage
- **End-to-end Integration**: 88 tests, >95% coverage
- **Data Consistency**: >85% coverage

## 📚 Documentation

### Developer Resources
- **[CLAUDE.md](CLAUDE.md)**: Comprehensive development guide
- **[TECHNICAL_IMPROVEMENTS_PLAN.md](TECHNICAL_IMPROVEMENTS_PLAN.md)**: Roadmap og progress tracking
- **[docs/ARCHITECTURE_OVERVIEW.md](docs/ARCHITECTURE_OVERVIEW.md)**: System architecture
- **[docs/DEVELOPER_GUIDE.md](docs/DEVELOPER_GUIDE.md)**: Detailed development patterns

### API Documentation
```r
# Generate roxygen2 documentation
roxygen2::roxygenise()

# View function documentation
?setup_column_management
?detect_columns_name_only
?validate_x_column_format
```

## 🔧 Configuration

### Environment Variables
```bash
# Logging configuration
export SPC_LOG_LEVEL=DEBUG    # DEBUG, INFO, WARN, ERROR

# Development settings
export TEST_MODE_AUTO_LOAD=TRUE
export AUTO_RESTORE_ENABLED=FALSE
```

### CSV Format Settings
```r
# Danish locale (automatic)
locale(
  decimal_mark = ",",
  grouping_mark = ".",
  encoding = "ISO-8859-1"
)
```

## 🛠️ Development

### Code Quality Standards
- **Test-Driven Development**: Skriv tests først
- **Danish Comments**: Funktionalitet beskrives på dansk
- **English Function Names**: API på engelsk
- **Robust Error Handling**: Graceful degradation patterns
- **Performance First**: Debounced operations og memory management

### Contribution Workflow
1. **Problem Definition**: Start med én linje problem statement
2. **Test Design**: Skriv tests for ønsket adfærd FØRST
3. **Implementation**: Minimal viable change
4. **Test Verification**: Alle tests skal bestå
5. **Integration Testing**: Test full app workflow
6. **Documentation**: Opdater denne README hvis nødvendigt

### Git Convention
```bash
# Danish conventional commits
git commit -m "feat(spc): tilføj support for U-kort beregninger

Implementer U-kort funktionalitet med:
- Automatisk rate beregning
- Konfidensgrænser baseret på Poisson
- Integration med eksisterende chart pipeline

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

## 📈 Performance

### Benchmarks
- **App Start**: < 3 sekunder (med cached libraries)
- **File Upload**: < 5 sekunder for 1000+ rækker
- **Chart Generation**: < 2 sekunder for standard datasets
- **Memory Usage**: < 100MB for typical sessions

### Optimization Features
- **Lazy Loading**: Reactive expressions kun når nødvendigt
- **Debounced Operations**: Undgår excessive computations
- **Memory Management**: Automatic cleanup ved session end
- **Caching**: Intelligent data og computation caching

## 🏥 Clinical Integration

### Hospital Compatibility
- **Windows Support**: Testet på Windows hospital netværk
- **UTF-8 & ISO-8859-1**: Dual encoding support
- **Network Restrictions**: Fungerer bag hospital firewalls
- **Data Security**: Ingen data forlader local environment

### Quality Improvement Workflow
1. **Data Export**: Fra hospital systemer som CSV/Excel
2. **SPC Analysis**: Upload til app for automatisk analyse
3. **Chart Generation**: Professional SPC charts med kontrolgrænser
4. **Results Export**: Excel rapport med metadata for arkivering
5. **Quality Review**: Charts klar til quality meetings

## 🐛 Troubleshooting

### Common Issues
```r
# Debug logging
SPC_LOG_LEVEL=DEBUG R -e "shiny::runApp('.', port = 4040)"

# Clear session state
rm(list = ls())
source('global.R')

# Test specific components
testthat::test_file('tests/testthat/test-data-consistency.R')
```

### Error Recovery
- **File Upload Fejl**: Tjek encoding og CSV format
- **Auto-detektion Fejl**: Bekræft standard kolonne navne
- **Chart Generation Fejl**: Valider data typer og missing values
- **Performance Problemer**: Reducer data størrelse eller clear cache

## 📝 Changelog

Se [TECHNICAL_IMPROVEMENTS_PLAN.md](TECHNICAL_IMPROVEMENTS_PLAN.md) for detaljeret progress tracking.

### Recent Updates
- ✅ **Phase 4**: Centraliseret state management implementation
- ✅ **A) Code Quality**: 223 debug statements → struktureret logging system
- ✅ **B) Testing Infrastructure**: Comprehensive test coverage >90%
- 🚧 **C) Documentation Excellence**: roxygen2 integration og developer guides

## 📄 License

Dette projekt er udviklet til intern hospitalsbrug. Kontakt udviklingsteam for licensing spørgsmål.

## 🤝 Support

### Development Team
- **Technical Lead**: Se [CLAUDE.md](CLAUDE.md) for development guidelines
- **Issues**: Rapporter via GitHub Issues eller internal ticketing system
- **Documentation**: Konsulter [docs/](docs/) directory for detaljeret guidance

### Resources
- **qicharts2 Documentation**: https://github.com/anhoej/qicharts2
- **Shiny Best Practices**: Se [SHINY_BEST_PRACTICES_FASER.md](SHINY_BEST_PRACTICES_FASER.md)
- **Danish R Community**: Integration med lokale R brugergrupper

---

**Udviklet med ❤️ for dansk sundhedsvæsen**

*Sidste opdatering: 16. september 2025*