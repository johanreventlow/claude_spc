# Makefile for claudespc R package
# Provides build pipeline for package development and deployment

# Variables
PACKAGE_NAME = claudespc
VERSION = $(shell grep '^Version:' DESCRIPTION | sed 's/Version: //')
TARBALL = $(PACKAGE_NAME)_$(VERSION).tar.gz

# Default target
.PHONY: all
all: check

# Install package dependencies
.PHONY: deps
deps:
	@echo "Installing package dependencies..."
	Rscript -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); if (!requireNamespace('devtools', quietly = TRUE)) install.packages('devtools')"
	Rscript -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); if (!requireNamespace('roxygen2', quietly = TRUE)) install.packages('roxygen2')"
	Rscript -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); if (!requireNamespace('testthat', quietly = TRUE)) install.packages('testthat')"
	Rscript -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); if (!requireNamespace('golem', quietly = TRUE)) install.packages('golem')"
	Rscript -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::install_deps(dependencies = TRUE)"

# Generate documentation
.PHONY: document
document:
	@echo "Generating documentation..."
	Rscript -e "devtools::document()"

# Build package
.PHONY: build
build: document
	@echo "Building package..."
	R CMD build .

# Install package locally
.PHONY: install
install: build
	@echo "Installing package locally..."
	R CMD INSTALL $(TARBALL)

# Run R CMD check
.PHONY: check
check: document
	@echo "Running R CMD check..."
	Rscript -e "devtools::check()"

# Run tests
.PHONY: test
test:
	@echo "Running tests..."
	Rscript -e "devtools::test()"

# Run tests with coverage
.PHONY: coverage
coverage:
	@echo "Running test coverage..."
	Rscript -e "covr::package_coverage()"

# Lint code
.PHONY: lint
lint:
	@echo "Linting code..."
	Rscript -e "lintr::lint_package()"

# Clean build artifacts
.PHONY: clean
clean:
	@echo "Cleaning build artifacts..."
	rm -f $(PACKAGE_NAME)_*.tar.gz
	rm -rf $(PACKAGE_NAME).Rcheck/
	rm -rf man/*.Rd

# Development workflow: install and test
.PHONY: dev
dev: install test
	@echo "Development install and test complete"

# Production build: full check and build
.PHONY: prod
prod: deps check build
	@echo "Production build complete: $(TARBALL)"

# Quick development cycle
.PHONY: quick
quick: document install
	@echo "Quick install complete"

# Run app after installing package
.PHONY: run
run: install
	@echo "Running app..."
	Rscript -e "claudespc::run_app()"

# CI/CD pipeline simulation
.PHONY: ci
ci: deps document lint test check build
	@echo "CI pipeline complete"

# Help target
.PHONY: help
help:
	@echo "Available targets:"
	@echo "  deps     - Install package dependencies"
	@echo "  document - Generate documentation"
	@echo "  build    - Build package tarball"
	@echo "  install  - Install package locally"
	@echo "  check    - Run R CMD check"
	@echo "  test     - Run tests"
	@echo "  coverage - Run test coverage"
	@echo "  lint     - Lint code"
	@echo "  clean    - Clean build artifacts"
	@echo "  dev      - Development workflow (install + test)"
	@echo "  prod     - Production build (full check + build)"
	@echo "  quick    - Quick development cycle (document + install)"
	@echo "  run      - Run app after installing"
	@echo "  ci       - Full CI pipeline"
	@echo "  help     - Show this help"