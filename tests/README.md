# Test Structure

## Overview

Tests er organiseret i kategorier for optimal feedback loop og CI/CD performance.

## Directory Structure

```
tests/
├── testthat/           # Fast unit tests (seconds)
│   ├── test-*.R        # Unit tests for individual components
│   └── helper-*.R      # Test helpers and utilities
│
├── performance/        # Slow performance tests (minutes)
│   ├── test-*-performance.R      # Performance benchmarks
│   ├── test-startup-*.R          # Startup optimization tests
│   └── test-microbenchmark-*.R   # Microbenchmark tests
│
├── integration/        # Integration tests (future)
│   └── test-full-workflow.R      # End-to-end workflow tests
│
├── run_unit_tests.R         # Run only unit tests
├── run_performance_tests.R  # Run only performance tests
├── run_integration_tests.R  # Run only integration tests
└── run_all_tests.R          # Run all test suites
```

## Test Runners

### Quick Unit Tests (CI/CD)

```r
# Run fast unit tests only
Rscript tests/run_unit_tests.R
```

**Use case**: Continuous integration, pre-commit hooks, quick feedback.

### Performance Tests (Release branches)

```r
# Run performance benchmarks
Rscript tests/run_performance_tests.R
```

**Use case**: Release validation, performance regression detection.

### Integration Tests

```r
# Run integration tests
Rscript tests/run_integration_tests.R
```

**Use case**: Full workflow validation, pre-release testing.

### All Tests (Full validation)

```r
# Run comprehensive test suite
Rscript tests/run_all_tests.R
```

**Use case**: Pre-release validation, major refactoring verification.

## Test Categories

### Unit Tests (`testthat/`)

**Characteristics**:
- Fast execution (< 10 seconds total)
- Test individual functions and components
- Mock external dependencies
- Run on every commit

**Examples**:
- `test-safe-operation.R` - Error handling utilities
- `test-logging.R` - Logging infrastructure
- `test-autodetect.R` - Column auto-detection logic

### Performance Tests (`performance/`)

**Characteristics**:
- Slow execution (minutes)
- Benchmarking and profiling
- Real data and dependencies
- Run on release branches only

**Examples**:
- `test_startup_performance.R` - App initialization benchmarks
- `test-fase5-performance.R` - Plot generation performance
- `test-microbenchmark-integration.R` - Component microbenchmarks

### Integration Tests (`integration/`)

**Characteristics**:
- Medium execution time
- Test component interaction
- Verify workflows end-to-end
- Run before releases

**Examples** (future):
- `test-full-workflow.R` - Complete user workflow
- `test-session-lifecycle.R` - Session management

## CI/CD Integration

### GitHub Actions Example

```yaml
# .github/workflows/test.yml

jobs:
  unit-tests:
    runs-on: ubuntu-latest
    steps:
      - name: Run unit tests
        run: Rscript tests/run_unit_tests.R

  performance-tests:
    runs-on: ubuntu-latest
    # Only on master branch
    if: github.ref == 'refs/heads/master'
    steps:
      - name: Run performance tests
        run: Rscript tests/run_performance_tests.R
```

## Writing Tests

### Unit Test Template

```r
# tests/testthat/test-my-component.R

test_that("component does expected behavior", {
  # Arrange
  input <- create_test_input()

  # Act
  result <- my_function(input)

  # Assert
  expect_equal(result, expected_value)
})
```

### Performance Test Template

```r
# tests/performance/test-my-benchmark.R

test_that("operation completes within threshold", {
  # Benchmark
  timing <- system.time({
    result <- expensive_operation()
  })

  # Assert performance
  expect_lt(timing["elapsed"], 1.0)  # < 1 second
})
```

## Best Practices

1. **Keep unit tests fast** - Mock external dependencies
2. **Isolate performance tests** - Don't block development feedback
3. **Use descriptive names** - Clear test intent
4. **One assertion per test** - Easy failure diagnosis
5. **Clean up after tests** - No side effects

## Migration Notes

**Sprint 4 Fase 3** - Separated performance tests from unit tests:
- Moved 16 performance test files to `tests/performance/`
- Created test runner scripts for selective execution
- Improved CI/CD feedback loop (unit tests ~seconds vs all tests ~minutes)

---

**Last Updated**: 2025-01-10 (Sprint 4 Fase 3)
