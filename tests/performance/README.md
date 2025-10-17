# Performance Benchmark Suite

## Overview

Comprehensive performance benchmarks for BFHcharts vs qicharts2 backends, including memory profiling and cache performance testing.

**Task:** 033 Stream 2 - Performance Optimization & Caching
**Created:** 2025-10-17

---

## Test Files

### 1. test-bfh-vs-qic-benchmark.R
**Purpose:** Compare BFHcharts and qicharts2 rendering performance

**Tests:**
- Performance by data size (50, 100, 500, 1000 rows)
- Performance by chart type (run, i, p, c, u, xbar, s)
- Multi-phase chart performance
- Latency percentiles (P50, P95, P99)
- Cache hit vs miss speedup
- Cache hit rate measurement
- Parallel rendering with cache

**Duration:** ~10-15 minutes

### 2. test-bfh-memory-profile.R
**Purpose:** Memory leak detection and profiling

**Tests:**
- Long session memory leak (1000 renders, ~4 hours simulation)
- Memory usage by data size
- Memory usage by chart type
- Profvis profiling (single and multi-chart)
- Memory stability tests
- Cleanup verification

**Duration:** ~20-30 minutes

### 3. generate_benchmark_report.R
**Purpose:** Generate comprehensive markdown report

**Features:**
- Aggregates all benchmark results
- Automated pass/fail checking
- Context-aware recommendations
- Can run as script or sourced function

**Duration:** <1 minute

---

## Quick Start

### Run All Benchmarks
```r
# Load project
library(SPCify)

# Run benchmarks
testthat::test_file('tests/performance/test-bfh-vs-qic-benchmark.R')
testthat::test_file('tests/performance/test-bfh-memory-profile.R')

# Generate report
source('tests/performance/generate_benchmark_report.R')
generate_performance_report()
```

### Run Specific Tests
```r
# Only data size benchmarks
testthat::test_file(
  'tests/performance/test-bfh-vs-qic-benchmark.R',
  filter = "data size"
)

# Only cache tests
testthat::test_file(
  'tests/performance/test-bfh-vs-qic-benchmark.R',
  filter = "Cache"
)

# Only memory leak test
testthat::test_file(
  'tests/performance/test-bfh-memory-profile.R',
  filter = "long session"
)
```

### View Results
```r
# Open profiling report
browseURL('tests/performance/bfh_profile.html')

# Read benchmark report
file.show('tests/performance/BENCHMARK_REPORT.md')

# Load raw results
results <- readRDS('tests/performance/benchmark_run_chart_results.rds')
print(results)
```

---

## Requirements

### R Packages
- **bench** - Statistical benchmarking
- **profvis** - Memory and performance profiling
- **testthat** - Test framework
- **BFHcharts** - SPC chart backend
- **qicharts2** - Comparison baseline

### System Requirements
- Minimum 8 GB RAM (for long session test)
- ~1 GB disk space for results and reports
- Interactive R session (for profvis)

---

## Performance Targets

### Rendering Performance
- **Target:** BFHcharts ≤ 110% qicharts2 time (P50)
- **Measured:** Median time across all chart types and data sizes

### Latency
- **P50:** <500ms (medium datasets, 100-1000 rows)
- **P95:** <1s (medium datasets)
- **P99:** <2s (medium datasets)

### Memory
- **Long session:** <50 MB growth over 1000 renders
- **Stability:** No leaks in repeated identical renders

### Cache
- **Speedup:** ≥10x faster (cache hit vs miss)
- **Hit rate:** ≥80% (production target, measured via logging)

---

## Output Files

### Benchmark Results (.rds)
- `benchmark_run_chart_results.rds` - Data size benchmarks
- `benchmark_all_charts_results.rds` - Chart type benchmarks
- `benchmark_multiphase_results.rds` - Multi-phase benchmarks
- `cache_speedup_results.rds` - Cache speedup metrics
- `cache_hit_rate_results.rds` - Cache hit rate data

### Memory Results (.rds)
- `memory_profile_long_session.rds` - Long session test
- `memory_usage_by_size.rds` - Memory by data size
- `memory_usage_by_chart_type.rds` - Memory by chart type

### Profiling Reports (.html)
- `bfh_profile.html` - Single render profile
- `bfh_profile_multi_chart.html` - Multi-chart profile

### Report (.md)
- `BENCHMARK_REPORT.md` - Comprehensive performance report

---

## Interpreting Results

### Benchmark Tables
```
| Data Size | qicharts2 (P50) | BFHcharts (P50) | Ratio | Status |
|-----------|-----------------|-----------------|-------|--------|
| 100       | 45.2 ms         | 48.3 ms         | 1.07x | ✅ PASS |
```

- **Ratio < 1.0:** BFHcharts faster
- **Ratio = 1.0:** Equal performance
- **Ratio ≤ 1.10:** Within acceptable threshold (PASS)
- **Ratio > 1.10:** Performance regression (FAIL)

### Memory Profiles
- **Growth < 50 MB:** No leak (PASS)
- **Growth ≥ 50 MB:** Potential leak (FAIL)
- **Stable memory:** Good (repeated renders same memory)
- **Growing memory:** Bad (investigate allocation)

### Cache Performance
- **Speedup ≥ 10x:** Excellent
- **Speedup 5-10x:** Good
- **Speedup < 5x:** Poor (investigate)

---

## Troubleshooting

### Tests Skipped
```
Test skipped: bench package not available
```

**Solution:** Install missing packages
```r
install.packages(c("bench", "profvis"))
```

### Out of Memory
```
Error: cannot allocate vector of size X MB
```

**Solution:**
- Reduce iterations in long session test
- Close other applications
- Increase system RAM

### Profvis Not Working
```
Error: profvis requires interactive session
```

**Solution:**
- Run in RStudio or R console (not Rscript)
- Or skip profvis tests

### Slow Execution
**Expected:** Memory tests can take 20-30 minutes

**Options:**
- Run overnight
- Reduce iterations (edit test file)
- Run only specific tests

---

## Best Practices

### When to Run
- **Before major releases** - Validate performance
- **After BFHcharts updates** - Check for regressions
- **Monthly** - Monitor trends
- **After optimization work** - Measure improvements

### How Often
- **Full suite:** Monthly or before releases
- **Quick benchmarks:** After significant changes
- **Memory tests:** Quarterly or when leak suspected

### What to Track
- Benchmark ratios over time (trend analysis)
- Memory growth patterns
- Cache performance metrics
- Any FAIL status (investigate immediately)

---

## Contributing

### Adding New Benchmarks
1. Add test to appropriate file
2. Follow existing patterns (use `bench::mark()`)
3. Save results as `.rds` file
4. Update report generator if needed
5. Document in this README

### Modifying Thresholds
Current thresholds in tests:
- Performance: 1.10 (110%)
- Memory: 50 MB
- Cache speedup: 10x
- Cache hit rate: 80%

**To change:**
Edit test expectations in test files, then update this README.

---

## References

- [Task 033 Analysis](../../.claude/epics/bfhcharts-spc-migration/33-analysis.md)
- [Task 033 Definition](../../.claude/epics/bfhcharts-spc-migration/33.md)
- [Stream 2 Progress](../../.claude/epics/bfhcharts-spc-migration/updates/33/stream-2.md)
- [bench package docs](https://bench.r-lib.org/)
- [profvis package docs](https://rstudio.github.io/profvis/)

---

_Last updated: 2025-10-17_
_Maintainer: Task 033 Stream 2_
