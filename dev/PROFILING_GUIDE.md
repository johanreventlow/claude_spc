# Profiling Guide - SPCify Development Tools

> **Note**: Disse værktøjer er markeret som `@keywords internal` og eksporteres ikke i NAMESPACE. De er beregnet til lokal udvikling og performance-analyse.

## Oversigt

SPCify indeholder tre profiling utilities til performance-analyse under udvikling:

1. **`profile_reactive()`** - Detaljeret profiling af reactive expressions
2. **`benchmark_reactives()`** - Sammenligning af alternative implementations
3. **`profile_data_pipeline()`** - Step-by-step pipeline timing

Disse funktioner findes i `R/utils_profiling.R`.

---

## 1. Profile Reactive Expressions

### Brug
Identificér performance bottlenecks i reactive expressions med detaljeret call stack analyse.

### Setup
```r
# Load funktionen manuelt (ikke eksporteret)
source("R/utils_profiling.R")

# Kræver profvis package
install.packages("profvis")
```

### Eksempel
```r
# I server function eller interactive session
result <- profile_reactive(
  reactive({
    app_state$data$current_data %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::group_by(category) %>%
      dplyr::summarise(
        mean = mean(value),
        sd = sd(value)
      )
  }),
  label = "data_aggregation",
  interval = 0.005  # 5ms sampling interval
)

# View interactive profiling results
print(result)
```

### Output
- **Flame graph** af function calls
- **Line-by-line timing**
- **Memory allocations** per function
- **Call stack** med execution time

### Use Cases
- Slow reactive expressions
- Identificer nested loops
- Memory-intensive operations
- Unødvendige re-computations

---

## 2. Benchmark Reactive Implementations

### Brug
Sammenlign performance af alternative implementations for at vælge den mest effektive.

### Setup
```r
source("R/utils_profiling.R")

# Kræver bench package
install.packages("bench")
```

### Eksempel
```r
# Sammenlign base R vs tidyverse filtering
benchmark_result <- benchmark_reactives(
  base_r = reactive({
    data[data$value > 10 & !is.na(data$value), ]
  }),

  tidyverse = reactive({
    data %>%
      dplyr::filter(value > 10, !is.na(value))
  }),

  data.table = reactive({
    dt[value > 10 & !is.na(value)]
  }),

  times = 20,
  check = FALSE  # Set TRUE to verify identical results
)

# View results
print(benchmark_result)
summary(benchmark_result)

# Visualize
plot(benchmark_result)
```

### Output Metrics
- **Median execution time** (mest robust mål)
- **Memory allocations**
- **Garbage collection counts**
- **Iterations per second**
- **Min/max timing** (identificer outliers)

### Interpretation
```r
# Fokuser på median, ikke mean (robust mod outliers)
# Memory allocations: Lavere er bedre
# GC counts: Højere indikerer memory pressure

# Eksempel output:
# expression      median   mem_alloc
# base_r          1.2ms    47KB
# tidyverse       2.1ms    85KB     # Mere readable, men langsommere
# data.table      0.8ms    12KB     # Hurtigst, men mindre readable
```

### Use Cases
- Optimering af kritiske reactive chains
- Før/efter refactoring validation
- Evaluering af tidyverse vs base R trade-offs
- Large dataset operations

---

## 3. Profile Data Processing Pipelines

### Brug
Detaljeret timing af hver transformation i en data pipeline for at identificere bottlenecks.

### Setup
```r
source("R/utils_profiling.R")

# Kræver pryr for memory tracking (optional)
install.packages("pryr")
```

### Eksempel
```r
# Definer pipeline steps
pipeline_profile <- profile_data_pipeline(
  pipeline_steps = list(
    load = function(d) {
      readr::read_csv(d, col_types = readr::cols())
    },

    validate = function(d) {
      d %>%
        dplyr::filter(!is.na(x_column), !is.na(y_column))
    },

    transform = function(d) {
      d %>%
        dplyr::mutate(
          date = lubridate::dmy(date_str),
          scaled_value = scale(value)
        )
    },

    aggregate = function(d) {
      d %>%
        dplyr::group_by(month = lubridate::floor_date(date, "month")) %>%
        dplyr::summarise(
          mean_value = mean(scaled_value),
          n = n()
        )
    }
  ),
  input_data = "data/large_dataset.csv"
)

# View timing breakdown
print(pipeline_profile)
```

### Output
```
  step        time_ms  memory_mb  output_rows
  load        234.50   12.30      50000
  validate     45.20    2.10      48500
  transform   156.80    8.40      48500
  aggregate    89.30    1.20      36
```

### Analysis
```r
# Identificer langsomste step
slowest_step <- pipeline_profile[which.max(pipeline_profile$time_ms), ]

# Total pipeline time
total_time <- sum(pipeline_profile$time_ms)

# Memory intensive steps
high_memory <- pipeline_profile[pipeline_profile$memory_mb > 5, ]

# Visualize
barplot(
  pipeline_profile$time_ms,
  names.arg = pipeline_profile$step,
  ylab = "Time (ms)",
  main = "Pipeline Step Performance"
)
```

### Use Cases
- File upload processing optimization
- Auto-detection pipeline tuning
- SPC data preparation performance
- Identifying redundant transformations

---

## Best Practices

### 1. Reproducible Profiling
```r
# Set seed for consistent data generation
set.seed(42)

# Use fixed dataset size
test_data <- generate_test_data(n_rows = 10000)

# Run multiple iterations
replicate(5, profile_reactive(...))
```

### 2. Realistic Data
```r
# Use production-sized datasets
# Performance characteristics change with scale

# Small data (< 1000 rows): Overhead dominates
# Medium data (1000-10000): Representative
# Large data (> 10000): Real bottlenecks appear
```

### 3. Isolate Components
```r
# Profile single operations, not entire app
# Makes it easier to identify specific bottlenecks

# ❌ Don't profile entire reactive chain
# ✅ Do profile individual reactives
```

### 4. Compare Against Baseline
```r
# Always measure before optimization
baseline <- benchmark_reactives(current = reactive({ ... }))

# Implement optimization
# ...

# Measure improvement
optimized <- benchmark_reactives(
  baseline = reactive({ old_implementation }),
  optimized = reactive({ new_implementation })
)

# Verify improvement
improvement_pct <- (baseline$median - optimized$median) / baseline$median * 100
```

---

## Integration with TDD

### Performance Regression Tests
```r
# tests/testthat/test-performance-regression.R

test_that("Data aggregation completes within 100ms", {
  skip_on_ci()  # Performance tests kan være flaky på CI

  # Setup
  test_data <- generate_test_data(10000)

  # Benchmark
  timing <- bench::mark(
    aggregate_data(test_data),
    iterations = 10
  )

  # Assert performance threshold
  expect_lt(
    median(timing$median),
    as_bench_time("100ms")
  )
})
```

### Load Testing
```r
# Gradvis øgning af dataset size
sizes <- c(100, 1000, 10000, 50000)

performance_by_size <- purrr::map_dfr(sizes, function(n) {
  data <- generate_test_data(n)

  timing <- bench::mark(
    process_data(data),
    iterations = 5
  )

  tibble::tibble(
    n_rows = n,
    median_ms = as.numeric(timing$median) * 1000,
    memory_mb = as.numeric(timing$mem_alloc) / 1024^2
  )
})

# Check for linear vs quadratic scaling
plot(performance_by_size$n_rows, performance_by_size$median_ms)
```

---

## Profiling Checklist

Før du committer performance-kritisk kode:

- [ ] Profiled med realistiske data størrelser
- [ ] Benchmarked mod baseline implementation
- [ ] Verificeret memory allocations er acceptable
- [ ] Dokumenteret trade-offs (readability vs performance)
- [ ] Tilføjet performance regression test hvis kritisk
- [ ] Valideret at optimization ikke bryder tests

---

## Troubleshooting

### "profvis package not found"
```r
install.packages("profvis")
```

### "bench package not found"
```r
install.packages("bench")
```

### "Memory tracking returns NA"
```r
# pryr er optional, installér for bedre memory tracking
install.packages("pryr")

# Fallback: gc() bruges automatisk hvis pryr mangler
```

### Profiling hænger på MacOS
```r
# Kendt issue med profvis på nogle MacOS versioner
# Løsning: Opdater R til seneste version
# eller brug bench::mark() i stedet
```

---

## Relaterede Ressourcer

- **Profvis documentation**: https://rstudio.github.io/profvis/
- **bench package**: https://bench.r-lib.org/
- **R Performance optimization**: https://adv-r.hadley.nz/perf-improve.html
- **Shiny performance**: https://shiny.rstudio.com/articles/performance.html

---

## Maintained by
Development Tools - SPCify Project
Last updated: 2025-10-11
