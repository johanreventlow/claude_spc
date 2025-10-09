#!/usr/bin/env Rscript
# Quick QIC Caching Performance Benchmark
# Sprint 4 Fase 2 verification

cat("=== QIC Caching Performance Benchmark ===\n\n")

# Minimal setup
suppressPackageStartupMessages({
  library(qicharts2)
  library(digest)
})

# Source only necessary files
source("R/config_system_config.R")
source("R/utils_qic_caching.R")

# Create test data
set.seed(42)
test_data <- data.frame(
  Dato = 1:100,
  Værdi = rnorm(100, mean = 50, sd = 10)
)

# Create QIC cache
cat("Creating QIC cache...\n")
qic_cache <- create_qic_cache()

# Test parameters
params <- list(
  x = "Dato",
  y = "Værdi",
  chart = "run"
)

cat("\n--- Benchmark 1: Cache Miss (First Call) ---\n")
time_miss <- system.time({
  result1 <- qicharts2::qic(
    data = test_data,
    x = Dato,
    y = Værdi,
    chart = "run"
  )

  # Cache the result
  cache_key <- generate_qic_cache_key(test_data, params)
  qic_cache$set(cache_key, result1)
})

cat("Time:", round(time_miss["elapsed"] * 1000, 2), "ms\n")

cat("\n--- Benchmark 2: Cache Hit (Second Call) ---\n")
time_hit <- system.time({
  cache_key <- generate_qic_cache_key(test_data, params)
  result2 <- qic_cache$get(cache_key)
})

cat("Time:", round(time_hit["elapsed"] * 1000, 2), "ms\n")

# Calculate improvement
improvement_ms <- (time_miss["elapsed"] - time_hit["elapsed"]) * 1000
improvement_pct <- ((time_miss["elapsed"] - time_hit["elapsed"]) / time_miss["elapsed"]) * 100

cat("\n=== Results ===\n")
cat("Cache miss:", round(time_miss["elapsed"] * 1000, 2), "ms\n")
cat("Cache hit:", round(time_hit["elapsed"] * 1000, 2), "ms\n")
cat("Improvement:", round(improvement_ms, 2), "ms (", round(improvement_pct, 1), "%)\n")
cat("Cache size:", qic_cache$size(), "entries\n")

# Verify results are identical
if (!is.null(result2) && identical(class(result1), class(result2))) {
  cat("\n✅ Cache hit returned valid result\n")
} else {
  cat("\n❌ Cache hit failed\n")
}

cat("\n=== Cache Key Generation Test ===\n")
# Test cache key uniqueness
data_copy <- test_data
key1 <- generate_qic_cache_key(test_data, params)
key2 <- generate_qic_cache_key(data_copy, params)
cat("Same data, same params: Keys identical?", identical(key1, key2), "\n")

# Different data
different_data <- test_data
different_data$Værdi[1] <- 999
key3 <- generate_qic_cache_key(different_data, params)
cat("Different data: Keys different?", !identical(key1, key3), "\n")

cat("\n=== Cache Invalidation Test ===\n")
initial_size <- qic_cache$size()
cat("Cache size before clear:", initial_size, "\n")
qic_cache$clear()
cat("Cache size after clear:", qic_cache$size(), "\n")
cat("Invalidation works?", qic_cache$size() == 0, "\n")

cat("\n✅ QIC Caching Benchmark Complete\n")
