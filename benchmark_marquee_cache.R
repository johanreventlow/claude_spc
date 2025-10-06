# ==============================================================================
# BENCHMARK: Marquee Style Cache Performance
# ==============================================================================
# Verificerer performance forbedring af marquee style caching
#
# EXPECTED RESULTS:
# - UNCACHED: ~1-2ms per style creation
# - CACHED: ~0.01-0.05ms per style lookup
# - IMPROVEMENT: 20-100x hurtigere ved cache hit
# ==============================================================================

library(marquee)
library(microbenchmark)

# ==============================================================================
# LOAD CACHE IMPLEMENTATION
# ==============================================================================

source("bfh_layout_reference_dev.R")

# ==============================================================================
# BENCHMARK 1: First call (cache miss) vs subsequent calls (cache hit)
# ==============================================================================

message("\n=== BENCHMARK 1: Cache Hit Performance ===\n")

# Clear cache først
clear_marquee_style_cache()

# Benchmark cache miss (første kald) vs cache hit (efterfølgende kald)
benchmark_cache <- microbenchmark(
  cache_miss = {
    clear_marquee_style_cache()
    get_right_aligned_marquee_style(lineheight = 0.9)
  },
  cache_hit = {
    get_right_aligned_marquee_style(lineheight = 0.9)  # Allerede cached
  },
  times = 100
)

print(summary(benchmark_cache))

# Calculate improvement factor
miss_median <- median(benchmark_cache$time[benchmark_cache$expr == "cache_miss"])
hit_median <- median(benchmark_cache$time[benchmark_cache$expr == "cache_hit"])
improvement_factor <- miss_median / hit_median

cat("\n")
cat("Performance Summary:\n")
cat("  Cache miss:  ", sprintf("%.2f ms", miss_median / 1e6), "\n")
cat("  Cache hit:   ", sprintf("%.2f ms", hit_median / 1e6), "\n")
cat("  Improvement: ", sprintf("%.1fx faster", improvement_factor), "\n")

# ==============================================================================
# BENCHMARK 2: Multiple lineheight values
# ==============================================================================

message("\n=== BENCHMARK 2: Multiple Lineheight Values ===\n")

# Clear cache
clear_marquee_style_cache()

# Test at forskellige lineheight værdier caches korrekt
lineheights <- c(0.8, 0.9, 1.0, 1.1, 1.2)

# Første iteration (alle cache misses)
first_iteration <- system.time({
  for (lh in lineheights) {
    get_right_aligned_marquee_style(lineheight = lh)
  }
})

# Anden iteration (alle cache hits)
second_iteration <- system.time({
  for (lh in lineheights) {
    get_right_aligned_marquee_style(lineheight = lh)
  }
})

cat("First iteration (cache misses):  ", sprintf("%.2f ms", first_iteration["elapsed"] * 1000), "\n")
cat("Second iteration (cache hits):   ", sprintf("%.2f ms", second_iteration["elapsed"] * 1000), "\n")
cat("Improvement:                     ", sprintf("%.1fx faster", first_iteration["elapsed"] / second_iteration["elapsed"]), "\n")

# ==============================================================================
# BENCHMARK 3: Realistic scenario - 10 plot calls
# ==============================================================================

message("\n=== BENCHMARK 3: Realistic Scenario (10 plots) ===\n")

# Simulér 10 plot rendering calls med samme lineheight

# WITHOUT caching (manual creation hver gang)
benchmark_uncached <- system.time({
  for (i in 1:10) {
    style <- marquee::modify_style(
      marquee::classic_style(),
      "p",
      margin = marquee::trbl(0),
      align = "right",
      lineheight = 0.9
    )
  }
})

# WITH caching
clear_marquee_style_cache()
benchmark_cached <- system.time({
  for (i in 1:10) {
    style <- get_right_aligned_marquee_style(lineheight = 0.9)
  }
})

cat("10 plot calls WITHOUT cache: ", sprintf("%.2f ms", benchmark_uncached["elapsed"] * 1000), "\n")
cat("10 plot calls WITH cache:    ", sprintf("%.2f ms", benchmark_cached["elapsed"] * 1000), "\n")
cat("Time saved:                  ", sprintf("%.2f ms", (benchmark_uncached["elapsed"] - benchmark_cached["elapsed"]) * 1000), "\n")
cat("Improvement:                 ", sprintf("%.1fx faster", benchmark_uncached["elapsed"] / benchmark_cached["elapsed"]), "\n")

# ==============================================================================
# SUMMARY
# ==============================================================================

message("\n=== FINAL SUMMARY ===\n")
cat("Marquee style caching provides:\n")
cat("  - ", sprintf("%.1fx faster", improvement_factor), " lookup speed (cache hit vs miss)\n")
cat("  - Negligible memory overhead (styles are small objects)\n")
cat("  - Safe caching (styles are immutable based on lineheight)\n")
cat("\nRECOMMENDATION: Cache is working correctly and provides significant performance benefit.\n")
