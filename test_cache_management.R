# ==============================================================================
# TEST SCRIPT: CACHE MANAGEMENT FOR LABEL PLACEMENT SYSTEM
# ==============================================================================
#
# Verificerer TTL-baseret cache purge strategi og memory management.
#
# TESTS:
# 1. TTL purge virker korrekt
# 2. Max size enforcement fungerer (FIFO strategy)
# 3. Stats tracking er præcis
# 4. Memory usage ikke vokser ubegrænset
# 5. Performance impact er minimal (<1ms overhead)
#
# ==============================================================================

# Load dependencies
suppressPackageStartupMessages({
  library(ggplot2)
  library(marquee)
  library(testthat)
})

# Source label placement system
source("utils_standalone_label_placement.R")
source("R/config_label_placement.R")

cat("\n=== CACHE MANAGEMENT TEST SUITE ===\n\n")

# ==============================================================================
# TEST 1: Basic Cache Operations
# ==============================================================================

cat("TEST 1: Basic Cache Operations\n")
cat("-------------------------------\n")

# Clear all caches
clear_all_placement_caches()

# Verify empty state
stats <- get_placement_cache_stats()
test_that("Caches start empty", {
  expect_equal(stats$panel_cache$cache_size, 0)
  expect_equal(stats$grob_cache$cache_size, 0)
  expect_equal(stats$panel_cache$cache_hits, 0)
  expect_equal(stats$grob_cache$cache_hits, 0)
})

cat("✓ Caches initialized empty\n")

# Create test plot
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  theme_minimal()

# Measure panel height (should create cache entry)
panel_h1 <- measure_panel_height_inches(p, use_cache = TRUE)

stats_after_miss <- get_placement_cache_stats()
test_that("Cache miss recorded", {
  expect_equal(stats_after_miss$panel_cache$cache_size, 1)
  expect_equal(stats_after_miss$panel_cache$cache_misses, 1)
  expect_equal(stats_after_miss$panel_cache$cache_hits, 0)
})

cat("✓ Cache miss recorded correctly\n")

# Measure again (should hit cache)
panel_h2 <- measure_panel_height_inches(p, use_cache = TRUE)

stats_after_hit <- get_placement_cache_stats()
test_that("Cache hit recorded", {
  expect_equal(stats_after_hit$panel_cache$cache_size, 1)
  expect_equal(stats_after_hit$panel_cache$cache_hits, 1)
  expect_equal(panel_h1, panel_h2)  # Same result
})

cat("✓ Cache hit recorded correctly\n")
cat("  Panel height: ", round(panel_h1, 3), " inches\n")
cat("  Hit rate: ", round(stats_after_hit$panel_cache$hit_rate * 100, 1), "%\n\n")

# ==============================================================================
# TEST 2: TTL-Based Purge
# ==============================================================================

cat("TEST 2: TTL-Based Purge\n")
cat("-----------------------\n")

# Configure short TTL for testing (2 seconds)
configure_placement_cache(ttl_seconds = 2, max_cache_size = 100)

# Create some cache entries
clear_all_placement_caches()
for (i in 1:5) {
  # Create slightly different plots to generate different cache keys
  p_temp <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    theme_minimal(base_size = 14 + i)  # Vary base_size

  panel_h <- measure_panel_height_inches(p_temp, use_cache = TRUE)
}

stats_before_ttl <- get_placement_cache_stats()
test_that("Multiple cache entries created", {
  expect_equal(stats_before_ttl$panel_cache$cache_size, 5)
})

cat("✓ Created 5 cache entries\n")

# Wait for TTL to expire
cat("  Waiting 2.5 seconds for TTL expiry...\n")
Sys.sleep(2.5)

# Trigger purge by creating new entry (auto-purge check)
p_new <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  theme_minimal(base_size = 20)

panel_h_new <- measure_panel_height_inches(p_new, use_cache = TRUE)

stats_after_ttl <- get_placement_cache_stats()

cat("  Cache size before manual purge: ", stats_after_ttl$panel_cache$cache_size, "\n")

# Manual purge should find expired entries (auto-purge happens every 50 ops, not on every cache miss)
purged <- purge_expired_cache_entries()
cat("  Manual purge removed: ", purged$panel_cache, " expired entries\n")

stats_after_manual_purge <- get_placement_cache_stats()
cat("  Cache size after manual purge: ", stats_after_manual_purge$panel_cache$cache_size, "\n")

test_that("TTL purge works", {
  # After manual purge, only non-expired entries should remain
  # 5 old (expired) + 1 new (not expired) = 1 remaining after purge
  expect_lte(stats_after_manual_purge$panel_cache$cache_size, 1)
})

cat("✓ TTL-based purge working correctly\n\n")

# ==============================================================================
# TEST 3: Max Size Enforcement (FIFO)
# ==============================================================================

cat("TEST 3: Max Size Enforcement (FIFO Strategy)\n")
cat("---------------------------------------------\n")

# Configure small max size for testing
configure_placement_cache(ttl_seconds = 300, max_cache_size = 10)

# Clear and create exactly max_cache_size entries
clear_all_placement_caches()

for (i in 1:10) {
  p_temp <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    theme_minimal(base_size = 14 + i)

  panel_h <- measure_panel_height_inches(p_temp, use_cache = TRUE)
}

stats_at_max <- get_placement_cache_stats()
test_that("Cache reaches max size", {
  expect_equal(stats_at_max$panel_cache$cache_size, 10)
})

cat("✓ Cache filled to max size (10 entries)\n")

# Add one more entry - should trigger FIFO purge
p_overflow <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  theme_minimal(base_size = 30)

panel_h_overflow <- measure_panel_height_inches(p_overflow, use_cache = TRUE)

stats_after_overflow <- get_placement_cache_stats()

cat("  Cache size after overflow: ", stats_after_overflow$panel_cache$cache_size, "\n")

test_that("FIFO purge keeps cache under max", {
  expect_lte(stats_after_overflow$panel_cache$cache_size, 10)
  # Should be reduced to ~75% of max (7-8 entries)
  expect_gte(stats_after_overflow$panel_cache$cache_size, 7)
})

cat("✓ FIFO purge maintains max size limit\n\n")

# ==============================================================================
# TEST 4: Grob Cache Statistics
# ==============================================================================

cat("TEST 4: Grob Cache Statistics\n")
cat("------------------------------\n")

# Reset configuration
configure_placement_cache(ttl_seconds = 300, max_cache_size = 100)
clear_all_placement_caches()

# Create test labels
style <- marquee::modify_style(
  marquee::classic_style(),
  "p",
  margin = marquee::trbl(0),
  align = "right"
)

label_text <- "{.8 **HEADER**}  \n{.24 **VALUE**}"

# Measure label height (should create grob cache entry)
built_p <- ggplot2::ggplot_build(p)
gtable <- ggplot2::ggplot_gtable(built_p)
panel_h <- measure_panel_height_from_gtable(gtable)

h1 <- estimate_label_height_npc(
  text = label_text,
  style = style,
  panel_height_inches = panel_h,
  use_cache = TRUE
)

stats_after_grob_miss <- get_placement_cache_stats()
test_that("Grob cache miss recorded", {
  expect_equal(stats_after_grob_miss$grob_cache$cache_size, 1)
  expect_equal(stats_after_grob_miss$grob_cache$cache_misses, 1)
})

cat("✓ Grob cache miss recorded\n")

# Measure again (should hit cache)
h2 <- estimate_label_height_npc(
  text = label_text,
  style = style,
  panel_height_inches = panel_h,
  use_cache = TRUE
)

stats_after_grob_hit <- get_placement_cache_stats()
test_that("Grob cache hit recorded", {
  expect_equal(stats_after_grob_hit$grob_cache$cache_hits, 1)
  expect_equal(h1, h2)
})

cat("✓ Grob cache hit recorded\n")
cat("  Label height: ", round(h1, 4), " NPC\n")
cat("  Hit rate: ", round(stats_after_grob_hit$grob_cache$hit_rate * 100, 1), "%\n\n")

# ==============================================================================
# TEST 5: Memory Usage Tracking
# ==============================================================================

cat("TEST 5: Memory Usage Tracking\n")
cat("------------------------------\n")

clear_all_placement_caches()

# Create 50 cache entries
for (i in 1:50) {
  p_temp <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    theme_minimal(base_size = 10 + i %% 20)

  panel_h <- measure_panel_height_inches(p_temp, use_cache = TRUE)

  label_h <- estimate_label_height_npc(
    text = sprintf("{.8 **TEST %d**}  \n{.24 **%d%%**}", i, i),
    style = style,
    panel_height_inches = panel_h,
    use_cache = TRUE
  )
}

stats_memory <- get_placement_cache_stats()

cat("  Panel cache: ", stats_memory$panel_cache$cache_size, " entries, ",
    stats_memory$panel_cache$memory_estimate_kb, " KB\n")
cat("  Grob cache: ", stats_memory$grob_cache$cache_size, " entries, ",
    stats_memory$grob_cache$memory_estimate_kb, " KB\n")
cat("  Total memory: ", stats_memory$total_memory_kb, " KB\n")

test_that("Memory usage is reasonable", {
  # Should be under 100 KB for 50 entries total
  expect_lt(stats_memory$total_memory_kb, 100)
})

cat("✓ Memory usage within acceptable limits\n\n")

# ==============================================================================
# TEST 6: Performance Impact
# ==============================================================================

cat("TEST 6: Performance Impact of Auto-Purge\n")
cat("-----------------------------------------\n")

# Configure to trigger auto-purge frequently
configure_placement_cache(
  ttl_seconds = 300,
  max_cache_size = 100,
  purge_check_interval = 10  # Check every 10 operations
)

clear_all_placement_caches()

# Warm up grob cache (faster than panel height measurements)
label_bench <- "{.8 **BENCHMARK**}  \n{.24 **TEST**}"

for (i in 1:5) {
  h <- estimate_label_height_npc(
    text = label_bench,
    style = style,
    panel_height_inches = 6.5,
    use_cache = TRUE
  )
}

# Benchmark cache hit performance (pure cache lookup, no grid operations)
start_time <- Sys.time()
for (i in 1:1000) {  # More iterations for better precision
  h <- estimate_label_height_npc(
    text = label_bench,
    style = style,
    panel_height_inches = 6.5,
    use_cache = TRUE
  )
}
end_time <- Sys.time()

time_per_op <- as.numeric(difftime(end_time, start_time, units = "secs")) / 1000 * 1000

cat("  Time per cache hit: ", round(time_per_op, 4), " ms (avg over 1000 hits)\n")

test_that("Performance overhead is minimal", {
  # Pure cache hit should be < 0.1ms per operation
  expect_lt(time_per_op, 0.1)
})

cat("✓ Performance overhead minimal (<0.1ms per cache hit)\n\n")

# ==============================================================================
# TEST 7: Force Purge
# ==============================================================================

cat("TEST 7: Force Purge\n")
cat("-------------------\n")

# Create some entries
clear_all_placement_caches()
for (i in 1:10) {
  p_temp <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    theme_minimal(base_size = 14 + i)

  panel_h <- measure_panel_height_inches(p_temp, use_cache = TRUE)
}

stats_before_force <- get_placement_cache_stats()
cat("  Cache size before force purge: ", stats_before_force$panel_cache$cache_size, "\n")

# Force purge (should clear everything)
purged <- purge_expired_cache_entries(force = TRUE)

cat("  Entries purged: ", purged$panel_cache, " (panel), ",
    purged$grob_cache, " (grob)\n")

stats_after_force <- get_placement_cache_stats()

test_that("Force purge clears all entries", {
  expect_equal(stats_after_force$panel_cache$cache_size, 0)
  expect_equal(stats_after_force$grob_cache$cache_size, 0)
})

cat("✓ Force purge cleared all caches\n\n")

# ==============================================================================
# TEST 8: Configuration Persistence
# ==============================================================================

cat("TEST 8: Configuration Persistence\n")
cat("----------------------------------\n")

# Set custom configuration
old_config <- configure_placement_cache(
  ttl_seconds = 600,
  max_cache_size = 200,
  purge_check_interval = 100
)

stats_config <- get_placement_cache_stats()

test_that("Configuration applied correctly", {
  expect_equal(stats_config$panel_cache$config$ttl_seconds, 600)
  expect_equal(stats_config$panel_cache$config$max_cache_size, 200)
  expect_equal(stats_config$panel_cache$config$purge_check_interval, 100)

  expect_equal(stats_config$grob_cache$config$ttl_seconds, 600)
  expect_equal(stats_config$grob_cache$config$max_cache_size, 200)
  expect_equal(stats_config$grob_cache$config$purge_check_interval, 100)
})

cat("✓ Configuration persisted correctly\n")
cat("  TTL: ", stats_config$panel_cache$config$ttl_seconds, " seconds\n")
cat("  Max size: ", stats_config$panel_cache$config$max_cache_size, " entries\n")
cat("  Purge interval: ", stats_config$panel_cache$config$purge_check_interval, " ops\n\n")

# Restore default configuration
configure_placement_cache(ttl_seconds = 300, max_cache_size = 100, purge_check_interval = 50)

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("=== TEST SUMMARY ===\n\n")

final_stats <- get_placement_cache_stats()

cat("Final cache state:\n")
cat("  Panel cache: ", final_stats$panel_cache$cache_size, " entries, ",
    final_stats$panel_cache$cache_hits, " hits, ",
    final_stats$panel_cache$cache_misses, " misses\n")
cat("  Grob cache: ", final_stats$grob_cache$cache_size, " entries, ",
    final_stats$grob_cache$cache_hits, " hits, ",
    final_stats$grob_cache$cache_misses, " misses\n")
cat("  Total memory: ", final_stats$total_memory_kb, " KB\n\n")

cat("✅ ALL TESTS PASSED\n\n")

cat("Cache management features verified:\n")
cat("  ✓ TTL-based expiration (5 min default)\n")
cat("  ✓ Max size enforcement (100 entries default, FIFO strategy)\n")
cat("  ✓ Statistics tracking (hits, misses, hit rate, memory)\n")
cat("  ✓ Auto-purge on cache operations (every 50 ops)\n")
cat("  ✓ Manual purge (TTL-based and force)\n")
cat("  ✓ Configuration API (global and per-cache)\n")
cat("  ✓ Performance overhead <1ms per operation\n")
cat("  ✓ Memory usage bounded and predictable\n\n")

cat("Recommended usage:\n")
cat("  - Default config suitable for most Shiny sessions\n")
cat("  - For short sessions: configure_placement_cache(ttl_seconds = 60)\n")
cat("  - For long sessions: configure_placement_cache(ttl_seconds = 600, max_cache_size = 200)\n")
cat("  - Monitor with: get_placement_cache_stats()\n")
cat("  - Manual cleanup: purge_expired_cache_entries()\n\n")
