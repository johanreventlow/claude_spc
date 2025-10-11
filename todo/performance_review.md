# Performance Analysis Report

  ## Executive Summary

  - Plot caching keeps full datasets and ggplot objects in memory indefinitely; without eviction the cache grows until
    the R process exhausts RAM.
  - QIC cache invalidation clears everything on almost every edit, so expensive qicharts2 work is redone whenever users
    touch the data.
  - The rendering pipeline re-hashes the entire dataset multiple times per update; for larger uploads the hashing
    overhead alone adds >100 ms per redraw.
  - Additional medium issues include row-wise apply() copies on every visualization refresh and an auto-detect cache
    that never purges expired entries.
  - Fixes focus on adding real cache eviction, making invalidation selective, sharing data signatures, and tightening
    hot-path vectorisation.

  ## Critical Issues (Immediate Action Required)

  ### Issue 1: Plot Cache Retains Unbounded Data

  - Type: Memory Leak
  - Location: R/utils_server_plot_optimization.R:37, R/utils_server_plot_optimization.R:49, R/
    utils_server_plot_optimization.R:371
  - Impact: Each unique preprocessing result stores a full data frame (≈2.3 MB for 50k×6 sample) plus prepared data
    and ggplot objects; without eviction or periodic cleanup, a moderate editing session can easily accumulate >200 MB,
    risking OOM and GC churn.
  - Root Cause: .plot_cache_env only drops entries when the same key is read after expiry; there is no size cap or
    background sweep, and large objects are cached for 15–60 minutes.
  - Recommended Fix: Introduce cache housekeeping that runs on the existing background task loop to remove expired items
    and enforce a hard size limit (e.g. reuse CACHE_CONFIG$size_limit_entries); skip caching of whole raw datasets when
    the key is unique per update or store lightweight signatures instead.
  - Validation: After implementing eviction, drive tests/performance/test-qic-caching-benchmark.R and monitor a
    profiling run (pryr::mem_change around repeated redraws) to confirm cache size stays bounded.

  ## High Priority Issues

  ### Issue 2: “Selective” QIC Invalidation Clears Entire Cache

  - Type: Cache Inefficiency / Bottleneck
  - Location: R/utils_qic_cache_invalidation.R:94-129
  - Impact: For every table edit or value change the code calls qic_cache$clear(), so the next render pays the full
    qicharts2 cost (~190 ms for 200 rows measured via microbenchmark). Combined with preprocessing, each UI tweak incurs
    several hundred milliseconds.
  - Root Cause: The selective branch is a placeholder that still clears the whole cache, so all contexts lose memoized
    results.
  - Recommended Fix: Make invalidation key-aware—derive the cache key prefix from chart type or data signature and
    remove only matching entries; fall back to full clears only when structure changes.
  - Validation: Re-run tests/performance/test-qic-caching-benchmark.R and log qic_cache$stats() during interactive edits
    to verify hit rate stays >80 % after normal value edits.

  ### Issue 3: Repeated Full-Data Hashing in Render Pipeline

  - Type: Bottleneck / Algorithm Complexity
  - Location: R/utils_server_plot_optimization.R:37-55, R/utils_server_plot_optimization.R:383-399, R/
    utils_qic_caching.R:189-193
  - Impact: The same data frame is hashed at least three times per render (preprocess, plot data, final plot, plus
    QIC key). For a 500k×6 dataset a single digest() call costs ~44 ms; multiplied across stages this adds >130 ms per
    redraw before qicharts2 even runs.
  - Root Cause: Each layer builds its own cache key from the entire data object instead of reusing a shared signature.
  - Recommended Fix: Compute one data signature when app_state$data$current_data changes (store it in state) and pass it
    through; build derivative cache keys from the signature rather than re-hashing the data repeatedly.
  - Validation: Profile redraws with profvis::profvis before/after to confirm hashing time drops; ensure regression
    tests in tests/performance/test-fase5-performance.R still pass.

  ## Medium Priority Issues

  ### Issue 4: Row-Wise apply() in Visualization Cache

  - Type: Bottleneck
  - Location: R/mod_spc_chart_server.R:64-69
  - Impact: apply(data, 1, …) converts the entire data frame to a matrix, duplicating memory and doing per-row R
    function calls. On large uploads this copies several megabytes each time the visualization cache refreshes, adding
    latency and GC load.
  - Root Cause: Using apply() for row filtering instead of vectorised helpers.
  - Recommended Fix: Replace with vectorised checks (rowSums(!is.na(data)) > 0 or reuse the existing
    filter_complete_cases_optimized()), avoiding matrix coercion and per-row R closures.
  - Validation: Microbenchmark the new approach versus current code on a representative dataset and re-run tests/test-
    mod-spc-chart-integration.R.

  ### Issue 5: Auto-Detect Cache Has No Cleanup Path

  - Type: Memory Leak
  - Location: R/utils_server_performance_opt.R:123-179
  - Impact: .cache_env accumulates one entry per unique dataset; expired entries are only removed if the same key is
    queried again, so varied uploads leave stale results indefinitely. Each entry is small, but long-lived R processes
    will keep growing the environment.
  - Root Cause: No eviction or periodic sweep; TTL logic is read-through only.
  - Recommended Fix: Reuse the background maintenance cycle to cull expired keys and cap cache size (mirror the approach
    suggested for .plot_cache_env).
  - Validation: After the change, inspect ls(environmentName) across a long test run to ensure the cache remains within
    the configured limit and run tests/test-autodetect-engine.R to confirm correctness.

  ## Optimization Opportunities

  - Add cache statistics emission (hit/miss, size) to get_cache_statistics() so observability dashboards can detect
    regressions early.
  - Move token-limit enforcement in comprehensive_system_cleanup() away from repeated tibble/dplyr pipelines to lighter
    base R operations for the periodic task (R/utils_ui_ui_updates.R:928-934).
  - Consider Shiny’s bindCache() for plot outputs to piggyback on Posit’s automatic eviction policies rather than
    maintaining bespoke caches.

  ## Performance Metrics

  - qicharts2 render (run chart, 200 points): mean ≈190 ms (microbenchmark, 20 runs).
  - digest::digest() on 500k×6 numeric frame: ≈44 ms per call (system.time benchmark).
  - Cached preprocessing payload (list(data=…) for 50k×6): ≈2.3 MB (measured via object.size).

  ## Recommended Action Plan

  1. Implement background eviction for .plot_cache_env and align cache limits; retest memory profile. (Large impact,
     medium effort)
  2. Replace blanket QIC invalidation with keyed deletion and verify improved hit rates. (Large impact, medium effort)
  3. Introduce shared data signatures across caching layers to cut repeated hashing costs. (High impact, medium effort)
  4. Vectorise the non-empty row filter in mod_spc_chart_server and benchmark. (Medium impact, low effort)
  5. Add expiry enforcement to the auto-detect cache and monitor its size. (Medium impact, low effort)
  6. Once fixes land, rerun the full performance suite (tests/performance/), capture new baselines, and update
     observability dashboards to watch cache health.
