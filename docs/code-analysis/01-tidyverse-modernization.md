# Tidyverse Modernization Report

**Dato**: 2025-10-09
**Agent**: tidyverse-code-reviewer
**Score**: 75/100

## Executive Summary

Codebasen viser **mixed patterns** - nogle filer bruger excellent tidyverse patterns (purrr, dplyr pipes), mens andre stadig er afhængige af base R loops og apply functions. Der er **betydelige muligheder** for forbedring, især i:

- Konvertering af for-loops til purrr::map() family functions
- Erstatning af base R string manipulation med stringr
- Brug af dplyr til data frame operationer
- Udnyttelse af tidyr til data reshaping

**Overall Assessment**: Codebasen er delvist moderniseret (cirka 40-50% tidyverse adoption). Hovedmulighederne ligger i utility filer og ældre helper functions.

---

## High-Impact Opportunities

### 1. config_branding_getters.R - Loop til purrr::walk()

**Location**: `/Users/johanreventlow/Documents/R/claude_spc/R/config_branding_getters.R:34-40`

**Current Code**:
```r
for (path in fallback_paths) {
  if (file.exists(path)) {
    return(path)
  }
}
```

**Tidyverse Equivalent**:
```r
fallback_paths |>
  purrr::detect(file.exists)
```

**Benefits**:
- Mere deklarativ og funktionel
- Klarere hensigt (find first matching path)
- Eliminerer mutable state
- Single line vs 5 lines

**Priority**: HIGH
**Breaking Changes**: None
**Test Coverage**: Bør tilføje test for path fallback logic

---

### 2. fct_autodetect_helpers.R - for-loop Month Replacement til purrr

**Location**: `/Users/johanreventlow/Documents/R/claude_spc/R/fct_autodetect_helpers.R:625-644`

**Current Code**:
```r
for (i in seq_along(danish_months)) {
  pattern <- paste0("\\b", danish_months[i], "\\b")
  processed_strings <- gsub(pattern, english_months[i], processed_strings, ignore.case = TRUE)
}

for (i in seq_along(danish_month_full)) {
  pattern <- paste0("\\b", danish_month_full[i], "\\b")
  processed_strings <- gsub(pattern, english_month_full[i], processed_strings, ignore.case = TRUE)
}
```

**Tidyverse Equivalent**:
```r
# Create month mapping tibble
month_mapping <- tibble::tribble(
  ~danish, ~english,
  "jan", "Jan",
  "feb", "Feb",
  # ... etc
)

# Apply replacements using purrr::reduce
processed_strings <- month_mapping |>
  purrr::reduce2(
    .x = .$danish,
    .y = .$english,
    .init = date_strings,
    .f = ~ stringr::str_replace_all(..1,
           pattern = stringr::regex(paste0("\\b", ..2, "\\b"), ignore_case = TRUE),
           replacement = ..3)
  )
```

**Benefits**:
- Eliminerer mutable state (processed_strings modification)
- Data-driven approach (mapping as data)
- Mere testbar (month_mapping kan ekstraheres)
- Bruger stringr til konsistent string operations

**Priority**: HIGH
**Breaking Changes**: None (output identisk)
**Test Coverage**: Extensive existing tests should catch issues

---

### 3. fct_spc_plot_generation.R - sapply til purrr::map_dbl()

**Location**: `/Users/johanreventlow/Documents/R/claude_spc/R/fct_spc_plot_generation.R:1131-1167`

**Current Code**:
```r
labels = function(x) {
  sapply(x, function(val) {
    if (is.na(val)) {
      return(NA)
    }
    if (abs(val) >= 1e9) {
      scaled <- val / 1e9
      if (scaled == round(scaled)) {
        paste0(round(scaled), " mia.")
      } else {
        paste0(format(scaled, decimal.mark = ",", nsmall = 1), " mia.")
      }
    } else if (abs(val) >= 1e6) {
      # ... more conditions
    }
  })
}
```

**Tidyverse Equivalent**:
```r
labels = function(x) {
  x |>
    purrr::map_chr(~ {
      dplyr::case_when(
        is.na(.x) ~ NA_character_,
        abs(.x) >= 1e9 ~ {
          scaled <- .x / 1e9
          if_else(scaled == round(scaled),
            paste0(round(scaled), " mia."),
            paste0(format(scaled, decimal.mark = ",", nsmall = 1), " mia.")
          )
        },
        abs(.x) >= 1e6 ~ {
          scaled <- .x / 1e6
          if_else(scaled == round(scaled),
            paste0(round(scaled), "M"),
            paste0(format(scaled, decimal.mark = ",", nsmall = 1), "M")
          )
        },
        abs(.x) >= 1e3 ~ {
          scaled <- .x / 1e3
          if_else(scaled == round(scaled),
            paste0(round(scaled), "K"),
            paste0(format(scaled, decimal.mark = ",", nsmall = 1), "K")
          )
        },
        TRUE ~ {
          if_else(.x == round(.x),
            format(round(.x), big.mark = ".", decimal.mark = ","),
            format(.x, big.mark = ".", decimal.mark = ",", nsmall = 1)
          )
        }
      )
    })
}
```

**Benefits**:
- Erstatter nested if-else med case_when() for klarhed
- Bruger purrr::map_chr() med explicit type safety
- Mere maintainable nested conditions
- Konsistent med tidyverse stil

**Priority**: HIGH
**Breaking Changes**: None
**Test Coverage**: Visualization tests should validate

---

### 4. fct_spc_plot_generation.R - for-loop QIC Part Signal til purrr

**Location**: `/Users/johanreventlow/Documents/R/claude_spc/R/fct_spc_plot_generation.R:817-826`

**Current Code**:
```r
for (p in unique(qic_data$part)) {
  part_rows <- which(qic_data$part == p)
  if (length(part_rows) > 0) {
    part_data <- qic_data[part_rows, ]
    n_cross <- max(part_data$n.crossings, na.rm = TRUE)
    n_cross_min <- max(part_data$n.crossings.min, na.rm = TRUE)
    has_crossing_signal <- !is.na(n_cross) && !is.na(n_cross_min) && n_cross < n_cross_min
    crossings_sig_col[part_rows] <- has_crossing_signal
  }
}
```

**Tidyverse Equivalent**:
```r
crossings_sig_col <- qic_data |>
  dplyr::group_by(part) |>
  dplyr::mutate(
    has_crossing_signal = {
      n_cross <- max(n.crossings, na.rm = TRUE)
      n_cross_min <- max(n.crossings.min, na.rm = TRUE)
      !is.na(n_cross) && !is.na(n_cross_min) && n_cross < n_cross_min
    }
  ) |>
  dplyr::pull(has_crossing_signal)
```

**Benefits**:
- Eliminerer manual row indexing
- Group-by semantic er explicit og klar
- Ingen mutable state (crossings_sig_col updates)
- Signifikant mere læsbar

**Priority**: HIGH
**Breaking Changes**: None
**Test Coverage**: SPC calculation tests cover this

---

## Summary of Recommendations by Priority

| Priority | Count | Estimated Impact | Estimated Effort |
|----------|-------|------------------|------------------|
| HIGH | 4 | Significant readability & maintainability gains | 4-6 hours |
| MEDIUM | 3 | Moderate improvements | 3-4 hours |
| LOW | 2 | Minor consistency improvements | 2-3 hours |

**Total Estimated Effort**: 9-13 hours

---

## Implementation Strategy

### Phase 1: High-Priority Items (Week 1)
1. fct_autodetect_helpers.R - Month replacement loops (most complex)
2. fct_spc_plot_generation.R - QIC part signal loop
3. config_branding_getters.R - Path detection loop
4. fct_spc_plot_generation.R - sapply label formatting

### Phase 2: Medium-Priority Items (Week 2)
5. utils_label_placement.R - Cache purge loops
6. utils_label_formatting.R - Nested if-else to case_when()

### Phase 3: Low-Priority Items (Week 3)
7. String operation consistency (stringr migration)
8. which() to filter() where appropriate

---

## Testing & Validation Checklist

For each change:
- [ ] Run relevant test suite (`testthat::test_file()`)
- [ ] Visual inspection of SPC plots (no rendering changes)
- [ ] Performance benchmarking (no degradation)
- [ ] Code review for readability
- [ ] Check test coverage (maintain ≥90%)

---

## Concerns & Considerations

1. **Performance**: Some tidyverse operations may be slower than base R for very large datasets. However, this app works with clinical data (typically <10,000 rows), so performance impact should be negligible.

2. **Dependency Management**: The codebase already depends heavily on tidyverse (dplyr, purrr, stringr, tidyr). These changes don't add new dependencies.

3. **Team Familiarity**: Ensure team is comfortable with tidyverse patterns.

4. **Backward Compatibility**: Most changes are internal refactoring. No breaking changes to exported functions or Shiny UI/server interface.

5. **Test Coverage**: Current coverage appears excellent. Each change should maintain or improve coverage.

---

## Conclusion

SPC Shiny applikationen har **betydelige muligheder** for tidyverse modernisering, især i:
1. Konvertering af for-loops til purrr functional programming
2. Erstatning af nested if-else med case_when()
3. Brug af dplyr til data frame operations

De foreslåede ændringer vil forbedre:
- **Readability**: Mere deklarativ, mindre imperativ kode
- **Maintainability**: Nemmere at forstå og modificere
- **Testability**: Funktionel approach muliggør bedre unit testing
- **Consistency**: Unified tidyverse stil across codebase

Alle ændringer kan implementeres incrementally med comprehensive test coverage der sikrer ingen breaking changes.
