# SPC Plot Modernization Plan

Planen beskriver de tre højst prioriterede forbedringer fra gennemgangen af `R/fct_spc_plot_generation.R` og tilhørende utils. Fokus er at modernisere dataflowet, nedbringe base-R afhængigheder og sikre alignment med projektets tidyverse-arkitektur.

---

## 1. Kommentar-mapping via tidyverse

- **Problem**: `extract_comment_data()` bruger `merge()`, base-subsetting og `data.frame`. Det gør logikken sværere at teste og øger risikoen for typekonvertering.
- **Løsning**:
  1. Introducér en tibble-pipeline (`tibble::tibble()` + `dplyr::left_join()`) i stedet for `merge()`.
  2. Replacer `sapply()` med `purrr::map_chr()` for sanitizing af kommentarer.
  3. Brug `tidyr::drop_na()` og `stringr::str_trim()` for at filtrere tomme tekster.
  4. Tilføj en fokuseret test i `tests/testthat/test-utils_spc_data_processing.R` (eller opret ny fil) der sikrer korrekt join og trimming.
- **Berørte filer**: `R/fct_spc_plot_generation.R`, evt. ny/udvidet testfil i `tests/testthat/`.
- **Test**: `R -e "library(SPCify); testthat::test_file('tests/testthat/test-utils_spc_data_processing.R')"`

---

## 2. Deklarativ rensning af QIC-argumenter

- **Problem**: `clean_qic_call_args()` arbejder med manuelle indeksjusteringer (`complete.cases()`, `which()`, `sum(removed_positions < pos)`), som er skrøbelige og svære at udvide.
- **Løsning**:
  1. Konstruer et tibble med `row_id`, `x`, `y` og evt. `n`.
  2. Brug `tidyr::drop_na()` til at fjerne rækker med manglende `x`/`y`.
  3. Beregn fjernede rækker via `setdiff()` og justér `part`/`freeze` med `purrr::map_dbl()` i en deklarativ pipeline.
  4. Indkapsl logikken i helper-funktioner for at lette test (fx `adjust_part_positions()`).
  5. Skriv nye tests der dækker scenarier med part/freeze og rækker fjernet midt i serien.
- **Berørte filer**: `R/fct_spc_plot_generation.R`, evt. helper i `R/utils_spc_data_processing.R`.
- **Test**: `R -e "library(SPCify); testthat::test_file('tests/testthat/test-spc_qic_arguments.R')"`

---

## 3. Udskift base-R i plot enhancement og logging

- **Problem**: Sektioner i `add_plot_enhancements()` og `generateSPCPlot()` bruger `data.frame()`, `rbind()`, `message()` og `.GlobalEnv`.
- **Løsning**:
  1. Erstat datastrukturer med tibbles og `dplyr::bind_rows()` for at undgå kopier og bevare kolonneklasser.
  2. Skift til `log_info()`/`log_debug()` i stedet for `message()`, og flyt counters til `app_state$performance`.
  3. Udnyt `dplyr::filter()` frem for base-subsetting, når extensions tilføjes til ggplot.
  4. Gennemgå caching/logging efter ændringer, så `safe_operation()` stadig fanger fejl korrekt.
- **Berørte filer**: `R/fct_spc_plot_generation.R`, `R/state_management.R` (til performance-slot), tests hvis logik ændrer output.
- **Test**:
  - `R -e "library(SPCify); testthat::test_dir('tests/testthat')"`
  - `R -e "library(SPCify); testthat::test_file('tests/testthat/test-spc_plot_generation.R')"`

---

### Generelle noter
- Følg TDD: skriv eller opdater tests før implementering.
- Kør `lintr` (`R -e "devtools::lint()"`) efter refaktoreringerne.
- Validér at eventuelle nye tidyverse-kald respekterer eksisterende cache- og logging-infrastruktur.
