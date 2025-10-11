# Konfigurationsopfølgning – SPCify

## 1. Ret testdata-sti i `inst/golem-config.yml`
- **Problem**: `testing.test_data_file` peger på `R/data/spc_exampledata.csv`, som ikke findes i repoet; testmiljøer får derfor tom indlæsning.
- **Handling**:
  1. Opdater YAML så `test_data_file` bruger den eksisterende fil `inst/extdata/spc_exampledata.csv` (eller tilføj den manglende fil).
  2. Kør `R -e "testthat::test_file('tests/testthat/test-yaml-config-adherence.R')"` for at sikre at konfigurationen godkendes.
  3. Udvid testen til at verificere at filen faktisk eksisterer (`file.exists(...)`) for at undgå regressioner.

## 2. Eliminer duplikerede konstanter i `utils_config_consolidation.R`
- **Problem**: `create_config_registry()` og `get_*_config()` kopierer konstanter (fx `DEBOUNCE_DELAYS` og observer-prioriteter) i stedet for at genbruge værdierne fra `config_*.R`, hvilket skaber divergerende defaults.
- **Handling**:
  1. Refaktor registry-loaderne til at trække værdier direkte fra de eksporterede objekter (fx ganske enkelt returnere `SPC_COLORS`, `DEBOUNCE_DELAYS`, `OBSERVER_PRIORITIES`).
  2. Opdater tilhørende tests i `tests/testthat/test-constants-architecture.R` til at sammenligne mod de autoritative konstanter og ikke mod hardcodede værdier.
  3. Kør hele testpakken for at sikre at registriet stadig fungerer: `R -e "library(SPCify); testthat::test_dir('tests/testthat')"` .

## 3. Skift til `safe_getenv()` for miljøvariabler
- **Problem**: 20+ direkte kald til `Sys.getenv()` omgår typekonvertering, logging og standardværdier; `safe_getenv()` er defineret men ikke brugt.
- **Handling**:
  1. Kortlæg alle runtime-kald (`rg "Sys.getenv"` i `R/`) og erstat med `safe_getenv()` med korrekt `type`-argument og meningsfulde defaults/fallbacks.
  2. Tilføj helper-funktioner hvor komplekse typer kræver ekstra parsing (fx JSON-lister).
  3. Udvid `tests/testthat/test-runtime-config-comprehensive.R` til at dække manglende/malformede variabler og bekræfte fejlmeddelelser.

## 4. Beskyt `.Renviron` og dokumentér hemmeligheder
- **Problem**: Deploy-guiden instruerer i at oprette `.Renviron`, men `.gitignore` ekskluderer den ikke; risiko for at hemmeligheder checkes ind.
- **Handling**:
  1. Tilføj `.Renviron` (og evt. `.env`, `.env.local`) til `.gitignore`.
  2. Opret `docs/ENVIRONMENT_VARIABLES.md` der oplister påkrævede og valgfrie variabler, eksempler til lokal `.Renviron`, og retningslinjer for produktion (Docker/Kubernetes secrets).
  3. Gennemgå historikken for at sikre at hemmeligheder ikke allerede er versioneret; hvis ja, roter dem.

## 5. Roxygen-dokumentation for konfig-gettere
- **Problem**: `config_chart_types.R`, `config_observer_priorities.R` og `config_branding_getters.R` mangler `@export` + beskrivelser, hvilket skjuler API’et i dokumentationen.
- **Handling**:
  1. Tilføj Roxygen-kommentarer med korte beskrivelser, argumentforklaringer og `@export` for de offentlige konstanter/funktioner.
  2. Kør `R -e "devtools::document()"` for at regenerere man-filerne.
  3. (Valgfrit) Tilføj snapshot-tests der sikrer at kritiske konstanter eksisterer i pakken med `expect_true(exists("OBSERVER_PRIORITIES"))`.

## 6. Flyt resterende “magic numbers” til konfigurationsfiler
- **Problem**: Enkelte hårdkodede værdier (fx kommentar-længder i `fct_spc_plot_generation.R` og fallback-prioriteter i `utils_cache_generators.R`) bør centraliseres.
- **Handling**:
  1. Definér passende konstanter i relevante `config_*.R` filer (fx `SPC_COMMENT_MAX_LENGTH` i `config_spc_config.R`).
  2. Opdater forbrugende funktioner til at hente værdierne fra konfigurationen.
  3. Tilføj tests der bruger konstanterne i assertions så fremtidige ændringer kræver opdatering samme sted.
